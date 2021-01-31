pub mod query;
use query::{And, Exists, Near, Not, Or, Phrase, Query, Range, Regex, Term};

use nom::branch::alt;
use nom::bytes::complete::{escaped, tag};
use nom::character::complete::{alphanumeric1, char, digit1, multispace0, none_of, one_of};
use nom::combinator::{map, map_res, opt};
use nom::sequence::{delimited, preceded, separated_pair};
use nom::IResult;

// TODO: move to mod.rs? make configurable?
const DEFAULT_FUZZINESS: u64 = 2;

fn eat_whitespace(input: &str) -> IResult<&str, ()> {
    let (input, _whitespace) = multispace0(input)?;
    Ok((input, ()))
}

/// Matches any characters that can be escaped by `\`
fn escapable(input: &str) -> IResult<&str, char> {
    one_of("\".*")(input)
}

fn escaped_without_spaces(input: &str) -> IResult<&str, &str> {
    escaped(none_of("\\\" ^~"), '\\', escapable)(input)
}

fn escaped_with_spaces(input: &str) -> IResult<&str, &str> {
    escaped(none_of("\\\"^~"), '\\', escapable)(input)
}

fn phrase_inner(input: &str) -> IResult<&str, &str> {
    delimited(char('"'), escaped_with_spaces, char('"'))(input)
}

fn within(input: &str) -> IResult<&str, Query> {
    let (input, _) = eat_whitespace(input)?;
    // TODO: handle multiple spaces around `WITHIN`
    let (input, (phrase, distance)) = separated_pair(
        phrase_inner,
        alt((tag(" WITHIN "), tag("~"))),
        map_res(digit1, |s: &str| s.parse::<u64>()),
    )(input)?;
    Ok((input, Query::near(phrase, distance, false)))
}

fn adjacent(input: &str) -> IResult<&str, Query> {
    let (input, _) = eat_whitespace(input)?;
    // TODO: handle multiple spaces around `ADJ`
    let (input, (phrase, distance)) = separated_pair(
        phrase_inner,
        tag(" ADJ "),
        map_res(digit1, |s: &str| s.parse::<u64>()),
    )(input)?;
    Ok((input, Query::near(phrase, distance, true)))
}

fn phrase(input: &str) -> IResult<&str, Phrase> {
    map(phrase_inner, |s| Phrase::new(s))(input)
}

fn term(input: &str) -> IResult<&str, Term> {
    let (input, _) = eat_whitespace(input)?;
    let (input, word) = escaped_without_spaces(input)?;
    let (input, boost) = map_res::<_, _, _, _, <f64 as std::str::FromStr>::Err, _, _>(
        opt(preceded(char('^'), digit1)),
        |res: Option<&str>| match res {
            Some(num) => Ok(Some(num.parse::<f64>()?)),
            None => Ok(None),
        },
    )(input)?;
    let (input, fuzziness) = map_res::<_, _, _, _, <u64 as std::str::FromStr>::Err, _, _>(
        opt(preceded(char('~'), opt(digit1))),
        |res: Option<Option<&str>>| match res {
            Some(None) => Ok(Some(DEFAULT_FUZZINESS)),
            Some(Some(num)) => Ok(Some(num.parse::<u64>()?)),
            None => Ok(None),
        },
    )(input)?;
    let (input, boost) = match boost {
        Some(boost) => (input, Some(boost)),
        None => map_res::<_, _, _, _, <f64 as std::str::FromStr>::Err, _, _>(
            opt(preceded(char('^'), digit1)),
            |res: Option<&str>| match res {
                Some(num) => Ok(Some(num.parse::<f64>()?)),
                None => Ok(None),
            },
        )(input)?,
    };
    Ok((input, {
        let mut term = Term::new(word);
        if let Some(boost) = boost {
            term = term.set_boost(boost);
        }
        if let Some(fuzziness) = fuzziness {
            term = term.set_fuzziness(fuzziness);
        }
        term
    }))
}

fn query(input: &str) -> IResult<&str, Query> {
    let (input, _) = eat_whitespace(input)?;
    // attempt to parse a field:query pair
    let result: IResult<&str, (&str, Query)> = separated_pair(
        alphanumeric1,
        tag(":"),
        alt((
            within,
            adjacent,
            map(term, From::from),
            map(phrase, From::from),
        )),
    )(input);
    match result {
        // input was a field:query pair
        Ok((input, (field, query))) => {
            let query = query.set_field(field);
            Ok((input, query))
        }
        // input wasn't a field:query pair, non-field parsing
        Err(_) => alt((
            within,
            adjacent,
            map(term, From::from),
            map(phrase, From::from),
        ))(input),
    }
}

pub fn parse(input: &str) -> IResult<&str, Query> {
    let mut queries = vec![];
    let mut input = input;
    while !input.is_empty() {
        let (i, query) = query(input)?;
        queries.push(query);
        let (i, _whitespace) = multispace0(i)?;
        input = i;
    }
    let query = if queries.len() == 1 {
        let mut queries = queries;
        queries.remove(0)
    } else {
        // Default is OR
        // TODO: make the default configurable
        Query::or(queries)
    };
    Ok((input, query))
}

#[cfg(feature = "json")]
pub fn to_json(input: &str) -> Result<String, String> {
    serde_json::to_string(&parse(input)?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_term() {
        let expected = Ok(("", Query::Term(Term::new("word"))));
        let actual = parse("word");
        assert_eq!(expected, actual);
        let expected = Ok(("", Query::Term(Term::new("#word"))));
        let actual = parse("#word");
        assert_eq!(expected, actual);
        let expected = Ok((" eggs", Term::new("#word")));
        let actual = term("#word eggs");
        assert_eq!(expected, actual);
    }

    #[test]
    fn or() {
        let expected = Ok((
            "",
            Query::or(vec![Term::new("#word").into(), Term::new("eggs").into()]),
        ));
        let actual = parse("#word eggs");
        assert_eq!(expected, actual);
    }

    #[test]
    fn field_term() {
        let expected = Ok(("", Query::Term(Term::new("word").set_field("eggs"))));
        let actual = parse("eggs:word");
        assert_eq!(expected, actual);
        let actual = parse("eggs: word");
        assert_eq!(expected, actual);
        let actual = parse("eggs:  word");
        assert_eq!(expected, actual);
    }

    #[test]
    fn single_term_boosted() {
        let expected = Ok(("", Query::Term(Term::new("word").set_boost(5.0))));
        let actual = parse("word^5");
        assert_eq!(expected, actual);
    }

    #[test]
    fn single_term_fuzzy() {
        let expected = Ok(("", Query::Term(Term::new("word").set_fuzziness(5))));
        let actual = parse("word~5");
        assert_eq!(expected, actual);
        let expected = Ok(("", Query::Term(Term::new("word").set_fuzziness(2))));
        let actual = parse("word~");
        assert_eq!(expected, actual);
    }

    #[test]
    fn single_term_fuzzy_and_boosted() {
        let expected = Ok((
            "",
            Query::Term(Term::new("word").set_boost(5.0).set_fuzziness(4)),
        ));
        let actual = parse("word^5~4");
        assert_eq!(expected, actual);
        let expected = Ok((
            "",
            Query::Term(Term::new("word").set_fuzziness(5).set_boost(4.0)),
        ));
        let actual = parse("word~5^4");
        assert_eq!(expected, actual);
    }

    #[test]
    fn phrase() {
        let expected = Ok((
            "",
            Query::Phrase(Phrase::new("the quick brown fox jumps over the lazy dog")),
        ));
        let actual = parse(r#""the quick brown fox jumps over the lazy dog""#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn near_unordered() {
        let expected = Ok((
            "",
            Query::near("the quick brown fox jumps over the lazy dog", 3, false),
        ));
        let actual = parse(r#""the quick brown fox jumps over the lazy dog" WITHIN 3"#);
        assert_eq!(expected, actual);
        let actual = parse(r#""the quick brown fox jumps over the lazy dog"~3"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn near_unordered_unquoted() {
        let expected = Ok((
            "",
            Query::near("the quick brown fox jumps over the lazy dog", 3, false),
        ));
        let actual = parse("the quick brown fox jumps over the lazy dog WITHIN 3");
        assert_eq!(expected, actual);
    }

    #[test]
    fn near_ordered() {
        let expected = Ok((
            "",
            Query::near("the quick brown fox jumps over the lazy dog", 3, true),
        ));
        let actual = parse(r#""the quick brown fox jumps over the lazy dog" ADJ 3"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn near_ordered_unquoted() {
        let expected = Ok((
            "",
            Query::near("the quick brown fox jumps over the lazy dog", 3, true),
        ));
        let actual = parse("the quick brown fox jumps over the lazy dog ADJ 3");
        assert_eq!(expected, actual);
    }

    #[test]
    #[ignore]
    fn metaphone() {
        // let query = "~smith"
        todo!()
    }

    // The `es_docs_*` tests are all taken from the Elastic query string syntax docs at
    // https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-string-query.html#query-string-syntax

    #[test]
    fn es_docs_field_term() {
        let expected = Ok(("", Query::Term(Term::new("active").set_field("status"))));
        let actual = parse("status:active");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_field_term_or() {
        let expected = Ok((
            "",
            Query::or(vec![
                Term::new("quick").set_field("title").into(),
                Term::new("brown").set_field("title").into(),
            ]),
        ));
        let actual = parse("title:(quick OR brown)");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_field_phrase() {
        let expected = Ok((
            "",
            Query::Phrase(Phrase::new("John Smith").set_field("author")),
        ));
        let actual = parse(r#"author:"John Smith""#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_field_escaped_space() {
        let expected = Ok(("", Query::Term(Term::new("Alice").set_field("first name"))));
        let actual = parse(r#"first\ name:Alice"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_field_wildcard() {
        let expected = Ok((
            "",
            Query::or(vec![
                Term::new("quick").set_field("book.*").into(),
                Term::new("brown").set_field("book.*").into(),
            ]),
        ));
        let actual = parse(r#"book.\*:(quick OR brown)"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_field_exists() {
        let expected = Ok(("", Query::exists("title")));
        let actual = parse("_exists_:title");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_wildcards() {
        let expected = Ok((
            "",
            Query::or(vec![Term::new("qu?ck").into(), Term::new("bro*").into()]),
        ));
        let actual = parse("qu?ck bro*");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_regex() {
        let expected = Ok((
            "",
            Query::Regex(Regex::new("joh?n(ath[oa]n)").set_field("name")),
        ));
        let actual = parse(r#"name:/joh?n(ath[oa]n)/"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_default_word_fuzziness() {
        let expected = Ok((
            "",
            Query::or(vec![
                Term::new("quikc").set_fuzziness(2).into(),
                Term::new("brwn").set_fuzziness(2).into(),
                Term::new("foks").set_fuzziness(2).into(),
            ]),
        ));
        let actual = parse("quikc~ brwn~ foks~");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_word_fuzziness() {
        let expected = Ok(("", Query::Term(Term::new("quikc").set_fuzziness(1))));
        let actual = parse("quikc~1");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_proximity() {
        let expected = Ok(("", Query::near("fox quick", 5, false)));
        let actual = parse(r#""fox quick"~5"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_range_inclusive() {
        let expected = Ok((
            "",
            Query::Range(Range::new("date", "2012-01-01", true, "2012-12-31", true)),
        ));
        let actual = parse("date:[2012-01-01 TO 2012-12-31]");
        assert_eq!(expected, actual);
        let expected = Ok(("", Query::Range(Range::new("count", "1", true, "5", true))));
        let actual = parse("count:[1 TO 5]");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_range_exclusive() {
        let expected = Ok((
            "",
            Query::Range(Range::new("tag", "alpha", false, "omega", false)),
        ));
        let actual = parse("tag:{alpha TO omega}");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_range_inclusive_unbounded() {
        let expected = Ok(("", Query::Range(Range::new("count", "10", true, "*", true))));
        let actual = parse("count:[10 TO *]");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_range_exclusive_unbounded() {
        let expected = Ok((
            "",
            Query::Range(Range::new("date", "*", false, "2012-01-01", false)),
        ));
        let actual = parse("date:{* TO 2012-01-01}");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_range_mixed() {
        let expected = Ok(("", Query::Range(Range::new("count", "1", true, "5", false))));
        let actual = parse("count:[1 TO 5}");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_range_short_syntax_gt() {
        let expected = Ok(("", Query::Range(Range::new("age", "10", false, "*", true))));
        let actual = parse("age:>10");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_range_short_syntax_gte() {
        let expected = Ok(("", Query::Range(Range::new("age", "10", true, "*", true))));
        let actual = parse("age:>=10");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_range_short_syntax_lt() {
        let expected = Ok(("", Query::Range(Range::new("age", "*", true, "10", false))));
        let actual = parse("age:<10");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_range_short_syntax_lte() {
        let expected = Ok(("", Query::Range(Range::new("age", "*", true, "10", true))));
        let actual = parse("age:<=10");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_range_short_syntax_and() {
        let expected = Ok(("", Query::Range(Range::new("age", "10", true, "20", false))));
        let actual = parse("age:(>=10 AND <20)");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_range_short_syntax_plus() {
        let expected = Ok(("", Query::Range(Range::new("age", "10", true, "20", false))));
        let actual = parse("age:(+>=10 +<20)");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_boost_word() {
        let expected = Ok((
            "",
            Query::or(vec![
                Term::new("quick").set_boost(2.0).into(),
                Term::new("fox").into(),
            ]),
        ));
        let actual = parse("quick^2 fox");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_boost_phrase() {
        let expected = Ok((
            "",
            Query::Phrase(Phrase::new("john smith").set_boost(2.0).into()),
        ));
        let actual = parse(r#""john smith"^2"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_boost_group() {
        let expected = Ok((
            "",
            Query::or(vec![
                Term::new("foo").set_boost(4.0).into(),
                Term::new("bar").set_boost(4.0).into(),
            ]),
        ));
        let actual = parse("(foo bar)^4");
        assert_eq!(expected, actual);
    }

    #[test]
    #[ignore = "not yet supported"]
    fn es_docs_boolean_short_syntax() {
        let expected = Ok((
            "",
            Query::and(vec![
                Query::or(vec![
                    Query::and(vec![Term::new("quick").into(), Term::new("fox").into()]),
                    Query::and(vec![Term::new("brown").into(), Term::new("fox").into()]),
                    Term::new("fox").into(),
                ]),
                Query::not(Term::new("news").into()),
            ]),
        ));
        let actual = parse("quick brown +fox -news");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_boolean_short_syntax_words() {
        let expected = Ok((
            "",
            Query::and(vec![
                Query::or(vec![
                    Query::and(vec![Term::new("quick").into(), Term::new("fox").into()]),
                    Query::and(vec![Term::new("brown").into(), Term::new("fox").into()]),
                    Term::new("fox").into(),
                ]),
                Query::not(Term::new("news").into()),
            ]),
        ));
        let actual = parse("((quick AND fox) OR (brown AND fox) OR fox) AND NOT news");
        assert_eq!(expected, actual);
    }

    // // NOTE: this test isn't actually in the docs explicitly but is included for completeness
    #[test]
    fn es_docs_boolean_short_syntax_symbols() {
        let expected = Ok((
            "",
            Query::and(vec![
                Query::or(vec![
                    Query::and(vec![Term::new("quick").into(), Term::new("fox").into()]),
                    Query::and(vec![Term::new("brown").into(), Term::new("fox").into()]),
                    Term::new("fox").into(),
                ]),
                Query::not(Term::new("news").into()),
            ]),
        ));
        let actual = parse("((quick && fox) || (brown && fox) || fox) && ! news");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_group() {
        let expected = Ok((
            "",
            Query::and(vec![
                Query::or(vec![Term::new("quick").into(), Term::new("brown").into()]),
                Term::new("fox").into(),
            ]),
        ));
        let actual = parse("(quick OR brown) AND fox");
        assert_eq!(expected, actual);
    }

    #[test]
    fn es_docs_group_field_boost() {
        let expected = Ok((
            "",
            Query::or(vec![
                Term::new("active").set_field("status").into(),
                Term::new("pending").set_field("status").into(),
                Term::new("full").set_field("title").set_boost(2.0).into(),
                Term::new("text").set_field("title").set_boost(2.0).into(),
                Term::new("search").set_field("title").set_boost(2.0).into(),
            ]),
        ));
        let actual = parse("status:(active OR pending) title:(full text search)^2");
        assert_eq!(expected, actual);
    }

    // es docs reserved characters: + - = && || > < ! ( ) { } [ ] ^ " ~ * ? : \ /
}
