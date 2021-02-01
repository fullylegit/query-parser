#![forbid(unsafe_code)]
#![deny(unused_must_use)]
pub mod query;
use query::{And, Exists, Near, Not, Or, Phrase, Query, Range, Regex, Term};

use log::trace;
use nom::bytes::complete::{escaped, tag, take_until, take_while};
use nom::character::complete::{
    alphanumeric1, anychar, char, digit1, multispace0, none_of, one_of, satisfy,
};
use nom::combinator::{map, map_res, opt};
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated};
use nom::IResult;
use nom::{branch::alt, error};

// TODO: move to mod.rs? make configurable?
const DEFAULT_FUZZINESS: u64 = 2;

fn eat_whitespace(input: &str) -> IResult<&str, ()> {
    let (input, _whitespace) = multispace0(input)?;
    Ok((input, ()))
}

fn boost(input: &str) -> IResult<&str, Option<f64>> {
    map_res::<_, _, _, _, <f64 as std::str::FromStr>::Err, _, _>(
        opt(preceded(char('^'), digit1)),
        |res: Option<&str>| match res {
            Some(num) => Ok(Some(num.parse::<f64>()?)),
            None => Ok(None),
        },
    )(input)
}

fn fuzziness(input: &str) -> IResult<&str, Option<u64>> {
    map_res::<_, _, _, _, <u64 as std::str::FromStr>::Err, _, _>(
        opt(preceded(char('~'), opt(digit1))),
        |res: Option<Option<&str>>| match res {
            Some(None) => Ok(Some(DEFAULT_FUZZINESS)),
            Some(Some(num)) => Ok(Some(num.parse::<u64>()?)),
            None => Ok(None),
        },
    )(input)
}

// es docs reserved characters: + - = && || > < ! ( ) { } [ ] ^ " ~ * ? : \ /
/// Matches any characters that can be escaped by `\`
fn escapable(input: &str) -> IResult<&str, char> {
    one_of("\".*()[]{}^~ :")(input)
}

fn escaped_without_spaces(input: &str) -> IResult<&str, &str> {
    escaped(none_of("\\\" ^~()[]{}:"), '\\', escapable)(input)
}

fn escaped_with_spaces(input: &str) -> IResult<&str, &str> {
    escaped(none_of("\\\"^~()[]{}:"), '\\', escapable)(input)
}

fn phrase_inner(input: &str) -> IResult<&str, &str> {
    delimited(char('"'), escaped_with_spaces, char('"'))(input)
}

// TODO: refactor the common parts of `adjacent` and `within` into a single parser
fn within(input: &str) -> IResult<&str, Query> {
    let (input, _) = eat_whitespace(input)?;
    // TODO: handle multiple spaces around `WITHIN`
    let (input, phrase, distance) = match input.chars().next() {
        Some('"') => {
            // quoted phrase
            let (input, (phrase, distance)) = separated_pair(
                phrase_inner,
                alt((tag(" WITHIN "), tag("~"))),
                map_res(digit1, |s: &str| s.parse::<u64>()),
            )(input)?;
            (input, phrase, distance)
        }
        _ => {
            // unquoted phrase - only supports WITHIN, not ~
            let (input, phrase) = take_until(" WITHIN")(input)?;
            let (input, distance) =
                preceded(tag(" WITHIN "), map_res(digit1, |s: &str| s.parse::<u64>()))(input)?;
            (input, phrase, distance)
        }
    };

    Ok((input, Query::near(phrase, distance, false)))
}

// TODO: refactor the common parts of `adjacent` and `within` into a single parser
fn adjacent(input: &str) -> IResult<&str, Query> {
    let (input, _) = eat_whitespace(input)?;
    // TODO: handle multiple spaces around `ADJ`
    let (input, phrase, distance) = match input.chars().next() {
        Some('"') => {
            // quoted phrase
            let (input, (phrase, distance)) = separated_pair(
                phrase_inner,
                tag(" ADJ "),
                map_res(digit1, |s: &str| s.parse::<u64>()),
            )(input)?;
            (input, phrase, distance)
        }
        _ => {
            // unquoted phrase
            let (input, phrase) = take_until(" ADJ")(input)?;
            let (input, distance) =
                preceded(tag(" ADJ "), map_res(digit1, |s: &str| s.parse::<u64>()))(input)?;
            (input, phrase, distance)
        }
    };
    Ok((input, Query::near(phrase, distance, true)))
}

fn phrase(input: &str) -> IResult<&str, Phrase> {
    map(phrase_inner, |s| Phrase::new(s))(input)
}

fn term(input: &str) -> IResult<&str, Term> {
    let (input, _) = eat_whitespace(input)?;
    let (input, word) = escaped_without_spaces(input)?;
    let (input, maybe_boost) = boost(input)?;
    let (input, fuzziness) = fuzziness(input)?;
    let (input, boost) = match maybe_boost {
        Some(boost) => (input, Some(boost)),
        None => boost(input)?,
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

fn group(input: &str) -> IResult<&str, Query> {
    let (input, _) = eat_whitespace(input)?;
    trace!("group: {:?}", input);
    let (input, query) = delimited(char('('), parse, char(')'))(input)?;
    trace!("input: {:?}\nbody: {:?}", input, query);
    Ok((input, query))
}

fn query(input: &str) -> IResult<&str, Query> {
    let (input, _) = eat_whitespace(input)?;
    // attempt to parse a field:query pair
    let result: IResult<&str, (&str, Query)> = separated_pair(
        escaped_without_spaces,
        tag(":"),
        alt((
            group,
            within,
            adjacent,
            map(term, From::from),
            map(phrase, From::from),
        )),
    )(input);
    trace!("pair: {:?} {:?}", input, result);
    match result {
        // input was a field:query pair
        Ok((input, (field, query))) => {
            let query = match query {
                // exists is a special case where the pair is backwards
                Query::Term(term) if field == "_exists_" => Query::exists(term),
                query => query.set_field(field),
            };
            Ok((input, query))
        }
        // input wasn't a field:query pair, non-field parsing
        Err(_) => alt((
            group,
            within,
            adjacent,
            map(term, From::from),
            map(phrase, From::from),
        ))(input),
    }
}

pub fn parse(input: &str) -> IResult<&str, Query> {
    trace!("parse: {:?}", input);
    let mut queries = vec![];
    let mut input = input;
    while !input.is_empty() {
        trace!("queries: {:?}", queries);
        let i = if let Ok((input, _)) = tag::<_, _, ()>("AND ")(input) {
            trace!("got AND: {:?}", input);
            let (input, query) = query(input)?;
            match queries.pop() {
                Some(prev_query) => queries.push(Query::and(vec![prev_query, query])),
                None => {
                    return Err(nom::Err::Error(nom::error::make_error(
                        input,
                        nom::error::ErrorKind::Tag,
                    )))
                }
            }
            input
        } else if let Ok((input, _)) = tag::<_, _, ()>("OR ")(input) {
            trace!("got OR: {:?}", input);
            let (input, query) = query(input)?;
            match queries.pop() {
                Some(prev_query) => queries.push(Query::or(vec![prev_query, query])),
                None => {
                    // TODO: return a string: "Unexpected OR, expecting query"
                    return Err(nom::Err::Error(nom::error::make_error(
                        input,
                        nom::error::ErrorKind::Tag,
                    )));
                }
            }
            input
        } else if char::<_, ()>(')')(input).is_ok() {
            // end of a group, return all the queries. don't advance input
            break;
        } else if let Ok((input, Some(boost))) = boost(input) {
            trace!("input: {:?} boost: {:?}", input, boost);
            // boost
            match queries.pop() {
                Some(prev_query) => queries.push(prev_query.set_boost(boost)),
                None => {
                    // TODO: return a string: "Unexpected boost, expecting query"
                    return Err(nom::Err::Error(nom::error::make_error(
                        input,
                        nom::error::ErrorKind::Tag,
                    )));
                }
            }
            input
        } else {
            let (i, query) = query(input)?;
            queries.push(query);
            i
        };
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
    use std::panic::{self, UnwindSafe};
    use std::sync::Once;

    static INIT: Once = Once::new();

    fn setup() {
        INIT.call_once(|| {
            env_logger::init();
        });
    }

    fn run_test<T>(test: T) -> ()
    where
        T: FnOnce() -> () + UnwindSafe,
    {
        setup();

        let result = panic::catch_unwind(|| test());

        // teardown();

        assert!(result.is_ok())
    }

    #[test]
    fn single_term() {
        run_test(|| {
            let expected = Ok(("", Query::Term(Term::new("word"))));
            let actual = parse("word");
            assert_eq!(expected, actual);
            let expected = Ok(("", Query::Term(Term::new("#word"))));
            let actual = parse("#word");
            assert_eq!(expected, actual);
            let expected = Ok((" eggs", Term::new("#word")));
            let actual = term("#word eggs");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn or() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::or(vec![Term::new("#word").into(), Term::new("eggs").into()]),
            ));
            let actual = parse("#word eggs");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn field_term() {
        run_test(|| {
            let expected = Ok(("", Query::Term(Term::new("word").set_field("eggs"))));
            let actual = parse("eggs:word");
            assert_eq!(expected, actual);
            let actual = parse("eggs: word");
            assert_eq!(expected, actual);
            let actual = parse("eggs:  word");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn single_term_boosted() {
        run_test(|| {
            let expected = Ok(("", Query::Term(Term::new("word").set_boost(5.0))));
            let actual = parse("word^5");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn single_term_fuzzy() {
        run_test(|| {
            let expected = Ok(("", Query::Term(Term::new("word").set_fuzziness(5))));
            let actual = parse("word~5");
            assert_eq!(expected, actual);
            let expected = Ok(("", Query::Term(Term::new("word").set_fuzziness(2))));
            let actual = parse("word~");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn single_term_fuzzy_and_boosted() {
        run_test(|| {
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
        })
    }

    #[test]
    fn phrase() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Phrase(Phrase::new("the quick brown fox jumps over the lazy dog")),
            ));
            let actual = parse(r#""the quick brown fox jumps over the lazy dog""#);
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn near_unordered() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::near("the quick brown fox jumps over the lazy dog", 3, false),
            ));
            let actual = parse(r#""the quick brown fox jumps over the lazy dog" WITHIN 3"#);
            assert_eq!(expected, actual);
            let actual = parse(r#""the quick brown fox jumps over the lazy dog"~3"#);
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn near_unordered_unquoted() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::near("the quick brown fox jumps over the lazy dog", 3, false),
            ));
            let actual = parse("the quick brown fox jumps over the lazy dog WITHIN 3");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn near_ordered() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::near("the quick brown fox jumps over the lazy dog", 3, true),
            ));
            let actual = parse(r#""the quick brown fox jumps over the lazy dog" ADJ 3"#);
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn near_ordered_unquoted() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::near("the quick brown fox jumps over the lazy dog", 3, true),
            ));
            let actual = parse("the quick brown fox jumps over the lazy dog ADJ 3");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn metaphone() {
        run_test(|| {
            let expected = todo!();
            let actual = parse("~smith");
            // assert_eq!(expected, actual);
        })
    }

    // The `es_docs_*` tests are all taken from the Elastic query string syntax docs at
    // https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-string-query.html#query-string-syntax

    #[test]
    fn es_docs_field_term() {
        run_test(|| {
            let expected = Ok(("", Query::Term(Term::new("active").set_field("status"))));
            let actual = parse("status:active");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_field_term_or() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::or(vec![
                    Term::new("quick").set_field("title").into(),
                    Term::new("brown").set_field("title").into(),
                ]),
            ));
            let actual = parse("title:(quick OR brown)");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_field_phrase() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Phrase(Phrase::new("John Smith").set_field("author")),
            ));
            let actual = parse(r#"author:"John Smith""#);
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_field_escaped_space() {
        run_test(|| {
            let expected = Ok(("", Query::Term(Term::new("Alice").set_field("first name"))));
            let actual = parse(r#"first\ name:Alice"#);
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_field_wildcard() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::or(vec![
                    Term::new("quick").set_field("book.*").into(),
                    Term::new("brown").set_field("book.*").into(),
                ]),
            ));
            let actual = parse(r#"book.\*:(quick OR brown)"#);
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_field_exists() {
        run_test(|| {
            let expected = Ok(("", Query::exists(Term::new("title"))));
            let actual = parse("_exists_:title");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_wildcards() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::or(vec![Term::new("qu?ck").into(), Term::new("bro*").into()]),
            ));
            let actual = parse("qu?ck bro*");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_regex() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Regex(Regex::new("joh?n(ath[oa]n)").set_field("name")),
            ));
            let actual = parse(r#"name:/joh?n(ath[oa]n)/"#);
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_default_word_fuzziness() {
        run_test(|| {
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
        })
    }

    #[test]
    fn es_docs_word_fuzziness() {
        run_test(|| {
            let expected = Ok(("", Query::Term(Term::new("quikc").set_fuzziness(1))));
            let actual = parse("quikc~1");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_proximity() {
        run_test(|| {
            let expected = Ok(("", Query::near("fox quick", 5, false)));
            let actual = parse(r#""fox quick"~5"#);
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_inclusive() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Range(Range::new("date", "2012-01-01", true, "2012-12-31", true)),
            ));
            let actual = parse("date:[2012-01-01 TO 2012-12-31]");
            assert_eq!(expected, actual);
            let expected = Ok(("", Query::Range(Range::new("count", "1", true, "5", true))));
            let actual = parse("count:[1 TO 5]");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_exclusive() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Range(Range::new("tag", "alpha", false, "omega", false)),
            ));
            let actual = parse("tag:{alpha TO omega}");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_inclusive_unbounded() {
        run_test(|| {
            let expected = Ok(("", Query::Range(Range::new("count", "10", true, "*", true))));
            let actual = parse("count:[10 TO *]");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_exclusive_unbounded() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Range(Range::new("date", "*", false, "2012-01-01", false)),
            ));
            let actual = parse("date:{* TO 2012-01-01}");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_mixed() {
        run_test(|| {
            let expected = Ok(("", Query::Range(Range::new("count", "1", true, "5", false))));
            let actual = parse("count:[1 TO 5}");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_short_syntax_gt() {
        run_test(|| {
            let expected = Ok(("", Query::Range(Range::new("age", "10", false, "*", true))));
            let actual = parse("age:>10");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_short_syntax_gte() {
        run_test(|| {
            let expected = Ok(("", Query::Range(Range::new("age", "10", true, "*", true))));
            let actual = parse("age:>=10");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_short_syntax_lt() {
        run_test(|| {
            let expected = Ok(("", Query::Range(Range::new("age", "*", true, "10", false))));
            let actual = parse("age:<10");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_short_syntax_lte() {
        run_test(|| {
            let expected = Ok(("", Query::Range(Range::new("age", "*", true, "10", true))));
            let actual = parse("age:<=10");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_short_syntax_and() {
        run_test(|| {
            let expected = Ok(("", Query::Range(Range::new("age", "10", true, "20", false))));
            let actual = parse("age:(>=10 AND <20)");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_short_syntax_plus() {
        run_test(|| {
            let expected = Ok(("", Query::Range(Range::new("age", "10", true, "20", false))));
            let actual = parse("age:(+>=10 +<20)");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_boost_word() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::or(vec![
                    Term::new("quick").set_boost(2.0).into(),
                    Term::new("fox").into(),
                ]),
            ));
            let actual = parse("quick^2 fox");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_boost_phrase() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Phrase(Phrase::new("john smith").set_boost(2.0).into()),
            ));
            let actual = parse(r#""john smith"^2"#);
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_boost_group() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::or(vec![
                    Term::new("foo").set_boost(4.0).into(),
                    Term::new("bar").set_boost(4.0).into(),
                ]),
            ));
            let actual = parse("(foo bar)^4");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_boolean_short_syntax() {
        run_test(|| {
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
        })
    }

    #[test]
    fn es_docs_boolean_short_syntax_words() {
        run_test(|| {
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
        })
    }

    // // NOTE: this test isn't actually in the docs explicitly but is included for completeness
    #[test]
    fn es_docs_boolean_short_syntax_symbols() {
        run_test(|| {
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
        })
    }

    #[test]
    fn es_docs_group() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::and(vec![
                    Query::or(vec![Term::new("quick").into(), Term::new("brown").into()]),
                    Term::new("fox").into(),
                ]),
            ));
            let actual = parse("(quick OR brown) AND fox");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_group_field_boost() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::or(vec![
                    Query::or(vec![
                        Term::new("active").set_field("status").into(),
                        Term::new("pending").set_field("status").into(),
                    ]),
                    Query::or(vec![
                        Term::new("full").set_field("title").set_boost(2.0).into(),
                        Term::new("text").set_field("title").set_boost(2.0).into(),
                        Term::new("search").set_field("title").set_boost(2.0).into(),
                    ]),
                ]),
            ));
            let actual = parse("status:(active OR pending) title:(full text search)^2");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn nested_groups() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::and(vec![
                    Query::or(vec![
                        Term::new("hippo").into(),
                        Query::or(vec![
                            Term::new("eggs").into(),
                            Query::or(vec![Term::new("quick").into(), Term::new("brown").into()]),
                        ]),
                    ]),
                    Query::or(vec![
                        Query::or(vec![
                            Query::or(vec![Term::new("fox").into(), Term::new("elephant").into()]),
                            Term::new("peanut").into(),
                        ]),
                        Term::new("banana").into(),
                    ]),
                ]),
            ));
            let actual = parse(
            "(hippo OR (eggs OR (quick OR brown))) AND (((fox OR elephant) OR peanut) OR banana)",
        );
            assert_eq!(expected, actual);
        })
    }
}
