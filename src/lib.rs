#![forbid(unsafe_code)]
#![deny(unused_must_use)]
pub mod query;
#[cfg(feature = "phonetic")]
use query::Phonetic;
use query::{Phrase, Query, Range, Regex, Term};

use log::trace;
use nom::branch::alt;
use nom::bytes::complete::{escaped, escaped_transform, tag, take_until};
use nom::character::complete::{char, digit1, multispace0, none_of, one_of};
use nom::combinator::{map, map_res, opt, value};
use nom::sequence::{delimited, preceded, separated_pair, tuple};
use nom::IResult;

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    And,
    Or,
}

#[derive(Debug, Clone, Copy)]
pub struct Config {
    default_fuzziness: u64,
    default_operator: Operator,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            default_fuzziness: 2,
            default_operator: Operator::Or,
        }
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Parser {
    config: Config,
}

impl Parser {
    pub fn with_config(config: Config) -> Self {
        Self { config }
    }

    fn eat_whitespace<'a>(&self, input: &'a str) -> IResult<&'a str, ()> {
        let (input, _whitespace) = multispace0(input)?;
        Ok((input, ()))
    }

    fn float1<'a>(&self, input: &'a str) -> IResult<&'a str, &'a str> {
        let (i, negative) = opt(char('-'))(input)?;
        let (i, whole) = digit1(i)?;
        let (i, decimal) = opt(char('.'))(i)?;
        let (_, fraction) = match decimal {
            Some(_) => {
                let (i, fraction) = digit1(i)?;
                (i, Some(fraction))
            }
            None => (i, None),
        };
        let matching_len = negative.map(|_| 1).unwrap_or(0)
            + whole.len()
            + decimal.map(|_| 1).unwrap_or(0)
            + fraction.unwrap_or("").len();
        let (matching, remainder) = input.split_at(matching_len);
        Ok((remainder, matching))
    }

    fn boost<'a>(&self, input: &'a str) -> IResult<&'a str, Option<f64>> {
        map_res::<_, _, _, _, <f64 as std::str::FromStr>::Err, _, _>(
            opt(preceded(char('^'), |input| self.float1(input))),
            |res: Option<&str>| match res {
                Some(num) => Ok(Some(num.parse::<f64>()?)),
                None => Ok(None),
            },
        )(input)
    }

    fn fuzziness<'a>(&self, input: &'a str) -> IResult<&'a str, Option<u64>> {
        map_res::<_, _, _, _, <u64 as std::str::FromStr>::Err, _, _>(
            opt(preceded(char('~'), opt(digit1))),
            |res: Option<Option<&str>>| match res {
                Some(None) => Ok(Some(self.config.default_fuzziness)),
                Some(Some(num)) => Ok(Some(num.parse::<u64>()?)),
                None => Ok(None),
            },
        )(input)
    }

    // es docs reserved characters: + - = && || > < ! ( ) { } [ ] ^ " ~ * ? : \ /
    /// Matches any characters that can be escaped by `\`
    fn escapable<'a>(&self, input: &'a str) -> IResult<&'a str, char> {
        one_of("\"()[]{}^~ :*")(input)
    }

    #[rustfmt::skip]
    fn escapable_transform<'a>(&self, input: &'a str) -> IResult<&'a str, &'a str> {
        trace!("escapable_transform: {:?}", input);
        alt((
            value("*", tag("*")),
            value(" ", tag(" ")),
        ))(input)
    }

    fn escaped_without_spaces<'a>(&self, input: &'a str) -> IResult<&'a str, &'a str> {
        // TODO: change this to escaped_transform
        escaped(none_of("\\\"^~()[]{}: "), '\\', |input| {
            self.escapable(input)
        })(input)
    }

    fn escaped_with_spaces<'a>(&self, input: &'a str) -> IResult<&'a str, &'a str> {
        // TODO: change this to escaped_transform
        escaped(none_of("\\\"^~()[]{}:"), '\\', |input| {
            self.escapable(input)
        })(input)
    }

    fn escaped_field_name<'a>(&self, input: &'a str) -> IResult<&'a str, String> {
        escaped_transform(none_of("\\\"^~()[]{}:*"), '\\', |input| {
            self.escapable_transform(input)
        })(input)
    }

    fn phrase_inner<'a>(&self, input: &'a str) -> IResult<&'a str, &'a str> {
        delimited(
            char('"'),
            |input| self.escaped_with_spaces(input),
            char('"'),
        )(input)
    }

    fn regex<'a>(&self, input: &'a str) -> IResult<&'a str, Query<'a>> {
        let (input, regex) = delimited(char('/'), take_until("/"), char('/'))(input)?;
        Ok((input, Regex::new(regex).into()))
    }

    fn take_till_reserved_word<'a>(&self, input: &'a str) -> IResult<&'a str, &'a str> {
        // NOTE: this should only be run on unquoted phrases. Does not handle escaped chars
        let indexes = &[
            input.find(" AND "),
            input.find(" OR "),
            input.find(" NOT "),
            input.find("&&"),
            input.find("||"),
            input.find("!"),
            input.find(" WITHIN "),
            input.find(" ADJ "),
            input.find("("),
        ];
        let min = match indexes.iter().filter_map(Option::as_ref).min() {
            Some(min) => *min,
            None => {
                return Err(nom::Err::Error(nom::error::make_error(
                    input,
                    nom::error::ErrorKind::Tag,
                )))
            }
        };
        let (matched, remaining) = input.split_at(min);
        Ok((remaining, matched))
    }

    // TODO: refactor the common parts of `adjacent` and `within` into a single parser
    fn within<'a>(&self, input: &'a str) -> IResult<&'a str, Query<'a>> {
        let (input, _) = self.eat_whitespace(input)?;
        trace!("attepmt within: {:?}", input);
        // TODO: handle multiple spaces around `WITHIN`
        let (input, phrase, distance) = match input.chars().next() {
            Some('"') => {
                // quoted phrase
                let (input, (phrase, distance)) = separated_pair(
                    |input| self.phrase_inner(input),
                    alt((tag(" WITHIN "), tag("~"))),
                    map_res(digit1, |s: &str| s.parse::<u64>()),
                )(input)?;
                (input, phrase, distance)
            }
            _ => {
                // unquoted phrase - only supports WITHIN, not ~
                let (input, phrase) = self.take_till_reserved_word(input)?;
                let (input, distance) =
                    preceded(tag(" WITHIN "), map_res(digit1, |s: &str| s.parse::<u64>()))(input)?;
                trace!("got within: {:?} {:?}", input, distance);
                (input, phrase, distance)
            }
        };

        Ok((input, Query::near(phrase, distance, false)))
    }

    // TODO: refactor the common parts of `adjacent` and `within` into a single parser
    fn adjacent<'a>(&self, input: &'a str) -> IResult<&'a str, Query<'a>> {
        let (input, _) = self.eat_whitespace(input)?;
        trace!("attempt adjacent: {:?}", input);
        // TODO: handle multiple spaces around `ADJ`
        let (input, phrase, distance) = match input.chars().next() {
            Some('"') => {
                // quoted phrase
                let (input, (phrase, distance)) = separated_pair(
                    |input| self.phrase_inner(input),
                    tag(" ADJ "),
                    map_res(digit1, |s: &str| s.parse::<u64>()),
                )(input)?;
                (input, phrase, distance)
            }
            _ => {
                // unquoted phrase
                let (input, phrase) = self.take_till_reserved_word(input)?;
                let (input, distance) =
                    preceded(tag(" ADJ "), map_res(digit1, |s: &str| s.parse::<u64>()))(input)?;
                trace!("got adjacent: {:?} {:?}", input, distance);
                (input, phrase, distance)
            }
        };
        Ok((input, Query::near(phrase, distance, true)))
    }

    fn phrase<'a>(&self, input: &'a str) -> IResult<&'a str, Phrase<'a>> {
        map(|input| self.phrase_inner(input), |s| Phrase::new(s))(input)
    }

    fn term<'a>(&self, input: &'a str) -> IResult<&'a str, Term<'a>> {
        let (input, _) = self.eat_whitespace(input)?;
        let (input, word) = self.escaped_without_spaces(input)?;
        let (input, maybe_boost) = self.boost(input)?;
        let (input, fuzziness) = self.fuzziness(input)?;
        let (input, boost) = match maybe_boost {
            Some(boost) => (input, Some(boost)),
            None => self.boost(input)?,
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

    fn group<'a>(&self, input: &'a str) -> IResult<&'a str, Query<'a>> {
        let (input, _) = self.eat_whitespace(input)?;
        trace!("attempt group: {:?}", input);
        let (input, query) = delimited(char('('), |input| self.parse(input), char(')'))(input)?;
        trace!("got group input: {:?} body: {:?}", input, query);
        Ok((input, query))
    }

    #[rustfmt::skip]
    fn range<'a>(&self, input: &'a str) -> IResult<&'a str, Query<'a>> {
        let (input, (from_type, _, from, _, _, _, to, _, to_type)) = tuple((
            alt((
                char('['),
                char('{'),
            )),
            multispace0,
            |input| self.escaped_without_spaces(input),
            multispace0,
            tag("TO"),
            multispace0,
            |input| self.escaped_without_spaces(input),
            multispace0,
            alt((
                char(']'),
                char('}'),
            )),
        ))(input)?;
        trace!("range: {:?} from_type: {:?}, from: {:?}, to: {:?}, to_type: {:?}", input, from_type, from, to, to_type);
        let query = Range::new(from, from_type == '[', to, to_type == ']').into();
        Ok((input, query))
    }

    fn query<'a>(&self, input: &'a str) -> IResult<&'a str, Query<'a>> {
        let (input, _) = self.eat_whitespace(input)?;
        // attempt to parse a field:query pair
        let result: IResult<&'a str, (String, Query)> = separated_pair(
            |input| self.escaped_field_name(input),
            tag(":"),
            alt((
                |input| self.group(input),
                |input| self.within(input),
                |input| self.adjacent(input),
                |input| self.range(input),
                map(|input| self.regex(input), From::from),
                map(|input| self.term(input), From::from),
                map(|input| self.phrase(input), From::from),
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
                |input| self.group(input),
                |input| self.within(input),
                |input| self.adjacent(input),
                map(|input| self.regex(input), From::from),
                map(|input| self.term(input), From::from),
                map(|input| self.phrase(input), From::from),
            ))(input),
        }
    }

    pub fn parse<'a>(&self, input: &'a str) -> IResult<&'a str, Query<'a>> {
        trace!("parse: {:?}", input);
        let mut queries = vec![];
        let mut input = input;
        while !input.is_empty() {
            trace!("queries: {:?}", queries);
            let (i, _) = self.eat_whitespace(input)?;
            let i = if let Ok((input, _)) = alt((tag::<_, _, ()>("AND "), tag::<_, _, ()>("&&")))(i)
            {
                trace!("got AND: {:?}", input);
                let (input, query) = self.parse(input)?;
                match queries.pop() {
                    Some(Query::And(and)) => queries.push(and.push(query).into()),
                    Some(prev_query) => match query {
                        Query::And(and) => queries.push(and.prepend(prev_query).into()),
                        _ => queries.push(Query::and(vec![prev_query, query])),
                    },
                    None => {
                        return Err(nom::Err::Error(nom::error::make_error(
                            input,
                            nom::error::ErrorKind::Tag,
                        )))
                    }
                }
                input
            } else if let Ok((input, _)) = alt((tag::<_, _, ()>("OR "), tag::<_, _, ()>("||")))(i) {
                trace!("got OR: {:?}", input);
                let (input, query) = self.parse(input)?;
                match queries.pop() {
                    // don't want to do this if the prev or was a different group
                    Some(Query::Or(or)) => queries.push(or.push(query).into()),
                    Some(prev_query) => match query {
                        Query::Or(or) => queries.push(or.prepend(prev_query).into()),
                        _ => queries.push(Query::or(vec![prev_query, query])),
                    },
                    None => {
                        // TODO: return a string: "Unexpected OR, expecting query"
                        return Err(nom::Err::Error(nom::error::make_error(
                            input,
                            nom::error::ErrorKind::Tag,
                        )));
                    }
                }
                input
            } else if let Ok((input, _)) = alt((tag::<_, _, ()>("NOT "), tag::<_, _, ()>("!")))(i) {
                trace!("got NOT: {:?}", input);
                let (input, query) = self.query(input)?;
                queries.push(Query::not(query));
                input
            } else if char::<_, ()>(')')(i).is_ok() {
                trace!("got end of group");
                // end of a group, return all the queries. don't advance input
                break;
            } else if let Ok((input, Some(boost))) = self.boost(i) {
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
                trace!("attempt query: {:?}", i);
                let (i, query) = self.query(i)?;
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
            match self.config.default_operator {
                Operator::Or => Query::or(queries),
                Operator::And => Query::and(queries),
            }
        };
        Ok((input, query))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::panic::{self, UnwindSafe};
    use std::sync::Once;

    static INIT: Once = Once::new();

    fn parse(input: &str) -> IResult<&str, Query> {
        Parser::default().parse(input)
    }

    fn setup() {
        INIT.call_once(|| {
            env_logger::init();
        });
    }

    fn run_test<F>(test: F) -> ()
    where
        F: FnOnce() -> (),
        F: UnwindSafe,
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
            let actual = Parser::default().term("#word eggs");
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
            let expected = Ok(("", Query::Term(Term::new("word").set_field("eggs".into()))));
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
    fn single_term_fractional_boosted() {
        run_test(|| {
            let expected = Ok(("", Query::Term(Term::new("word").set_boost(5.05))));
            let actual = parse("word^5.05");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn single_term_negative_boosted() {
        run_test(|| {
            let expected = Ok(("", Query::Term(Term::new("word").set_boost(-5.05))));
            let actual = parse("word^-5.05");
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
    fn nested_groups() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::and(vec![
                    Query::or(vec![
                        Term::new("hippo").into(),
                        Term::new("eggs").into(),
                        Term::new("quick").into(),
                        Term::new("brown").into(),
                    ]),
                    Query::or(vec![
                        Term::new("fox").into(),
                        Term::new("elephant").into(),
                        Term::new("peanut").into(),
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

    #[test]
    fn not() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::and(vec![
                    Query::not(Term::new("news").into()),
                    Term::new("eggs").into(),
                ]),
            ));
            let actual = parse("NOT news AND eggs");
            assert_eq!(expected, actual);
        })
    }

    #[cfg(feature = "phonetic")]
    #[test]
    fn phonetic() {
        run_test(|| {
            let expected = Ok(("", Phonetic::new("smith").into()));
            let actual = parse("~smith");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn term_and_near() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::and(vec![
                    Term::new("eggs").set_field("_id".to_owned()).into(),
                    Query::near("john smith", 3, false),
                ]),
            ));
            let actual = parse(r#"_id:eggs AND "john smith" WITHIN 3"#);
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn field_exists_boosted() {
        run_test(|| {
            let expected = Ok(("", Query::exists(Term::new("title")).set_boost(3.3)));
            let actual = parse("_exists_:title^3.3");
            assert_eq!(expected, actual);
        })
    }

    // The `es_docs_*` tests are all taken from the Elastic query string syntax docs at
    // https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-string-query.html#query-string-syntax

    #[test]
    fn es_docs_field_term() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Term(Term::new("active").set_field("status".into())),
            ));
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
                    Term::new("quick").set_field("title".into()).into(),
                    Term::new("brown").set_field("title".into()).into(),
                ]),
            ));
            let actual = parse("title:(quick OR brown)");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_field_term_many_or() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::or(vec![
                    Term::new("quick").set_field("title".into()).into(),
                    Term::new("brown").set_field("title".into()).into(),
                    Term::new("fox").set_field("title".into()).into(),
                    Term::new("elephant").set_field("title".into()).into(),
                ]),
            ));
            let actual = parse("title:(quick OR brown OR fox OR elephant)");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_field_phrase() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Phrase(Phrase::new("John Smith").set_field("author".into())),
            ));
            let actual = parse(r#"author:"John Smith""#);
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_field_escaped_space() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Term(Term::new("Alice").set_field("first name".into())),
            ));
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
                    Term::new("quick").set_field("book.*".into()).into(),
                    Term::new("brown").set_field("book.*".into()).into(),
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
                Query::Regex(Regex::new("joh?n(ath[oa]n)").set_field("name".into())),
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
                Query::Range(
                    Range::new("2012-01-01", true, "2012-12-31", true).set_field("date".into()),
                ),
            ));
            let actual = parse("date:[2012-01-01 TO 2012-12-31]");
            assert_eq!(expected, actual);
            let expected = Ok((
                "",
                Query::Range(
                    Range::new("1", true, "5", true)
                        .set_field("count".into())
                        .into(),
                ),
            ));
            let actual = parse("count:[1 TO 5]");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_exclusive() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Range(Range::new("alpha", false, "omega", false).set_field("tag".into())),
            ));
            let actual = parse("tag:{alpha TO omega}");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_inclusive_unbounded() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Range(Range::new("10", true, "*", true).set_field("count".into())),
            ));
            let actual = parse("count:[10 TO *]");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_exclusive_unbounded() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Range(Range::new("*", false, "2012-01-01", false).set_field("date".into())),
            ));
            let actual = parse("date:{* TO 2012-01-01}");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_mixed() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Range(Range::new("1", true, "5", false).set_field("count".into())),
            ));
            let actual = parse("count:[1 TO 5}");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_short_syntax_gt() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Range(Range::new("10", false, "*", true).set_field("age".into())),
            ));
            let actual = parse("age:>10");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_short_syntax_gte() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Range(Range::new("10", true, "*", true).set_field("age".into())),
            ));
            let actual = parse("age:>=10");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_short_syntax_lt() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Range(Range::new("*", true, "10", false).set_field("age".into())),
            ));
            let actual = parse("age:<10");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_short_syntax_lte() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Range(Range::new("*", true, "10", true).set_field("age".into())),
            ));
            let actual = parse("age:<=10");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_short_syntax_and() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Range(Range::new("10", true, "20", false).set_field("age".into())),
            ));
            let actual = parse("age:(>=10 AND <20)");
            assert_eq!(expected, actual);
        })
    }

    #[test]
    fn es_docs_range_short_syntax_plus() {
        run_test(|| {
            let expected = Ok((
                "",
                Query::Range(Range::new("10", true, "20", false).set_field("age".into())),
            ));
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
            // TODO: confirm which of these is correct
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
            // let expected = Ok((
            //     "",
            //     Query::and(vec![
            //         Query::or(vec![Term::new("quick").into(), Term::new("brown").into()]),
            //         Term::new("fox").into(),
            //         Query::not(Term::new("fox").into()),
            //     ]),
            // ));
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
                        Term::new("active").set_field("status".into()).into(),
                        Term::new("pending").set_field("status".into()).into(),
                    ]),
                    Query::or(vec![
                        Term::new("full")
                            .set_field("title".into())
                            .set_boost(2.0)
                            .into(),
                        Term::new("text")
                            .set_field("title".into())
                            .set_boost(2.0)
                            .into(),
                        Term::new("search")
                            .set_field("title".into())
                            .set_boost(2.0)
                            .into(),
                    ]),
                ]),
            ));
            let actual = parse("status:(active OR pending) title:(full text search)^2");
            assert_eq!(expected, actual);
        })
    }
}
