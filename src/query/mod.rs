#[cfg(any(feature = "serde"))]
use serde::Serialize;

const DEFAULT_BOOST: f64 = 1.0;

#[cfg_attr(
    feature = "serde",
    derive("Serialize"),
    serde(tag = "type", rename_all = "snake_case")
)]
#[derive(Debug, PartialEq)]
pub enum Query<'a> {
    Term(Term<'a>),
    Phrase(Phrase<'a>),
    Near(Near<'a>),
    Or(Or<'a>),
    And(And<'a>),
    Not(Not<'a>),
    Exists(Exists<'a>),
    Regex(Regex<'a>),
    Range(Range<'a>),
    // Phonetic(Phonetic<'a>),
}

impl<'a> From<Term<'a>> for Query<'a> {
    fn from(term: Term<'a>) -> Self {
        Self::Term(term)
    }
}

impl<'a> From<Phrase<'a>> for Query<'a> {
    fn from(phrase: Phrase<'a>) -> Self {
        Self::Phrase(phrase)
    }
}

impl<'a> From<Near<'a>> for Query<'a> {
    fn from(near: Near<'a>) -> Self {
        Self::Near(near)
    }
}

impl<'a> From<Or<'a>> for Query<'a> {
    fn from(or: Or<'a>) -> Self {
        Self::Or(or)
    }
}

impl<'a> From<And<'a>> for Query<'a> {
    fn from(and: And<'a>) -> Self {
        Self::And(and)
    }
}

impl<'a> From<Not<'a>> for Query<'a> {
    fn from(not: Not<'a>) -> Self {
        Self::Not(not)
    }
}

impl<'a> From<Exists<'a>> for Query<'a> {
    fn from(exists: Exists<'a>) -> Self {
        Self::Exists(exists)
    }
}

impl<'a> From<Regex<'a>> for Query<'a> {
    fn from(regex: Regex<'a>) -> Self {
        Self::Regex(regex)
    }
}

impl<'a> From<Range<'a>> for Query<'a> {
    fn from(range: Range<'a>) -> Self {
        Self::Range(range)
    }
}

impl<'a> Query<'a> {
    // TODO: remove/refactor somehow
    pub(crate) fn set_field(self, field: &'a str) -> Self {
        match self {
            Query::Term(term) => Query::Term(term.set_field(field)),
            Query::Regex(regex) => Query::Regex(regex.set_field(field)),
            Query::Phrase(phrase) => Query::Phrase(phrase.set_field(field)),
            // or
            // and
            _ => self,
        }
    }

    pub(crate) fn set_boost(self, boost: f64) -> Self {
        match self {
            Query::Or(or) => or.set_boost(boost).into(),
            Query::And(and) => and.set_boost(boost).into(),
            Query::Term(term) => term.set_boost(boost).into(),
            // TODO: add all the other types
            _ => self,
        }
    }

    // Term(Term<'a>),
    // Phrase(Phrase<'a>),
    // Near(Near<'a>),
    // Regex(Regex<'a>),
    // Range(Range<'a>),
    pub(crate) fn near(phrase: &'a str, distance: u64, ordered: bool) -> Self {
        Self::Near(Near::new(phrase, distance, ordered))
    }

    pub(crate) fn or(queries: Vec<Query<'a>>) -> Self {
        Self::Or(Or::new(queries))
    }

    pub(crate) fn and(queries: Vec<Query<'a>>) -> Self {
        Self::And(And::new(queries))
    }

    pub(crate) fn not(query: Query<'a>) -> Self {
        Self::Not(Not::new(query))
    }

    pub(crate) fn exists(field: &'a str) -> Self {
        Self::Exists(Exists::new(field))
    }
}

#[cfg_attr(feature = "json", derive)]
#[derive(Debug, Default, PartialEq)]
pub struct Term<'a> {
    term: &'a str,
    boost: f64,
    fuzziness: u64,
    field: Option<&'a str>,
}

impl<'a> Term<'a> {
    pub(crate) fn new(term: &'a str) -> Self {
        Self {
            term,
            boost: DEFAULT_BOOST,
            ..Default::default()
        }
    }

    pub(crate) fn set_boost(self, boost: f64) -> Self {
        // TODO: must be positive
        Self { boost, ..self }
    }

    pub(crate) fn set_fuzziness(self, fuzziness: u64) -> Self {
        Self { fuzziness, ..self }
    }

    pub(crate) fn set_field(self, field: &'a str) -> Self {
        Self {
            field: Some(field),
            ..self
        }
    }

    pub fn term(&self) -> &str {
        self.term
    }

    pub fn boost(&self) -> f64 {
        self.boost
    }

    pub fn fuzziness(&self) -> u64 {
        self.fuzziness
    }
}

#[cfg_attr(feature = "json", derive)]
#[derive(Debug, PartialEq)]
pub struct Phrase<'a> {
    phrase: &'a str,
    boost: f64,
    field: Option<&'a str>,
}

impl<'a> Phrase<'a> {
    pub(crate) fn new(phrase: &'a str) -> Self {
        Self {
            phrase,
            boost: DEFAULT_BOOST,
            field: None,
        }
    }

    pub(crate) fn set_field(self, field: &'a str) -> Self {
        Self {
            field: Some(field),
            ..self
        }
    }

    pub(crate) fn set_boost(self, boost: f64) -> Self {
        // TODO: must be positive
        Self { boost, ..self }
    }
}

#[cfg_attr(feature = "json", derive)]
#[derive(Debug, PartialEq)]
pub struct Near<'a> {
    phrase: &'a str,
    ordered: bool,
    distance: u64,
}

impl<'a> Near<'a> {
    fn new(phrase: &'a str, distance: u64, ordered: bool) -> Self {
        Self {
            phrase,
            distance,
            ordered,
        }
    }
}
#[cfg_attr(feature = "json", derive)]
#[derive(Debug, PartialEq)]
pub struct Or<'a> {
    queries: Vec<Query<'a>>,
}

impl<'a> Or<'a> {
    fn new(queries: Vec<Query<'a>>) -> Self {
        Self { queries }
    }

    fn set_boost(self, boost: f64) -> Self {
        Self {
            queries: self
                .queries
                .into_iter()
                .map(|q| q.set_boost(boost))
                .collect(),
        }
    }
}

impl<'a> From<Vec<Query<'a>>> for Or<'a> {
    fn from(queries: Vec<Query<'a>>) -> Self {
        Self::new(queries)
    }
}

#[cfg_attr(feature = "json", derive)]
#[derive(Debug, PartialEq)]
pub struct And<'a> {
    queries: Vec<Query<'a>>,
}

impl<'a> And<'a> {
    fn new(queries: Vec<Query<'a>>) -> Self {
        Self { queries }
    }

    fn set_boost(self, boost: f64) -> Self {
        Self {
            queries: self
                .queries
                .into_iter()
                .map(|q| q.set_boost(boost))
                .collect(),
        }
    }
}

impl<'a> From<Vec<Query<'a>>> for And<'a> {
    fn from(queries: Vec<Query<'a>>) -> Self {
        Self::new(queries)
    }
}

#[cfg_attr(feature = "json", derive)]
#[derive(Debug, PartialEq)]
pub struct Not<'a> {
    query: Box<Query<'a>>,
}

impl<'a> Not<'a> {
    fn new(query: Query<'a>) -> Self {
        Self {
            query: Box::new(query),
        }
    }
}

#[cfg_attr(feature = "json", derive)]
#[derive(Debug, PartialEq)]
pub struct Exists<'a> {
    field: &'a str,
}

impl<'a> Exists<'a> {
    fn new(field: &'a str) -> Self {
        Self { field }
    }
}

#[cfg_attr(feature = "json", derive)]
#[derive(Debug, PartialEq)]
pub struct Regex<'a> {
    regex: &'a str,
    field: Option<&'a str>,
}

impl<'a> Regex<'a> {
    pub(crate) fn new(regex: &'a str) -> Self {
        Self { regex, field: None }
    }

    pub(crate) fn set_field(self, field: &'a str) -> Self {
        Self {
            field: Some(field),
            ..self
        }
    }
}

#[cfg_attr(feature = "json", derive)]
#[derive(Debug, PartialEq)]
pub struct Range<'a> {
    field: &'a str,
    from: &'a str,
    from_inclusive: bool,
    to: &'a str,
    to_inclusive: bool,
}

impl<'a> Range<'a> {
    pub(crate) fn new(
        field: &'a str,
        from: &'a str,
        from_inclusive: bool,
        to: &'a str,
        to_inclusive: bool,
    ) -> Self {
        Self {
            field,
            from,
            from_inclusive,
            to,
            to_inclusive,
        }
    }
}

// #[cfg_attr(feature = "json", derive)]
// #[derive(Debug, PartialEq)]
// pub struct Phonetic<'a> {
//     field: &'a str,
//     original: &'a str,
//     metaphone: String,
//     douple_metaphone: DoubleMetaphone,
//     // soundex
//     // refined_soundex
//     // caverphone1
//     // caverphone2
//     // cologne
//     // nysiis
//     // koelnerphonetik
//     // haasephonetik
//     // beider_morse
//     // daitch_mokotoff
// }
//
// impl<'a> Phonetic<'a> {
//     fn new() -> Self {
//         todo!()
//     }
// }
//
// #[cfg_attr(feature = "json", derive)]
// #[derive(Debug, PartialEq)]
// pub struct DoubleMetaphone {
//     primary: String,
//     secondary: Option<String>,
// }
