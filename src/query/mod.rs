#[cfg(feature = "serde")]
use serde::Serialize;

#[cfg_attr(
    feature = "serde",
    derive(Serialize),
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
    #[cfg(feature = "phonetic")]
    Phonetic(Phonetic<'a>),
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

#[cfg(feature = "phonetic")]
impl<'a> From<Phonetic<'a>> for Query<'a> {
    fn from(phonetic: Phonetic<'a>) -> Self {
        Self::Phonetic(phonetic)
    }
}

impl<'a> Query<'a> {
    // TODO: remove/refactor somehow
    pub(crate) fn set_field(self, field: String) -> Self {
        match self {
            Query::Term(term) => Query::Term(term.set_field(field)),
            Query::Regex(regex) => Query::Regex(regex.set_field(field)),
            Query::Phrase(phrase) => Query::Phrase(phrase.set_field(field)),
            Query::Range(range) => Query::Range(range.set_field(field)),
            Query::Or(or) => or.set_field(field).into(),
            Query::And(and) => and.set_field(field).into(),
            // TODO: add all the other types
            _ => self,
        }
    }

    pub(crate) fn set_boost(self, boost: f64) -> Self {
        match self {
            Query::Or(or) => or.set_boost(boost).into(),
            Query::And(and) => and.set_boost(boost).into(),
            Query::Term(term) => term.set_boost(boost).into(),
            Query::Phrase(phrase) => phrase.set_boost(boost).into(),
            Query::Exists(exists) => exists.set_boost(boost).into(),
            // TODO: add all the other types
            _ => self,
        }
    }

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

    pub(crate) fn exists(term: Term<'a>) -> Self {
        Self::Exists(Exists::from(term))
    }
}

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Debug, Default, PartialEq)]
pub struct Term<'a> {
    term: &'a str,
    boost: Option<f64>,
    fuzziness: Option<u64>,
    field: Option<String>,
}

impl<'a> Term<'a> {
    pub(crate) fn new(term: &'a str) -> Self {
        Self {
            term,
            boost: None,
            ..Default::default()
        }
    }

    pub(crate) fn set_boost(self, boost: f64) -> Self {
        // TODO: must be positive
        Self {
            boost: Some(boost),
            ..self
        }
    }

    pub(crate) fn set_fuzziness(self, fuzziness: u64) -> Self {
        Self {
            fuzziness: Some(fuzziness),
            ..self
        }
    }

    pub(crate) fn set_field(self, field: String) -> Self {
        Self {
            field: Some(field),
            ..self
        }
    }

    pub fn term(&self) -> &str {
        self.term
    }

    pub fn boost(&self) -> Option<f64> {
        self.boost
    }

    pub fn fuzziness(&self) -> Option<u64> {
        self.fuzziness
    }
}

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Debug, PartialEq)]
pub struct Phrase<'a> {
    phrase: &'a str,
    boost: Option<f64>,
    field: Option<String>,
}

impl<'a> Phrase<'a> {
    pub(crate) fn new(phrase: &'a str) -> Self {
        Self {
            phrase,
            boost: None,
            field: None,
        }
    }

    pub(crate) fn set_field(self, field: String) -> Self {
        Self {
            field: Some(field),
            ..self
        }
    }

    pub(crate) fn set_boost(self, boost: f64) -> Self {
        // TODO: must be positive
        Self {
            boost: Some(boost),
            ..self
        }
    }
}

#[cfg_attr(feature = "serde", derive(Serialize))]
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
#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Debug, PartialEq)]
pub struct Or<'a> {
    queries: Vec<Query<'a>>,
}

impl<'a> Or<'a> {
    fn new(queries: Vec<Query<'a>>) -> Self {
        Self { queries }
    }

    pub(crate) fn push(self, query: Query<'a>) -> Self {
        let mut queries = self.queries;
        if let Query::Or(or) = query {
            queries.extend(or.queries)
        } else {
            queries.push(query);
        }
        Self { queries }
    }

    pub(crate) fn prepend(self, query: Query<'a>) -> Self {
        let mut queries = self.queries;
        queries.insert(0, query);
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

    fn set_field(self, field: String) -> Self {
        Self {
            queries: self
                .queries
                .into_iter()
                .map(|q| q.set_field(field.clone()))
                .collect(),
        }
    }
}

impl<'a> From<Vec<Query<'a>>> for Or<'a> {
    fn from(queries: Vec<Query<'a>>) -> Self {
        Self::new(queries)
    }
}

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Debug, PartialEq)]
pub struct And<'a> {
    queries: Vec<Query<'a>>,
}

impl<'a> And<'a> {
    fn new(queries: Vec<Query<'a>>) -> Self {
        Self { queries }
    }

    pub(crate) fn push(self, query: Query<'a>) -> Self {
        let mut queries = self.queries;
        if let Query::And(and) = query {
            queries.extend(and.queries)
        } else {
            queries.push(query);
        }
        Self { queries }
    }

    pub(crate) fn prepend(self, query: Query<'a>) -> Self {
        let mut queries = self.queries;
        queries.insert(0, query);
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

    fn set_field(self, field: String) -> Self {
        Self {
            queries: self
                .queries
                .into_iter()
                .map(|q| q.set_field(field.clone()))
                .collect(),
        }
    }
}

impl<'a> From<Vec<Query<'a>>> for And<'a> {
    fn from(queries: Vec<Query<'a>>) -> Self {
        Self::new(queries)
    }
}

#[cfg_attr(feature = "serde", derive(Serialize))]
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

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Debug, PartialEq)]
pub struct Exists<'a> {
    field: &'a str,
    boost: Option<f64>,
}

impl<'a> From<Term<'a>> for Exists<'a> {
    fn from(term: Term<'a>) -> Self {
        Self {
            field: term.term,
            boost: term.boost,
        }
    }
}

impl<'a> Exists<'a> {
    pub(crate) fn set_boost(self, boost: f64) -> Self {
        Self {
            boost: Some(boost),
            ..self
        }
    }
}

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Debug, PartialEq)]
pub struct Regex<'a> {
    regex: &'a str,
    field: Option<String>,
}

impl<'a> Regex<'a> {
    pub(crate) fn new(regex: &'a str) -> Self {
        Self { regex, field: None }
    }

    pub(crate) fn set_field(self, field: String) -> Self {
        Self {
            field: Some(field),
            ..self
        }
    }
}

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Debug, PartialEq)]
pub struct Range<'a> {
    field: Option<String>,
    from: &'a str,
    from_inclusive: bool,
    to: &'a str,
    to_inclusive: bool,
}

impl<'a> Range<'a> {
    pub(crate) fn new(
        from: &'a str,
        from_inclusive: bool,
        to: &'a str,
        to_inclusive: bool,
    ) -> Self {
        Self {
            field: None,
            from,
            from_inclusive,
            to,
            to_inclusive,
        }
    }

    pub(crate) fn set_field(self, field: String) -> Self {
        Self {
            field: Some(field),
            ..self
        }
    }
}

#[cfg(feature = "phonetic")]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Debug, PartialEq)]
pub struct Phonetic<'a> {
    field: Option<&'a str>,
    original: &'a str,
    metaphone: String,
    douple_metaphone: DoubleMetaphone,
    // soundex
    // refined_soundex
    // caverphone1
    // caverphone2
    // cologne
    // nysiis
    // koelnerphonetik
    // haasephonetik
    // beider_morse
    // daitch_mokotoff
}

#[cfg(feature = "phonetic")]
impl<'a> Phonetic<'a> {
    pub(crate) fn new(original: &'a str) -> Self {
        Self {
            field: None,
            original,
            metaphone: String::new(),
            douple_metaphone: DoubleMetaphone {
                primary: String::new(),
                secondary: None,
            },
        }
    }
}

#[cfg(feature = "phonetic")]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Debug, PartialEq)]
pub struct DoubleMetaphone {
    primary: String,
    secondary: Option<String>,
}
