use nom::{InputIter, InputLength, InputTake, Needed, Slice};
use std::iter::Enumerate;
use std::ops::{Range, RangeFrom, RangeFull, RangeTo};
use std::str::FromStr;

type Span<'a> = nom_locate::LocatedSpan<&'a str>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token<'a> {
    /// `&`
    And(Span<'a>),

    /// '@'
    At(Span<'a>),

    /// `break`
    Break(Span<'a>),

    /// `class`
    Class(Span<'a>),

    /// `continue`
    Continue(Span<'a>),

    /// `else`
    Else(Span<'a>),

    /// `for`
    For(Span<'a>),

    /// `in`
    In(Span<'a>),

    /// `to`
    To(Span<'a>),

    /// `step`
    Step(Span<'a>),

    /// `func`
    Function(Span<'a>),

    /// `if`
    If(Span<'a>),

    /// `import`
    Import(Span<'a>),

    /// `let`
    Let(Span<'a>),

    /// `new`
    New(Span<'a>),

    /// `on`
    On(Span<'a>),

    /// `|`
    Or(Span<'a>),

    /// `parallel`
    Parallel(Span<'a>),

    /// `return`
    Return(Span<'a>),

    /// `unit`
    Unit(Span<'a>),

    /// `while`
    While(Span<'a>),

    /// .
    Dot(Span<'a>),

    /// :
    Colon(Span<'a>),

    /// ,
    Comma(Span<'a>),

    /// {
    LeftBrace(Span<'a>),

    /// [
    LeftBracket(Span<'a>),

    /// (
    LeftParen(Span<'a>),

    /// }
    RightBrace(Span<'a>),

    /// ]
    RightBracket(Span<'a>),

    /// )
    RightParen(Span<'a>),

    /// ;
    Semicolon(Span<'a>),

    /// :=
    Assign(Span<'a>),

    /// =
    Equal(Span<'a>),

    /// >
    Greater(Span<'a>),

    /// >=
    GreaterOrEqual(Span<'a>),

    /// <
    Less(Span<'a>),

    /// <=
    LessOrEqual(Span<'a>),

    /// -
    Minus(Span<'a>),

    /// !
    Not(Span<'a>),

    /// !=
    NotEqual(Span<'a>),

    /// +
    Plus(Span<'a>),

    /// /
    Slash(Span<'a>),

    /// *
    Star(Span<'a>),

    /// %
    Percentage(Span<'a>),

    /// Null literal
    Null(Span<'a>),

    /// Boolean literal
    Boolean(Span<'a>),

    /// Integer literal
    Integer(Span<'a>),

    /// Real literal
    Real(Span<'a>),

    /// SemVer literal
    SemVer(Span<'a>),

    /// String literal
    String(Span<'a>),

    /// Identifier
    Ident(Span<'a>),

    /// None
    None,
}

impl<'a> Token<'a> {
    pub fn as_bool(&self) -> bool {
        if let Token::Boolean(span) = self {
            bool::from_str(&span.to_string()).unwrap()
        } else {
            unreachable!()
        }
    }

    pub fn as_i64(&self) -> i64 {
        if let Token::Integer(span) = self {
            // Replace the '_' first
            let raw: String = span.to_string().replace('_', "");
            // Now parse the integer
            i64::from_str(&raw).unwrap()
        } else {
            unreachable!()
        }
    }

    pub fn as_f64(&self) -> f64 {
        if let Token::Real(span) = self {
            f64::from_str(&span.to_string()).unwrap()
        } else {
            unreachable!()
        }
    }

    pub fn as_string(&self) -> String {
        match &self {
            Token::String(span) | Token::Ident(span) | Token::SemVer(span) => span.to_string(),
            _ => unreachable!(),
        }
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Token::None)
    }

    pub fn inner(&self) -> &Span {
        use Token::*;

        match self {
            At(span) | And(span) | Break(span) | Class(span) | Continue(span) | Else(span) | For(span) | In(span) | To(span) | Step(span) | Function(span)
            | If(span) | Import(span) | Let(span) | On(span) | Or(span) | Return(span) | Unit(span) | While(span)
            | Dot(span) | Colon(span) | Comma(span) | LeftBrace(span) | LeftBracket(span) | LeftParen(span)
            | Parallel(span) | RightBrace(span) | RightBracket(span) | RightParen(span) | Semicolon(span)
            | Assign(span) | Equal(span) | Greater(span) | GreaterOrEqual(span) | Less(span) | LessOrEqual(span)
            | Minus(span) | Not(span) | NotEqual(span) | Plus(span) | Slash(span) | Star(span) | Percentage(span)
            | Null(span) | Boolean(span) | Integer(span) | Real(span) | SemVer(span) | String(span) | Ident(span)
            | New(span) => span,
            // None should have been filtered out already.
            None => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Tokens<'a> {
    pub tok: &'a [Token<'a>],
    pub start: usize,
    pub end: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(vec: &'a [Token]) -> Self {
        Tokens {
            tok: vec,
            start: 0,
            end: vec.len(),
        }
    }
}

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tok.len()
    }
}

impl<'a> InputTake for Tokens<'a> {
    #[inline]
    fn take(
        &self,
        count: usize,
    ) -> Self {
        Tokens {
            tok: &self.tok[0..count],
            start: 0,
            end: count,
        }
    }

    #[inline]
    fn take_split(
        &self,
        count: usize,
    ) -> (Self, Self) {
        let (prefix, suffix) = self.tok.split_at(count);
        let first = Tokens {
            tok: prefix,
            start: 0,
            end: prefix.len(),
        };
        let second = Tokens {
            tok: suffix,
            start: 0,
            end: suffix.len(),
        };
        (second, first)
    }
}

impl<'a> InputLength for Token<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a> Slice<Range<usize>> for Tokens<'a> {
    #[inline]
    fn slice(
        &self,
        range: Range<usize>,
    ) -> Self {
        Tokens {
            tok: self.tok.slice(range.clone()),
            start: self.start + range.start,
            end: self.start + range.end,
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Tokens<'a> {
    #[inline]
    fn slice(
        &self,
        range: RangeTo<usize>,
    ) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> Slice<RangeFrom<usize>> for Tokens<'a> {
    #[inline]
    fn slice(
        &self,
        range: RangeFrom<usize>,
    ) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a> Slice<RangeFull> for Tokens<'a> {
    #[inline]
    fn slice(
        &self,
        _: RangeFull,
    ) -> Self {
        Tokens {
            tok: self.tok,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a Token<'a>;
    type Iter = Enumerate<::std::slice::Iter<'a, Token<'a>>>;
    type IterElem = ::std::slice::Iter<'a, Token<'a>>;

    #[inline]
    fn iter_indices(&self) -> Enumerate<::std::slice::Iter<'a, Token<'a>>> {
        self.tok.iter().enumerate()
    }

    #[inline]
    fn iter_elements(&self) -> ::std::slice::Iter<'a, Token<'a>> {
        self.tok.iter()
    }

    #[inline]
    fn position<P>(
        &self,
        predicate: P,
    ) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tok.iter().position(predicate)
    }

    #[inline]
    fn slice_index(
        &self,
        count: usize,
    ) -> Result<usize, Needed> {
        if self.tok.len() >= count {
            Ok(count)
        } else {
            Err(Needed::new(count - self.tok.len()))
        }
    }
}
