//  SPEC.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 15:36:16
//  Last edited:
//    07 Feb 2023, 18:58:25
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines non-AST things for the AST, such as debug structures
//!   (TextRange) or additional enums (MergeStrategy).
// 

use std::fmt::{Debug, Display, Formatter, Result as FResult};

use enum_debug::EnumDebug;
use nom::AsBytes;
use nom_locate::LocatedSpan;
use num_traits::AsPrimitive;


/***** LIBRARY *****/
/// Defines a TextPos, which is a singular position within the source text.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TextPos {
    /// The line number of this position (i.e., the Y-coordinate). Stored as a zero-indexed number.
    pub line : usize,
    /// The column number of this position (i.e., the X-coordinate). Stored as a zero-indexed number.
    pub col  : usize,
}

impl TextPos {
    /// Constructor for the TextPos that takes a zero-indexed position.
    /// 
    /// # Arguments
    /// - `line`: The line number for this position (zero-indexed).
    /// - `col`: The column number for this position (zero-indexed).
    /// 
    /// # Returns
    /// A new TextPos instance that points to the given position.
    #[inline]
    pub fn new0(line: impl AsPrimitive<usize>, col: impl AsPrimitive<usize>) -> Self {
        Self {
            line : line.as_(),
            col  : col.as_(),
        }
    }
    /// Constructor for the TextPos that takes a one-indexed position.
    /// 
    /// # Arguments
    /// - `line`: The line number for this position (one-indexed).
    /// - `col`: The column number for this position (one-indexed).
    /// 
    /// # Returns
    /// A new TextPos instance that points to the given position.
    #[inline]
    pub fn new1(line: impl AsPrimitive<usize>, col: impl AsPrimitive<usize>) -> Self {
        Self {
            line : line.as_() - 1,
            col  : col.as_() - 1,
        }
    }

    /// Returns a TextPos that points to the beginning of the given LocatedSpan.
    /// 
    /// # Arguments
    /// - `span`: The Span to take the start position of.
    /// 
    /// # Returns
    /// A new TextPos that points to the start of the given span.
    #[inline]
    pub fn start_of<T: AsBytes, X>(span: &LocatedSpan<T, X>) -> Self {
        Self {
            line : span.location_line() as usize - 1,
            col  : span.get_column() - 1,
        }
    }

    /// Returns a TextPos that points to the end of the given LocatedSpan, inclusive.
    /// 
    /// # Arguments
    /// - `span`: The Span to take the end position of.
    /// 
    /// # Returns
    /// A new TextPos that points to the end of the given span, inclusive.
    #[inline]
    pub fn end_of<T: AsBytes, X>(span: &LocatedSpan<T, X>) -> Self {
        // Get the bytes of the Span's type.
        let bs: &[u8] = span.fragment().as_bytes();

        // Get the position of the last newline and count them while at it
        let mut n_nls   : usize         = 0;
        let mut last_nl : Option<usize> = None;
        for (i, b) in bs.iter().enumerate() {
            if *b == b'\n' {
                n_nls   += 1;
                last_nl  = Some(i);
            }
        }

        // Use those to compute offsets for the lines and columns
        Self {
            line : span.location_line() as usize - 1 + n_nls,
            col  : if let Some(last_nl) = last_nl { if bs.len() >= 2 + last_nl { bs.len() - 2 - last_nl } else { 0 } } else if !bs.is_empty() { bs.len() - 1 } else { 0 },
        }
    }



    /// Sets the internal position as a zero-indexed value.
    /// 
    /// # Arguments
    /// - `line`: The new zero-indexed value for the line position.
    /// - `col`: The new zero-indexed value for the column position.
    #[inline]
    pub fn set0(&mut self, line: impl AsPrimitive<usize>, col: impl AsPrimitive<usize>) { self.line = line.as_(); self.col = col.as_(); }
    /// Sets the internal line as a zero-indexed value.
    /// 
    /// As an alternative to this function, you can also simply change the fields in this TextPos directly.
    /// 
    /// # Arguments
    /// - `value`: The new zero-indexed value for the line position.
    #[inline]
    pub fn set_line0(&mut self, value: impl AsPrimitive<usize>) { self.line = value.as_(); }
    /// Sets the internal column as a zero-indexed value.
    /// 
    /// As an alternative to this function, you can also simply change the fields in this TextPos directly.
    /// 
    /// # Arguments
    /// - `value`: The new zero-indexed value for the column position.
    #[inline]
    pub fn set_col0(&mut self, value: impl AsPrimitive<usize>) { self.col = value.as_(); }

    /// Sets the internal position as a one-indexed value.
    /// 
    /// # Arguments
    /// - `line`: The new one-indexed value for the line position.
    /// - `col`: The new one-indexed value for the column position.
    /// 
    /// # Panics
    /// This function panics if the given `line` or `col` are `0`.
    #[inline]
    pub fn set1(&mut self, line: impl AsPrimitive<usize>, col: impl AsPrimitive<usize>) { self.line = line.as_() - 1; self.col = col.as_() - 1; }
    /// Sets the internal line as a one-indexed value.
    /// 
    /// # Arguments
    /// - `value`: The new one-indexed value for the line position.
    /// 
    /// # Panics
    /// This function panics if the given `value` is `0`.
    #[inline]
    pub fn set_line1(&mut self, value: impl AsPrimitive<usize>) { self.line = value.as_() - 1; }
    /// Sets the internal column as a one-indexed value.
    /// 
    /// # Arguments
    /// - `value`: The new one-indexed value for the column position.
    /// 
    /// # Panics
    /// This function panics if the given `value` is `0`.
    #[inline]
    pub fn set_col1(&mut self, value: impl AsPrimitive<usize>) { self.col = value.as_() - 1; }



    /// Returns the internal line as a zero-indexed value.
    /// 
    /// Note that this is equivalent to directly reading the internal `line`-field.
    #[inline]
    pub const fn line0(&self) -> usize { self.line }
    /// Returns the internal column as a zero-indexed value.
    /// 
    /// Note that this is equivalent to directly reading the internal `col`-field.
    #[inline]
    pub const fn col0(&self) -> usize { self.col }

    /// Returns the internal line as a one-indexed value.
    #[inline]
    pub const fn line1(&self) -> usize { self.line + 1 }
    /// Returns the internal column as a one-indexed value.
    #[inline]
    pub const fn col1(&self) -> usize { self.col + 1 }
}

impl Display for TextPos {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

impl AsRef<TextPos> for TextPos {
    #[inline]
    fn as_ref(&self) -> &Self { self }
}
impl AsMut<TextPos> for TextPos {
    #[inline]
    fn as_mut(&mut self) -> &mut Self { self }
}
impl From<&TextPos> for TextPos {
    #[inline]
    fn from(value: &TextPos) -> Self { *value }
}
impl From<&mut TextPos> for TextPos {
    #[inline]
    fn from(value: &mut TextPos) -> Self { *value }
}

impl<T: AsPrimitive<usize>, U: AsPrimitive<usize>> From<(T, U)> for TextPos {
    #[inline]
    fn from(value: (T, U)) -> Self { Self::new0(value.0, value.1) }
}
impl<T: AsPrimitive<usize>, U: AsPrimitive<usize>> From<&(T, U)> for TextPos {
    #[inline]
    fn from(value: &(T, U)) -> Self { Self::new0(value.0, value.1) }
}
impl<T: AsPrimitive<usize>, U: AsPrimitive<usize>> From<&mut (T, U)> for TextPos {
    #[inline]
    fn from(value: &mut (T, U)) -> Self { Self::new0(value.0, value.1) }
}



/// Defines TextRange, which is a continious range within the source text.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TextRange {
    /// The start of the range, inclusive.
    pub start : TextPos,
    /// The end of the range, inclusive.
    pub end   : TextPos,
}

impl TextRange {
    /// Constructor for the TextRange.
    /// 
    /// # Arguments
    /// - `start`: The start of the range, inclusive.
    /// - `stop`: the end of the range, inclusive.
    /// 
    /// # Returns
    /// A new TextRange that represents the range between the given positions.
    #[inline]
    pub fn new(start: impl Into<TextPos>, end: impl Into<TextPos>) -> Self {
        Self {
            start : start.into(),
            end   : end.into(),
        }
    }
}

impl Display for TextRange {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "{}-{}", self.start, self.end)
    }
}

impl AsRef<TextRange> for TextRange {
    #[inline]
    fn as_ref(&self) -> &Self { self }
}
impl AsMut<TextRange> for TextRange {
    #[inline]
    fn as_mut(&mut self) -> &mut Self { self }
}
impl From<&TextRange> for TextRange {
    #[inline]
    fn from(value: &TextRange) -> Self { *value }
}
impl From<&mut TextRange> for TextRange {
    #[inline]
    fn from(value: &mut TextRange) -> Self { *value }
}

impl<T: AsBytes, X> From<LocatedSpan<T, X>> for TextRange {
    #[inline]
    fn from(value: LocatedSpan<T, X>) -> Self {
        Self {
            start : TextPos::start_of(&value),
            end   : TextPos::end_of(&value),
        }
    }
}
impl<T: AsBytes, X> From<&LocatedSpan<T, X>> for TextRange {
    #[inline]
    fn from(value: &LocatedSpan<T, X>) -> Self {
        Self {
            start : TextPos::start_of(value),
            end   : TextPos::end_of(value),
        }
    }
}
impl<T: AsBytes, X> From<&mut LocatedSpan<T, X>> for TextRange {
    #[inline]
    fn from(value: &mut LocatedSpan<T, X>) -> Self {
        Self {
            start : TextPos::start_of(value),
            end   : TextPos::end_of(value),
        }
    }
}



/// Defines the binding power that operators may have.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct BindingPower {
    /// The binding power on the lefthand-side of the operator. `None` for unary expressions that only bind to the right.
    pub left  : Option<u32>,
    /// The binding power on the righthand-side of the operator. `None` for unary expressions that only bind to the left.
    pub right : Option<u32>,
}
impl BindingPower {
    /// Constructor for the BindingPower that initializes it to "no power set".
    /// 
    /// # Returns
    /// A new BindingPower instance with both fields set to `None`.
    #[inline]
    pub const fn none() -> Self {
        Self {
            left  : None,
            right : None,
        }
    }
}

/// Defines merge strategies for the parallel statements.
#[derive(Clone, Copy, Debug, EnumDebug, Eq, PartialEq, Hash)]
pub enum MergeStrategy {
    /// Take the value that arrived first. The statement will already return as soon as this statement is in, not the rest.
    First,
    /// Take the value that arrived first. The statement will still block until all values returned.
    FirstBlocking,
    /// Take the value that arrived last.
    Last,

    /// Add all the resulting values together. This means that they must all be numeric.
    Sum,
    /// Multiple all the resulting values together. This means that they must all be numeric.
    Product,

    /// Take the largest value. Use on booleans to get an 'OR'-effect (i.e., it returns true iff there is at least one true).
    Max,
    /// Take the smallest value. Use on booleans to get an 'AND'-effect (i.e., it returns false iff there is at least one false).
    Min,

    /// Returns all values as an Array.
    All,

    /// No merge strategy needed/defined
    None,
}

impl From<String> for MergeStrategy {
    #[inline]
    fn from(value: String) -> Self {
        Self::from(value.as_str())
    }
}
impl From<&String> for MergeStrategy {
    #[inline]
    fn from(value: &String) -> Self {
        Self::from(value.as_str())
    }
}
impl From<&str> for MergeStrategy {
    #[inline]
    fn from(value: &str) -> Self {
        match value.to_lowercase().as_str() {
            "first"  => Self::First,
            "first*" => Self::FirstBlocking,
            "last"   => Self::Last,

            "+" | "sum"     => Self::Sum,
            "*" | "product" => Self::Product,

            "max" => Self::Max,
            "min" => Self::Min,

            "all" => Self::All,

            _ => Self::None,
        }
    }
}



/// Provides a generilizations of AST nodes that allows it to get some common properties.
pub trait Node: Clone + Debug {
    /// Returns the internal TextRange of the node if it had any.
    fn range(&self) -> Option<TextRange>;
}
