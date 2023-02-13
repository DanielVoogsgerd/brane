//  SPEC.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 15:36:16
//  Last edited:
//    13 Feb 2023, 11:42:32
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines non-AST things for the AST, such as debug structures
//!   (TextRange) or additional enums (MergeStrategy).
// 

use std::fmt::{Debug, Display, Formatter, Result as FResult};

use nom::AsBytes;
use nom_locate::LocatedSpan;
use num_traits::AsPrimitive;
use unicode_segmentation::UnicodeSegmentation as _;

use crate::warnings::{AnnotationWarning, WarningCode};
use super::expressions::Expression;
use super::statements::{RawAnnotation, RawAnnotationKind};


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
        let mut line : usize = span.location_line() as usize - 1;
        let mut col  : usize = span.get_column() - 1;
        for c in String::from_utf8_lossy(bs).graphemes(true).skip(1) {
            if c == "\n" {
                // Move to the next line
                line += 1;
                col   = 0;
            } else {
                // Normal character
                col += 1;
            }
        }

        // Use those to compute offsets for the lines and columns
        Self {
            line,
            col,
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



/// Provides the contents of each annotation. This defines the annotations that the compiler accepts, essentially.
#[derive(Clone, Debug)]
pub enum Annotation {
    /// Disables a specific warning message with the given identifier
    Allow(WarningCode),
    /// Scopes any external function occurring in the given statement to a specific location.
    On(Expression),
}

impl Annotation {
    /// Constructor for the Annotation that will create it from its parsed counterpart.
    /// 
    /// # Arguments
    /// - `annot`: The parsed RawAnnotation that we will create this Annotation from.
    /// 
    /// # Returns
    /// A tuple with a new list of `Annotation` instances that were contained within and a list of warnings that occurred while parsing the annotations.
    /// 
    /// Note that the annotation list may be empty, in which case the annotation did not make sense to us at all. This is then coupled with one or more warnings.
    pub fn from(value: RawAnnotation) -> (Vec<Self>, Vec<AnnotationWarning>) {
        // Parse the parsed annotation
        match value.kind {
            RawAnnotationKind::Identifier(ident) => {
                // There are no such annotations yet
                (vec![], vec![ AnnotationWarning::UnknownAnnotation{ raw: ident.name, range: value.range } ])
            },

            RawAnnotationKind::KeyValue(ident, value) => {
                // Match on the identifier to find what it is
                match ident.name.as_str() {
                    "on" => (vec![ Self::On(value) ], vec![]),

                    // The rest is unknown to us
                    _ => (vec![], vec![ AnnotationWarning::UnknownAnnotation{ raw: ident.name, range: value.range } ]),
                }
            },

            RawAnnotationKind::KeyList(ident, list) => {
                // Match on the identifier to find what it is, then process the list
                match ident.name.as_str() {
                    "allow" => {
                        // Assert it is a list of identifiers
                        let mut annots : Vec<Annotation>        = Vec::with_capacity(list.len());
                        let mut warns  : Vec<AnnotationWarning> = vec![];
                        for annot in list {
                            match annot.kind {
                                RawAnnotationKind::Identifier(ident) => match WarningCode::try_from(&ident.name) {
                                    Ok(code) => { annots.push(Annotation::Allow(code)); },
                                    Err(_)   => { warns.push(AnnotationWarning::UnknownWarningCode{ raw: ident.name, range: annot.range }); }
                                },

                                // The rest is illegal for a warning code
                                _ => { warns.push(AnnotationWarning::IllegalWarningCode{ range: annot.range }); },
                            }
                        }

                        // Return them
                        (annots, warns)
                    },

                    // The rest is unknown to us
                    _ => (vec![], vec![ AnnotationWarning::UnknownAnnotation{ raw: ident.name, range: value.range } ]),
                }
            },
        }
    }

    /// Constructor for the Annotation that will create it from a reference to its parsed counterpart.
    /// 
    /// # Arguments
    /// - `annot`: A reference to the parsed Annotation that we will create this Annotation from.
    /// 
    /// # Returns
    /// A tuple with a new list of `Annotation` instances that were contained within and a list of warnings that occurred while parsing the annotations.
    /// 
    /// Note that the annotation list may be empty, in which case the annotation did not make sense to us at all. This is then coupled with one or more warnings.
    #[inline]
    pub fn from_ref(value: &RawAnnotation) -> (Vec<Self>, Vec<AnnotationWarning>) {
        Self::from(value.clone())
    }
}

impl AsRef<Annotation> for Annotation {
    #[inline]
    fn as_ref(&self) -> &Self { self }
}
impl AsMut<Annotation> for Annotation {
    #[inline]
    fn as_mut(&mut self) -> &mut Self { self }
}
impl From<&Annotation> for Annotation {
    #[inline]
    fn from(value: &Annotation) -> Self { value.clone() }
}
impl From<&mut Annotation> for Annotation {
    #[inline]
    fn from(value: &mut Annotation) -> Self { value.clone() }
}





/// Provides a generilizations of AST nodes that allows it to get some common properties.
pub trait Node: Clone + Debug {
    /// Returns the internal TextRange of the node if it had any.
    fn range(&self) -> Option<TextRange>;
}
