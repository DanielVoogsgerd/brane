//  AUXILLARY.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 15:35:30
//  Last edited:
//    09 Feb 2023, 15:04:28
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines auxillary AST nodes such as identifiers or property
//!   definitions.
// 

use enum_debug::EnumDebug;

use super::spec::{Node, TextRange};
use super::types;
use super::expressions::Expression;


/***** LIBRARY *****/
/// An annotation is an additional bit of information that the compiler can use to do stuff.
#[derive(Clone, Debug)]
pub struct Annotation {
    /// The specific form of annotation
    pub kind  : AnnotationKind,
    /// The range where we found the annotation
    pub range : Option<TextRange>,
}
impl Node for Annotation {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}

/// Specifies the allows annotation variants.
#[derive(Clone, Debug, EnumDebug)]
pub enum AnnotationKind {
    /// It's a separate identifier.
    Identifier(Identifier),
    /// It's a key/value pair.
    KeyValue(Identifier, Expression),
    /// It's a key/list pair.
    KeyList(Identifier, Vec<Annotation>),
}



/// An identifier is a simply, well, identifier that the user has defined.
#[derive(Clone, Debug)]
pub struct Identifier {
    /// The name given to this identifier.
    pub name  : String,
    /// The range in the source text for this identifier.
    pub range : Option<TextRange>,
}
impl Node for Identifier {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}



/// A DataType is a DataType with a range attached to it.
#[derive(Clone, Debug)]
pub struct DataType {
    /// The data type specified.
    pub data_type : types::DataType,
    /// The range in the source text for this type.
    pub range     : Option<TextRange>,
}
impl DataType {
    /// Constructor for the DataType that initializes it as a `DataType::Any` that has no link to the source text.
    /// 
    /// # Returns
    /// A new `DataType` that represents an undetermined type.
    #[inline]
    pub const fn any() -> Self {
        Self {
            data_type : types::DataType::Any,
            range     : None,
        }
    }
}
impl Node for DataType {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}



/// Defines merge strategies for the parallel statements. These are kind of identifiers but only a limited set; think of it as very scoped keywords.
#[derive(Clone, Copy, Debug)]
pub struct MergeStrategy {
    /// Defines the specific variant, i.e., the strategy.
    pub kind  : MergeStrategyKind,
    /// Defines the range we parsed it from.
    pub range : Option<TextRange>,
}
impl Node for MergeStrategy {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}

/// Determines the possible merge strategy variants.
#[derive(Clone, Copy, Debug, EnumDebug)]
pub enum MergeStrategyKind {
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
}
