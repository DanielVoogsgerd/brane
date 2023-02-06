//  AUXILLARY.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 15:35:30
//  Last edited:
//    06 Feb 2023, 16:24:30
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines auxillary AST nodes such as identifiers or property
//!   definitions.
// 

use super::spec::{Node, TextRange};
use super::types;


/***** LIBRARY *****/
/// An identifier is a simply, well, identifier that the user has defined.
#[derive(Clone, Debug)]
pub struct Identifier {
    /// The name given to this identifier.
    pub name  : String,
    /// The range in the source text for this identifier.
    pub range : TextRange,
}
impl Node for Identifier {
    #[inline]
    fn range(&self) -> TextRange { self.range }
}



/// A DataType is a DataType with a range attached to it.
#[derive(Clone, Debug)]
pub struct DataType {
    /// The data type specified.
    pub data_type : types::DataType,
    /// The range in the source text for this type.
    pub range     : TextRange,
}
impl Node for DataType {
    #[inline]
    fn range(&self) -> TextRange { self.range }
}
