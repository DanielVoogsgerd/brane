//  IDENTIFIERS.rs
//    by Lut99
// 
//  Created:
//    04 Jul 2023, 11:31:39
//  Last edited:
//    04 Jul 2023, 11:35:08
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the [`IdentifierExpression`], which abstracts over all the
//!   ways a variable -or rather, a particular piece of memory- can be
//!   represented.
// 

use enum_debug::EnumDebug;

use super::spec::TextRange;


/***** LIBRARY *****/
/// Defines a recursive node that represents a variable reference of _some_ kind.
/// 
/// Specifically, it can be used to write- or read a particular area of memory, either a variable, a field in a class, a slot in an array...
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct IdentifierExpression {
    /// The specific kind of identifier
    pub kind  : IdentifierExpressionKind,
    /// The range relating this identifier to the source code.
    pub range : TextRange,
}

/// Defines the specifics of a particular kind of identifier expression.
#[derive(Clone, Copy, Debug, EnumDebug, Eq, PartialEq)]
pub enum IdentifierExpressionKind {
    
}
