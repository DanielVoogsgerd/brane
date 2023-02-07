//  MOD.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 16:29:43
//  Last edited:
//    07 Feb 2023, 13:09:35
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the parser for BraneScript and Bakery, sharing wherever
//!   possible.
// 

// Declare submodules
mod utils;
mod bscript;


// Type aliases for this module
/// Defines the common input to every function.
pub(crate) type Input<'t, 's: 't> = &'t [crate::scanner::Token<'s>];

// Trait aliases for this module
/// Defines a common error trait for the nom errors we are interested in.
pub(crate) trait Error<'t, 's: 't>: nom::error::ContextError<Input<'t, 's>> + nom::error::ParseError<Input<'t, 's>> {}
impl<'t, 's: 't, T> Error<'t, 's> for T where T: nom::error::ContextError<Input<'t, 's>> + nom::error::ParseError<Input<'t, 's>> {}
