//  MOD.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 16:29:43
//  Last edited:
//    10 Feb 2023, 11:40:03
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
mod parser;

// Bring some part of it into the this module's scope
pub(crate) use parser::parse_tokens;


// Type aliases for this module
/// Defines the common input to every function.
pub(crate) type Input<'t, 's> = &'t [crate::scanner::Token<'s>];
/// Defines the common error returning from every function.
pub(crate) type Error<'t, 's> = crate::errors::NomError<'s, Input<'t, 's>>;

// // Trait aliases for this module
// /// Defines a common error trait for the nom errors we are interested in.
// pub(crate) trait Error<'t, 's: 't>: nom::error::ContextError<Input<'t, 's>> + nom::error::FromExternalError<Input<'t, 's>, crate::errors::ParseError<'s>> + nom::error::ParseError<Input<'t, 's>> {}
// impl<'t, 's: 't, T> Error<'t, 's> for T where T: nom::error::ContextError<Input<'t, 's>> + nom::error::FromExternalError<Input<'t, 's>, crate::errors::ParseError<'s>> + nom::error::ParseError<Input<'t, 's>> {}
