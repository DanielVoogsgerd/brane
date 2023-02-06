//  MOD.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 16:30:02
//  Last edited:
//    06 Feb 2023, 17:43:25
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the scanner for BraneScript and Bakery, sharing wherever
//!   possible.
// 

// Declare submodules
pub(crate) mod tokens;
mod comments;
mod punctuation;
mod operators;
mod keywords;
mod literals;
mod identifiers;
mod scanner;


// Type aliases for this module
/// Defines the common input to every function.
pub type Input<'s> = nom_locate::LocatedSpan<&'s str, ()>;

// Trait aliases for this module
/// Defines a common error trait for the nom errors we are interested in.
pub trait Error<'s>: nom::error::ContextError<Input<'s>> + nom::error::ParseError<Input<'s>> {}
impl<'s, T> Error<'s> for T where T: nom::error::ContextError<Input<'s>> + nom::error::ParseError<Input<'s>> {}
