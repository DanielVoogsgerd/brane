//  MOD.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 16:30:02
//  Last edited:
//    10 Feb 2023, 09:07:27
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
mod auxillary;
mod scanner;

// Bring some part of it into the this module's scope
pub(crate) use tokens::Token;
pub(crate) use scanner::scan_tokens;


// Type aliases for this module
/// Defines the common input to every function.
pub(crate) type Input<'s> = nom_locate::LocatedSpan<&'s str, ()>;

// Trait aliases for this module
/// Defines a common error trait for the nom errors we are interested in.
pub(crate) trait Error<'s>: nom::error::ContextError<Input<'s>> + nom::error::ParseError<Input<'s>> {}
impl<'s, T> Error<'s> for T where T: nom::error::ContextError<Input<'s>> + nom::error::ParseError<Input<'s>> {}
