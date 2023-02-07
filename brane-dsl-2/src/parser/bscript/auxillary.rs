//  AUXILLARY.rs
//    by Lut99
// 
//  Created:
//    07 Feb 2023, 19:02:01
//  Last edited:
//    07 Feb 2023, 19:24:32
//  Auto updated?
//    Yes
// 
//  Description:
//!   Provides functions for parsing BraneScript/Bakery AST auxillary
//!   structures, such as identifiers or data types.
// 

use nom::IResult;
use nom::combinator as comb;

use crate::ast::auxillary::{DataType, Identifier};
use crate::parser::{Error, Input};
use crate::parser::utils::tag_token;


/***** LIBRARY *****/
/// Attempts to parse an identifier off the top of the given tokenstream.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed identifier.
/// 
/// # Errors
/// This function errors if we failed to parse an identifier for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
#[inline]
pub fn parse_ident<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Identifier, E> {
    comb::map(tag_token!(Token::Identifier), |i| Identifier {
        name  : i.span().fragment().to_string(),
        range : Some(i.range()),
    })(input)
}

/// Attempts to parse a data type off the top of the given tokenstream.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed data type.
/// 
/// # Errors
/// This function errors if we failed to parse a data type for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
#[inline]
pub fn parse_type<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, DataType, E> {
    comb::map(tag_token!(Token::Identifier), |i| DataType {
        data_type : crate::ast::types::DataType::from(*i.span().fragment()),
        range     : Some(i.range()),
    })(input)
}
