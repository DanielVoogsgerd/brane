//  AUXILLARY.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 17:25:02
//  Last edited:
//    10 Feb 2023, 09:08:37
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines functions for parsing identifiers.
// 

use nom::IResult;
use nom::{branch, bytes::complete as bc, character::complete as cc, combinator as comb, multi, sequence as seq};

use super::{Error, Input};
use super::tokens::Token;


/***** LIBRARY *****/
/// Attempts to parse an identifier from the head of the given input.
/// 
/// # Arguments
/// - `input`: The input to parse off of.
/// 
/// # Returns
/// A tuple of the remaining input we did not parse and a token representing the parsed identifier.
/// 
/// # Errors
/// This function errors if we failed to parse an identifier for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn parse_ident<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, Token, E> {
    nom::error::context("an identifier", comb::map(
        comb::recognize(seq::pair(
            branch::alt((cc::alpha1, bc::tag("_"))),
            multi::many0(branch::alt((cc::alphanumeric1, bc::tag("_")))),
        )),
        Token::Identifier,
    ))(input)
}



/// Attempts to parse a data type from the head of the given input.
/// 
/// This function is a bit awkward due to conflicts with the identifier in the case of types like `int`, `string`, `ClassName`...
/// 
/// Typically, functions using this parser should either expect normal identifiers or a "type", which is then an explicit Array.
/// 
/// # Arguments
/// - `input`: The input to parse off of.
/// 
/// # Returns
/// A tuple of the remaining input we did not parse and a token representing the parsed identifier.
/// 
/// # Errors
/// This function errors if we failed to parse an identifier for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn parse_array_type<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, Token, E> {
    nom::error::context("an identifier", comb::map(
        comb::recognize(seq::pair(
            branch::alt((cc::alpha1, bc::tag("_"))),
            multi::many0(branch::alt((cc::alphanumeric1, bc::tag("_")))),
        )),
        Token::Identifier,
    ))(input)
}
