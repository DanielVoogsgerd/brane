//  COMMENTS.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 16:34:39
//  Last edited:
//    07 Feb 2023, 11:37:26
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines how to scan tokens.
// 

use nom::IResult;
use nom::{branch, bytes::complete as bc, combinator as comb, sequence as seq};

use super::{Error, Input};


/***** SCANNING FUNCTIONS *****/
/// Attempts to parse a single-line comment from the head of the given input.
/// 
/// # Arguments
/// - `input`: The input to parse off of.
/// 
/// # Returns
/// A tuple of the remaining input we did not parse and... nothing! Since we couldn't care less about comments.
/// 
/// # Errors
/// This function errors if we failed to parse a comment for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn parse_single<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, (), E> {
    comb::value((), seq::pair(
        bc::tag("//"),
        seq::terminated(
            comb::opt(bc::is_not("\n")),
            branch::alt((
                bc::tag("\n"),
                comb::eof,
            )),
        )
    ))(input)
}

/// Attempts to parse a multi-line comment from the head of the given input.
/// 
/// # Arguments
/// - `input`: The input to parse off of.
/// 
/// # Returns
/// A tuple of the remaining input we did not parse and... nothing! Since we couldn't care less about comments.
/// 
/// # Errors
/// This function errors if we failed to parse a comment for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn parse_multi<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, (), E> {
    comb::value((), seq::pair(
        bc::tag("/*"),
        comb::cut(seq::pair(
            bc::take_until("*/"),
            bc::tag("*/")
        )),
    ))(input)
}





/***** LIBRARY *****/
/// Attempts to parse a comment from the head of the given input.
/// 
/// # Arguments
/// - `input`: The input to parse off of.
/// 
/// # Returns
/// A tuple of the remaining input we did not parse and... nothing! Since we couldn't care less about comments.
/// 
/// # Errors
/// This function errors if we failed to parse a comment for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn parse<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, (), E> {
    branch::alt((
        parse_single,
        parse_multi,
    ))(input)
}
