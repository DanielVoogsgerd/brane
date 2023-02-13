//  LITERALS.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 16:56:44
//  Last edited:
//    13 Feb 2023, 11:52:28
//  Auto updated?
//    Yes
// 
//  Description:
//!   Scans literals from the input.
// 

use nom::IResult;
use nom::{branch, bytes::complete as bc, character::complete as cc, combinator as comb, multi, sequence as seq};

use super::{Error, Input};
use super::tokens::Token;
use super::keywords;


/***** SCANNING FUNCTIONS *****/
/// Parses a boolean token off of the head of the given input.
/// 
/// # Arguments
/// - `input`: The input text to scan.
/// 
/// # Returns
/// The remaining tokens and the scanned token.
/// 
/// # Errors
/// This function errors if we could not parse the literal token.
fn boolean<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, Token, E> {
    nom::error::context("a boolean literal", comb::map(
        branch::alt((
            seq::terminated(bc::tag("true"),  comb::peek(keywords::separator)),
            seq::terminated(bc::tag("false"), comb::peek(keywords::separator)),
        )),
        Token::Boolean,
    ))(input)
}

/// Parses an integer token off of the head of the given input.
/// 
/// # Arguments
/// - `input`: The input text to scan.
/// 
/// # Returns
/// The remaining tokens and the scanned token.
/// 
/// # Errors
/// This function errors if we could not parse the literal token.
fn integer<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, Token, E> {
    nom::error::context("an integer literal", comb::map(
        comb::recognize(
            multi::many1(
                seq::terminated(
                    cc::one_of("0123456789"),
                    multi::many0(cc::char('_')),
                )
            )
        ),
        Token::Integer,
    ))(input)
}

/// Parses a real token off of the head of the given input.
/// 
/// # Arguments
/// - `input`: The input text to scan.
/// 
/// # Returns
/// The remaining tokens and the scanned token.
/// 
/// # Errors
/// This function errors if we could not parse the literal token.
fn real<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, Token, E> {
    nom::error::context("a real literal", comb::map(
        branch::alt((
            comb::recognize(seq::tuple((
                cc::char('.'),
                integer,
                comb::opt(seq::tuple((cc::one_of("eE"), comb::opt(cc::one_of("+-")), integer))),
            ))),
            comb::recognize(seq::tuple((
                integer,
                comb::opt(seq::preceded(cc::char('.'), integer)),
                cc::one_of("eE"),
                comb::opt(cc::one_of("+-")),
                integer,
            ))),
            comb::recognize(seq::tuple((integer, cc::char('.'), comb::opt(integer)))),
        )),
        Token::Real,
    ))(input)
}

/// Parses a string token off of the head of the given input.
/// 
/// # Arguments
/// - `input`: The input text to scan.
/// 
/// # Returns
/// The remaining tokens and the scanned token.
/// 
/// # Errors
/// This function errors if we could not parse the literal token.
fn string<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, Token, E> {
    nom::error::context("a string literal", comb::map(
        comb::recognize(
            seq::preceded(
                cc::char('\"'),
                comb::cut(seq::terminated(
                    bc::escaped(bc::is_not("\"\\"), '\\', cc::one_of("\"ntr\\\'")),
                    cc::char('\"'),
                )),
            )
        ),
        Token::String,
    ))(input)
}

/// Parses a null-token off of the head of the given input.
/// 
/// # Arguments
/// - `input`: The input text to scan.
/// 
/// # Returns
/// The remaining tokens and the scanned token.
/// 
/// # Errors
/// This function errors if we could not parse the literal token.
#[inline]
fn null<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, Token, E> {
    nom::error::context("a null literal", comb::map(
        bc::tag("null"),
        Token::Null,
    ))(input)
}





/***** LIBRARY *****/
/// Attempts to parse a literal from the head of the given input.
/// 
/// # Arguments
/// - `input`: The input to parse off of.
/// 
/// # Returns
/// A tuple of the remaining input we did not parse and a token representing the parsed literal.
/// 
/// # Errors
/// This function errors if we failed to parse a comment for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn parse<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, Token, E> {
    branch::alt((
        boolean,
        real,
        integer,
        string,
        null,
    ))(input)
}
