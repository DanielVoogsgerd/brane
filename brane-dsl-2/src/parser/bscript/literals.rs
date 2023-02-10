//  LITERALS.rs
//    by Lut99
// 
//  Created:
//    07 Feb 2023, 15:04:30
//  Last edited:
//    10 Feb 2023, 11:44:59
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines parses for literals in the BraneScript/Bakery AST.
// 

use std::str::FromStr as _;

use nom::IResult;
use nom::{branch, combinator as comb};

use crate::errors::{NomError, NomErrorKind, ParseError};
use crate::ast::spec::TextRange;
use crate::ast::expressions::{Literal, LiteralKind};
use crate::parser::{Error, Input};
use crate::parser::utils::tag_token;


/***** HELPER FUNCTIONS *****/
/// Processes the matched string to get a value that has escaped characters already resolved.
/// 
/// # Arguments
/// - `raw`: The raw string that was matched.
/// 
/// # Returns
/// The compatible value for the string.
fn process_string_value(raw: &str) -> String {
    // Loop to add
    let mut res     : String = String::with_capacity(raw.len());
    let mut escaped : bool   = false;
    for c in raw.chars() {
        // Check if escaped
        if escaped {
            // We are; match a specific set of characters
            if c == 'n' {
                res.push('\n');
            } else if c == 'r' {
                res.push('\r');
            } else if c == 't' {
                res.push('\t');
            } else {
                // Other characters are just passed "as-is", but without the slash (unless this is a slash aof course).
                res.push(c);
            }
            escaped = false;
        } else if c == '\\' {
            // Going into escape mode
            escaped = true;
        } else {
            res.push(c);
        }
    }

    // Done
    res
}





/***** SCANNING FUNCTIONS *****/
/// Attempts to parse a boolean literal off the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The Input list of tokens to parse from.
/// 
/// # Returns
/// A tuple with the remaining input and the parsed literal.
/// 
/// # Errors
///  This function errors if we failed to parse a literal for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid. For literals, this typically means an invalid value (e.g., out-of-bounds etc).
fn boolean<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Literal, Error<'t, 's>> {
    comb::map_res(
        nom::error::context("a boolean literal", tag_token!('t, 's, Token::Boolean)),
        |t| {
            // Attempt to parse the value
            let value: bool = match bool::from_str(t.fragment()) {
                Ok(value) => value,
                Err(err)  => { return Err(ParseError::BoolParseError { raw: t.fragment(), err }); },
            };

            // Return a literal with that value
            Ok(Literal{ kind: LiteralKind::Boolean{ value }, range: Some(TextRange::from(t.span())) })
        },
    )(input)
}

/// Attempts to parse an integer literal off the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The Input list of tokens to parse from.
/// 
/// # Returns
/// A tuple with the remaining input and the parsed literal.
/// 
/// # Errors
///  This function errors if we failed to parse a literal for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid. For literals, this typically means an invalid value (e.g., out-of-bounds etc).
fn integer<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Literal, Error<'t, 's>> {
    comb::map_res(
        nom::error::context("an integer literal", tag_token!('t, 's, Token::Integer)),
        |t| {
            // Attempt to parse the value
            let value: i64 = match i64::from_str(&t.fragment().replace('_', "")) {
                Ok(value) => value,
                Err(err)  => { return Err(ParseError::IntParseError { raw: t.fragment(), err }); },
            };

            // Return a literal with that value
            Ok(Literal{ kind: LiteralKind::Integer{ value }, range: Some(TextRange::from(t.span())) })
        },
    )(input)
}

/// Attempts to parse a real literal off the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The Input list of tokens to parse from.
/// 
/// # Returns
/// A tuple with the remaining input and the parsed literal.
/// 
/// # Errors
///  This function errors if we failed to parse a literal for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid. For literals, this typically means an invalid value (e.g., out-of-bounds etc).
fn real<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Literal, Error<'t, 's>> {
    comb::map_res(
        nom::error::context("a real literal", tag_token!('t, 's, Token::Real)),
        |t| {
            // Attempt to parse the value
            let value: f64 = match f64::from_str(t.fragment()) {
                Ok(value) => value,
                Err(err)  => { return Err(ParseError::RealParseError { raw: t.fragment(), err }); },
            };

            // Return a literal with that value
            Ok(Literal{ kind: LiteralKind::Real{ value }, range: Some(TextRange::from(t.span())) })
        },
    )(input)
}





/***** LIBRARY *****/
/// Parses a literal from the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The Input list of tokens to parse from.
/// 
/// # Returns
/// A tuple with the remaining input and the parsed literal.
/// 
/// # Errors
///  This function errors if we failed to parse an operator for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn parse<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Literal, Error<'t, 's>> {
    // Parse the boolean, integer and real separate to catch the external errors and re-emit them as failures
    match boolean(input) {
        Ok((rem, lit))                           => { return Ok((rem, lit)); },
        Err(nom::Err::Error(NomError{ errors })) => if let [ (_, NomErrorKind::External(_, _)), .. ] = errors[..] { return Err(nom::Err::Failure(NomError{ errors })); },
        Err(err)                                 => { return Err(err); },
    }
    match integer(input) {
        Ok((rem, lit))                           => { return Ok((rem, lit)); },
        Err(nom::Err::Error(NomError{ errors })) => if let [ (_, NomErrorKind::External(_, _)), .. ] = errors[..] { return Err(nom::Err::Failure(NomError{ errors })); },
        Err(err)                                 => { return Err(err); },
    }
    match real(input) {
        Ok((rem, lit))                           => { return Ok((rem, lit)); },
        Err(nom::Err::Error(NomError{ errors })) => if let [ (_, NomErrorKind::External(_, _)), .. ] = errors[..] { return Err(nom::Err::Failure(NomError{ errors })); },
        Err(err)                                 => { return Err(err); },
    }

    // Otherwise, try any of the other literals
    branch::alt((
        comb::map(tag_token!('t, 's, Token::String), |t| Literal{ kind: LiteralKind::String{ value: process_string_value(*t.span().fragment()) }, range: Some(TextRange::from(t.span())) }),
        comb::map(tag_token!('t, 's, Token::Null),   |t| Literal{ kind: LiteralKind::Null, range: Some(TextRange::from(t.span())) }),
    ))(input)
}
