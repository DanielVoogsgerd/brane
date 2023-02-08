//  AUXILLARY.rs
//    by Lut99
// 
//  Created:
//    07 Feb 2023, 19:02:01
//  Last edited:
//    08 Feb 2023, 12:37:31
//  Auto updated?
//    Yes
// 
//  Description:
//!   Provides functions for parsing BraneScript/Bakery AST auxillary
//!   structures, such as identifiers or data types.
// 

use nom::IResult;
use nom::{branch, combinator as comb, sequence as seq};

use crate::ast::spec::TextRange;
use crate::ast::auxillary::{DataType, Identifier, MergeStrategy, MergeStrategyKind};
use crate::scanner::Token;
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
pub(crate) fn parse_ident<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Identifier, E> {
    comb::map(tag_token!('t, 's, Token::Identifier), |i| Identifier {
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
pub(crate) fn parse_type<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, DataType, E> {
    comb::map(tag_token!('t, 's, Token::Identifier), |i| DataType {
        data_type : crate::ast::types::DataType::from(*i.span().fragment()),
        range     : Some(i.range()),
    })(input)
}

/// Attempts to parse a merge strategy off the top of the given tokenstream.
/// 
/// # Arguments
/// - `input`: The new merge strategy to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed strategy.
/// /// 
/// # Errors
/// This function errors if we failed to parse a data type for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn parse_merge<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, MergeStrategy, E> {
    // Expect a very weird set of tokens, just to make the identifiers we expect
    comb::map_parser(
        seq::pair(
            comb::opt(tag_token!('t, 's, Token::Identifier)),
            comb::opt(branch::alt((
                tag_token!('t, 's, Token::Add),
                tag_token!('t, 's, Token::Mul),
            ))),
        ),
        |(text, plus_star): (Option<&'t Token<'s>>, Option<&'t Token<'s>>)| {
            // Match the set
            match (text, plus_star) {
                (Some(text), Some(plus_star)) => {
                    // Match the text
                    if text.fragment() == "first" && plus_star.fragment() == "*" {
                        Ok(((Some(text), Some(plus_star)), MergeStrategy{ kind: MergeStrategyKind::FirstBlocking, range: Some(TextRange::new(text.start_of(), plus_star.end_of())) }))
                    } else {
                        Err(nom::Err::Error(E::from_error_kind(input, nom::error::ErrorKind::Complete)))
                    }
                },

                (Some(text), None) => {
                    // Match the text to find the kind
                    let kind: MergeStrategyKind = match text.fragment() {
                        "first"   => MergeStrategyKind::First,
                        "last"    => MergeStrategyKind::Last,
                        "sum"     => MergeStrategyKind::Sum,
                        "product" => MergeStrategyKind::Product,
                        "max"     => MergeStrategyKind::Max,
                        "min"     => MergeStrategyKind::Min,
                        "all"     => MergeStrategyKind::All,
                        _         => { return Err(nom::Err::Error(E::append(input, nom::error::ErrorKind::Alt, E::from_error_kind(input, nom::error::ErrorKind::Tag)))); },
                    };

                    // Return the strat
                    Ok((
                        (Some(text), None),
                        MergeStrategy {
                            kind,
                            range : Some(text.range()),
                        },
                    ))
                },

                (None, Some(plus_star)) => {
                    // Match on the plus or star found
                    let kind: MergeStrategyKind = match plus_star.fragment() {
                        "+" => MergeStrategyKind::Sum,
                        "*" => MergeStrategyKind::Product,
                        _   => { return Err(nom::Err::Error(E::append(input, nom::error::ErrorKind::Alt, E::from_error_kind(input, nom::error::ErrorKind::Tag)))); },
                    };

                    // Return the strat
                    Ok((
                        (None, Some(plus_star)),
                        MergeStrategy {
                            kind,
                            range : Some(plus_star.range()),
                        },
                    ))
                },

                // We do expect at least any of them
                (None, None) => Err(nom::Err::Error(E::from_error_kind(input, nom::error::ErrorKind::Alt))),
            }
        },
    )(input)
}
