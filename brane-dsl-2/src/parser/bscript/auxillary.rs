//  AUXILLARY.rs
//    by Lut99
// 
//  Created:
//    07 Feb 2023, 19:02:01
//  Last edited:
//    10 Feb 2023, 19:03:46
//  Auto updated?
//    Yes
// 
//  Description:
//!   Provides functions for parsing BraneScript/Bakery AST auxillary
//!   structures, such as identifiers or data types.
// 

use nom::IResult;
use nom::{branch, combinator as comb, sequence as seq};

use crate::errors::ParseError;
use crate::ast::spec::TextRange;
use crate::ast::auxillary::{Annotation, AnnotationKind, DataType, Identifier, MergeStrategy, MergeStrategyKind};
use crate::ast::types;
use crate::ast::expressions::Expression;
use crate::scanner::Token;
use crate::parser::{Error, Input};
use crate::parser::utils::{self, tag_token};
use super::expressions;


/***** LIBRARY *****/
/// Parses a statement-annotation thing from the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The new TokenStream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed annotations (a single annotation notation `#[ ... ]` can have multiple annotations within it).
/// 
/// # Errors
/// This function errors if we failed to parse a definition for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn parse_annots<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Vec<Annotation>, Error<'t, 's>> {
    nom::error::context("an annotation", seq::preceded(
        tag_token!('t, 's, Token::Hashtag),
        comb::cut(seq::delimited(
            tag_token!('t, 's, Token::LeftBracket),
            utils::separated_list0(
                tag_token!('t, 's, Token::Comma),
                annot,
            ),
            tag_token!('t, 's, Token::RightBracket),
        )),
    ))(input)
}

/// Parses an annotation from the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The new TokenStream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed annotations (a single annotation notation `#[ ... ]` can have multiple annotations within it).
/// 
/// # Errors
/// This function errors if we failed to parse a definition for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn annot<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Annotation, Error<'t, 's>> {
    branch::alt((
        // It's an identifier/expression pair
        comb::map(
            seq::separated_pair(
                parse_ident,
                tag_token!('t, 's, Token::Equals),
                expressions::parse,
            ),
            |(key, value): (Identifier, Expression)| {
                // Find a range covering both
                let range: Option<TextRange> = match (key.range, value.range) {
                    (Some(TextRange{ start, .. }), Some(TextRange{ end, .. })) => Some(TextRange::new(start, end)),
                    _                                                          => None,
                };

                // Create an annotation with it
                Annotation {
                    kind : AnnotationKind::KeyValue(key, value),
                    range,
                }
            },
        ),

        // It's a key/list pair
        comb::map(
            seq::pair(
                parse_ident,
                seq::pair(
                    seq::preceded(
                        tag_token!('t, 's, Token::LeftParen),
                        utils::separated_list1(
                            tag_token!('t, 's, Token::Comma),
                            annot,
                        ),
                    ),
                    tag_token!('t, 's, Token::RightParen),
                ),
            ),
            |(key, (list, rparen)): (Identifier, (Vec<Annotation>, &Token))| {
                let range: Option<TextRange> = key.range.map(|r| TextRange::new(r.start, rparen.end_of()));
                Annotation {
                    kind : AnnotationKind::KeyList(key, list),
                    range,
                }
            },
        ),

        // It's a single identifier
        comb::map(
            parse_ident,
            |ident: Identifier| {
                let range: Option<TextRange> = ident.range;
                Annotation {
                    kind : AnnotationKind::Identifier(ident),
                    range,
                }
            },
        ),
    ))(input)
}



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
pub(crate) fn parse_ident<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Identifier, Error<'t, 's>> {
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
pub(crate) fn parse_type<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, DataType, Error<'t, 's>> {
    nom::error::context("a data type", branch::alt((
        // The type is infix to the array brackets (`[`, type, `]`)
        comb::map(
            seq::pair(
                tag_token!('t, 's, Token::LeftBracket),
                comb::cut(seq::pair(
                    parse_type,
                    tag_token!('t, 's, Token::RightBracket),
                )),
            ),
            |(lbrack, (data_type, rbrack)): (&Token, (DataType, &Token))| {
                DataType {
                    data_type : types::DataType::Array(Box::new(data_type.data_type)),
                    range     : Some(TextRange::new(lbrack.start_of(), rbrack.end_of())),
                }
            },
        ),

        // Otherwise, it's either a class identifier or an identifier postfixed with `[]`.
        comb::map(
            parse_ident,
            |ident: Identifier| {
                DataType {
                    data_type : types::DataType::from(ident.name),
                    range     : ident.range,
                }
            }
        ),
    )))(input)
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
pub(crate) fn parse_merge<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, MergeStrategy, Error<'t, 's>> {
    // Expect a very weird set of tokens, just to make the identifiers we expect
    comb::map_res(
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
                        Ok(MergeStrategy{ kind: MergeStrategyKind::FirstBlocking, range: Some(TextRange::new(text.start_of(), plus_star.end_of())) })
                    } else {
                        Err(ParseError::UnknownMergeStrategy{ raw: format!("{}{}", text.fragment(), plus_star.fragment()) })
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
                        _         => { return Err(ParseError::UnknownMergeStrategy{ raw: text.fragment().into() }); },
                    };

                    // Return the strat
                    Ok(MergeStrategy {
                        kind,
                        range : Some(text.range()),
                    })
                },

                (None, Some(plus_star)) => {
                    // Match on the plus or star found
                    let kind: MergeStrategyKind = match plus_star.fragment() {
                        "+" => MergeStrategyKind::Sum,
                        "*" => MergeStrategyKind::Product,
                        _   => { return Err(ParseError::UnknownMergeStrategy{ raw: plus_star.fragment().into() }); },
                    };

                    // Return the strat
                    Ok(MergeStrategy {
                        kind,
                        range : Some(plus_star.range()),
                    })
                },

                // We do expect at least any of them
                (None, None) => Err(ParseError::NoMergeStrategy),
            }
        },
    )(input)
}
