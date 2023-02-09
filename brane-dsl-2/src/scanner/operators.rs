//  OPERATORS.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 17:32:53
//  Last edited:
//    09 Feb 2023, 15:00:26
//  Auto updated?
//    Yes
// 
//  Description:
//!   Parses operators in BraneScript and Bakery.
// 

use nom::IResult;
use nom::{branch, bytes::complete as bc, combinator as comb};

use super::{Error, Input};
use super::tokens::Token;


/***** LIBRARY *****/
/// Attempts to parse an operator from the head of the given input.
/// 
/// # Arguments
/// - `input`: The input to parse off of.
/// 
/// # Returns
/// A tuple of the remaining input we did not parse and a token representing the parsed operator.
/// 
/// # Errors
/// This function errors if we failed to parse a operator for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn parse<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, Token, E> {
    branch::alt((
        // Comparison
        comb::map(bc::tag("=="), Token::Eq),
        comb::map(bc::tag("!="), Token::Ne),
        comb::map(bc::tag("<"),  Token::Lt),
        comb::map(bc::tag("<="), Token::Le),
        comb::map(bc::tag(">"),  Token::Gt),
        comb::map(bc::tag(">="), Token::Ge),

        // Logical
        comb::map(bc::tag("&"), Token::And),
        comb::map(bc::tag("|"), Token::Or),
        comb::map(bc::tag("!"), Token::Not),

        // Arithmetic
        comb::map(bc::tag("+"), Token::Add),
        comb::map(bc::tag("-"), Token::Min),
        comb::map(bc::tag("*"), Token::Mul),
        comb::map(bc::tag("/"), Token::Div),
        comb::map(bc::tag("%"), Token::Mod),

        // Miscellaneous
        comb::map(bc::tag(":="), Token::Assign),
        comb::map(bc::tag("="),  Token::Equals),
    ))(input)
}
