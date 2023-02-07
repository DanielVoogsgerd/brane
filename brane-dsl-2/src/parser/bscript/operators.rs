//  OPERATORS.rs
//    by Lut99
// 
//  Created:
//    07 Feb 2023, 13:05:29
//  Last edited:
//    07 Feb 2023, 19:36:19
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines how to parse the operators in the BraneScript/Bakery AST.
// 

use nom::IResult;
use nom::{branch, combinator as comb};

use crate::ast::expressions::{BinaryOperator, BinaryOperatorKind, UnaryOperator, UnaryOperatorKind};
use crate::parser::{Error, Input};
use crate::parser::utils::tag_token;


/***** SCANNING FUNCTIONS *****/
/// Parses a unary operator.
/// 
/// # Arguments
/// - `input`: The Input list of tokens to parse from.
/// 
/// # Returns
/// A tuple with the remaining input and the parsed unary operator.
/// 
/// # Errors
///  This function errors if we failed to parse an operator for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
#[inline]
pub fn unary<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, UnaryOperator, E> {
    branch::alt((
        comb::map(tag_token!(Token::Min), |o| UnaryOperator{ kind: UnaryOperatorKind::Not, range: Some(o.range()) }),
        comb::map(tag_token!(Token::Not), |o| UnaryOperator{ kind: UnaryOperatorKind::Not, range: Some(o.range()) }),
    ))(input)
}

/// Parses a binary operator.
/// 
/// # Arguments
/// - `input`: The Input list of tokens to parse from.
/// 
/// # Returns
/// A tuple with the remaining input and the parsed binary operator.
/// 
/// # Errors
///  This function errors if we failed to parse an operator for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub fn binary<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, BinaryOperator, E> {
    branch::alt((
        comb::map(tag_token!(Token::Add), |o| BinaryOperator{ kind: BinaryOperatorKind::Add, range: Some(o.range()) }),
        comb::map(tag_token!(Token::Min), |o| BinaryOperator{ kind: BinaryOperatorKind::Sub, range: Some(o.range()) }),
        comb::map(tag_token!(Token::Mul), |o| BinaryOperator{ kind: BinaryOperatorKind::Mul, range: Some(o.range()) }),
        comb::map(tag_token!(Token::Div), |o| BinaryOperator{ kind: BinaryOperatorKind::Div, range: Some(o.range()) }),
        comb::map(tag_token!(Token::Mod), |o| BinaryOperator{ kind: BinaryOperatorKind::Mod, range: Some(o.range()) }),
        comb::map(tag_token!(Token::Or),  |o| BinaryOperator{ kind: BinaryOperatorKind::Or, range: Some(o.range()) }),
        comb::map(tag_token!(Token::And), |o| BinaryOperator{ kind: BinaryOperatorKind::And, range: Some(o.range()) }),
        comb::map(tag_token!(Token::Eq),  |o| BinaryOperator{ kind: BinaryOperatorKind::Eq, range: Some(o.range()) }),
        comb::map(tag_token!(Token::Ne),  |o| BinaryOperator{ kind: BinaryOperatorKind::Ne, range: Some(o.range()) }),
        comb::map(tag_token!(Token::Lt),  |o| BinaryOperator{ kind: BinaryOperatorKind::Lt, range: Some(o.range()) }),
        comb::map(tag_token!(Token::Le),  |o| BinaryOperator{ kind: BinaryOperatorKind::Le, range: Some(o.range()) }),
        comb::map(tag_token!(Token::Gt),  |o| BinaryOperator{ kind: BinaryOperatorKind::Gt, range: Some(o.range()) }),
        comb::map(tag_token!(Token::Ge),  |o| BinaryOperator{ kind: BinaryOperatorKind::Ge, range: Some(o.range()) }),
    ))(input)
}





// /***** LIBRARY *****/
// /// Parses an operator, unary or binary, from the head of the given input.
// /// 
// /// # Arguments
// /// - `input`: The Input list of tokens to parse from.
// /// 
// /// # Returns
// /// A tuple with the remaining input and the parsed binary operator.
// /// 
// /// # Errors
// ///  This function errors if we failed to parse an operator for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
// #[inline]
// pub fn parse<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Operator, E> {
//     branch::alt((
//         comb::map(unary,  Operator::Unary),
//         comb::map(binary, Operator::Binary),
//     ))(input)
// }
