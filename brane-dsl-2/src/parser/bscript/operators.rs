//  OPERATORS.rs
//    by Lut99
// 
//  Created:
//    07 Feb 2023, 13:05:29
//  Last edited:
//    10 Feb 2023, 09:02:53
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines how to parse the operators in the BraneScript/Bakery AST.
// 

use nom::IResult;
use nom::{branch, combinator as comb};

use crate::ast::spec::TextRange;
use crate::ast::expressions::{BinaryOperator, BinaryOperatorKind, ExpressionPostfix, ExpressionPostfixKind, UnaryOperator, UnaryOperatorKind};
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
pub(crate) fn unary<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, UnaryOperator, E> {
    branch::alt((
        comb::map(tag_token!('t, 's, Token::Min), |o| UnaryOperator{ kind: UnaryOperatorKind::Not, range: Some(o.range()) }),
        comb::map(tag_token!('t, 's, Token::Not), |o| UnaryOperator{ kind: UnaryOperatorKind::Not, range: Some(o.range()) }),
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
fn binary<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, BinaryOperator, E> {
    branch::alt((
        comb::map(tag_token!('t, 's, Token::Add), |o| BinaryOperator{ kind: BinaryOperatorKind::Add, range: Some(o.range()) }),
        comb::map(tag_token!('t, 's, Token::Min), |o| BinaryOperator{ kind: BinaryOperatorKind::Sub, range: Some(o.range()) }),
        comb::map(tag_token!('t, 's, Token::Mul), |o| BinaryOperator{ kind: BinaryOperatorKind::Mul, range: Some(o.range()) }),
        comb::map(tag_token!('t, 's, Token::Div), |o| BinaryOperator{ kind: BinaryOperatorKind::Div, range: Some(o.range()) }),
        comb::map(tag_token!('t, 's, Token::Mod), |o| BinaryOperator{ kind: BinaryOperatorKind::Mod, range: Some(o.range()) }),
        comb::map(tag_token!('t, 's, Token::Or),  |o| BinaryOperator{ kind: BinaryOperatorKind::Or, range: Some(o.range()) }),
        comb::map(tag_token!('t, 's, Token::And), |o| BinaryOperator{ kind: BinaryOperatorKind::And, range: Some(o.range()) }),
        comb::map(tag_token!('t, 's, Token::Eq),  |o| BinaryOperator{ kind: BinaryOperatorKind::Eq, range: Some(o.range()) }),
        comb::map(tag_token!('t, 's, Token::Ne),  |o| BinaryOperator{ kind: BinaryOperatorKind::Ne, range: Some(o.range()) }),
        comb::map(tag_token!('t, 's, Token::Lt),  |o| BinaryOperator{ kind: BinaryOperatorKind::Lt, range: Some(o.range()) }),
        comb::map(tag_token!('t, 's, Token::Le),  |o| BinaryOperator{ kind: BinaryOperatorKind::Le, range: Some(o.range()) }),
        comb::map(tag_token!('t, 's, Token::Gt),  |o| BinaryOperator{ kind: BinaryOperatorKind::Gt, range: Some(o.range()) }),
        comb::map(tag_token!('t, 's, Token::Ge),  |o| BinaryOperator{ kind: BinaryOperatorKind::Ge, range: Some(o.range()) }),
    ))(input)
}

/// Parses an expression postfix.
/// 
/// # Arguments
/// - `input`: The Input list of tokens to parse from.
/// 
/// # Returns
/// A tuple with the remaining input and the parsed expression postfix.
/// 
/// # Errors
///  This function errors if we failed to parse a postfix for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn postfix<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, ExpressionPostfix, E> {
    branch::alt((
        // Binary expressions are postfix, too
        comb::map(binary, |o| {
            let range: Option<TextRange> = o.range;
            ExpressionPostfix{ kind: ExpressionPostfixKind::BinaryOperator(o), range }
        }),

        // Typecasts (i.e., `as`)
        comb::map(tag_token!('t, 's, Token::As), |a| ExpressionPostfix{ kind: ExpressionPostfixKind::Cast, range: Some(a.range()) }),
        // Array indices (i.e., `[`)
        comb::map(tag_token!('t, 's, Token::LeftBracket), |b| ExpressionPostfix{ kind: ExpressionPostfixKind::ArrayIndex, range: Some(b.range()) }),
        // Function calls (i.e., `(`)
        comb::map(tag_token!('t, 's, Token::LeftParen), |p| ExpressionPostfix{ kind: ExpressionPostfixKind::Call, range: Some(p.range()) }),
        // Projections (i.e., `.`)
        comb::map(tag_token!('t, 's, Token::Dot), |d| ExpressionPostfix{ kind: ExpressionPostfixKind::Proj, range: Some(d.range()) }),
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
