//  EXPRESSION.rs
//    by Lut99
// 
//  Created:
//    16 Aug &2022, 14:42:43
//  Last edited:
//    06 Feb 2023, 14:29:51
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines functions for parsing BraneScript / Bakery expressions.
// 

use std::num::NonZeroUsize;

use nom::error::{ContextError, ParseError};
use nom::{branch, combinator as comb, multi, sequence as seq};
use nom::{IResult, Parser};

use super::ast::{Expr, Node, Operator, UnaOp};
use crate::spec::{TextPos, TextRange};
use crate::parser::{identifier, instance, literal, operator};
use crate::scanner::{Token, Tokens};
use crate::tag_token;
use crate::location::AllowedLocations;


/// Parses an expression.
///
/// # Arguments
/// - `input`: The input stream of tokens that we use to parse expressions from.
/// 
/// # Returns
/// A tuple of the remaining tokens and a parsed expression if there was an expression on top.
/// 
/// # Errors
/// This function returns a nom::Error if it failed to parse an expression.
pub fn parse<'a, E: ParseError<Tokens<'a>> + ContextError<Tokens<'a>>>(input: Tokens<'a>) -> IResult<Tokens, Expr, E> {
    // Use a pratt parser(?) to actually parse it
    expr_pratt(input, 0)
}

/// Parses the expressions in a pratt-parser style.
/// 
/// Explanation of pratt parsers may be found here: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html.
/// 
/// # Arguments
/// - `input`: The input stream of tokens that we use to parse expressions from.
/// - `min_bp`: The minimum binding power of operators to parse (to allow presedence and such).
/// 
/// # Returns
/// A tuple of the remaining tokens and a parsed expression if there was an expression on top.
/// 
/// # Errors
/// This function returns a nom::Error if it failed to parse an expression.
fn expr_pratt<'a, E: ParseError<Tokens<'a>> + ContextError<Tokens<'a>>>(
    input: Tokens<'a>,
    min_bp: u8,
) -> IResult<Tokens, Expr, E> {
    // We can usually parse a normal expression, unless we have some operator that is unary and precedes its operand
    let (mut remainder, mut lhs) = match operator::unary_operator::<E>(input) {
        // Parse array expressions (not really an operator but marked as such because the index is)
        Ok((r, UnaOp::Idx{ range })) => {
            // Parse the rest as (the rest of) an array
            array_expr(&Some(range)).parse(r)?
        },
        // Parse parenthesis (not really an operator but marked as such because...? I think because that's convient with the unary_operator function?)
        Ok((r, UnaOp::Prio{ range: _ })) => seq::terminated(self::parse, tag_token!(Token::RightParen)).parse(r)?,

        // Any other operator is parsed as a unary expression
        Ok((r, operator)) => {
            // In any other case, parse an entire expression, where we assume everything to be part of this expression as long as it binds less strongly
            let (_, r_bp) = operator.binding_power();
            let (r, rhs)  = expr_pratt(r, r_bp)?;
            let range: TextRange = TextRange::new(operator.start().clone(), rhs.end().clone());

            // That makes the unary operatation
            (
                r,
                Expr::new_unaop(
                    operator,
                    Box::new(rhs),

                    range,
                ),
            )
        },

        // Any other _non-operator_ is parsed as an atomic expression (literals, identifiers, etc).
        _ => expr_atom(input)?,
    };

    // Any other expressions are _after_ the (first) expression, so the idea is to keep popping until we have parsed the whole tree
    loop {
        match operator::parse::<E>(remainder) {
            Ok((r, Operator::Binary(operator))) => {
                // Find the right expression for this operator
                let (left_bp, right_bp) = operator.binding_power();
                // Note that we don't bind if the lefthand side is actually binding stronger (they will steal the LHS expression away, we will get the full expression later).
                if left_bp < min_bp { break; }
                let (r, rhs) = expr_pratt(r, right_bp)?;

                // We then return the parsed bit as the new LHS, the unparsed bit is the remainder to-be-parsed part.
                let range: TextRange = TextRange::new(lhs.start().clone(), rhs.end().clone());
                lhs = Expr::new_binop(
                    operator,
                    Box::new(lhs),
                    Box::new(rhs),
                    range,
                );
                remainder = r;
            }

            // This unary operator binds _after_ the expression it binds
            Ok((r, Operator::Unary(operator))) => {
                // Quit if there is already something that binds our LHS; otherwise, we can bind it
                let (left_bp, _) = operator.binding_power();
                if left_bp < min_bp { break; }

                // We then return the parsed bit as the new LHS, the unparsed bit is the remainder to-be-parsed part.
                lhs = match operator {
                    UnaOp::Idx{ .. } => {
                        // Array indexing has a custom AST entry because it's essentialy a "dynamically configurable" operator
                        let (r, rhs) = comb::cut(self::parse)(r)?;
                        let (r, bracket) = comb::cut(tag_token!(Token::RightBracket))(r)?;
                        remainder = r;

                        let range: TextRange = TextRange::new(lhs.start().clone(), TextPos::end_of(bracket.tok[0].inner()));
                        Expr::new_array_index(
                            Box::new(lhs),
                            Box::new(rhs),

                            range,
                        )
                    },
                    UnaOp::Proj{ .. } => {
                        // We need to see an identifier first that represents the field to project
                        let (r, id) = comb::cut(identifier::parse)(r)?;
                        remainder = r;

                        // Then the rest is this operator
                        let range: TextRange = TextRange::new(lhs.start().clone(), id.end().clone());
                        Expr::new_proj(Box::new(lhs), id, range)
                    },
                    UnaOp::Prio{ .. } => {
                        // The same goes for a function call as an array index, seeing the arguments as configuration options
                        let (r, args)  : (_, Vec<Expr>) = comb::cut(multi::separated_list0(tag_token!(Token::Comma), self::parse))(r)?;
                        let (r, paren) : (_, Tokens)    = comb::cut(tag_token!(Token::LeftParen))(r)?;
                        remainder = r;

                        // Create the new function call
                        let range: TextRange = TextRange::new(lhs.start().clone(), TextPos::end_of(paren.tok[0].inner()));
                        Expr::new_call(
                            Box::new(lhs),
                            args.into_iter().map(|a| Box::new(a)).collect(),
                            range,
                            AllowedLocations::All,
                        )
                    },

                    operator => {
                        // In the other cases, it's a normal unary expression
                        let range: TextRange = TextRange::new(lhs.start().clone(), operator.end().clone());
                        Expr::new_unaop(
                            operator,
                            Box::new(lhs),

                            range,
                        )
                    }
                };
            }
            _ => break,
        }
    }

    Ok((remainder, lhs))
}

/// Parses the given token stream as a literal or a variable reference.
/// 
/// # Arguments
/// - `input`: The input stream of tokens that we use to parse expressions from.
/// 
/// # Returns
/// A tuple of the remaining tokens and a parsed expression if there was an expression on top.
/// 
/// # Errors
/// This function returns a nom::Error if it failed to parse an expression.
fn expr_atom<'a, E: ParseError<Tokens<'a>> + ContextError<Tokens<'a>>>(
    input: Tokens<'a>
) -> IResult<Tokens, Expr, E> {
    branch::alt((
        instance::parse,
        comb::map(literal::parse, |l| Expr::Literal{ literal: l }),
        comb::map(identifier::parse, Expr::new_varref),
    ))
    .parse(input)
}

/// Parses the given token stream as an array expression.
/// 
/// # Arguments
/// - `input`: The input stream of tokens that we use to parse expressions from.
/// - `start_range`: If not None, skips parsing the initial '[' bracket and instead uses the given range as the start range.
/// 
/// # Returns
/// A tuple of the remaining tokens and a parsed expression if there was an expression on top.
/// 
/// # Errors
/// This function returns a nom::Error if it failed to parse an expression.
fn array_expr<'a, 'b, E: ParseError<Tokens<'a>> + ContextError<Tokens<'a>>>(
    start_range : &'b Option<TextRange>,
) -> impl 'b + Parser<Tokens<'a>, Expr, E> {
    // Return a closure that does the actual thingy
    move |input: Tokens<'a>| -> IResult<Tokens, Expr, E> {
        // Parse the first bracket if needed
        let (r, range): (Tokens<'a>, TextRange) = if let Some(range) = start_range.as_ref() {
            (input, range.clone())
        } else {
            let (r, t) = tag_token!(Token::LeftBracket).parse(input)?;
            (r, TextRange::from(t.tok[0].inner()))
        };

        // It's an array-index; but we parse it as an array expression (so parse a comma-separated list of expressions)
        let (r, entries) = comb::opt(seq::terminated(
            seq::pair(
                self::parse,
                multi::many0(seq::preceded(tag_token!(Token::Comma), self::parse)),
            ),
            comb::opt(tag_token!(Token::Comma)),
        )).parse(r)?;
        let (r, bracket) = tag_token!(Token::RightBracket).parse(r)?;

        // Return the array with its elements
        if let Some((head, entries)) = entries {
            let mut e = Vec::with_capacity(entries.len() + 1);
            e.push(Box::new(head));
            e.append(&mut entries.into_iter().map(Box::new).collect());

            // Return it
            Ok((r, Expr::new_array(e, TextRange::new(range.start, TextPos::end_of(bracket.tok[0].inner())))))
        } else {
            // It's an empty Array
            Ok((r, Expr::new_array(vec![], TextRange::new(range.start, TextPos::end_of(bracket.tok[0].inner())))))
        }
    }
}
