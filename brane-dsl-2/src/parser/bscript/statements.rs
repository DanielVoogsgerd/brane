//  STATEMENTS.rs
//    by Lut99
// 
//  Created:
//    08 Feb 2023, 10:19:08
//  Last edited:
//    09 Feb 2023, 18:44:38
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines parsers for statements as allowed in BraneScript.
// 

use nom::IResult;
use nom::{branch, combinator as comb, multi, sequence as seq};

use crate::ast::spec::TextRange;
use crate::ast::auxillary::{Annotation, DataType, Identifier};
use crate::ast::expressions::{Block, Expression, ExpressionKind, Literal};
use crate::ast::statements::{ArgDef, ClassMemberDef, FunctionDef, PropertyDef, Statement, StatementKind};
use crate::scanner::Token;
use crate::parser::{Error, Input};
use crate::parser::utils::tag_token;
use super::{auxillary, blocks, expressions, literals};


/***** HELPER SCANNING FUNCTIONS *****/
/// Parses an argument definition from the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed argument definition.
/// 
/// # Errors
/// This function errors if we failed to parse a definition for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn arg_def<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, ArgDef, E> {
    comb::map(
        seq::pair(
            auxillary::parse_ident,
            comb::opt(seq::preceded(
                tag_token!('t, 's, Token::Colon),
                nom::error::context("argument definition", comb::cut(auxillary::parse_type)),
            )),
        ),
        |(name, data_type): (Identifier, Option<DataType>)| {
            // Compute the range
            let range: Option<TextRange> = match (name.range, &data_type) {
                (Some(TextRange{ start, .. }), Some(DataType{ range: Some(TextRange{ end, .. }), .. })) => Some(TextRange::new(start, end)),
                _                                                                                       => None,
            };

            // Return it as an argument definition
            ArgDef {
                name,
                data_type : data_type.unwrap_or(DataType::any()),
                range,
            }
        }
    )(input)
}

/// Parses a member definition for in a class from the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed member definition.
/// 
/// # Errors
/// This function errors if we failed to parse a definition for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn member_def<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, ClassMemberDef, E> {
    branch::alt((
        // A field/property
        comb::map(
            seq::pair(
                auxillary::parse_ident,
                nom::error::context("property definition", comb::cut(seq::pair(
                    seq::preceded(
                        tag_token!('t, 's, Token::Colon),
                        auxillary::parse_type,
                    ),
                    tag_token!('t, 's, Token::Semicolon),
                ))),
            ),
            |(name, (data_type, semicolon)): (Identifier, (DataType, &Token))| {
                let range: Option<TextRange> = name.range.map(|r| TextRange::new(r.start, semicolon.end_of()));
                ClassMemberDef::Property(PropertyDef {
                    name,
                    data_type,
                    range,
                })
            }
        ),
        // A method
        comb::map(
            func_def,
            |def: FunctionDef| ClassMemberDef::Method(def),
        ),
    ))(input)
}





/***** STATEMENT SCANNING FUNCTIONS *****/
/// Creates a parser for import statements that incorporates the given annotations.
/// 
/// # Arguments
/// - `annots`: The list of parsed annotations to add to the this statement.
/// 
/// # Returns
/// A new closure that parses the import statement á lá nom.
fn import<'t, 's, 'a, E>(annots: &'a [Annotation]) -> impl 'a + FnMut(Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, E>
where
    's: 't,
    E: Error<'t, 's>,
{
    |input| {
        comb::map(
            seq::tuple((
                tag_token!('t, 's, Token::Import),
                nom::error::context("import statement", comb::cut(
                    seq::pair(
                        auxillary::parse_ident,
                        // Parse the optional (major, minor, patch)-triplet
                        comb::opt(seq::delimited(
                            tag_token!('t, 's, Token::LeftBracket),
                            seq::separated_pair(
                                literals::parse,
                                tag_token!('t, 's, Token::Comma),
                                seq::separated_pair(
                                    literals::parse,
                                    tag_token!('t, 's, Token::Comma),
                                    literals::parse,
                                ),
                            ),
                            tag_token!('t, 's, Token::RightBracket),
                        )),
                    )
                )),
                tag_token!('t, 's, Token::Semicolon),
            )),
            |(import, (package, version), semicolon): (&Token, (Identifier, Option<(Literal, (Literal, Literal))>), &Token)| {
                Statement {
                    kind : StatementKind::Import {
                        name    : package,
                        version : version.map(|(major, (minor, patch))| (major, minor, patch)),
                    },
                    annots : annots.into(),
                    range : Some(TextRange::new(import.start_of(), semicolon.end_of())),
                }
            },
        )(input)
    }
}

/// Parses a function definition from the start of the given token stream.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed function definition.
/// 
/// # Errors
/// This function errors if we failed to parse a definition for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn func_def<'t, 's, E>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, FunctionDef, E>
where
    E: Error<'t, 's>,
{
    comb::map(
        seq::pair(
            tag_token!('t, 's, Token::Func),
            nom::error::context("function definition", comb::cut(
                seq::tuple((
                    auxillary::parse_ident,
                    seq::delimited(
                        tag_token!('t, 's, Token::LeftParen),
                        multi::separated_list0(tag_token!('t, 's, Token::Comma), arg_def),
                        tag_token!('t, 's, Token::RightParen),
                    ),
                    comb::opt(seq::preceded(
                        tag_token!('t, 's, Token::Colon),
                        auxillary::parse_type,
                    )),
                    blocks::parse,
                )),
            )),
        ),
        |(func, (name, args, ret_type, body)): (&Token, (Identifier, Vec<ArgDef>, Option<DataType>, Block))| {
            let range: Option<TextRange> = body.range.map(|r| TextRange::new(func.start_of(), r.end));
            FunctionDef {
                name,
                args,
                ret : ret_type.unwrap_or(DataType::any()),
                body,

                range,
            }
        }
    )(input)
}

/// Creates a parser for class definitions that incorporates the given annotations.
/// 
/// # Arguments
/// - `annots`: The list of parsed annotations to add to the this statement.
/// 
/// # Returns
/// A new closure that parses the import statement á lá nom.
fn class_def<'t, 's, 'a, E>(annots: &'a [Annotation]) -> impl 'a + FnMut(Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, E>
where
    's: 't,
    E: Error<'t, 's>,
{
    |input| {
        comb::map(
            seq::pair(
                tag_token!('t, 's, Token::Class),
                nom::error::context("class definition", comb::cut(
                    seq::tuple((
                        auxillary::parse_ident,
                        seq::preceded(
                            tag_token!('t, 's, Token::LeftBrace),
                            multi::many0(member_def),
                        ),
                        tag_token!('t, 's, Token::RightBrace),
                    ))
                )),
            ),
            |(class, (name, members, rbrace)): (&Token, (Identifier, Vec<ClassMemberDef>, &Token))| {
                Statement {
                    kind : StatementKind::ClassDef {
                        name,
                        defs : members,
                    },
                    annots : annots.into(),
                    range  : Some(TextRange::new(class.start_of(), rbrace.end_of())),
                }
            }
        )(input)
    }
}

/// Creates a parser for variable definitions (let-assign-statements) that incorporates the given annotations.
/// 
/// # Arguments
/// - `annots`: The list of parsed annotations to add to the this statement.
/// 
/// # Returns
/// A new closure that parses the import statement á lá nom.
fn var_def<'t, 's, 'a, E>(annots: &'a [Annotation]) -> impl 'a + FnMut(Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, E>
where
    's: 't,
    E: Error<'t, 's>,
{
    |input| {
        comb::map(
            seq::pair(
                tag_token!('t, 's, Token::Let),
                nom::error::context("variable definition/let assign", comb::cut(
                    seq::tuple((
                        auxillary::parse_ident,
                        comb::opt(seq::preceded(
                            tag_token!('t, 's, Token::Colon),
                            auxillary::parse_type,
                        )),
                        comb::opt(seq::preceded(
                            tag_token!('t, 's, Token::Assign),
                            expressions::parse,
                        )),
                        tag_token!('t, 's, Token::Semicolon),
                    )),
                )),
            ),
            |(let_kw, (name, data_type, value, semicolon)): (&Token, (Identifier, Option<DataType>, Option<Expression>, &Token))| {
                Statement {
                    kind : StatementKind::VarDef {
                        name,
                        data_type : data_type.unwrap_or_else(DataType::any),
                        value,
                    },
                    annots : annots.into(),
                    range  : Some(TextRange::new(let_kw.start_of(), semicolon.end_of())),
                }
            }
        )(input)
    }
}


/// Creates a parser for for-loops that incorporates the given annotations.
/// 
/// # Arguments
/// - `annots`: The list of parsed annotations to add to the this statement.
/// 
/// # Returns
/// A new closure that parses the import statement á lá nom.
fn for_loop<'t, 's, 'a, E>(annots: &'a [Annotation]) -> impl 'a + FnMut(Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, E>
where
    's: 't,
    E: Error<'t, 's>,
{
    |input| {
        comb::map(
            seq::pair(
                tag_token!('t, 's, Token::For),
                nom::error::context("for-loop", comb::cut(
                    seq::pair(
                        seq::delimited(
                            tag_token!('t, 's, Token::LeftParen),
                            seq::pair(
                                seq::separated_pair(
                                    expressions::parse,
                                    tag_token!('t, 's, Token::To),
                                    expressions::parse,
                                ),
                                comb::opt(seq::preceded(
                                    tag_token!('t, 's, Token::Step),
                                    literals::parse,
                                )),
                            ),
                            tag_token!('t, 's, Token::RightParen),
                        ),
                        blocks::parse,
                    ),
                )),
            ),
            |(for_kw, (((start, stop), step), body)): (&Token, (((Expression, Expression), Option<Literal>), Block))| {
                let range: Option<TextRange> = body.range.map(|r| TextRange::new(for_kw.start_of(), r.end));
                Statement {
                    kind : StatementKind::For {
                        start,
                        stop,
                        step,

                        block : body,
                    },
                    annots : annots.into(),
                    range,
                }
            }
        )(input)
    }
}

/// Creates a parser for while-loops that incorporates the given annotations.
/// 
/// # Arguments
/// - `annots`: The list of parsed annotations to add to the this statement.
/// 
/// # Returns
/// A new closure that parses the import statement á lá nom.
fn while_loop<'t, 's, 'a, E>(annots: &'a [Annotation]) -> impl 'a + FnMut(Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, E>
where
    's: 't,
    E: Error<'t, 's>,
{
    |input| {
        comb::map(
            seq::pair(
                tag_token!('t, 's, Token::While),
                nom::error::context("while-loop", comb::cut(
                    seq::pair(
                        seq::delimited(
                            tag_token!('t, 's, Token::LeftParen),
                            expressions::parse,
                            tag_token!('t, 's, Token::RightParen),
                        ),
                        blocks::parse,
                    ),
                )),
            ),
            |(for_kw, (cond, body)): (&Token, (Expression, Block))| {
                let range: Option<TextRange> = body.range.map(|r| TextRange::new(for_kw.start_of(), r.end));
                Statement {
                    kind : StatementKind::While {
                        cond,
                        block : body,
                    },
                    annots : annots.into(),
                    range,
                }
            }
        )(input)
    }
}

/// Creates a parser for return-statements that incorporates the given annotations.
/// 
/// # Arguments
/// - `annots`: The list of parsed annotations to add to the this statement.
/// 
/// # Returns
/// A new closure that parses the import statement á lá nom.
fn return_stmt<'t, 's, 'a, E>(annots: &'a [Annotation]) -> impl 'a + FnMut(Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, E>
where
    's: 't,
    E: Error<'t, 's>,
{
    |input| {
        comb::map(
            seq::pair(
                tag_token!('t, 's, Token::Return),
                nom::error::context("return statement", comb::cut(
                    seq::pair(
                        comb::opt(expressions::parse),
                        tag_token!('t, 's, Token::Semicolon),
                    ),
                )),
            ),
            |(return_kw, (value, semicolon)): (&Token, (Option<Expression>, &Token))| {
                Statement {
                    kind : StatementKind::Return {
                        value,
                    },
                    annots : annots.into(),
                    range  : Some(TextRange::new(return_kw.start_of(), semicolon.end_of())),
                }
            },
        )(input)
    }
}



/// Creates a parser for assign-statements that incorporates the given annotations.
/// 
/// # Arguments
/// - `annots`: The list of parsed annotations to add to the this statement.
/// 
/// # Returns
/// A new closure that parses the import statement á lá nom.
fn assign_stmt<'t, 's, 'a, E>(annots: &'a [Annotation]) -> impl 'a + FnMut(Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, E>
where
    's: 't,
    E: Error<'t, 's>,
{
    |input| {
        comb::map(
            seq::pair(
                seq::pair(
                    seq::terminated(
                        auxillary::parse_ident,
                        tag_token!('t, 's, Token::Assign),
                    ),
                    nom::error::context("assign statement", comb::cut(expressions::parse)),
                ),
                tag_token!('t, 's, Token::Semicolon),
            ),
            |((name, value), semicolon): ((Identifier, Expression), &Token)| {
                let range: Option<TextRange> = name.range.map(|r| TextRange::new(r.start, semicolon.end_of()));
                Statement {
                    kind   : StatementKind::Assign { name, value },
                    annots : annots.into(),
                    range,
                }
            }
        )(input)
    }
}

/// Creates a parser for bare expressions that incorporates the given annotations.
/// 
/// # Arguments
/// - `annots`: The list of parsed annotations to add to the this statement.
/// 
/// # Returns
/// A new closure that parses the import statement á lá nom.
fn expr<'t, 's, 'a, E>(annots: &'a [Annotation]) -> impl 'a + FnMut(Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, E>
where
    's: 't,
    E: Error<'t, 's>,
{
    |input| {
        // First, always parse an expression...
        let (rem, expr): (Input, Expression) = expressions::parse(input)?;

        // In some cases, do _not_ expect a closing semicolon (toplevel block, if-statement or parallel statement)
        let (rem, range): (Input, Option<TextRange>) = match &expr.kind {
            // The exceptions (no semicolon)
            ExpressionKind::Block(_)       |
            ExpressionKind::If{ .. }       |
            ExpressionKind::Parallel{ .. } => (rem, expr.range),

            // The regular ones
            _ => {
                // Parse the additional semicolon, then return the remaining tokens and the updated range
                let (rem, semicolon): (Input, &Token) = nom::error::context("expression statement", comb::cut(tag_token!('t, 's, Token::Semicolon)))(rem)?;
                (rem, expr.range.map(|r| TextRange::new(r.start, semicolon.end_of())))
            }
        };

        // Checks out
        Ok((
            rem,
            Statement {
                kind   : StatementKind::Expression(expr),
                annots : annots.into(),
                range,
            },
        ))
    }
}





/***** LIBRARY *****/
/// Attempts to parse a statement off the top of the given tokenstream.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed statement.
/// 
/// # Errors
/// This function errors if we failed to parse a statement for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn parse<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, E> {
    // Parse the annotations matching this statement
    let (rem, annots): (Input, Vec<Annotation>) = comb::map(multi::many0(auxillary::parse_annots), |a| a.into_iter().flatten().collect())(input)?;

    // Pass it the actual statement to parse
    let (rem, stmt): (Input, Statement) = branch::alt((
        // Definitions
        import(&annots),
        comb::map(func_def, |d| {
            let range: Option<TextRange> = d.range;
            Statement{ kind: StatementKind::FunctionDef(d), annots: annots.clone(), range }
        }),
        class_def(&annots),
        var_def(&annots),

        // Control flow
        for_loop(&annots),
        while_loop(&annots),
        return_stmt(&annots),

        // Miscellaneous
        assign_stmt(&annots),
        expr(&annots),
    ))(rem)?;

    // Ok, return
    // Note we don't return the branch directly to satisfy `annot` lifetimes (lest it would be dropped before the fn is dropped, for some reason?)
    Ok((rem, stmt))
}
