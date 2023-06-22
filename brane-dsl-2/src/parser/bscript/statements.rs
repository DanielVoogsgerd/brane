//  STATEMENTS.rs
//    by Lut99
// 
//  Created:
//    08 Feb 2023, 10:19:08
//  Last edited:
//    22 Jun 2023, 19:21:21
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines parsers for statements as allowed in BraneScript.
// 

use enum_debug::EnumDebug as _;
use nom::IResult;
use nom::{branch, combinator as comb, multi, sequence as seq};
use nom::error::{ErrorKind, FromExternalError as _};

use crate::errors::{NomError, ParseError};
use crate::ast::spec::TextRange;
use crate::ast::auxillary::{DataType, Identifier};
use crate::ast::expressions::{Block, Expression, ExpressionKind, Literal};
use crate::ast::statements::{ArgDef, ClassMemberDef, ClassMemberDefKind, FunctionDef, RawAnnotation, RawAnnotationKind, Statement, StatementKind};
use crate::scanner::Token;
use crate::parser::{Error, Input};
use crate::parser::utils::{self, tag_token};
use super::{auxillary, blocks, expressions, literals};


/***** HELPER FUNCTIONS *****/
/// Helper function (to be used as a closure) that extends the range of the given statement over the given semicolon, and then returns the former only.
/// 
/// # Arguments
/// - `input`: The tuple encoding the [`Statement`] who's range to extend on the first position, and the [`Token`] to extend the statement's range over on the second.
/// 
/// # Returns
/// The given `input.0`, with its range extended.
#[inline]
fn extend_stmt_range_with_semicolon(input: (Statement, &Token)) -> Statement {
    let (mut stmt, semicolon): (Statement, &Token) = input;
    stmt.range = stmt.range.map(|s| TextRange::new(s.start, semicolon.range().end));
    stmt
}





/***** HELPER SCANNING FUNCTIONS *****/
// /// Parses a statement that never ends in a semicolon.
// /// 
// /// # Arguments
// /// - `input`: The new TokenStream to parse from.
// /// 
// /// # Returns
// /// A tuple of the remaining, unparsed tokenstream and the parsed statement.
// /// 
// /// # Errors
// /// This function errors if we failed to parse a statement for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
// fn stmt_with_semicolon<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, Error<'t, 's>> {
    
// }

/// Parses a mandatory semicolon.
/// 
/// This is the same as using [`tag_token!`] on a [`Token::Semicolon`], except that it [`comb::cut`]s the error and returns a custom [`ParseError::MissingSemicolon`] error upon failure.
/// 
/// # Arguments
/// - `input`: The TokenStream to parse a semicolon from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed semicolon.
/// 
/// # Errors
/// This function errors if we failed to parse a semicolon for whatever reason. In that case, a [`nom::Err::Failure`] should be expected.
#[inline]
fn mandatory_semicolon<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, &'t Token<'s>, Error<'t, 's>> {
    comb::cut(tag_token!('t, 's, Token::Semicolon))(input).map_err(|err| match err {
        // Convert the failure into a missing semicolon one
        nom::Err::Failure(_) => nom::Err::Failure(NomError::from_external_error(input, ErrorKind::Tag, ParseError::MissingSemicolon { what: "Expression".into() })),

        // Propagate the other errors, although they _should_ not occur due to the cut
        nom::Err::Error(err)      => nom::Err::Error(err),
        nom::Err::Incomplete(err) => nom::Err::Incomplete(err),
    })
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
fn annot<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, RawAnnotation, Error<'t, 's>> {
    branch::alt((
        // It's an identifier/expression pair
        comb::map(
            seq::separated_pair(
                auxillary::parse_ident,
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
                RawAnnotation {
                    kind : RawAnnotationKind::KeyValue(key, value),
                    range,
                }
            },
        ),

        // It's a key/list pair
        comb::map(
            seq::pair(
                auxillary::parse_ident,
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
            |(key, (list, rparen)): (Identifier, (Vec<RawAnnotation>, &Token))| {
                let range: Option<TextRange> = key.range.map(|r| TextRange::new(r.start, rparen.end_of()));
                RawAnnotation {
                    kind : RawAnnotationKind::KeyList(key, list),
                    range,
                }
            },
        ),

        // It's a single identifier
        comb::map(
            auxillary::parse_ident,
            |ident: Identifier| {
                let range: Option<TextRange> = ident.range;
                RawAnnotation {
                    kind : RawAnnotationKind::Identifier(ident),
                    range,
                }
            },
        ),
    ))(input)
}

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
fn arg_def<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, ArgDef, Error<'t, 's>> {
    comb::map(
        nom::error::context("an argument definition", seq::pair(
            auxillary::parse_ident,
            comb::opt(seq::preceded(
                tag_token!('t, 's, Token::Colon),
                comb::cut(auxillary::parse_type),
            )),
        )),
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
fn member_def<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, ClassMemberDef, Error<'t, 's>> {
    nom::error::context("class member definition", branch::alt((
        // A parent annotation
        comb::map(
            parent_annots,
            |annot: Statement| {
                if let StatementKind::ParentAnnotation{ annots } = annot.kind {
                    ClassMemberDef {
                        kind   : ClassMemberDefKind::ParentAnnotation{ annots },
                        annots : vec![],
                        range  : annot.range,
                    }
                } else {
                    unreachable!();
                }
            },
        ),
        // A normal annotation
        comb::map(
            annots,
            |annot: Statement| {
                if let StatementKind::Annotation{ annots } = annot.kind {
                    ClassMemberDef {
                        kind   : ClassMemberDefKind::Annotation{ annots },
                        annots : vec![],
                        range  : annot.range,
                    }
                } else {
                    unreachable!();
                }
            },
        ),

        // A field/property
        comb::map(
            nom::error::context("a property/field definition", seq::pair(
                auxillary::parse_ident,
                comb::cut(seq::pair(
                    seq::preceded(
                        tag_token!('t, 's, Token::Colon),
                        auxillary::parse_type,
                    ),
                    tag_token!('t, 's, Token::Semicolon),
                )),
            )),
            |(name, (data_type, semicolon)): (Identifier, (DataType, &Token))| {
                let range: Option<TextRange> = name.range.map(|r| TextRange::new(r.start, semicolon.end_of()));
                ClassMemberDef {
                    kind : ClassMemberDefKind::Property{
                        name,
                        data_type,
                    },
                    annots : vec![],
                    range,
                }
            }
        ),
        // A method
        comb::map(
            func_def,
            |def: FunctionDef| {
                let range: Option<TextRange> = def.range;
                ClassMemberDef {
                    kind   : ClassMemberDefKind::Method(def),
                    annots : vec![],
                    range,
                }
            },
        ),
    )))(input)
}





/***** STATEMENT SCANNING FUNCTIONS *****/
/// Parses a statement-annotation thing from the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The new TokenStream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed annotation statement.
/// 
/// # Errors
/// This function errors if we failed to parse an annotation for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn annots<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, Error<'t, 's>> {
    nom::error::context("an annotation", comb::map(
        seq::pair(
            tag_token!('t, 's, Token::Hashtag),
            comb::cut(seq::pair(
                seq::preceded(
                    tag_token!('t, 's, Token::LeftBracket),
                    utils::separated_list0(
                        tag_token!('t, 's, Token::Comma),
                        annot,
                    ),
                ),
                tag_token!('t, 's, Token::RightBracket),
            )),
        ),
        |(hashtag, (annots, rbrack)): (&Token, (Vec<RawAnnotation>, &Token))| {
            Statement {
                kind   : StatementKind::Annotation { annots },
                annots : vec![],
                range  : Some(TextRange::new(hashtag.start_of(), rbrack.end_of())),
            }
        }
    ))(input)
}

/// Parses a statement-annotation thing that acts on the parent from the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The tokenstream to parse off of.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed annotation statement.
/// 
/// # Errors
/// This function errors if we failed to parse an annotation for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn parent_annots<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, Error<'t, 's>> {
    nom::error::context("a parent annotation", comb::map(
        seq::pair(
            seq::terminated(tag_token!('t, 's, Token::Hashtag), tag_token!('t, 's, Token::Not)),
            comb::cut(seq::pair(
                seq::preceded(
                    tag_token!('t, 's, Token::LeftBracket),
                    utils::separated_list0(
                        tag_token!('t, 's, Token::Comma),
                        annot,
                    ),
                ),
                tag_token!('t, 's, Token::RightBracket),
            )),
        ),
        |(hashtag, (annots, rbrack)): (&Token, (Vec<RawAnnotation>, &Token))| {
            Statement {
                kind   : StatementKind::ParentAnnotation { annots },
                annots : vec![],
                range  : Some(TextRange::new(hashtag.start_of(), rbrack.end_of())),
            }
        }
    ))(input)
}



/// Parses a import-statement from the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The tokenstream to parse off of.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed import statement.
/// 
/// # Errors
/// This function errors if we failed to parse an import for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn import<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, Error<'t, 's>> {
    comb::map(
        nom::error::context("import statement", seq::tuple((
            tag_token!('t, 's, Token::Import),
            comb::cut(
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
            ),
            tag_token!('t, 's, Token::Semicolon),
        ))),
        |(import, (package, version), semicolon): (&Token, (Identifier, Option<(Literal, (Literal, Literal))>), &Token)| {
            Statement {
                kind : StatementKind::Import {
                    name    : package,
                    version : version.map(|(major, (minor, patch))| (major, minor, patch)),

                    st_entry : None,
                },
                annots : vec![],
                range  : Some(TextRange::new(import.start_of(), semicolon.end_of())),
            }
        },
    )(input)
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
fn func_def<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, FunctionDef, Error<'t, 's>> {
    comb::map(
        nom::error::context("a function definition", seq::pair(
            tag_token!('t, 's, Token::Func),
            comb::cut(
                seq::tuple((
                    auxillary::parse_ident,
                    seq::delimited(
                        tag_token!('t, 's, Token::LeftParen),
                        utils::separated_list0(tag_token!('t, 's, Token::Comma), arg_def),
                        tag_token!('t, 's, Token::RightParen),
                    ),
                    comb::opt(seq::preceded(
                        tag_token!('t, 's, Token::Colon),
                        auxillary::parse_type,
                    )),
                    blocks::parse,
                )),
            ),
        )),
        |(func, (name, args, ret_type, body)): (&Token, (Identifier, Vec<ArgDef>, Option<DataType>, Block))| {
            let range: Option<TextRange> = body.range.map(|r| TextRange::new(func.start_of(), r.end));
            FunctionDef {
                name,
                args,
                ret : ret_type.unwrap_or(DataType::any()),
                body,

                range,

                st_entry : None,
            }
        }
    )(input)
}

/// Parses a class-definition from the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The tokenstream to parse off of.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed class definition.
/// 
/// # Errors
/// This function errors if we failed to parse a definition for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn class_def<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, Error<'t, 's>> {
    comb::map(
        nom::error::context("a class definition", seq::pair(
            tag_token!('t, 's, Token::Class),
            comb::cut(
                seq::tuple((
                    auxillary::parse_ident,
                    seq::preceded(
                        tag_token!('t, 's, Token::LeftBrace),
                        multi::many0(member_def),
                    ),
                    tag_token!('t, 's, Token::RightBrace),
                ))
            ),
        )),
        |(class, (name, members, rbrace)): (&Token, (Identifier, Vec<ClassMemberDef>, &Token))| {
            Statement {
                kind : StatementKind::ClassDef {
                    name,
                    defs : members,

                    st_entry : None,
                },
                annots : vec![],
                range  : Some(TextRange::new(class.start_of(), rbrace.end_of())),
            }
        }
    )(input)
}

/// Parses a variable definition (i.e., let-assignment statement) from the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The tokenstream to parse off of.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed variable definition.
/// 
/// # Errors
/// This function errors if we failed to parse a definition for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn var_def<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, Error<'t, 's>> {
    comb::map(
        nom::error::context("a variable definition/let assign-statement", seq::pair(
            tag_token!('t, 's, Token::Let),
            comb::cut(
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
            ),
        )),
        |(let_kw, (name, data_type, value, semicolon)): (&Token, (Identifier, Option<DataType>, Option<Expression>, &Token))| {
            Statement {
                kind : StatementKind::VarDef {
                    name,
                    data_type : data_type.unwrap_or_else(DataType::any),

                    value,

                    st_entry : None,
                },
                annots : vec![],
                range  : Some(TextRange::new(let_kw.start_of(), semicolon.end_of())),
            }
        }
    )(input)
}



/// Parses a for-loop from the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The tokenstream to parse off of.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed for loop.
/// 
/// # Errors
/// This function errors if we failed to parse a loop for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn for_loop<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, Error<'t, 's>> {
    comb::map(
        nom::error::context("a for-loop", seq::pair(
            tag_token!('t, 's, Token::For),
            comb::cut(
                seq::pair(
                    seq::delimited(
                        tag_token!('t, 's, Token::LeftParen),
                        seq::tuple((
                            auxillary::parse_ident,
                            seq::preceded(
                                tag_token!('t, 's, Token::From),
                                expressions::parse,
                            ),
                            seq::preceded(
                                tag_token!('t, 's, Token::To),
                                expressions::parse,
                            ),
                            comb::opt(seq::preceded(
                                tag_token!('t, 's, Token::Step),
                                expressions::parse,
                            )),
                        )),
                        tag_token!('t, 's, Token::RightParen),
                    ),
                    blocks::parse,
                ),
            ),
        )),
        |(for_kw, ((name, start, stop, step), body)): (&Token, ((Identifier, Expression, Expression, Option<Expression>), Block))| {
            let range: Option<TextRange> = body.range.map(|r| TextRange::new(for_kw.start_of(), r.end));
            Statement {
                kind : StatementKind::For {
                    name,
                    start,
                    stop,
                    step,

                    block : body,

                    st_entry : None,
                },
                annots : vec![],
                range,
            }
        }
    )(input)
}

/// Parses a while-loop from the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The tokenstream to parse off of.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed while loop.
/// 
/// # Errors
/// This function errors if we failed to parse a loop for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn while_loop<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, Error<'t, 's>> {
    comb::map(
        nom::error::context("a while-loop", seq::pair(
            tag_token!('t, 's, Token::While),
            comb::cut(
                seq::pair(
                    seq::delimited(
                        tag_token!('t, 's, Token::LeftParen),
                        expressions::parse,
                        tag_token!('t, 's, Token::RightParen),
                    ),
                    blocks::parse,
                ),
            ),
        )),
        |(for_kw, (cond, body)): (&Token, (Expression, Block))| {
            let range: Option<TextRange> = body.range.map(|r| TextRange::new(for_kw.start_of(), r.end));
            Statement {
                kind : StatementKind::While {
                    cond,
                    block : body,
                },
                annots : vec![],
                range,
            }
        }
    )(input)
}

/// Parses a return-statement from the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The tokenstream to parse off of.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed return statement.
/// 
/// # Errors
/// This function errors if we failed to parse a return for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn return_stmt<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, Error<'t, 's>> {
    comb::map(
        nom::error::context("a return statement", seq::pair(
            tag_token!('t, 's, Token::Return),
            comb::cut(
                seq::pair(
                    comb::opt(expressions::parse),
                    tag_token!('t, 's, Token::Semicolon),
                ),
            ),
        )),
        |(return_kw, (value, semicolon)): (&Token, (Option<Expression>, &Token))| {
            Statement {
                kind : StatementKind::Return {
                    value,
                },
                annots : vec![],
                range  : Some(TextRange::new(return_kw.start_of(), semicolon.end_of())),
            }
        },
    )(input)
}



/// Parses an expression-statement from the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The tokenstream to parse off of.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed expression statement.
/// 
/// # Errors
/// This function errors if we failed to parse an expression for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn expr<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, Error<'t, 's>> {
    comb::map(
        nom::error::context("an expression-statement", seq::pair(
            expressions::parse,
            comb::opt(tag_token!('t, 's, Token::Semicolon)),
        )),
        |(expression, semicolon): (Expression, Option<&Token>)| {
            // Switch on semicolon given or not
            match semicolon {
                Some(semicolon) => {
                    // We return the expression wrapped in a discard (only way to parse that).
                    let range: Option<TextRange> = expression.range.map(|r| TextRange::new(r.start, semicolon.end_of()));
                    Statement {
                        kind : StatementKind::Expression(Expression {
                            kind : ExpressionKind::Discard{ expr: Box::new(expression) },
                            range,
                        }),
                        annots : vec![],
                        range,
                    }
                },

                None => {
                    // We return the expression as-is
                    let range: Option<TextRange> = expression.range;
                    Statement {
                        kind   : StatementKind::Expression(expression),
                        annots : vec![],
                        range,
                    }
                },
            }
        },
    )(input)
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
pub(crate) fn parse<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Statement, Error<'t, 's>> {
    // branch::alt((
    //     // Annotations (note the reverse order to be able to better `cut()`)
    //     parent_annots,
    //     annots,

    //     // Definitions
    //     import,
    //     comb::map(func_def, |d| {
    //         let range: Option<TextRange> = d.range;
    //         Statement{ kind: StatementKind::FunctionDef(d), annots: vec![], range }
    //     }),
    //     class_def,
    //     var_def,

    //     // Control flow
    //     for_loop,
    //     while_loop,
    //     return_stmt,

    //     // Miscellaneous
    //     expr,
    // ))(input)

    // Note that we iterate until we find a non-empty semicolon
    loop {
        branch::alt((
            // Some statements never take semicolons
            comb::map(
                seq::pair(
                    branch::alt((
                        // Annotations (note the reverse order to be able to better `cut()`)
                        parent_annots,
                        annots,

                        // Definitions
                        comb::map(func_def, |d| {
                            let range: Option<TextRange> = d.range;
                            Statement{ kind: StatementKind::FunctionDef(d), annots: vec![], range }
                        }),
                        class_def,

                        // Control flow
                        for_loop,
                        while_loop,
                    )),
                    mandatory_semicolon,
                ),
                extend_stmt_range_with_semicolon,
            ),

            // Some statements always take semicolons
            branch::alt((
                // Definitions
                import,
                var_def,

                // Control flow
                return_stmt,
            )),

            // And one particular type of statement has a mandatory semicolon only if it's not the last statement
            // NOTE: We assume short-circuit behaviour here
            branch::alt((
                // The expression is the last one...
                // TODO: This is probably infinitely recursing
                seq::terminated(expr, comb::peek(comb::not(parse))),
                // ...OR it has a semicolon
                comb::map(
                    seq::pair(expr, mandatory_semicolon),
                    extend_stmt_range_with_semicolon,
                ),
            )),
        ))(input)?
    }
}



// /// Parses as much statements off the top of the given tokenstream as it can.
// /// 
// /// Note that the delimiters between statements (`;`) is dynamic based on the statements parsed. This means the grammar is not fully context-free anymore.
// /// 
// /// # Arguments
// /// - `input`: The new tokenstream to parse from.
// /// 
// /// # Returns
// /// A tuple of the remaining, unparsed tokenstream and the parsed statement.
// /// 
// /// # Errors
// /// This function errors if we failed to parse a statement for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
// pub(crate) fn parse_multi0<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Vec<Statement>, Error<'t, 's>> {
//     // Parse the first statement if any, where we accept it if there was something else/nothing.
//     let (mut input, mut stmts): (Input, Vec<Statement>) = match parse(input) {
//         // Parsing success
//         Ok((input, stmt)) => (input, vec![ stmt ]),

//         // Handles the 'multi*0*` part
//         Err(nom::Err::Error(_)) => { return Ok((input, vec![])); },

//         // Incomplete output
//         Err(nom::Err::Incomplete(needed)) => { return Err(nom::Err::Error(NomError::from_external_error(input, ErrorKind::Eof, ParseError::IncompleteStatement { needed }))) },
//         // Parsing error (not just failed)
//         Err(nom::Err::Failure(err))       => { return Err(nom::Err::Failure(err)); },
//     };

//     // Now we do the remainder of a bit of a custom multi::many0, which expects a separating `;` or not based on the statement just parsed
//     loop {
//         use StatementKind::*;

//         // Parse a semicolon, perhaps, and then a statement, perhaps
//         let (next_input, semicolon): (Input, Result<&Token, nom::Err<NomError<Input>>>) = match multi::many1(tag_token!('t, 's, Token::Semicolon))(input) {
//             // Upon success, we return the statement
//             Ok((input, mut semicolons)) => (input, Ok(semicolons.swap_remove(0))),

//             // Upon an error (or incomplete), we continue but mark we did not find it
//             Err(nom::Err::Error(err))      => (input, Err(nom::Err::Error(err))),
//             Err(nom::Err::Incomplete(err)) => (input, Err(nom::Err::Incomplete(err))),

//             // Upon a failure, we should quit immediately
//             Err(nom::Err::Failure(err)) => { return Err(nom::Err::Failure(err)); },
//         };
//         let (next_input, next): (Input, Result<Statement, nom::Err<NomError<Input>>>) = match parse(next_input) {
//             // Upon success, we return the statement
//             Ok((input, stmt)) => (input, Ok(stmt)),

//             // Upon an error (or incomplete), we continue but mark we did not find it
//             Err(nom::Err::Error(err))      => (input, Err(nom::Err::Error(err))),
//             Err(nom::Err::Incomplete(err)) => (input, Err(nom::Err::Incomplete(err))),

//             // Upon a failure, we should quit immediately
//             Err(nom::Err::Failure(err)) => { return Err(nom::Err::Failure(err)); },
//         };

//         // Now see if we needed the semicolon
//         let mut last: &mut Statement = stmts.last_mut().unwrap();
//         match &last.kind {
//             Expression(_) => {
//                 // Only need a semicolon if there is another statement to parse
//                 if next.is_ok() {
//                     // Get the semicolon
//                     let semicolon: &Token = match semicolon {
//                         Ok(semicolon) => semicolon,
//                         Err(_)        => { return Err(nom::Err::Failure(NomError::from_external_error(input, ErrorKind::Tag, ParseError::MissingSemicolon { what: last.kind.variant().to_string() }))) },
//                     };

//                     // Add it to the range of this statement
//                     last.range = last.range.map(|r| TextRange::new(r.start, semicolon.range().end));
//                 }
//             },

//             // Always needs semicolon
//             Import { .. } |
//             VarDef { .. } |
//             Return { .. } => {
//                 // Get the semicolon
//                 let semicolon: &Token = match semicolon {
//                     Ok(semicolon) => semicolon,
//                     Err(_)        => { return Err(nom::Err::Failure(NomError::from_external_error(input, ErrorKind::Tag, ParseError::MissingSemicolon { what: last.kind.variant().to_string() }))) },
//                 };

//                 // Add it to the range of this statement
//                 last.range = last.range.map(|r| TextRange::new(r.start, semicolon.range().end));
//             },

//             // Never needs semicolon
//             Annotation{ .. }       |
//             ParentAnnotation{ .. } |
//             FunctionDef(_)         |
//             ClassDef { .. }        |
//             For { .. }             |
//             While { .. }           => {},
//         }

//         // Add the next to the list and consider it as last
//         input = next_input;
//         stmts.push(next?);
//     }
// }
