//  STATEMENTS.rs
//    by Lut99
// 
//  Created:
//    08 Feb 2023, 10:19:08
//  Last edited:
//    18 Jun 2023, 20:18:19
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines parsers for statements as allowed in BraneScript.
// 

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


/***** HELPER SCANNING FUNCTIONS *****/
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
    branch::alt((
        // Annotations (note the reverse order to be able to better `cut()`)
        parent_annots,
        annots,

        // Definitions
        import,
        comb::map(func_def, |d| {
            let range: Option<TextRange> = d.range;
            Statement{ kind: StatementKind::FunctionDef(d), annots: vec![], range }
        }),
        class_def,
        var_def,

        // Control flow
        for_loop,
        while_loop,
        return_stmt,

        // Miscellaneous
        expr,
    ))(input)
}



/// Parses as much statements off the top of the given tokenstream as it can.
/// 
/// Note that the delimiters between statements (`;`) is dynamic based on the statements parsed. This means the grammar is not fully context-free anymore.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed statement.
/// 
/// # Errors
/// This function errors if we failed to parse a statement for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn parse_multi0<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Vec<Statement>, Error<'t, 's>> {
    // Parse the first statement if any, where we accept it if there was something else/nothing.
    let (mut input, mut stmts): (Input, Vec<Statement>) = match parse(input) {
        // Parsing success
        Ok((input, stmt)) => (input, vec![ stmt ]),

        // Handles the 'multi*0*` part
        Err(nom::Err::Error(_)) => { return Ok((input, vec![])); },

        // Incomplete output
        Err(nom::Err::Incomplete(needed)) => { return Err(nom::Err::Error(NomError::from_external_error(input, ErrorKind::Eof, ParseError::IncompleteStatement { needed }))) },
        // Parsing error (not just failed)
        Err(nom::Err::Failure(err))       => { return Err(nom::Err::Failure(err)); },
    };

    // Now we do the remainder of a bit of a custom multi::many0, which expects a separating `;` or not based on the statement just parsed
    loop {
        use StatementKind::*;

        // Parse a semicolon, perhaps, and then a statement, perhaps
        let (next_input, semicolon): (Input, Token) = match tag_token!('t, 's, Token::Semicolon)(input) {
            Ok(res)      => res,
            Err(nom::Err::Error())
        };

        // Parse the next element and use that to decide if we need a semicolon
        let (next_input, next): (Input, Statement) = parse(input)?;
        match stmts.last().unwrap() {
            // Needs semicolon
            Import { .. } => { /* Nothing to do */ (false, None) },

            FunctionDef(def) => {
                // This is not a method (hence the `None`)
                let changed: bool = trav_func_def(def, None, &mut *stack, warnings, errors);

                // Return whether we've changed, but a definition never evaluates
                (changed, None)
            },

            ClassDef { name: cname, defs, st_entry } => {
                // Go through the definitions to find if anything needs resolving there
                let mut changed: bool = false;
                for def in defs {
                    // Push the annotations for these definitions onto the stack
                    let mut stack = stack.frame(&def.annots);

                    // Match on the kind itself
                    match &mut def.kind {
                        ClassMemberDefKind::Property { name, data_type } => {
                            // Simply populate the property's type as necessary
                            let mut entry: RefMut<DelayedEntry<LocalClassEntry>> = st_entry.as_ref().unwrap().borrow_mut();
                            let pentry: &mut LocalClassEntryMember = entry.defs.get_mut(&name.name).unwrap_or_else(|| panic!("Property '{}' in class '{}' has not been assigned an entry after resolve phase", name.name, cname.name));
                            if let LocalClassEntryMember::Property(property) = pentry {
                                if property.data_type.is_any() {
                                    property.data_type = data_type.data_type.clone();
                                    changed = true;
                                } else if !data_type.data_type.is_any() && property.data_type != data_type.data_type {
                                    panic!("Property '{}' already has a type at definition ({}) but that does not match annotated type ({})", name.name, property.data_type, data_type.data_type);
                                }
                            } else {
                                panic!("Property '{}' in class '{}' has been assigned a non-property member entry ({:?})", name.name, cname.name, pentry.variant());
                            }
                        },

                        ClassMemberDefKind::Method(method) => {
                            // For methods, we do the same as for normal functions - which is just calling this bad boy
                            changed |= trav_func_def(method, Some((DataType::Class(st_entry.as_ref().unwrap().borrow().name.clone()), cname.range)), &mut *stack, warnings, errors);
                        },

                        // Annotations shouldn't occur anymore
                        ClassMemberDefKind::Annotation { .. }       |
                        ClassMemberDefKind::ParentAnnotation { .. } => { unreachable!(); },
                    }
                }

                // Done (a definition never evaluates)
                (changed, None)
            },

            VarDef { name, data_type, value, st_entry } => {
                // Attempt to resolve the variable definition
                let (mut var_changed, var_type): (bool, DataType) = {
                    let mut entry: RefMut<DelayedEntry<VarEntry>> = st_entry.as_ref().unwrap().borrow_mut();
                    let changed: bool = if entry.data_type.is_any() {
                        entry.data_type = data_type.data_type.clone();
                        true
                    } else if !data_type.data_type.is_any() && entry.data_type != data_type.data_type {
                        panic!("Variable '{}' already has a type at definition ({}) but that does not match annotated type ({})", name.name, entry.data_type, data_type.data_type);
                    } else {
                        false
                    };
                    (changed, entry.data_type.clone())
                };

                // Then, recurse into the value expression
                if let Some(value) = value {
                    let (expr_changed, expr_type): (bool, DataType) = trav_expr(value, &mut *stack, warnings, errors);
                    if var_type.is_any() {
                        let mut entry: RefMut<DelayedEntry<VarEntry>> = st_entry.as_ref().unwrap().borrow_mut();
                        entry.data_type = expr_type;
                        var_changed = true;
                    } else if !expr_type.is_any() && var_type != expr_type {
                        errors.insert(Error::VariableAssign { name: name.name.clone(), def_type: var_type, got_type: expr_type, source: st_entry.as_ref().unwrap().borrow().range, range: name.range });
                    }

                    // Then, recurse into the value expression (a definition never evaluates)
                    (var_changed | expr_changed, None)
                } else {
                    (var_changed, None)
                }
            },



            // Control flow
            For { name, start, stop, step, block, st_entry } => {
                // Resolve the loop variable
                let mut changed: bool = false;
                {
                    let mut entry: RefMut<DelayedEntry<VarEntry>> = st_entry.as_ref().unwrap().borrow_mut();
                    if entry.data_type.is_any() {
                        entry.data_type = DataType::Integer;
                        changed = true;
                    } else if entry.data_type != DataType::Integer {
                        panic!("For-loop loop variable '{}' has non-Integer type {}", name.name, entry.data_type);
                    }
                }

                // Next, recurse into each of the expressions:
                // start...
                let (echanged, etype): (bool, DataType) = trav_expr(start, &mut *stack, warnings, errors);
                changed |= echanged;
                if !etype.is_any() && etype != DataType::Integer {
                    errors.insert(Error::ForStart { got_type: etype, range: start.range });
                }
                // ...stop...
                let (echanged, etype): (bool, DataType) = trav_expr(stop, &mut *stack, warnings, errors);
                changed |= echanged;
                if !etype.is_any() && etype != DataType::Integer {
                    errors.insert(Error::ForStop { got_type: etype, range: stop.range });
                }
                // ...and step
                if let Some(step) = step {
                    let (echanged, etype): (bool, DataType) = trav_expr(step, &mut *stack, warnings, errors);
                    changed |= echanged;
                    if !etype.is_any() && etype != DataType::Integer {
                        errors.insert(Error::ForStep { got_type: etype, range: step.range });
                    }
                }

                // Now recurse into the block
                let (bchanged, btype): (bool, (DataType, Option<Option<TextRange>>)) = trav_block(block, &mut *stack, warnings, errors);
                // Only return if it returned non-void
                if !btype.0.is_any() && !btype.0.is_void() {
                    // Emit the warning if it's not surpressed
                    let warn: Warning = Warning::NonVoidBlock { got_type: btype.0, because_what: "for-loop", because: stmt.range, range: btype.1.unwrap_or(block.range) };
                    if stack.is_allowed(warn.code()) { warnings.insert(warn); }
                }

                // Done (a for-loop never evaluates)
                (changed | bchanged, None)
            },

            While { cond, block } => {
                // Recurse into the expresion first
                let (cchanged, ctype): (bool, DataType) = trav_expr(cond, &mut *stack, warnings, errors);
                if !ctype.is_any() && ctype != DataType::Boolean {
                    errors.insert(Error::WhileCondition { got_type: ctype, range: cond.range });
                }

                // Then recurse into the block
                let (bchanged, btype): (bool, (DataType, Option<Option<TextRange>>)) = trav_block(block, &mut *stack, warnings, errors);
                // Only return if it returned non-void
                if !btype.0.is_any() && !btype.0.is_void() {
                    // Emit the warning if it's not surpressed
                    let warn: Warning = Warning::NonVoidBlock { got_type: btype.0, because_what: "while-loop", because: stmt.range, range: btype.1.unwrap_or(block.range) };
                    if stack.is_allowed(warn.code()) { warnings.insert(warn); }
                }

                // Done (a while-loop never evaluates)
                (cchanged | bchanged, None)
            },

            Return { value } => {
                // Recurse into the return statement to process the value
                let changed: bool = if let Some(value) = value {
                    trav_expr(value, &mut *stack, warnings, errors).0
                } else {
                    false
                };

                // NOTE: We do nothing with the return value for the simple fact that we do return type checking in a separate pass.

                // Return (the `Return` never evalutes)
                (changed, None)
            },



            // Miscellaneous
            Expression(expr) => {
                // Evaluate the expression, which we can directly return (since the expression type _does_ evaluate)
                let (changed, data_type): (bool, DataType) = trav_expr(expr, &mut *stack, warnings, errors);
                (changed, Some(data_type))
            },

            // Should not occur anymore
            Annotation{ .. }       |
            ParentAnnotation{ .. } => { unreachable!(); },
        }
    }
}
