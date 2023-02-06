//  TOKENS.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 16:33:22
//  Last edited:
//    06 Feb 2023, 17:47:59
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the tokens to which BraneScript and Bakery are scanned, and
//!   from which the AST is built.
// 

use enum_debug::EnumDebug;

use crate::ast::spec::TextRange;
use super::Input;


/***** LIBRARY *****/
/// Defines the tokens for both BraneScript and Bakery.
#[derive(Clone, Copy, Debug, EnumDebug)]
pub enum Token<'s> {
    // Identifiers
    /// Represents an identifier.
    Identifier(Input<'s>),

    // Literals
    /// A boolean value
    Boolean(Input<'s>),
    /// An integral number
    Integer(Input<'s>),
    /// A real/decimal number
    Real(Input<'s>),
    /// A string value
    String(Input<'s>),
    /// A null-value.
    Null(Input<'s>),

    // Keywords
    /// 'import'
    Import(Input<'s>),
    /// 'func'
    Func(Input<'s>),
    /// 'class'
    Class(Input<'s>),
    /// 'if'
    If(Input<'s>),
    /// 'else'
    Else(Input<'s>),
    /// 'for'
    For(Input<'s>),
    /// 'in'
    In(Input<'s>),
    /// 'to'
    To(Input<'s>),
    /// 'step'
    Step(Input<'s>),
    /// 'while'
    While(Input<'s>),
    /// 'parallel'
    Parallel(Input<'s>),
    /// 'return'
    Return(Input<'s>),
    /// 'let'
    Let(Input<'s>),
    /// 'new'
    New(Input<'s>),

    // Operators
    /// `==`
    Eq(Input<'s>),
    /// `!=`
    Ne(Input<'s>),
    /// `<`
    Lt(Input<'s>),
    /// `<=`
    Le(Input<'s>),
    /// `>`
    Gt(Input<'s>),
    /// `>=`
    Ge(Input<'s>),
    /// `&`
    And(Input<'s>),
    /// `|`
    Or(Input<'s>),
    /// `!`
    Not(Input<'s>),
    /// `+`
    Add(Input<'s>),
    /// `-`
    Min(Input<'s>),
    /// `*`
    Mul(Input<'s>),
    /// `/`
    Div(Input<'s>),
    /// `%`
    Mod(Input<'s>),
    /// `:=`
    Assign(Input<'s>),

    // Punctuation
    /// `.`
    Dot(Input<'s>),
    /// `,`
    Comma(Input<'s>),
    /// `:`
    Colon(Input<'s>),
    /// `;`
    Semicolon(Input<'s>),
    /// `(`
    LeftParen(Input<'s>),
    /// `)`
    RightParen(Input<'s>),
    /// `[`
    LeftBracket(Input<'s>),
    /// `]`
    RightBracket(Input<'s>),
    /// `{`
    LeftBrace(Input<'s>),
    /// `}`
    RightBrace(Input<'s>),
    /// `#`
    Hashtag(Input<'s>),
}

impl<'s> Token<'s> {
    /// Returns a range for this roken.
    #[inline]
    pub fn range(&self) -> TextRange {
        use Token::*;
        match self {
            Identifier(span) |

            Boolean(span) |
            Integer(span) |
            Real(span)    |
            String(span)  |
            Null(span)    |

            Import(span)   |
            Func(span)     |
            Class(span)    |
            If(span)       |
            Else(span)     |
            For(span)      |
            In(span)       |
            To(span)       |
            Step(span)     |
            While(span)    |
            Parallel(span) |
            Return(span)   |
            Let(span)      |
            New(span)      |
            
            Eq(span)     |
            Ne(span)     |
            Lt(span)     |
            Le(span)     |
            Gt(span)     |
            Ge(span)     |
            And(span)    |
            Or(span)     |
            Not(span)    |
            Add(span)    |
            Min(span)    |
            Mul(span)    |
            Div(span)    |
            Mod(span)    |
            Assign(span) |

            Dot(span)          |
            Comma(span)        |
            Colon(span)        |
            Semicolon(span)    |
            LeftParen(span)    |
            RightParen(span)   |
            LeftBracket(span)  |
            RightBracket(span) |
            LeftBrace(span)    |
            RightBrace(span)   |
            Hashtag(span)      => { TextRange::from(span) }
        }
    }
}
