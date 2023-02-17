//  TOPLEVEL.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 15:33:27
//  Last edited:
//    17 Feb 2023, 15:35:38
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines toplevel things in the AST such as a program or a statement.
// 

use enum_debug::EnumDebug;

use super::spec::{Annotation, Node, TextRange};
use super::symbol_tables::{DelayedEntryPtr, LocalClassEntry, LocalFuncEntry, PackageEntry, VarEntry};
use super::auxillary::{DataType, Identifier};
use super::expressions::{Block, Expression, Literal};


/***** LIBRARY *****/
/// A Statement is the smallest, still-valid snippet of a BraneScript/Bakery program.
#[derive(Clone, Debug)]
pub struct Statement {
    /// Any specific implementations of a statement.
    pub kind  : StatementKind,
    /// The range in the source text for this statement.
    pub range : Option<TextRange>,

    /// Any _parsed_ annotations that are added to this statement.
    pub annots : Vec<Annotation>,
}
impl Node for Statement {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}

/// Defines the StatementKind, which implements the specifics for each of the various statements.
#[derive(Clone, Debug, EnumDebug)]
pub enum StatementKind {
    // Annotations
    /// The most common case of annotation that annotates some subsequent statement with annotations.
    Annotation {
        /// The list of annotations defined.
        annots : Vec<RawAnnotation>,
    },

    /// A special case of annotation that is not attached to some statement from the outside, but rather from within.
    /// 
    /// Because it acts as a statement itself, can only be used in statement position.
    ParentAnnotation {
        /// The list of annotations defined.
        annots : Vec<RawAnnotation>,
    },



    // Definitions
    /// An import-statement defines an external package.
    Import {
        /// The name of the package which we import.
        name    : Identifier,
        /// An optional version, specified as a (major, minor, patch) triplet of integer literals.
        version : Option<(Literal, Literal, Literal)>,

        /// The symbol table entry for the declared package.
        st_entry : Option<DelayedEntryPtr<PackageEntry>>,
    },

    /// A definition of a function.
    FunctionDef(FunctionDef),

    /// A definition of a class.
    ClassDef {
        /// The name of the class.
        name : Identifier,
        /// The definitions in this class, which may be either properties or methods.
        defs : Vec<ClassMemberDef>,

        /// The symbol table entry for the declared class.
        st_entry : Option<DelayedEntryPtr<LocalClassEntry>>,
    },

    /// A definition of a variable (previously known as a let-expression).
    VarDef {
        /// The name of the variable.
        name      : Identifier,
        /// The data type of the variable.
        data_type : DataType,

        /// An optional value to assign to the variable.
        value : Option<Expression>,

        /// The symbol table entry for the declared variable.
        st_entry : Option<DelayedEntryPtr<VarEntry>>,
    },



    // Control flow
    /// A for-loop defines a series of statements that should be executed in succession, but with a predictable amount.
    For {
        /// The name of the loop identifier.
        name  : Identifier,
        /// The expression that computes the start value.
        start : Expression,
        /// The expression that computes the stop value.
        stop  : Expression,
        /// The expression that computes the step value.
        step  : Option<Literal>,

        /// The block of statements to be executed repeatedly.
        block : Block,

        /// The symbol table entry for the declared iterator variable.
        st_entry : Option<DelayedEntryPtr<VarEntry>>,
    },

    /// A while-loop defines a series of statements that should be executed in succession, but with an unpredictable amount.
    While {
        /// The condition to compute at the _start_ of every loop.
        cond  : Expression,
        /// The block of statements to be executed repeatedly.
        block : Block,
    },

    /// A return-statement escapes from the parent function or workflow.
    Return {
        /// The (optional) value to return.
        value : Option<Expression>,
    },



    // Miscellaneous
    /// An Expression defines some thing executable right here, right now.
    Expression(Expression),
}



/// Defines the possible annotations that are possible from a parsing perspective.
#[derive(Clone, Debug)]
pub struct RawAnnotation {
    /// The variant for this annotation.
    pub kind  : RawAnnotationKind,
    /// The range where we found it.
    pub range : Option<TextRange>,
}
impl Node for RawAnnotation {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}

/// Defines the variants of a RawAnnotation.
#[derive(Clone, Debug, EnumDebug)]
pub enum RawAnnotationKind {
    /// It's a separate identifier.
    Identifier(Identifier),
    /// It's a key/value pair.
    KeyValue(Identifier, Expression),
    /// It's a key/list pair.
    KeyList(Identifier, Vec<RawAnnotation>),
}



/// Defines how a function definition looks like.
#[derive(Clone, Debug)]
pub struct FunctionDef {
    /// The name of the function.
    pub name : Identifier,
    /// The list of arguments for this function.
    pub args : Vec<ArgDef>,
    /// The return type for this function.
    pub ret  : DataType,

    /// The body for this function.
    pub body : Block,

    /// The range in the source text for this definition (as a whole).
    pub range : Option<TextRange>,

    /// The symbol table entry for the declared function.
    pub st_entry : Option<DelayedEntryPtr<LocalFuncEntry>>,
}
impl Node for FunctionDef {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}

/// Defines the definition of a function argument.
/// 
/// May either be a normal argument or the special `Self` one.
#[derive(Clone, Debug)]
pub struct ArgDef {
    /// The name of the argument
    pub name      : Identifier,
    /// The data type of this argument.
    pub data_type : DataType,

    /// The range in the source text for this argument definition.
    pub range : Option<TextRange>,
}
impl Node for ArgDef {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}



/// Defines either one of two things: either a property definition or a function (method) definition.
#[derive(Clone, Debug)]
pub struct ClassMemberDef {
    /// The specific variant for this definition.
    pub kind   : ClassMemberDefKind,
    /// The list of parsed annotations that we keep for this member.
    pub annots : Vec<Annotation>,
    /// The range for this class member.
    pub range  : Option<TextRange>,
}
impl Node for ClassMemberDef {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}

/// Defines the variants for the class member definitions.
/// 
/// Note that annotations are here included as separated definition-like statements.
#[derive(Clone, Debug, EnumDebug)]
pub enum ClassMemberDefKind {
    // Annotations
    /// The most common case of annotation that annotates some subsequent statement with annotations.
    Annotation {
        /// The list of annotations defined.
        annots : Vec<RawAnnotation>,
    },

    /// A special case of annotation that is not attached to some statement from the outside, but rather from within.
    /// 
    /// Because it acts as a statement itself, can only be used in statement position.
    ParentAnnotation {
        /// The list of annotations defined.
        annots : Vec<RawAnnotation>,
    },



    // Real definitions
    /// It's a property
    Property {
        /// The name of the property.
        name      : Identifier,
        /// The data type of the property.
        data_type : DataType,
    },

    /// It's a method.
    Method(FunctionDef),
}
