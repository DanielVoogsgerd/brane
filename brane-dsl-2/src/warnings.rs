//  WARNINGS.rs
//    by Lut99
// 
//  Created:
//    11 Feb 2023, 18:14:09
//  Last edited:
//    26 May 2023, 10:16:47
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines warnings that may occur in the BraneScript/Bakery compiler.
// 

use std::convert::TryFrom;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::str::FromStr;

use console::{style, Style};

use crate::errors::print_range;
use crate::notes::{CompileNote, PrettyNote};
use crate::ast::spec::TextRange;
use crate::ast::types::DataType;


/***** HELPER MACROS *****/
/// Provides a template for writing the WarningCode struct
macro_rules! warning_codes {
    (
        $(
            $(#[$annot:ident $($args:tt)*])*
            $name:ident => $code:literal
        ),*
        $(,)?
    ) => {
        /// Defines shorthand (string) codes for referring to specific warnings. Mostly used to disable them.
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        pub enum WarningCode {
            $(
                $(#[$annot $($args)*])*
                $name,
            )*
        }

        impl WarningCode {
            /// Returns the string representation of this code.
            pub const fn code(&self) -> &'static str {
                match self {
                    $(
                        Self::$name => $code,
                    )*
                }
            }
        }

        impl Display for WarningCode {
            #[inline]
            fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
                write!(f, "{}", self.code())
            }
        }
        impl TryFrom<String> for WarningCode {
            type Error = ();

            #[inline]
            fn try_from(value: String) -> Result<Self, Self::Error> { Self::try_from(value.as_str()) }
        }
        impl TryFrom<&String> for WarningCode {
            type Error = ();

            #[inline]
            fn try_from(value: &String) -> Result<Self, Self::Error> { Self::try_from(value.as_str()) }
        }
        impl TryFrom<&str> for WarningCode {
            type Error = ();

            fn try_from(value: &str) -> Result<Self, Self::Error> {
                match value {
                    $(
                        $code => Ok(Self::$name),
                    )*

                    // Anything else causes warnings
                    _ => Err(()),
                }
            }
        }
        impl FromStr for WarningCode {
            type Err = ();

            #[inline]
            fn from_str(value: &str) -> Result<Self, Self::Err> { Self::try_from(value) }
        }
    };
}

/// Provides a template for warnings.
macro_rules! warning {
    (
        $(#[$outer:meta])*
        $wname:ident {
            $(
                $(#[$annot:ident $($args:tt)*])*
                $name:ident $({ $($field:ident : $field_ty:ty),* })?
            ),*
            $(,)?
        }$(,)?

        impl Display {
            $(
                $dname:ident $({ $($dfield:ident $(: $dfield_as:ident)?),* $(, ..)? })? => ($fmt:literal $(, $dval:expr)* $(,)?)
            ),*
            $(,)?
        }$(,)?

        impl Range {
            $(
                $rname:ident $({ $($rfield:ident $(: $rfield_as:ident)?),* $(, ..)? })? => $rval:expr,
            )*

            $(_ => $roval:expr $(,)?)?
        }$(,)?

        impl Notes {
            $(
                $nname:ident $({ $($nfield:ident $(: $nfield_as:ident)?),* $(, ..)? })? => $nval:expr,
            )*

            $(_ => $noval:expr $(,)?)?
        }$(,)?
    ) => {
        $(#[$outer])*
        #[derive(Debug)]
        pub enum $wname {
            $(
                $(#[$annot $($args)*])*
                $name $({ $($field : $field_ty),* })?,
            )*
        }
        impl Display for $wname {
            fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
                match self {
                    $(
                        $wname::$dname $({ $($dfield $(: $dfield_as)?,)* .. })? => write!(f, $fmt $(, $dval)*),
                    )*
                }
            }
        }
        impl Warning for $wname {
            fn code(&self) -> WarningCode {
                match self {
                    $(
                        $wname::$name { .. } => WarningCode::$name,
                    )*
                }
            }
        }
        impl PrettyWarning for $wname {
            fn range(&self) -> Option<TextRange> {
                match self {
                    $(
                        $wname::$rname $({ $($rfield $(: $rfield_as)?,)* .. })? => $rval,
                    )*
        
                    $(_ => $roval $(,)?)?
                }
            }

            fn notes(&self) -> Vec<Box<dyn PrettyNote>> {
                match self {
                    $(
                        $wname::$nname $({ $($nfield $(: $nfield_as)?,)* .. })? => $nval,
                    )*
        
                    $(_ => $noval,)?
                }
            }
        }
    }
}





/***** AUXILLARY *****/
warning_codes!{
    // Annotation warnings
    /// Occurs when some unknown annotation is used.
    UnknownAnnotation  => "unknown_annot",
    /// Occurs when an illegal kind of annotation is given for a warning code.
    IllegalWarningCode => "illegal_warn_code",
    /// Occurs when a warning code given in an annotation is unknown to us (meta).
    UnknownWarningCode => "unknown_warn_code",
    /// Occurs when an annotation is valid but unused.
    UnusedAnnotation   => "unused_annot",

    // Resolve warnings
    /// Occurs when a package shadows another package in the same scope.
    DuplicatePackageImport         => "duplicate_package",
    /// Occurs when a function shadows another function in the same scope.
    DuplicateFuncDefinition        => "duplicate_function",
    /// Occurs when a class shadows another class in the same scope.
    DuplicateClassDefinition       => "duplicate_class",
    /// Occurs when a property shadows another property in the same class.
    DuplicateClassMemberDefinition => "duplicate_class_member",

    // Type warnings
    /// Occurs when a block's return value is non-void and not used.
    NonVoidBlock => "non_void_block",
    /// Occurs when some code can never be reached.
    DeadCode     => "dead_code",
}



/// The pretty formatter for most notes.
#[derive(Debug)]
pub struct PrettyWarningFormatter<'w, 'f, 's> {
    /// The warning to format.
    warn : &'w dyn PrettyWarning,

    /// The name of the file we are compiling.
    file   : &'f str,
    /// The source text to use as context.
    source : &'s str,
}
impl<'w, 'f, 's> Display for PrettyWarningFormatter<'w, 'f, 's> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Get the ranges to print
        let range: Option<TextRange> = self.warn.range();

        // Print the main error
        if let Some(range) = range {
            // Write the top line
            writeln!(f, "{}: {}: {}", style(format!("{}:{}:{}", self.file, range.start.line, range.start.col)).bold(), style("note").green().bold(), self.warn)?;

            // Write the range
            print_range(f, range, self.source, Style::new().green().bold())?;
            writeln!(f)?;
        } else {
            // Write the top line without context
            writeln!(f, "{}: {}: {}", style(self.file), style("note").green().bold(), self.warn)?;
            writeln!(f)?;
        }

        // Done
        Ok(())
    }
}



/// A trait very similar to the `std::error::Error` trait except for warnings.
pub trait Warning: Debug + Display {
    /// Returns the code belonging to this Warning.
    fn code(&self) -> WarningCode;
}

/// A helper trait that we can use to cast a PrettyWarning-implemented (sized) type to a `&dyn PrettyWarning`.
pub trait PrettyWarningAsDyn {
    /// Returns this struct as a type-erased dynamic trait object reference.
    fn as_dyn(&self) -> &dyn PrettyWarning;
}
impl<T: PrettyWarning + Sized> PrettyWarningAsDyn for T {
    fn as_dyn(&self) -> &dyn PrettyWarning { self }
}

/// A trait very similar to the `PrettyError` trait in `errors.rs`, but for warnings.
pub trait PrettyWarning: Warning + PrettyWarningAsDyn {
    // Child-implemented
    /// Returns the range that this warning concerns itself with.
    /// 
    /// # Returns
    /// The matching range. If there is no range, then `None` is returned instead, and you can assume this Warning does not relate to the source.
    fn range(&self) -> Option<TextRange>;

    /// Returns any notes that should be attached when showing this warning.
    /// 
    /// # Returns
    /// A vector of `PrettyNote`-implementing `Note`-structures that describe the notes to add.
    fn notes(&self) -> Vec<Box<dyn PrettyNote>>;


    // Globally provided
    /// Returns a formatter for a Warning that writes it to some formatter with some additional context information attached to it.
    /// 
    /// # Arguments
    /// - `file`: Some name that represents the source. Typically the filename for a file, or something like "<stdin>" for stdin.
    /// - `source`: The source text to use for context. We assume that the positions in this note match that of the given source text.
    /// 
    /// # Returns
    /// A `PrettyWarningFormatter` that implements Display.
    #[inline]
    fn display_with_source<'n, 'f, 's>(&'n self, file: &'f str, source: &'s str) -> PrettyWarningFormatter<'n, 'f, 's> { PrettyWarningFormatter{ warn: self.as_dyn(), file, source } }
}





/***** LIBRARY *****/
/// Defines toplevel warnings for the BraneScript/Bakery compiler.
#[derive(Debug)]
pub enum DslWarning {
    /// Defines warnings that originate from the resolve traversal.
    Resolve(ResolveWarning),
    /// Defines warnings that originate from the annotation traversal.
    Annotation(AnnotationWarning),
}
impl Display for DslWarning {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use DslWarning::*;
        match self {
            Resolve(warn)    => write!(f, "{}", warn),
            Annotation(warn) => write!(f, "{}", warn),
        }
    }
}
impl Warning for DslWarning {
    fn code(&self) -> WarningCode {
        use DslWarning::*;
        match self {
            Resolve(warn)    => warn.code(),
            Annotation(warn) => warn.code(),
        }
    }
}
impl PrettyWarning for DslWarning {
    fn range(&self) -> Option<TextRange> {
        use DslWarning::*;
        match self {
            Resolve(warn)    => warn.range(),
            Annotation(warn) => warn.range(),
        }
    }

    fn notes(&self) -> Vec<Box<dyn PrettyNote>> {
        use DslWarning::*;
        match self {
            Resolve(warn)    => warn.notes(),
            Annotation(warn) => warn.notes(),
        }
    }
}
impl From<ResolveWarning> for DslWarning {
    #[inline]
    fn from(value: ResolveWarning) -> Self { DslWarning::Resolve(value) }
}
impl From<AnnotationWarning> for DslWarning {
    #[inline]
    fn from(value: AnnotationWarning) -> Self { DslWarning::Annotation(value) }
}



warning! {
    /// Describes warnings that may originate when resolving types.
    TypingWarning {
        /// A block that should return void does not.
        /// 
        /// `because_what` here gives answer to: 'Because of this ...'.
        NonVoidBlock { got_type: DataType, because_what: &'static str, because: Option<TextRange>, range: Option<TextRange> },
    },
    impl Display {
        NonVoidBlock { got_type } => ("Block evaluates to a {got_type} value, but this value is unused"),
    },
    impl Range {
        NonVoidBlock { range } => *range,
    },
    impl Notes {
        NonVoidBlock { because_what, because } => vec![ Box::new(CompileNote::BecauseOf{ what: because_what, range: *because }) ],
    },
}



warning! {
    /// Describes warnings taht may originate when resolving symbol table entries.
    ResolveWarning {
        /// A duplicate package declaration (import).
        DuplicatePackageImport { name: String, range: Option<TextRange>, prev: Option<TextRange> },
        /// A duplicate function delcaration.
        DuplicateFuncDefinition { name: String, range: Option<TextRange>, prev: Option<TextRange> },
        /// A duplicate class declaration.
        DuplicateClassDefinition{ name: String, range: Option<TextRange>, prev: Option<TextRange> },
        /// A duplicate proprety declaration.
        DuplicateClassMemberDefinition{ name: String, class: String, range: Option<TextRange>, prev: Option<TextRange>, prev_variant: &'static str },
    },
    impl Display {
        DuplicatePackageImport{ name }                => ("Package '{}' was already imported (not overwriting)", name),
        DuplicateFuncDefinition{ name }               => ("Function '{}' was already defined (not overwriting)", name),
        DuplicateClassDefinition{ name }              => ("Class '{}' was already defined (not overwriting)", name),
        DuplicateClassMemberDefinition{ name, class } => ("Class member '{}' was already defined in class '{}' (not overwriting)", name, class),
    },
    impl Range {
        DuplicatePackageImport{ range }         => *range,
        DuplicateFuncDefinition{ range }        => *range,
        DuplicateClassDefinition{ range }       => *range,
        DuplicateClassMemberDefinition{ range } => *range,
    },
    impl Notes {
        DuplicatePackageImport{ prev }                       => vec![ Box::new(CompileNote::DefinedAt{ what: "Package", range: *prev }) ],
        DuplicateFuncDefinition{ prev }                      => vec![ Box::new(CompileNote::DefinedAt{ what: "Function", range: *prev }) ],
        DuplicateClassDefinition{ prev }                     => vec![ Box::new(CompileNote::DefinedAt{ what: "Class", range: *prev }) ],
        DuplicateClassMemberDefinition{ prev, prev_variant } => vec![ Box::new(CompileNote::DefinedAt{ what: prev_variant, range: *prev }) ],
    },
}



warning! {
    /// Describes warnings that may originate when working with annotations.
    AnnotationWarning {
        /// An annotation was given that we did not know.
        UnknownAnnotation{ raw: String, range: Option<TextRange> },
        /// The given warning code was of illegal annotation type.
        IllegalWarningCode{ range: Option<TextRange> },
        /// The given warning code was unknown to us.
        UnknownWarningCode{ raw: String, range: Option<TextRange> },
        /// An annotation was defined but not matched with any statement.
        UnusedAnnotation{ range: Option<TextRange> },
    }
    impl Display {
        UnknownAnnotation{ raw }  => ("Unknown annotation '{}'", raw),
        IllegalWarningCode{}      => ("Warning codes can only be single identifiers"),
        UnknownWarningCode{ raw } => ("Unknown warning code '{}'", raw),
        UnusedAnnotation{}        => ("Unused annotation"),
    }
    impl Range {
        UnknownAnnotation{ range, .. }  => *range,
        IllegalWarningCode{ range, .. } => *range,
        UnknownWarningCode{ range, .. } => *range,
        UnusedAnnotation{ range, .. }   => *range,
    }
    impl Notes {
        _ => vec![],
    }
}
