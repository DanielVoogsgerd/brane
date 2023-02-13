//  WARNINGS.rs
//    by Lut99
// 
//  Created:
//    11 Feb 2023, 18:14:09
//  Last edited:
//    13 Feb 2023, 12:49:57
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
use crate::notes::PrettyNote;
use crate::ast::spec::TextRange;


/***** AUXILLARY *****/
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

warning_codes!{
    /// Occurs when some unknown annotation is used.
    UnknownAnnotation  => "unknown_annot",
    /// Occurs when an illegal kind of annotation is given for a warning code.
    IllegalWarningCode => "illegal_warn_code",
    /// Occurs when a warning code given in an annotation is unknown to us (meta).
    UnknownWarningCode => "unknown_warn_code",
    /// Occurs when an annotation is valid but unused.
    UnusedAnnotation   => "unused_annot",

    /// Occurs when some code can never be reached.
    DeadCode => "dead_code",
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
/// Describes warnings that may originate when working with annotations.
#[derive(Debug)]
pub enum AnnotationWarning {
    /// An annotation was given that we did not know.
    UnknownAnnotation{ raw: String, range: Option<TextRange> },
    /// The given warning code was of illegal annotation type.
    IllegalWarningCode{ range: Option<TextRange> },
    /// The given warning code was unknown to us.
    UnknownWarningCode{ raw: String, range: Option<TextRange> },
    /// An annotation was defined but not matched with any statement.
    UnusedAnnotation{ range: Option<TextRange> },
}
impl Display for AnnotationWarning {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use AnnotationWarning::*;
        match self {
            UnknownAnnotation{ raw, .. }  => write!(f, "Unknown annotation '{}'", raw),
            IllegalWarningCode{ .. }      => write!(f, "Warning codes can only be single identifiers"),
            UnknownWarningCode{ raw, .. } => write!(f, "Unknown warning code '{}'", raw),
            UnusedAnnotation{ .. }        => write!(f, "Unused annotation"),
        }
    }
}
impl Warning for AnnotationWarning {
    fn code(&self) -> WarningCode {
        use AnnotationWarning::*;
        match self {
            UnknownAnnotation{ .. }  => WarningCode::UnknownAnnotation,
            IllegalWarningCode{ .. } => WarningCode::IllegalWarningCode,
            UnknownWarningCode{ .. } => WarningCode::UnknownWarningCode,
            UnusedAnnotation{ .. }   => WarningCode::UnusedAnnotation,
        }
    }
}
impl PrettyWarning for AnnotationWarning {
    fn range(&self) -> Option<TextRange> {
        use AnnotationWarning::*;
        match self {
            UnknownAnnotation{ range, .. }  => *range,
            IllegalWarningCode{ range, .. } => *range,
            UnknownWarningCode{ range, .. } => *range,
            UnusedAnnotation{ range, .. }   => *range,
        }
    }

    fn notes(&self) -> Vec<Box<dyn PrettyNote>> {
        // No notes yet
        vec![]
    }
}
