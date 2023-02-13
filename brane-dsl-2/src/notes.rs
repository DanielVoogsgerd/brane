//  NOTES.rs
//    by Lut99
// 
//  Created:
//    09 Feb 2023, 08:37:07
//  Last edited:
//    13 Feb 2023, 17:17:26
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines enums that are very much like errors, but are semantically
//!   different. The Notes are used to provide context to an error or
//!   warning.
// 

use std::fmt::{Debug, Display, Formatter, Result as FResult};

use console::{style, Style};

use crate::errors::print_range;
use crate::ast::spec::TextRange;


/***** AUXILLARY *****/
/// The pretty formatter for most notes.
#[derive(Debug)]
pub struct PrettyNoteFormatter<'n, 'f, 's> {
    /// The note to format.
    note : &'n dyn PrettyNote,

    /// The name of the file we are compiling.
    file   : &'f str,
    /// The source text to use as context.
    source : &'s str,
}
impl<'n, 'f, 's> Display for PrettyNoteFormatter<'n, 'f, 's> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Get the ranges to print
        let range: Option<TextRange> = self.note.range();

        // Print the main error
        if let Some(range) = range {
            // Write the top line
            writeln!(f, "{}: {}: {}", style(format!("{}:{}:{}", self.file, range.start.line, range.start.col)).bold(), style("note").green().bold(), self.note)?;

            // Write the range
            print_range(f, range, self.source, Style::new().green().bold())?;
            writeln!(f)?;
        } else {
            // Write the top line without context
            writeln!(f, "{}: {}: {}", style(self.file), style("note").green().bold(), self.note)?;
            writeln!(f)?;
        }

        // Done
        Ok(())
    }
}



/// A trait very similar to the `std::error::Error` trait except for Notes.
pub trait Note: Debug + Display {}

/// A helper trait that we can use to cast a PrettyNote-implemented (sized) type to a `&dyn PrettyNote`.
pub trait PrettyNoteAsDyn {
    /// Returns this struct as a type-erased dynamic trait object reference.
    fn as_dyn(&self) -> &dyn PrettyNote;
}
impl<T: PrettyNote + Sized> PrettyNoteAsDyn for T {
    fn as_dyn(&self) -> &dyn PrettyNote { self }
}

/// A trait very similar to the `PrettyError` trait in `errors.rs`, but for Notes.
pub trait PrettyNote: Note + PrettyNoteAsDyn {
    // Child-implemented
    /// Returns the range that this note concerns itself with.
    /// 
    /// # Returns
    /// The matching range. If there is no range, then `None` is returned for the range instead, and you can assume this Note does not relate to the source.
    fn range(&self) -> Option<TextRange>;


    // Globally provided
    /// Returns a formatter for a Note that writes it to some formatter with some additional context information attached to it.
    /// 
    /// # Arguments
    /// - `file`: Some name that represents the source. Typically the filename for a file, or something like "<stdin>" for stdin.
    /// - `source`: The source text to use for context. We assume that the positions in this note match that of the given source text.
    /// 
    /// # Returns
    /// A `PrettyNoteFormatter` that implements Display.
    #[inline]
    fn display_with_source<'n, 'f, 's>(&'n self, file: &'f str, source: &'s str) -> PrettyNoteFormatter<'n, 'f, 's> { PrettyNoteFormatter{ note: self.as_dyn(), file, source } }
}





/***** LIBRARY *****/
/// Defines generic notes that can be used in many places.
#[derive(Debug)]
pub enum CompileNote {
    /// Defines where something was defined.
    /// 
    /// `what` here gives answer to: `... defined here`.
    DefinedAt{ what: &'static str, range: Option<TextRange> },
    /// Notes that whatever we are talking about is defined in something else.
    /// 
    /// `what` here gives answer to: `Defined in ...`.
    DefinedIn{ what: &'static str, range: Option<TextRange> },
    /// Notes that whatever we are talking about is part of something else.
    /// 
    /// `what` here gives answer to: `Part of ...`.
    PartOf{ what: &'static str, range: Option<TextRange> },
}
impl Display for CompileNote {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use CompileNote::*;
        match self {
            DefinedAt{ what, .. } => write!(f, "{} defined here", what),
            DefinedIn{ what, .. } => write!(f, "Defined in {}", what),
            PartOf{ what, .. }    => write!(f, "Part of {}", what),
        }
    }
}
impl Note for CompileNote {}
impl PrettyNote for CompileNote {
    fn range(&self) -> Option<TextRange> {
        use CompileNote::*;
        match self {
            DefinedAt{ range, .. } => *range,
            DefinedIn{ range, .. } => *range,
            PartOf{ range, .. }    => *range,
        }
    }
}



/// Defines the types of notes that may occur when scanning or parsing.
#[derive(Debug)]
pub enum NomNote {
    /// A piece of context given during scanning.
    ScanContext{ context: &'static str, range: TextRange },
    /// A piece of context given during parsing.
    ParseContext{ context: &'static str, range: Option<TextRange> },
}
impl Display for NomNote {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use NomNote::*;
        match self {
            ScanContext{ context, .. }  => write!(f, "The above occurred while scanning {}", context),
            ParseContext{ context, .. } => write!(f, "The above occurred while parsing {}", context),
        }
    }
}
impl Note for NomNote {}
impl PrettyNote for NomNote {
    fn range(&self) -> Option<TextRange> {
        use NomNote::*;
        match self {
            ScanContext{ range, .. }  => Some(*range),
            ParseContext{ range, .. } => *range,
        }
    }
}
