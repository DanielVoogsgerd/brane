//  NOTES.rs
//    by Lut99
// 
//  Created:
//    09 Feb 2023, 08:37:07
//  Last edited:
//    09 Feb 2023, 13:15:20
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
        let (message, range): (String, Option<TextRange>) = self.note.ranges();

        // Print the main error
        if let Some(range) = range {
            // Write the top line
            writeln!(f, "{}: {}: {}", style(format!("{}:{}:{}", self.file, range.start.line, range.start.col)).bold(), style("note").green().bold(), message)?;

            // Write the range
            print_range(f, range, self.source, Style::new().green().bold())?;
            writeln!(f)?;
        } else {
            // Write the top line without context
            writeln!(f, "{}: {}: {}", style(self.file), style("note").green().bold(), message)?;
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
    /// Returns the message and range that this note concerns itself with.
    /// 
    /// # Returns
    /// A tuple with the message to write and the matching range. If there is no range, then `None` is returned for the range instead, and you can assume this Note does not relate to the source.
    fn ranges(&self) -> (String, Option<TextRange>);


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
    fn ranges(&self) -> (String, Option<TextRange>) {
        use NomNote::*;
        match self {
            ScanContext{ range, .. }  => (self.to_string(), Some(*range)),
            ParseContext{ range, .. } => (self.to_string(), *range),
        }
    }
}
