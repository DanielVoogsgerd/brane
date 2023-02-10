//  PRINT AST.rs
//    by Lut99
// 
//  Created:
//    10 Feb 2023, 19:25:20
//  Last edited:
//    10 Feb 2023, 19:50:15
//  Auto updated?
//    Yes
// 
//  Description:
//!   Compiler traversal for printing the current AST.
// 

use std::fmt::{Display, Formatter, Result as FResult};
use std::io::Write;

use crate::ast::spec::Node;
use crate::ast::auxillary::Identifier;
use crate::ast::expressions::{Expression, ExpressionKind, Literal, LiteralKind};
use crate::ast::statements::{Statement, StatementKind};
use crate::ast::toplevel::Program;


/***** HELPER TRAITS *****/
/// Provides the convience 'trav()` function on each element in the tree
trait TravFormattable {
    /// Returns a TravFormatter for this struct.
    fn trav<'e>(&'e self) -> TravFormatter<'e, Self>;
}
impl<T: Node> TravFormattable for T {
    #[inline]
    fn trav<'e>(&'e self) -> TravFormatter<'e, Self> { TravFormatter{ elem: self } }
}





/***** TRAVERSAL STRUCTS *****/
/// Implements Display for one of the AST types.
#[derive(Debug)]
struct TravFormatter<'e, E: ?Sized> {
    /// The element to format
    elem : &'e E,
}



impl<'e> Display for TravFormatter<'e, Statement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Match on the specific kind of statement
        use StatementKind::*;
        match &self.elem.kind {
            // Definitions
            Import{ name, version } => {
                // Print the statement
                writeln!(f, "import {}{};", name.trav(), if let Some(version) = version { format!("[{}.{}.{}]", version.0.trav(), version.1.trav(), version.2.trav()) } else { String::new() })?;
            },

            // // Definitions
            // /// An import-statement defines an external package.
            // Import {
            //     /// The name of the package which we import.
            //     name    : Identifier,
            //     /// An optional version, specified as a (major, minor, patch) triplet of integer literals.
            //     version : Option<(Literal, Literal, Literal)>,
            // },
        
            // /// A definition of a function.
            // FunctionDef(FunctionDef),
        
            // /// A definition of a class.
            // ClassDef {
            //     /// The name of the class.
            //     name : Identifier,
            //     /// The definitions in this class, which may be either properties or methods.
            //     defs : Vec<ClassMemberDef>,
            // },
        
            // /// A definition of a variable (previously known as a let-expression).
            // VarDef {
            //     /// The name of the variable.
            //     name      : Identifier,
            //     /// The data type of the variable.
            //     data_type : DataType,
        
            //     /// An optional value to assign to the variable.
            //     value : Option<Expression>,
            // },
        
        
        
            // // Control flow
            // /// A for-loop defines a series of statements that should be executed in succession, but with a predictable amount.
            // For {
            //     /// The name of the loop identifier.
            //     name  : Identifier,
            //     /// The expression that computes the start value.
            //     start : Expression,
            //     /// The expression that computes the stop value.
            //     stop  : Expression,
            //     /// The expression that computes the step value.
            //     step  : Option<Literal>,
        
            //     /// The block of statements to be executed repeatedly.
            //     block : Block,
            // },
        
            // /// A while-loop defines a series of statements that should be executed in succession, but with an unpredictable amount.
            // While {
            //     /// The condition to compute at the _start_ of every loop.
            //     cond  : Expression,
            //     /// The block of statements to be executed repeatedly.
            //     block : Block,
            // },
        
            // /// A return-statement escapes from the parent function or workflow.
            // Return {
            //     /// The (optional) value to return.
            //     value : Option<Expression>,
            // },
        
        
        
            // // Miscellaneous
            // /// An Expression defines some thing executable right here, right now.
            // Expression(Expression),
        }

        // Done
        Ok(())
    }
}



impl<'e> Display for TravFormatter<'e, Expression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // // Match on the specific kind of statement
        // use ExpressionKind::*;
        // match &self.elem.kind {

        // }

        // Done
        Ok(())
    }
}




impl<'e> Display for TravFormatter<'e, Literal> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // // Match on the specific kind of literal
        // use LiteralKind::*;
        // match &self.elem.kind {

        // }

        // Done
        Ok(())
    }
}



impl<'e> Display for TravFormatter<'e, Identifier> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!()
    }
}





/***** LIBRARY *****/
/// Runs a full traversal on the given AST to print it to the given formatter.
/// 
/// This traversal does not mutate or 
/// 
/// # Arguments
/// - `out`: The `Write`r on which to print the given tree.
/// - `tree`: The AST to print.
/// 
/// # Errors
/// This function errors if we failed to write to the given writer.
pub fn traverse(out: &mut impl Write, tree: &Program) -> Result<(), std::io::Error> {
    // Simply call the traversal functions on each of the internal statements
    for s in &tree.stmts {
        
    }

    // Done
    Ok(())
}
