//  ANNOT STACK.rs
//    by Lut99
// 
//  Created:
//    11 Feb 2023, 18:08:46
//  Last edited:
//    11 Feb 2023, 18:37:34
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines a stack for annotations that we pop and push while running
//!   traversals. This will allows us to enable/disable compiler features
//!   based on what we are compiling.
// 

use crate::warnings::WarningCode;
use crate::ast::auxillary::AnnotationKind;
use crate::ast::expressions::Expression;


/***** LIBRARY *****/
/// Provides the contents of each annotation. This defines the annotations that the compiler accepts, essentially.
#[derive(Clone, Debug)]
pub enum Annotation {
    /// Disables a specific warning message with the given identifier
    Allow(WarningCode),
    /// Scopes any external function occurring in the given statement to a specific location.
    On(Expression),
}

impl Annotation {
    /// Constructor for the Annotation that will create it from its parsed counterpart.
    /// 
    /// # Arguments
    /// - `annot`: The parsed AnnotationKind that can be used to read the annotation.
    /// 
    /// # Returns
    /// A new list of `Annotation` instances that were contained within. Note that this list may be empty, in which case the annotation did not make sense to us.
    fn from(value: &AnnotationKind) -> Vec<Self> {
        // Parse the parsed annotation
        match value {
            AnnotationKind::Identifier(ident) => {
                // There are no such annotations yet
                vec![]
            },

            AnnotationKind::KeyValue(ident, value) => {
                // Match on the identifier to find what it is
                match ident.name.as_str() {
                    "on" => vec![ Self::On(value.clone()) ],

                    // The rest is unknown to us
                    _ => vec![],
                }
            },

            AnnotationKind::KeyList(ident, list) => {
                // Match on the identifier to find what it is, then process the list
                match ident.name.as_str() {
                    "allow" => {
                        // Assert it is a list of identifiers
                        for annot in list {
                            match &annot.kind {
                                AnnotationKind::Identifier(ident) => match &ident.name {
                                    
                                },

                                // The rest is an unknown warning code
                                
                            }
                        }
                    },
                }
            },
        }
    }
}



/// A thin wrapper around a vector to implement a stack for annotation features.
#[derive(Debug)]
pub struct AnnotationStack(Vec<Annotation>);

impl Default for AnnotationStack {
    #[inline]
    fn default() -> Self { Self::new() }
}
impl AnnotationStack {
    /// Constructor for the annotation stack that creates an empty stack.
    /// 
    /// # Returns
    /// A new AnnotationStack instance with nothing pushed on it yet.
    #[inline]
    pub fn new() -> Self {
        Self(vec![])
    }

    /// Constructor for the annotation stack that initializes it with allocated space for at least `capacity` elements.
    /// 
    /// Note that it does not populate them; this is just to avoid reallocation.
    /// 
    /// # Arguments
    /// - `capacity`: The initial capacity for the stack.
    /// 
    /// # Returns
    /// A new AnnotationStack instance with nothing pushed on it yet.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self(Vec::with_capacity(capacity))
    }
}
