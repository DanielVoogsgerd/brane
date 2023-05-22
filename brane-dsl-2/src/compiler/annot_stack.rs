//  ANNOT STACK.rs
//    by Lut99
// 
//  Created:
//    11 Feb 2023, 18:08:46
//  Last edited:
//    22 May 2023, 19:22:36
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines a stack for annotations that we pop and push while running
//!   traversals. This will allows us to enable/disable compiler features
//!   based on what we are compiling.
// 

use log::warn;

use crate::warnings::WarningCode;
use crate::ast::spec::Annotation;


/***** AUXILLARY *****/
/// Helper struct that will automatically pop its own frame when pushed to the [`AnnotationStack`] when it goes out-of-scope.
#[derive(Debug)]
pub struct AnnotationStackFrame<'s> {
    /// The [`AnnotationStack`] to pop from when done.
    stack : &'s AnnotationStack,
}
impl<'s> AnnotationStackFrame<'s> {
    /// Early-drops the frame, popping it off the stack already.
    #[inline]
    pub fn drop(self) {}
}





/***** LIBRARY *****/
/// A thin wrapper around a vector to implement a stack for annotation features.
#[derive(Debug)]
pub struct AnnotationStack {
    /// The stack with annotation itself.
    #[cfg(debug_assertions)]
    stack  : Vec<(u64, Annotation)>,
    #[cfg(not(debug_assertions))]
    stack  : Vec<Annotation>,
    /// The frames upon this stack, given in the number of annotations in them.
    /// 
    /// This is done this way to both be able to pop really efficiently, and be cache-friendly in the annotations themselves.
    frames : Vec<usize>,
}

impl Default for AnnotationStack {
    #[inline]
    fn default() -> Self { Self::new() }
}
impl AnnotationStack {
    /// Constructor for the AnnotationStack that initializes it without any annotations yet.
    /// 
    /// # Returns
    /// A new AnnotationStack instance that is empty.
    #[inline]
    pub fn new() -> Self {
        Self {
            stack  : vec![],
            frames : vec![],
        }
    }



    /// Pushes the given list of Annotations to this stack.
    /// 
    /// They are pushed as one frame, which means that they are also popped together when calling `AnnotationStack::pop()`.
    /// 
    /// # Arguments
    /// - `annots`: The list of [`Annotation`]s to push.
    pub fn push<'a>(&mut self, annots: impl IntoIterator<Item = &'a Annotation>) {
        let annots: Vec<Annotation> = annots.into_iter().cloned().collect();

        // Resize our internal arrays for optimal performance :sunglasses:
        if !annots.is_empty() && self.stack.len() + annots.len() >= self.stack.capacity() { self.stack.reserve(if annots.len() > self.stack.capacity() { annots.len() } else { self.stack.capacity() }); }
        if self.frames.len() == self.frames.capacity() { self.frames.reserve(self.frames.capacity()); }

        // Push the frame
        let frame_size: usize = annots.len();
        #[cfg(debug_assertions)]
        self.stack.extend(annots.into_iter().map(|a| (fastrand::u64(..), a)));
        #[cfg(not(debug_assertions))]
        self.stack.extend(annots);
        self.frames.push(frame_size);
    }

    /// Pops the top frame off the annotation stack.
    /// 
    /// Does nothing if there are no frames to push, but does emit a warning.
    pub fn pop(&mut self) {
        if let Some(size) = self.frames.pop() {
            let new_size: usize = self.stack.len().saturating_sub(size);
            self.stack.truncate(new_size);
        } else {
            warn!("Attempted to pop from the annotation stack while there were not frames on it (implies an ill-formed compiler pass)");
        }
    }

    /// Pushes a given stack on top of the frame, and returns a handle for automatically popping it.
    /// 
    /// They are pushed as one frame, which means that they are also popped together when calling `AnnotationStack::pop()`.
    /// 
    /// Note that the handle has some builtin checking for if it pops the correct stack when `debug_assertions` are enabled. However, it, in practise, just pops the top value.
    /// 
    /// # Arguments
    /// - `annots`: The list of [`Annotation`]s to push.
    /// 
    /// # Returns
    /// A new [`AnnotationStackGuard`] that automatically pops this frame when it gets dropped.
    pub fn frame<'a>(&mut self, annots: impl IntoIterator<Item = &'a Annotation>) -> AnnotationStackGuard {
        
    }



    /// Checks the given warning is disabled.
    /// 
    /// # Arguments
    /// - `code`: The WarningCode of the warning to check for.
    /// 
    /// # Returns
    /// True if it is disabled, or false otherwise.
    pub fn is_allowed(&self, code: WarningCode) -> bool {
        // Search in reverse order
        for annot in self.stack.iter().rev() {
            #[cfg(debug_assertions)]
            let annot = &annot.1;
            if let Annotation::Allow(allowed_code) = annot {
                // Return if allowed
                if *allowed_code == code { return true; }
            }
        }

        // Not found is not allowed
        false
    }
}
