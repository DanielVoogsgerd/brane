//  ANNOT STACK.rs
//    by Lut99
// 
//  Created:
//    11 Feb 2023, 18:08:46
//  Last edited:
//    14 Feb 2023, 16:00:32
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
/// A guard that automatically pops the annotation stack when it goes out-of-scope.
#[derive(Debug)]
pub struct AnnotationStackGuard<'s>{
    /// The stack to pop
    stack : &'s AnnotationStack,
}
impl<'s> Drop for AnnotationStack<'s> {
    fn drop(&mut self) {
        self.stack.pop();
    }
}





/***** LIBRARY *****/
/// A thin wrapper around a vector to implement a stack for annotation features.
#[derive(Debug)]
pub struct AnnotationStack {
    /// The stack with annotation itself.
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
    /// - `annots`: The list of (parsed) Annotations to push.
    pub fn push<'a>(&mut self, annots: impl IntoIterator<Item = &'a Annotation>) {
        let annots: Vec<Annotation> = annots.into_iter().cloned().collect();

        // Resize our internal arrays for optimal performance :sunglasses:
        if !annots.is_empty() && self.stack.len() + annots.len() >= self.stack.capacity() { self.stack.reserve(if annots.len() > self.stack.capacity() { annots.len() } else { self.stack.capacity() }); }
        if self.frames.len() == self.frames.capacity() { self.frames.reserve(self.frames.capacity()); }

        // Push the frame
        let frame_size: usize = annots.len();
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

    /// Pushes the given list of annotations to this stack, then returns a guard that automatically pops them when it goes out-of-scope.
    /// 
    /// They are pushed as one frame, which means that they are also popped together when calling `AnnotationStackGuard::pop()` (or when it goes out-of-scope).
    /// 
    /// Note, however, that the guard simply performs an "AnnotationStack::pop()" when it goes out-of-scope. When used normally, this shouldn't be an issue; but be aware of this when manually interlacing pops with guards or when using weird scoping.
    /// 
    /// # Arguments
    /// - `annots`: The list of (parsed) Annotations to push.
    pub fn push_with_guard<'s, 'a>(&'s mut self, annots: impl IntoIterator<Item = &'a Annotation>) -> AnnotationStackGuard<'s> {
        // Push first
        self.push(annots);
        // Return the guard for popping
        AnnotationStackGuard {
            stack : self,
        }
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
            if let Annotation::Allow(allowed_code) = annot {
                // Return if allowed
                if *allowed_code == code { return true; }
            }
        }

        // Not found is not allowed
        false
    }
}
