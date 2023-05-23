//  ANNOT STACK.rs
//    by Lut99
// 
//  Created:
//    11 Feb 2023, 18:08:46
//  Last edited:
//    23 May 2023, 10:57:14
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines a stack for annotations that we pop and push while running
//!   traversals. This will allows us to enable/disable compiler features
//!   based on what we are compiling.
// 

use std::ops::{Deref, DerefMut};

use log::warn;

use crate::warnings::WarningCode;
use crate::ast::spec::Annotation;


/***** AUXILLARY *****/
/// Helper struct that will automatically pop its own frame when pushed to the [`AnnotationStack`] when it goes out-of-scope.
#[derive(Debug)]
pub struct AnnotationStackGuard<'s> {
    /// The [`AnnotationStack`] to pop from when done.
    stack : &'s mut AnnotationStack,
    /// The identifier of the frame we are supposed to pop, which we use for assertion purposes.
    #[cfg(debug_assertions)]
    id    : u64,
}
impl<'s> AnnotationStackGuard<'s> {
    /// Early-drops the frame, popping it off the stack already.
    #[inline]
    pub fn pop(self) {}
}
impl<'s> Drop for AnnotationStackGuard<'s> {
    fn drop(&mut self) {
        // Attempt to get the top frame on the stack
        if let Some(frame) = self.stack.frames.pop() {
            // Assert it is the correct one
            #[cfg(debug_assertions)]
            if frame.0 != self.id { panic!("AnnotationStackGuard for frame {} attempted to pop frame with ID {}", self.id, frame.0); }
            #[cfg(debug_assertions)]
            let frame: usize = frame.1;

            // Now pop the stack
            let new_size: usize = self.stack.stack.len().saturating_sub(frame);
            self.stack.stack.truncate(new_size);

        } else {
            #[cfg(debug_assertions)]
            panic!("AnnotationStackGuard for frame {} attempted to pop empty AnnotationStack", self.id);
            #[cfg(not(debug_assertions))]
            panic!("AnnotationStackGuard attempted to pop empty AnnotationStack");
        }
    }
}

impl<'s> Deref for AnnotationStackGuard<'s> {
    type Target = AnnotationStack;

    #[inline]
    fn deref(&self) -> &Self::Target { self.stack }
}
impl<'s> DerefMut for AnnotationStackGuard<'s> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target { self.stack }
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
    #[cfg(debug_assertions)]
    frames : Vec<(u64, usize)>,
    #[cfg(not(debug_assertions))]
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
    pub fn push<A: Into<Annotation>>(&mut self, annots: impl IntoIterator<Item = A>) {
        // Convert the iterator to a list we know
        let annots: Vec<Annotation> = annots.into_iter().map(|a| a.into()).collect();

        // Resize our internal arrays for optimal performance :sunglasses:
        if !annots.is_empty() && self.stack.len() + annots.len() >= self.stack.capacity() { self.stack.reserve(if annots.len() > self.stack.capacity() { annots.len() } else { self.stack.capacity() }); }
        if self.frames.len() == self.frames.capacity() { self.frames.reserve(self.frames.capacity()); }

        // Push the frame
        let frame_size: usize = annots.len();
        self.stack.extend(annots);
        #[cfg(debug_assertions)]
        self.frames.push((fastrand::u64(..), frame_size));
        #[cfg(not(debug_assertions))]
        self.frames.push(frame_size);
    }

    /// Pops the top frame off the annotation stack.
    /// 
    /// Does nothing if there are no frames to push, but does emit a warning.
    pub fn pop(&mut self) {
        if let Some((_, size)) = self.frames.pop() {
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
    pub fn frame<A: Into<Annotation>>(&mut self, annots: impl IntoIterator<Item = A>) -> AnnotationStackGuard {
        // Convert the iterator to a list we know
        let annots: Vec<Annotation> = annots.into_iter().map(|a| a.into()).collect();

        // Resize our internal arrays for optimal performance :sunglasses:
        if !annots.is_empty() && self.stack.len() + annots.len() >= self.stack.capacity() { self.stack.reserve(if annots.len() > self.stack.capacity() { annots.len() } else { self.stack.capacity() }); }
        if self.frames.len() == self.frames.capacity() { self.frames.reserve(self.frames.capacity()); }

        // Push the frame
        let id: u64 = fastrand::u64(..);
        let frame_size: usize = annots.len();
        self.stack.extend(annots);
        #[cfg(debug_assertions)]
        self.frames.push((id, frame_size));
        #[cfg(not(debug_assertions))]
        self.frames.push(frame_size);

        // Now return a guard for the frame with that ID
        AnnotationStackGuard {
            stack : self,
            #[cfg(debug_assertions)]
            id,
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
