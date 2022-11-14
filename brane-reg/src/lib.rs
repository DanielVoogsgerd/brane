//  LIB.rs
//    by Lut99
// 
//  Created:
//    26 Sep 2022, 15:12:09
//  Last edited:
//    06 Nov 2022, 17:05:33
//  Auto updated?
//    Yes
// 
//  Description:
//!   The `brane-reg` service implements a domain-local registry for both
//!   containers and datasets.
// 

// Declare the modules
pub mod errors;
pub mod spec;
pub mod store;
pub mod server;
pub mod health;
pub mod version;
pub mod data;