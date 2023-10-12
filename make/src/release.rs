//  RELEASE.rs
//    by Lut99
// 
//  Created:
//    12 Oct 2023, 12:08:56
//  Last edited:
//    12 Oct 2023, 16:32:05
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the toplevel things for buildings things in release mode.
// 

use std::error;
use std::fmt::{Display, Formatter, Result as FResult};

use log::info;


/***** ERRORS *****/
/// Defines errors that occur in release mode.
#[derive(Debug)]
pub enum Error {

}
impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // use Error::*;
        // match self {

        // }
        Ok(())
    }
}
impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        // use Error::*;
        // match self {

        // }
        None
    }
}




/***** LIBRARY *****/
/// Builds a particular target in release mode.
/// 
/// # Errors
/// This function may error if we failed to build the target.
pub fn build_target() -> Result<(), Error> {
    info!("Building in `release`-mode");
    Ok(())
}
