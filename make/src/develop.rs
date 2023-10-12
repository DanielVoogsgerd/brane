//  DEVELOP.rs
//    by Lut99
// 
//  Created:
//    12 Oct 2023, 12:06:40
//  Last edited:
//    12 Oct 2023, 12:08:46
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the toplevel functions of building stuff in `develop`-mode.
// 

use std::error;
use std::fmt::{Display, Formatter, Result as FResult};


/***** ERRORS *****/
/// Defines errors that occur in development mode.
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
/// Builds a particular target in development mode.
/// 
/// # Errors
/// This function may error if we failed to build the target.
pub fn build_target() -> Result<(), Error> {
    Ok(())
}
