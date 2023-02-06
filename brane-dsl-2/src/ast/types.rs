//  TYPES.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 16:07:39
//  Last edited:
//    06 Feb 2023, 16:09:17
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the data types allowed in the AST, as well as some functions
//!   that determine what is convertible to what.
// 

use enum_debug::EnumDebug;


/***** LIBRARY *****/
/// Defines the allowed / supported data types by BraneScript and Bakery.
#[derive(Clone, Debug, EnumDebug)]
pub enum DataType {
    /// No compile-time type is deduced; essentially means "to-be-assessed".
    Any,
}
