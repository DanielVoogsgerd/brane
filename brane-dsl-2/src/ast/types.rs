//  TYPES.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 16:07:39
//  Last edited:
//    07 Feb 2023, 19:29:15
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the data types allowed in the AST, as well as some functions
//!   that determine what is convertible to what.
// 

use std::str::FromStr;

use enum_debug::EnumDebug;


/***** LIBRARY *****/
/// Defines the allowed / supported data types by BraneScript and Bakery.
#[derive(Clone, Debug, EnumDebug)]
pub enum DataType {
    // Special types
    /// No compile-time type is deduced; essentially means "to-be-assessed".
    Any,

    // Atomic types
    /// A boolean value.
    Boolean,
    /// An integer value.
    Integer,
    /// A float value.
    Real,
    /// A string value.
    String,

    // Composite types
    /// An object divided into homogeneous sub-types.
    Array(Box<Self>),
    /// An object divided into heteregenous sub-types. We disambiguate by name.
    Class(String),
    /// A callable object, taking in the given arguments and returning the given type.
    Function(Vec<Self>, Box<Self>),
}

impl From<String> for DataType {
    #[inline]
    fn from(value: String) -> Self { Self::from(value.as_str()) }
}
impl From<&String> for DataType {
    #[inline]
    fn from(value: &String) -> Self { Self::from(value.as_str()) }
}
impl From<&str> for DataType {
    fn from(value: &str) -> Self {
        // First: any arrays are done recursively
        if !value.is_empty() && &value[..1] == "[" && &value[value.len() - 1..] == "]" {
            return Self::Array(Box::new(Self::from(&value[1..value.len() - 1])));
        } else if value.len() >= 2 && &value[value.len() - 2..] == "[]" {
            return Self::Array(Box::new(Self::from(&value[..value.len() - 2])));
        }

        // Otherwise, match literals & classes
        use DataType::*;
        match value {
            // Literal types
            "bool" | "boolean" => Boolean,
            "int"  | "integer" => Integer,
            "float" | "real"   => Real,
            "string"           => String,

            // The rest is always a class
            value => Class(value.into()),
        }
    }
}
impl FromStr for DataType {
    type Err = std::convert::Infallible;

    #[inline]
    fn from_str(s: &str) -> Result<Self, Self::Err> { return Ok(Self::from(s)) }
}
