//  TYPES.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 16:07:39
//  Last edited:
//    31 May 2023, 19:08:44
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the data types allowed in the AST, as well as some functions
//!   that determine what is convertible to what.
// 

use std::fmt::{Display, Formatter, Result as FResult};
use std::str::FromStr;

use enum_debug::EnumDebug;


/***** LIBRARY *****/
/// Defines the allowed / supported data types by BraneScript and Bakery.
#[derive(Clone, Debug, EnumDebug, Eq, Hash, PartialEq)]
pub enum DataType {
    // Special types
    /// No compile-time type is deduced; essentially means "to-be-assessed".
    Any,
    /// A zero-sized, "none" type.
    Void,

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

impl DataType {
    /// Returns whether this DataType is `Any` or not.
    /// 
    /// If so, then it means that this DataType is effectively unknown until runtime, and the compiler should try to refine its type if possible.
    #[inline]
    pub fn is_any(&self) -> bool { matches!(self, Self::Any) }

    /// Returns whether this DataType is `Void` or not.
    #[inline]
    pub fn is_void(&self) -> bool { matches!(self, Self::Void) }
}

impl Display for DataType {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use DataType::*;
        match self {
            Any  => write!(f, "any"),
            Void => write!(f, "()"),

            Boolean => write!(f, "bool"),
            Integer => write!(f, "int"),
            Real    => write!(f, "real"),
            String  => write!(f, "string"),

            Array(elem_type)    => write!(f, "[{}]", elem_type),
            Class(name)         => write!(f, "{}", name),
            Function(args, ret) => write!(f, "func({}) -> {}", args.iter().map(|a| format!("{}", a)).collect::<Vec<std::string::String>>().join(","), ret),
        }
    }
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
