//  DATA TYPE.rs
//    by Lut99
// 
//  Created:
//    30 Aug 2022, 12:02:57
//  Last edited:
//    03 Nov 2022, 17:54:15
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines a DataType enum that is optimized for execution (and
//!   transferral along the wire).
// 

use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};

use serde::{Deserialize, Serialize};

use crate::spec::BuiltinClasses;


/***** AUXILLARY ERRORS *****/
/// Defines errors that occur when parsing DataTypes.
#[derive(Debug)]
pub enum DataTypeError {
    /// The given string was not recognized.
    UnknownDataType{ raw: String },
}

impl Display for DataTypeError {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use DataTypeError::*;
        match self {
            UnknownDataType{ raw } => write!(f, "Unknown data type '{}'", raw),
        }
    }
}

impl Error for DataTypeError {}





/***** LIBRARY *****/
/// Defines a DataType enum that is optimized for execution (and transferral along the wire).
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind")]
pub enum DataType {
    // Meta types
    /// Any type is accepted.
    #[serde(rename = "any")]
    Any,
    /// No type is accepted.
    #[serde(rename = "void")]
    Void,

    // Permissive types
    /// Allows both integers and reals.
    #[serde(rename = "num")]
    Numeric,
    /// Allows integers, reals and strings.
    #[serde(rename = "add")]
    Addable,
    /// Allows any callable object.
    #[serde(rename = "call")]
    Callable,
    /// Allows everything but Void
    #[serde(rename = "nvd")]
    NonVoid,

    // Atomic types (sorry Thomas)
    /// Only Boolean values are accepted (i.e., true or false, 1 or 0, yes or no, etc).
    #[serde(rename = "bool")]
    Boolean,
    /// Only Integral values are accepted (i.e., non-decimal numbers)
    #[serde(rename = "int")]
    Integer,
    /// Only Real values are accepted (i.e., decimal numbers)
    #[serde(rename = "real")]
    Real,
    /// Only String values are accepted (i.e., arrays of characters)
    #[serde(rename = "str")]
    String,
    /// Only Semantic versioning are accepted (i.e., major.minor.patch)
    #[serde(rename = "ver")]
    Semver,

    // Composite types (sorry Thomas)
    /// Arrays (i.e., a memory area divided into homogeneous types).
    #[serde(rename = "arr")]
    Array{
        #[serde(rename = "t")]
        elem_type : Box<DataType>,
    },
    /// Functions (i.e., executable pieces of code). Contains both the types (and arity) of its arguments and the return type.
    #[serde(rename = "func")]
    Function{
        #[serde(rename = "a")]
        args : Vec<DataType>,
        #[serde(rename = "t")]
        ret  : Box<DataType>,
    },
    /// Classes (i.e., a memory area divided into heterogeneous types). The usize indexes the signature into the Workflow's global buffers.
    #[serde(rename = "clss")]
    Class{
        #[serde(rename = "n")]
        name : String,
    },
    /// An externally represented dataset. The string is its identifier.
    #[serde(rename = "data")]
    Data,
    /// An externally represented dataset but one that may also be generated by functions.
    #[serde(rename = "res")]
    IntermediateResult,
}

impl DataType {
    /// Returns if this DataType is the same or at least targeted of the given one.
    /// 
    /// A common use-case for this function is checking return types, where the return type would be the given one.
    /// 
    /// # Arguments
    /// - `allowed`: The DataType that describes what is allowed.
    /// 
    /// # Returns
    /// Whether or not this DataType "is the same" as the other one.
    #[inline]
    pub fn allowed_by(&self, other: &Self) -> bool {
        use DataType::*;
        match (self, other) {
            // Individual cases
            (Data, IntermediateResult) => true,

            // Group cases
            (Integer, Numeric) => true,
            (Real, Numeric)    => true,

            (Integer, Addable) => true,
            (Real, Addable)    => true,
            (String, Addable)  => true,

            (Function{ .. }, Callable)  => true,

            (Void, NonVoid) => false,
            (_, NonVoid)    => true,

            (_, Any) => true,

            // Recursive cases
            (Array{ elem_type: lhs }, Array{ elem_type: rhs }) => lhs.allowed_by(rhs),

            // General case
            (t1, t2) => t1 == t2,
        }
    }



    /// Returns if this DataType is Void (i.e., no value).
    #[inline]
    pub fn is_void(&self) -> bool { if let Self::Void = self { true } else { false } }
}

impl Display for DataType {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use DataType::*;
        match self {
            Any  => write!(f, "Any"),
            Void => write!(f, "Void"),

            Numeric  => write!(f, "Numeric"),
            Addable  => write!(f, "Addable (Numeric or String)"),
            Callable => write!(f, "Callable (Function)"),
            NonVoid  => write!(f, "non-Void"),

            Boolean => write!(f, "Boolean"),
            Integer => write!(f, "Integer"),
            Real    => write!(f, "Real"),
            String  => write!(f, "String"),
            Semver  => write!(f, "Semver"),

            Array{ elem_type }    => write!(f, "Array<{}>", elem_type),
            Function{ args, ret } => write!(f, "Func<({}){}>", args.iter().map(|t| format!("{}", t)).collect::<Vec<std::string::String>>().join(", "), if &**ret != &DataType::Void { format!(" -> {}", ret) } else { std::string::String::new() }),
            Class{ name }         => write!(f, "Class<{}>", name),
            Data                  => write!(f, "Data"),
            IntermediateResult    => write!(f, "IntermediateResult"),
        }
    }
}

impl From<brane_dsl::DataType> for DataType {
    #[inline]
    fn from(value: brane_dsl::DataType) -> Self {
        use brane_dsl::DataType::*;
        match value {
            Any  => Self::Any,
            Void => Self::Void,

            Boolean => Self::Boolean,
            Integer => Self::Integer,
            Real    => Self::Real,
            String  => Self::String,
            Semver  => Self::Semver,

            Array(a)      => Self::Array{ elem_type: a.into() },
            Function(sig) => Self::Function{ args: sig.args.into_iter().map(|d| d.into()).collect(), ret: Box::new(sig.ret.into()) },
            Class(name)   => {
                // Match if 'Data' or 'IntermediateResult'
                if name == BuiltinClasses::Data.name() {
                    Self::Data
                } else if name == BuiltinClasses::IntermediateResult.name() {
                    Self::IntermediateResult
                } else {
                    Self::Class{ name }
                }
            },
        }
    }
}

impl From<&brane_dsl::DataType> for DataType {
    #[inline]
    fn from(value: &brane_dsl::DataType) -> Self {
        use brane_dsl::DataType::*;
        match value {
            Any  => Self::Any,
            Void => Self::Void,

            Boolean => Self::Boolean,
            Integer => Self::Integer,
            Real    => Self::Real,
            String  => Self::String,
            Semver  => Self::Semver,

            Array(a)      => Self::Array{ elem_type: a.into() },
            Function(sig) => Self::Function{ args: sig.args.iter().map(|d| d.into()).collect(), ret: Box::new((&sig.ret).into()) },
            Class(name)   => {
                // Match if 'Data' or 'IntermediateResult'
                if name == BuiltinClasses::Data.name() {
                    Self::Data
                } else if name == BuiltinClasses::IntermediateResult.name() {
                    Self::IntermediateResult
                } else {
                    Self::Class{ name: name.clone() }
                }
            },
        }
    }
}

impl From<Box<brane_dsl::DataType>> for Box<DataType> {
    #[inline]
    fn from(value: Box<brane_dsl::DataType>) -> Self {
        Self::from(&value)
    }
}

impl From<&Box<brane_dsl::DataType>> for Box<DataType> {
    #[inline]
    fn from(value: &Box<brane_dsl::DataType>) -> Self {
        Box::new(DataType::from(value.as_ref()))
    }
}

impl From<&str> for DataType {
    fn from(value: &str) -> Self {
        // First: any arrays are done recursively
        if !value.is_empty() && &value[..1] == "[" && &value[value.len() - 1..] == "]" {
            return Self::Array{ elem_type: Box::new(Self::from(&value[1..value.len() - 1])) };
        } else if value.len() >= 2 && &value[value.len() - 2..] == "[]" {
            return Self::Array{ elem_type: Box::new(Self::from(&value[..value.len() - 2])) };
        }

        // Otherwise, match literals & classes
        use DataType::*;
        match value {
            // Literal types
            "bool" | "boolean" => Boolean,
            "int"  | "integer" => Integer,
            "float" | "real"   => Real,
            "string"           => String,

            // The rest is always a class unless it's data or an intermediate result
            value => if value == BuiltinClasses::Data.name() {
                Data
            } else if value == BuiltinClasses::IntermediateResult.name() {
                IntermediateResult
            } else {
                Class{ name: value.into() }
            },
        }
    }
}

impl From<&String> for DataType {
    #[inline]
    fn from(value: &String) -> Self {
        // Use the string-one
        Self::from(value.as_str())
    }
}

impl From<String> for DataType {
    #[inline]
    fn from(value: String) -> Self {
        // Use the string-one
        Self::from(value.as_str())
    }
}
