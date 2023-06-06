//  TYPES.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 16:07:39
//  Last edited:
//    06 Jun 2023, 15:22:42
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


/***** AUXILLARY *****/
/// Special formatter for the [`DataTypeGroup`] that formats it as a list _if_ it is finite.
/// 
/// Note that somehow constructing this for a [`DataTypeGroup`] that is _not_ finite will cause panics.
#[derive(Debug)]
pub struct DataTypeGroupListFormatter<'g, 'w> {
    /// The group to format
    group : &'g DataTypeGroup,
    /// The 'connection word' to use in between the last two items
    word  : &'w str,
}
impl<'g, 'w> Display for DataTypeGroupListFormatter<'g, 'w> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use DataTypeGroup::*;
        match self.group {
            Numeric => write!(f, "integer {} real", self.word),

            // Otherwise, it isn't finite
            group => { panic!("Cannot format {:?} as a list: not a finite group", group.variant()); },
        }
    }
}





/***** LIBRARY *****/
/// Defines certain groups of types.
#[derive(Clone, Copy, Debug, EnumDebug, Eq, Hash, PartialEq)]
pub enum DataTypeGroup {
    /// Defines anyone's game - i.e., every type.
    All,
    /// Defines anyone's game as long as it's not [`DataType::Void`].
    NonVoid,
    /// Defines no-one's game - i.e., no type (empty set).
    None,

    /// Defines the numeric types (i.e., integers and floats)
    Numeric,
}

impl DataTypeGroup {
    /// Returns whether the given data type is a part of this group.
    /// 
    /// # Arguments
    /// - `data_type`: The [`DataType`] to check if it is a part of us.
    /// 
    /// # Returns
    /// True if the given `data_type` is part of this group, or false otherwise.
    #[inline]
    pub fn contains(&self, data_type: impl AsRef<DataType>) -> bool {
        let data_type: &DataType = data_type.as_ref();

        // The group determines how we can check the data type
        use DataTypeGroup::*;
        match self {
            All     => true,
            NonVoid => !data_type.is_void(),
            None    => false,

            Numeric => matches!(data_type, DataType::Integer) || matches!(data_type, DataType::Real),
        }
    }

    /// Returns whether this group is a finite group.
    /// 
    /// Effectively returns if this group is composed of atomic types only, for the reason that composite types either imply recursion or an open-ended set of variations (e.g., Classes).
    /// 
    /// # Returns
    /// True if all types in this group can be iterated, or false otherwise.
    #[inline]
    pub fn is_finite(&self) -> bool { 
        use DataTypeGroup::*;
        match self {
            All     |
            NonVoid |
            None    => false,

            Numeric => true,
        }
    }



    /// Returns a formatter that formats this data type as a list.
    /// 
    /// # Arguments
    /// - `word`: The last word in the list, indicating what kind of list it is. Think of stuff like `and` or `or`.
    /// 
    /// # Returns
    /// A new [`DataTypeGroupListFormatter`] instance that implements [`Display`]. Note that, when this data type is not finite (i.e., [`DataTypeGroup::is_finite()`] returns false), we cannot enumerate it and thus [`None`] is returned instead.
    #[inline]
    pub fn display_list<'s, 'w>(&'s self, word: &'w str) -> Option<DataTypeGroupListFormatter<'s, 'w>> {
        if self.is_finite() {
            Some(DataTypeGroupListFormatter { group: self, word })
        } else {
            None
        }
    }
}
impl Display for DataTypeGroup {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use DataTypeGroup::*;
        match self {
            All     => write!(f, "all"),
            NonVoid => write!(f, "non-void"),
            None    => write!(f, "no"),

            Numeric => write!(f, "numeric"),
        }
    }
}

impl AsRef<DataTypeGroup> for DataTypeGroup {
    #[inline]
    fn as_ref(&self) -> &Self { self }
}
impl AsMut<DataTypeGroup> for DataTypeGroup {
    #[inline]
    fn as_mut(&mut self) -> &mut Self { self }
}
impl From<&DataTypeGroup> for DataTypeGroup {
    #[inline]
    fn from(value: &DataTypeGroup) -> Self { *value }
}
impl From<&mut DataTypeGroup> for DataTypeGroup {
    #[inline]
    fn from(value: &mut DataTypeGroup) -> Self { *value }
}



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
    /// Companion function for [`DataTypeGroup::contains()`], which can be used to check if this data type is part of the given group.
    /// 
    /// # Arguments
    /// - `group`: The [`DataTypeGroup`] to see if we are part of.
    /// 
    /// # Returns
    /// True if we are part of `group`, false if we are not.
    #[inline]
    pub fn is_part_of(&self, group: impl AsRef<DataTypeGroup>) -> bool { group.as_ref().contains(self) }

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

impl AsRef<DataType> for DataType {
    #[inline]
    fn as_ref(&self) -> &Self { self }
}
impl AsMut<DataType> for DataType {
    #[inline]
    fn as_mut(&mut self) -> &mut Self { self }
}
impl From<&DataType> for DataType {
    #[inline]
    fn from(value: &DataType) -> Self { value.clone() }
}
impl From<&mut DataType> for DataType {
    #[inline]
    fn from(value: &mut DataType) -> Self { value.clone() }
}
