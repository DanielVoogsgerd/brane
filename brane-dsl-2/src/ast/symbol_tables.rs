//  SYMBOL TABLES.rs
//    by Lut99
// 
//  Created:
//    11 Feb 2023, 17:54:32
//  Last edited:
//    13 Feb 2023, 18:21:57
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines SymbolTables, which are not nodes in the AST but rather a
//!   separate data structure for keeping track of definitions.
// 

use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use enum_debug::EnumDebug;

use specifications::version::Version;

use super::spec::TextRange;
use super::types::DataType;


/***** AUXILLARY *****/
/// The `DelayedEntry` wraps another type of entry to mark it either as a _resolved_ entry (i.e., given the information we have we can start to emit errors and warnings), or as a _phantom_ entry (i.e., it is likely defined somewhere else, errors have to wait until linking).
#[derive(Clone, Debug, EnumDebug)]
pub enum DelayedEntry<T> {
    /// The entry is already resolved, i.e., we know its declaration.
    Resolved(T),
    /// The entry is not yet resolved, i.e., its declaration won't be known until linking time.
    Phantom(T),
}
impl<T> DelayedEntry<T> {
    /// Returns whether this entry is resolved.
    /// 
    /// # Returns
    /// true if it is, false it is a `DelayedEntry::Phantom`.
    #[inline]
    pub fn is_resolved(&self) -> bool { matches!(self, Self::Resolved(_)) }

    /// Returns whether this entry is phantom.
    /// 
    /// # Returns
    /// true if it is, false it is a `DelayedEntry::Resolved`.
    #[inline]
    pub fn is_phantom(&self) -> bool { matches!(self, Self::Phantom(_)) }



    /// Returns the internal entry regardless of whether it is resolved or not.
    /// 
    /// # Returns
    /// A reference to the internal entry.
    #[inline]
    pub fn entry(&self) -> &T {
        match self {
            Self::Resolved(entry) |
            Self::Phantom(entry)  => entry,
        }
    }

    /// Returns the internal entry regardless of whether it is resolved or not.
    /// 
    /// # Returns
    /// A mutable reference to the internal entry.
    #[inline]
    pub fn entry_mut(&mut self) -> &mut T {
        match self {
            Self::Resolved(entry) |
            Self::Phantom(entry)  => entry,
        }
    }

    /// Consumes the DelayedEntry to return the internal entry, regardless of whether it is resolved or not.
    /// 
    /// # Returns
    /// The internal entry.
    #[inline]
    pub fn into_entry(self) -> T {
        match self {
            Self::Resolved(entry) |
            Self::Phantom(entry)  => entry,
        }
    }
}
impl<T> Deref for DelayedEntry<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target { self.entry() }
}
impl<T> DerefMut for DelayedEntry<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target { self.entry_mut() }
}

/// A wrapper around a DelayedEntry that incorporates all the pointer jazz.
#[derive(Clone, Debug)]
pub struct DelayedEntryPtr<T> {
    /// The entry we wrap.
    pub ptr : Rc<RefCell<DelayedEntry<T>>>,
}
impl<T> DelayedEntryPtr<T> {
    /// Constructor for the DelayedEntryPtr that initializes it as a resolved pointer from the given object.
    /// 
    /// # Arguments
    /// - `entry`: The entry to wrap.
    /// 
    /// # Returns
    /// A new DelayedEntryPtr instance that wraps around `DelayedEntry::Resolved(entry)`.
    #[inline]
    pub fn resolved(entry: T) -> Self {
        Self{ ptr: Rc::new(RefCell::new(DelayedEntry::Resolved(entry))) }
    }

    /// Constructor for the DelayedEntryPtr that initializes it as a phantom pointer from the given object.
    /// 
    /// # Arguments
    /// - `entry`: The entry to wrap.
    /// 
    /// # Returns
    /// A new DelayedEntryPtr instance that wraps around `DelayedEntry::Phantom(entry)`.
    #[inline]
    pub fn phantom(entry: T) -> Self {
        Self{ ptr: Rc::new(RefCell::new(DelayedEntry::Phantom(entry))) }
    }
}
impl<T> Deref for DelayedEntryPtr<T> {
    type Target = Rc<RefCell<DelayedEntry<T>>>;

    #[inline]
    fn deref(&self) -> &Self::Target { &self.ptr }
}
impl<T> From<DelayedEntry<T>> for DelayedEntryPtr<T> {
    #[inline]
    fn from(value: DelayedEntry<T>) -> Self {
        DelayedEntryPtr{ ptr: Rc::new(RefCell::new(value)) }
    }
}



/// A PackageEntry defines a package that contains external function entries.
/// 
/// Note that these are always phantom, since we don't package information at the resolve stage yet.
#[derive(Clone, Debug)]
pub struct PackageEntry {
    /// The name of the package.
    pub name    : String,
    /// The version of the package.
    pub version : Version,

    /// The list of external functions provided by this package.
    pub funcs   : Vec<DelayedEntryPtr<ExternalFuncEntry>>,
    /// The list of classes defined in this entry.
    pub classes : Vec<DelayedEntryPtr<ClassEntry>>,

    /// The location in the source where this package was declared. Obviously `None` if phantom, but may also be None in other cases.
    pub range : Option<TextRange>,
}

/// Defines a symbol table entry for an external function.
#[derive(Clone, Debug)]
pub struct ExternalFuncEntry {
    /// The name of the function.
    pub name     : String,
    /// The argument types of the function.
    pub args     : Vec<VarEntry>,
    /// The return type of the function.
    pub ret_type : DataType,

    /// Reference to the parent package.
    pub package : DelayedEntryPtr<PackageEntry>,
}



/// Defines a local function in the symbol table.
#[derive(Clone, Debug)]
pub struct LocalFuncEntry {
    /// The name of the function.
    pub name     : String,
    /// The argument types of the function.
    pub args     : Vec<DelayedEntryPtr<VarEntry>>,
    /// The return type of the function.
    pub ret_type : DataType,

    /// The location in the source where this function was declared. Obviously `None` if phantom, but may also be None in other cases.
    pub range : Option<TextRange>,
}



/// Defines a class in the symbol table.
#[derive(Clone, Debug)]
pub struct ClassEntry {
    /// The name of the class (also its signature).
    pub name : String,

    /// The properties defined within this class.
    pub props   : HashMap<String, VarEntry>,
    /// The methods defined within this class.
    pub methods : HashMap<String, LocalFuncEntry>,

    /// The location in the source where this class was declared. Obviously `None` if phantom, but may also be None in other cases.
    pub range : Option<TextRange>,
}



/// Defines a variable entry in the symbol table, which can either be an argument or a real variable.
#[derive(Clone, Debug)]
pub struct VarEntry {
    /// The name of the variable.
    pub name      : String,
    /// The type of the variable.
    pub data_type : DataType,

    /// Whether this variable is shadowed or not. This property is only used for toplevel variables, where we decide if we should export them or not.
    pub shadowed : bool,

    /// The location in the source where this variable was declared. Obviously `None` if phantom, but may also be None in other cases.
    pub range : Option<TextRange>,
}





/***** LIBRARY *****/
/// Defines a SymbolTable, which maps information we are interested in to an identifier, per-scope.
#[derive(Clone, Debug)]
pub struct SymbolTable {
    /// The identifier that live in the package namespace.
    pub packages : HashMap<String, DelayedEntryPtr<PackageEntry>>,
    /// The identifiers that live in the (local) function namespace.
    pub funcs    : HashMap<String, DelayedEntryPtr<LocalFuncEntry>>,
    /// The identifiers that live in the class (type) namespace.
    pub classes  : HashMap<String, DelayedEntryPtr<ClassEntry>>,
    /// The identifiers that live in the variable namespace.
    pub vars     : HashMap<String, DelayedEntryPtr<VarEntry>>,

    /// The parent symbol table to report to. Is `None` is this is the root.
    pub parent : Option<Rc<RefCell<SymbolTable>>>,
    /// Whether this symbol table does consider the parent to be a "parent scope", i.e., can it access variables declared there.
    pub nests  : bool,
}

impl SymbolTable {
    /// Constructor for the SymbolTable that does not populate any entries yet.
    /// 
    /// # Returns
    /// A new SymbolTable instance without any entries.
    #[inline]
    pub fn empty() -> Self {
        Self {
            packages : HashMap::new(),
            funcs    : HashMap::new(),
            classes  : HashMap::new(),
            vars     : HashMap::new(),

            parent : None,
            nests  : false,
        }
    }



    /// Searches this symbol table and any parents to find a package with the given name.
    /// 
    /// The first match found is returned, to allow shadowing.
    /// 
    /// # Arguments
    /// - `name`: The name of the package to search for.
    /// 
    /// # Returns
    /// The entry of the package if we found any, or else `None`.
    pub fn resolve_package(&self, name: impl AsRef<str>) -> Option<DelayedEntryPtr<PackageEntry>> {
        let name: &str = name.as_ref();

        // Search either this table or the parent
        match self.packages.get(name) {
            Some(entry) => Some(entry.clone()),
            None        => self.parent.as_ref().map(|p| p.borrow().resolve_package(name)).unwrap_or(None),
        }
    }

    /// Searches this symbol table and any parents to find a local function with the given name.
    /// 
    /// The first match found is returned, to allow shadowing.
    /// 
    /// # Arguments
    /// - `name`: The name of the function to search for.
    /// 
    /// # Returns
    /// The entry of the function if we found any, or else `None`.
    pub fn resolve_func(&self, name: impl AsRef<str>) -> Option<DelayedEntryPtr<LocalFuncEntry>> {
        let name: &str = name.as_ref();

        // Search either this table or the parent
        match self.funcs.get(name) {
            Some(entry) => Some(entry.clone()),
            None        => self.parent.as_ref().map(|p| p.borrow().resolve_func(name)).unwrap_or(None),
        }
    }

    /// Searches this symbol table and any parents to find a class with the given name.
    /// 
    /// The first match found is returned, to allow shadowing.
    /// 
    /// # Arguments
    /// - `name`: The name of the class to search for.
    /// 
    /// # Returns
    /// The entry of the class if we found any, or else `None`.
    pub fn resolve_class(&self, name: impl AsRef<str>) -> Option<DelayedEntryPtr<ClassEntry>> {
        let name: &str = name.as_ref();

        // Search either this table or the parent
        match self.classes.get(name) {
            Some(entry) => Some(entry.clone()),
            None        => self.parent.as_ref().map(|p| p.borrow().resolve_class(name)).unwrap_or(None),
        }
    }

    /// Searches this symbol table and any parents to find a variable with the given name.
    /// 
    /// The first match found is returned, to allow shadowing.
    /// 
    /// # Arguments
    /// - `name`: The name of the variable to search for.
    /// 
    /// # Returns
    /// The entry of the variable if we found any, or else `None`.
    pub fn resolve_var(&self, name: impl AsRef<str>) -> Option<DelayedEntryPtr<VarEntry>> {
        let name: &str = name.as_ref();

        // Search either this table or the parent
        match self.vars.get(name) {
            Some(entry) => Some(entry.clone()),
            None        => self.parent.as_ref().map(|p| p.borrow().resolve_var(name)).unwrap_or(None),
        }
    }
}
