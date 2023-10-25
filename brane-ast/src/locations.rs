//  LOCATIONS.rs
//    by Lut99
// 
//  Created:
//    07 Sep 2022, 10:48:30
//  Last edited:
//    25 Oct 2023, 12:00:20
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines a few enums that help analysing location restrictions on a
//!   node.
// 

use std::fmt::{Display, Formatter, Result as FResult};
use std::ops::{Deref, DerefMut};

use brane_dsl::location::AllowedLocations;

use enum_debug::EnumDebug;
use serde::{Deserialize, Serialize};


/***** LIBRARY *****/
// /// Defines a single location to run.
// pub type Location = String;
/// Defines a single location to run a task on.
/// 
/// Note that this **includes** the central node. As such, don't use this for planning; use [`WorkLocation`] instead.
#[derive(Clone, Debug, Deserialize, EnumDebug, Eq, Hash, PartialEq, Serialize)]
pub enum Location {
    /// The central node, which may have data
    Central,
    /// A worker node, which can execute stuff and have data
    Worker(WorkLocation),
}

impl Location {
    /// Creates a new Location that points to the central node.
    /// 
    /// # Returns
    /// A new [`Self::Central`](Location::Central).
    #[inline]
    pub fn new_central() -> Self { Self::Central }

    /// Creates a new Location that points to the given worker node.
    /// 
    /// # Arguments
    /// - `identifier`: The identifier/name of the worker node.
    /// 
    /// # Returns
    /// A new [`Self::Worker`](Location::Worker).
    #[inline]
    pub fn new_worker(identifier: impl Into<WorkLocation>) -> Self { Self::Worker(identifier.into()) }



    /// Returns if this Location is a central location.
    /// 
    /// # Returns
    /// True if self is [`Self::Central`](Location::Central), or false otherwise.
    #[inline]
    pub fn is_central(&self) -> bool { matches!(self, Self::Central) }

    /// Returns if this Location is a worker location.
    /// 
    /// # Returns
    /// True if self is [`Self::Worker`](Location::Worker), or false otherwise.
    #[inline]
    pub fn is_worker(&self) -> bool { matches!(self, Self::Worker(_)) }
    /// Returns the worker location embedded in this location.
    /// 
    /// # Returns
    /// A reference to the internal [`WorkLocation`].
    /// 
    /// # Panics
    /// This function panics if we are not a [`Self::Worker`](Location::Worker).
    #[inline]
    pub fn worker(&self) -> &WorkLocation { if let Self::Worker(w) = self { w } else { panic!("Failed to unwrap {:?} as a Location::Worker", self.variant()); } }
    /// Returns the worker location embedded in this location.
    /// 
    /// # Returns
    /// A mutable reference to the internal [`WorkLocation`].
    /// 
    /// # Panics
    /// This function panics if we are not a [`Self::Worker`](Location::Worker).
    #[inline]
    pub fn worker_mut(&mut self) -> &mut WorkLocation { if let Self::Worker(w) = self { w } else { panic!("Failed to unwrap {:?} as a Location::Worker", self.variant()); } }
    /// Returns the worker location embedded in this location.
    /// 
    /// # Returns
    /// The internal [`WorkLocation`].
    /// 
    /// # Panics
    /// This function panics if we are not a [`Self::Worker`](Location::Worker).
    #[inline]
    pub fn into_worker(self) -> WorkLocation { if let Self::Worker(w) = self { w } else { panic!("Failed to unwrap {:?} as a Location::Worker", self.variant()); } }
}
impl Display for Location {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Central     => write!(f, "@<central>"),
            Self::Worker(loc) => write!(f, "{loc}"),
        }
    }
}
impl PartialEq<WorkLocation> for Location {
    #[inline]
    fn eq(&self, other: &WorkLocation) -> bool {
        if let Self::Worker(loc) = self {
            return loc == other
        } else {
            false
        }
    }
}

impl AsRef<Location> for Location {
    #[inline]
    fn as_ref(&self) -> &Self { self }
}
impl AsMut<Location> for Location {
    #[inline]
    fn as_mut(&mut self) -> &mut Self { self }
}
impl From<&Location> for Location {
    #[inline]
    fn from(value: &Self) -> Self { value.clone() }
}
impl From<&mut Location> for Location {
    #[inline]
    fn from(value: &mut Self) -> Self { value.clone() }
}

impl From<WorkLocation> for Location {
    #[inline]
    fn from(value: WorkLocation) -> Self { Self::Worker(value) }
}
impl From<&WorkLocation> for Location {
    #[inline]
    fn from(value: &WorkLocation) -> Self { Self::from(value.clone()) }
}
impl From<&mut WorkLocation> for Location {
    #[inline]
    fn from(value: &mut WorkLocation) -> Self { Self::from(value.clone()) }
}



/// Defines a [`Location`] but then one where work can happen.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct WorkLocation(String);

impl WorkLocation {
    /// Constructs a new WorkLocation based on the given identifier.
    /// 
    /// # Arguments
    /// - `identifier`: The identifier/name of the worker node.
    /// 
    /// # Returns
    /// A new instance of Self that refers to the given worker.
    #[inline]
    pub fn new(identifier: impl Into<String>) -> Self { Self(identifier.into()) }

    /// Creates a new [`WorkLocation`] from the given [`brane_dsl`] [`Location`](brane_dsl::Location).
    /// 
    /// # Arguments
    /// - `location`: the [`brane_dsl`] [`Location`](brane_dsl::Location) to mimic.
    /// 
    /// # Returns
    /// A new instance of Self that represents the same location as `location`.
    #[inline]
    pub fn from_dsl(location: impl Into<brane_dsl::Location>) -> Self { Self(location.into().0) }
}
impl Display for WorkLocation {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "@{}", self.0)
    }
}
impl PartialEq<Location> for WorkLocation {
    #[inline]
    fn eq(&self, other: &Location) -> bool { other.eq(self) }
}

impl Deref for WorkLocation {
    type Target = String;

    #[inline]
    fn deref(&self) -> &Self::Target { &self.0 }
}
impl DerefMut for WorkLocation {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
}

impl AsRef<WorkLocation> for WorkLocation {
    #[inline]
    fn as_ref(&self) -> &Self { self }
}
impl AsMut<WorkLocation> for WorkLocation {
    #[inline]
    fn as_mut(&mut self) -> &mut Self { self }
}
impl From<&WorkLocation> for WorkLocation {
    #[inline]
    fn from(value: &Self) -> Self { value.clone() }
}
impl From<&mut WorkLocation> for WorkLocation {
    #[inline]
    fn from(value: &mut Self) -> Self { value.clone() }
}

impl<T: Deref<Target = str>> From<T> for WorkLocation {
    #[inline]
    fn from(value: T) -> Self { Self(String::from(value.deref())) }
}
impl From<Location> for WorkLocation {
    #[inline]
    fn from(value: Location) -> Self { value.into_worker() }
}
impl From<&Location> for WorkLocation {
    #[inline]
    fn from(value: &Location) -> Self { value.worker().clone() }
}
impl From<&mut Location> for WorkLocation {
    #[inline]
    fn from(value: &mut Location) -> Self { value.worker().clone() }
}



/// Contains location restrictions for a certain node.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Locations {
    /// All locations are allowed.
    All,
    /// If not all, then the following locations are allowed as a whitelist.
    Restricted(Vec<WorkLocation>),
}

impl Locations {
    /// Returns the restrictive list of locations if this Locations is, in fact, restrictive.
    /// 
    /// # Returns
    /// A slice that to the whitelist of locations.
    /// 
    /// # Panics
    /// This function panics if the Locations was not `Locations::Restricted`. Use `Locations::is_restrictive` to query beforehand.
    #[inline]
    pub fn restricted(&self) -> &[WorkLocation] { if let Self::Restricted(locs) = self { locs } else { panic!("Cannot unwrap Locations::{self:?} as restricted"); } }



    /// Returns whether this Locations is an open-to-all kinda thing.
    #[inline]
    pub fn is_all(&self) -> bool { matches!(self, Self::All) }

    /// Returns whether this Locations is a restrictive list.
    #[inline]
    pub fn is_restrictive(&self) -> bool { matches!(self, Self::Restricted(_)) }
}

impl From<AllowedLocations> for Locations {
    #[inline]
    fn from(value: AllowedLocations) -> Self {
        match value {
            AllowedLocations::All             => Self::All,
            AllowedLocations::Exclusive(locs) => Self::Restricted(locs.into_iter().map(|l| WorkLocation::from_dsl(l)).collect()),
        }
    }
}
