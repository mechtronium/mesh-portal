use crate::error::MsgErr;
use crate::version::v0_0_1::id::id::Point;
use crate::version::v0_0_1::messaging::messaging::ScopeGrant;
use crate::version::v0_0_1::parse::error::result;
use crate::version::v0_0_1::parse::{MapResolver, particle_perms, permissions, permissions_mask, privilege};
use crate::version::v0_0_1::selector::selector::{PointKindHierarchy, PointSelector};
use nom::combinator::all_consuming;
use nom_supreme::parser_ext::MapRes;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::ops;
use std::ops::{Deref, DerefMut};
use std::str::FromStr;
use crate::version::v0_0_1::messaging::messaging::ScopeGrantAspect::Priv;
use crate::version::v0_0_1::span::{new_span};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Access {
    // bool is true if Super is also Owner
    Super(bool),
    Owner,
    Enumerated(EnumeratedAccess),
}

impl Access {
    pub fn has_super(&self) -> bool {
        match self {
            Access::Super(_) => true,
            _ => false,
        }
    }

    pub fn has_owner(&self) -> bool {
        match self {
            Access::Owner => true,
            Access::Super(owner) => owner.clone(),
            _ => false,
        }
    }

    pub fn has_full(&self) -> bool {
        match self {
            Access::Super(_) => true,
            Access::Owner => true,
            Access::Enumerated(_) => false,
        }
    }

    pub fn none() -> Self {
        Self::Enumerated(EnumeratedAccess::none())
    }

    pub fn permissions(&self) -> Permissions {
        match self {
            Access::Super(_) => Permissions::full(),
            Access::Owner => Permissions::full(),
            Access::Enumerated(enumerated) => enumerated.permissions.clone(),
        }
    }

    pub fn check_privilege(&self, privilege: &str) -> Result<(), MsgErr> {
        match self {
            Access::Super(_) => Ok(()),
            Access::Owner => Ok(()),
            Access::Enumerated(enumerated) => {
                match enumerated.privileges.has(privilege).is_ok() {
                    true => Ok(()),
                    false => Err(format!("'{}'", privilege).into()),
                }
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Privileges {
    Full,
    Enumerated(EnumeratedPrivileges),
}

impl Privileges {
    pub fn has(&self, privilege: &str) -> Result<(), ()> {
        match self {
            Privileges::Full => Ok(()),
            Privileges::Enumerated(privileges) => privileges.has(privilege),
        }
    }

    pub fn none() -> Self {
        Self::Enumerated(EnumeratedPrivileges::none())
    }

    pub fn or(mut self, other: &Self) -> Self {
        match self {
            Privileges::Full => self,
            Privileges::Enumerated(privileges) => match other {
                Privileges::Full => Privileges::Full,
                Privileges::Enumerated(other) => Privileges::Enumerated(privileges.or(other)),
            },
        }
    }

    pub fn and(self, other: &Self) -> Privileges {
        match other {
            Privileges::Full => self,
            Privileges::Enumerated(enumerated_other) => match self {
                Privileges::Full => other.clone(),
                Privileges::Enumerated(enumerated_self) => {
                    Privileges::Enumerated(enumerated_self.and(enumerated_other))
                }
            },
        }
    }


}

impl ops::BitOr<&Privilege> for &Privileges {
    type Output = Privileges;

    fn bitor(self, rhs: &Privilege) -> Self::Output {
        match rhs {
            Privilege::Full => Privileges::Full,
            Privilege::Single(p) => {
                match self {
                    Privileges::Full => Privileges::Full,
                    Privileges::Enumerated(enumerated) => {
                        let mut enumerated = enumerated.clone();
                        enumerated.set.insert( p.to_string() );
                        Privileges::Enumerated(enumerated)
                    }
                }
            }
        }
    }
}

impl ops::BitOr<&Privilege> for Privileges {
    type Output = Privileges;

    fn bitor(self, rhs: &Privilege) -> Self::Output {
        match rhs {
            Privilege::Full => Privileges::Full,
            Privilege::Single(p) => {
                match self {
                    Privileges::Full => Privileges::Full,
                    Privileges::Enumerated(enumerated) => {
                        let mut enumerated = enumerated.clone();
                        enumerated.set.insert( p.to_string() );
                        Privileges::Enumerated(enumerated)
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnumeratedPrivileges {
    set: HashSet<String>,
}

impl EnumeratedPrivileges {
    pub fn new() -> Self {
        Self {
            set: HashSet::new(),
        }
    }

    pub fn none() -> Self {
        Self {
            set: HashSet::new(),
        }
    }

    pub fn or(mut self, other: &Self) -> Self {
        for p in other.set.iter() {
            if self.has(p).is_err() {
                self.add(p.as_str());
            }
        }
        self
    }

    pub fn and(mut self, other: &Self) -> Self {
        self.set.retain(|p| other.has(p).is_ok());
        self
    }

    pub fn add(&mut self, privilege: &str) {
        self.set.insert(privilege.to_string());
    }

    pub fn has(&self, privilege: &str) -> Result<(), ()> {
        let privilege = privilege.to_string();
        if self.set.contains(&privilege) {
            Ok(())
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum Privilege {
    Full,
    Single(String),
}

impl ToString for Privilege {
    fn to_string(&self) -> String {
        match self {
            Privilege::Full => "*".to_string(),
            Privilege::Single(name) => name.clone(),
        }
    }
}

impl FromStr for Privilege {
    type Err = MsgErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let span = new_span(s);
        Ok(result(all_consuming(privilege)(span))?)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnumeratedAccess {
    pub permissions: Permissions,
    pub privileges: Privileges,
}

impl EnumeratedAccess {
    pub fn mask(&mut self, scope_grant: &ScopeGrant) {}

    pub fn full() -> Self {
        Self {
            permissions: Permissions::full(),
            privileges: Privileges::Full,
        }
    }

    pub fn none() -> Self {
        Self {
            permissions: Permissions::none(),
            privileges: Privileges::none(),
        }
    }

    pub fn and(&mut self, access: &Self) {
        self.permissions.and(&access.permissions);
        self.privileges = self.privileges.clone().and(&access.privileges);
    }

    pub fn clear_privs(&mut self) {
        self.privileges = Privileges::none()
    }

    pub fn add(&mut self, grant: &AccessGrant) {
        match &grant.kind {
            AccessGrantKindDef::Super => {
                // we can't mask Super with Enumerated... it does nothing
            }
            AccessGrantKindDef::Privilege(prv) => {
                self.privileges = self.privileges.clone() | prv;
            },
            AccessGrantKindDef::PermissionsMask(mask) => match mask.kind {
                PermissionsMaskKind::Or => self.permissions.or(&mask.permissions),
                PermissionsMaskKind::And => self.permissions.and(&mask.permissions),
            },
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct PermissionsMask {
    pub kind: PermissionsMaskKind,
    pub permissions: Permissions,
}

impl FromStr for PermissionsMask {
    type Err = MsgErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = new_span(s);
        Ok(result(all_consuming(permissions_mask)(s))?)
    }
}

impl ToString for PermissionsMask {
    fn to_string(&self) -> String {
        match self.kind {
            PermissionsMaskKind::Or => format!("+{}", self.permissions.to_string()),
            PermissionsMaskKind::And => format!("&{}", self.permissions.to_string()),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Permissions {
    pub child: ChildPerms,
    pub particle: ParticlePerms,
}

impl FromStr for Permissions {
    type Err = MsgErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        result(permissions(new_span(s)))
    }
}

impl Permissions {
    pub fn full() -> Self {
        Self {
            child: ChildPerms::full(),
            particle: ParticlePerms::full(),
        }
    }

    pub fn none() -> Self {
        Self {
            child: ChildPerms::none(),
            particle: ParticlePerms::none(),
        }
    }

    pub fn or(&mut self, permissions: &Permissions) {
        self.child.or(&permissions.child);
        self.particle.or(&permissions.particle);
    }

    pub fn and(&mut self, permissions: &Permissions) {
        self.child.and(&permissions.child);
        self.particle.and(&permissions.particle);
    }
}


impl ToString for Permissions {
    fn to_string(&self) -> String {
        format!("{}-{}", self.child.to_string(), self.particle.to_string())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct ChildPerms {
    pub create: bool,
    pub select: bool,
    pub delete: bool,
}

impl ChildPerms {
    pub fn full() -> Self {
        Self {
            create: true,
            select: true,
            delete: true,
        }
    }

    pub fn none() -> Self {
        Self {
            create: false,
            select: false,
            delete: false,
        }
    }

    pub fn or(&mut self, block: &ChildPerms) {
        self.create |= block.create;
        self.select |= block.select;
        self.delete |= block.delete;
    }

    pub fn and(&mut self, block: &ChildPerms) {
        self.create &= block.create;
        self.select &= block.select;
        self.delete &= block.delete;
    }
}

impl ToString for ChildPerms {
    fn to_string(&self) -> String {
        let mut rtn = String::new();

        if self.create {
            rtn.push_str("C");
        } else {
            rtn.push_str("c");
        }

        if self.select {
            rtn.push_str("S");
        } else {
            rtn.push_str("s");
        }

        if self.delete {
            rtn.push_str("D");
        } else {
            rtn.push_str("d");
        }

        rtn
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct ParticlePerms {
    pub read: bool,
    pub write: bool,
    pub execute: bool,
}

impl ParticlePerms {
    pub fn full() -> Self {
        Self {
            read: true,
            write: true,
            execute: true,
        }
    }

    pub fn none() -> Self {
        Self {
            read: false,
            write: false,
            execute: false,
        }
    }

    pub fn or(&mut self, block: &ParticlePerms) {
        self.read |= block.read;
        self.write |= block.write;
        self.execute |= block.execute;
    }

    pub fn and(&mut self, block: &ParticlePerms) {
        self.read &= block.read;
        self.write &= block.write;
        self.execute &= block.execute;
    }
}

impl FromStr for ParticlePerms {
    type Err = MsgErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = new_span(s);
        Ok(result(all_consuming(particle_perms)(s))?)
    }
}

impl ToString for ParticlePerms {
    fn to_string(&self) -> String {
        let mut rtn = String::new();

        if self.read {
            rtn.push_str("R");
        } else {
            rtn.push_str("r");
        }

        if self.write {
            rtn.push_str("W");
        } else {
            rtn.push_str("w");
        }

        if self.execute {
            rtn.push_str("X");
        } else {
            rtn.push_str("x");
        }

        rtn
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum PermissionsMaskKind {
    Or,
    And,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AccessGrantDef<Priv, PermMask, PointSelector, Point> {
    pub kind: AccessGrantKindDef<Priv, PermMask>,
    pub on_point: PointSelector,
    pub to_point: PointSelector,
    pub by_particle: Point,
}

pub type GrantTo = GrantToDef<PointSelector>;
pub enum GrantToDef<PointSelector> {
    World,
    PointSelector(PointSelector)
}

impl GrantTo {
    pub fn is_match( &self, hierarchy: &PointKindHierarchy) -> Result<(),()> {
        match self {
            GrantTo::World => Ok(()),
            GrantTo::PointSelector(selector) => {
                match selector.matches(hierarchy) {
                    true => Ok(()),
                    false => Err(())
                }
            }
        }
    }
}

pub type AccessGrant = AccessGrantDef<Privilege, PermissionsMask, PointSelector, Point>;
pub type AccessGrantKind = AccessGrantKindDef<Privilege, PermissionsMask>;


#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AccessGrantKindDef<Priv, PermMask> {
    Super,
    Privilege(Priv),
    PermissionsMask(PermMask),
}

impl<Priv, PermMask> ToString for AccessGrantKindDef<Priv, PermMask> {
    fn to_string(&self) -> String {
        match self {
            AccessGrantKindDef::Super => "super".to_string(),
            AccessGrantKindDef::Privilege(_) => "priv".to_string(),
            AccessGrantKindDef::PermissionsMask(_) => "perm".to_string(),
        }
    }
}