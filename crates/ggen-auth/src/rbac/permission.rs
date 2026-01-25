//! Permission system using bitflags for efficient permission checking

use serde::{Deserialize, Serialize};
use std::fmt;

bitflags::bitflags! {
    /// Fine-grained permissions for resource access
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
    #[serde(transparent)]
    pub struct Permissions: u32 {
        /// Read access to resource
        const READ    = 0b0001;
        /// Write/modify access to resource
        const WRITE   = 0b0010;
        /// Delete access to resource
        const DELETE  = 0b0100;
        /// Execute access to resource (run templates, scripts)
        const EXECUTE = 0b1000;

        /// Common permission sets
        const READ_WRITE = Self::READ.bits() | Self::WRITE.bits();
        const READ_EXECUTE = Self::READ.bits() | Self::EXECUTE.bits();
        const ALL = Self::READ.bits() | Self::WRITE.bits() | Self::DELETE.bits() | Self::EXECUTE.bits();
    }
}

/// Individual permission type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Permission {
    /// Read access
    Read,
    /// Write access
    Write,
    /// Delete access
    Delete,
    /// Execute access
    Execute,
}

impl Permission {
    /// Convert to bitflag
    pub fn to_bits(self) -> Permissions {
        match self {
            Permission::Read => Permissions::READ,
            Permission::Write => Permissions::WRITE,
            Permission::Delete => Permissions::DELETE,
            Permission::Execute => Permissions::EXECUTE,
        }
    }

    /// Get all permissions
    pub fn all() -> Vec<Permission> {
        vec![
            Permission::Read,
            Permission::Write,
            Permission::Delete,
            Permission::Execute,
        ]
    }

    /// Parse from string
    pub fn from_str(s: &str) -> Option<Permission> {
        match s.to_lowercase().as_str() {
            "read" => Some(Permission::Read),
            "write" => Some(Permission::Write),
            "delete" => Some(Permission::Delete),
            "execute" => Some(Permission::Execute),
            _ => None,
        }
    }
}

impl fmt::Display for Permission {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Permission::Read => write!(f, "read"),
            Permission::Write => write!(f, "write"),
            Permission::Delete => write!(f, "delete"),
            Permission::Execute => write!(f, "execute"),
        }
    }
}

impl Permissions {
    /// Check if permissions include a specific permission
    pub fn has_permission(&self, permission: Permission) -> bool {
        self.contains(permission.to_bits())
    }

    /// Add a permission
    pub fn add_permission(&mut self, permission: Permission) {
        self.insert(permission.to_bits());
    }

    /// Remove a permission
    pub fn remove_permission(&mut self, permission: Permission) {
        self.remove(permission.to_bits());
    }

    /// Create from individual permissions
    pub fn from_permissions(permissions: &[Permission]) -> Self {
        permissions
            .iter()
            .fold(Permissions::empty(), |acc, p| acc | p.to_bits())
    }

    /// Get list of individual permissions
    pub fn to_vec(&self) -> Vec<Permission> {
        Permission::all()
            .into_iter()
            .filter(|p| self.has_permission(*p))
            .collect()
    }
}

impl Default for Permissions {
    fn default() -> Self {
        Permissions::empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_permission_to_bits() {
        // Arrange & Act
        let read_bits = Permission::Read.to_bits();
        let write_bits = Permission::Write.to_bits();

        // Assert
        assert_eq!(read_bits, Permissions::READ);
        assert_eq!(write_bits, Permissions::WRITE);
    }

    #[test]
    fn test_permissions_has_permission() {
        // Arrange
        let perms = Permissions::READ | Permissions::WRITE;

        // Act & Assert
        assert!(perms.has_permission(Permission::Read));
        assert!(perms.has_permission(Permission::Write));
        assert!(!perms.has_permission(Permission::Delete));
        assert!(!perms.has_permission(Permission::Execute));
    }

    #[test]
    fn test_permissions_add_permission() {
        // Arrange
        let mut perms = Permissions::READ;

        // Act
        perms.add_permission(Permission::Write);

        // Assert
        assert!(perms.has_permission(Permission::Read));
        assert!(perms.has_permission(Permission::Write));
    }

    #[test]
    fn test_permissions_remove_permission() {
        // Arrange
        let mut perms = Permissions::READ | Permissions::WRITE;

        // Act
        perms.remove_permission(Permission::Write);

        // Assert
        assert!(perms.has_permission(Permission::Read));
        assert!(!perms.has_permission(Permission::Write));
    }

    #[test]
    fn test_permissions_from_permissions() {
        // Arrange
        let permission_list = vec![Permission::Read, Permission::Execute];

        // Act
        let perms = Permissions::from_permissions(&permission_list);

        // Assert
        assert!(perms.has_permission(Permission::Read));
        assert!(perms.has_permission(Permission::Execute));
        assert!(!perms.has_permission(Permission::Write));
    }

    #[test]
    fn test_permissions_to_vec() {
        // Arrange
        let perms = Permissions::READ | Permissions::DELETE;

        // Act
        let vec = perms.to_vec();

        // Assert
        assert_eq!(vec.len(), 2);
        assert!(vec.contains(&Permission::Read));
        assert!(vec.contains(&Permission::Delete));
    }

    #[test]
    fn test_permission_from_str() {
        // Arrange & Act & Assert
        assert_eq!(Permission::from_str("read"), Some(Permission::Read));
        assert_eq!(Permission::from_str("WRITE"), Some(Permission::Write));
        assert_eq!(Permission::from_str("Delete"), Some(Permission::Delete));
        assert_eq!(Permission::from_str("execute"), Some(Permission::Execute));
        assert_eq!(Permission::from_str("invalid"), None);
    }

    #[test]
    fn test_permission_display() {
        // Arrange & Act & Assert
        assert_eq!(format!("{}", Permission::Read), "read");
        assert_eq!(format!("{}", Permission::Write), "write");
        assert_eq!(format!("{}", Permission::Delete), "delete");
        assert_eq!(format!("{}", Permission::Execute), "execute");
    }

    #[test]
    fn test_permissions_all_constant() {
        // Arrange
        let all = Permissions::ALL;

        // Act & Assert
        assert!(all.has_permission(Permission::Read));
        assert!(all.has_permission(Permission::Write));
        assert!(all.has_permission(Permission::Delete));
        assert!(all.has_permission(Permission::Execute));
    }

    #[test]
    fn test_permissions_read_write_constant() {
        // Arrange
        let read_write = Permissions::READ_WRITE;

        // Act & Assert
        assert!(read_write.has_permission(Permission::Read));
        assert!(read_write.has_permission(Permission::Write));
        assert!(!read_write.has_permission(Permission::Delete));
    }
}
