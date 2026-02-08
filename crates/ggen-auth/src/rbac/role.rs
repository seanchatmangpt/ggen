//! Role hierarchy and role-permission management

use super::permission::{Permission, Permissions};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Role level in hierarchy
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum RoleLevel {
    /// Guest user - minimal permissions
    Guest = 0,
    /// Regular user - standard permissions
    User = 1,
    /// Manager - elevated permissions
    Manager = 2,
    /// Administrator - full permissions
    Admin = 3,
}

impl RoleLevel {
    /// Check if this role level is higher or equal to another
    pub fn is_higher_or_equal(&self, other: &RoleLevel) -> bool {
        (*self as u8) >= (*other as u8)
    }

    /// Parse from string
    pub fn from_str(s: &str) -> Option<RoleLevel> {
        match s.to_lowercase().as_str() {
            "guest" => Some(RoleLevel::Guest),
            "user" => Some(RoleLevel::User),
            "manager" => Some(RoleLevel::Manager),
            "admin" => Some(RoleLevel::Admin),
            _ => None,
        }
    }
}

/// Role with permissions and metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Role {
    /// Role identifier
    pub id: String,
    /// Role name
    pub name: String,
    /// Role level in hierarchy
    pub level: RoleLevel,
    /// Permissions granted by this role
    pub permissions: Permissions,
    /// Description of the role
    pub description: String,
    /// When the role was created
    pub created_at: DateTime<Utc>,
    /// Whether the role is active
    pub is_active: bool,
}

impl Role {
    /// Create a new role
    pub fn new(
        id: String, name: String, level: RoleLevel, permissions: Permissions, description: String,
    ) -> Self {
        Self {
            id,
            name,
            level,
            permissions,
            description,
            created_at: Utc::now(),
            is_active: true,
        }
    }

    /// Check if role has a specific permission
    pub fn has_permission(&self, permission: Permission) -> bool {
        self.permissions.has_permission(permission)
    }

    /// Add a permission to this role
    pub fn add_permission(&mut self, permission: Permission) {
        self.permissions.add_permission(permission);
    }

    /// Remove a permission from this role
    pub fn remove_permission(&mut self, permission: Permission) {
        self.permissions.remove_permission(permission);
    }

    /// Check if this role is higher or equal in hierarchy
    pub fn is_higher_or_equal(&self, other: &Role) -> bool {
        self.level.is_higher_or_equal(&other.level)
    }

    /// Create a guest role (read-only)
    pub fn guest() -> Self {
        Self::new(
            "role_guest".to_string(),
            "Guest".to_string(),
            RoleLevel::Guest,
            Permissions::READ,
            "Guest user with read-only access".to_string(),
        )
    }

    /// Create a user role (read, write, execute)
    pub fn user() -> Self {
        Self::new(
            "role_user".to_string(),
            "User".to_string(),
            RoleLevel::User,
            Permissions::READ | Permissions::WRITE | Permissions::EXECUTE,
            "Regular user with standard permissions".to_string(),
        )
    }

    /// Create a manager role (all except system-level operations)
    pub fn manager() -> Self {
        Self::new(
            "role_manager".to_string(),
            "Manager".to_string(),
            RoleLevel::Manager,
            Permissions::ALL,
            "Manager with elevated permissions".to_string(),
        )
    }

    /// Create an admin role (all permissions)
    pub fn admin() -> Self {
        Self::new(
            "role_admin".to_string(),
            "Admin".to_string(),
            RoleLevel::Admin,
            Permissions::ALL,
            "Administrator with full system access".to_string(),
        )
    }
}

/// Role hierarchy management
#[derive(Debug, Clone)]
pub struct RoleHierarchy {
    roles: Vec<Role>,
}

impl RoleHierarchy {
    /// Create a new role hierarchy
    pub fn new() -> Self {
        Self {
            roles: vec![Role::guest(), Role::user(), Role::manager(), Role::admin()],
        }
    }

    /// Add a custom role
    pub fn add_role(&mut self, role: Role) {
        self.roles.push(role);
    }

    /// Get role by ID
    pub fn get_role(&self, role_id: &str) -> Option<&Role> {
        self.roles.iter().find(|r| r.id == role_id)
    }

    /// Get role by name
    pub fn get_role_by_name(&self, name: &str) -> Option<&Role> {
        self.roles.iter().find(|r| r.name == name)
    }

    /// Get all roles at or above a certain level
    pub fn get_roles_at_level(&self, level: RoleLevel) -> Vec<&Role> {
        self.roles
            .iter()
            .filter(|r| r.level.is_higher_or_equal(&level))
            .collect()
    }

    /// Check if a set of roles has a specific permission
    pub fn has_permission(&self, role_ids: &[String], permission: Permission) -> bool {
        role_ids.iter().any(|id| {
            self.get_role(id)
                .map(|r| r.has_permission(permission))
                .unwrap_or(false)
        })
    }

    /// Get combined permissions from multiple roles
    pub fn combined_permissions(&self, role_ids: &[String]) -> Permissions {
        role_ids
            .iter()
            .filter_map(|id| self.get_role(id))
            .fold(Permissions::empty(), |acc, role| acc | role.permissions)
    }
}

impl Default for RoleHierarchy {
    fn default() -> Self {
        Self::new()
    }
}

/// User-Role assignment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UserRole {
    /// User identifier
    pub user_id: String,
    /// Role identifier
    pub role_id: String,
    /// When the role was assigned
    pub assigned_at: DateTime<Utc>,
    /// Optional expiration date
    pub expires_at: Option<DateTime<Utc>>,
    /// Who assigned this role
    pub assigned_by: String,
}

impl UserRole {
    /// Create a new user-role assignment
    pub fn new(user_id: String, role_id: String, assigned_by: String) -> Self {
        Self {
            user_id,
            role_id,
            assigned_at: Utc::now(),
            expires_at: None,
            assigned_by,
        }
    }

    /// Check if assignment is expired
    pub fn is_expired(&self) -> bool {
        self.expires_at
            .map(|expires| Utc::now() > expires)
            .unwrap_or(false)
    }

    /// Set expiration date
    pub fn with_expiration(mut self, expires_at: DateTime<Utc>) -> Self {
        self.expires_at = Some(expires_at);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_role_level_hierarchy() {
        // Arrange & Act & Assert
        assert!(RoleLevel::Admin.is_higher_or_equal(&RoleLevel::Manager));
        assert!(RoleLevel::Manager.is_higher_or_equal(&RoleLevel::User));
        assert!(RoleLevel::User.is_higher_or_equal(&RoleLevel::Guest));
        assert!(!RoleLevel::Guest.is_higher_or_equal(&RoleLevel::User));
    }

    #[test]
    fn test_role_guest_permissions() {
        // Arrange
        let role = Role::guest();

        // Act & Assert
        assert!(role.has_permission(Permission::Read));
        assert!(!role.has_permission(Permission::Write));
        assert!(!role.has_permission(Permission::Delete));
        assert!(!role.has_permission(Permission::Execute));
    }

    #[test]
    fn test_role_user_permissions() {
        // Arrange
        let role = Role::user();

        // Act & Assert
        assert!(role.has_permission(Permission::Read));
        assert!(role.has_permission(Permission::Write));
        assert!(!role.has_permission(Permission::Delete));
        assert!(role.has_permission(Permission::Execute));
    }

    #[test]
    fn test_role_admin_permissions() {
        // Arrange
        let role = Role::admin();

        // Act & Assert
        assert!(role.has_permission(Permission::Read));
        assert!(role.has_permission(Permission::Write));
        assert!(role.has_permission(Permission::Delete));
        assert!(role.has_permission(Permission::Execute));
    }

    #[test]
    fn test_role_add_permission() {
        // Arrange
        let mut role = Role::guest();

        // Act
        role.add_permission(Permission::Write);

        // Assert
        assert!(role.has_permission(Permission::Write));
    }

    #[test]
    fn test_role_remove_permission() {
        // Arrange
        let mut role = Role::admin();

        // Act
        role.remove_permission(Permission::Delete);

        // Assert
        assert!(!role.has_permission(Permission::Delete));
        assert!(role.has_permission(Permission::Read));
    }

    #[test]
    fn test_role_is_higher_or_equal() {
        // Arrange
        let admin = Role::admin();
        let user = Role::user();

        // Act & Assert
        assert!(admin.is_higher_or_equal(&user));
        assert!(!user.is_higher_or_equal(&admin));
    }

    #[test]
    fn test_role_hierarchy_new() {
        // Arrange & Act
        let hierarchy = RoleHierarchy::new();

        // Assert
        assert_eq!(hierarchy.roles.len(), 4);
    }

    #[test]
    fn test_role_hierarchy_get_role() {
        // Arrange
        let hierarchy = RoleHierarchy::new();

        // Act
        let role = hierarchy.get_role("role_admin");

        // Assert
        assert!(role.is_some());
        assert_eq!(role.unwrap().name, "Admin");
    }

    #[test]
    fn test_role_hierarchy_has_permission() {
        // Arrange
        let hierarchy = RoleHierarchy::new();
        let role_ids = vec!["role_user".to_string()];

        // Act
        let has_read = hierarchy.has_permission(&role_ids, Permission::Read);
        let has_delete = hierarchy.has_permission(&role_ids, Permission::Delete);

        // Assert
        assert!(has_read);
        assert!(!has_delete);
    }

    #[test]
    fn test_role_hierarchy_combined_permissions() {
        // Arrange
        let hierarchy = RoleHierarchy::new();
        let role_ids = vec!["role_guest".to_string(), "role_user".to_string()];

        // Act
        let perms = hierarchy.combined_permissions(&role_ids);

        // Assert
        assert!(perms.has_permission(Permission::Read));
        assert!(perms.has_permission(Permission::Write));
        assert!(perms.has_permission(Permission::Execute));
    }

    #[test]
    fn test_user_role_new() {
        // Arrange & Act
        let user_role = UserRole::new(
            "user123".to_string(),
            "role_admin".to_string(),
            "admin456".to_string(),
        );

        // Assert
        assert_eq!(user_role.user_id, "user123");
        assert_eq!(user_role.role_id, "role_admin");
        assert!(!user_role.is_expired());
    }

    #[test]
    fn test_user_role_expiration() {
        // Arrange
        let past_date = Utc::now() - chrono::Duration::days(1);
        let user_role = UserRole::new(
            "user123".to_string(),
            "role_admin".to_string(),
            "admin456".to_string(),
        )
        .with_expiration(past_date);

        // Act
        let is_expired = user_role.is_expired();

        // Assert
        assert!(is_expired);
    }
}
