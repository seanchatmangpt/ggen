//! Resource ownership and authorization

use super::permission::Permissions;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Resource type enumeration
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ResourceType {
    /// Template resource
    Template,
    /// Project resource
    Project,
    /// Package in marketplace
    Package,
    /// API key
    ApiKey,
    /// User data
    UserData,
    /// Organization
    Organization,
    /// Custom resource type
    Custom(String),
}

impl ResourceType {
    /// Parse from string
    pub fn from_string(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "template" => ResourceType::Template,
            "project" => ResourceType::Project,
            "package" => ResourceType::Package,
            "apikey" => ResourceType::ApiKey,
            "userdata" => ResourceType::UserData,
            "organization" => ResourceType::Organization,
            _ => ResourceType::Custom(s.to_string()),
        }
    }
}

/// Resource owner information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceOwner {
    /// Owner user ID
    pub user_id: String,
    /// Owner organization ID (optional)
    pub org_id: Option<String>,
    /// When ownership was established
    pub owned_since: DateTime<Utc>,
}

impl ResourceOwner {
    /// Create a new resource owner
    pub fn new(user_id: &str) -> Self {
        Self {
            user_id: user_id.to_string(),
            org_id: None,
            owned_since: Utc::now(),
        }
    }

    /// Create owner with organization
    pub fn with_org(user_id: &str, org_id: &str) -> Self {
        Self {
            user_id: user_id.to_string(),
            org_id: Some(org_id.to_string()),
            owned_since: Utc::now(),
        }
    }

    /// Check if user is the owner
    pub fn is_owner(&self, user_id: &str) -> bool {
        self.user_id == user_id
    }

    /// Check if user is in owner's organization
    pub fn is_org_member(&self, org_id: &str) -> bool {
        self.org_id.as_ref().map(|id| id == org_id).unwrap_or(false)
    }
}

/// Resource with ownership and permissions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Resource {
    /// Resource identifier
    pub id: String,
    /// Resource type
    pub resource_type: ResourceType,
    /// Resource owner
    pub owner: ResourceOwner,
    /// Public permissions (applied to all users)
    pub permissions: Permissions,
}

impl Resource {
    /// Create a new resource
    pub fn new(
        id: String, resource_type: ResourceType, owner: ResourceOwner, permissions: Permissions,
    ) -> Self {
        Self {
            id,
            resource_type,
            owner,
            permissions,
        }
    }

    /// Check if user is the owner
    pub fn is_owner(&self, user_id: &str) -> bool {
        self.owner.is_owner(user_id)
    }

    /// Check if user is in owner's organization
    pub fn is_org_member(&self, org_id: &str) -> bool {
        self.owner.is_org_member(org_id)
    }
}

/// Resource access control entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceAcl {
    /// Resource ID
    pub resource_id: String,
    /// User or role ID
    pub principal_id: String,
    /// Principal type (user or role)
    pub principal_type: PrincipalType,
    /// Granted permissions
    pub permissions: Permissions,
    /// When ACL was created
    pub created_at: DateTime<Utc>,
    /// Optional expiration
    pub expires_at: Option<DateTime<Utc>>,
}

/// Principal type for ACL entries
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum PrincipalType {
    /// User principal
    User,
    /// Role principal
    Role,
    /// Organization principal
    Organization,
}

impl ResourceAcl {
    /// Create a new ACL entry
    pub fn new(
        resource_id: String, principal_id: String, principal_type: PrincipalType,
        permissions: Permissions,
    ) -> Self {
        Self {
            resource_id,
            principal_id,
            principal_type,
            permissions,
            created_at: Utc::now(),
            expires_at: None,
        }
    }

    /// Check if ACL is expired
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
    use crate::rbac::permission::Permission;

    #[test]
    fn test_resource_type_from_string() {
        // Arrange & Act & Assert
        assert_eq!(
            ResourceType::from_string("template"),
            ResourceType::Template
        );
        assert_eq!(ResourceType::from_string("PROJECT"), ResourceType::Project);
        assert_eq!(
            ResourceType::from_string("custom"),
            ResourceType::Custom("custom".to_string())
        );
    }

    #[test]
    fn test_resource_owner_new() {
        // Arrange & Act
        let owner = ResourceOwner::new("user123");

        // Assert
        assert_eq!(owner.user_id, "user123");
        assert!(owner.org_id.is_none());
    }

    #[test]
    fn test_resource_owner_with_org() {
        // Arrange & Act
        let owner = ResourceOwner::with_org("user123", "org456");

        // Assert
        assert_eq!(owner.user_id, "user123");
        assert_eq!(owner.org_id, Some("org456".to_string()));
    }

    #[test]
    fn test_resource_owner_is_owner() {
        // Arrange
        let owner = ResourceOwner::new("user123");

        // Act & Assert
        assert!(owner.is_owner("user123"));
        assert!(!owner.is_owner("user456"));
    }

    #[test]
    fn test_resource_owner_is_org_member() {
        // Arrange
        let owner = ResourceOwner::with_org("user123", "org456");

        // Act & Assert
        assert!(owner.is_org_member("org456"));
        assert!(!owner.is_org_member("org789"));
    }

    #[test]
    fn test_resource_new() {
        // Arrange
        let owner = ResourceOwner::new("user123");
        let permissions = Permissions::READ;

        // Act
        let resource = Resource::new(
            "resource1".to_string(),
            ResourceType::Template,
            owner,
            permissions,
        );

        // Assert
        assert_eq!(resource.id, "resource1");
        assert_eq!(resource.resource_type, ResourceType::Template);
    }

    #[test]
    fn test_resource_is_owner() {
        // Arrange
        let owner = ResourceOwner::new("user123");
        let resource = Resource::new(
            "resource1".to_string(),
            ResourceType::Template,
            owner,
            Permissions::empty(),
        );

        // Act & Assert
        assert!(resource.is_owner("user123"));
        assert!(!resource.is_owner("user456"));
    }

    #[test]
    fn test_resource_acl_new() {
        // Arrange & Act
        let acl = ResourceAcl::new(
            "resource1".to_string(),
            "user123".to_string(),
            PrincipalType::User,
            Permissions::READ | Permissions::WRITE,
        );

        // Assert
        assert_eq!(acl.resource_id, "resource1");
        assert_eq!(acl.principal_id, "user123");
        assert_eq!(acl.principal_type, PrincipalType::User);
        assert!(acl.permissions.has_permission(Permission::Read));
        assert!(acl.permissions.has_permission(Permission::Write));
    }

    #[test]
    fn test_resource_acl_expiration() {
        // Arrange
        let past_date = Utc::now() - chrono::Duration::days(1);
        let acl = ResourceAcl::new(
            "resource1".to_string(),
            "user123".to_string(),
            PrincipalType::User,
            Permissions::READ,
        )
        .with_expiration(past_date);

        // Act
        let is_expired = acl.is_expired();

        // Assert
        assert!(is_expired);
    }

    #[test]
    fn test_resource_acl_not_expired() {
        // Arrange
        let future_date = Utc::now() + chrono::Duration::days(1);
        let acl = ResourceAcl::new(
            "resource1".to_string(),
            "user123".to_string(),
            PrincipalType::User,
            Permissions::READ,
        )
        .with_expiration(future_date);

        // Act
        let is_expired = acl.is_expired();

        // Assert
        assert!(!is_expired);
    }
}
