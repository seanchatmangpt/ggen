//! Role-Based Access Control (RBAC) system for ggen
//!
//! Implements enterprise-grade authorization with:
//! - Role hierarchy (Admin > Manager > User > Guest)
//! - Fine-grained permissions (read, write, delete, execute)
//! - Resource-level authorization
//! - Policy engine for attribute-based access control (ABAC)
//! - Privilege escalation prevention

pub mod context;
pub mod permission;
pub mod policy;
pub mod resource;
pub mod role;

pub use context::{AuthorizationContext, AuthorizationDecision, AuthorizationRequest};
pub use permission::{Permission, Permissions};
pub use policy::{Effect, Policy, PolicyEngine, PolicyRule};
pub use resource::{Resource, ResourceOwner, ResourceType};
pub use role::{Role, RoleHierarchy, RoleLevel, UserRole};

use crate::errors::AuthError;

/// Result type for RBAC operations
pub type RbacResult<T> = Result<T, AuthError>;

/// Check if a user with given roles can perform an action on a resource
pub fn authorize(
    user_id: &str, roles: &[Role], resource: &Resource, permission: Permission,
    policy_engine: &PolicyEngine,
) -> RbacResult<bool> {
    // Create authorization context
    let context = AuthorizationContext::new(user_id, roles, resource);

    // Create authorization request
    let request = AuthorizationRequest {
        user_id: user_id.to_string(),
        resource_type: resource.resource_type.clone(),
        resource_id: resource.id.clone(),
        action: permission,
        context: context.clone(),
    };

    // Check ownership first
    if resource.owner.user_id == user_id {
        return Ok(true);
    }

    // Check role permissions
    for role in roles {
        if role.has_permission(permission) {
            // Role has permission, now check policies
            let decision = policy_engine.evaluate(&request)?;
            return Ok(decision.is_allowed());
        }
    }

    Ok(false)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_authorize_owner_always_allowed() {
        // Arrange
        let user_id = "user123";
        let resource = Resource {
            id: "resource1".to_string(),
            resource_type: ResourceType::Template,
            owner: ResourceOwner::new(user_id),
            permissions: Permissions::empty(),
        };
        let roles = vec![];
        let policy_engine = PolicyEngine::new();

        // Act
        let result = authorize(user_id, &roles, &resource, Permission::READ, &policy_engine);

        // Assert
        assert!(result.is_ok());
        assert!(result.unwrap());
    }

    #[test]
    fn test_authorize_with_role_permission() {
        // Arrange
        let user_id = "user123";
        let owner_id = "owner456";
        let resource = Resource {
            id: "resource1".to_string(),
            resource_type: ResourceType::Template,
            owner: ResourceOwner::new(owner_id),
            permissions: Permissions::empty(),
        };
        let roles = vec![Role::user()];
        let policy_engine = PolicyEngine::new();

        // Act
        let result = authorize(user_id, &roles, &resource, Permission::READ, &policy_engine);

        // Assert
        assert!(result.is_ok());
        assert!(result.unwrap());
    }

    #[test]
    fn test_authorize_without_permission_denied() {
        // Arrange
        let user_id = "user123";
        let owner_id = "owner456";
        let resource = Resource {
            id: "resource1".to_string(),
            resource_type: ResourceType::Template,
            owner: ResourceOwner::new(owner_id),
            permissions: Permissions::empty(),
        };
        let roles = vec![Role::guest()];
        let policy_engine = PolicyEngine::new();

        // Act
        let result = authorize(
            user_id,
            &roles,
            &resource,
            Permission::DELETE,
            &policy_engine,
        );

        // Assert
        assert!(result.is_ok());
        assert!(!result.unwrap());
    }
}
