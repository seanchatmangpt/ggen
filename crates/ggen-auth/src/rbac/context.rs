//! Authorization context and request structures

use super::permission::Permission;
use super::resource::{Resource, ResourceType};
use super::role::Role;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Authorization context containing user and resource information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuthorizationContext {
    /// User ID making the request
    pub user_id: String,
    /// User's roles
    pub roles: Vec<Role>,
    /// Resource being accessed
    pub resource: Resource,
    /// Additional context attributes
    pub attributes: HashMap<String, String>,
    /// Request timestamp
    pub timestamp: i64,
}

impl AuthorizationContext {
    /// Create a new authorization context
    pub fn new(user_id: &str, roles: &[Role], resource: &Resource) -> Self {
        Self {
            user_id: user_id.to_string(),
            roles: roles.to_vec(),
            resource: resource.clone(),
            attributes: HashMap::new(),
            timestamp: chrono::Utc::now().timestamp(),
        }
    }

    /// Add a context attribute
    pub fn with_attribute(mut self, key: String, value: String) -> Self {
        self.attributes.insert(key, value);
        self
    }

    /// Add multiple attributes
    pub fn with_attributes(mut self, attributes: HashMap<String, String>) -> Self {
        self.attributes.extend(attributes);
        self
    }

    /// Get an attribute value
    pub fn get_attribute(&self, key: &str) -> Option<&String> {
        self.attributes.get(key)
    }

    /// Check if user has a specific role
    pub fn has_role(&self, role_id: &str) -> bool {
        self.roles.iter().any(|r| r.id == role_id)
    }

    /// Check if user has permission through any role
    pub fn has_permission(&self, permission: Permission) -> bool {
        self.roles.iter().any(|r| r.has_permission(permission))
    }

    /// Check if user is the resource owner
    pub fn is_resource_owner(&self) -> bool {
        self.resource.is_owner(&self.user_id)
    }
}

/// Authorization request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuthorizationRequest {
    /// User ID making the request
    pub user_id: String,
    /// Type of resource being accessed
    pub resource_type: ResourceType,
    /// ID of resource being accessed
    pub resource_id: String,
    /// Action being performed
    pub action: Permission,
    /// Full authorization context
    pub context: AuthorizationContext,
}

impl AuthorizationRequest {
    /// Create a new authorization request
    pub fn new(
        user_id: String, resource_type: ResourceType, resource_id: String, action: Permission,
        context: AuthorizationContext,
    ) -> Self {
        Self {
            user_id,
            resource_type,
            resource_id,
            action,
            context,
        }
    }

    /// Check if request is for resource ownership
    pub fn is_owner_request(&self) -> bool {
        self.context.is_resource_owner()
    }

    /// Check if user has required permission through roles
    pub fn has_role_permission(&self) -> bool {
        self.context.has_permission(self.action)
    }
}

/// Authorization decision result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuthorizationDecision {
    /// Whether the action is allowed
    pub allowed: bool,
    /// Effect that was applied
    pub effect: super::policy::Effect,
    /// IDs of policies that were applied
    pub applied_policies: Vec<String>,
}

impl AuthorizationDecision {
    /// Create an allow decision
    pub fn allow() -> Self {
        Self {
            allowed: true,
            effect: super::policy::Effect::Allow,
            applied_policies: vec![],
        }
    }

    /// Create a deny decision
    pub fn deny() -> Self {
        Self {
            allowed: false,
            effect: super::policy::Effect::Deny,
            applied_policies: vec![],
        }
    }

    /// Check if allowed
    pub fn is_allowed(&self) -> bool {
        self.allowed
    }

    /// Check if denied
    pub fn is_denied(&self) -> bool {
        !self.allowed
    }

    /// Add applied policy ID
    pub fn with_policy(mut self, policy_id: String) -> Self {
        self.applied_policies.push(policy_id);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rbac::permission::Permissions;
    use crate::rbac::resource::{Resource, ResourceOwner};

    fn create_test_resource(owner_id: &str) -> Resource {
        Resource {
            id: "test_resource".to_string(),
            resource_type: ResourceType::Template,
            owner: ResourceOwner::new(owner_id),
            permissions: Permissions::empty(),
        }
    }

    #[test]
    fn test_authorization_context_new() {
        // Arrange
        let user_id = "user123";
        let roles = vec![Role::user()];
        let resource = create_test_resource("owner456");

        // Act
        let context = AuthorizationContext::new(user_id, &roles, &resource);

        // Assert
        assert_eq!(context.user_id, user_id);
        assert_eq!(context.roles.len(), 1);
        assert_eq!(context.resource.id, "test_resource");
    }

    #[test]
    fn test_authorization_context_with_attribute() {
        // Arrange
        let user_id = "user123";
        let roles = vec![Role::user()];
        let resource = create_test_resource("owner456");

        // Act
        let context = AuthorizationContext::new(user_id, &roles, &resource)
            .with_attribute("ip_address".to_string(), "192.168.1.1".to_string());

        // Assert
        assert_eq!(
            context.get_attribute("ip_address"),
            Some(&"192.168.1.1".to_string())
        );
    }

    #[test]
    fn test_authorization_context_has_role() {
        // Arrange
        let user_id = "user123";
        let roles = vec![Role::user()];
        let resource = create_test_resource("owner456");
        let context = AuthorizationContext::new(user_id, &roles, &resource);

        // Act & Assert
        assert!(context.has_role("role_user"));
        assert!(!context.has_role("role_admin"));
    }

    #[test]
    fn test_authorization_context_has_permission() {
        // Arrange
        let user_id = "user123";
        let roles = vec![Role::user()];
        let resource = create_test_resource("owner456");
        let context = AuthorizationContext::new(user_id, &roles, &resource);

        // Act & Assert
        assert!(context.has_permission(Permission::Read));
        assert!(context.has_permission(Permission::Write));
        assert!(!context.has_permission(Permission::Delete));
    }

    #[test]
    fn test_authorization_context_is_resource_owner() {
        // Arrange
        let user_id = "user123";
        let roles = vec![Role::user()];
        let resource = create_test_resource(user_id);
        let context = AuthorizationContext::new(user_id, &roles, &resource);

        // Act & Assert
        assert!(context.is_resource_owner());
    }

    #[test]
    fn test_authorization_context_not_resource_owner() {
        // Arrange
        let user_id = "user123";
        let roles = vec![Role::user()];
        let resource = create_test_resource("owner456");
        let context = AuthorizationContext::new(user_id, &roles, &resource);

        // Act & Assert
        assert!(!context.is_resource_owner());
    }

    #[test]
    fn test_authorization_request_new() {
        // Arrange
        let user_id = "user123";
        let roles = vec![Role::user()];
        let resource = create_test_resource("owner456");
        let context = AuthorizationContext::new(user_id, &roles, &resource);

        // Act
        let request = AuthorizationRequest::new(
            user_id.to_string(),
            ResourceType::Template,
            "test_resource".to_string(),
            Permission::Read,
            context,
        );

        // Assert
        assert_eq!(request.user_id, user_id);
        assert_eq!(request.resource_id, "test_resource");
        assert_eq!(request.action, Permission::Read);
    }

    #[test]
    fn test_authorization_request_is_owner_request() {
        // Arrange
        let user_id = "user123";
        let roles = vec![Role::user()];
        let resource = create_test_resource(user_id);
        let context = AuthorizationContext::new(user_id, &roles, &resource);
        let request = AuthorizationRequest::new(
            user_id.to_string(),
            ResourceType::Template,
            "test_resource".to_string(),
            Permission::Read,
            context,
        );

        // Act & Assert
        assert!(request.is_owner_request());
    }

    #[test]
    fn test_authorization_request_has_role_permission() {
        // Arrange
        let user_id = "user123";
        let roles = vec![Role::user()];
        let resource = create_test_resource("owner456");
        let context = AuthorizationContext::new(user_id, &roles, &resource);
        let request = AuthorizationRequest::new(
            user_id.to_string(),
            ResourceType::Template,
            "test_resource".to_string(),
            Permission::Read,
            context,
        );

        // Act & Assert
        assert!(request.has_role_permission());
    }

    #[test]
    fn test_authorization_decision_allow() {
        // Arrange & Act
        let decision = AuthorizationDecision::allow();

        // Assert
        assert!(decision.is_allowed());
        assert!(!decision.is_denied());
    }

    #[test]
    fn test_authorization_decision_deny() {
        // Arrange & Act
        let decision = AuthorizationDecision::deny();

        // Assert
        assert!(!decision.is_allowed());
        assert!(decision.is_denied());
    }

    #[test]
    fn test_authorization_decision_with_policy() {
        // Arrange
        let decision = AuthorizationDecision::allow();

        // Act
        let decision_with_policy = decision.with_policy("policy1".to_string());

        // Assert
        assert_eq!(decision_with_policy.applied_policies.len(), 1);
        assert_eq!(decision_with_policy.applied_policies[0], "policy1");
    }
}
