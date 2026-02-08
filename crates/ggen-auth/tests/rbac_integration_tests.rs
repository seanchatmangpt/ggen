//! Integration tests for RBAC system (15+ tests)

use ggen_auth::rbac::*;
use ggen_auth::{
    authorize, AuthorizationContext, Permission, Permissions, PolicyEngine, Resource,
    ResourceOwner, ResourceType, Role, RoleHierarchy,
};

// ============================================================================
// Full Authorization Flow Tests (5 tests)
// ============================================================================

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
    let result = authorize(
        user_id,
        &roles,
        &resource,
        Permission::Delete,
        &policy_engine,
    );

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
    let result = authorize(user_id, &roles, &resource, Permission::Read, &policy_engine);

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
        Permission::Delete,
        &policy_engine,
    );

    // Assert
    assert!(result.is_ok());
    assert!(!result.unwrap());
}

#[test]
fn test_authorize_admin_full_access() {
    // Arrange
    let user_id = "admin123";
    let owner_id = "owner456";
    let resource = Resource {
        id: "resource1".to_string(),
        resource_type: ResourceType::Template,
        owner: ResourceOwner::new(owner_id),
        permissions: Permissions::empty(),
    };
    let roles = vec![Role::admin()];
    let policy_engine = PolicyEngine::new();

    // Act
    let read_result = authorize(user_id, &roles, &resource, Permission::Read, &policy_engine);
    let write_result = authorize(
        user_id,
        &roles,
        &resource,
        Permission::Write,
        &policy_engine,
    );
    let delete_result = authorize(
        user_id,
        &roles,
        &resource,
        Permission::Delete,
        &policy_engine,
    );
    let execute_result = authorize(
        user_id,
        &roles,
        &resource,
        Permission::Execute,
        &policy_engine,
    );

    // Assert
    assert!(read_result.is_ok() && read_result.unwrap());
    assert!(write_result.is_ok() && write_result.unwrap());
    assert!(delete_result.is_ok() && delete_result.unwrap());
    assert!(execute_result.is_ok() && execute_result.unwrap());
}

#[test]
fn test_authorize_manager_elevated_access() {
    // Arrange
    let user_id = "manager123";
    let owner_id = "owner456";
    let resource = Resource {
        id: "resource1".to_string(),
        resource_type: ResourceType::Template,
        owner: ResourceOwner::new(owner_id),
        permissions: Permissions::empty(),
    };
    let roles = vec![Role::manager()];
    let policy_engine = PolicyEngine::new();

    // Act
    let read_result = authorize(user_id, &roles, &resource, Permission::Read, &policy_engine);
    let delete_result = authorize(
        user_id,
        &roles,
        &resource,
        Permission::Delete,
        &policy_engine,
    );

    // Assert
    assert!(read_result.is_ok() && read_result.unwrap());
    assert!(delete_result.is_ok() && delete_result.unwrap());
}

// ============================================================================
// Multi-Role Authorization Tests (3 tests)
// ============================================================================

#[test]
fn test_authorize_multiple_roles_combined_permissions() {
    // Arrange
    let user_id = "user123";
    let owner_id = "owner456";
    let resource = Resource {
        id: "resource1".to_string(),
        resource_type: ResourceType::Template,
        owner: ResourceOwner::new(owner_id),
        permissions: Permissions::empty(),
    };
    let roles = vec![Role::guest(), Role::user()];
    let policy_engine = PolicyEngine::new();

    // Act
    let read_result = authorize(user_id, &roles, &resource, Permission::Read, &policy_engine);
    let write_result = authorize(
        user_id,
        &roles,
        &resource,
        Permission::Write,
        &policy_engine,
    );

    // Assert - User role should grant write permission
    assert!(read_result.is_ok() && read_result.unwrap());
    assert!(write_result.is_ok() && write_result.unwrap());
}

#[test]
fn test_role_hierarchy_permission_inheritance() {
    // Arrange
    let hierarchy = RoleHierarchy::new();
    let admin_roles = vec!["role_admin".to_string()];
    let user_roles = vec!["role_user".to_string()];

    // Act
    let admin_has_delete = hierarchy.has_permission(&admin_roles, Permission::Delete);
    let user_has_delete = hierarchy.has_permission(&user_roles, Permission::Delete);

    // Assert
    assert!(admin_has_delete);
    assert!(!user_has_delete);
}

#[test]
fn test_role_hierarchy_combined_from_multiple() {
    // Arrange
    let hierarchy = RoleHierarchy::new();
    let roles = vec!["role_guest".to_string(), "role_manager".to_string()];

    // Act
    let combined = hierarchy.combined_permissions(&roles);

    // Assert - Should have all permissions from manager role
    assert!(combined.has_permission(Permission::Read));
    assert!(combined.has_permission(Permission::Write));
    assert!(combined.has_permission(Permission::Delete));
    assert!(combined.has_permission(Permission::Execute));
}

// ============================================================================
// Policy-Based Authorization Tests (4 tests)
// ============================================================================

#[test]
fn test_policy_based_authorization_with_condition() {
    // Arrange
    let user_id = "user123";
    let owner_id = "owner456";
    let resource = Resource {
        id: "resource1".to_string(),
        resource_type: ResourceType::Template,
        owner: ResourceOwner::new(owner_id),
        permissions: Permissions::empty(),
    };

    let mut policy_engine = PolicyEngine::new();
    let policy = policy::Policy::new(
        "template_read".to_string(),
        "Template Read Policy".to_string(),
        "Allow read for templates".to_string(),
        vec![policy::PolicyRule::new(
            "rule1".to_string(),
            "Allow Template Read".to_string(),
            "Allow read".to_string(),
            policy::Effect::Allow,
            vec![
                policy::Condition::ResourceTypeEquals(ResourceType::Template),
                policy::Condition::PermissionEquals(Permission::Read),
            ],
            100,
        )],
    );
    policy_engine.add_policy(policy);

    let roles = vec![Role::user()];

    // Act
    let result = authorize(user_id, &roles, &resource, Permission::Read, &policy_engine);

    // Assert
    assert!(result.is_ok());
    assert!(result.unwrap());
}

#[test]
fn test_policy_based_authorization_deny_rule() {
    // Arrange
    let user_id = "user123";
    let owner_id = "owner456";
    let resource = Resource {
        id: "resource1".to_string(),
        resource_type: ResourceType::Template,
        owner: ResourceOwner::new(owner_id),
        permissions: Permissions::empty(),
    };

    let mut policy_engine = PolicyEngine::restrictive();
    let deny_policy = policy::Policy::new(
        "deny_delete".to_string(),
        "Deny Delete Policy".to_string(),
        "Deny delete operations".to_string(),
        vec![policy::PolicyRule::new(
            "rule1".to_string(),
            "Deny Delete".to_string(),
            "Deny delete".to_string(),
            policy::Effect::Deny,
            vec![policy::Condition::PermissionEquals(Permission::Delete)],
            100,
        )],
    );
    policy_engine.add_policy(deny_policy);

    let roles = vec![Role::admin()];

    // Act
    let result = authorize(
        user_id,
        &roles,
        &resource,
        Permission::Delete,
        &policy_engine,
    );

    // Assert - Even admin should be denied by explicit deny policy
    assert!(result.is_ok());
    assert!(!result.unwrap());
}

#[test]
fn test_policy_with_user_attribute_condition() {
    // Arrange
    let context = AuthorizationContext::new("user123", &[Role::user()], &create_test_resource())
        .with_attribute("department".to_string(), "engineering".to_string());

    let request = context::AuthorizationRequest::new(
        "user123".to_string(),
        ResourceType::Template,
        "resource1".to_string(),
        Permission::Read,
        context,
    );

    let condition =
        policy::Condition::AttributeEquals("department".to_string(), "engineering".to_string());

    // Act
    let result = condition.evaluate(&request);

    // Assert
    assert!(result);
}

#[test]
fn test_policy_priority_ordering() {
    // Arrange
    let mut engine = PolicyEngine::new();

    // Low priority allow rule
    let low_priority_policy = policy::Policy::new(
        "low".to_string(),
        "Low Priority".to_string(),
        "Low".to_string(),
        vec![policy::PolicyRule::new(
            "rule1".to_string(),
            "Low Allow".to_string(),
            "Allow".to_string(),
            policy::Effect::Allow,
            vec![],
            10,
        )],
    );

    // High priority deny rule
    let high_priority_policy = policy::Policy::new(
        "high".to_string(),
        "High Priority".to_string(),
        "High".to_string(),
        vec![policy::PolicyRule::new(
            "rule2".to_string(),
            "High Deny".to_string(),
            "Deny".to_string(),
            policy::Effect::Deny,
            vec![],
            100,
        )],
    );

    engine.add_policy(low_priority_policy);
    engine.add_policy(high_priority_policy);

    let context = AuthorizationContext::new("user123", &[Role::user()], &create_test_resource());
    let request = context::AuthorizationRequest::new(
        "user123".to_string(),
        ResourceType::Template,
        "resource1".to_string(),
        Permission::Read,
        context,
    );

    // Act
    let result = engine.evaluate(&request);

    // Assert - High priority deny should win
    assert!(result.is_ok());
    assert!(result.unwrap().is_denied());
}

// ============================================================================
// Resource Ownership Tests (3 tests)
// ============================================================================

#[test]
fn test_resource_owner_bypass_permission_check() {
    // Arrange
    let user_id = "owner123";
    let resource = Resource {
        id: "resource1".to_string(),
        resource_type: ResourceType::Template,
        owner: ResourceOwner::new(user_id),
        permissions: Permissions::empty(),
    };
    let roles = vec![]; // No roles
    let policy_engine = PolicyEngine::restrictive(); // Restrictive policy

    // Act - Owner should bypass all checks
    let result = authorize(
        user_id,
        &roles,
        &resource,
        Permission::Delete,
        &policy_engine,
    );

    // Assert
    assert!(result.is_ok());
    assert!(result.unwrap());
}

#[test]
fn test_organization_membership_authorization() {
    // Arrange
    let user_id = "user123";
    let owner = ResourceOwner::with_org("owner456", "org789");
    let resource = Resource {
        id: "resource1".to_string(),
        resource_type: ResourceType::Project,
        owner,
        permissions: Permissions::READ | Permissions::WRITE,
    };

    // Act
    let is_org_member = resource.is_org_member("org789");
    let is_not_member = resource.is_org_member("org111");

    // Assert
    assert!(is_org_member);
    assert!(!is_not_member);
}

#[test]
fn test_resource_public_permissions() {
    // Arrange
    let resource = Resource {
        id: "resource1".to_string(),
        resource_type: ResourceType::Template,
        owner: ResourceOwner::new("owner123"),
        permissions: Permissions::READ | Permissions::EXECUTE,
    };

    // Act & Assert
    assert!(resource.permissions.has_permission(Permission::Read));
    assert!(resource.permissions.has_permission(Permission::Execute));
    assert!(!resource.permissions.has_permission(Permission::Write));
    assert!(!resource.permissions.has_permission(Permission::Delete));
}

// Helper function
fn create_test_resource() -> Resource {
    Resource {
        id: "test_resource".to_string(),
        resource_type: ResourceType::Template,
        owner: ResourceOwner::new("owner123"),
        permissions: Permissions::empty(),
    }
}
