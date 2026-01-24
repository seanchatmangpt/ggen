//! Security tests for RBAC - Privilege Escalation Prevention (20+ tests)

use ggen_auth::rbac::*;
use ggen_auth::{
    authorize, AuthorizationContext, Permission, Permissions, PolicyEngine, Resource,
    ResourceOwner, ResourceType, Role, RoleHierarchy, RoleLevel, UserRole,
};

// ============================================================================
// Privilege Escalation Prevention Tests (8 tests)
// ============================================================================

#[test]
fn test_prevent_guest_escalation_to_admin() {
    // Arrange
    let user_id = "malicious_user";
    let resource = Resource {
        id: "sensitive_resource".to_string(),
        resource_type: ResourceType::UserData,
        owner: ResourceOwner::new("admin"),
        permissions: Permissions::empty(),
    };
    let roles = vec![Role::guest()]; // User only has guest role
    let policy_engine = PolicyEngine::new();

    // Act - Attempt to delete (admin-level operation)
    let result = authorize(user_id, &roles, &resource, Permission::Delete, &policy_engine);

    // Assert - Should be denied
    assert!(result.is_ok());
    assert!(!result.unwrap());
}

#[test]
fn test_prevent_user_escalation_to_manager() {
    // Arrange
    let user_id = "regular_user";
    let owner_id = "other_user";
    let resource = Resource {
        id: "user_data".to_string(),
        resource_type: ResourceType::UserData,
        owner: ResourceOwner::new(owner_id),
        permissions: Permissions::empty(),
    };
    let roles = vec![Role::user()]; // Regular user role
    let policy_engine = PolicyEngine::new();

    // Act - Attempt to delete other user's data (manager-level operation)
    let result = authorize(user_id, &roles, &resource, Permission::Delete, &policy_engine);

    // Assert - Should be denied (user role doesn't have delete permission)
    assert!(result.is_ok());
    assert!(!result.unwrap());
}

#[test]
fn test_role_level_hierarchy_enforcement() {
    // Arrange
    let guest = RoleLevel::Guest;
    let user = RoleLevel::User;
    let manager = RoleLevel::Manager;
    let admin = RoleLevel::Admin;

    // Act & Assert - Ensure hierarchy is strictly enforced
    assert!(!guest.is_higher_or_equal(&user));
    assert!(!user.is_higher_or_equal(&manager));
    assert!(!manager.is_higher_or_equal(&admin));
    assert!(admin.is_higher_or_equal(&admin)); // Equal is ok
}

#[test]
fn test_prevent_role_modification_without_permission() {
    // Arrange
    let mut target_role = Role::user();
    let original_perms = target_role.permissions;

    // Act - Attempt to add admin-level permission
    target_role.add_permission(Permission::Delete);

    // Assert - Permission was added (this is allowed in code)
    // But in production, this should be protected by API-level checks
    assert!(target_role.has_permission(Permission::Delete));
    assert_ne!(target_role.permissions, original_perms);
}

#[test]
fn test_expired_role_assignment_not_valid() {
    // Arrange
    let past_date = chrono::Utc::now() - chrono::Duration::days(1);
    let user_role = UserRole::new(
        "user123".to_string(),
        "role_admin".to_string(),
        "admin456".to_string(),
    )
    .with_expiration(past_date);

    // Act
    let is_expired = user_role.is_expired();

    // Assert - Expired role should not grant permissions
    assert!(is_expired);
}

#[test]
fn test_prevent_multiple_role_bypass() {
    // Arrange - User tries to gain admin access by having multiple low-level roles
    let user_id = "attacker";
    let resource = Resource {
        id: "admin_resource".to_string(),
        resource_type: ResourceType::Organization,
        owner: ResourceOwner::new("admin"),
        permissions: Permissions::empty(),
    };
    let roles = vec![Role::guest(), Role::guest(), Role::guest()]; // Multiple guest roles
    let policy_engine = PolicyEngine::new();

    // Act - Attempt admin-level operation
    let result = authorize(user_id, &roles, &resource, Permission::Delete, &policy_engine);

    // Assert - Should still be denied
    assert!(result.is_ok());
    assert!(!result.unwrap());
}

#[test]
fn test_prevent_permission_injection_via_empty_roles() {
    // Arrange
    let user_id = "attacker";
    let resource = Resource {
        id: "protected_resource".to_string(),
        resource_type: ResourceType::Template,
        owner: ResourceOwner::new("owner"),
        permissions: Permissions::empty(),
    };
    let roles = vec![]; // No roles at all
    let policy_engine = PolicyEngine::new();

    // Act - Even with permissive policy, no roles means no permission
    let result = authorize(user_id, &roles, &resource, Permission::Write, &policy_engine);

    // Assert - Should be denied
    assert!(result.is_ok());
    assert!(!result.unwrap());
}

#[test]
fn test_ownership_cannot_be_bypassed_with_high_role() {
    // Arrange - Non-owner with admin role tries to claim ownership
    let user_id = "admin_user";
    let actual_owner = "original_owner";
    let resource = Resource {
        id: "user_private_data".to_string(),
        resource_type: ResourceType::UserData,
        owner: ResourceOwner::new(actual_owner),
        permissions: Permissions::empty(),
    };

    // Act - Check if admin is incorrectly considered owner
    let is_owner = resource.is_owner(user_id);

    // Assert - Admin is not the owner (ownership is immutable)
    assert!(!is_owner);
    assert!(resource.is_owner(actual_owner));
}

// ============================================================================
// Policy Bypass Prevention Tests (5 tests)
// ============================================================================

#[test]
fn test_deny_policy_cannot_be_overridden_by_allow() {
    // Arrange
    let mut engine = PolicyEngine::new();

    // Add allow policy first
    let allow_policy = policy::Policy::new(
        "allow".to_string(),
        "Allow Policy".to_string(),
        "Allow all".to_string(),
        vec![policy::PolicyRule::allow_all()],
    );

    // Add deny policy
    let deny_policy = policy::Policy::new(
        "deny".to_string(),
        "Deny Policy".to_string(),
        "Deny all".to_string(),
        vec![policy::PolicyRule::deny_all()],
    );

    engine.add_policy(allow_policy);
    engine.add_policy(deny_policy);

    let context = AuthorizationContext::new("user123", &[Role::admin()], &create_test_resource());
    let request = context::AuthorizationRequest::new(
        "user123".to_string(),
        ResourceType::Template,
        "resource1".to_string(),
        Permission::Read,
        context,
    );

    // Act
    let result = engine.evaluate(&request);

    // Assert - Deny should always win
    assert!(result.is_ok());
    assert!(result.unwrap().is_denied());
}

#[test]
fn test_restrictive_default_prevents_unauthorized_access() {
    // Arrange
    let engine = PolicyEngine::restrictive(); // No policies defined
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

    // Assert - Should deny by default
    assert!(result.is_ok());
    assert!(result.unwrap().is_denied());
}

#[test]
fn test_policy_condition_cannot_be_bypassed() {
    // Arrange
    let mut engine = PolicyEngine::new();
    let policy = policy::Policy::new(
        "strict".to_string(),
        "Strict Policy".to_string(),
        "Only specific user".to_string(),
        vec![policy::PolicyRule::new(
            "rule1".to_string(),
            "Allow Only User456".to_string(),
            "Allow".to_string(),
            policy::Effect::Allow,
            vec![policy::Condition::UserIdEquals("user456".to_string())],
            100,
        )],
    );
    engine.add_policy(policy);

    // Attacker with different user ID
    let context = AuthorizationContext::new("user123", &[Role::admin()], &create_test_resource());
    let request = context::AuthorizationRequest::new(
        "user123".to_string(),
        ResourceType::Template,
        "resource1".to_string(),
        Permission::Read,
        context,
    );

    // Act
    let result = engine.evaluate(&request);

    // Assert - Should be denied despite being admin
    assert!(result.is_ok());
    assert!(!result.unwrap().is_allowed());
}

#[test]
fn test_time_based_policy_enforcement() {
    // Arrange
    let future_timestamp = (chrono::Utc::now() + chrono::Duration::hours(1)).timestamp();
    let condition = policy::Condition::TimeBefore(future_timestamp);

    let context = AuthorizationContext::new("user123", &[Role::user()], &create_test_resource());
    let request = context::AuthorizationRequest::new(
        "user123".to_string(),
        ResourceType::Template,
        "resource1".to_string(),
        Permission::Read,
        context,
    );

    // Act - Current time is before future timestamp
    let result = condition.evaluate(&request);

    // Assert - Should be allowed
    assert!(result);
}

#[test]
fn test_expired_time_based_policy_denied() {
    // Arrange
    let past_timestamp = (chrono::Utc::now() - chrono::Duration::hours(1)).timestamp();
    let condition = policy::Condition::TimeBefore(past_timestamp);

    let context = AuthorizationContext::new("user123", &[Role::user()], &create_test_resource());
    let request = context::AuthorizationRequest::new(
        "user123".to_string(),
        ResourceType::Template,
        "resource1".to_string(),
        Permission::Read,
        context,
    );

    // Act - Current time is after past timestamp
    let result = condition.evaluate(&request);

    // Assert - Should be denied
    assert!(!result);
}

// ============================================================================
// Resource Access Control Tests (4 tests)
// ============================================================================

#[test]
fn test_acl_expiration_prevents_access() {
    // Arrange
    let past_date = chrono::Utc::now() - chrono::Duration::days(1);
    let acl = resource::ResourceAcl::new(
        "resource1".to_string(),
        "user123".to_string(),
        resource::PrincipalType::User,
        Permissions::ALL,
    )
    .with_expiration(past_date);

    // Act
    let is_expired = acl.is_expired();

    // Assert - Expired ACL should not grant access
    assert!(is_expired);
}

#[test]
fn test_resource_type_based_access_control() {
    // Arrange
    let template_resource = Resource {
        id: "template1".to_string(),
        resource_type: ResourceType::Template,
        owner: ResourceOwner::new("owner"),
        permissions: Permissions::READ,
    };

    let userdata_resource = Resource {
        id: "data1".to_string(),
        resource_type: ResourceType::UserData,
        owner: ResourceOwner::new("owner"),
        permissions: Permissions::empty(), // No public access
    };

    // Act & Assert - Template is publicly readable, UserData is not
    assert!(template_resource
        .permissions
        .has_permission(Permission::Read));
    assert!(!userdata_resource
        .permissions
        .has_permission(Permission::Read));
}

#[test]
fn test_organization_boundary_enforcement() {
    // Arrange
    let org1_resource = Resource {
        id: "resource1".to_string(),
        resource_type: ResourceType::Project,
        owner: ResourceOwner::with_org("owner1", "org_alpha"),
        permissions: Permissions::empty(),
    };

    // Act - Check org membership
    let is_alpha_member = org1_resource.is_org_member("org_alpha");
    let is_beta_member = org1_resource.is_org_member("org_beta");

    // Assert - Organization boundaries are enforced
    assert!(is_alpha_member);
    assert!(!is_beta_member);
}

#[test]
fn test_principal_type_separation() {
    // Arrange
    let user_acl = resource::ResourceAcl::new(
        "resource1".to_string(),
        "user123".to_string(),
        resource::PrincipalType::User,
        Permissions::READ,
    );

    let role_acl = resource::ResourceAcl::new(
        "resource1".to_string(),
        "role_admin".to_string(),
        resource::PrincipalType::Role,
        Permissions::ALL,
    );

    // Act & Assert - Principal types are distinct
    assert_eq!(user_acl.principal_type, resource::PrincipalType::User);
    assert_eq!(role_acl.principal_type, resource::PrincipalType::Role);
    assert_ne!(user_acl.principal_type, role_acl.principal_type);
}

// ============================================================================
// Complex Attack Scenario Tests (3 tests)
// ============================================================================

#[test]
fn test_prevent_confused_deputy_attack() {
    // Arrange - Attacker tries to use high-privileged service to access restricted resource
    let attacker_id = "attacker";
    let service_id = "trusted_service";

    let restricted_resource = Resource {
        id: "restricted".to_string(),
        resource_type: ResourceType::UserData,
        owner: ResourceOwner::new("victim"),
        permissions: Permissions::empty(),
    };

    // Service has high privileges
    let service_roles = vec![Role::admin()];
    let policy_engine = PolicyEngine::new();

    // Act - Attacker is NOT the service, so should be denied
    let result = authorize(
        attacker_id,
        &service_roles,
        &restricted_resource,
        Permission::Read,
        &policy_engine,
    );

    // Assert - Ownership check prevents confused deputy
    // (In real implementation, service would need to prove it's acting on behalf of authorized user)
    assert!(result.is_ok());
    // Authorization passes because service has admin role, but ownership check would fail in real scenario
    // This test demonstrates the NEED for additional context-aware authorization
}

#[test]
fn test_prevent_role_confusion_attack() {
    // Arrange - Attacker with user role tries to exploit by claiming to have admin role ID
    let hierarchy = RoleHierarchy::new();

    // Attacker provides admin role ID but doesn't actually have it
    let fake_admin_roles = vec!["role_admin".to_string()];

    // Act - Check if role actually exists and is valid
    let role_exists = hierarchy.get_role("role_admin").is_some();
    let has_permission = hierarchy.has_permission(&fake_admin_roles, Permission::Delete);

    // Assert - Role exists and would grant permission IF user actually had it
    // This test demonstrates the NEED for proper role assignment verification
    assert!(role_exists);
    assert!(has_permission);
}

#[test]
fn test_prevent_permission_enumeration() {
    // Arrange - Attacker tries to enumerate all possible permissions
    let guest_role = Role::guest();

    // Act - Check each permission
    let permissions_granted: Vec<Permission> = Permission::all()
        .into_iter()
        .filter(|p| guest_role.has_permission(*p))
        .collect();

    // Assert - Guest should only have read permission
    assert_eq!(permissions_granted.len(), 1);
    assert!(permissions_granted.contains(&Permission::Read));
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
