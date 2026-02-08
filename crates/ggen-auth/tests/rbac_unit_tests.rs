//! Comprehensive unit tests for RBAC system (Chicago TDD - AAA pattern)

use ggen_auth::rbac::*;
use ggen_auth::{Permission, Permissions, Resource, ResourceOwner, ResourceType, Role, RoleLevel};

// ============================================================================
// Permission Tests (10 tests)
// ============================================================================

#[test]
fn test_permission_to_bits_read() {
    // Arrange
    let permission = Permission::Read;

    // Act
    let bits = permission.to_bits();

    // Assert
    assert_eq!(bits, Permissions::READ);
}

#[test]
fn test_permission_to_bits_all_types() {
    // Arrange & Act & Assert
    assert_eq!(Permission::Read.to_bits(), Permissions::READ);
    assert_eq!(Permission::Write.to_bits(), Permissions::WRITE);
    assert_eq!(Permission::Delete.to_bits(), Permissions::DELETE);
    assert_eq!(Permission::Execute.to_bits(), Permissions::EXECUTE);
}

#[test]
fn test_permissions_has_permission_single() {
    // Arrange
    let perms = Permissions::READ;

    // Act & Assert
    assert!(perms.has_permission(Permission::Read));
    assert!(!perms.has_permission(Permission::Write));
}

#[test]
fn test_permissions_has_permission_multiple() {
    // Arrange
    let perms = Permissions::READ | Permissions::WRITE | Permissions::EXECUTE;

    // Act & Assert
    assert!(perms.has_permission(Permission::Read));
    assert!(perms.has_permission(Permission::Write));
    assert!(perms.has_permission(Permission::Execute));
    assert!(!perms.has_permission(Permission::Delete));
}

#[test]
fn test_permissions_add_permission() {
    // Arrange
    let mut perms = Permissions::READ;

    // Act
    perms.add_permission(Permission::Write);
    perms.add_permission(Permission::Execute);

    // Assert
    assert!(perms.has_permission(Permission::Read));
    assert!(perms.has_permission(Permission::Write));
    assert!(perms.has_permission(Permission::Execute));
    assert!(!perms.has_permission(Permission::Delete));
}

#[test]
fn test_permissions_remove_permission() {
    // Arrange
    let mut perms = Permissions::ALL;

    // Act
    perms.remove_permission(Permission::Delete);
    perms.remove_permission(Permission::Execute);

    // Assert
    assert!(perms.has_permission(Permission::Read));
    assert!(perms.has_permission(Permission::Write));
    assert!(!perms.has_permission(Permission::Delete));
    assert!(!perms.has_permission(Permission::Execute));
}

#[test]
fn test_permissions_from_permissions() {
    // Arrange
    let permission_list = vec![Permission::Read, Permission::Delete];

    // Act
    let perms = Permissions::from_permissions(&permission_list);

    // Assert
    assert!(perms.has_permission(Permission::Read));
    assert!(perms.has_permission(Permission::Delete));
    assert!(!perms.has_permission(Permission::Write));
    assert!(!perms.has_permission(Permission::Execute));
}

#[test]
fn test_permissions_to_vec() {
    // Arrange
    let perms = Permissions::READ | Permissions::WRITE;

    // Act
    let vec = perms.to_vec();

    // Assert
    assert_eq!(vec.len(), 2);
    assert!(vec.contains(&Permission::Read));
    assert!(vec.contains(&Permission::Write));
}

#[test]
fn test_permission_from_str_valid() {
    // Arrange & Act & Assert
    assert_eq!(Permission::from_str("read"), Some(Permission::Read));
    assert_eq!(Permission::from_str("WRITE"), Some(Permission::Write));
    assert_eq!(Permission::from_str("Delete"), Some(Permission::Delete));
    assert_eq!(Permission::from_str("EXECUTE"), Some(Permission::Execute));
}

#[test]
fn test_permission_from_str_invalid() {
    // Arrange & Act
    let result = Permission::from_str("invalid");

    // Assert
    assert_eq!(result, None);
}

// ============================================================================
// Role Tests (10 tests)
// ============================================================================

#[test]
fn test_role_level_hierarchy() {
    // Arrange
    let guest = RoleLevel::Guest;
    let user = RoleLevel::User;
    let manager = RoleLevel::Manager;
    let admin = RoleLevel::Admin;

    // Act & Assert
    assert!(admin.is_higher_or_equal(&manager));
    assert!(admin.is_higher_or_equal(&user));
    assert!(admin.is_higher_or_equal(&guest));
    assert!(manager.is_higher_or_equal(&user));
    assert!(manager.is_higher_or_equal(&guest));
    assert!(user.is_higher_or_equal(&guest));
    assert!(!guest.is_higher_or_equal(&user));
}

#[test]
fn test_role_guest_default_permissions() {
    // Arrange
    let role = Role::guest();

    // Act & Assert
    assert_eq!(role.level, RoleLevel::Guest);
    assert!(role.has_permission(Permission::Read));
    assert!(!role.has_permission(Permission::Write));
    assert!(!role.has_permission(Permission::Delete));
    assert!(!role.has_permission(Permission::Execute));
}

#[test]
fn test_role_user_default_permissions() {
    // Arrange
    let role = Role::user();

    // Act & Assert
    assert_eq!(role.level, RoleLevel::User);
    assert!(role.has_permission(Permission::Read));
    assert!(role.has_permission(Permission::Write));
    assert!(!role.has_permission(Permission::Delete));
    assert!(role.has_permission(Permission::Execute));
}

#[test]
fn test_role_manager_default_permissions() {
    // Arrange
    let role = Role::manager();

    // Act & Assert
    assert_eq!(role.level, RoleLevel::Manager);
    assert!(role.has_permission(Permission::Read));
    assert!(role.has_permission(Permission::Write));
    assert!(role.has_permission(Permission::Delete));
    assert!(role.has_permission(Permission::Execute));
}

#[test]
fn test_role_admin_default_permissions() {
    // Arrange
    let role = Role::admin();

    // Act & Assert
    assert_eq!(role.level, RoleLevel::Admin);
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
    role.add_permission(Permission::Execute);

    // Assert
    assert!(role.has_permission(Permission::Read));
    assert!(role.has_permission(Permission::Write));
    assert!(role.has_permission(Permission::Execute));
}

#[test]
fn test_role_remove_permission() {
    // Arrange
    let mut role = Role::admin();

    // Act
    role.remove_permission(Permission::Delete);

    // Assert
    assert!(role.has_permission(Permission::Read));
    assert!(role.has_permission(Permission::Write));
    assert!(role.has_permission(Permission::Execute));
    assert!(!role.has_permission(Permission::Delete));
}

#[test]
fn test_role_hierarchy_comparison() {
    // Arrange
    let admin = Role::admin();
    let manager = Role::manager();
    let user = Role::user();
    let guest = Role::guest();

    // Act & Assert
    assert!(admin.is_higher_or_equal(&manager));
    assert!(admin.is_higher_or_equal(&user));
    assert!(admin.is_higher_or_equal(&guest));
    assert!(!guest.is_higher_or_equal(&user));
}

#[test]
fn test_role_hierarchy_get_role() {
    // Arrange
    let hierarchy = RoleHierarchy::new();

    // Act
    let admin_role = hierarchy.get_role("role_admin");
    let user_role = hierarchy.get_role("role_user");
    let invalid_role = hierarchy.get_role("role_invalid");

    // Assert
    assert!(admin_role.is_some());
    assert_eq!(admin_role.unwrap().name, "Admin");
    assert!(user_role.is_some());
    assert_eq!(user_role.unwrap().name, "User");
    assert!(invalid_role.is_none());
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
    assert!(!perms.has_permission(Permission::Delete));
}

// ============================================================================
// Resource Tests (5 tests)
// ============================================================================

#[test]
fn test_resource_owner_is_owner() {
    // Arrange
    let owner = ResourceOwner::new("user123");

    // Act
    let is_owner = owner.is_owner("user123");
    let is_not_owner = owner.is_owner("user456");

    // Assert
    assert!(is_owner);
    assert!(!is_not_owner);
}

#[test]
fn test_resource_owner_with_org() {
    // Arrange
    let owner = ResourceOwner::with_org("user123", "org456");

    // Act & Assert
    assert_eq!(owner.user_id, "user123");
    assert_eq!(owner.org_id, Some("org456".to_string()));
    assert!(owner.is_org_member("org456"));
    assert!(!owner.is_org_member("org789"));
}

#[test]
fn test_resource_is_owner() {
    // Arrange
    let owner = ResourceOwner::new("user123");
    let resource = Resource {
        id: "resource1".to_string(),
        resource_type: ResourceType::Template,
        owner,
        permissions: Permissions::empty(),
    };

    // Act & Assert
    assert!(resource.is_owner("user123"));
    assert!(!resource.is_owner("user456"));
}

#[test]
fn test_resource_type_from_str() {
    // Arrange & Act & Assert
    assert_eq!(ResourceType::from_str("template"), ResourceType::Template);
    assert_eq!(ResourceType::from_str("PROJECT"), ResourceType::Project);
    assert_eq!(ResourceType::from_str("package"), ResourceType::Package);
    assert_eq!(
        ResourceType::from_str("custom"),
        ResourceType::Custom("custom".to_string())
    );
}

#[test]
fn test_resource_acl_expiration() {
    // Arrange
    let past_date = chrono::Utc::now() - chrono::Duration::days(1);
    let future_date = chrono::Utc::now() + chrono::Duration::days(1);

    let expired_acl = resource::ResourceAcl::new(
        "resource1".to_string(),
        "user123".to_string(),
        resource::PrincipalType::User,
        Permissions::READ,
    )
    .with_expiration(past_date);

    let valid_acl = resource::ResourceAcl::new(
        "resource1".to_string(),
        "user123".to_string(),
        resource::PrincipalType::User,
        Permissions::READ,
    )
    .with_expiration(future_date);

    // Act & Assert
    assert!(expired_acl.is_expired());
    assert!(!valid_acl.is_expired());
}

// ============================================================================
// Policy Engine Tests (5 tests)
// ============================================================================

#[test]
fn test_policy_engine_permissive_default() {
    // Arrange
    let engine = policy::PolicyEngine::new();
    let context = context::AuthorizationContext::new(
        "user123",
        &[Role::user()],
        &create_test_resource("owner456"),
    );
    let request = context::AuthorizationRequest::new(
        "user123".to_string(),
        ResourceType::Template,
        "resource1".to_string(),
        Permission::Read,
        context,
    );

    // Act
    let result = engine.evaluate(&request);

    // Assert
    assert!(result.is_ok());
    assert!(result.unwrap().is_allowed());
}

#[test]
fn test_policy_engine_restrictive_default() {
    // Arrange
    let engine = policy::PolicyEngine::restrictive();
    let context = context::AuthorizationContext::new(
        "user123",
        &[Role::user()],
        &create_test_resource("owner456"),
    );
    let request = context::AuthorizationRequest::new(
        "user123".to_string(),
        ResourceType::Template,
        "resource1".to_string(),
        Permission::Read,
        context,
    );

    // Act
    let result = engine.evaluate(&request);

    // Assert
    assert!(result.is_ok());
    assert!(result.unwrap().is_denied());
}

#[test]
fn test_policy_engine_deny_precedence() {
    // Arrange
    let mut engine = policy::PolicyEngine::new();

    let allow_policy = policy::Policy::new(
        "allow".to_string(),
        "Allow Policy".to_string(),
        "Allow".to_string(),
        vec![policy::PolicyRule::allow_all()],
    );

    let deny_policy = policy::Policy::new(
        "deny".to_string(),
        "Deny Policy".to_string(),
        "Deny".to_string(),
        vec![policy::PolicyRule::deny_all()],
    );

    engine.add_policy(allow_policy);
    engine.add_policy(deny_policy);

    let context = context::AuthorizationContext::new(
        "user123",
        &[Role::user()],
        &create_test_resource("owner456"),
    );
    let request = context::AuthorizationRequest::new(
        "user123".to_string(),
        ResourceType::Template,
        "resource1".to_string(),
        Permission::Read,
        context,
    );

    // Act
    let result = engine.evaluate(&request);

    // Assert - Deny should take precedence
    assert!(result.is_ok());
    assert!(result.unwrap().is_denied());
}

#[test]
fn test_policy_condition_and() {
    // Arrange
    let condition = policy::Condition::And(vec![
        policy::Condition::UserIdEquals("user123".to_string()),
        policy::Condition::ResourceTypeEquals(ResourceType::Template),
    ]);

    let context = context::AuthorizationContext::new(
        "user123",
        &[Role::user()],
        &create_test_resource("owner456"),
    );
    let request = context::AuthorizationRequest::new(
        "user123".to_string(),
        ResourceType::Template,
        "resource1".to_string(),
        Permission::Read,
        context,
    );

    // Act
    let result = condition.evaluate(&request);

    // Assert
    assert!(result);
}

#[test]
fn test_policy_condition_not() {
    // Arrange
    let condition = policy::Condition::Not(Box::new(policy::Condition::UserIdEquals(
        "user456".to_string(),
    )));

    let context = context::AuthorizationContext::new(
        "user123",
        &[Role::user()],
        &create_test_resource("owner456"),
    );
    let request = context::AuthorizationRequest::new(
        "user123".to_string(),
        ResourceType::Template,
        "resource1".to_string(),
        Permission::Read,
        context,
    );

    // Act
    let result = condition.evaluate(&request);

    // Assert
    assert!(result);
}

// ============================================================================
// Authorization Context Tests (3 tests)
// ============================================================================

#[test]
fn test_authorization_context_has_role() {
    // Arrange
    let context = context::AuthorizationContext::new(
        "user123",
        &[Role::user(), Role::manager()],
        &create_test_resource("owner456"),
    );

    // Act & Assert
    assert!(context.has_role("role_user"));
    assert!(context.has_role("role_manager"));
    assert!(!context.has_role("role_admin"));
}

#[test]
fn test_authorization_context_has_permission() {
    // Arrange
    let context = context::AuthorizationContext::new(
        "user123",
        &[Role::user()],
        &create_test_resource("owner456"),
    );

    // Act & Assert
    assert!(context.has_permission(Permission::Read));
    assert!(context.has_permission(Permission::Write));
    assert!(!context.has_permission(Permission::Delete));
}

#[test]
fn test_authorization_context_is_resource_owner() {
    // Arrange
    let owner_context = context::AuthorizationContext::new(
        "user123",
        &[Role::user()],
        &create_test_resource("user123"),
    );

    let non_owner_context = context::AuthorizationContext::new(
        "user123",
        &[Role::user()],
        &create_test_resource("owner456"),
    );

    // Act & Assert
    assert!(owner_context.is_resource_owner());
    assert!(!non_owner_context.is_resource_owner());
}

// Helper function
fn create_test_resource(owner_id: &str) -> Resource {
    Resource {
        id: "test_resource".to_string(),
        resource_type: ResourceType::Template,
        owner: ResourceOwner::new(owner_id),
        permissions: Permissions::empty(),
    }
}
