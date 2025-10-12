//! Unit tests for policy enforcement behaviors
//!
//! Tests policy validation and enforcement logic to ensure security
//! constraints are properly applied and validated.

use rstest::*;
use cleanroom::policy::{Policy, NetProfile, FsProfile, ProcProfile, ResourceLimits};
use cleanroom::runtime::{NetController, FsController, ProcController};
use std::path::PathBuf;

#[test]
fn locked_policy_enforces_maximum_security_constraints() {
    // Given: A locked-down policy
    let policy = Policy::locked();

    // When: The policy is applied
    // Then: Maximum security constraints are enforced
    assert!(matches!(policy.net, NetProfile::Offline));
    assert!(matches!(policy.fs, FsProfile::ReadOnly { workdir: true }));
    assert!(matches!(policy.proc, ProcProfile::Isolated));
    
    // Resource limits should be strict
    assert!(policy.limits.cpu_time_secs.unwrap() <= 300);
    assert!(policy.limits.memory_bytes.unwrap() <= 512 * 1024 * 1024);
    assert!(policy.limits.process_count.unwrap() <= 1);
}

#[test]
fn permissive_policy_allows_necessary_operations() {
    // Given: A permissive policy
    let policy = Policy::permissive();

    // When: The policy is applied
    // Then: Necessary operations are allowed
    assert!(matches!(policy.net, NetProfile::Open));
    assert!(matches!(policy.fs, FsProfile::Writable { workdir: true }));
    assert!(matches!(policy.proc, ProcProfile::Standard));
    
    // Resource limits should be more generous
    assert!(policy.limits.cpu_time_secs.unwrap() >= 60);
    assert!(policy.limits.memory_bytes.unwrap() >= 256 * 1024 * 1024);
    assert!(policy.limits.process_count.unwrap() >= 4);
}

#[rstest]
#[case(NetProfile::Offline, false)]
#[case(NetProfile::Limited { allowed_ports: vec![80, 443] }, true)]
#[case(NetProfile::Open, true)]
fn network_profile_blocks_or_allows_connections_appropriately(
    #[case] profile: NetProfile,
    #[case] expected_allowed: bool,
) {
    // Given: A network controller with the specified profile
    let controller = NetController::new(profile);

    // When: Checking if connections are allowed
    let is_allowed = controller.is_dns_allowed().unwrap();

    // Then: The result matches expectations
    assert_eq!(is_allowed, expected_allowed);
}

#[test]
fn network_profile_offline_blocks_all_connections() {
    // Given: An offline network profile
    let controller = NetController::new(NetProfile::Offline);

    // When: Checking connection permissions
    // Then: All connections are blocked
    assert!(controller.is_offline());
    assert!(!controller.is_limited());
    assert!(!controller.is_open());
}

#[test]
fn network_profile_limited_allows_specific_ports() {
    // Given: A limited network profile with specific ports
    let mut controller = NetController::new(NetProfile::Limited { 
        allowed_ports: vec![80, 443] 
    });

    // When: Adding allowed ports
    controller.allow_port(8080).unwrap();

    // Then: The ports are tracked correctly
    assert!(controller.allowed_ports().contains(&80));
    assert!(controller.allowed_ports().contains(&443));
    assert!(controller.allowed_ports().contains(&8080));
}

#[test]
fn filesystem_profile_enforces_read_only_rootfs() {
    // Given: A read-only filesystem profile
    let controller = FsController::new(
        FsProfile::ReadOnly { workdir: true },
        PathBuf::from("/tmp/test")
    );

    // When: Checking filesystem permissions
    // Then: Read-only constraints are enforced
    assert!(controller.is_read_only());
    
    // Workdir should be allowed
    assert!(controller.is_allowed(&PathBuf::from("/tmp/test/workdir")).unwrap());
    
    // Root filesystem should be restricted
    assert!(!controller.is_allowed(&PathBuf::from("/etc/passwd")).unwrap());
}

#[test]
fn filesystem_profile_writable_allows_modifications() {
    // Given: A writable filesystem profile
    let controller = FsController::new(
        FsProfile::Writable { workdir: true },
        PathBuf::from("/tmp/test")
    );

    // When: Checking filesystem permissions
    // Then: Writable constraints are enforced
    assert!(!controller.is_read_only());
    
    // Workdir should be allowed
    assert!(controller.is_allowed(&PathBuf::from("/tmp/test/workdir")).unwrap());
}

#[test]
fn process_profile_drops_capabilities_and_runs_non_root() {
    // Given: An isolated process profile
    let controller = ProcController::new(ProcProfile::Isolated);

    // When: Checking process constraints
    // Then: Security constraints are enforced
    assert!(controller.is_non_root());
    assert!(controller.capabilities_dropped());
    assert!(controller.is_isolated());
    assert!(!controller.is_strict());
}

#[test]
fn process_profile_strict_enforces_maximum_isolation() {
    // Given: A strict process profile
    let controller = ProcController::new(ProcProfile::Strict);

    // When: Checking process constraints
    // Then: Maximum isolation is enforced
    assert!(controller.is_non_root());
    assert!(controller.capabilities_dropped());
    assert!(controller.is_isolated());
    assert!(controller.is_strict());
}

#[test]
fn process_profile_standard_allows_normal_operations() {
    // Given: A standard process profile
    let controller = ProcController::new(ProcProfile::Standard);

    // When: Checking process constraints
    // Then: Normal operations are allowed
    assert!(controller.is_non_root()); // Still non-root by default
    assert!(controller.capabilities_dropped()); // Still drops capabilities
    assert!(!controller.is_isolated());
    assert!(!controller.is_strict());
}

#[test]
fn resource_limits_validate_reasonable_constraints() {
    // Given: A process controller with resource limits
    let controller = ProcController::new(ProcProfile::Strict);

    // When: Applying security constraints
    let result = controller.apply_security_constraints();

    // Then: Constraints are validated successfully
    assert!(result.is_ok());
}

#[test]
fn resource_limits_reject_excessive_constraints() {
    // Given: A process controller with excessive resource limits
    let mut controller = ProcController::new(ProcProfile::Strict);
    controller.resource_limits.cpu_time_secs = Some(7200); // 2 hours
    controller.resource_limits.memory_bytes = Some(2 * 1024 * 1024 * 1024); // 2GB

    // When: Applying security constraints
    let result = controller.apply_security_constraints();

    // Then: Excessive constraints are rejected
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert!(error.to_string().contains("limit too high"));
}

#[test]
fn policy_default_is_locked_for_security() {
    // Given: A default policy
    let policy = Policy::default();

    // When: Checking the policy configuration
    // Then: It defaults to locked for security
    assert!(matches!(policy.net, NetProfile::Offline));
    assert!(matches!(policy.fs, FsProfile::ReadOnly { workdir: true }));
    assert!(matches!(policy.proc, ProcProfile::Isolated));
}

#[test]
fn policy_can_be_customized_while_maintaining_security() {
    // Given: A custom policy configuration
    let policy = Policy {
        net: NetProfile::Limited { allowed_ports: vec![80, 443] },
        fs: FsProfile::ReadOnly { workdir: true },
        proc: ProcProfile::Isolated,
        limits: ResourceLimits::strict(),
    };

    // When: Checking the policy configuration
    // Then: Custom settings are preserved while maintaining security
    assert!(matches!(policy.net, NetProfile::Limited { .. }));
    assert!(matches!(policy.fs, FsProfile::ReadOnly { .. }));
    assert!(matches!(policy.proc, ProcProfile::Isolated));
}
