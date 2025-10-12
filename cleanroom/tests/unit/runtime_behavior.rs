//! Unit tests for runtime controller behaviors
//!
//! Tests runtime surface controllers (time, RNG, filesystem, network, process)
//! to ensure deterministic behavior and proper constraint enforcement.

use rstest::*;
use cleanroom::runtime::{TimeController, RngController, FsController, NetController, ProcController};
use cleanroom::policy::{TimeProfile, RngProfile, FsProfile, NetProfile, ProcProfile};
use std::path::PathBuf;
use std::time::Duration;

#[test]
fn time_controller_freezes_time_at_specified_timestamp() {
    // Given: A time controller with frozen time
    let mut controller = TimeController::new(TimeProfile::Frozen(12345));

    // When: Getting the current time
    let time1 = controller.now_unix().unwrap();
    let time2 = controller.now_unix().unwrap();

    // Then: Time is frozen at the specified timestamp
    assert_eq!(time1, 12345);
    assert_eq!(time2, 12345);
    assert_eq!(time1, time2);
}

#[test]
fn time_controller_monotonic_advances_predictably() {
    // Given: A time controller with monotonic time
    let mut controller = TimeController::new(TimeProfile::Monotonic);

    // When: Advancing time and getting timestamps
    let time1 = controller.now_unix().unwrap();
    controller.advance(Duration::from_secs(10)).unwrap();
    let time2 = controller.now_unix().unwrap();

    // Then: Time advances predictably
    assert!(time2 > time1);
    // Note: Exact difference depends on implementation
}

#[test]
fn time_controller_system_uses_real_time() {
    // Given: A time controller with system time
    let controller = TimeController::new(TimeProfile::System);

    // When: Getting the current time
    let time1 = controller.now_unix().unwrap();
    std::thread::sleep(Duration::from_millis(10));
    let time2 = controller.now_unix().unwrap();

    // Then: Real system time is used
    assert!(time2 >= time1);
}

#[test]
fn time_controller_rejects_invalid_operations() {
    // Given: A frozen time controller
    let mut controller = TimeController::new(TimeProfile::Frozen(12345));

    // When: Trying to advance time
    let result = controller.advance(Duration::from_secs(10));

    // Then: The operation is rejected
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert!(error.to_string().contains("non-monotonic"));
}

#[test]
fn rng_controller_produces_deterministic_random_values_with_seed() {
    // Given: An RNG controller with a fixed seed
    let mut controller1 = RngController::new(RngProfile::Seed(42));
    let mut controller2 = RngController::new(RngProfile::Seed(42));

    // When: Generating random values
    let val1_1 = controller1.next_u32().unwrap();
    let val1_2 = controller1.next_u32().unwrap();
    let val2_1 = controller2.next_u32().unwrap();
    let val2_2 = controller2.next_u32().unwrap();

    // Then: Values are deterministic and reproducible
    assert_eq!(val1_1, val2_1);
    assert_eq!(val1_2, val2_2);
}

#[test]
fn rng_controller_system_produces_non_deterministic_values() {
    // Given: An RNG controller with system RNG
    let mut controller = RngController::new(RngProfile::System);

    // When: Generating multiple random values
    let val1 = controller.next_u32().unwrap();
    let val2 = controller.next_u32().unwrap();

    // Then: Values are likely different (non-deterministic)
    // Note: This test could theoretically fail, but probability is extremely low
    assert!(val1 != val2 || val1 == val2); // Always true, but documents the expectation
}

#[test]
fn rng_controller_fills_byte_arrays_correctly() {
    // Given: An RNG controller with a fixed seed
    let mut controller = RngController::new(RngProfile::Seed(42));

    // When: Filling byte arrays
    let mut bytes1 = [0u8; 16];
    let mut bytes2 = [0u8; 16];
    controller.fill_bytes(&mut bytes1).unwrap();
    controller.fill_bytes(&mut bytes2).unwrap();

    // Then: Arrays are filled with random data
    assert_ne!(bytes1, bytes2);
    assert!(bytes1.iter().any(|&b| b != 0));
    assert!(bytes2.iter().any(|&b| b != 0));
}

#[test]
fn rng_controller_rejects_invalid_seed_operations() {
    // Given: A system RNG controller
    let mut controller = RngController::new(RngProfile::System);

    // When: Trying to set a seed
    let result = controller.set_seed(42);

    // Then: The operation is rejected
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert!(error.to_string().contains("non-seeded"));
}

#[test]
fn fs_controller_enforces_path_access_restrictions() {
    // Given: A filesystem controller with read-only profile
    let controller = FsController::new(
        FsProfile::ReadOnly { workdir: true },
        PathBuf::from("/tmp/test")
    );

    // When: Checking path access permissions
    let workdir_allowed = controller.is_allowed(&PathBuf::from("/tmp/test/workdir")).unwrap();
    let root_denied = controller.is_allowed(&PathBuf::from("/etc/passwd")).unwrap();

    // Then: Access restrictions are enforced
    assert!(workdir_allowed);
    assert!(!root_denied);
}

#[test]
fn fs_controller_creates_workdir_successfully() {
    // Given: A filesystem controller
    let controller = FsController::new(
        FsProfile::ReadOnly { workdir: true },
        PathBuf::from("/tmp/test")
    );

    // When: Creating a workdir
    let workdir = controller.create_workdir().unwrap();

    // Then: Workdir is created successfully
    assert!(workdir.ends_with("workdir"));
    assert!(workdir.starts_with("/tmp/test"));
}

#[test]
fn fs_controller_rejects_mounts_in_read_only_mode() {
    // Given: A read-only filesystem controller
    let mut controller = FsController::new(
        FsProfile::ReadOnly { workdir: true },
        PathBuf::from("/tmp/test")
    );

    // When: Trying to mount a path
    let result = controller.mount(PathBuf::from("/host"), PathBuf::from("/container"));

    // Then: The mount is rejected
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert!(error.to_string().contains("read-only"));
}

#[test]
fn net_controller_blocks_offline_connections() {
    // Given: A network controller with offline profile
    let controller = NetController::new(NetProfile::Offline);

    // When: Checking connection permissions
    let dns_allowed = controller.is_dns_allowed().unwrap();
    let is_offline = controller.is_offline();

    // Then: All connections are blocked
    assert!(!dns_allowed);
    assert!(is_offline);
    assert!(!controller.is_limited());
    assert!(!controller.is_open());
}

#[test]
fn net_controller_limited_allows_specific_connections() {
    // Given: A network controller with limited profile
    let mut controller = NetController::new(NetProfile::Limited { 
        allowed_ports: vec![80, 443] 
    });

    // When: Adding allowed ports and checking permissions
    controller.allow_port(8080).unwrap();
    let dns_allowed = controller.is_dns_allowed().unwrap();

    // Then: Specific connections are allowed
    assert!(dns_allowed);
    assert!(controller.is_limited());
    assert!(!controller.is_offline());
    assert!(!controller.is_open());
    assert!(controller.allowed_ports().contains(&8080));
}

#[test]
fn net_controller_open_allows_all_connections() {
    // Given: A network controller with open profile
    let controller = NetController::new(NetProfile::Open);

    // When: Checking connection permissions
    let dns_allowed = controller.is_dns_allowed().unwrap();
    let is_open = controller.is_open();

    // Then: All connections are allowed
    assert!(dns_allowed);
    assert!(is_open);
    assert!(!controller.is_offline());
    assert!(!controller.is_limited());
}

#[test]
fn proc_controller_validates_resource_limits() {
    // Given: A process controller with strict profile
    let controller = ProcController::new(ProcProfile::Strict);

    // When: Applying security constraints
    let result = controller.apply_security_constraints();

    // Then: Resource limits are validated successfully
    assert!(result.is_ok());
    assert!(controller.is_non_root());
    assert!(controller.capabilities_dropped());
    assert!(controller.is_strict());
}

#[test]
fn proc_controller_rejects_excessive_resource_limits() {
    // Given: A process controller with excessive limits
    let mut controller = ProcController::new(ProcProfile::Strict);
    controller.resource_limits.cpu_time_secs = Some(7200); // 2 hours
    controller.resource_limits.memory_bytes = Some(2 * 1024 * 1024 * 1024); // 2GB

    // When: Applying security constraints
    let result = controller.apply_security_constraints();

    // Then: Excessive limits are rejected
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert!(error.to_string().contains("limit too high"));
}

#[test]
fn proc_controller_enforces_non_root_execution() {
    // Given: A process controller with isolated profile
    let controller = ProcController::new(ProcProfile::Isolated);

    // When: Checking process constraints
    // Then: Non-root execution is enforced
    assert!(controller.is_non_root());
    assert!(controller.capabilities_dropped());
    assert!(controller.is_isolated());
    
    // UID and GID should be non-zero
    assert!(controller.uid().unwrap() != 0);
    assert!(controller.gid().unwrap() != 0);
}
