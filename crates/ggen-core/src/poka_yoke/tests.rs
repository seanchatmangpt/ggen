//! Integration tests for poka-yoke mechanisms.
//!
//! Individual mechanism tests are in their respective module files.
//! This file tests cross-mechanism interactions.

use super::*;

#[test]
fn test_poka_yoke_modules_accessible() {
    // Verify all modules are properly exported
    let _ = std::any::type_name::<AtomicFileWriter<Uncommitted>>();
    let _ = std::any::type_name::<ValidatedPath>();
    let _ = std::any::type_name::<TimeoutIO>();
    let _ = std::any::type_name::<SanitizedInput>();
    let _ = std::any::type_name::<LockfileGuard>();
    let _ = std::any::type_name::<NetworkRetry>();
    let _ = std::any::type_name::<DryRunMode>();
}
