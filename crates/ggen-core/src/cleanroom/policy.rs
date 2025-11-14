//! Type-level policy enforcement for cleanroom environments
//!
//! This module provides compile-time guarantees about the security posture
//! of your cleanroom environment through Rust's type system.
//!
//! ## Policies
//!
//! - `Locked`: Maximum security, all deterministic surfaces enforced
//! - `Permissive`: Integration testing, some non-deterministic surfaces allowed
//!
//! ## Example
//!
//! ```rust,no_run
//! use ggen_core::cleanroom::{CleanroomCore, policy::{Locked, Permissive}};
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! // This is locked down - cannot access network
//! let locked_env = CleanroomCore::<Locked>::builder()
//!     .time_frozen(42)
//!     .net_offline()
//!     .build()?;
//!
//! // This is permissive - can access network for integration tests
//! let permissive_env = CleanroomCore::<Permissive>::builder()
//!     .build()?;
//! # Ok(())
//! # }
//! ```

use std::marker::PhantomData;
use serde::{Serialize, Deserialize};

/// Policy trait for type-level enforcement
///
/// Policies are zero-sized types that exist only at compile time to enforce
/// security posture through the type system.
pub trait Policy: Sized + Send + Sync + 'static {
    /// Policy name for attestation
    fn name() -> &'static str;

    /// Whether this policy requires all deterministic surfaces
    fn requires_determinism() -> bool;

    /// Whether this policy allows network access
    fn allows_network() -> bool;

    /// Whether this policy allows root privileges
    fn allows_root() -> bool;

    /// Whether this policy allows real filesystem access
    fn allows_real_fs() -> bool;

    /// Security level (0 = permissive, 100 = locked)
    fn security_level() -> u8;
}

/// Locked policy - Maximum security, all deterministic surfaces enforced
///
/// Use this for:
/// - Unit tests
/// - Pure functional tests
/// - Tests that should be 100% reproducible
/// - Security-critical validation
///
/// **Constraints:**
/// - Time must be frozen or stepped
/// - RNG must be seeded
/// - Filesystem must be ephemeral or read-only
/// - Network must be offline
/// - Process must be non-root with dropped capabilities
///
/// # Example
///
/// ```rust,no_run
/// use ggen_core::cleanroom::{CleanroomCore, policy::Locked};
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let env = CleanroomCore::<Locked>::builder()
///     .time_frozen(42)
///     .rng_seeded(42)
///     .fs_ephemeral()
///     .net_offline()
///     .build()?;
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Locked;

impl Policy for Locked {
    fn name() -> &'static str {
        "Locked"
    }

    fn requires_determinism() -> bool {
        true
    }

    fn allows_network() -> bool {
        false
    }

    fn allows_root() -> bool {
        false
    }

    fn allows_real_fs() -> bool {
        false
    }

    fn security_level() -> u8 {
        100
    }
}

/// Permissive policy - Integration testing, some non-deterministic surfaces allowed
///
/// Use this for:
/// - Integration tests with external services
/// - Tests that need real network access
/// - Tests that need real filesystem access
/// - Performance benchmarks
///
/// **Constraints:**
/// - Can use real time (non-deterministic)
/// - Can use real RNG (non-deterministic)
/// - Can access real filesystem
/// - Can access network
/// - May run as root (for container tests)
///
/// # Example
///
/// ```rust,no_run
/// use ggen_core::cleanroom::{CleanroomCore, policy::Permissive};
///
/// let env = CleanroomCore::<Permissive>::builder()
///     .build()?; // All surfaces default to real/permissive
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Permissive;

impl Policy for Permissive {
    fn name() -> &'static str {
        "Permissive"
    }

    fn requires_determinism() -> bool {
        false
    }

    fn allows_network() -> bool {
        true
    }

    fn allows_root() -> bool {
        true
    }

    fn allows_real_fs() -> bool {
        true
    }

    fn security_level() -> u8 {
        30
    }
}

/// Policy validator - Runtime checks that match compile-time policy
///
/// This ensures that the runtime configuration matches the policy constraints
/// set at the type level.
pub struct PolicyValidator<P: Policy> {
    _policy: PhantomData<P>,
}

impl<P: Policy> PolicyValidator<P> {
    /// Create new policy validator
    pub fn new() -> Self {
        Self {
            _policy: PhantomData,
        }
    }

    /// Validate that surfaces match policy requirements
    pub fn validate_surfaces(&self, surfaces: &super::surfaces::DeterministicSurfaces) -> Result<(), String> {
        // Check determinism requirement
        if P::requires_determinism() && !surfaces.is_fully_deterministic() {
            return Err(format!(
                "Policy '{}' requires full determinism, but surfaces are not fully deterministic (score: {})",
                P::name(),
                surfaces.determinism_score()
            ));
        }

        // Check network constraints
        if !P::allows_network() && matches!(surfaces.net_mode(), super::surfaces::NetMode::Real) {
            return Err(format!(
                "Policy '{}' does not allow network access, but network mode is Real",
                P::name()
            ));
        }

        // Check filesystem constraints
        if !P::allows_real_fs() && matches!(surfaces.fs_mode(), super::surfaces::FsMode::Real) {
            return Err(format!(
                "Policy '{}' does not allow real filesystem access, but fs mode is Real",
                P::name()
            ));
        }

        // Check root constraints
        if !P::allows_root() && matches!(surfaces.proc_mode(), super::surfaces::ProcMode::Real) {
            return Err(format!(
                "Policy '{}' does not allow root process, but proc mode is Real",
                P::name()
            ));
        }

        Ok(())
    }

    /// Get policy metadata for attestation
    pub fn metadata(&self) -> PolicyMetadata {
        PolicyMetadata {
            name: P::name().to_string(),
            requires_determinism: P::requires_determinism(),
            allows_network: P::allows_network(),
            allows_root: P::allows_root(),
            allows_real_fs: P::allows_real_fs(),
            security_level: P::security_level(),
        }
    }
}

impl<P: Policy> Default for PolicyValidator<P> {
    fn default() -> Self {
        Self::new()
    }
}

/// Policy metadata for attestation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyMetadata {
    pub name: String,
    pub requires_determinism: bool,
    pub allows_network: bool,
    pub allows_root: bool,
    pub allows_real_fs: bool,
    pub security_level: u8,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cleanroom::surfaces::DeterministicSurfaces;
    use chicago_tdd_tools::{test, async_test};

    test!(test_locked_policy, {
        assert_eq!(Locked::name(), "Locked");
        assert_eq!(Locked::security_level(), 100);
        assert!(Locked::requires_determinism());
        assert!(!Locked::allows_network());
        assert!(!Locked::allows_root());
        assert!(!Locked::allows_real_fs());
    });

    test!(test_permissive_policy, {
        assert_eq!(Permissive::name(), "Permissive");
        assert_eq!(Permissive::security_level(), 30);
        assert!(!Permissive::requires_determinism());
        assert!(Permissive::allows_network());
        assert!(Permissive::allows_root());
        assert!(Permissive::allows_real_fs());
    });

    test!(test_locked_validator_accepts_deterministic, {
        let validator = PolicyValidator::<Locked>::new();
        let surfaces = DeterministicSurfaces::deterministic(42);

        assert!(validator.validate_surfaces(&surfaces).is_ok());
    });

    test!(test_locked_validator_rejects_permissive, {
        let validator = PolicyValidator::<Locked>::new();
        let surfaces = DeterministicSurfaces::permissive();

        assert!(validator.validate_surfaces(&surfaces).is_err());
    });

    test!(test_permissive_validator_accepts_all, {
        let validator = PolicyValidator::<Permissive>::new();
        let surfaces = DeterministicSurfaces::permissive();

        assert!(validator.validate_surfaces(&surfaces).is_ok());

        let det_surfaces = DeterministicSurfaces::deterministic(42);
        assert!(validator.validate_surfaces(&det_surfaces).is_ok());
    });

    test!(test_policy_metadata, {
        let validator = PolicyValidator::<Locked>::new();
        let metadata = validator.metadata();

        assert_eq!(metadata.name, "Locked");
        assert_eq!(metadata.security_level, 100);
        assert!(metadata.requires_determinism);
    });
}
