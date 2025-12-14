//! Enterprise types for FMEA & Poka-Yoke marketplace framework.
//!
//! This module provides core types for Fortune 500 enterprise features:
//! - [`enterprise`] - Enterprise configuration, path protection, Poka-Yoke config
//! - [`fmea`] - FMEA failure modes, RPN scores, validation
//! - [`path_protection`] - Glob-based path matching for domain protection
//! - [`codeowners`] - CODEOWNERS generation from noun OWNERS files

pub mod codeowners;
pub mod enterprise;
pub mod fmea;
pub mod path_protection;

// Re-exports for convenience
pub use codeowners::{CodeownersGenerator, OwnerEntry, OwnersFile};
pub use enterprise::{
    DomainProtectionStrategy, EnterpriseConfig, GenerationConfig, PokaYokeConfig, ProtectedPath,
    RegeneratePath,
};
pub use fmea::{
    Detection, FailureModeEntry, FmeaConfig, FmeaValidationError, Occurrence, RpnLevel, RpnScore,
    Severity,
};
pub use path_protection::{PathProtectionConfig, PathProtectionError};
