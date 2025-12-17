//! # ggen-e2e: End-to-End Testing Framework
//!
//! Cross-platform E2E testing for `ggen sync` using testcontainers-rs for Linux
//! container testing and native execution for macOS. Tests verify byte-for-byte
//! identical output across platforms.
//!
//! ## Modules
//!
//! - [`platform`] - Platform detection (OS/Arch) and capabilities
//! - [`error`] - Comprehensive error types for all E2E operations
//! - [`fixture`] - Test fixture management and discovery
//! - [`golden`] - Golden file comparison and validation
//! - [`container`] - Testcontainer lifecycle management
//! - [`runner`] - Test execution orchestration
//! - [`result`] - Test result and status tracking
//! - [`comparison`] - Cross-platform comparison analysis

pub mod comparison;
pub mod container;
pub mod error;
pub mod fixture;
pub mod golden;
pub mod platform;
pub mod result;
pub mod runner;

// Re-export commonly used types
pub use comparison::CrossPlatformComparison;
pub use container::ContainerConfig;
pub use error::{ContainerError, E2EError, FixtureError, GoldenError, PlatformError, RunnerError};
pub use fixture::TestFixture;
pub use golden::{GoldenFile, GoldenMismatch};
pub use platform::{Arch, Os, Platform};
pub use result::{TestExecution, TestResult, TestStatus};
pub use runner::TestRunner;
