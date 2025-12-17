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

pub mod error;
pub mod platform;
pub mod fixture;
pub mod golden;
pub mod container;
pub mod runner;
pub mod result;
pub mod comparison;

// Re-export commonly used types
pub use error::{E2EError, PlatformError, FixtureError, GoldenError, ContainerError, RunnerError};
pub use platform::{Platform, Os, Arch};
pub use fixture::TestFixture;
pub use golden::{GoldenFile, GoldenMismatch};
pub use container::ContainerConfig;
pub use runner::TestRunner;
pub use result::{TestResult, TestStatus, TestExecution};
pub use comparison::CrossPlatformComparison;
