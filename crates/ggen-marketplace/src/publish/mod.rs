//! Publish Module for ggen Marketplace (Feature 014-marketplace-gpack, Story 1)
//!
//! Provides functionality to publish gpack packages to crates.io and other registries.
//!
//! ## Quick Start
//!
//! ```rust,ignore
//! use ggen_marketplace::publish::{execute_publish, PublishConfig};
//!
//! // Publish a package with dry-run
//! let config = PublishConfig::new("./my-package")
//!     .with_dry_run(true)
//!     .with_allow_warnings(true);
//!
//! let result = execute_publish(&config).await?;
//! result.display();
//! ```
//!
//! ## Components
//!
//! - **`manifest`**: YAML to TOML conversion for Cargo manifests
//! - **`format`**: Validation rules for crate names, SemVer, and FMEA requirements
//! - **`crates_client`**: Async HTTP client for crates.io API
//! - **`command`**: CLI command implementation
//!
//! ## Workflow
//!
//! 1. Create `gpack.yaml` manifest in your package directory
//! 2. Run validation with `--dry-run` to check for errors
//! 3. Set `CARGO_REGISTRY_TOKEN` environment variable
//! 4. Publish to crates.io
//!
//! ## gpack.yaml Format
//!
//! ```yaml
//! package:
//!   name: my-crate-gpack
//!   version: 1.0.0
//!   edition: "2021"
//!   description: My awesome gpack package
//!   license: MIT
//!   authors:
//!     - "Your Name <you@example.com>"
//!   keywords:
//!     - gpack
//!     - cli
//!   categories:
//!     - development-tools
//!
//! dependencies:
//!   serde: "1.0"
//!   tokio:
//!     version: "1.0"
//!     features: ["full"]
//!     optional: true
//!
//! features:
//!   default: ["serde"]
//!   full: ["tokio"]
//! ```
//!
//! ## Validation Rules
//!
//! The validator enforces these rules before publishing:
//!
//! - **Package name**: ASCII alphanumeric, underscores, hyphens; starts with letter
//! - **Version**: Valid SemVer (MAJOR.MINOR.PATCH)
//! - **Description**: Required for crates.io
//! - **License**: Required (SPDX identifier or license file)
//! - **Keywords**: Maximum 5, ASCII only
//! - **Categories**: Maximum 5, must be valid crates.io categories
//! - **FMEA**: Optional (enable with `--require-fmea`)
//!
//! ## Common Errors and Fixes
//!
//! | Error | Fix |
//! |-------|-----|
//! | "Package name cannot be empty" | Add `name` field |
//! | "Version is not valid SemVer" | Use MAJOR.MINOR.PATCH format |
//! | "Description is required" | Add `description` field |
//! | "License is required" | Add `license` or `license_file` |
//! | "Too many keywords" | Keep to 5 keywords max |
//! | "FMEA documentation required" | Set `fmea_documented: true` or remove `--require-fmea` |
//!
//! ## Environment Variables
//!
//! - `CARGO_REGISTRY_TOKEN`: API token for crates.io (get from https://crates.io/settings/tokens)
//!
//! ## Troubleshooting
//!
//! ### "Authentication failed"
//!
//! Ensure `CARGO_REGISTRY_TOKEN` is set:
//! ```bash
//! export CARGO_REGISTRY_TOKEN="your-token-here"
//! ```
//!
//! ### "Network timeout"
//!
//! The client retries with exponential backoff (3 attempts, up to 5 seconds).
//! Check your network connection if timeouts persist.
//!
//! ### "Validation failed"
//!
//! Run with `--dry-run` first to see all validation errors:
//! ```bash
//! ggen marketplace publish --package ./my-crate --dry-run
//! ```

pub mod command;
pub mod crates_client;
pub mod format;
pub mod manifest;

// Re-exports for convenience
pub use command::{execute_publish, PublishCommandResult, PublishConfig};
pub use crates_client::{CratesClient, CratesClientConfig, PublishResult};
pub use format::{ManifestValidator, ValidationError, ValidationResult as FormatValidationResult};
pub use manifest::{CargoManifest, GpackManifest, ManifestConverter};
