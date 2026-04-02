//! GGEN Packs Services
//!
//! Core services for intelligent package management across the system.
//! These services are used by all ggen packs commands to perform package
//! discovery, analysis, management, and compliance operations.

pub mod discovery;

pub use discovery::PackageDiscoveryService;
