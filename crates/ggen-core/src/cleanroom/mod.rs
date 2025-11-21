//! Cleanroom 80/20 Production Testing Framework
//!
//! This module provides deterministic, reproducible test environments with:
//! - 5 deterministic surfaces: Process, FileSystem, Network, Time, RNG
//! - Type-level policy enforcement (Locked vs Permissive)
//! - Attestation and forensics for every test run
//! - Flake detection and reproducibility validation
//!
//! ## Philosophy
//!
//! Cleanroom owns the dark matter (80% of hidden toil):
//! - Env/setup drift
//! - Container glue and service fixtures
//! - Flaky IO, time, and network
//! - Cross-platform path/shell differences
//! - Coverage mapping from containers
//! - Secrets handling, redaction, and logs
//! - Reproducers and artifact capture
//!
//! And satisfies dark energy (80% of forces):
//! - Security baselines by default
//! - Reproducibility attestation
//! - Compliance evidence (SBOM, provenance)
//! - Cost and latency control in CI
//! - Determinism across the matrix

pub mod surfaces;
pub mod policy;
pub mod attestation;
pub mod forensics;
pub mod scenario;
pub mod flake;

use std::path::{Path, PathBuf};
use std::time::Duration;
use tempfile::TempDir;

use crate::error::Result;
use surfaces::{DeterministicSurfaces, TimeMode, RngMode, FsMode, NetMode, ProcMode};
use policy::Policy;
use attestation::Attestation;
use forensics::ForensicsPack;

/// Cleanroom environment with deterministic surfaces and type-level policy
///
/// This is the main entry point for cleanroom testing. It extends the existing
/// CleanroomEnv from the test suite with 80/20 deterministic surfaces.
///
/// # Example
///
/// ```rust,no_run
/// use ggen_core::cleanroom::{CleanroomCore, Policy};
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let env = CleanroomCore::builder()
///     .policy(Policy::Locked)
///     .time_frozen(42)
///     .rng_seeded(42)
///     .fs_ephemeral()
///     .net_offline()
///     .build()?;
///
/// // All operations in this environment are deterministic
/// env.run_test(|| {
///     // Test code here
/// })?;
/// # Ok(())
/// # }
/// ```
pub struct CleanroomCore<P: Policy> {
    /// Temporary directory (auto-deleted on drop)
    temp_dir: TempDir,
    /// Project root within temp directory
    project_root: PathBuf,
    /// Deterministic surfaces configuration
    surfaces: DeterministicSurfaces,
    /// Policy marker (type-level enforcement)
    _policy: std::marker::PhantomData<P>,
    /// Attestation collector
    attestation: Attestation,
    /// Forensics pack builder
    forensics: ForensicsPack,
}

/// Builder for CleanroomCore with fluent API
pub struct CleanroomBuilder<P: Policy> {
    time_mode: Option<TimeMode>,
    rng_mode: Option<RngMode>,
    fs_mode: Option<FsMode>,
    net_mode: Option<NetMode>,
    proc_mode: Option<ProcMode>,
    _policy: std::marker::PhantomData<P>,
}

impl<P: Policy> CleanroomCore<P> {
    /// Create a new builder for cleanroom environment
    pub fn builder() -> CleanroomBuilder<P> {
        CleanroomBuilder {
            time_mode: None,
            rng_mode: None,
            fs_mode: None,
            net_mode: None,
            proc_mode: None,
            _policy: std::marker::PhantomData,
        }
    }

    /// Get path to project root
    pub fn root(&self) -> &Path {
        &self.project_root
    }

    /// Get attestation for this test run
    pub fn attestation(&self) -> &Attestation {
        &self.attestation
    }

    /// Get forensics pack for this test run
    pub fn forensics(&self) -> &ForensicsPack {
        &self.forensics
    }

    /// Export attestation to JSON
    pub fn export_attestation(&self, path: impl AsRef<Path>) -> Result<()> {
        self.attestation.export(path)
    }

    /// Generate forensics pack (includes logs, env, attestation)
    pub fn generate_forensics_pack(&self, path: impl AsRef<Path>) -> Result<()> {
        self.forensics.generate(path, &self.attestation)
    }

    /// Validate determinism by running test N times
    pub fn validate_determinism<F>(&self, test_fn: F, iterations: usize) -> Result<bool>
    where
        F: Fn() -> Result<()>,
    {
        flake::validate_determinism(test_fn, iterations, &self.surfaces)
    }
}

impl<P: Policy> CleanroomBuilder<P> {
    /// Set time mode (frozen, stepped, or real)
    pub fn time(mut self, mode: TimeMode) -> Self {
        self.time_mode = Some(mode);
        self
    }

    /// Freeze time at a specific seed
    pub fn time_frozen(self, seed: u64) -> Self {
        self.time(TimeMode::Frozen(seed))
    }

    /// Set RNG mode (seeded or real)
    pub fn rng(mut self, mode: RngMode) -> Self {
        self.rng_mode = Some(mode);
        self
    }

    /// Seed RNG for reproducibility
    pub fn rng_seeded(self, seed: u64) -> Self {
        self.rng(RngMode::Seeded(seed))
    }

    /// Set filesystem mode (ephemeral, read-only, or real)
    pub fn fs(mut self, mode: FsMode) -> Self {
        self.fs_mode = Some(mode);
        self
    }

    /// Use ephemeral filesystem (tmpfs-backed)
    pub fn fs_ephemeral(self) -> Self {
        self.fs(FsMode::Ephemeral)
    }

    /// Set network mode (offline, limited, or real)
    pub fn net(mut self, mode: NetMode) -> Self {
        self.net_mode = Some(mode);
        self
    }

    /// Run in offline mode (no network access)
    pub fn net_offline(self) -> Self {
        self.net(NetMode::Offline)
    }

    /// Set process mode (non-root, capabilities, etc.)
    pub fn proc(mut self, mode: ProcMode) -> Self {
        self.proc_mode = Some(mode);
        self
    }

    /// Build the cleanroom environment
    pub fn build(self) -> Result<CleanroomCore<P>> {
        let temp_dir = TempDir::new()?;
        let project_root = temp_dir.path().to_path_buf();

        let surfaces = DeterministicSurfaces::new(
            self.time_mode.unwrap_or(TimeMode::Real),
            self.rng_mode.unwrap_or(RngMode::Real),
            self.fs_mode.unwrap_or(FsMode::Real),
            self.net_mode.unwrap_or(NetMode::Real),
            self.proc_mode.unwrap_or(ProcMode::Real),
        );

        let attestation = Attestation::new(&surfaces, P::name());
        let forensics = ForensicsPack::new(&project_root);

        Ok(CleanroomCore {
            temp_dir,
            project_root,
            surfaces,
            _policy: std::marker::PhantomData,
            attestation,
            forensics,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cleanroom::policy::{Locked, Permissive};

    #[allow(clippy::expect_used)]
    #[test]
    #[allow(clippy::expect_used)]
    fn test_cleanroom_core_builder() {
        let env = CleanroomCore::<Locked>::builder()
            .time_frozen(42)
            .rng_seeded(42)
            .fs_ephemeral()
            .net_offline()
            .build()
            .expect("Failed to build cleanroom");

        assert!(env.root().exists());
    }

    #[allow(clippy::expect_used)]
    #[test]
    #[allow(clippy::expect_used)]
    fn test_cleanroom_deterministic_surfaces() {
        let env = CleanroomCore::<Locked>::builder()
            .time_frozen(42)
            .rng_seeded(42)
            .build()
            .expect("Failed to build cleanroom");

        // Time should be frozen
        assert!(matches!(env.surfaces.time_mode(), TimeMode::Frozen(42)));

        // RNG should be seeded
        assert!(matches!(env.surfaces.rng_mode(), RngMode::Seeded(42)));
    }

    #[allow(clippy::expect_used)]
    #[test]
    #[allow(clippy::expect_used)]
    fn test_cleanroom_attestation() {
        let env = CleanroomCore::<Locked>::builder()
            .build()
            .expect("Failed to build cleanroom");

        let attestation = env.attestation();
        assert_eq!(attestation.policy(), "Locked");
    }
}
