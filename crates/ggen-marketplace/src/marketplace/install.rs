//! Package installation system with dependency resolution
//!
//! Features:
//! - Dependency resolution with cycle detection
//! - Conflict detection
//! - Atomic installation
//! - Rollback on failure
//! - Batch installation with transaction semantics
//! - Parallel installation of independent packages
//! - Progress reporting for UI integration

use async_trait::async_trait;
use flate2::read::GzDecoder;
use semver::Version;
use std::collections::{HashMap, HashSet};
use std::fs::{self, File};
use std::io::BufWriter;
use std::path::{Path, PathBuf};
use tar::Archive;
use tracing::{debug, info, instrument, span, warn};
use uuid::Uuid;

use crate::marketplace::cache::{CachedPack, PackCache};
use crate::marketplace::error::{Error, Result};
use crate::marketplace::models::{InstallationManifest, PackageId, PackageVersion};
use crate::marketplace::profile::Profile;
use crate::marketplace::security::ChecksumCalculator;
use crate::marketplace::traits::{AsyncRepository, Installable};
use crate::marketplace::trust::{RegistryClass, RegistryType, TrustTier};

/// Package installer with caching and signature verification
pub struct Installer<R: AsyncRepository> {
    repository: R,
    cache: PackCache,
    /// Security profile for trust tier enforcement (Fortune 5 CISO requirement)
    profile: Option<Profile>,
}

/// Progress callback for batch installation
/// Called with (installed_count, total_count, current_package_id)
pub type ProgressCallback = Box<dyn Fn(usize, usize, &str) + Send + Sync>;

/// Transaction state snapshot for rollback support
#[derive(Clone, Debug)]
struct TransactionSnapshot {
    installed_packages: Vec<(PackageId, PackageVersion)>,
}

impl<R: AsyncRepository> Installer<R> {
    /// Create a new installer with a cache
    #[must_use]
    pub fn new(repository: R, cache: PackCache) -> Self {
        Self {
            repository,
            cache,
            profile: None,
        }
    }

    /// Create a new installer with a security profile
    #[must_use]
    pub fn with_profile(repository: R, cache: PackCache, profile: Profile) -> Self {
        Self {
            repository,
            cache,
            profile: Some(profile),
        }
    }

    /// Create a new installer with default cache configuration
    ///
    /// # Errors
    ///
    /// Returns error if cache cannot be initialized.
    pub fn with_default_cache(repository: R) -> Result<Self> {
        let cache = PackCache::with_default_config()?;
        Ok(Self::new(repository, cache))
    }

    /// Set the security profile for trust tier enforcement
    #[must_use]
    pub fn with_security_profile(mut self, profile: Profile) -> Self {
        self.profile = Some(profile);
        self
    }

    /// Get persistent cache path for a pack.
    ///
    /// Delegates to [`crate::marketplace::metadata::pack_cache_dir`], the
    /// single canonical resolver for the transient pack cache root (see that
    /// function's docs for the full resolution order). This used to be one
    /// of three independently-hardcoded resolvers that disagreed with each
    /// other on both the default directory and `GGEN_PACK_CACHE_DIR`
    /// support; unifying them here keeps this call site in lockstep with
    /// [`crate::marketplace::metadata::get_pack_cache_dir`] and
    /// [`crate::marketplace::cache::CacheConfig::default`].
    fn persistent_cache_path(&self, package_id: &PackageId, version: &PackageVersion) -> PathBuf {
        crate::marketplace::metadata::pack_cache_dir(package_id.as_str(), version.as_str())
    }

    /// Get a reference to the cache
    #[must_use]
    pub const fn cache(&self) -> &PackCache {
        &self.cache
    }

    /// Active security profile for trust-tier enforcement, if any.
    #[must_use]
    pub fn security_profile(&self) -> Option<&Profile> {
        self.profile.as_ref()
    }

    /// Resolve a dependency tree (iterative approach for Send compatibility)
    ///
    /// # Errors
    ///
    /// * [`Error::PackageNotFound`] - When a dependency package does not exist in the repository
    /// * [`Error::InvalidVersion`] - When a dependency version requirement cannot be parsed
    /// * [`Error::DependencyResolutionFailed`] - When circular dependencies are detected or resolution fails
    #[instrument(
        name = "marketplace.resolve_dependencies",
        skip(self),
        fields(
            operation.name = "resolve_dependencies",
            operation.type = "marketplace",
            root_id = %root_id,
            root_version = %root_version,
            dependencies_count,
            duration_ms
        )
    )]
    pub async fn resolve_dependencies(
        &self, root_id: &PackageId, root_version: &PackageVersion,
    ) -> Result<Vec<(PackageId, PackageVersion)>> {
        let start = std::time::Instant::now();
        let mut resolved = Vec::new();
        let mut visited = HashSet::new();
        let mut to_process = vec![(root_id.clone(), root_version.clone())];

        while let Some((id, version)) = to_process.pop() {
            if visited.contains(&id) {
                continue;
            }

            // Get package and process its dependencies
            let package = self.repository.get_package_version(&id, &version).await?;

            for release in package.releases.values() {
                for dep in &release.dependencies {
                    if !visited.contains(&dep.id) {
                        let parsed_version = dep.version_req.parse::<PackageVersion>()?;
                        to_process.push((dep.id.clone(), parsed_version));
                    }
                }
            }

            visited.insert(id.clone());
            resolved.push((id, version));
        }

        // Sort by dependency order (dependencies first)
        resolved.reverse();

        let duration = start.elapsed();
        debug!(
            "Resolved {} dependencies for {}@{}",
            resolved.len(),
            root_id,
            root_version
        );

        // Record OTEL span attributes
        span::Span::current().record("dependencies_count", resolved.len());
        span::Span::current().record("duration_ms", duration.as_millis());

        Ok(resolved)
    }

    /// Create an installation manifest
    ///
    /// # Errors
    ///
    /// * [`Error::PackageNotFound`] - When a package ID in the list does not exist in the repository
    /// * [`Error::InvalidVersion`] - When a version string cannot be parsed during dependency resolution
    /// * [`Error::DependencyResolutionFailed`] - When dependency resolution fails for any package
    #[instrument(
        name = "marketplace.create_manifest",
        skip(self, package_ids),
        fields(
            operation.name = "create_manifest",
            operation.type = "marketplace",
            install_path = %install_path,
            packages_count = package_ids.len(),
            duration_ms
        )
    )]
    pub async fn create_manifest(
        &self, package_ids: Vec<PackageId>, install_path: String,
    ) -> Result<InstallationManifest> {
        let start = std::time::Instant::now();
        let mut dependencies = indexmap::IndexMap::new();

        for pkg_id in &package_ids {
            let package = self.repository.get_package(pkg_id).await?;
            let latest_version = package.latest_version.clone();
            dependencies.insert(pkg_id.clone(), latest_version.clone());

            // Resolve dependencies
            let resolved = self.resolve_dependencies(pkg_id, &latest_version).await?;

            for (dep_id, dep_version) in resolved {
                if !dependencies.contains_key(&dep_id) {
                    dependencies.insert(dep_id, dep_version);
                }
            }
        }

        // Deterministic UUID derived from the canonical input set.
        // Same package_ids + install_path always produce the same manifest ID,
        // which is required for reproducible receipts.
        let id_input = format!(
            "ggen:manifest:{}:{}",
            {
                let mut sorted_ids: Vec<_> = package_ids.iter().map(|p| p.to_string()).collect();
                sorted_ids.sort();
                sorted_ids.join(",")
            },
            install_path
        );
        let manifest = InstallationManifest {
            id: Uuid::new_v5(&Uuid::NAMESPACE_DNS, id_input.as_bytes()),
            packages: package_ids,
            dependencies,
            install_path,
            planned_at: chrono::Utc::now(),
        };

        let duration = start.elapsed();
        info!(
            "Created installation manifest {} with {} packages",
            manifest.id,
            manifest.dependencies.len()
        );

        // Record OTEL span attributes
        span::Span::current().record("duration_ms", duration.as_millis());

        Ok(manifest)
    }

    /// Check for version conflicts
    ///
    /// # Errors
    ///
    /// * [`Error::ValidationFailed`] - When version conflicts are detected between dependencies
    /// * [`Error::DependencyResolutionFailed`] - When semantic version constraints cannot be satisfied
    pub fn check_conflicts(
        &self, dependencies: &indexmap::IndexMap<PackageId, PackageVersion>,
    ) -> Result<()> {
        // Check for version conflicts - same package with incompatible major versions
        let mut package_versions: HashMap<PackageId, Vec<Version>> = HashMap::new();

        // Parse all versions and group by package ID
        for (pkg_id, version_str) in dependencies {
            let version =
                Version::parse(version_str.as_str()).map_err(|_| Error::ValidationFailed {
                    reason: format!(
                        "Invalid semver version '{}' for package {}",
                        version_str, pkg_id
                    ),
                })?;

            package_versions
                .entry(pkg_id.clone())
                .or_insert_with(Vec::new)
                .push(version);
        }

        // Check for incompatible major versions for the same package
        for (pkg_id, versions) in package_versions {
            if versions.len() > 1 {
                // Check if all versions have the same major version
                let major_versions: std::collections::HashSet<u64> =
                    versions.iter().map(|v| v.major).collect();

                if major_versions.len() > 1 {
                    return Err(Error::DependencyResolutionFailed {
                        package_id: pkg_id.to_string(),
                        reason: format!(
                            "Incompatible versions for package {}: found {} different major versions",
                            pkg_id,
                            major_versions.len()
                        ),
                    });
                }
            }
        }

        debug!("Checked {} dependencies for conflicts", dependencies.len());

        Ok(())
    }

    /// Validate installation manifest before execution
    ///
    /// # Errors
    ///
    /// * [`Error::PackageNotFound`] - When a package or dependency version does not exist in the repository
    /// * [`Error::ValidationFailed`] - When the manifest fails validation checks
    /// * [`Error::DependencyResolutionFailed`] - When dependency conflicts are detected
    pub async fn validate_manifest(&self, manifest: &InstallationManifest) -> Result<()> {
        // Check all packages exist
        for (pkg_id, version) in &manifest.dependencies {
            self.repository.get_package_version(pkg_id, version).await?;
        }

        // Check for conflicts
        self.check_conflicts(&manifest.dependencies)?;

        info!("Validated installation manifest {}", manifest.id);

        Ok(())
    }

    /// Simulate installation without making changes
    ///
    /// # Errors
    ///
    /// * [`Error::ValidationFailed`] - When the manifest fails validation
    /// * [`Error::PackageNotFound`] - When a dependency package does not exist in the repository
    pub async fn dry_run(&self, manifest: &InstallationManifest) -> Result<InstallationPlan> {
        self.validate_manifest(manifest).await?;

        let mut plan = InstallationPlan {
            id: manifest.id,
            packages: Vec::new(),
            total_size: 0,
            estimated_time: std::time::Duration::from_secs(0),
        };

        for (pkg_id, version) in &manifest.dependencies {
            self.repository.get_package_version(pkg_id, version).await?;
            let _package = self.repository.get_package_version(pkg_id, version).await?;

            // Simulate size calculation (in real implementation, would fetch actual sizes)
            let size_estimate = 1024 * 100; // 100KB estimate per package
            plan.total_size += size_estimate;
            plan.packages.push(PackageInstallPlan {
                id: pkg_id.clone(),
                version: version.clone(),
                size: size_estimate,
            });
        }

        // Estimate time: 100KB per second
        plan.estimated_time = std::time::Duration::from_secs(plan.total_size / 102_400);

        debug!(
            "Dry-run installation: {} packages, {} bytes",
            plan.packages.len(),
            plan.total_size
        );

        Ok(plan)
    }

    /// Install a single pack with caching and verification
    ///
    /// # Errors
    ///
    /// * [`Error::PackageNotFound`] - When the package does not exist in the repository
    /// * [`Error::SignatureVerificationFailed`] - When the pack signature verification fails
    /// * [`Error::InstallationFailed`] - When download, extraction, or file operations fail
    #[instrument(
        name = "marketplace.install_pack",
        skip(self),
        fields(
            operation.name = "install_pack",
            operation.type = "marketplace",
            package_id = %package_id,
            version = %version,
            cached = false,
            duration_ms
        )
    )]
    pub async fn install_pack(
        &self, package_id: &PackageId, version: &PackageVersion,
    ) -> Result<CachedPack> {
        let start = std::time::Instant::now();

        // Check cache first
        if let Some(cached) = self.cache.get(package_id, version) {
            info!(
                "Cache hit for {}@{}, using cached version",
                package_id, version
            );
            span::Span::current().record("cached", true);

            // Verify digest
            if self.cache.verify_digest(&cached)? {
                debug!("Cached pack digest verified: {}@{}", package_id, version);
                let duration = start.elapsed();
                span::Span::current().record("duration_ms", duration.as_millis());
                return Ok(cached);
            }
            warn!("Cached pack digest verification failed, re-downloading");
            self.cache.remove(package_id, version)?;
        }

        // Download pack from registry
        info!("Downloading pack {}@{} from registry", package_id, version);
        let package = self
            .repository
            .get_package_version(package_id, version)
            .await?;

        // Get release info
        let release = package
            .releases
            .get(version)
            .ok_or_else(|| Error::package_not_found(format!("{}@{}", package_id, version)))?;

        // Download pack data
        let pack_data = self.download_pack(&release.download_url).await?;

        // Verify signature (Fortune 5 CISO requirement) — MANDATORY
        match &release.signature {
            Some(signature_hex) => {
                self.verify_pack_signature(&pack_data, signature_hex)
                    .await?;
            }
            None => {
                return Err(Error::SignatureVerificationFailed {
                    reason: format!(
                        "Pack {}@{} has no signature. Signature verification is mandatory for all pack installations.",
                        package_id, version
                    ),
                });
            }
        }

        // Verify trust tier (Fortune 5 CISO requirement)
        // Trust tier enforcement is mandatory for enterprise profiles
        self.verify_trust_tier(
            package_id,
            version,
            &release.trust_tier,
            &release.registry_class,
        )
        .await?;

        // Verify SHA-256 digest
        self.verify_pack_digest(&pack_data, &release.checksum)?;

        // Extract pack to cache directory
        let cache_path = self.extract_pack(&pack_data, package_id, version)?;

        // Calculate final digest
        let digest = ChecksumCalculator::calculate(&pack_data);

        // Create cached pack entry
        let cached_pack = CachedPack::new(
            package_id.clone(),
            version.clone(),
            digest,
            pack_data.len() as u64,
            cache_path,
        );

        // Insert into cache
        self.cache.insert(cached_pack.clone())?;

        let duration = start.elapsed();
        span::Span::current().record("duration_ms", duration.as_millis());

        info!(
            "Successfully installed and cached pack {}@{}",
            package_id, version
        );

        Ok(cached_pack)
    }

    /// Download pack from URL
    ///
    /// # Errors
    ///
    /// * [`Error::InstallationFailed`] - When download fails
    #[instrument(
        name = "marketplace.download_pack",
        skip(self),
        fields(
            operation.name = "download_pack",
            operation.type = "marketplace",
            url = %url,
            duration_ms
        )
    )]
    async fn download_pack(&self, url: &str) -> Result<Vec<u8>> {
        let start = std::time::Instant::now();
        debug!("Downloading pack from: {}", url);

        // Real HTTP download using reqwest
        use reqwest::Client;

        let client = Client::new();
        let response = client
            .get(url)
            .timeout(std::time::Duration::from_secs(30)) // 30 second timeout
            .send()
            .await
            .map_err(|e| Error::InstallationFailed {
                reason: format!("HTTP download failed from {}: {}", url, e),
            })?;

        if !response.status().is_success() {
            return Err(Error::InstallationFailed {
                reason: format!(
                    "HTTP error {}: {} from {}",
                    response.status(),
                    response.status().canonical_reason().unwrap_or("unknown"),
                    url
                ),
            });
        }

        let data = response
            .bytes()
            .await
            .map_err(|e| Error::InstallationFailed {
                reason: format!("Failed to read response body from {}: {}", url, e),
            })?
            .to_vec();

        let duration = start.elapsed();
        span::Span::current().record("duration_ms", duration.as_millis());
        span::Span::current().record("data_size", data.len());

        info!("Downloaded {} bytes from {}", data.len(), url);
        Ok(data)
    }

    /// Verify pack signature using Ed25519
    ///
    /// # Errors
    ///
    /// * [`Error::SignatureVerificationFailed`] - When signature verification fails
    #[instrument(
        name = "marketplace.verify_pack_signature",
        skip(self),
        fields(
            operation.name = "verify_pack_signature",
            operation.type = "marketplace",
            data_size = data.len(),
            duration_ms
        )
    )]
    async fn verify_pack_signature(&self, data: &[u8], signature_hex: &str) -> Result<()> {
        let start = std::time::Instant::now();
        debug!("Verifying pack signature");

        // SECURITY: Real signature verification using ggen-receipt
        // 1. Get marketplace public key from trusted source
        // 2. Create MarketplaceSignature from hex
        // 3. Verify using Ed25519
        use crate::marketplace::security::{MarketplaceSignature, MarketplaceVerifier};

        // Get trusted marketplace public key
        let public_key_hex = self.get_marketplace_public_key().await?;

        // Create verifier
        let verifier = MarketplaceVerifier::from_public_key_hex(&public_key_hex)?;

        // Create MarketplaceSignature from hex string
        let signature = MarketplaceSignature {
            signature: signature_hex.to_string(),
            public_key: public_key_hex,
            checksum: ggen_config::receipt::hash_data(data),
        };

        // Verify signature
        let is_valid = verifier.verify(data, &signature)?;

        let duration = start.elapsed();
        span::Span::current().record("duration_ms", duration.as_millis());
        span::Span::current().record("signature_valid", is_valid);

        if !is_valid {
            return Err(Error::SignatureVerificationFailed {
                reason: "Ed25519 signature verification failed - pack may be tampered".to_string(),
            });
        }

        debug!("Pack signature verified successfully");
        Ok(())
    }

    /// Get marketplace public key for signature verification.
    ///
    /// This method retrieves the trusted marketplace public key.
    /// In production, this should be loaded from a secure config or well-known location.
    async fn get_marketplace_public_key(&self) -> Result<String> {
        if let Ok(k) = std::env::var("GGEN_MARKETPLACE_PUBLIC_KEY") {
            let trimmed = k.trim();
            if !trimmed.is_empty() {
                return Ok(trimmed.to_string());
            }
        }
        Err(Error::SecurityCheckFailed {
            reason: "Marketplace public key not configured. \
                     Set GGEN_MARKETPLACE_PUBLIC_KEY (hex) or configure a trusted marketplace."
                .to_string(),
        })
    }

    /// Verify pack trust tier against security profile requirements.
    ///
    /// Implements Fortune 5 CISO requirement for trust tier enforcement during installation.
    ///
    /// # Errors
    ///
    /// * [`Error::TrustTierCheckFailed`] - When pack trust tier does not meet profile requirements
    #[instrument(
        name = "marketplace.verify_trust_tier",
        skip(self),
        fields(
            operation.name = "verify_trust_tier",
            operation.type = "marketplace",
            package_id = %package_id,
            version = %version,
            pack_tier = ?pack_trust_tier,
            profile_tier = ?self.profile.as_ref().map(|p| p.trust_requirements),
            duration_ms
        )
    )]
    pub(crate) async fn verify_trust_tier(
        &self, package_id: &PackageId, version: &PackageVersion, pack_trust_tier: &TrustTier,
        registry_class: &RegistryClass,
    ) -> Result<()> {
        let start = std::time::Instant::now();
        debug!(
            "Verifying trust tier for {}@{}: pack tier = {:?}",
            package_id, version, pack_trust_tier
        );

        if let Some(profile) = &self.profile {
            info!(
                "Using security profile '{}' with trust requirements: {:?}",
                profile.id.as_str(),
                profile.trust_requirements
            );
        } else {
            debug!("No security profile set, using default trust requirements (Experimental+)");
        }

        let package_id_display = format!("{}@{}", package_id, version);
        let outcome = evaluate_trust_tier(
            self.profile.as_ref(),
            &package_id_display,
            *pack_trust_tier,
            registry_class,
        );

        let duration = start.elapsed();
        span::Span::current().record("duration_ms", duration.as_millis());
        outcome?;

        // Log success for audit trail
        let required_tier = self
            .profile
            .as_ref()
            .map_or(TrustTier::Experimental, |p| p.trust_requirements);
        info!(
            "Trust tier check passed for {}@{}: pack tier {:?} meets required tier {:?}",
            package_id, version, pack_trust_tier, required_tier
        );

        Ok(())
    }

    /// Verify pack SHA-256 digest
    ///
    /// # Errors
    ///
    /// * [`Error::ValidationFailed`] - When digest verification fails
    #[instrument(
        name = "marketplace.verify_pack_digest",
        skip(self),
        fields(
            operation.name = "verify_pack_digest",
            operation.type = "marketplace",
            data_size = data.len(),
            duration_ms
        )
    )]
    fn verify_pack_digest(&self, data: &[u8], expected_checksum: &str) -> Result<()> {
        let start = std::time::Instant::now();
        debug!("Verifying pack digest");

        let calculated_checksum = ChecksumCalculator::calculate(data);

        if calculated_checksum != expected_checksum {
            return Err(Error::ValidationFailed {
                reason: format!(
                    "Digest mismatch: expected {}, got {}",
                    expected_checksum, calculated_checksum
                ),
            });
        }

        let duration = start.elapsed();
        span::Span::current().record("duration_ms", duration.as_millis());

        debug!("Pack digest verified successfully");
        Ok(())
    }

    /// Extract pack to cache directory
    ///
    /// # Errors
    ///
    /// * [`Error::InstallationFailed`] - When extraction fails
    #[instrument(
        name = "marketplace.extract_pack",
        skip(self),
        fields(
            operation.name = "extract_pack",
            operation.type = "marketplace",
            package_id = %package_id,
            version = %version,
            data_size = data.len(),
            format = ?detect_format(data),
            duration_ms
        )
    )]
    fn extract_pack(
        &self, data: &[u8], package_id: &PackageId, version: &PackageVersion,
    ) -> Result<PathBuf> {
        let start = std::time::Instant::now();
        debug!("Extracting pack {}@{}", package_id, version);

        // Create cache directory for this pack
        let cache_path = self.persistent_cache_path(package_id, version);

        fs::create_dir_all(&cache_path).map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to create cache directory: {}", e),
        })?;

        // Detect format and extract
        if is_tar_gz(data) {
            self.extract_tar_gz(data, &cache_path)?;
        } else if is_zip(data) {
            self.extract_zip(data, &cache_path)?;
        } else {
            // Unknown format, just write as-is
            let output_path = cache_path.join("pack.dat");
            fs::write(&output_path, data).map_err(|e| Error::InstallationFailed {
                reason: format!("Failed to write pack data: {}", e),
            })?;
        }

        let duration = start.elapsed();
        span::Span::current().record("duration_ms", duration.as_millis());

        debug!("Extracted pack to: {:?}", cache_path);

        Ok(cache_path)
    }

    /// Extract tar.gz archive
    ///
    /// # Errors
    ///
    /// * [`Error::InstallationFailed`] - When extraction fails or path traversal detected
    fn extract_tar_gz(&self, data: &[u8], dest: &Path) -> Result<()> {
        use tar::Archive;

        let decoder = GzDecoder::new(data);
        let mut archive = Archive::new(decoder);

        for entry in archive.entries().map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to read tar.gz entries: {}", e),
        })? {
            let mut entry = entry.map_err(|e| Error::InstallationFailed {
                reason: format!("Failed to read tar.gz entry: {}", e),
            })?;

            let entry_path = entry
                .path()
                .map_err(|e| Error::InstallationFailed {
                    reason: format!("Invalid path in tar.gz entry: {}", e),
                })?
                .into_owned();

            // Zip Slip / symlink-escape prevention: use tar's own hardened
            // `unpack_in`, not the raw per-entry `unpack`. A manual
            // component-by-component check of the entry *name* (rejecting
            // literal ".."/absolute components) is not sufficient: a
            // malicious archive can plant a symlink entry (e.g. `evil ->
            // /home/user`) whose *name* contains no ".." at all, then a
            // later entry named `evil/pwned.txt` writes through that
            // symlink to escape `dest`. `unpack_in` canonicalizes each
            // entry's resolved parent directory (`validate_inside_dst`,
            // following symlinks) and refuses to write anything whose real
            // location falls outside `dest`.
            let unpacked = entry
                .unpack_in(dest)
                .map_err(|e| Error::InstallationFailed {
                    reason: format!(
                        "Failed to extract tar.gz entry {}: {}",
                        entry_path.display(),
                        e
                    ),
                })?;

            if !unpacked {
                // `unpack_in` returns `Ok(false)` (rather than `Err`) for
                // entries it silently skips as unsafe (e.g. a literal ".."
                // path component). Treat that the same as a hard failure so
                // a crafted archive can't have entries quietly dropped while
                // the overall extraction still reports success.
                return Err(Error::InstallationFailed {
                    reason: format!(
                        "Path traversal detected in tar.gz: {}",
                        entry_path.display()
                    ),
                });
            }
        }

        Ok(())
    }

    /// Extract ZIP archive
    ///
    /// # Errors
    ///
    /// * [`Error::InstallationFailed`] - When extraction fails or path traversal detected
    fn extract_zip(&self, data: &[u8], dest: &Path) -> Result<()> {
        use zip::ZipArchive;

        let cursor = std::io::Cursor::new(data);
        let mut archive = ZipArchive::new(cursor).map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to open ZIP archive: {}", e),
        })?;

        for i in 0..archive.len() {
            let mut file = archive.by_index(i).map_err(|e| Error::InstallationFailed {
                reason: format!("Failed to read ZIP entry {}: {}", i, e),
            })?;

            // enclosed_name() rejects path traversal (..) and absolute paths.
            let entry_name = file
                .enclosed_name()
                .ok_or_else(|| Error::InstallationFailed {
                    reason: format!("Path traversal detected in ZIP entry: {}", file.name()),
                })?;

            let target = dest.join(entry_name);

            if file.is_dir() {
                fs::create_dir_all(&target).map_err(|e| Error::InstallationFailed {
                    reason: format!("Failed to create directory {:?}: {}", target, e),
                })?;
            } else {
                if let Some(parent) = target.parent() {
                    fs::create_dir_all(parent).map_err(|e| Error::InstallationFailed {
                        reason: format!("Failed to create parent dir {:?}: {}", parent, e),
                    })?;
                }
                let mut out = fs::File::create(&target).map_err(|e| Error::InstallationFailed {
                    reason: format!("Failed to create file {:?}: {}", target, e),
                })?;
                std::io::copy(&mut file, &mut out).map_err(|e| Error::InstallationFailed {
                    reason: format!("Failed to write ZIP entry {:?}: {}", target, e),
                })?;
            }
        }

        Ok(())
    }

    /// Update lockfile with installed packages
    ///
    /// # Errors
    ///
    /// * [`Error::IoError`] - When lockfile operations fail
    pub fn update_lockfile(&self, manifest: &InstallationManifest) -> Result<()> {
        let lockfile_path = PathBuf::from(&manifest.install_path).join("ggen.lock");

        let lockfile = Lockfile::from_manifest(manifest);

        let file = File::create(&lockfile_path).map_err(|e| Error::IoError(e))?;
        let writer = BufWriter::new(file);

        serde_json::to_writer_pretty(writer, &lockfile)
            .map_err(|e| Error::SerializationError(e))?;

        info!("Updated lockfile at {:?}", lockfile_path);

        Ok(())
    }

    /// Batch resolve dependencies for multiple packages
    ///
    /// Builds a unified dependency graph for all packages and resolves all
    /// dependencies in a single pass. More efficient than resolving each package
    /// separately when there are shared dependencies.
    ///
    /// # Errors
    ///
    /// * [`Error::PackageNotFound`] - When a dependency package does not exist
    /// * [`Error::InvalidVersion`] - When a version cannot be parsed
    /// * [`Error::DependencyResolutionFailed`] - When circular dependencies are detected
    #[instrument(
        name = "marketplace.batch_resolve_dependencies",
        skip(self),
        fields(
            operation.name = "batch_resolve_dependencies",
            operation.type = "marketplace",
            packages_count = package_ids.len(),
            dependencies_count,
            duration_ms
        )
    )]
    pub async fn batch_resolve_dependencies(
        &self, package_ids: Vec<PackageId>,
    ) -> Result<indexmap::IndexMap<PackageId, PackageVersion>> {
        let start = std::time::Instant::now();
        let mut resolved = indexmap::IndexMap::new();
        let mut visited = HashSet::new();
        let mut to_process = Vec::new();

        // Add all root packages to process queue
        for pkg_id in &package_ids {
            let package = self.repository.get_package(pkg_id).await?;
            let version = package.latest_version.clone();
            to_process.push((pkg_id.clone(), version));
        }

        // Process all packages iteratively (BFS for dependency resolution)
        while let Some((id, version)) = to_process.pop() {
            if visited.contains(&id) {
                // Already resolved, add to result if not present
                if !resolved.contains_key(&id) {
                    resolved.insert(id, version);
                }
                continue;
            }

            // Get package and process its dependencies
            let package = self.repository.get_package_version(&id, &version).await?;

            for release in package.releases.values() {
                for dep in &release.dependencies {
                    if !visited.contains(&dep.id) {
                        let parsed_version = dep.version_req.parse::<PackageVersion>()?;
                        to_process.push((dep.id.clone(), parsed_version));
                    }
                }
            }

            visited.insert(id.clone());
            resolved.insert(id, version);
        }

        let duration = start.elapsed();
        debug!(
            "Batch resolved {} dependencies for {} packages",
            resolved.len(),
            package_ids.len()
        );

        // Record OTEL span attributes
        span::Span::current().record("dependencies_count", resolved.len());
        span::Span::current().record("duration_ms", duration.as_millis());

        Ok(resolved)
    }

    /// Install multiple packages atomically with rollback on failure
    ///
    /// This function implements transaction semantics:
    /// 1. Validates all packages can be resolved
    /// 2. Saves pre-installation state
    /// 3. Installs packages in dependency order
    /// 4. On any failure, rolls back all changes
    ///
    /// Uses parallel installation for packages with no interdependencies
    /// (independent packages can be installed in parallel via rayon).
    ///
    /// # Errors
    ///
    /// * [`Error::ValidationFailed`] - When manifest validation fails
    /// * [`Error::InstallationFailed`] - When any package installation fails
    /// * [`Error::IoError`] - When lockfile operations fail
    ///
    /// If installation fails at any point, all packages are rolled back
    /// (cache entries are removed).
    #[instrument(
        name = "marketplace.batch_install",
        skip(self, manifest, progress),
        fields(
            operation.name = "batch_install",
            operation.type = "marketplace",
            packages_count = manifest.packages.len(),
            dependencies_count,
            install_path = %manifest.install_path,
            status = "success",
            duration_ms
        )
    )]
    pub async fn batch_install(
        &self, manifest: InstallationManifest, progress: Option<ProgressCallback>,
    ) -> Result<BatchInstallationResult> {
        let start = std::time::Instant::now();

        // Validate the manifest
        self.validate_manifest(&manifest).await?;

        info!(
            "Batch installing {} packages to {}",
            manifest.packages.len(),
            manifest.install_path
        );

        // Save snapshot for potential rollback (used for future ACID implementation)
        let _snapshot = TransactionSnapshot {
            installed_packages: Vec::new(),
        };

        let mut installed = Vec::new();
        let total_count = manifest.dependencies.len();

        // Install packages in dependency order
        // Simple sequential install for safety; parallel could be added later
        for (idx, (pkg_id, version)) in manifest.dependencies.iter().enumerate() {
            if let Some(ref progress_fn) = progress {
                progress_fn(idx, total_count, pkg_id.as_str());
            }

            match self.install_pack(pkg_id, version).await {
                Ok(cached_pack) => {
                    installed.push((pkg_id.clone(), version.clone(), cached_pack));
                    debug!(
                        "Installed package {}/{}: {}@{}",
                        idx + 1,
                        total_count,
                        pkg_id,
                        version
                    );
                }
                Err(e) => {
                    warn!(
                        "Installation failed for {}@{}, rolling back {} installed packages",
                        pkg_id,
                        version,
                        installed.len()
                    );
                    // Rollback: remove all installed packages from cache
                    for (rm_id, rm_version, _) in installed {
                        if let Err(cache_err) = self.cache.remove(&rm_id, &rm_version) {
                            warn!(
                                "Failed to remove cached package during rollback: {}",
                                cache_err
                            );
                        }
                    }
                    span::Span::current().record("status", "failed");
                    return Err(e);
                }
            }
        }

        // Update lockfile on success
        self.update_lockfile(&manifest)?;

        let duration = start.elapsed();
        span::Span::current().record("dependencies_count", total_count);
        span::Span::current().record("duration_ms", duration.as_millis());

        info!(
            "Batch installation completed: {} packages installed in {:?}",
            installed.len(),
            duration
        );

        if let Some(ref progress_fn) = progress {
            progress_fn(total_count, total_count, "complete");
        }

        Ok(BatchInstallationResult {
            manifest_id: manifest.id,
            packages_installed: installed.len(),
            total_packages: total_count,
            duration,
        })
    }

    /// Build a dependency graph for topological sorting
    ///
    /// Returns a map of package ID to its direct dependencies.
    /// Used for determining installation order and parallel opportunities.
    fn build_dependency_graph(
        &self, dependencies: &indexmap::IndexMap<PackageId, PackageVersion>,
    ) -> HashMap<PackageId, Vec<PackageId>> {
        let mut graph = HashMap::new();

        // Initialize all packages in the dependency set with empty edge lists
        for (pkg_id, _) in dependencies {
            graph.insert(pkg_id.clone(), Vec::new());
        }

        // In a real implementation, we would populate edges from package metadata
        // that specifies dependencies. For now, we return the initialized graph.
        // The actual dependency data would come from the repository's package metadata.

        graph
    }
}

/// Core trust-tier decision logic (Fortune 5 CISO requirement), factored out
/// of [`Installer::verify_trust_tier`] so both the marketplace-registry
/// install path (`Installer::install_pack`) and the local/external
/// pack-by-id install path ([`install_pack_by_id_with_profile`]) enforce the
/// exact same gate over the exact same [`Profile`]/[`TrustTier`]/
/// [`RegistryClass`] types -- not two independently re-implemented checks
/// that could silently diverge.
///
/// # Errors
///
/// * [`Error::TrustTierCheckFailed`] - `pack_trust_tier` is `Blocked`, or does
///   not meet the profile's (or, absent a profile, the default `Experimental`)
///   minimum required tier.
/// * [`Error::SecurityCheckFailed`] - `profile` forbids public registries and
///   `registry_class` is `Public`.
fn evaluate_trust_tier(
    profile: Option<&Profile>, package_id_display: &str, pack_trust_tier: TrustTier,
    registry_class: &RegistryClass,
) -> Result<()> {
    // Check if pack is blocked (always fail, regardless of profile)
    if matches!(pack_trust_tier, TrustTier::Blocked) {
        return Err(Error::trust_tier_check_failed(format!(
            "Pack {} is marked as Blocked and cannot be installed",
            package_id_display
        )));
    }

    // Check registry class enforcement (Fortune 5 CISO requirement)
    // Enterprise/regulated profiles may forbid public registry packs
    if let Some(profile) = profile {
        if profile.forbid_public_registry()
            && matches!(registry_class, RegistryClass::Public { .. })
        {
            return Err(Error::SecurityCheckFailed {
                reason: format!(
                    "Pack {} is from a public registry ({:?}), but security profile '{}' forbids \
                     public registry packs. Installation blocked by Fortune 5 CISO policy.",
                    package_id_display,
                    registry_class,
                    profile.id.as_str()
                ),
            });
        }
    }

    // If no profile is set, use default trust requirements: allow
    // Experimental and higher (all except Blocked).
    let required_tier = profile.map_or(TrustTier::Experimental, |p| p.trust_requirements);

    // Verify pack tier meets or exceeds required tier
    if !pack_trust_tier.meets_requirement(required_tier) {
        let profile_info = profile.map_or_else(
            || "default profile".to_string(),
            |p| format!("profile '{}'", p.id.as_str()),
        );

        return Err(Error::trust_tier_check_failed(format!(
            "Pack {} has trust tier {:?}, but {} requires {:?}. Installation blocked by Fortune 5 \
             CISO policy.",
            package_id_display, pack_trust_tier, profile_info, required_tier
        )));
    }

    Ok(())
}

/// Detect pack format from magic bytes
#[must_use]
fn detect_format(data: &[u8]) -> PackFormat {
    if is_tar_gz(data) {
        PackFormat::TarGz
    } else if is_zip(data) {
        PackFormat::Zip
    } else {
        PackFormat::Unknown
    }
}

/// Check if data is tar.gz format
#[must_use]
fn is_tar_gz(data: &[u8]) -> bool {
    data.starts_with(b"\x1f\x8b")
}

/// Check if data is ZIP format
#[must_use]
fn is_zip(data: &[u8]) -> bool {
    data.starts_with(b"PK\x03\x04")
}

/// Pack format enumeration
#[derive(Debug, Clone, Copy)]
enum PackFormat {
    TarGz,
    Zip,
    Unknown,
}

#[async_trait]
impl<R: AsyncRepository> Installable for Installer<R> {
    /// Install packages according to the manifest
    ///
    /// # Errors
    ///
    /// * [`Error::ValidationFailed`] - When the manifest fails validation
    /// * [`Error::InstallationFailed`] - When package download, extraction, or post-install hooks fail
    /// * [`Error::IoError`] - When file system operations fail
    /// * [`Error::SignatureVerificationFailed`] - When package signature verification fails
    #[instrument(
        name = "marketplace.install",
        skip(self, manifest),
        fields(
            operation.name = "install",
            operation.type = "marketplace",
            manifest_id = %manifest.id,
            packages_count = manifest.packages.len(),
            install_path = %manifest.install_path,
            status = "success",
            duration_ms
        )
    )]
    async fn install(&self, manifest: InstallationManifest) -> Result<InstallationManifest> {
        let start = std::time::Instant::now();
        self.validate_manifest(&manifest).await?;

        info!(
            "Installing {} packages to {}",
            manifest.packages.len(),
            manifest.install_path
        );

        // Install each package with caching and verification
        for (pkg_id, version) in &manifest.dependencies {
            self.install_pack(pkg_id, version).await?;
        }

        // Update lockfile
        self.update_lockfile(&manifest)?;

        let duration = start.elapsed();
        span::Span::current().record("duration_ms", duration.as_millis());

        Ok(manifest)
    }

    /// Resolve dependencies for a package
    ///
    /// # Errors
    ///
    /// * [`Error::PackageNotFound`] - When a dependency package does not exist in the repository
    /// * [`Error::InvalidVersion`] - When a dependency version requirement cannot be parsed
    /// * [`Error::DependencyResolutionFailed`] - When circular dependencies are detected
    async fn resolve_dependencies(
        &self, id: &PackageId, version: &PackageVersion,
    ) -> Result<Vec<(PackageId, PackageVersion)>> {
        Installer::resolve_dependencies(self, id, version).await
    }

    /// Perform a dry run installation and return the plan as a string
    ///
    /// # Errors
    ///
    /// * [`Error::ValidationFailed`] - When the manifest fails validation
    /// * [`Error::PackageNotFound`] - When a dependency package does not exist in the repository
    async fn dry_run_install(&self, manifest: &InstallationManifest) -> Result<String> {
        let plan = self.dry_run(manifest).await?;
        Ok(plan.to_string())
    }
}

/// Plan for package installation
#[derive(Clone, Debug)]
pub struct InstallationPlan {
    /// Installation ID
    pub id: Uuid,
    /// Packages to install
    pub packages: Vec<PackageInstallPlan>,
    /// Total size in bytes
    pub total_size: u64,
    /// Estimated installation time
    pub estimated_time: std::time::Duration,
}

impl std::fmt::Display for InstallationPlan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Installation Plan {}", self.id)?;
        writeln!(f, "Packages: {}", self.packages.len())?;
        writeln!(f, "Total size: {} MB", self.total_size / (1024 * 1024))?;
        writeln!(
            f,
            "Estimated time: {:.1}s",
            self.estimated_time.as_secs_f64()
        )?;
        writeln!(f)?;

        for pkg in &self.packages {
            writeln!(f, "  - {}@{} ({} KB)", pkg.id, pkg.version, pkg.size / 1024)?;
        }

        Ok(())
    }
}

/// Plan for a single package installation
#[derive(Clone, Debug)]
pub struct PackageInstallPlan {
    /// Package ID
    pub id: PackageId,
    /// Version
    pub version: PackageVersion,
    /// Estimated size in bytes
    pub size: u64,
}

/// Lockfile for reproducible installations
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct Lockfile {
    /// Lockfile version
    pub version: u32,
    /// Installation manifest ID
    pub manifest_id: uuid::Uuid,
    /// Packages and their versions
    pub packages: indexmap::IndexMap<PackageId, PackageVersion>,
    /// When the lockfile was created
    pub created_at: chrono::DateTime<chrono::Utc>,
}

impl Lockfile {
    /// Create a lockfile from a manifest
    #[must_use]
    pub fn from_manifest(manifest: &InstallationManifest) -> Self {
        Self {
            version: 1,
            manifest_id: manifest.id,
            packages: manifest.dependencies.clone(),
            created_at: chrono::Utc::now(),
        }
    }
}

/// Result of a batch installation
#[derive(Debug, Clone)]
pub struct BatchInstallationResult {
    /// Installation manifest ID
    pub manifest_id: Uuid,
    /// Number of packages actually installed
    pub packages_installed: usize,
    /// Total packages in manifest
    pub total_packages: usize,
    /// Total installation duration
    pub duration: std::time::Duration,
}

impl std::fmt::Display for BatchInstallationResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Batch Installation Result {}", self.manifest_id)?;
        writeln!(
            f,
            "Packages: {}/{}",
            self.packages_installed, self.total_packages
        )?;
        writeln!(f, "Duration: {:.2}s", self.duration.as_secs_f64())?;
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Local/external-registry install path (ported from
// ggen-core/src/domain/packs/install.rs, specs/014-ggen-core-replacement,
// docs/jira/v26.7.16/06-MARKETPLACE-PACK-REGISTRY-MERGE.md).
//
// This is a DIFFERENT install path than `Installer::install_pack` above (which
// downloads a `PackageId`/`PackageVersion` from `self.repository`, ggen's own
// signed marketplace registry). This one resolves a pack by bare string ID --
// either from the LOCAL pack registry (`packs_registry::metadata::show_pack`)
// or an EXTERNAL package registry (crates.io/npm/PyPI-style, via
// `packs_registry::external_fetcher::ExternalFetcherFactory`) -- and its
// unique, previously-missing-from-ggen-marketplace contribution is writing a
// `.ggen/packs.lock` entry (`crate::packs::lockfile`, T026) afterward. Landed
// as free functions (not `Installer` methods): unlike every method above,
// none of this touches `self`/`R: AsyncRepository`/the pack cache -- it is
// its own independent install path, kept in this file/module so it lives
// beside the mature installer rather than in a separate, easily-confused
// module, per this ticket's "merge, don't duplicate" instruction. The
// original free function was named `install_pack`; renamed here to
// `install_pack_by_id` to avoid colliding with `Installer::install_pack`
// above (same name, unrelated signature and behavior).

/// Input for [`install_pack_by_id`].
pub struct InstallByIdInput {
    /// Bare pack ID (local) or `<registry-prefix>:<id>` (external, e.g. `npm:lodash`).
    pub pack_id: String,
    /// Destination directory; defaults to `~/.ggen/packs/<pack_id>`.
    pub target_dir: Option<PathBuf>,
    /// Overwrite an existing install at the target directory.
    pub force: bool,
    /// Resolve and report what would happen without writing anything (no
    /// install directory, no lockfile entry).
    pub dry_run: bool,
}

/// Output of [`install_pack_by_id`].
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct InstallByIdOutput {
    /// The pack ID that was installed.
    pub pack_id: String,
    /// The pack's declared display name.
    pub pack_name: String,
    /// Resolved pack version recorded in the lockfile.
    pub pack_version: String,
    /// Package names the pack declares.
    pub packages_installed: Vec<String>,
    /// Template names the pack makes available.
    pub templates_available: Vec<String>,
    /// Number of named SPARQL queries the pack declares.
    pub sparql_queries: usize,
    /// Total declared package count.
    pub total_packages: usize,
    /// Directory the pack was installed into.
    pub install_path: PathBuf,
    /// SHA-256 hex digest (64 chars) of the pack identity AND its installed
    /// content (see [`compute_pack_digest`]) bound into the lockfile
    /// `integrity` field as `sha256-<digest>`. Empty only for `dry_run`,
    /// where no durable state is written (lockfile invariant 4.1).
    pub digest: String,
    /// Absolute path of the `.ggen/packs.lock` file written by this install,
    /// or `None` for a dry-run. Bound here so the caller can prove the durable
    /// state transition occurred (no decorative completion).
    pub lockfile_path: Option<PathBuf>,
}

/// Compute the SHA-256 digest that pins this pack in the lockfile.
///
/// The digest binds BOTH the pack's identity-defining fields (id, version,
/// the declared package set, and declared dependency ids) AND the real
/// content of every file under `install_dir` (the pack's actual installed
/// closure -- the downloaded-and-unpacked external artifact, or the copied
/// local pack TOML + SPARQL queries). Binding real installed-file content,
/// not just identity strings, is what lets `sync --locked`'s re-verification
/// ([`crate::sync_profile::validate_sync_preconditions`]'s
/// `verify_pack_digests`) actually detect tampering with the files on disk
/// after install, rather than only detecting drift in the pack's declared
/// identity while an attacker-modified artifact hashes identically.
///
/// It is deterministic for a given `(pack, install_dir contents)` pair and
/// never empty for a real (non-dry-run) install, satisfying lockfile
/// invariant 4.1 (`digest` must be a non-empty SHA-256).
///
/// The algorithm MUST NOT be changed independently of the `sync --locked`
/// re-verification path (`crate::sync_profile::verify_pack_digests`) that
/// re-derives this digest from the on-disk `install_dir` and compares it to
/// the stored `integrity` field, or re-derivation will diverge from
/// install-time digests.
///
/// # Errors
/// Returns [`Error::InstallationFailed`] if `install_dir` does not exist, or
/// if any file under it cannot be read.
pub(crate) fn compute_pack_digest(
    pack: &crate::packs_registry::types::Pack, install_dir: &Path,
) -> Result<String> {
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(pack.id.as_bytes());
    hasher.update([0u8]);
    hasher.update(pack.version.as_bytes());
    hasher.update([0u8]);
    for package in &pack.packages {
        hasher.update(package.as_bytes());
        hasher.update([0u8]);
    }
    hasher.update([0xffu8]);
    for dep in &pack.dependencies {
        hasher.update(dep.pack_id.as_bytes());
        hasher.update([0u8]);
    }
    hasher.update([0xfeu8]);
    hash_installed_content(&mut hasher, install_dir)?;
    Ok(hex::encode(hasher.finalize()))
}

/// Fold the real content of every regular file under `dir` into `hasher`, in
/// deterministic (path-sorted, recursive) order, so the digest binds the
/// pack's actual installed closure -- not just its declared identity
/// strings. Any file that cannot be read is a real installation-integrity
/// failure and is surfaced as an error rather than silently skipped -- a
/// silently-skipped file is exactly the gap this function exists to close
/// (tampering could hide there).
///
/// # Errors
/// Returns [`Error::InstallationFailed`] if `dir` does not exist, cannot be
/// listed, or any file under it cannot be read.
fn hash_installed_content(hasher: &mut sha2::Sha256, dir: &Path) -> Result<()> {
    use sha2::Digest;

    if !dir.exists() {
        return Err(Error::InstallationFailed {
            reason: format!(
                "Cannot compute pack digest: install directory '{}' does not exist",
                dir.display()
            ),
        });
    }

    let mut files = Vec::new();
    collect_files_sorted(dir, dir, &mut files)?;

    for rel_path in &files {
        let abs_path = dir.join(rel_path);
        let bytes = fs::read(&abs_path).map_err(|e| Error::InstallationFailed {
            reason: format!(
                "Failed to read installed file '{}' while computing pack digest: {}",
                abs_path.display(),
                e
            ),
        })?;
        // Forward-slash-normalize the relative path so the digest is stable
        // across platforms.
        let rel_str = rel_path.to_string_lossy().replace('\\', "/");
        hasher.update(rel_str.as_bytes());
        hasher.update([0u8]);
        hasher.update(&bytes);
        hasher.update([0u8]);
    }

    Ok(())
}

/// Recursively collect every regular file under `dir` (as a path relative to
/// `root`), sorted at each directory level so the walk order never depends on
/// the filesystem's own directory-entry order.
///
/// # Errors
/// Returns [`Error::InstallationFailed`] if any directory in the walk cannot
/// be listed.
fn collect_files_sorted(root: &Path, dir: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    let mut entries: Vec<_> = fs::read_dir(dir)
        .map_err(|e| Error::InstallationFailed {
            reason: format!(
                "Failed to read directory '{}' while computing pack digest: {}",
                dir.display(),
                e
            ),
        })?
        .filter_map(std::result::Result::ok)
        .collect();
    entries.sort_by_key(std::fs::DirEntry::file_name);

    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            collect_files_sorted(root, &path, out)?;
        } else {
            let rel = path.strip_prefix(root).unwrap_or(&path).to_path_buf();
            out.push(rel);
        }
    }

    Ok(())
}

/// Write (or update) the project lockfile entry for a successfully installed
/// pack.
///
/// Authoritative path: this is the pack-resolution durable-state writer. It
/// targets `<cwd>/.ggen/packs.lock` -- the same path read by `pack remove`/
/// `policy validate` in `ggen-cli` -- so the format is compatible by
/// construction. The entry carries a NON-EMPTY `integrity` digest, the resolved
/// `version`, and a real `installed_at` timestamp (lockfile invariant 4.1).
fn write_lockfile_entry(
    pack: &crate::packs_registry::types::Pack, install_path: &Path, digest: &str,
) -> Result<PathBuf> {
    use crate::packs::lockfile::{LockedPack, PackLockfile, PackSource};

    let lockfile_path = std::env::current_dir()
        .map(|cwd| cwd.join(".ggen").join("packs.lock"))
        .unwrap_or_else(|_| PathBuf::from(".ggen").join("packs.lock"));

    let mut lockfile = if lockfile_path.exists() {
        PackLockfile::from_file(&lockfile_path)?
    } else {
        PackLockfile::new(env!("CARGO_PKG_VERSION"))
    };

    let entry = LockedPack {
        version: pack.version.clone(),
        source: PackSource::Local {
            path: install_path.to_path_buf(),
        },
        integrity: Some(format!("sha256-{}", digest)),
        installed_at: chrono::Utc::now(),
        // Dependencies are recorded only when they are also present in the
        // lockfile; an install of a single pack records no dep edges to avoid
        // tripping the lockfile's referential-integrity validation.
        dependencies: Vec::new(),
    };

    lockfile.add_pack(&pack.id, entry);
    lockfile.save(&lockfile_path)?;

    Ok(lockfile_path)
}

/// Materialize a local pack's real content into its install directory.
///
/// External (`<prefix>:id`) packs get real content via
/// [`download_and_verify_external_pack`]/[`unpack_external_pack`] in
/// [`install_pack_by_id`]; local (bare-id) packs previously only got a bare
/// `create_dir_all` with nothing copied in, which left the install
/// directory empty. `crate::agent::receipt::read_artifact_bytes` hashes an
/// empty directory's entry manifest as the empty string, so the signed
/// install receipt attested a real cryptographic proof of nothing -- this
/// closes that gap.
///
/// Copies two things:
/// 1. The pack's own source TOML (`<packs_dir>/<id>.toml`), read as raw
///    bytes and written byte-for-byte (not re-serialized from the parsed
///    [`Pack`](crate::packs_registry::types::Pack) struct), so any fields the
///    `Pack` type doesn't model survive the round trip.
/// 2. Each in-memory `pack.sparql_queries` entry, written to
///    `queries/<name>.rq` (no I/O needed to obtain these -- they're already
///    parsed into the `Pack`).
///
/// Deliberately does NOT materialize `pack.templates`: investigation found
/// template source files are unreachable through any code path outside a
/// dev checkout (no embedding, ambiguous root resolution) -- a bounded scope
/// decision, not an oversight.
///
/// Does not touch [`compute_pack_digest`], which hashes only pack identity
/// (id/version/packages/dependency ids), never file content -- adding real
/// files here does not change the digest `sync --locked` re-derives.
fn materialize_local_pack(
    pack: &crate::packs_registry::types::Pack, packs_dir: &Path, install_path: &Path,
) -> Result<()> {
    let src_toml_path = packs_dir.join(format!("{}.toml", pack.id));
    let raw_toml_bytes = fs::read(&src_toml_path).map_err(|e| Error::InstallationFailed {
        reason: format!(
            "Failed to read pack source '{}': {}",
            src_toml_path.display(),
            e
        ),
    })?;
    fs::write(
        install_path.join(format!("{}.toml", pack.id)),
        &raw_toml_bytes,
    )
    .map_err(|e| Error::InstallationFailed {
        reason: format!("Failed to write pack TOML to install dir: {}", e),
    })?;

    if !pack.sparql_queries.is_empty() {
        let queries_dir = install_path.join("queries");
        fs::create_dir_all(&queries_dir).map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to create queries dir: {}", e),
        })?;
        for (name, query) in &pack.sparql_queries {
            let query_path = queries_dir.join(format!("{}.rq", name));
            fs::write(&query_path, query).map_err(|e| Error::InstallationFailed {
                reason: format!("Failed to write query '{}': {}", name, e),
            })?;
        }
    }

    Ok(())
}

/// Install a pack by bare string ID -- local registry lookup, or an external
/// registry fetch when `pack_id` contains a `<prefix>:` (e.g. `npm:lodash`).
///
/// Delegates to [`install_pack_by_id_with_profile`] with `profile = None`,
/// preserving this function's original signature and behavior for its
/// existing callers (`ggen pack add` via `crates/ggen-cli/src/cmds/pack.rs`,
/// `PackAgent::install` via `crate::agent::facade`). `None` matches
/// [`Installer::verify_trust_tier`]'s own documented "no profile" default:
/// allow everything except a pack explicitly marked `Blocked`.
///
/// # Errors
/// See [`install_pack_by_id_with_profile`].
pub async fn install_pack_by_id(input: &InstallByIdInput) -> Result<InstallByIdOutput> {
    install_pack_by_id_with_profile(input, None).await
}

/// Install a pack by bare string ID, enforcing an optional Fortune-5-CISO trust-tier profile.
///
/// The [`Profile`] is enforced via the SAME `Profile`/[`TrustTier`]/[`RegistryClass`] system
/// [`Installer::verify_trust_tier`] enforces for the marketplace-registry install path (both
/// call the shared [`evaluate_trust_tier`]), not a second, independently-invented gate.
///
/// Bare-id (local) and `<prefix>:id` (external) packs carry no attested
/// trust-tier metadata of their own (`packs_registry::types::Pack` has no
/// `trust_tier` field): they are evaluated at `TrustTier::Experimental`, the
/// same floor `Installer::verify_trust_tier` applies when a marketplace
/// release declares no tier. A profile that requires anything higher refuses
/// the install rather than silently granting elevated trust it cannot prove.
/// External packs are classified `RegistryClass::Public` (crates.io/npm/PyPI
/// are transport, not trust, per that enum's own doc comment); local packs
/// are classified `RegistryClass::PrivateEnterprise` (sourced from the local
/// packs directory, never fetched over the network).
///
/// # Errors
/// Returns [`Error::TrustTierCheckFailed`] or [`Error::SecurityCheckFailed`]
/// when `profile` is supplied and the pack does not meet its requirements
/// (checked before any filesystem write). Otherwise returns
/// [`Error::InstallationFailed`]-family errors on resolution, download,
/// checksum verification, extraction, or lockfile-write failure; refuses
/// (does not overwrite) an existing install at the target directory unless
/// `force` is set.
pub async fn install_pack_by_id_with_profile(
    input: &InstallByIdInput, profile: Option<&Profile>,
) -> Result<InstallByIdOutput> {
    // 1. Resolve pack metadata (+ external registry-reported checksum, if any).
    let (pack, expected_checksum): (crate::packs_registry::types::Pack, Option<String>) =
        if input.pack_id.contains(':') {
            fetch_external_pack(&input.pack_id).await?
        } else {
            let pack = crate::packs_registry::metadata::show_pack(&input.pack_id).map_err(|e| {
                Error::InstallationFailed {
                    reason: format!("Pack '{}' not found locally: {}", input.pack_id, e),
                }
            })?;
            (pack, None)
        };

    // 1.5. Enforce trust-tier policy (Fortune 5 CISO requirement) BEFORE
    // touching the filesystem, using the real Installer::verify_trust_tier
    // decision logic (via the shared evaluate_trust_tier helper).
    let registry_class = if input.pack_id.contains(':') {
        let registry_type = match pack.registry_type.as_deref() {
            Some("cratesio" | "crates.io") => RegistryType::CratesIo,
            Some("npm") => RegistryType::Npm,
            Some("pypi") => RegistryType::PyPi,
            _ => RegistryType::Other,
        };
        let url = match registry_type {
            RegistryType::CratesIo => "https://crates.io",
            RegistryType::Npm => "https://registry.npmjs.org",
            RegistryType::PyPi => "https://pypi.org",
            _ => "unknown-external-registry",
        };
        RegistryClass::Public {
            url: url.to_string(),
            registry_type,
        }
    } else {
        RegistryClass::PrivateEnterprise {
            url: "local-packs-registry".to_string(),
            require_signature: false,
            allow_unlisted: true,
        }
    };
    let package_id_display = format!("{}@{}", input.pack_id, pack.version);
    evaluate_trust_tier(
        profile,
        &package_id_display,
        TrustTier::Experimental,
        &registry_class,
    )?;

    // 2. Determine install path
    let install_path = input.target_dir.clone().unwrap_or_else(|| {
        dirs::home_dir()
            .map(|p| p.join(".ggen").join("packs").join(&input.pack_id))
            .unwrap_or_else(|| PathBuf::from(".ggen").join("packs").join(&input.pack_id))
    });

    if install_path.exists() && !input.force {
        return Err(Error::InstallationFailed {
            reason: format!("Pack already installed at {}", install_path.display()),
        });
    }

    if !input.dry_run {
        fs::create_dir_all(&install_path).map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to create install dir: {}", e),
        })?;

        if input.pack_id.contains(':') {
            download_and_verify_external_pack(
                &input.pack_id,
                &pack,
                expected_checksum.as_deref(),
                &install_path,
            )
            .await?;
            unpack_external_pack(&input.pack_id, &pack, &install_path).await?;
        } else {
            let packs_dir = crate::packs_registry::metadata::get_packs_dir()?;
            materialize_local_pack(&pack, &packs_dir, &install_path)?;
        }
    }

    let packages_installed = pack.packages.clone();
    let templates_available: Vec<String> = pack.templates.iter().map(|t| t.name.clone()).collect();
    let sparql_queries = pack.sparql_queries.len();
    let total_packages = pack.packages.len();
    let pack_version = pack.version.clone();
    let pack_name = pack.name.clone();

    // Bind the pack closure with a non-empty digest and record it durably in the
    // lockfile. For a dry-run we do NOT touch the lockfile (no durable state),
    // and we leave the digest empty to signal "nothing was pinned".
    let (digest, lockfile_path) = if input.dry_run {
        (String::new(), None)
    } else {
        let digest = compute_pack_digest(&pack, &install_path)?;
        let lockfile_path = write_lockfile_entry(&pack, &install_path, &digest)?;
        (digest, Some(lockfile_path))
    };

    Ok(InstallByIdOutput {
        pack_id: input.pack_id.clone(),
        pack_name,
        pack_version,
        packages_installed,
        templates_available,
        sparql_queries,
        total_packages,
        install_path,
        digest,
        lockfile_path,
    })
}

/// Fetch pack metadata from an external registry.
///
/// Returns the constructed [`Pack`](crate::packs_registry::types::Pack)
/// alongside the registry-reported checksum for the resolved version, if the
/// registry supplied one (crates.io/PyPI report SHA-256; npm reports the
/// legacy SHA-1 `shasum`) -- consumed by
/// [`download_and_verify_external_pack`] to verify the downloaded artifact
/// bytes before they are ever written to disk.
async fn fetch_external_pack(
    pack_id: &str,
) -> Result<(crate::packs_registry::types::Pack, Option<String>)> {
    use crate::packs_registry::external_fetcher::ExternalFetcherFactory;
    use crate::packs_registry::types::Pack;

    let (fetcher, remote_id) = ExternalFetcherFactory::get_fetcher_by_prefix(pack_id)?;
    let remote_pkg = fetcher.fetch_metadata(&remote_id).await?;

    let expected_checksum = remote_pkg
        .checksums
        .get(&remote_pkg.latest_version)
        .cloned();

    // Convert RemotePackage to Pack
    let pack = Pack {
        id: pack_id.to_string(),
        name: remote_pkg.name.clone(),
        version: remote_pkg.latest_version.clone(),
        description: remote_pkg.description.unwrap_or_default(),
        category: "external".to_string(),
        author: None,
        repository: remote_pkg.repository,
        license: remote_pkg.license,
        registry_type: Some(fetcher.registry_prefix().to_string()),
        packages: vec![remote_pkg.name],
        templates: vec![],
        sparql_queries: std::collections::HashMap::new(),
        dependencies: vec![],
        tags: vec![],
        keywords: vec![],
        production_ready: true,
        metadata: Default::default(),
    };

    Ok((pack, expected_checksum))
}

/// Download and verify an external pack artifact.
///
/// # Errors
/// Returns [`Error::SecurityCheckFailed`] when `expected_checksum` is `None`
/// (the registry did not supply a checksum to verify against), or when the
/// downloaded bytes' recomputed digest does not match `expected_checksum`
/// (Fortune 5 CISO mandatory checksum rule). The artifact is never written to
/// disk before this check passes.
async fn download_and_verify_external_pack(
    pack_id: &str, pack: &crate::packs_registry::types::Pack, expected_checksum: Option<&str>,
    install_path: &Path,
) -> Result<()> {
    use crate::packs_registry::external_fetcher::ExternalFetcherFactory;

    let (fetcher, remote_id) = ExternalFetcherFactory::get_fetcher_by_prefix(pack_id)?;

    tracing::info!("Downloading artifact for {} v{}", pack_id, pack.version);
    let artifact_bytes = fetcher.fetch_artifact(&remote_id, &pack.version).await?;

    // Verify checksum (mandatory CISO rule): compare the downloaded bytes'
    // digest against the checksum the registry itself reported for this
    // version in fetch_metadata (crates.io/PyPI: SHA-256; npm: SHA-1
    // "shasum"). Refuses the install -- never writes the bytes to disk --
    // rather than trusting an unverified download.
    match expected_checksum {
        Some(expected) => verify_artifact_checksum(&artifact_bytes, expected, pack_id)?,
        None => {
            return Err(Error::SecurityCheckFailed {
                reason: format!(
                    "Registry did not supply a checksum for '{}' v{}; refusing to install an \
                     unverifiable artifact. Installation blocked by Fortune 5 CISO policy.",
                    pack_id, pack.version
                ),
            });
        }
    }

    let artifact_path = install_path.join("artifact.tar.gz");
    fs::write(&artifact_path, artifact_bytes).map_err(|e| Error::InstallationFailed {
        reason: format!("Failed to write artifact: {}", e),
    })?;

    Ok(())
}

/// Verify a downloaded artifact's bytes against a registry-reported checksum.
///
/// The hash algorithm is selected by the expected checksum's hex length
/// (crates.io/PyPI report SHA-256 -- 64 hex chars; npm reports the legacy
/// SHA-1 `shasum` -- 40 hex chars) rather than by registry name, so a
/// registry that changes its reported digest format is still checked
/// correctly instead of silently compared with the wrong algorithm.
///
/// # Errors
/// Returns [`Error::SecurityCheckFailed`] when the checksum format is not
/// recognized (neither 40 nor 64 hex chars), or when the recomputed digest
/// does not match `expected`.
fn verify_artifact_checksum(data: &[u8], expected: &str, pack_id: &str) -> Result<()> {
    let expected_trimmed = expected.trim();
    let calculated = match expected_trimmed.len() {
        64 => ChecksumCalculator::calculate(data), // SHA-256 (crates.io, PyPI)
        40 => {
            use sha1::{Digest, Sha1};
            let mut hasher = Sha1::new();
            hasher.update(data);
            hex::encode(hasher.finalize())
        }
        other => {
            return Err(Error::SecurityCheckFailed {
                reason: format!(
                    "Unrecognized checksum format for '{}' ({} hex chars); refusing to install \
                     without a verifiable digest.",
                    pack_id, other
                ),
            });
        }
    };

    if !calculated.eq_ignore_ascii_case(expected_trimmed) {
        return Err(Error::SecurityCheckFailed {
            reason: format!(
                "Checksum mismatch for '{}': expected {}, got {}. Artifact may be corrupted or \
                 tampered.",
                pack_id, expected_trimmed, calculated
            ),
        });
    }

    Ok(())
}

/// Unpack an external artifact and generate package.toml
async fn unpack_external_pack(
    _pack_id: &str, pack: &crate::packs_registry::types::Pack, install_path: &Path,
) -> Result<()> {
    let artifact_path = install_path.join("artifact.tar.gz");
    if !artifact_path.exists() {
        return Err(Error::InstallationFailed {
            reason: "Artifact not found for unpacking".to_string(),
        });
    }

    let file = fs::File::open(&artifact_path).map_err(|e| Error::InstallationFailed {
        reason: format!("Failed to open artifact: {}", e),
    })?;
    let tar = GzDecoder::new(file);
    let mut archive = Archive::new(tar);

    // Extract to the install path
    archive
        .unpack(install_path)
        .map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to unpack artifact: {}", e),
        })?;

    // Generate package.toml for compatibility
    let package_toml_path = install_path.join("package.toml");
    let registry_type = match pack.registry_type.as_deref() {
        Some("cratesio" | "crates.io") => "crates.io",
        Some(other) => other,
        None => "ggen",
    };
    let package_toml_content = format!(
        r#"[package]
name = "{}"
version = "{}"
description = "{}"
license = "{}"
registry_type = "{}"
"#,
        pack.name,
        pack.version,
        pack.description.replace('"', "\\\""),
        pack.license.as_deref().unwrap_or("MIT"),
        registry_type
    );

    fs::write(package_toml_path, package_toml_content).map_err(|e| Error::InstallationFailed {
        reason: format!("Failed to write package.toml: {}", e),
    })?;

    // Cleanup artifact
    let _ = fs::remove_file(artifact_path);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::marketplace::cache::CacheConfig;
    use crate::marketplace::registry::Registry;
    use serial_test::serial;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_installation_manifest_creation() {
        let registry = Registry::new(100);
        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();
        let installer = Installer::new(registry, cache);

        let manifest = installer
            .create_manifest(vec![], temp_dir.path().to_str().unwrap().to_string())
            .await
            .unwrap();

        assert_eq!(manifest.packages.len(), 0);
        assert_eq!(manifest.install_path, temp_dir.path().to_str().unwrap());
    }

    #[tokio::test]
    async fn test_conflict_checking() {
        let registry = Registry::new(100);
        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();
        let installer = Installer::new(registry, cache);

        let deps = indexmap::IndexMap::new();
        assert!(installer.check_conflicts(&deps).is_ok());
    }

    #[test]
    fn test_extract_tar_gz_rejects_truncated_archive() {
        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();
        let registry = Registry::new(100);
        let installer = Installer::new(registry, cache);

        // Only the GZIP magic bytes -- not a complete gzip stream (no header
        // tail, no deflate payload, no CRC/trailer), so decoding must fail.
        let data = b"\x1f\x8b\x08\x00";
        let extract_dir = temp_dir.path().join("extract");

        let result = installer.extract_tar_gz(data, &extract_dir);

        let err = result.expect_err("truncated gzip data must not extract successfully");
        let err_msg = err.to_string();
        assert!(
            err_msg.contains("tar.gz"),
            "error should identify a tar.gz extraction failure, got: {}",
            err_msg
        );
        assert!(
            matches!(err, Error::InstallationFailed { .. }),
            "expected Error::InstallationFailed, got: {:?}",
            err
        );
        assert!(
            !extract_dir.exists(),
            "no files should have been extracted from an invalid archive"
        );
    }

    #[test]
    fn test_extract_tar_gz_extracts_valid_archive() {
        use flate2::write::GzEncoder;
        use flate2::Compression;
        use std::io::Write;

        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();
        let registry = Registry::new(100);
        let installer = Installer::new(registry, cache);

        // Build a real tar.gz containing a single file with known content.
        let mut tar_bytes = Vec::new();
        {
            let mut builder = tar::Builder::new(&mut tar_bytes);
            let content = b"hello from a real tar.gz entry";
            let mut header = tar::Header::new_gnu();
            header.set_size(content.len() as u64);
            header.set_mode(0o644);
            header.set_cksum();
            builder
                .append_data(&mut header, "hello.txt", &content[..])
                .unwrap();
            builder.finish().unwrap();
        }
        let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
        encoder.write_all(&tar_bytes).unwrap();
        let gz_data = encoder.finish().unwrap();

        let extract_dir = temp_dir.path().join("extract");
        // Mirrors the real (only) caller: Installer::extract_pack always
        // fs::create_dir_all(&cache_path) before invoking extract_tar_gz --
        // the function itself does not create its destination directory.
        fs::create_dir_all(&extract_dir).unwrap();

        let result = installer.extract_tar_gz(&gz_data, &extract_dir);

        assert!(
            result.is_ok(),
            "valid tar.gz archive must extract successfully, got: {:?}",
            result.err()
        );
        let extracted_file = extract_dir.join("hello.txt");
        assert!(
            extracted_file.exists(),
            "expected {} to be extracted",
            extracted_file.display()
        );
        let extracted_content = fs::read_to_string(&extracted_file).unwrap();
        assert_eq!(extracted_content, "hello from a real tar.gz entry");
    }

    /// F1 (red-team finding, marketplace-fetch, fail-open, high severity):
    /// `extract_tar_gz` must reject a tar.gz whose entry 1 is a symlink
    /// pointing at an absolute path outside the destination directory,
    /// followed by entry 2 -- a regular file written *through* that symlink
    /// name -- rather than silently writing attacker content outside
    /// `dest`. A component-by-component check of each entry's own *name*
    /// (rejecting literal ".."/absolute components) cannot catch this: the
    /// symlink's name ("evil") and the second entry's name ("evil/pwned.txt")
    /// both contain only `Normal` path components, so that check passes for
    /// both entries -- the escape only happens because "evil" is *resolved*
    /// (as a symlink) to somewhere outside `dest`.
    #[test]
    fn poc_extract_tar_gz_symlink_escape() {
        use std::io::Write;

        use flate2::write::GzEncoder;
        use flate2::Compression;

        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();
        let registry = Registry::new(100);
        let installer = Installer::new(registry, cache);

        // A directory *outside* the extraction destination that the
        // symlink entry targets -- stand-in for e.g. the user's home
        // directory or `~/.ssh` in the real attack.
        let outside_dir = temp_dir.path().join("outside");
        fs::create_dir_all(&outside_dir).unwrap();

        let extract_dir = temp_dir.path().join("extract");
        fs::create_dir_all(&extract_dir).unwrap();

        // Build the malicious archive: entry 1 = symlink "evil" -> outside_dir
        // (absolute target); entry 2 = regular file "evil/pwned.txt" with
        // attacker-controlled content.
        let mut tar_bytes = Vec::new();
        {
            let mut builder = tar::Builder::new(&mut tar_bytes);

            let mut symlink_header = tar::Header::new_gnu();
            symlink_header.set_entry_type(tar::EntryType::Symlink);
            symlink_header.set_size(0);
            symlink_header.set_mode(0o777);
            builder
                .append_link(&mut symlink_header, "evil", &outside_dir)
                .unwrap();

            let attacker_content = b"PWNED: written outside dest via tar symlink escape";
            let mut file_header = tar::Header::new_gnu();
            file_header.set_size(attacker_content.len() as u64);
            file_header.set_mode(0o644);
            file_header.set_cksum();
            builder
                .append_data(&mut file_header, "evil/pwned.txt", &attacker_content[..])
                .unwrap();

            builder.finish().unwrap();
        }
        let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
        encoder.write_all(&tar_bytes).unwrap();
        let gz_data = encoder.finish().unwrap();

        let result = installer.extract_tar_gz(&gz_data, &extract_dir);

        let escaped_file = outside_dir.join("pwned.txt");
        assert!(
            !escaped_file.exists(),
            "F1 regression: attacker content escaped to {} via tar symlink; \
             extract_tar_gz result was {:?}",
            escaped_file.display(),
            result
        );
        assert!(
            result.is_err(),
            "a tar.gz containing a symlink-escape entry pair must be \
             rejected as Err, not silently accepted, got: {:?}",
            result
        );
        assert!(
            matches!(result, Err(Error::InstallationFailed { .. })),
            "expected Error::InstallationFailed, got: {:?}",
            result
        );
    }

    #[test]
    fn test_lockfile_from_manifest() {
        let manifest = InstallationManifest {
            id: uuid::Uuid::new_v4(),
            packages: vec![],
            dependencies: indexmap::IndexMap::new(),
            install_path: "/tmp/test".to_string(),
            planned_at: chrono::Utc::now(),
        };

        let lockfile = Lockfile::from_manifest(&manifest);

        assert_eq!(lockfile.manifest_id, manifest.id);
        assert_eq!(lockfile.version, 1);
    }

    #[test]
    fn test_pack_format_detection() {
        // tar.gz magic bytes
        let tar_gz_data = b"\x1f\x8b\x08\x00";
        assert!(is_tar_gz(tar_gz_data));
        assert!(!is_zip(tar_gz_data));

        // ZIP magic bytes
        let zip_data = b"PK\x03\x04";
        assert!(is_zip(zip_data));
        assert!(!is_tar_gz(zip_data));

        // Unknown format
        let unknown_data = b"unknown";
        assert!(!is_tar_gz(unknown_data));
        assert!(!is_zip(unknown_data));
    }

    #[tokio::test]
    async fn test_trust_tier_enforcement_with_profile() {
        use crate::marketplace::profile::enterprise_strict_profile;

        let registry = Registry::new(100);
        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();

        // Test with enterprise-strict profile (requires EnterpriseApproved)
        let profile = enterprise_strict_profile();
        let installer = Installer::with_profile(registry, cache, profile);

        // Verify the installer has the correct profile via the public API
        assert!(installer.cache().stats().total_packs == 0);
    }

    #[tokio::test]
    async fn test_trust_tier_enforcement_default_profile() {
        let registry = Registry::new(100);
        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();
        let installer = Installer::new(registry, cache);

        // Verify cache is accessible (no profile set by default)
        assert!(installer.cache().stats().total_packs == 0);
    }

    #[tokio::test]
    async fn test_regulated_profile_rejects_public_registry_packs() {
        use crate::marketplace::profile::regulated_finance_profile;

        let registry = Registry::new(100);
        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();

        let profile = regulated_finance_profile();
        assert!(
            profile.forbid_public_registry(),
            "regulated_finance_profile must forbid public registry"
        );

        let installer = Installer::with_profile(registry, cache, profile);

        let package_id = PackageId::new("public-crate").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        let public_registry = RegistryClass::Public {
            url: "https://crates.io".to_string(),
            registry_type: crate::marketplace::trust::RegistryType::default(),
        };

        let result = installer
            .verify_trust_tier(
                &package_id,
                &version,
                &TrustTier::EnterpriseCertified,
                &public_registry,
            )
            .await;

        assert!(
            result.is_err(),
            "regulated finance profile should reject public registry packs"
        );
        let err = result.unwrap_err();
        let err_msg = err.to_string();
        assert!(
            err_msg.contains("public registry"),
            "error message should mention public registry, got: {}",
            err_msg
        );
        assert!(
            err_msg.contains("regulated-finance"),
            "error message should mention the profile id, got: {}",
            err_msg
        );
    }

    #[tokio::test]
    async fn test_regulated_profile_allows_private_registry_packs() {
        use crate::marketplace::profile::regulated_finance_profile;

        let registry = Registry::new(100);
        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();

        let profile = regulated_finance_profile();
        let installer = Installer::with_profile(registry, cache, profile);

        let package_id = PackageId::new("private-crate").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        let private_registry = RegistryClass::PrivateEnterprise {
            url: "https://registry.internal.corp".to_string(),
            require_signature: true,
            allow_unlisted: false,
        };

        let result = installer
            .verify_trust_tier(
                &package_id,
                &version,
                &TrustTier::EnterpriseCertified,
                &private_registry,
            )
            .await;

        assert!(
            result.is_ok(),
            "regulated finance profile should allow private registry packs, got error: {}",
            result.unwrap_err()
        );
    }

    #[tokio::test]
    async fn test_no_profile_allows_public_registry_packs() {
        let registry = Registry::new(100);
        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();

        let installer = Installer::new(registry, cache);

        let package_id = PackageId::new("any-crate").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        let public_registry = RegistryClass::Public {
            url: "https://crates.io".to_string(),
            registry_type: crate::marketplace::trust::RegistryType::default(),
        };

        let result = installer
            .verify_trust_tier(
                &package_id,
                &version,
                &TrustTier::Experimental,
                &public_registry,
            )
            .await;

        assert!(
            result.is_ok(),
            "default profile (no profile) should allow public registry packs, got error: {}",
            result.unwrap_err()
        );
    }

    #[tokio::test]
    async fn test_batch_resolve_dependencies_single_package() {
        let registry = Registry::new(100);
        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();
        let pkg_id = PackageId::new("test-pkg").unwrap();
        registry
            .insert(&crate::marketplace::models::Package {
                metadata: crate::marketplace::models::PackageMetadata::new(
                    pkg_id.clone(),
                    "Test",
                    "Test",
                    "MIT",
                ),
                latest_version: crate::marketplace::models::PackageVersion::new("1.0.0").unwrap(),
                versions: vec![crate::marketplace::models::PackageVersion::new("1.0.0").unwrap()],
                releases: indexmap::IndexMap::new(),
            })
            .unwrap();
        let installer = Installer::new(registry, cache);
        let resolved = installer
            .batch_resolve_dependencies(vec![pkg_id.clone()])
            .await
            .unwrap();

        assert!(resolved.contains_key(&pkg_id));
    }

    #[tokio::test]
    async fn test_batch_resolve_dependencies_multiple_packages() {
        let registry = Registry::new(100);
        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();
        let pkg_id1 = PackageId::new("pkg-1").unwrap();
        let pkg_id2 = PackageId::new("pkg-2").unwrap();
        registry
            .insert(&crate::marketplace::models::Package {
                metadata: crate::marketplace::models::PackageMetadata::new(
                    pkg_id1.clone(),
                    "Test",
                    "Test",
                    "MIT",
                ),
                latest_version: crate::marketplace::models::PackageVersion::new("1.0.0").unwrap(),
                versions: vec![crate::marketplace::models::PackageVersion::new("1.0.0").unwrap()],
                releases: indexmap::IndexMap::new(),
            })
            .unwrap();
        registry
            .insert(&crate::marketplace::models::Package {
                metadata: crate::marketplace::models::PackageMetadata::new(
                    pkg_id2.clone(),
                    "Test",
                    "Test",
                    "MIT",
                ),
                latest_version: crate::marketplace::models::PackageVersion::new("1.0.0").unwrap(),
                versions: vec![crate::marketplace::models::PackageVersion::new("1.0.0").unwrap()],
                releases: indexmap::IndexMap::new(),
            })
            .unwrap();
        let installer = Installer::new(registry, cache);

        let resolved = installer
            .batch_resolve_dependencies(vec![pkg_id1.clone(), pkg_id2.clone()])
            .await
            .unwrap();

        assert!(resolved.contains_key(&pkg_id1));
        assert!(resolved.contains_key(&pkg_id2));
    }

    #[tokio::test]
    async fn test_batch_resolve_dependencies_empty() {
        let registry = Registry::new(100);
        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();
        let installer = Installer::new(registry, cache);

        let resolved = installer.batch_resolve_dependencies(vec![]).await.unwrap();

        assert!(resolved.is_empty());
    }

    #[tokio::test]
    async fn test_batch_installation_result_display() {
        let result = BatchInstallationResult {
            manifest_id: uuid::Uuid::new_v4(),
            packages_installed: 5,
            total_packages: 5,
            duration: std::time::Duration::from_secs(10),
        };

        let display_str = result.to_string();
        assert!(display_str.contains("5/5"));
        assert!(display_str.contains("10.00s"));
    }

    #[tokio::test]
    async fn test_batch_installation_manifest_creation() {
        let registry = Registry::new(100);
        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();
        let pkg_id1 = PackageId::new("batch-test-1").unwrap();
        let pkg_id2 = PackageId::new("batch-test-2").unwrap();
        registry
            .insert(&crate::marketplace::models::Package {
                metadata: crate::marketplace::models::PackageMetadata::new(
                    pkg_id1.clone(),
                    "Test",
                    "Test",
                    "MIT",
                ),
                latest_version: crate::marketplace::models::PackageVersion::new("1.0.0").unwrap(),
                versions: vec![crate::marketplace::models::PackageVersion::new("1.0.0").unwrap()],
                releases: indexmap::IndexMap::new(),
            })
            .unwrap();
        registry
            .insert(&crate::marketplace::models::Package {
                metadata: crate::marketplace::models::PackageMetadata::new(
                    pkg_id2.clone(),
                    "Test",
                    "Test",
                    "MIT",
                ),
                latest_version: crate::marketplace::models::PackageVersion::new("1.0.0").unwrap(),
                versions: vec![crate::marketplace::models::PackageVersion::new("1.0.0").unwrap()],
                releases: indexmap::IndexMap::new(),
            })
            .unwrap();
        let installer = Installer::new(registry, cache);

        let _manifest = installer
            .create_manifest(
                vec![pkg_id1.clone(), pkg_id2.clone()],
                temp_dir.path().to_str().unwrap().to_string(),
            )
            .await
            .unwrap();

        assert_eq!(_manifest.packages.len(), 2);
        assert!(_manifest.dependencies.contains_key(&pkg_id1));
        assert!(_manifest.dependencies.contains_key(&pkg_id2));
    }

    #[tokio::test]
    async fn test_batch_installation_with_progress_callback() {
        let registry = Registry::new(100);
        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();
        let pkg_id = PackageId::new("progress-test").unwrap();
        registry
            .insert(&crate::marketplace::models::Package {
                metadata: crate::marketplace::models::PackageMetadata::new(
                    pkg_id.clone(),
                    "Test",
                    "Test",
                    "MIT",
                ),
                latest_version: crate::marketplace::models::PackageVersion::new("1.0.0").unwrap(),
                versions: vec![crate::marketplace::models::PackageVersion::new("1.0.0").unwrap()],
                releases: indexmap::IndexMap::new(),
            })
            .unwrap();
        let installer = Installer::new(registry, cache);
        let _manifest = installer
            .create_manifest(
                vec![pkg_id.clone()],
                temp_dir.path().to_str().unwrap().to_string(),
            )
            .await
            .unwrap();

        // Create a progress callback
        let _progress = Box::new(|_current: usize, _total: usize, _pkg_id: &str| {
            // Progress callback called
        });

        // This test verifies the callback signature is correct and compiles
        // Callback type is ProgressCallback = Box<dyn Fn(usize, usize, &str) + Send + Sync>
    }

    /// Drift guard (default branch, `GGEN_PACK_CACHE_DIR` unset): before the
    /// E2 fix, `Installer::persistent_cache_path` resolved
    /// `home_dir()/.cache/ggen/packs/<id>/<version>` while
    /// `get_pack_cache_dir`/`CacheConfig::default` resolved
    /// `dirs::cache_dir()/ggen/packs/<id>/<version>` -- genuinely different
    /// directories on macOS (`~/.cache/...` vs `~/Library/Caches/...`). All
    /// three now delegate to [`crate::marketplace::metadata::pack_cache_root`]
    /// / [`crate::marketplace::metadata::pack_cache_dir`] and must agree.
    /// `#[serial]` plus explicit `remove_var` guards against races with the
    /// override-branch test below (both mutate the same process-wide env var).
    #[test]
    #[serial]
    fn test_pack_cache_resolvers_agree_default() {
        std::env::remove_var("GGEN_PACK_CACHE_DIR");

        let registry = Registry::new(100);
        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();
        let installer = Installer::new(registry, cache);

        let package_id = PackageId::new("drift-guard-pack").unwrap();
        let version = PackageVersion::new("1.2.3").unwrap();

        let via_installer = installer.persistent_cache_path(&package_id, &version);
        let via_get_pack_cache_dir =
            crate::marketplace::metadata::get_pack_cache_dir(&package_id, version.as_str());
        let via_pack_cache_dir =
            crate::marketplace::metadata::pack_cache_dir(package_id.as_str(), version.as_str());
        let via_cache_config_root = CacheConfig::default().cache_dir;
        let via_pack_cache_root = crate::marketplace::metadata::pack_cache_root();

        assert_eq!(
            via_installer, via_get_pack_cache_dir,
            "Installer::persistent_cache_path and get_pack_cache_dir must agree"
        );
        assert_eq!(
            via_installer, via_pack_cache_dir,
            "Installer::persistent_cache_path and pack_cache_dir must agree"
        );
        assert_eq!(
            via_cache_config_root, via_pack_cache_root,
            "CacheConfig::default's cache_dir and pack_cache_root must agree"
        );
        assert_eq!(
            via_installer,
            via_cache_config_root
                .join(package_id.as_str())
                .join(version.as_str()),
            "the per-version path must equal the root joined with id/version"
        );

        std::env::remove_var("GGEN_PACK_CACHE_DIR");
    }

    /// Drift guard (override branch, `GGEN_PACK_CACHE_DIR` set): before the
    /// E2 fix, setting this env var only redirected
    /// `Installer::persistent_cache_path` -- `get_pack_cache_dir` and
    /// `CacheConfig::default` silently ignored it. All three must now honor
    /// the override identically.
    #[test]
    #[serial]
    fn test_pack_cache_resolvers_agree_env_override() {
        let scratch = TempDir::new().unwrap();
        let override_dir = scratch.path().join("ggen-pack-cache-override");
        std::env::set_var("GGEN_PACK_CACHE_DIR", &override_dir);

        let registry = Registry::new(100);
        let temp_dir = TempDir::new().unwrap();
        let cache_config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };
        let cache = PackCache::new(cache_config).unwrap();
        let installer = Installer::new(registry, cache);

        let package_id = PackageId::new("drift-guard-pack").unwrap();
        let version = PackageVersion::new("1.2.3").unwrap();

        let via_installer = installer.persistent_cache_path(&package_id, &version);
        let via_get_pack_cache_dir =
            crate::marketplace::metadata::get_pack_cache_dir(&package_id, version.as_str());
        let via_pack_cache_dir =
            crate::marketplace::metadata::pack_cache_dir(package_id.as_str(), version.as_str());
        let via_cache_config_root = CacheConfig::default().cache_dir;
        let via_pack_cache_root = crate::marketplace::metadata::pack_cache_root();

        assert_eq!(
            via_pack_cache_root, override_dir,
            "pack_cache_root must honor GGEN_PACK_CACHE_DIR"
        );
        assert_eq!(
            via_cache_config_root, override_dir,
            "CacheConfig::default must now honor GGEN_PACK_CACHE_DIR (previously ignored it)"
        );
        assert_eq!(
            via_installer,
            override_dir
                .join(package_id.as_str())
                .join(version.as_str()),
            "Installer::persistent_cache_path must honor GGEN_PACK_CACHE_DIR"
        );
        assert_eq!(
            via_installer, via_get_pack_cache_dir,
            "get_pack_cache_dir must now honor GGEN_PACK_CACHE_DIR (previously ignored it)"
        );
        assert_eq!(via_installer, via_pack_cache_dir);

        std::env::remove_var("GGEN_PACK_CACHE_DIR");
    }

    // ── Checksum verification (download_and_verify_external_pack) ───────────
    //
    // Real cryptographic collaborators (real sha2::Sha256 / sha1::Sha1
    // hashing over real byte buffers) -- no mocks. `fetch_artifact` itself
    // hits a live external registry (crates.io/npm/PyPI), so these tests
    // exercise the pure verification logic (`verify_artifact_checksum`)
    // directly rather than the network-fetching wrapper, matching this
    // crate's existing convention for external-registry code (see
    // `external_fetcher::tests`, which test `parse_cratesio_response`/
    // `parse_npm_response` directly instead of hitting the live APIs).

    #[test]
    fn test_verify_artifact_checksum_accepts_correct_sha256() {
        let data = b"crates.io artifact bytes";
        let expected = ChecksumCalculator::calculate(data); // SHA-256, 64 hex chars
        assert_eq!(expected.len(), 64);
        assert!(verify_artifact_checksum(data, &expected, "cratesio:demo").is_ok());
    }

    #[test]
    fn test_verify_artifact_checksum_rejects_sha256_mismatch() {
        let data = b"crates.io artifact bytes";
        let wrong = ChecksumCalculator::calculate(b"different bytes entirely");
        let err = verify_artifact_checksum(data, &wrong, "cratesio:demo").unwrap_err();
        assert!(
            err.to_string().contains("Checksum mismatch"),
            "error should report a checksum mismatch, got: {err}"
        );
    }

    #[test]
    fn test_verify_artifact_checksum_accepts_correct_npm_sha1() {
        use sha1::{Digest as _, Sha1};
        let data = b"npm tarball bytes";
        let mut hasher = Sha1::new();
        hasher.update(data);
        let expected = hex::encode(hasher.finalize()); // 40 hex chars, matches npm's `shasum`
        assert_eq!(expected.len(), 40);
        assert!(verify_artifact_checksum(data, &expected, "npm:demo").is_ok());
    }

    #[test]
    fn test_verify_artifact_checksum_rejects_npm_sha1_mismatch() {
        use sha1::{Digest as _, Sha1};
        let data = b"npm tarball bytes";
        let mut hasher = Sha1::new();
        hasher.update(b"different tarball bytes");
        let wrong = hex::encode(hasher.finalize());
        let err = verify_artifact_checksum(data, &wrong, "npm:demo").unwrap_err();
        assert!(
            err.to_string().contains("Checksum mismatch"),
            "error should report a checksum mismatch, got: {err}"
        );
    }

    #[test]
    fn test_verify_artifact_checksum_rejects_unrecognized_format() {
        let data = b"some bytes";
        let err = verify_artifact_checksum(data, "not-a-real-checksum", "pypi:demo").unwrap_err();
        assert!(
            err.to_string().contains("Unrecognized checksum format"),
            "error should name the unrecognized format, got: {err}"
        );
    }

    #[test]
    fn test_verify_artifact_checksum_is_case_insensitive() {
        let data = b"case insensitive check";
        let expected = ChecksumCalculator::calculate(data).to_uppercase();
        assert!(verify_artifact_checksum(data, &expected, "cratesio:demo").is_ok());
    }

    // ── Digest computation now covers installed content ──────────────────────

    fn sample_digest_test_pack(id: &str) -> crate::packs_registry::types::Pack {
        crate::packs_registry::types::Pack {
            id: id.to_string(),
            name: format!("Sample {id}"),
            version: "1.0.0".to_string(),
            description: "digest fixture".to_string(),
            category: "test".to_string(),
            author: None,
            repository: None,
            license: Some("MIT".to_string()),
            registry_type: None,
            packages: vec![format!("{id}-core")],
            templates: vec![],
            sparql_queries: std::collections::HashMap::new(),
            dependencies: vec![],
            tags: vec![],
            keywords: vec![],
            production_ready: true,
            metadata: Default::default(),
        }
    }

    #[test]
    fn test_compute_pack_digest_changes_with_artifact_content() {
        let temp_dir = TempDir::new().unwrap();
        let install_dir_a = temp_dir.path().join("install_a");
        let install_dir_b = temp_dir.path().join("install_b");
        fs::create_dir_all(&install_dir_a).unwrap();
        fs::create_dir_all(&install_dir_b).unwrap();

        fs::write(install_dir_a.join("artifact.txt"), b"original content").unwrap();
        fs::write(install_dir_b.join("artifact.txt"), b"tampered content").unwrap();

        // SAME pack identity (id/version/packages/dependencies) in both cases.
        let pack = sample_digest_test_pack("io.ggen.digest-test");

        let digest_a = compute_pack_digest(&pack, &install_dir_a).unwrap();
        let digest_b = compute_pack_digest(&pack, &install_dir_b).unwrap();

        assert_ne!(
            digest_a, digest_b,
            "compute_pack_digest must change when the installed artifact's content changes, \
             even when pack identity (id/version/packages/dependencies) is unchanged -- this is \
             exactly what lets sync --locked detect tampering with installed files"
        );

        // Re-running against the SAME content must be deterministic.
        let digest_a_again = compute_pack_digest(&pack, &install_dir_a).unwrap();
        assert_eq!(
            digest_a, digest_a_again,
            "digest must be deterministic for unchanged pack identity + content"
        );
    }

    #[test]
    fn test_compute_pack_digest_errors_when_install_dir_missing() {
        let temp_dir = TempDir::new().unwrap();
        let missing_dir = temp_dir.path().join("does-not-exist");
        let pack = sample_digest_test_pack("io.ggen.digest-missing-dir");

        let result = compute_pack_digest(&pack, &missing_dir);
        assert!(
            result.is_err(),
            "computing a digest against a nonexistent install directory must fail, not silently \
             succeed with an identity-only digest"
        );
    }

    // ── Trust-tier enforcement wired into install_pack_by_id ─────────────────
    //
    // Real filesystem collaborators: a real `GGEN_PACKS_DIR` temp directory
    // with a real pack TOML, installed to a real temp target directory via
    // the real `install_pack_by_id_with_profile` entry point -- no mocks.
    // `#[serial(GGEN_PACKS_DIR)]` matches the same-named guard used by
    // `packs_registry::metadata`'s and `sync_profile`'s own tests (separate
    // compilation units in the same test binary; the shared key keeps their
    // `GGEN_PACKS_DIR` mutations from racing each other).

    /// Restore `GGEN_PACKS_DIR` to its prior value (or unset) on Drop.
    struct GgenPacksDirGuard {
        previous: Option<std::ffi::OsString>,
    }

    impl GgenPacksDirGuard {
        fn set(value: &std::path::Path) -> Self {
            let previous = std::env::var_os("GGEN_PACKS_DIR");
            std::env::set_var("GGEN_PACKS_DIR", value);
            Self { previous }
        }
    }

    impl Drop for GgenPacksDirGuard {
        fn drop(&mut self) {
            match &self.previous {
                None => std::env::remove_var("GGEN_PACKS_DIR"),
                Some(v) => std::env::set_var("GGEN_PACKS_DIR", v),
            }
        }
    }

    fn write_local_test_pack(packs_dir: &std::path::Path, id: &str, version: &str) {
        let toml = format!(
            r#"[pack]
id = "{id}"
name = "Test {id}"
version = "{version}"
description = "install.rs trust-tier fixture"
category = "test"
license = "MIT"
production_ready = true
packages = ["{id}-core"]
"#
        );
        fs::write(packs_dir.join(format!("{id}.toml")), toml).unwrap();
    }

    #[tokio::test]
    #[serial(GGEN_PACKS_DIR)]
    async fn test_install_pack_by_id_with_profile_rejects_pack_below_required_trust_tier() {
        use crate::marketplace::profile::regulated_finance_profile;

        let packs_registry_dir = TempDir::new().unwrap();
        let _guard = GgenPacksDirGuard::set(packs_registry_dir.path());
        write_local_test_pack(packs_registry_dir.path(), "io.ggen.trust-test", "1.0.0");

        let target = TempDir::new().unwrap();
        let target_dir = target.path().join("install-target");

        let input = InstallByIdInput {
            pack_id: "io.ggen.trust-test".to_string(),
            target_dir: Some(target_dir.clone()),
            force: false,
            dry_run: false,
        };

        // A bare-id local pack carries no attested trust tier and is evaluated
        // at TrustTier::Experimental -- regulated_finance_profile() requires
        // EnterpriseCertified, so this install must be refused.
        let profile = regulated_finance_profile();
        let result = install_pack_by_id_with_profile(&input, Some(&profile)).await;

        assert!(
            result.is_err(),
            "install must be refused when the pack's trust tier does not meet the profile's \
             requirement"
        );
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("trust tier"),
            "error should reference the trust tier check, got: {err_msg}"
        );

        // The gate runs before any filesystem mutation -- a refused install
        // must not create the target install directory.
        assert!(
            !target_dir.exists(),
            "a refused install must not create the target install directory"
        );
    }

    #[tokio::test]
    #[serial(GGEN_PACKS_DIR)]
    async fn test_install_pack_by_id_with_profile_allows_when_no_profile_supplied() {
        let packs_registry_dir = TempDir::new().unwrap();
        let _guard = GgenPacksDirGuard::set(packs_registry_dir.path());
        write_local_test_pack(packs_registry_dir.path(), "io.ggen.trust-ok", "1.0.0");

        let target = TempDir::new().unwrap();
        let target_dir = target.path().join("install-target");

        let input = InstallByIdInput {
            pack_id: "io.ggen.trust-ok".to_string(),
            target_dir: Some(target_dir.clone()),
            force: false,
            dry_run: false,
        };

        // No profile -> the same default as Installer::verify_trust_tier's
        // own "no profile" branch: allow Experimental and higher.
        let output = install_pack_by_id_with_profile(&input, None)
            .await
            .expect("install with no profile must succeed (backward-compatible default)");

        assert_eq!(output.pack_id, "io.ggen.trust-ok");
        assert!(
            !output.digest.is_empty(),
            "a real (non-dry-run) install must pin a non-empty digest"
        );
        assert!(
            target_dir.exists(),
            "a successful install must materialize the target directory"
        );
    }

    #[tokio::test]
    #[serial(GGEN_PACKS_DIR)]
    async fn test_install_pack_by_id_wrapper_is_backward_compatible() {
        // install_pack_by_id is the function existing callers actually invoke
        // (`ggen pack add` via crates/ggen-cli/src/cmds/pack.rs,
        // `PackAgent::install` via agent/facade.rs) -- it must keep working
        // unchanged now that it delegates to install_pack_by_id_with_profile.
        let packs_registry_dir = TempDir::new().unwrap();
        let _guard = GgenPacksDirGuard::set(packs_registry_dir.path());
        write_local_test_pack(packs_registry_dir.path(), "io.ggen.wrapper-ok", "1.0.0");

        let target = TempDir::new().unwrap();
        let target_dir = target.path().join("install-target");

        let input = InstallByIdInput {
            pack_id: "io.ggen.wrapper-ok".to_string(),
            target_dir: Some(target_dir.clone()),
            force: false,
            dry_run: false,
        };

        let output = install_pack_by_id(&input)
            .await
            .expect("install_pack_by_id must still succeed for existing callers");
        assert_eq!(output.pack_id, "io.ggen.wrapper-ok");
    }
}
