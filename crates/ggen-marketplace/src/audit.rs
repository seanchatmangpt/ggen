//! Audit integration for package installations
//!
//! Features:
//! - Create InstallationRecord structs
//! - Log to ggen audit trail with OTEL spans
//! - Record: crate, version, FMEA status, guards, deps
//! - Enable audit queries

use crate::error::Result;
use crate::install_validator::{InstallValidationResult, PokaYokeGuard, GuardResult};
use crate::models::{PackageId, PackageVersion};
use crate::resolver::ResolvedDependency;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use tracing::{debug, info, span, Level};
use uuid::Uuid;

/// Installation audit record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstallationRecord {
    /// Unique record ID
    pub id: Uuid,
    /// Installation timestamp
    pub timestamp: DateTime<Utc>,
    /// Package ID
    pub package_id: PackageId,
    /// Package version
    pub version: PackageVersion,
    /// Installation path
    pub install_path: PathBuf,
    /// Installation status
    pub status: InstallationStatus,
    /// FMEA validation status
    pub fmea_status: FmeaAuditStatus,
    /// Applied poka-yoke guards
    pub guards: Vec<GuardAuditEntry>,
    /// Dependencies installed
    pub dependencies: Vec<DependencyAuditEntry>,
    /// Duration in milliseconds
    pub duration_ms: u64,
    /// User who initiated the installation
    pub initiated_by: String,
    /// Additional metadata
    pub metadata: BTreeMap<String, String>,
}

/// Installation status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum InstallationStatus {
    /// Installation succeeded
    Success,
    /// Installation failed
    Failed,
    /// Installation was rolled back
    RolledBack,
    /// Installation is pending
    Pending,
    /// Installation was cancelled
    Cancelled,
}

impl std::fmt::Display for InstallationStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Success => write!(f, "success"),
            Self::Failed => write!(f, "failed"),
            Self::RolledBack => write!(f, "rolled_back"),
            Self::Pending => write!(f, "pending"),
            Self::Cancelled => write!(f, "cancelled"),
        }
    }
}

/// FMEA validation audit status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaAuditStatus {
    /// Whether FMEA validation passed
    pub passed: bool,
    /// Whether --force-fmea was used
    pub force_applied: bool,
    /// Number of critical failures
    pub critical_failures: usize,
    /// Total Risk Priority Number
    pub total_rpn: u32,
    /// Maximum allowed RPN
    pub max_rpn: u32,
    /// Validation warnings
    pub warnings: Vec<String>,
    /// Validation errors
    pub errors: Vec<String>,
}

impl From<&InstallValidationResult> for FmeaAuditStatus {
    fn from(result: &InstallValidationResult) -> Self {
        Self {
            passed: result.passed,
            force_applied: result.force_applied,
            critical_failures: result.critical_failures,
            total_rpn: result.total_rpn,
            max_rpn: 200, // Default threshold
            warnings: result.warnings.clone(),
            errors: result.errors.clone(),
        }
    }
}

/// Guard audit entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GuardAuditEntry {
    /// Guard type name
    pub guard_type: String,
    /// Whether the guard passed
    pub passed: bool,
    /// Guard message
    pub message: String,
    /// Guard configuration (serialized)
    pub config: String,
}

impl GuardAuditEntry {
    pub fn from_guard_result(guard: &PokaYokeGuard, result: &GuardResult) -> Self {
        let guard_type = match guard {
            PokaYokeGuard::DirectorySeparation { .. } => "DirectorySeparation",
            PokaYokeGuard::TraitBoundaries { .. } => "TraitBoundaries",
            PokaYokeGuard::PathProtection { .. } => "PathProtection",
            PokaYokeGuard::VersionConstraints { .. } => "VersionConstraints",
            PokaYokeGuard::ChecksumValidation { .. } => "ChecksumValidation",
            PokaYokeGuard::SandboxIsolation { .. } => "SandboxIsolation",
        };

        Self {
            guard_type: guard_type.to_string(),
            passed: result.passed,
            message: result.message.clone(),
            config: serde_json::to_string(guard).unwrap_or_default(),
        }
    }
}

/// Dependency audit entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyAuditEntry {
    /// Package ID
    pub package_id: PackageId,
    /// Version
    pub version: PackageVersion,
    /// Checksum
    pub checksum: String,
    /// Installation depth (0 = root)
    pub depth: usize,
    /// Whether this is a direct dependency
    pub direct: bool,
}

impl From<&ResolvedDependency> for DependencyAuditEntry {
    fn from(dep: &ResolvedDependency) -> Self {
        Self {
            package_id: dep.id.clone(),
            version: dep.version.clone(),
            checksum: dep.checksum.clone(),
            depth: dep.depth,
            direct: dep.depth == 0,
        }
    }
}

/// Audit trail for installations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditTrail {
    /// Audit trail version
    pub version: u32,
    /// All installation records
    pub records: Vec<InstallationRecord>,
    /// When the trail was created
    pub created_at: DateTime<Utc>,
    /// When the trail was last updated
    pub updated_at: DateTime<Utc>,
}

impl Default for AuditTrail {
    fn default() -> Self {
        let now = Utc::now();
        Self {
            version: 1,
            records: Vec::new(),
            created_at: now,
            updated_at: now,
        }
    }
}

/// Audit manager for storing and querying records
pub struct AuditManager {
    /// Audit file path
    audit_path: PathBuf,
    /// In-memory trail
    trail: AuditTrail,
}

impl AuditManager {
    /// Create a new audit manager
    pub fn new(audit_dir: impl Into<PathBuf>) -> Result<Self> {
        let audit_dir = audit_dir.into();
        std::fs::create_dir_all(&audit_dir)?;

        let audit_path = audit_dir.join("audit.json");

        let trail = if audit_path.exists() {
            let content = std::fs::read_to_string(&audit_path)?;
            serde_json::from_str(&content).unwrap_or_default()
        } else {
            AuditTrail::default()
        };

        Ok(Self { audit_path, trail })
    }

    /// Create audit manager with default path
    pub fn with_home() -> Result<Self> {
        let home = dirs::home_dir()
            .ok_or_else(|| crate::error::Error::ConfigError(
                "Could not find home directory".to_string()
            ))?;
        let audit_dir = home.join(".ggen").join("audit");
        Self::new(audit_dir)
    }

    /// Record an installation
    pub fn record_installation(
        &mut self,
        package_id: &PackageId,
        version: &PackageVersion,
        install_path: &Path,
        status: InstallationStatus,
        validation_result: Option<&InstallValidationResult>,
        dependencies: &[ResolvedDependency],
        duration_ms: u64,
    ) -> Result<Uuid> {
        let span = span!(
            Level::INFO,
            "audit_installation",
            package = %package_id,
            version = %version,
            status = %status
        );
        let _enter = span.enter();

        let id = Uuid::new_v4();

        let fmea_status = validation_result
            .map(FmeaAuditStatus::from)
            .unwrap_or(FmeaAuditStatus {
                passed: true,
                force_applied: false,
                critical_failures: 0,
                total_rpn: 0,
                max_rpn: 200,
                warnings: Vec::new(),
                errors: Vec::new(),
            });

        let guards: Vec<GuardAuditEntry> = validation_result
            .map(|v| {
                v.applied_guards
                    .iter()
                    .map(|(g, r)| GuardAuditEntry::from_guard_result(g, r))
                    .collect()
            })
            .unwrap_or_default();

        let dep_entries: Vec<DependencyAuditEntry> = dependencies
            .iter()
            .map(DependencyAuditEntry::from)
            .collect();

        let record = InstallationRecord {
            id,
            timestamp: Utc::now(),
            package_id: package_id.clone(),
            version: version.clone(),
            install_path: install_path.to_path_buf(),
            status,
            fmea_status,
            guards,
            dependencies: dep_entries,
            duration_ms,
            initiated_by: whoami::username(),
            metadata: BTreeMap::new(),
        };

        info!(
            record_id = %id,
            package = %package_id,
            version = %version,
            status = %status,
            deps = %dependencies.len(),
            guards = %record.guards.len(),
            "Recorded installation audit"
        );

        self.trail.records.push(record);
        self.trail.updated_at = Utc::now();

        self.save()?;

        Ok(id)
    }

    /// Save audit trail to disk
    fn save(&self) -> Result<()> {
        let content = serde_json::to_string_pretty(&self.trail)?;
        std::fs::write(&self.audit_path, content)?;
        debug!(path = %self.audit_path.display(), "Saved audit trail");
        Ok(())
    }

    /// Get all records
    pub fn all_records(&self) -> &[InstallationRecord] {
        &self.trail.records
    }

    /// Get record by ID
    pub fn get_record(&self, id: &Uuid) -> Option<&InstallationRecord> {
        self.trail.records.iter().find(|r| &r.id == id)
    }

    /// Query records by package
    pub fn query_by_package(&self, package_id: &PackageId) -> Vec<&InstallationRecord> {
        self.trail
            .records
            .iter()
            .filter(|r| &r.package_id == package_id)
            .collect()
    }

    /// Query records by status
    pub fn query_by_status(&self, status: InstallationStatus) -> Vec<&InstallationRecord> {
        self.trail
            .records
            .iter()
            .filter(|r| r.status == status)
            .collect()
    }

    /// Query records by date range
    pub fn query_by_date_range(
        &self,
        from: DateTime<Utc>,
        to: DateTime<Utc>,
    ) -> Vec<&InstallationRecord> {
        self.trail
            .records
            .iter()
            .filter(|r| r.timestamp >= from && r.timestamp <= to)
            .collect()
    }

    /// Query records with FMEA failures
    pub fn query_fmea_failures(&self) -> Vec<&InstallationRecord> {
        self.trail
            .records
            .iter()
            .filter(|r| !r.fmea_status.passed || r.fmea_status.force_applied)
            .collect()
    }

    /// Get installation statistics
    pub fn statistics(&self) -> AuditStatistics {
        let total = self.trail.records.len();
        let successful = self.trail.records.iter()
            .filter(|r| r.status == InstallationStatus::Success)
            .count();
        let failed = self.trail.records.iter()
            .filter(|r| r.status == InstallationStatus::Failed)
            .count();
        let force_fmea_used = self.trail.records.iter()
            .filter(|r| r.fmea_status.force_applied)
            .count();

        let avg_duration: u64 = if total > 0 {
            self.trail.records.iter().map(|r| r.duration_ms).sum::<u64>() / total as u64
        } else {
            0
        };

        let unique_packages: std::collections::HashSet<&PackageId> = self.trail
            .records
            .iter()
            .map(|r| &r.package_id)
            .collect();

        AuditStatistics {
            total_installations: total,
            successful_installations: successful,
            failed_installations: failed,
            force_fmea_count: force_fmea_used,
            average_duration_ms: avg_duration,
            unique_packages: unique_packages.len(),
        }
    }

    /// Clear all records (for testing)
    pub fn clear(&mut self) -> Result<()> {
        self.trail.records.clear();
        self.trail.updated_at = Utc::now();
        self.save()
    }

    /// Export audit trail to JSON
    pub fn export_json(&self) -> Result<String> {
        Ok(serde_json::to_string_pretty(&self.trail)?)
    }
}

/// Audit statistics
#[derive(Debug, Clone)]
pub struct AuditStatistics {
    /// Total number of installations
    pub total_installations: usize,
    /// Number of successful installations
    pub successful_installations: usize,
    /// Number of failed installations
    pub failed_installations: usize,
    /// Number of times --force-fmea was used
    pub force_fmea_count: usize,
    /// Average installation duration in milliseconds
    pub average_duration_ms: u64,
    /// Number of unique packages installed
    pub unique_packages: usize,
}

impl std::fmt::Display for AuditStatistics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Audit Statistics:")?;
        writeln!(f, "  Total installations: {}", self.total_installations)?;
        writeln!(f, "  Successful: {} ({:.1}%)",
            self.successful_installations,
            if self.total_installations > 0 {
                (self.successful_installations as f64 / self.total_installations as f64) * 100.0
            } else {
                0.0
            }
        )?;
        writeln!(f, "  Failed: {}", self.failed_installations)?;
        writeln!(f, "  Force FMEA used: {}", self.force_fmea_count)?;
        writeln!(f, "  Average duration: {}ms", self.average_duration_ms)?;
        writeln!(f, "  Unique packages: {}", self.unique_packages)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_dependency(id: &str) -> ResolvedDependency {
        ResolvedDependency {
            id: PackageId::new(id).unwrap(),
            version: PackageVersion::new("1.0.0").unwrap(),
            checksum: "a".repeat(64),
            download_url: format!("https://example.com/{}.tar.gz", id),
            dependencies: vec![],
            depth: 0,
        }
    }

    fn create_test_validation_result(package_id: &str, passed: bool) -> InstallValidationResult {
        InstallValidationResult {
            package_id: PackageId::new(package_id).unwrap(),
            version: PackageVersion::new("1.0.0").unwrap(),
            passed,
            force_applied: !passed,
            critical_failures: if passed { 0 } else { 1 },
            total_rpn: if passed { 50 } else { 250 },
            applied_guards: vec![],
            warnings: vec![],
            errors: if passed { vec![] } else { vec!["Test error".to_string()] },
            validated_at: Utc::now(),
        }
    }

    #[test]
    fn test_installation_status_display() {
        assert_eq!(InstallationStatus::Success.to_string(), "success");
        assert_eq!(InstallationStatus::Failed.to_string(), "failed");
        assert_eq!(InstallationStatus::RolledBack.to_string(), "rolled_back");
    }

    #[test]
    fn test_fmea_audit_status_from_validation() {
        let validation = create_test_validation_result("test-pkg", true);
        let status = FmeaAuditStatus::from(&validation);

        assert!(status.passed);
        assert!(!status.force_applied);
        assert_eq!(status.critical_failures, 0);
    }

    #[test]
    fn test_dependency_audit_entry_from_resolved() {
        let dep = create_test_dependency("test-dep");
        let entry = DependencyAuditEntry::from(&dep);

        assert_eq!(entry.package_id.as_str(), "test-dep");
        assert_eq!(entry.version.as_str(), "1.0.0");
        assert!(entry.direct);
    }

    #[test]
    fn test_audit_manager_creation() {
        let temp_dir = TempDir::new().unwrap();
        let manager = AuditManager::new(temp_dir.path()).unwrap();

        assert!(manager.all_records().is_empty());
    }

    #[test]
    fn test_audit_manager_record_installation() {
        let temp_dir = TempDir::new().unwrap();
        let mut manager = AuditManager::new(temp_dir.path()).unwrap();

        let pkg_id = PackageId::new("test-pkg").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        let deps = vec![create_test_dependency("test-dep")];
        let validation = create_test_validation_result("test-pkg", true);

        let record_id = manager.record_installation(
            &pkg_id,
            &version,
            Path::new("/tmp/install"),
            InstallationStatus::Success,
            Some(&validation),
            &deps,
            1000,
        ).unwrap();

        assert_eq!(manager.all_records().len(), 1);

        let record = manager.get_record(&record_id).unwrap();
        assert_eq!(record.package_id, pkg_id);
        assert_eq!(record.status, InstallationStatus::Success);
    }

    #[test]
    fn test_audit_manager_query_by_package() {
        let temp_dir = TempDir::new().unwrap();
        let mut manager = AuditManager::new(temp_dir.path()).unwrap();

        let pkg_a = PackageId::new("pkg-a").unwrap();
        let pkg_b = PackageId::new("pkg-b").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();

        manager.record_installation(
            &pkg_a,
            &version,
            Path::new("/tmp/a"),
            InstallationStatus::Success,
            None,
            &[],
            100,
        ).unwrap();

        manager.record_installation(
            &pkg_b,
            &version,
            Path::new("/tmp/b"),
            InstallationStatus::Success,
            None,
            &[],
            100,
        ).unwrap();

        manager.record_installation(
            &pkg_a,
            &PackageVersion::new("2.0.0").unwrap(),
            Path::new("/tmp/a"),
            InstallationStatus::Success,
            None,
            &[],
            100,
        ).unwrap();

        let pkg_a_records = manager.query_by_package(&pkg_a);
        assert_eq!(pkg_a_records.len(), 2);
    }

    #[test]
    fn test_audit_manager_query_by_status() {
        let temp_dir = TempDir::new().unwrap();
        let mut manager = AuditManager::new(temp_dir.path()).unwrap();

        let pkg = PackageId::new("test-pkg").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();

        manager.record_installation(
            &pkg,
            &version,
            Path::new("/tmp"),
            InstallationStatus::Success,
            None,
            &[],
            100,
        ).unwrap();

        manager.record_installation(
            &pkg,
            &version,
            Path::new("/tmp"),
            InstallationStatus::Failed,
            None,
            &[],
            100,
        ).unwrap();

        let successful = manager.query_by_status(InstallationStatus::Success);
        assert_eq!(successful.len(), 1);

        let failed = manager.query_by_status(InstallationStatus::Failed);
        assert_eq!(failed.len(), 1);
    }

    #[test]
    fn test_audit_manager_statistics() {
        let temp_dir = TempDir::new().unwrap();
        let mut manager = AuditManager::new(temp_dir.path()).unwrap();

        let pkg = PackageId::new("test-pkg").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();

        manager.record_installation(
            &pkg,
            &version,
            Path::new("/tmp"),
            InstallationStatus::Success,
            None,
            &[],
            100,
        ).unwrap();

        manager.record_installation(
            &pkg,
            &version,
            Path::new("/tmp"),
            InstallationStatus::Failed,
            None,
            &[],
            200,
        ).unwrap();

        let stats = manager.statistics();

        assert_eq!(stats.total_installations, 2);
        assert_eq!(stats.successful_installations, 1);
        assert_eq!(stats.failed_installations, 1);
        assert_eq!(stats.average_duration_ms, 150);
        assert_eq!(stats.unique_packages, 1);
    }

    #[test]
    fn test_audit_persistence() {
        let temp_dir = TempDir::new().unwrap();

        // Create and record
        {
            let mut manager = AuditManager::new(temp_dir.path()).unwrap();
            let pkg = PackageId::new("test-pkg").unwrap();
            let version = PackageVersion::new("1.0.0").unwrap();

            manager.record_installation(
                &pkg,
                &version,
                Path::new("/tmp"),
                InstallationStatus::Success,
                None,
                &[],
                100,
            ).unwrap();
        }

        // Reload and verify
        {
            let manager = AuditManager::new(temp_dir.path()).unwrap();
            assert_eq!(manager.all_records().len(), 1);
        }
    }
}
