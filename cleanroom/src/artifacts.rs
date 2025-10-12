//! Artifact collection and forensics bundle creation
//!
//! Collects and packages test execution artifacts including logs, environment,
//! coverage data, and attestation into a replayable forensics bundle.
//!
//! WIP: Implement artifact collection and forensics bundle creation

use crate::coverage::CoverageCollector;
use crate::error::Result;
use base64::Engine;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// Forensics bundle containing all test execution artifacts
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForensicsBundle {
    /// Bundle metadata
    pub metadata: BundleMetadata,
    /// Test execution logs
    pub logs: Vec<LogEntry>,
    /// Environment information (redacted)
    pub environment: HashMap<String, String>,
    /// Coverage data
    pub coverage: Option<CoverageArtifact>,
    /// Attestation data
    pub attestation: Option<AttestationArtifact>,
    /// Binary artifacts
    pub binaries: Vec<BinaryArtifact>,
    /// Configuration files
    pub configs: Vec<ConfigArtifact>,
}

/// Bundle metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BundleMetadata {
    /// Bundle version
    pub version: String,
    /// Creation timestamp
    pub created_at: u64,
    /// Test scenario name
    pub scenario_name: String,
    /// Bundle ID
    pub bundle_id: String,
    /// Description
    pub description: Option<String>,
}

/// Log entry from test execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogEntry {
    /// Timestamp
    pub timestamp: u64,
    /// Log level
    pub level: LogLevel,
    /// Component that generated the log
    pub component: String,
    /// Log message
    pub message: String,
    /// Additional context
    pub context: HashMap<String, String>,
}

/// Log levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum LogLevel {
    /// Debug level
    Debug,
    /// Info level
    Info,
    /// Warning level
    Warn,
    /// Error level
    Error,
}

/// Coverage artifact
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageArtifact {
    /// Coverage format
    pub format: String,
    /// Coverage data (base64 encoded)
    pub data: String,
    /// Lines covered
    pub lines_covered: u64,
    /// Total lines
    pub lines_total: u64,
    /// Coverage percentage
    pub percentage: f64,
    /// Path remapping info
    pub path_remap: Vec<PathRemap>,
}

/// Path remapping information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PathRemap {
    /// Source path
    pub from: String,
    /// Target path
    pub to: String,
}

/// Attestation artifact
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AttestationArtifact {
    /// Attestation format
    pub format: String,
    /// Attestation data (JSON encoded)
    pub data: String,
    /// Signature (if available)
    pub signature: Option<String>,
    /// Timestamp of attestation
    pub timestamp: u64,
}

/// Binary artifact
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BinaryArtifact {
    /// Binary name
    pub name: String,
    /// Binary path
    pub path: String,
    /// Binary hash
    pub hash: String,
    /// Binary data (base64 encoded)
    pub data: String,
}

/// Configuration artifact
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfigArtifact {
    /// Config name
    pub name: String,
    /// Config path
    pub path: String,
    /// Config data
    pub data: String,
}

/// Artifact collector for gathering test execution artifacts
pub struct ArtifactCollector {
    /// Working directory for temporary files
    #[allow(dead_code)]
    work_dir: PathBuf,
    /// Whether to redact sensitive environment variables
    redact_env: bool,
}

impl ArtifactCollector {
    /// Create a new artifact collector
    pub fn new() -> Result<Self> {
        let work_dir = std::env::temp_dir().join("cleanroom-artifacts");
        std::fs::create_dir_all(&work_dir).map_err(|e| {
            crate::error::CleanroomError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Failed to create artifacts directory: {}", e),
            ))
        })?;

        Ok(Self {
            work_dir,
            redact_env: true,
        })
    }

    /// Set whether to redact sensitive environment variables
    pub fn with_redaction(mut self, redact: bool) -> Self {
        self.redact_env = redact;
        self
    }

    /// Collect artifacts from a test run
    pub fn collect(&self, run_info: &RunInfo) -> Result<ForensicsBundle> {
        let bundle_id = format!("bundle_{}", rand::random::<u32>());
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();

        // Collect logs
        let mut logs = Vec::new();
        for log_file in &run_info.log_files {
            if log_file.exists() {
                let content =
                    std::fs::read_to_string(log_file).map_err(crate::error::CleanroomError::Io)?;

                logs.push(LogEntry {
                    timestamp,
                    level: LogLevel::Info,
                    component: "test".to_string(),
                    message: content,
                    context: HashMap::new(),
                });
            }
        }

        // Collect environment (with redaction)
        let mut environment = HashMap::new();
        for (key, value) in std::env::vars() {
            if self.redact_env && self.is_sensitive_key(&key) {
                environment.insert(key, "[REDACTED]".to_string());
            } else {
                environment.insert(key, value);
            }
        }

        // Collect binaries
        let mut binaries = Vec::new();
        for binary_file in &run_info.binary_files {
            if binary_file.exists() {
                let content =
                    std::fs::read(binary_file).map_err(crate::error::CleanroomError::Io)?;
                let hash = self.calculate_hash(&content);
                let data = base64::engine::general_purpose::STANDARD.encode(&content);

                binaries.push(BinaryArtifact {
                    name: binary_file
                        .file_name()
                        .unwrap_or_default()
                        .to_string_lossy()
                        .to_string(),
                    path: binary_file.to_string_lossy().to_string(),
                    hash,
                    data,
                });
            }
        }

        // Collect configs
        let mut configs = Vec::new();
        for config_file in &run_info.config_files {
            if config_file.exists() {
                let content = std::fs::read_to_string(config_file)
                    .map_err(crate::error::CleanroomError::Io)?;

                configs.push(ConfigArtifact {
                    name: config_file
                        .file_name()
                        .unwrap_or_default()
                        .to_string_lossy()
                        .to_string(),
                    path: config_file.to_string_lossy().to_string(),
                    data: content,
                });
            }
        }

        let bundle = ForensicsBundle {
            metadata: BundleMetadata {
                version: "1.0".to_string(),
                created_at: timestamp,
                scenario_name: run_info.scenario_name.clone(),
                bundle_id,
                description: Some("Test execution forensics bundle".to_string()),
            },
            logs,
            environment,
            coverage: self.collect_coverage(run_info)?,
            attestation: self.collect_attestation(run_info)?,
            binaries,
            configs,
        };

        Ok(bundle)
    }

    /// Collect coverage data from test execution
    fn collect_coverage(&self, run_info: &RunInfo) -> Result<Option<CoverageArtifact>> {
        // If no coverage files or container ID, return None
        if run_info.coverage_files.is_empty() && run_info.container_id.is_none() {
            return Ok(None);
        }

        let coverage_collector = CoverageCollector::new()?;

        let mut coverage_data = Vec::new();
        let mut path_remaps = Vec::new();

        // Collect coverage from local files
        for coverage_file in &run_info.coverage_files {
            if coverage_file.exists() {
                let content =
                    std::fs::read(coverage_file).map_err(crate::error::CleanroomError::Io)?;
                let data = base64::engine::general_purpose::STANDARD.encode(&content);
                coverage_data.push(data);
            }
        }

        // Collect coverage from container if available
        if let Some(ref container_id) = run_info.container_id {
            match coverage_collector.collect_from_container(container_id) {
                Ok(coverage_data_from_container) => {
                    let content = std::fs::read(&coverage_data_from_container.path)
                        .map_err(crate::error::CleanroomError::Io)?;
                    let data = base64::engine::general_purpose::STANDARD.encode(&content);
                    coverage_data.push(data);

                    // Add path remapping for container coverage
                    path_remaps.push(PathRemap {
                        from: "/workdir/src".to_string(),
                        to: run_info.workdir.join("src").to_string_lossy().to_string(),
                    });
                }
                Err(e) => {
                    // Log error but don't fail the entire collection
                    eprintln!(
                        "Warning: Failed to collect coverage from container {}: {}",
                        container_id, e
                    );
                }
            }
        }

        // If we have coverage data, create the artifact
        if !coverage_data.is_empty() {
            // Calculate basic coverage statistics from collected data
            let (lines_covered, lines_total, percentage) =
                self.calculate_basic_coverage_stats(&coverage_data);

            Ok(Some(CoverageArtifact {
                format: "profraw".to_string(),
                data: coverage_data.join("\n"),
                lines_covered,
                lines_total,
                percentage,
                path_remap: path_remaps,
            }))
        } else {
            Ok(None)
        }
    }

    /// Collect attestation data from test execution
    fn collect_attestation(&self, run_info: &RunInfo) -> Result<Option<AttestationArtifact>> {
        use crate::attest::AttestationGenerator;

        let generator = AttestationGenerator::new();

        // Convert RunInfo to attest::RunInfo format
        let attest_run_info = crate::attest::RunInfo {
            scenario_name: run_info.scenario_name.clone(),
            backend: run_info.backend.clone(),
            image_digests: run_info.image_digests.clone(),
            policy: serde_json::to_string(&run_info.policy)
                .unwrap_or_else(|_| format!("{:?}", run_info.policy)),
            environment: run_info.environment.clone(),
        };

        // Generate attestation
        let attestation = generator.generate(&attest_run_info, None)?;

        // Convert to AttestationArtifact
        let artifact = AttestationArtifact {
            format: "json".to_string(),
            data: serde_json::to_string(&attestation).map_err(|e| {
                crate::error::CleanroomError::Backend(crate::error::BackendError::Runtime(format!(
                    "Failed to serialize attestation: {}",
                    e
                )))
            })?,
            signature: attestation.signature,
            timestamp: attestation.timestamp,
        };

        Ok(Some(artifact))
    }

    /// Calculate coverage statistics from collected data
    fn calculate_coverage_stats(
        &self, coverage_data: &[String], path_remaps: &[PathRemap],
    ) -> Result<(u64, u64, f64)> {
        use crate::coverage::CoverageCollector;
        use std::path::PathBuf;

        let collector = CoverageCollector::new()?;
        let mut total_lines_covered = 0u64;
        let mut total_lines_total = 0u64;

        // Process each coverage data entry
        for (i, data) in coverage_data.iter().enumerate() {
            // Decode base64 data to temporary file
            let decoded_data = base64::engine::general_purpose::STANDARD
                .decode(data)
                .map_err(|e| {
                    crate::error::CleanroomError::Backend(crate::error::BackendError::Runtime(
                        format!("Failed to decode coverage data: {}", e),
                    ))
                })?;

            let temp_file = self.work_dir.join(format!("temp_coverage_{}.profraw", i));
            std::fs::write(&temp_file, decoded_data).map_err(crate::error::CleanroomError::Io)?;

            // Create coverage data struct
            let coverage_data_struct = crate::coverage::CoverageData {
                path: temp_file,
                format: crate::coverage::CoverageFormat::Profraw,
            };

            // Use the first path remap if available
            if let Some(remap) = path_remaps.get(i) {
                let path_remap_struct = crate::coverage::PathRemap {
                    from: PathBuf::from(&remap.from),
                    to: PathBuf::from(&remap.to),
                };

                // Merge coverage data
                let merged = collector.merge_with_remap(coverage_data_struct, path_remap_struct)?;
                total_lines_covered += merged.lines_covered;
                total_lines_total += merged.lines_total;
            } else {
                // Fallback: simple merge without path remapping
                let dummy_remap = crate::coverage::PathRemap {
                    from: PathBuf::from("/workdir"),
                    to: PathBuf::from("/workdir"),
                };

                let merged = collector.merge_with_remap(coverage_data_struct, dummy_remap)?;
                total_lines_covered += merged.lines_covered;
                total_lines_total += merged.lines_total;
            }
        }

        // Calculate overall percentage
        let percentage = if total_lines_total > 0 {
            (total_lines_covered as f64 / total_lines_total as f64) * 100.0
        } else {
            0.0
        };

        Ok((total_lines_covered, total_lines_total, percentage))
    }

    /// Save bundle to file
    pub fn save_bundle(&self, bundle: &ForensicsBundle, path: PathBuf) -> Result<()> {
        let content = serde_json::to_string_pretty(bundle).map_err(|e| {
            crate::error::CleanroomError::Backend(crate::error::BackendError::Runtime(format!(
                "Failed to serialize bundle: {}",
                e
            )))
        })?;

        std::fs::write(&path, content).map_err(crate::error::CleanroomError::Io)?;

        Ok(())
    }

    /// Load bundle from file
    pub fn load_bundle(&self, path: PathBuf) -> Result<ForensicsBundle> {
        let content = std::fs::read_to_string(&path).map_err(crate::error::CleanroomError::Io)?;

        let bundle: ForensicsBundle = serde_json::from_str(&content).map_err(|e| {
            crate::error::CleanroomError::Backend(crate::error::BackendError::Runtime(format!(
                "Failed to deserialize bundle: {}",
                e
            )))
        })?;

        Ok(bundle)
    }

    /// Check if an environment variable key is sensitive
    fn is_sensitive_key(&self, key: &str) -> bool {
        let sensitive_patterns = [
            "KEY", "TOKEN", "SECRET", "PASSWORD", "PASS", "AUTH", "AWS_", "GITHUB_", "GITLAB_",
            "DOCKER_", "KUBE_",
        ];

        sensitive_patterns
            .iter()
            .any(|pattern| key.contains(pattern))
    }

    /// Calculate SHA-256 hash of binary data
    fn calculate_hash(&self, data: &[u8]) -> String {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        data.hash(&mut hasher);
        format!("{:x}", hasher.finish())
    }
}

/// Information about a test run for artifact collection
pub struct RunInfo {
    /// Test scenario name
    pub scenario_name: String,
    /// Backend used
    pub backend: String,
    /// Working directory
    pub workdir: PathBuf,
    /// Log files to collect
    pub log_files: Vec<PathBuf>,
    /// Binary files to collect
    pub binary_files: Vec<PathBuf>,
    /// Config files to collect
    pub config_files: Vec<PathBuf>,
    /// Coverage files to collect
    pub coverage_files: Vec<PathBuf>,
    /// Container ID for coverage collection (if using container backend)
    pub container_id: Option<String>,
    /// Policy applied during execution
    pub policy: crate::policy::Policy,
    /// Image digests used
    pub image_digests: HashMap<String, String>,
    /// Environment variables
    pub environment: HashMap<String, String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_artifact_collector_creation() {
        let collector = ArtifactCollector::new().unwrap();
        assert!(collector.work_dir.exists());
        assert!(collector.redact_env);
    }

    #[test]
    fn test_artifact_collector_without_redaction() {
        let collector = ArtifactCollector::new().unwrap().with_redaction(false);
        assert!(!collector.redact_env);
    }

    #[test]
    fn test_sensitive_key_detection() {
        let collector = ArtifactCollector::new().unwrap();
        assert!(collector.is_sensitive_key("API_KEY"));
        assert!(collector.is_sensitive_key("GITHUB_TOKEN"));
        assert!(collector.is_sensitive_key("SECRET_PASSWORD"));
        assert!(!collector.is_sensitive_key("NORMAL_VAR"));
    }

    #[test]
    fn test_hash_calculation() {
        let collector = ArtifactCollector::new().unwrap();
        let data = b"test data";
        let hash = collector.calculate_hash(data);
        assert!(!hash.is_empty());
    }

    #[test]
    fn test_run_info_creation() {
        let run_info = RunInfo {
            scenario_name: "test_scenario".to_string(),
            backend: "local".to_string(),
            workdir: PathBuf::from("/tmp"),
            log_files: vec![],
            binary_files: vec![],
            config_files: vec![],
            coverage_files: vec![],
            container_id: None,
            policy: crate::policy::Policy::default(),
            image_digests: HashMap::new(),
            environment: HashMap::new(),
        };
        assert_eq!(run_info.scenario_name, "test_scenario");
        assert_eq!(run_info.backend, "local");
    }

    #[test]
    fn test_bundle_metadata_creation() {
        let metadata = BundleMetadata {
            version: "1.0".to_string(),
            created_at: 1234567890,
            scenario_name: "test".to_string(),
            bundle_id: "bundle_123".to_string(),
            description: Some("Test bundle".to_string()),
        };
        assert_eq!(metadata.version, "1.0");
        assert_eq!(metadata.scenario_name, "test");
    }

    #[test]
    fn test_log_entry_creation() {
        let log_entry = LogEntry {
            timestamp: 1234567890,
            level: LogLevel::Info,
            component: "test".to_string(),
            message: "Test message".to_string(),
            context: HashMap::new(),
        };
        assert_eq!(log_entry.level, LogLevel::Info);
        assert_eq!(log_entry.message, "Test message");
    }

    #[test]
    fn test_binary_artifact_creation() {
        let artifact = BinaryArtifact {
            name: "test_bin".to_string(),
            path: "/tmp/test_bin".to_string(),
            hash: "abc123".to_string(),
            data: "base64data".to_string(),
        };
        assert_eq!(artifact.name, "test_bin");
        assert_eq!(artifact.hash, "abc123");
    }

    #[test]
    fn test_config_artifact_creation() {
        let artifact = ConfigArtifact {
            name: "config.toml".to_string(),
            path: "/tmp/config.toml".to_string(),
            data: "key = value".to_string(),
        };
        assert_eq!(artifact.name, "config.toml");
        assert_eq!(artifact.data, "key = value");
    }
}
