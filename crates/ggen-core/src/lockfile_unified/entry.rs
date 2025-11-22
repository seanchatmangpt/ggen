//! Unified lockfile entry types supporting all use cases
//!
//! This module provides a unified entry type that can represent entries
//! from any of the three existing lockfile implementations.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;

use super::traits::LockEntry;

/// Unified lock entry supporting packs, ontologies, and gpacks
///
/// This type consolidates the features of:
/// - `LockEntry` from `lockfile.rs` (PQC signatures)
/// - `LockedPackage` from `lock_manager.rs` (ontology metadata)
/// - `LockedPack` from `packs/lockfile.rs` (source tracking)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct UnifiedLockEntry {
    /// Entry identifier
    #[serde(skip)]
    id: String,

    /// Version string (semver or custom)
    pub version: String,

    /// Integrity hash (SHA256, lowercase hex, 64 chars)
    pub integrity: String,

    /// Source location
    pub source: LockSource,

    /// When this entry was added/updated
    pub locked_at: DateTime<Utc>,

    /// Dependencies (list of entry IDs)
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub dependencies: Vec<String>,

    /// Extended metadata (domain-specific)
    #[serde(default, skip_serializing_if = "ExtendedMetadata::is_empty")]
    pub metadata: ExtendedMetadata,

    /// PQC signature (optional)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub pqc: Option<PqcSignature>,
}

impl UnifiedLockEntry {
    /// Create a new unified lock entry
    pub fn new(
        id: impl Into<String>,
        version: impl Into<String>,
        integrity: impl Into<String>,
        source: LockSource,
    ) -> Self {
        Self {
            id: id.into(),
            version: version.into(),
            integrity: integrity.into(),
            source,
            locked_at: Utc::now(),
            dependencies: Vec::new(),
            metadata: ExtendedMetadata::default(),
            pqc: None,
        }
    }

    /// Set entry ID (used when loading from map)
    pub fn with_id(mut self, id: impl Into<String>) -> Self {
        self.id = id.into();
        self
    }

    /// Add dependencies
    pub fn with_dependencies(mut self, deps: Vec<String>) -> Self {
        self.dependencies = deps;
        self
    }

    /// Add PQC signature
    pub fn with_pqc(mut self, pqc: PqcSignature) -> Self {
        self.pqc = Some(pqc);
        self
    }

    /// Add extended metadata
    pub fn with_metadata(mut self, metadata: ExtendedMetadata) -> Self {
        self.metadata = metadata;
        self
    }
}

impl LockEntry for UnifiedLockEntry {
    fn id(&self) -> &str {
        &self.id
    }

    fn version(&self) -> &str {
        &self.version
    }

    fn integrity(&self) -> Option<&str> {
        Some(&self.integrity)
    }

    fn dependencies(&self) -> &[String] {
        &self.dependencies
    }
}

/// Source of a locked entry
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "type")]
pub enum LockSource {
    /// Registry (marketplace)
    Registry {
        /// Registry URL
        url: String,
        /// Resolved download URL (optional)
        #[serde(skip_serializing_if = "Option::is_none")]
        resolved: Option<String>,
    },

    /// Git repository
    Git {
        /// Repository URL
        url: String,
        /// Branch name
        #[serde(skip_serializing_if = "Option::is_none")]
        branch: Option<String>,
        /// Tag
        #[serde(skip_serializing_if = "Option::is_none")]
        tag: Option<String>,
        /// Commit SHA
        #[serde(skip_serializing_if = "Option::is_none")]
        commit: Option<String>,
    },

    /// GitHub shorthand
    GitHub {
        /// Organization or user
        org: String,
        /// Repository name
        repo: String,
        /// Branch or tag
        branch: String,
    },

    /// Local filesystem
    Local {
        /// Path to pack directory
        path: PathBuf,
    },
}

impl std::fmt::Display for LockSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LockSource::Registry { url, .. } => write!(f, "registry+{}", url),
            LockSource::Git { url, branch, .. } => {
                if let Some(b) = branch {
                    write!(f, "git+{}#{}", url, b)
                } else {
                    write!(f, "git+{}", url)
                }
            }
            LockSource::GitHub { org, repo, branch } => {
                write!(f, "github:{}/{}#{}", org, repo, branch)
            }
            LockSource::Local { path } => write!(f, "path:{}", path.display()),
        }
    }
}

/// PQC signature data
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PqcSignature {
    /// Algorithm identifier (e.g., "ML-DSA-65", "Dilithium3")
    pub algorithm: String,
    /// Base64-encoded signature
    pub signature: String,
    /// Base64-encoded public key
    pub pubkey: String,
}

impl PqcSignature {
    /// Create a new PQC signature
    pub fn new(
        algorithm: impl Into<String>,
        signature: impl Into<String>,
        pubkey: impl Into<String>,
    ) -> Self {
        Self {
            algorithm: algorithm.into(),
            signature: signature.into(),
            pubkey: pubkey.into(),
        }
    }

    /// Create ML-DSA-65 signature
    pub fn ml_dsa_65(signature: impl Into<String>, pubkey: impl Into<String>) -> Self {
        Self::new("ML-DSA-65", signature, pubkey)
    }
}

/// Extended metadata for domain-specific needs
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq)]
pub struct ExtendedMetadata {
    /// For ontology packs: namespace URI
    #[serde(skip_serializing_if = "Option::is_none")]
    pub namespace: Option<String>,

    /// For ontology packs: class count
    #[serde(skip_serializing_if = "Option::is_none")]
    pub classes_count: Option<usize>,

    /// For ontology packs: property count
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties_count: Option<usize>,

    /// For composition: strategy used
    #[serde(skip_serializing_if = "Option::is_none")]
    pub composition_strategy: Option<String>,

    /// Installation timestamp (if different from locked_at)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub installed_at: Option<DateTime<Utc>>,

    /// Custom key-value pairs for extensibility
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub custom: BTreeMap<String, String>,
}

impl ExtendedMetadata {
    /// Check if metadata is empty (for serialization skip)
    pub fn is_empty(&self) -> bool {
        self.namespace.is_none()
            && self.classes_count.is_none()
            && self.properties_count.is_none()
            && self.composition_strategy.is_none()
            && self.installed_at.is_none()
            && self.custom.is_empty()
    }

    /// Create ontology-specific metadata
    pub fn ontology(namespace: impl Into<String>, classes: usize, properties: usize) -> Self {
        Self {
            namespace: Some(namespace.into()),
            classes_count: Some(classes),
            properties_count: Some(properties),
            ..Default::default()
        }
    }
}

// ============================================================================
// Backward-Compatible Conversions
// ============================================================================

/// Convert from existing LockEntry (lockfile.rs)
impl From<crate::lockfile::LockEntry> for UnifiedLockEntry {
    fn from(entry: crate::lockfile::LockEntry) -> Self {
        Self {
            id: entry.id.clone(),
            version: entry.version,
            integrity: entry.sha256,
            source: LockSource::Git {
                url: entry.source,
                branch: None,
                tag: None,
                commit: None,
            },
            locked_at: Utc::now(),
            dependencies: entry.dependencies.unwrap_or_default(),
            metadata: ExtendedMetadata::default(),
            pqc: entry.pqc_signature.map(|sig| PqcSignature {
                algorithm: "Dilithium3".to_string(),
                signature: sig,
                pubkey: entry.pqc_pubkey.unwrap_or_default(),
            }),
        }
    }
}

/// Convert from PackLockfile's LockedPack (packs/lockfile.rs)
impl From<crate::packs::lockfile::LockedPack> for UnifiedLockEntry {
    fn from(pack: crate::packs::lockfile::LockedPack) -> Self {
        Self {
            id: String::new(), // Set via with_id()
            version: pack.version,
            integrity: pack.integrity.unwrap_or_default(),
            source: match pack.source {
                crate::packs::lockfile::PackSource::Registry { url } => {
                    LockSource::Registry { url, resolved: None }
                }
                crate::packs::lockfile::PackSource::GitHub { org, repo, branch } => {
                    LockSource::GitHub { org, repo, branch }
                }
                crate::packs::lockfile::PackSource::Local { path } => LockSource::Local { path },
            },
            locked_at: pack.installed_at,
            dependencies: pack.dependencies,
            metadata: ExtendedMetadata {
                installed_at: Some(pack.installed_at),
                ..Default::default()
            },
            pqc: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unified_entry_creation() {
        let entry = UnifiedLockEntry::new(
            "io.ggen.test",
            "1.0.0",
            "abc123def456",
            LockSource::Registry {
                url: "https://registry.ggen.io".into(),
                resolved: None,
            },
        );

        assert_eq!(entry.version, "1.0.0");
        assert_eq!(entry.integrity, "abc123def456");
    }

    #[test]
    fn test_lock_source_display() {
        let registry = LockSource::Registry {
            url: "https://registry.ggen.io".into(),
            resolved: None,
        };
        assert_eq!(registry.to_string(), "registry+https://registry.ggen.io");

        let github = LockSource::GitHub {
            org: "seanchatmangpt".into(),
            repo: "ggen".into(),
            branch: "main".into(),
        };
        assert_eq!(github.to_string(), "github:seanchatmangpt/ggen#main");
    }

    #[test]
    fn test_extended_metadata_is_empty() {
        let empty = ExtendedMetadata::default();
        assert!(empty.is_empty());

        let with_namespace = ExtendedMetadata {
            namespace: Some("https://schema.org/".into()),
            ..Default::default()
        };
        assert!(!with_namespace.is_empty());
    }

    #[test]
    fn test_pqc_signature_creation() {
        let sig = PqcSignature::ml_dsa_65("base64sig", "base64pubkey");
        assert_eq!(sig.algorithm, "ML-DSA-65");
    }
}
