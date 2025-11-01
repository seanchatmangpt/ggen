use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

pub mod package;
pub mod query;
pub mod signature;
pub mod template_package;

pub use package::{ContentId, Package, PackageId, PackageMetadata};
pub use query::{Query, SearchQuery, SearchResults};
pub use signature::{PublicKey, Signature};
pub use template_package::{TemplatePackage, TemplateInfo, TemplateType, TemplateVariable, TemplateExample};

/// Package version following semantic versioning
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
    pub pre_release: Option<String>,
}

impl Version {
    pub fn new(major: u32, minor: u32, patch: u32) -> Self {
        Self {
            major,
            minor,
            patch,
            pre_release: None,
        }
    }

    pub fn with_pre_release(mut self, pre_release: impl Into<String>) -> Self {
        self.pre_release = Some(pre_release.into());
        self
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)?;
        if let Some(pre) = &self.pre_release {
            write!(f, "-{}", pre)?;
        }
        Ok(())
    }
}

/// Package category for classification
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Category {
    WebService,
    Database,
    Authentication,
    Middleware,
    Testing,
    Deployment,
    Monitoring,
    Documentation,
    Utility,
    Custom(String),
}

impl fmt::Display for Category {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::WebService => write!(f, "web-service"),
            Self::Database => write!(f, "database"),
            Self::Authentication => write!(f, "authentication"),
            Self::Middleware => write!(f, "middleware"),
            Self::Testing => write!(f, "testing"),
            Self::Deployment => write!(f, "deployment"),
            Self::Monitoring => write!(f, "monitoring"),
            Self::Documentation => write!(f, "documentation"),
            Self::Utility => write!(f, "utility"),
            Self::Custom(name) => write!(f, "{}", name),
        }
    }
}

/// Package statistics and metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageStats {
    pub downloads: u64,
    pub stars: u32,
    pub watchers: u32,
    pub forks: u32,
    pub contributors: u32,
    pub last_updated: chrono::DateTime<chrono::Utc>,
}

impl Default for PackageStats {
    fn default() -> Self {
        Self {
            downloads: 0,
            stars: 0,
            watchers: 0,
            forks: 0,
            contributors: 0,
            last_updated: chrono::Utc::now(),
        }
    }
}

/// Package dependency specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependency {
    pub package_id: PackageId,
    pub version_requirement: VersionRequirement,
    pub optional: bool,
}

/// Version requirement for dependencies
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum VersionRequirement {
    Exact(Version),
    Range { min: Version, max: Version },
    Minimum(Version),
    Compatible(Version), // ^x.y.z
    Any,
}

/// Registry configuration metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegistryMetadata {
    pub name: String,
    pub description: String,
    pub version: String,
    pub package_count: usize,
    pub api_version: String,
    pub features: Vec<String>,
}

/// Registry capabilities and features
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum RegistryCapability {
    Search,
    Publish,
    Delete,
    Analytics,
    Webhooks,
    Mirroring,
}

/// Rate limit configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RateLimit {
    pub requests_per_minute: u32,
    pub burst_size: u32,
}

/// User or organization identity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Identity {
    pub id: String,
    pub name: String,
    pub email: Option<String>,
    pub public_key: PublicKey,
    pub metadata: HashMap<String, String>,
}
