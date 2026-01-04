//! Dependency resolution with version constraints (T008)
//!
//! This module implements SAT-based dependency resolution similar to cargo,
//! supporting version constraints, feature flags, and conflict detection.
//!
//! ## Constraint Syntax
//!
//! - Exact: `1.0.0`, `=1.0.0`
//! - At least: `>=1.0.0`
//! - Less than: `<2.0.0`
//! - Range: `>=1.0.0, <2.0.0`
//! - Caret: `^1.2.3` (compatible with 1.x.x)
//! - Tilde: `~1.2.3` (compatible with 1.2.x)
//! - Wildcard: `1.*`, `1.2.*`

use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::str::FromStr;

use super::error::{GpackError, GpackResult};
use super::manifest::PackageDependency;

/// A resolved dependency with exact version
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedDependency {
    /// Package name
    pub name: String,
    /// Resolved exact version
    pub version: String,
    /// Checksum for verification
    pub checksum: String,
    /// Download URL
    pub download_url: String,
    /// Enabled features
    pub features: Vec<String>,
    /// Dependencies of this package
    pub dependencies: Vec<String>,
    /// Whether this is a direct dependency (vs transitive)
    #[serde(default = "default_true")]
    pub is_direct: bool,
}

fn default_true() -> bool {
    true
}

impl ResolvedDependency {
    /// Create a new resolved dependency
    pub fn new(name: impl Into<String>, version: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            version: version.into(),
            checksum: String::new(),
            download_url: String::new(),
            features: Vec::new(),
            dependencies: Vec::new(),
            is_direct: true,
        }
    }

    /// Mark as transitive (not direct)
    pub fn transitive(mut self) -> Self {
        self.is_direct = false;
        self
    }

    /// Add checksum
    pub fn with_checksum(mut self, checksum: impl Into<String>) -> Self {
        self.checksum = checksum.into();
        self
    }

    /// Add download URL
    pub fn with_download_url(mut self, url: impl Into<String>) -> Self {
        self.download_url = url.into();
        self
    }

    /// Add features
    pub fn with_features(mut self, features: Vec<String>) -> Self {
        self.features = features;
        self
    }

    /// Add dependencies
    pub fn with_dependencies(mut self, deps: Vec<String>) -> Self {
        self.dependencies = deps;
        self
    }
}

impl fmt::Display for ResolvedDependency {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@{}", self.name, self.version)
    }
}

/// Resolution context with constraints
#[derive(Debug, Default)]
pub struct ResolutionContext {
    /// Version constraints per package
    pub constraints: HashMap<String, Vec<VersionConstraintSpec>>,
    /// Required features per package
    pub features: HashMap<String, HashSet<String>>,
    /// Already resolved packages
    pub resolved: HashMap<String, ResolvedDependency>,
}

/// A version constraint specification (source + constraint)
#[derive(Debug, Clone)]
pub struct VersionConstraintSpec {
    /// The constraint source (which package requires this)
    pub source: String,
    /// The constraint expression (e.g., "^1.0", ">=2.0,<3.0")
    pub constraint: String,
}

/// Version constraint enum (T008 requirement)
///
/// Represents different types of version constraints following semver conventions.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum VersionConstraint {
    /// Exact version match (e.g., `=1.0.0`, `1.0.0`)
    Exact {
        /// The exact version required
        version: String,
    },

    /// Minimum version (e.g., `>=1.0.0`)
    AtLeast {
        /// Minimum version (inclusive)
        version: String,
    },

    /// Maximum version exclusive (e.g., `<2.0.0`)
    LessThan {
        /// Maximum version (exclusive)
        version: String,
    },

    /// Maximum version inclusive (e.g., `<=2.0.0`)
    AtMost {
        /// Maximum version (inclusive)
        version: String,
    },

    /// Greater than (e.g., `>1.0.0`)
    GreaterThan {
        /// Minimum version (exclusive)
        version: String,
    },

    /// Range constraint (e.g., `>=1.0.0, <2.0.0`)
    Range {
        /// Minimum version (inclusive)
        min: String,
        /// Maximum version (exclusive)
        max: String,
    },

    /// Caret constraint (e.g., `^1.2.3` matches `>=1.2.3, <2.0.0`)
    Caret {
        /// Base version for caret constraint
        version: String,
    },

    /// Tilde constraint (e.g., `~1.2.3` matches `>=1.2.3, <1.3.0`)
    Tilde {
        /// Base version for tilde constraint
        version: String,
    },

    /// Wildcard constraint (e.g., `1.*` matches any 1.x.x)
    Wildcard {
        /// Major version
        major: u32,
        /// Optional minor version
        minor: Option<u32>,
    },

    /// Any version
    Any,
}

impl VersionConstraint {
    /// Check if a version satisfies this constraint
    pub fn matches(&self, version: &str) -> GpackResult<bool> {
        let version = semver::Version::parse(version)
            .map_err(|e| GpackError::VersionConstraintError(e.to_string()))?;

        match self {
            Self::Exact { version: v } => {
                let req = semver::Version::parse(v)
                    .map_err(|e| GpackError::VersionConstraintError(e.to_string()))?;
                Ok(version == req)
            }

            Self::AtLeast { version: v } => {
                let req = semver::Version::parse(v)
                    .map_err(|e| GpackError::VersionConstraintError(e.to_string()))?;
                Ok(version >= req)
            }

            Self::LessThan { version: v } => {
                let req = semver::Version::parse(v)
                    .map_err(|e| GpackError::VersionConstraintError(e.to_string()))?;
                Ok(version < req)
            }

            Self::AtMost { version: v } => {
                let req = semver::Version::parse(v)
                    .map_err(|e| GpackError::VersionConstraintError(e.to_string()))?;
                Ok(version <= req)
            }

            Self::GreaterThan { version: v } => {
                let req = semver::Version::parse(v)
                    .map_err(|e| GpackError::VersionConstraintError(e.to_string()))?;
                Ok(version > req)
            }

            Self::Range { min, max } => {
                let min_v = semver::Version::parse(min)
                    .map_err(|e| GpackError::VersionConstraintError(e.to_string()))?;
                let max_v = semver::Version::parse(max)
                    .map_err(|e| GpackError::VersionConstraintError(e.to_string()))?;
                Ok(version >= min_v && version < max_v)
            }

            Self::Caret { version: v } => {
                let req = semver::VersionReq::parse(&format!("^{}", v))
                    .map_err(|e| GpackError::VersionConstraintError(e.to_string()))?;
                Ok(req.matches(&version))
            }

            Self::Tilde { version: v } => {
                let req = semver::VersionReq::parse(&format!("~{}", v))
                    .map_err(|e| GpackError::VersionConstraintError(e.to_string()))?;
                Ok(req.matches(&version))
            }

            Self::Wildcard { major, minor } => {
                if version.major != *major as u64 {
                    return Ok(false);
                }
                match minor {
                    Some(m) => Ok(version.minor == *m as u64),
                    None => Ok(true),
                }
            }

            Self::Any => Ok(true),
        }
    }

    /// Parse a constraint from a string
    pub fn parse(s: &str) -> GpackResult<Self> {
        let s = s.trim();

        if s == "*" || s.is_empty() {
            return Ok(Self::Any);
        }

        // Handle caret
        if let Some(rest) = s.strip_prefix('^') {
            return Ok(Self::Caret {
                version: rest.trim().to_string(),
            });
        }

        // Handle tilde
        if let Some(rest) = s.strip_prefix('~') {
            return Ok(Self::Tilde {
                version: rest.trim().to_string(),
            });
        }

        // Handle wildcard
        if s.ends_with(".*") || s.ends_with('*') {
            let parts: Vec<&str> = s
                .trim_end_matches('*')
                .trim_end_matches('.')
                .split('.')
                .collect();
            if parts.is_empty() || parts[0].is_empty() {
                return Ok(Self::Any);
            }
            let major = parts[0].parse::<u32>().map_err(|_| {
                GpackError::InvalidConstraint {
                    constraint: s.to_string(),
                    reason: "Invalid major version in wildcard".to_string(),
                }
            })?;
            let minor = if parts.len() > 1 {
                Some(parts[1].parse::<u32>().map_err(|_| {
                    GpackError::InvalidConstraint {
                        constraint: s.to_string(),
                        reason: "Invalid minor version in wildcard".to_string(),
                    }
                })?)
            } else {
                None
            };
            return Ok(Self::Wildcard { major, minor });
        }

        // Handle range (comma separated)
        if s.contains(',') {
            let parts: Vec<&str> = s.split(',').map(|p| p.trim()).collect();
            if parts.len() != 2 {
                return Err(GpackError::InvalidConstraint {
                    constraint: s.to_string(),
                    reason: "Range must have exactly two parts".to_string(),
                });
            }
            // Parse both parts and extract min/max
            let first = Self::parse(parts[0])?;
            let second = Self::parse(parts[1])?;

            let min = match first {
                Self::AtLeast { version } | Self::GreaterThan { version } => version,
                _ => {
                    return Err(GpackError::InvalidConstraint {
                        constraint: s.to_string(),
                        reason: "First part of range must be >= or >".to_string(),
                    });
                }
            };
            let max = match second {
                Self::LessThan { version } | Self::AtMost { version } => version,
                _ => {
                    return Err(GpackError::InvalidConstraint {
                        constraint: s.to_string(),
                        reason: "Second part of range must be < or <=".to_string(),
                    });
                }
            };

            return Ok(Self::Range { min, max });
        }

        // Handle comparison operators
        if let Some(rest) = s.strip_prefix(">=") {
            return Ok(Self::AtLeast {
                version: rest.trim().to_string(),
            });
        }
        if let Some(rest) = s.strip_prefix("<=") {
            return Ok(Self::AtMost {
                version: rest.trim().to_string(),
            });
        }
        if let Some(rest) = s.strip_prefix('>') {
            return Ok(Self::GreaterThan {
                version: rest.trim().to_string(),
            });
        }
        if let Some(rest) = s.strip_prefix('<') {
            return Ok(Self::LessThan {
                version: rest.trim().to_string(),
            });
        }
        if let Some(rest) = s.strip_prefix('=') {
            return Ok(Self::Exact {
                version: rest.trim().to_string(),
            });
        }

        // Default: exact match
        Ok(Self::Exact {
            version: s.to_string(),
        })
    }

    /// Check if this constraint is satisfiable (basic check)
    pub fn is_satisfiable(&self) -> bool {
        match self {
            Self::Range { min, max } => {
                // Try to parse and compare
                if let (Ok(min_v), Ok(max_v)) =
                    (semver::Version::parse(min), semver::Version::parse(max))
                {
                    min_v < max_v
                } else {
                    true // Assume satisfiable if can't parse
                }
            }
            _ => true,
        }
    }

    /// Get the minimum version that satisfies this constraint
    pub fn minimum_version(&self) -> Option<String> {
        match self {
            Self::Exact { version }
            | Self::AtLeast { version }
            | Self::Caret { version }
            | Self::Tilde { version } => Some(version.clone()),
            Self::GreaterThan { version } => {
                // Would need to increment, but as approximation return as-is
                Some(version.clone())
            }
            Self::Range { min, .. } => Some(min.clone()),
            Self::Wildcard { major, minor } => {
                Some(format!("{}.{}.0", major, minor.unwrap_or(0)))
            }
            Self::LessThan { .. } | Self::AtMost { .. } | Self::Any => None,
        }
    }
}

impl fmt::Display for VersionConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Exact { version } => write!(f, "={}", version),
            Self::AtLeast { version } => write!(f, ">={}", version),
            Self::LessThan { version } => write!(f, "<{}", version),
            Self::AtMost { version } => write!(f, "<={}", version),
            Self::GreaterThan { version } => write!(f, ">{}", version),
            Self::Range { min, max } => write!(f, ">={}, <{}", min, max),
            Self::Caret { version } => write!(f, "^{}", version),
            Self::Tilde { version } => write!(f, "~{}", version),
            Self::Wildcard {
                major,
                minor: Some(m),
            } => write!(f, "{}.{}.*", major, m),
            Self::Wildcard { major, minor: None } => write!(f, "{}.*", major),
            Self::Any => write!(f, "*"),
        }
    }
}

impl FromStr for VersionConstraint {
    type Err = GpackError;

    fn from_str(s: &str) -> GpackResult<Self> {
        Self::parse(s)
    }
}

/// Dependency resolver using SAT-based algorithm
#[derive(Debug)]
pub struct DependencyResolver {
    /// Resolution context
    context: ResolutionContext,
    /// Maximum resolution depth to prevent infinite loops
    max_depth: usize,
}

impl Default for DependencyResolver {
    fn default() -> Self {
        Self::new()
    }
}

impl DependencyResolver {
    /// Create a new resolver
    pub fn new() -> Self {
        Self {
            context: ResolutionContext::default(),
            max_depth: 100,
        }
    }

    /// Create resolver with custom max depth
    pub fn with_max_depth(max_depth: usize) -> Self {
        Self {
            context: ResolutionContext::default(),
            max_depth,
        }
    }

    /// Add a dependency to resolve
    pub fn add_dependency(
        &mut self,
        name: &str,
        dependency: &PackageDependency,
        source: &str,
    ) -> GpackResult<()> {
        let version_constraint = dependency
            .version()
            .ok_or_else(|| {
                GpackError::ResolutionError(format!(
                    "No version constraint for {}",
                    name
                ))
            })?
            .to_string();

        let constraint = VersionConstraintSpec {
            source: source.to_string(),
            constraint: version_constraint,
        };

        self.context
            .constraints
            .entry(name.to_string())
            .or_default()
            .push(constraint);

        Ok(())
    }

    /// Resolve all dependencies
    pub fn resolve(&mut self) -> GpackResult<Vec<ResolvedDependency>> {
        // TODO: Implement SAT-based resolution
        // 1. Convert constraints to SAT clauses
        // 2. Solve SAT problem
        // 3. Extract resolved versions
        // 4. Verify no conflicts

        // For now, return empty (stub implementation)
        Ok(self.context.resolved.values().cloned().collect())
    }

    /// Check if a version satisfies a constraint
    pub fn satisfies(version: &str, constraint: &str) -> GpackResult<bool> {
        let version = semver::Version::parse(version)
            .map_err(|e| GpackError::VersionConstraintError(e.to_string()))?;

        let req = semver::VersionReq::parse(constraint)
            .map_err(|e| GpackError::VersionConstraintError(e.to_string()))?;

        Ok(req.matches(&version))
    }

    /// Find the best matching version from a list
    pub fn find_best_match(
        versions: &[String],
        constraint: &str,
    ) -> GpackResult<Option<String>> {
        let req = semver::VersionReq::parse(constraint)
            .map_err(|e| GpackError::VersionConstraintError(e.to_string()))?;

        let mut matching: Vec<semver::Version> = versions
            .iter()
            .filter_map(|v| semver::Version::parse(v).ok())
            .filter(|v| req.matches(v))
            .collect();

        // Sort descending to get the highest matching version
        matching.sort();
        matching.reverse();

        Ok(matching.first().map(|v| v.to_string()))
    }

    /// Get current context
    pub fn context(&self) -> &ResolutionContext {
        &self.context
    }

    /// Get max depth
    pub fn max_depth(&self) -> usize {
        self.max_depth
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolver_creation() {
        let resolver = DependencyResolver::new();
        assert_eq!(resolver.max_depth(), 100);
    }

    #[test]
    fn test_version_satisfies() {
        assert!(DependencyResolver::satisfies("1.2.3", "^1.0").unwrap());
        assert!(DependencyResolver::satisfies("1.2.3", ">=1.0,<2.0").unwrap());
        assert!(!DependencyResolver::satisfies("2.0.0", "^1.0").unwrap());
    }

    #[test]
    fn test_find_best_match() {
        let versions = vec![
            "1.0.0".to_string(),
            "1.1.0".to_string(),
            "1.2.0".to_string(),
            "2.0.0".to_string(),
        ];

        let best = DependencyResolver::find_best_match(&versions, "^1.0")
            .unwrap()
            .unwrap();
        assert_eq!(best, "1.2.0");
    }

    #[test]
    fn test_find_best_match_no_match() {
        let versions = vec!["1.0.0".to_string(), "1.1.0".to_string()];

        let best = DependencyResolver::find_best_match(&versions, "^2.0").unwrap();
        assert!(best.is_none());
    }

    // T008: VersionConstraint tests
    #[test]
    fn test_constraint_exact() {
        let c = VersionConstraint::parse("1.0.0").unwrap();
        assert!(matches!(c, VersionConstraint::Exact { .. }));
        assert!(c.matches("1.0.0").unwrap());
        assert!(!c.matches("1.0.1").unwrap());
    }

    #[test]
    fn test_constraint_at_least() {
        let c = VersionConstraint::parse(">=1.0.0").unwrap();
        assert!(c.matches("1.0.0").unwrap());
        assert!(c.matches("2.0.0").unwrap());
        assert!(!c.matches("0.9.0").unwrap());
    }

    #[test]
    fn test_constraint_less_than() {
        let c = VersionConstraint::parse("<2.0.0").unwrap();
        assert!(c.matches("1.9.9").unwrap());
        assert!(!c.matches("2.0.0").unwrap());
    }

    #[test]
    fn test_constraint_range() {
        let c = VersionConstraint::parse(">=1.0.0, <2.0.0").unwrap();
        assert!(c.matches("1.0.0").unwrap());
        assert!(c.matches("1.5.0").unwrap());
        assert!(!c.matches("2.0.0").unwrap());
        assert!(!c.matches("0.9.0").unwrap());
    }

    #[test]
    fn test_constraint_caret() {
        let c = VersionConstraint::parse("^1.2.3").unwrap();
        assert!(c.matches("1.2.3").unwrap());
        assert!(c.matches("1.9.0").unwrap());
        assert!(!c.matches("2.0.0").unwrap());
    }

    #[test]
    fn test_constraint_tilde() {
        let c = VersionConstraint::parse("~1.2.3").unwrap();
        assert!(c.matches("1.2.3").unwrap());
        assert!(c.matches("1.2.9").unwrap());
        assert!(!c.matches("1.3.0").unwrap());
    }

    #[test]
    fn test_constraint_wildcard() {
        let c = VersionConstraint::parse("1.*").unwrap();
        assert!(c.matches("1.0.0").unwrap());
        assert!(c.matches("1.9.9").unwrap());
        assert!(!c.matches("2.0.0").unwrap());
    }

    #[test]
    fn test_constraint_any() {
        let c = VersionConstraint::parse("*").unwrap();
        assert!(c.matches("0.0.1").unwrap());
        assert!(c.matches("99.99.99").unwrap());
    }

    #[test]
    fn test_constraint_display() {
        let c = VersionConstraint::parse(">=1.0.0, <2.0.0").unwrap();
        assert_eq!(c.to_string(), ">=1.0.0, <2.0.0");
    }

    #[test]
    fn test_constraint_is_satisfiable() {
        let valid = VersionConstraint::Range {
            min: "1.0.0".to_string(),
            max: "2.0.0".to_string(),
        };
        assert!(valid.is_satisfiable());

        let invalid = VersionConstraint::Range {
            min: "2.0.0".to_string(),
            max: "1.0.0".to_string(),
        };
        assert!(!invalid.is_satisfiable());
    }

    #[test]
    fn test_constraint_minimum_version() {
        let c = VersionConstraint::parse(">=1.5.0").unwrap();
        assert_eq!(c.minimum_version(), Some("1.5.0".to_string()));

        let c = VersionConstraint::parse("*").unwrap();
        assert!(c.minimum_version().is_none());
    }

    #[test]
    fn test_resolved_dependency_creation() {
        let dep = ResolvedDependency::new("tokio", "1.0.0")
            .with_checksum("sha256:abc123")
            .with_features(vec!["full".to_string()]);

        assert!(dep.is_direct);
        assert_eq!(dep.checksum, "sha256:abc123");
        assert_eq!(dep.features.len(), 1);
    }

    #[test]
    fn test_resolved_dependency_transitive() {
        let dep = ResolvedDependency::new("bytes", "1.0.0").transitive();
        assert!(!dep.is_direct);
    }

    #[test]
    fn test_resolved_dependency_display() {
        let dep = ResolvedDependency::new("serde", "1.0.0");
        assert_eq!(format!("{}", dep), "serde@1.0.0");
    }
}
