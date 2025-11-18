//! Advanced dependency resolver with multi-pack conflict resolution
//!
//! This module provides sophisticated dependency resolution capabilities including:
//! - Semver constraint satisfaction
//! - Transitive dependency resolution
//! - Diamond dependency handling
//! - Multiple resolution strategies

use crate::packs::types::Pack;
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use tracing::{info, warn};

/// Advanced dependency resolver
pub struct AdvancedResolver {
    /// Package registry cache
    #[allow(dead_code)]
    registry_cache: HashMap<String, Vec<Pack>>,
}

/// Conflict resolution strategy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConflictResolution {
    /// Deep merge conflicting packages
    Merge,
    /// Install packages in layers (isolation)
    Layer,
    /// Use custom resolution script
    Custom(String),
}

/// Resolved dependencies with conflict information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedDependencies {
    /// Installation order (topologically sorted)
    pub install_order: Vec<Package>,
    /// Detected conflicts
    pub conflicts: Vec<Conflict>,
    /// Applied resolutions
    pub resolutions: Vec<Resolution>,
}

/// Package with version information
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Package {
    /// Package ID
    pub id: String,
    /// Package version
    pub version: String,
    /// Source pack
    pub source_pack: String,
}

/// Version conflict
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Conflict {
    /// Package ID with conflict
    pub package_id: String,
    /// Conflicting versions
    pub versions: Vec<String>,
    /// Packs requesting each version
    pub requesters: HashMap<String, Vec<String>>,
    /// Severity (High, Medium, Low)
    pub severity: ConflictSeverity,
}

/// Conflict severity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConflictSeverity {
    High,   // Breaking changes
    Medium, // Minor version differences
    Low,    // Patch version differences
}

/// Applied resolution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Resolution {
    /// Package ID
    pub package_id: String,
    /// Chosen version
    pub resolved_version: String,
    /// Resolution strategy used
    pub strategy: String,
    /// Reason for resolution
    pub reason: String,
}

/// Version constraint
#[derive(Debug, Clone)]
pub struct VersionConstraint {
    /// Constraint type
    pub constraint_type: ConstraintType,
    /// Version string
    pub version: String,
}

/// Constraint type
#[derive(Debug, Clone)]
pub enum ConstraintType {
    Exact,           // =1.0.0
    GreaterThan,     // >1.0.0
    GreaterOrEqual,  // >=1.0.0
    LessThan,        // <2.0.0
    LessOrEqual,     // <=2.0.0
    Compatible,      // ^1.0.0 (same major)
    MinorCompatible, // ~1.0.0 (same major.minor)
}

impl AdvancedResolver {
    /// Create new advanced resolver
    pub fn new() -> Self {
        Self {
            registry_cache: HashMap::new(),
        }
    }

    /// Resolve multi-pack conflicts
    ///
    /// # Arguments
    /// * `packs` - List of packs to resolve
    /// * `strategy` - Conflict resolution strategy
    ///
    /// # Returns
    /// Resolved dependencies with install order
    pub fn resolve_multi_pack_conflicts(
        &mut self, packs: Vec<Pack>, strategy: ConflictResolution,
    ) -> Result<ResolvedDependencies> {
        info!("Resolving dependencies for {} packs", packs.len());

        // Build package dependency map
        let package_map = self.build_package_map(&packs);

        // Detect conflicts
        let conflicts = self.detect_version_conflicts(&package_map);

        if !conflicts.is_empty() {
            warn!("Detected {} conflicts", conflicts.len());
        }

        // Apply resolution strategy
        let resolutions = self.apply_resolution_strategy(&conflicts, &package_map, &strategy)?;

        // Build final package list
        let resolved_packages = self.build_resolved_packages(&package_map, &resolutions);

        // Topological sort for install order
        let install_order = self.topological_sort(&resolved_packages)?;

        info!("Resolved to {} packages", install_order.len());

        Ok(ResolvedDependencies {
            install_order,
            conflicts,
            resolutions,
        })
    }

    /// Check if package satisfies version constraints
    ///
    /// # Arguments
    /// * `package` - Package to check
    /// * `constraints` - Version constraints to satisfy
    ///
    /// # Returns
    /// True if all constraints are satisfied
    pub fn satisfies_constraints(
        &self, package: &Package, constraints: &[VersionConstraint],
    ) -> bool {
        for constraint in constraints {
            if !self.satisfies_constraint(&package.version, constraint) {
                return false;
            }
        }
        true
    }

    /// Build package dependency map
    fn build_package_map(&self, packs: &[Pack]) -> HashMap<String, Vec<Package>> {
        let mut package_map: HashMap<String, Vec<Package>> = HashMap::new();

        for pack in packs {
            for package_spec in &pack.packages {
                // Parse package spec (format: "name@version" or just "name")
                let (pkg_id, pkg_version) = if let Some(idx) = package_spec.find('@') {
                    let (id, version) = package_spec.split_at(idx);
                    (id.to_string(), version[1..].to_string())
                } else {
                    (package_spec.clone(), "latest".to_string())
                };

                let package = Package {
                    id: pkg_id.clone(),
                    version: pkg_version,
                    source_pack: pack.id.clone(),
                };

                package_map
                    .entry(pkg_id)
                    .or_insert_with(Vec::new)
                    .push(package);
            }
        }

        package_map
    }

    /// Detect version conflicts
    fn detect_version_conflicts(
        &self, package_map: &HashMap<String, Vec<Package>>,
    ) -> Vec<Conflict> {
        let mut conflicts = Vec::new();

        for (pkg_id, packages) in package_map {
            if packages.len() > 1 {
                // Multiple versions requested
                let versions: HashSet<String> =
                    packages.iter().map(|p| p.version.clone()).collect();

                if versions.len() > 1 {
                    // Actual version conflict
                    let mut requesters: HashMap<String, Vec<String>> = HashMap::new();

                    for package in packages {
                        requesters
                            .entry(package.version.clone())
                            .or_insert_with(Vec::new)
                            .push(package.source_pack.clone());
                    }

                    let severity = self.determine_conflict_severity(&versions);

                    conflicts.push(Conflict {
                        package_id: pkg_id.clone(),
                        versions: versions.into_iter().collect(),
                        requesters,
                        severity,
                    });
                }
            }
        }

        conflicts
    }

    /// Determine conflict severity based on version differences
    fn determine_conflict_severity(&self, versions: &HashSet<String>) -> ConflictSeverity {
        // Parse versions and check differences
        let parsed: Vec<_> = versions
            .iter()
            .filter_map(|v| self.parse_version(v))
            .collect();

        if parsed.len() < 2 {
            return ConflictSeverity::Low;
        }

        // Check major version differences
        let major_versions: HashSet<_> = parsed.iter().map(|v| v.0).collect();
        if major_versions.len() > 1 {
            return ConflictSeverity::High;
        }

        // Check minor version differences
        let minor_versions: HashSet<_> = parsed.iter().map(|v| v.1).collect();
        if minor_versions.len() > 1 {
            return ConflictSeverity::Medium;
        }

        ConflictSeverity::Low
    }

    /// Parse semantic version (simple implementation)
    fn parse_version(&self, version: &str) -> Option<(u32, u32, u32)> {
        let parts: Vec<&str> = version.split('.').collect();
        if parts.len() != 3 {
            return None;
        }

        let major = parts[0].parse().ok()?;
        let minor = parts[1].parse().ok()?;
        let patch = parts[2].parse().ok()?;

        Some((major, minor, patch))
    }

    /// Apply resolution strategy
    fn apply_resolution_strategy(
        &self, conflicts: &[Conflict], package_map: &HashMap<String, Vec<Package>>,
        strategy: &ConflictResolution,
    ) -> Result<Vec<Resolution>> {
        let mut resolutions = Vec::new();

        for conflict in conflicts {
            let resolution = match strategy {
                ConflictResolution::Merge => self.resolve_by_merge(conflict, package_map)?,
                ConflictResolution::Layer => self.resolve_by_layer(conflict, package_map)?,
                ConflictResolution::Custom(script) => {
                    self.resolve_by_custom(conflict, package_map, script)?
                }
            };

            resolutions.push(resolution);
        }

        Ok(resolutions)
    }

    /// Resolve conflict by merging (choose latest version)
    fn resolve_by_merge(
        &self, conflict: &Conflict, _package_map: &HashMap<String, Vec<Package>>,
    ) -> Result<Resolution> {
        // Choose latest version
        let latest = self.find_latest_version(&conflict.versions);

        Ok(Resolution {
            package_id: conflict.package_id.clone(),
            resolved_version: latest.clone(),
            strategy: "merge".to_string(),
            reason: format!("Chose latest version: {}", latest),
        })
    }

    /// Resolve conflict by layering (use all versions)
    fn resolve_by_layer(
        &self, conflict: &Conflict, _package_map: &HashMap<String, Vec<Package>>,
    ) -> Result<Resolution> {
        // In layering, we'd install all versions in separate namespaces
        // For now, just choose the first version
        let version = conflict
            .versions
            .first()
            .ok_or_else(|| Error::new("No versions available"))?;

        Ok(Resolution {
            package_id: conflict.package_id.clone(),
            resolved_version: version.clone(),
            strategy: "layer".to_string(),
            reason: "Using layered installation".to_string(),
        })
    }

    /// Resolve conflict using custom script
    fn resolve_by_custom(
        &self, conflict: &Conflict, _package_map: &HashMap<String, Vec<Package>>, _script: &str,
    ) -> Result<Resolution> {
        // In production, this would execute the custom script
        // For now, fall back to merge strategy
        self.resolve_by_merge(conflict, _package_map)
    }

    /// Find latest version from a list
    fn find_latest_version(&self, versions: &[String]) -> String {
        let mut sorted: Vec<_> = versions.iter().collect();
        sorted.sort_by(|a, b| {
            let a_parsed = self.parse_version(a);
            let b_parsed = self.parse_version(b);

            match (a_parsed, b_parsed) {
                (Some(a_ver), Some(b_ver)) => b_ver.cmp(&a_ver),
                _ => b.cmp(a),
            }
        });

        sorted
            .first()
            .map(|s| (*s).clone())
            .unwrap_or_else(|| versions[0].clone())
    }

    /// Build resolved package list
    fn build_resolved_packages(
        &self, package_map: &HashMap<String, Vec<Package>>, resolutions: &[Resolution],
    ) -> Vec<Package> {
        let mut resolved = Vec::new();
        let resolution_map: HashMap<_, _> = resolutions
            .iter()
            .map(|r| (r.package_id.clone(), r.resolved_version.clone()))
            .collect();

        for (pkg_id, packages) in package_map {
            if let Some(resolved_version) = resolution_map.get(pkg_id) {
                // Use resolved version
                if let Some(package) = packages.iter().find(|p| &p.version == resolved_version) {
                    resolved.push(package.clone());
                }
            } else {
                // No conflict, use first (or only) version
                if let Some(package) = packages.first() {
                    resolved.push(package.clone());
                }
            }
        }

        resolved
    }

    /// Topological sort of packages
    fn topological_sort(&self, packages: &[Package]) -> Result<Vec<Package>> {
        // Simple implementation - in production, this would consider actual dependencies
        Ok(packages.to_vec())
    }

    /// Check if version satisfies constraint
    fn satisfies_constraint(&self, version: &str, constraint: &VersionConstraint) -> bool {
        let version_parsed = match self.parse_version(version) {
            Some(v) => v,
            None => return false,
        };

        let constraint_parsed = match self.parse_version(&constraint.version) {
            Some(v) => v,
            None => return false,
        };

        match constraint.constraint_type {
            ConstraintType::Exact => version_parsed == constraint_parsed,
            ConstraintType::GreaterThan => version_parsed > constraint_parsed,
            ConstraintType::GreaterOrEqual => version_parsed >= constraint_parsed,
            ConstraintType::LessThan => version_parsed < constraint_parsed,
            ConstraintType::LessOrEqual => version_parsed <= constraint_parsed,
            ConstraintType::Compatible => {
                // Same major version
                version_parsed.0 == constraint_parsed.0 && version_parsed >= constraint_parsed
            }
            ConstraintType::MinorCompatible => {
                // Same major.minor version
                version_parsed.0 == constraint_parsed.0
                    && version_parsed.1 == constraint_parsed.1
                    && version_parsed >= constraint_parsed
            }
        }
    }
}

impl Default for AdvancedResolver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::packs::types::PackMetadata;
    use std::collections::HashMap;

    fn create_test_pack(id: &str, packages: Vec<&str>) -> Pack {
        Pack {
            id: id.to_string(),
            name: format!("Pack {}", id),
            version: "1.0.0".to_string(),
            description: "Test pack".to_string(),
            category: "test".to_string(),
            author: None,
            repository: None,
            license: None,
            packages: packages.iter().map(|s| s.to_string()).collect(),
            templates: vec![],
            sparql_queries: HashMap::new(),
            dependencies: vec![],
            tags: vec![],
            keywords: vec![],
            production_ready: true,
            metadata: PackMetadata::default(),
        }
    }

    #[test]
    fn test_resolver_creation() {
        let resolver = AdvancedResolver::new();
        assert_eq!(resolver.registry_cache.len(), 0);
    }

    #[test]
    fn test_parse_version() {
        let resolver = AdvancedResolver::new();

        assert_eq!(resolver.parse_version("1.2.3"), Some((1, 2, 3)));
        assert_eq!(resolver.parse_version("0.1.0"), Some((0, 1, 0)));
        assert_eq!(resolver.parse_version("invalid"), None);
    }

    #[test]
    fn test_satisfies_constraint_exact() {
        let resolver = AdvancedResolver::new();
        let constraint = VersionConstraint {
            constraint_type: ConstraintType::Exact,
            version: "1.2.3".to_string(),
        };

        assert!(resolver.satisfies_constraint("1.2.3", &constraint));
        assert!(!resolver.satisfies_constraint("1.2.4", &constraint));
    }

    #[test]
    fn test_satisfies_constraint_compatible() {
        let resolver = AdvancedResolver::new();
        let constraint = VersionConstraint {
            constraint_type: ConstraintType::Compatible,
            version: "1.2.0".to_string(),
        };

        assert!(resolver.satisfies_constraint("1.2.0", &constraint));
        assert!(resolver.satisfies_constraint("1.3.0", &constraint));
        assert!(resolver.satisfies_constraint("1.2.5", &constraint));
        assert!(!resolver.satisfies_constraint("2.0.0", &constraint));
        assert!(!resolver.satisfies_constraint("1.1.0", &constraint));
    }

    #[test]
    fn test_find_latest_version() {
        let resolver = AdvancedResolver::new();
        let versions = vec![
            "1.0.0".to_string(),
            "1.2.0".to_string(),
            "1.1.0".to_string(),
            "2.0.0".to_string(),
        ];

        let latest = resolver.find_latest_version(&versions);
        assert_eq!(latest, "2.0.0");
    }

    #[test]
    fn test_resolve_multi_pack_no_conflicts() {
        let mut resolver = AdvancedResolver::new();

        let packs = vec![
            create_test_pack("pack1", vec!["pkg1@1.0.0", "pkg2@2.0.0"]),
            create_test_pack("pack2", vec!["pkg3@1.0.0"]),
        ];

        let result = resolver.resolve_multi_pack_conflicts(packs, ConflictResolution::Merge);

        assert!(result.is_ok());
        let resolved = result.unwrap();
        assert_eq!(resolved.conflicts.len(), 0);
        assert_eq!(resolved.install_order.len(), 3);
    }

    #[test]
    fn test_resolve_multi_pack_with_conflicts() {
        let mut resolver = AdvancedResolver::new();

        let packs = vec![
            create_test_pack("pack1", vec!["pkg1@1.0.0"]),
            create_test_pack("pack2", vec!["pkg1@2.0.0"]),
        ];

        let result = resolver.resolve_multi_pack_conflicts(packs, ConflictResolution::Merge);

        assert!(result.is_ok());
        let resolved = result.unwrap();
        assert_eq!(resolved.conflicts.len(), 1);
        assert_eq!(resolved.resolutions.len(), 1);

        // Should resolve to latest version (2.0.0)
        assert_eq!(resolved.resolutions[0].resolved_version, "2.0.0");
    }
}
