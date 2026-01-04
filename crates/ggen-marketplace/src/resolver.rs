//! Dependency resolver engine for marketplace package installation
//!
//! Features:
//! - Constraint satisfaction algorithm for version ranges
//! - Build resolution tree: packages -> resolved versions
//! - Detect conflicts early: incompatible versions
//! - Return detailed error on conflict with suggestions
//! - Handle cycles and missing packages gracefully

use crate::error::{Error, Result};
use crate::models::{Package, PackageDependency, PackageId, PackageVersion};
use crate::traits::AsyncRepository;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashSet, VecDeque};
use tracing::{debug, error, info, warn};

/// Version constraint for dependency resolution
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum VersionConstraint {
    /// Exact version match: =1.0.0
    Exact(PackageVersion),
    /// Minimum version: >=1.0.0
    Minimum(PackageVersion),
    /// Maximum version: <=1.0.0
    Maximum(PackageVersion),
    /// Range: >=1.0.0,<2.0.0
    Range {
        min: Option<PackageVersion>,
        max: Option<PackageVersion>,
    },
    /// Caret: ^1.0.0 (compatible with 1.x.x)
    Caret(PackageVersion),
    /// Tilde: ~1.0.0 (compatible with 1.0.x)
    Tilde(PackageVersion),
    /// Any version: *
    Any,
}

impl VersionConstraint {
    /// Parse a version requirement string into a constraint
    pub fn parse(req: &str) -> Result<Self> {
        let req = req.trim();

        if req.is_empty() || req == "*" {
            return Ok(Self::Any);
        }

        if req.starts_with('^') {
            let version = PackageVersion::new(req.trim_start_matches('^'))?;
            return Ok(Self::Caret(version));
        }

        if req.starts_with('~') {
            let version = PackageVersion::new(req.trim_start_matches('~'))?;
            return Ok(Self::Tilde(version));
        }

        if req.starts_with(">=") {
            let version = PackageVersion::new(req.trim_start_matches(">=").trim())?;
            return Ok(Self::Minimum(version));
        }

        if req.starts_with("<=") {
            let version = PackageVersion::new(req.trim_start_matches("<=").trim())?;
            return Ok(Self::Maximum(version));
        }

        if req.starts_with('=') {
            let version = PackageVersion::new(req.trim_start_matches('=').trim())?;
            return Ok(Self::Exact(version));
        }

        // Try to parse as exact version
        let version = PackageVersion::new(req)?;
        Ok(Self::Exact(version))
    }

    /// Check if a version satisfies this constraint
    pub fn satisfies(&self, version: &PackageVersion) -> bool {
        match self {
            Self::Exact(v) => version == v,
            Self::Minimum(v) => version >= v,
            Self::Maximum(v) => version <= v,
            Self::Range { min, max } => {
                let min_ok = min.as_ref().map_or(true, |m| version >= m);
                let max_ok = max.as_ref().map_or(true, |m| version <= m);
                min_ok && max_ok
            }
            Self::Caret(v) => {
                // Compatible with same major version
                let v_parts = Self::parse_version_parts(v.as_str());
                let check_parts = Self::parse_version_parts(version.as_str());
                v_parts.0 == check_parts.0 && version >= v
            }
            Self::Tilde(v) => {
                // Compatible with same major.minor
                let v_parts = Self::parse_version_parts(v.as_str());
                let check_parts = Self::parse_version_parts(version.as_str());
                v_parts.0 == check_parts.0 && v_parts.1 == check_parts.1 && version >= v
            }
            Self::Any => true,
        }
    }

    fn parse_version_parts(version: &str) -> (u32, u32, u32) {
        let normalized = version.strip_prefix('v').unwrap_or(version);
        let parts: Vec<u32> = normalized
            .split(|c: char| !c.is_numeric())
            .take(3)
            .filter_map(|p| p.parse().ok())
            .collect();

        (
            parts.first().copied().unwrap_or(0),
            parts.get(1).copied().unwrap_or(0),
            parts.get(2).copied().unwrap_or(0),
        )
    }
}

/// A resolved dependency with its version
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedDependency {
    /// Package ID
    pub id: PackageId,
    /// Resolved version
    pub version: PackageVersion,
    /// Checksum of the package
    pub checksum: String,
    /// Download URL
    pub download_url: String,
    /// Dependencies of this package
    pub dependencies: Vec<PackageId>,
    /// Resolution depth (0 = root)
    pub depth: usize,
}

/// Resolution tree node
#[derive(Debug, Clone)]
pub struct ResolutionNode {
    /// Package ID
    pub id: PackageId,
    /// Resolved version
    pub version: PackageVersion,
    /// Children (dependencies)
    pub children: Vec<ResolutionNode>,
}

/// Conflict between dependencies
#[derive(Debug, Clone)]
pub struct DependencyConflict {
    /// Package that has conflict
    pub package_id: PackageId,
    /// Version required by first requester
    pub version_a: PackageVersion,
    /// First requester
    pub requester_a: PackageId,
    /// Version required by second requester
    pub version_b: PackageVersion,
    /// Second requester
    pub requester_b: PackageId,
    /// Suggestions to resolve conflict
    pub suggestions: Vec<String>,
}

impl std::fmt::Display for DependencyConflict {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Conflict for '{}': {} requires {}, but {} requires {}",
            self.package_id, self.requester_a, self.version_a,
            self.requester_b, self.version_b
        )
    }
}

/// Resolution result
#[derive(Debug, Clone)]
pub struct ResolutionResult {
    /// Successfully resolved dependencies (sorted by install order)
    pub resolved: Vec<ResolvedDependency>,
    /// Resolution tree for visualization
    pub tree: ResolutionNode,
    /// Total packages resolved
    pub total_packages: usize,
    /// Resolution time
    pub resolution_time: std::time::Duration,
    /// Timestamp
    pub resolved_at: DateTime<Utc>,
}

/// Dependency resolver engine
pub struct DependencyResolver<R: AsyncRepository> {
    repository: R,
    max_depth: usize,
    max_iterations: usize,
}

impl<R: AsyncRepository> DependencyResolver<R> {
    /// Create a new dependency resolver
    pub fn new(repository: R) -> Self {
        Self {
            repository,
            max_depth: 100,
            max_iterations: 10000,
        }
    }

    /// Set maximum resolution depth
    pub fn with_max_depth(mut self, depth: usize) -> Self {
        self.max_depth = depth;
        self
    }

    /// Set maximum iterations for resolution
    pub fn with_max_iterations(mut self, iterations: usize) -> Self {
        self.max_iterations = iterations;
        self
    }

    /// Resolve dependencies for a package
    pub async fn resolve(
        &self,
        root_id: &PackageId,
        root_version: &PackageVersion,
    ) -> Result<ResolutionResult> {
        let start_time = std::time::Instant::now();

        info!(
            package = %root_id,
            version = %root_version,
            "Starting dependency resolution"
        );

        // Track visited packages to detect cycles
        let mut visited: HashSet<PackageId> = HashSet::new();
        // Track version selections
        let mut selections: BTreeMap<PackageId, (PackageVersion, PackageId)> = BTreeMap::new();
        // Queue for BFS resolution
        let mut queue: VecDeque<(PackageId, PackageVersion, PackageId, usize)> = VecDeque::new();

        // Start with root package
        queue.push_back((root_id.clone(), root_version.clone(), root_id.clone(), 0));
        selections.insert(root_id.clone(), (root_version.clone(), root_id.clone()));

        let mut iterations = 0;
        let mut conflicts: Vec<DependencyConflict> = Vec::new();

        while let Some((pkg_id, version, requester, depth)) = queue.pop_front() {
            iterations += 1;

            if iterations > self.max_iterations {
                error!("Resolution exceeded maximum iterations: {}", self.max_iterations);
                return Err(Error::DependencyResolutionFailed {
                    package_id: root_id.to_string(),
                    reason: format!(
                        "Resolution exceeded maximum iterations ({})",
                        self.max_iterations
                    ),
                });
            }

            if depth > self.max_depth {
                warn!(
                    package = %pkg_id,
                    depth = %depth,
                    "Maximum resolution depth exceeded"
                );
                continue;
            }

            // Skip if already visited at this resolution
            if visited.contains(&pkg_id) {
                // Check for version conflict
                if let Some((selected_version, selected_by)) = selections.get(&pkg_id) {
                    if *selected_version != version {
                        // Conflict detected
                        conflicts.push(DependencyConflict {
                            package_id: pkg_id.clone(),
                            version_a: selected_version.clone(),
                            requester_a: selected_by.clone(),
                            version_b: version,
                            requester_b: requester.clone(),
                            suggestions: vec![
                                format!("Try using version {} for all dependents", selected_version),
                                format!("Update {} to use a compatible version range", requester),
                            ],
                        });
                    }
                }
                continue;
            }

            visited.insert(pkg_id.clone());

            debug!(
                package = %pkg_id,
                version = %version,
                depth = %depth,
                "Resolving dependencies"
            );

            // Get package from repository
            let package = match self.repository.get_package_version(&pkg_id, &version).await {
                Ok(pkg) => pkg,
                Err(e) => {
                    if pkg_id == *root_id {
                        // Root package not found is fatal
                        return Err(e);
                    }
                    warn!(
                        package = %pkg_id,
                        version = %version,
                        error = %e,
                        "Optional dependency not found, skipping"
                    );
                    continue;
                }
            };

            // Process dependencies
            for release in package.releases.values() {
                for dep in &release.dependencies {
                    let constraint = VersionConstraint::parse(&dep.version_req)?;

                    // Find best matching version
                    let dep_version = self.find_best_version(&dep.id, &constraint).await?;

                    selections.insert(dep.id.clone(), (dep_version.clone(), pkg_id.clone()));
                    queue.push_back((
                        dep.id.clone(),
                        dep_version,
                        pkg_id.clone(),
                        depth + 1,
                    ));
                }
            }
        }

        // Check for unresolved conflicts
        if !conflicts.is_empty() {
            let conflict_messages: Vec<String> = conflicts
                .iter()
                .map(|c| c.to_string())
                .collect();

            return Err(Error::DependencyResolutionFailed {
                package_id: root_id.to_string(),
                reason: format!(
                    "Dependency conflicts detected:\n{}",
                    conflict_messages.join("\n")
                ),
            });
        }

        // Build resolved dependencies list in dependency order
        let resolved = self.build_resolved_list(&selections, root_id).await?;

        // Build resolution tree
        let tree = self.build_resolution_tree(root_id, root_version, &selections, &HashSet::new()).await?;

        let resolution_time = start_time.elapsed();

        info!(
            package = %root_id,
            total_resolved = %resolved.len(),
            time_ms = %resolution_time.as_millis(),
            "Dependency resolution complete"
        );

        Ok(ResolutionResult {
            total_packages: resolved.len(),
            resolved,
            tree,
            resolution_time,
            resolved_at: Utc::now(),
        })
    }

    /// Find the best matching version for a constraint
    async fn find_best_version(
        &self,
        package_id: &PackageId,
        constraint: &VersionConstraint,
    ) -> Result<PackageVersion> {
        let versions = self.repository.list_versions(package_id).await?;

        // Find all versions that satisfy the constraint
        let mut matching: Vec<&PackageVersion> = versions
            .iter()
            .filter(|v| constraint.satisfies(v))
            .collect();

        if matching.is_empty() {
            return Err(Error::DependencyResolutionFailed {
                package_id: package_id.to_string(),
                reason: format!(
                    "No version found satisfying constraint {:?}",
                    constraint
                ),
            });
        }

        // Sort descending (highest version first)
        matching.sort();
        matching.reverse();

        // Return the highest matching version
        Ok(matching[0].clone())
    }

    /// Build the resolved dependencies list in install order
    async fn build_resolved_list(
        &self,
        selections: &BTreeMap<PackageId, (PackageVersion, PackageId)>,
        root_id: &PackageId,
    ) -> Result<Vec<ResolvedDependency>> {
        let mut resolved = Vec::new();
        let mut visited: HashSet<PackageId> = HashSet::new();

        // Topological sort for correct install order
        let mut stack: Vec<(PackageId, usize)> = vec![(root_id.clone(), 0)];
        let mut order: Vec<(PackageId, PackageVersion, usize)> = Vec::new();

        while let Some((pkg_id, depth)) = stack.pop() {
            if visited.contains(&pkg_id) {
                continue;
            }
            visited.insert(pkg_id.clone());

            if let Some((version, _)) = selections.get(&pkg_id) {
                // Get package info
                if let Ok(package) = self.repository.get_package_version(&pkg_id, version).await {
                    // Push dependencies first (for topological order)
                    for release in package.releases.values() {
                        for dep in &release.dependencies {
                            if !visited.contains(&dep.id) {
                                stack.push((dep.id.clone(), depth + 1));
                            }
                        }
                    }
                }

                order.push((pkg_id.clone(), version.clone(), depth));
            }
        }

        // Reverse for install order (dependencies first)
        order.reverse();

        for (pkg_id, version, depth) in order {
            if let Ok(package) = self.repository.get_package_version(&pkg_id, &version).await {
                let release = package.releases.values().next();

                resolved.push(ResolvedDependency {
                    id: pkg_id.clone(),
                    version,
                    checksum: release.map_or_else(String::new, |r| r.checksum.clone()),
                    download_url: release.map_or_else(String::new, |r| r.download_url.clone()),
                    dependencies: release
                        .map_or_else(Vec::new, |r| r.dependencies.iter().map(|d| d.id.clone()).collect()),
                    depth,
                });
            }
        }

        Ok(resolved)
    }

    /// Build the resolution tree recursively
    async fn build_resolution_tree(
        &self,
        pkg_id: &PackageId,
        version: &PackageVersion,
        selections: &BTreeMap<PackageId, (PackageVersion, PackageId)>,
        visited: &HashSet<PackageId>,
    ) -> Result<ResolutionNode> {
        let mut new_visited = visited.clone();
        new_visited.insert(pkg_id.clone());

        let mut children = Vec::new();

        if let Ok(package) = self.repository.get_package_version(pkg_id, version).await {
            for release in package.releases.values() {
                for dep in &release.dependencies {
                    if !visited.contains(&dep.id) {
                        if let Some((dep_version, _)) = selections.get(&dep.id) {
                            if let Ok(child) = Box::pin(self.build_resolution_tree(
                                &dep.id,
                                dep_version,
                                selections,
                                &new_visited,
                            )).await {
                                children.push(child);
                            }
                        }
                    }
                }
            }
        }

        Ok(ResolutionNode {
            id: pkg_id.clone(),
            version: version.clone(),
            children,
        })
    }

    /// Detect cycles in dependencies
    pub async fn detect_cycles(&self, root_id: &PackageId) -> Result<Vec<Vec<PackageId>>> {
        let mut cycles: Vec<Vec<PackageId>> = Vec::new();
        let mut visited: HashSet<PackageId> = HashSet::new();
        let mut path: Vec<PackageId> = Vec::new();
        let mut rec_stack: HashSet<PackageId> = HashSet::new();

        self.detect_cycles_dfs(root_id, &mut visited, &mut path, &mut rec_stack, &mut cycles).await?;

        Ok(cycles)
    }

    async fn detect_cycles_dfs(
        &self,
        pkg_id: &PackageId,
        visited: &mut HashSet<PackageId>,
        path: &mut Vec<PackageId>,
        rec_stack: &mut HashSet<PackageId>,
        cycles: &mut Vec<Vec<PackageId>>,
    ) -> Result<()> {
        visited.insert(pkg_id.clone());
        rec_stack.insert(pkg_id.clone());
        path.push(pkg_id.clone());

        if let Ok(package) = self.repository.get_package(pkg_id).await {
            for release in package.releases.values() {
                for dep in &release.dependencies {
                    if !visited.contains(&dep.id) {
                        Box::pin(self.detect_cycles_dfs(&dep.id, visited, path, rec_stack, cycles)).await?;
                    } else if rec_stack.contains(&dep.id) {
                        // Found cycle
                        let cycle_start = path.iter().position(|p| p == &dep.id).unwrap_or(0);
                        let cycle: Vec<PackageId> = path[cycle_start..].to_vec();
                        cycles.push(cycle);
                    }
                }
            }
        }

        path.pop();
        rec_stack.remove(pkg_id);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::{PackageMetadata, ReleaseInfo};
    use crate::registry::Registry;

    fn create_test_package(
        id: &str,
        version: &str,
        dependencies: Vec<(&str, &str)>,
    ) -> Package {
        let pkg_id = PackageId::new(id).unwrap();
        let pkg_version = PackageVersion::new(version).unwrap();
        let metadata = PackageMetadata::new(
            pkg_id.clone(),
            id,
            format!("Test package {}", id),
            "MIT",
        );

        let deps: Vec<PackageDependency> = dependencies
            .into_iter()
            .map(|(dep_id, dep_ver)| PackageDependency {
                id: PackageId::new(dep_id).unwrap(),
                version_req: dep_ver.to_string(),
                optional: false,
            })
            .collect();

        let mut releases = indexmap::IndexMap::new();
        releases.insert(
            pkg_version.clone(),
            ReleaseInfo {
                version: pkg_version.clone(),
                released_at: Utc::now(),
                changelog: "Test release".to_string(),
                checksum: "a".repeat(64),
                download_url: format!("https://example.com/{}.tar.gz", id),
                dependencies: deps,
            },
        );

        Package {
            metadata,
            latest_version: pkg_version.clone(),
            versions: vec![pkg_version],
            releases,
        }
    }

    #[test]
    fn test_version_constraint_parse() {
        assert!(matches!(
            VersionConstraint::parse("*").unwrap(),
            VersionConstraint::Any
        ));

        assert!(matches!(
            VersionConstraint::parse("^1.0.0").unwrap(),
            VersionConstraint::Caret(_)
        ));

        assert!(matches!(
            VersionConstraint::parse("~1.0.0").unwrap(),
            VersionConstraint::Tilde(_)
        ));

        assert!(matches!(
            VersionConstraint::parse(">=1.0.0").unwrap(),
            VersionConstraint::Minimum(_)
        ));

        assert!(matches!(
            VersionConstraint::parse("1.0.0").unwrap(),
            VersionConstraint::Exact(_)
        ));
    }

    #[test]
    fn test_version_constraint_satisfies() {
        let v100 = PackageVersion::new("1.0.0").unwrap();
        let v110 = PackageVersion::new("1.1.0").unwrap();
        let v200 = PackageVersion::new("2.0.0").unwrap();

        // Any
        assert!(VersionConstraint::Any.satisfies(&v100));
        assert!(VersionConstraint::Any.satisfies(&v200));

        // Exact
        let exact = VersionConstraint::Exact(v100.clone());
        assert!(exact.satisfies(&v100));
        assert!(!exact.satisfies(&v110));

        // Minimum
        let min = VersionConstraint::Minimum(v100.clone());
        assert!(min.satisfies(&v100));
        assert!(min.satisfies(&v110));
        assert!(min.satisfies(&v200));

        // Caret (compatible with major)
        let caret = VersionConstraint::Caret(v100.clone());
        assert!(caret.satisfies(&v100));
        assert!(caret.satisfies(&v110));
        assert!(!caret.satisfies(&v200)); // Different major

        // Tilde (compatible with minor)
        let tilde = VersionConstraint::Tilde(v100.clone());
        assert!(tilde.satisfies(&v100));
        assert!(!tilde.satisfies(&v110)); // Different minor
    }

    #[tokio::test]
    async fn test_simple_resolution() {
        let registry = Registry::new(100).await;

        // Create test packages: A -> B -> C
        let pkg_c = create_test_package("pkg-c", "1.0.0", vec![]);
        let pkg_b = create_test_package("pkg-b", "1.0.0", vec![("pkg-c", "^1.0.0")]);
        let pkg_a = create_test_package("pkg-a", "1.0.0", vec![("pkg-b", "^1.0.0")]);

        registry.insert(pkg_c).unwrap();
        registry.insert(pkg_b).unwrap();
        registry.insert(pkg_a).unwrap();

        let resolver = DependencyResolver::new(registry);
        let root_id = PackageId::new("pkg-a").unwrap();
        let root_version = PackageVersion::new("1.0.0").unwrap();

        let result = resolver.resolve(&root_id, &root_version).await.unwrap();

        assert_eq!(result.total_packages, 3);
        assert!(result.resolved.iter().any(|r| r.id.as_str() == "pkg-a"));
        assert!(result.resolved.iter().any(|r| r.id.as_str() == "pkg-b"));
        assert!(result.resolved.iter().any(|r| r.id.as_str() == "pkg-c"));
    }

    #[tokio::test]
    async fn test_resolution_with_shared_dependency() {
        let registry = Registry::new(100).await;

        // A -> B, A -> C, B -> D, C -> D (diamond pattern)
        let pkg_d = create_test_package("pkg-d", "1.0.0", vec![]);
        let pkg_c = create_test_package("pkg-c", "1.0.0", vec![("pkg-d", "^1.0.0")]);
        let pkg_b = create_test_package("pkg-b", "1.0.0", vec![("pkg-d", "^1.0.0")]);
        let pkg_a = create_test_package("pkg-a", "1.0.0", vec![
            ("pkg-b", "^1.0.0"),
            ("pkg-c", "^1.0.0"),
        ]);

        registry.insert(pkg_d).unwrap();
        registry.insert(pkg_c).unwrap();
        registry.insert(pkg_b).unwrap();
        registry.insert(pkg_a).unwrap();

        let resolver = DependencyResolver::new(registry);
        let root_id = PackageId::new("pkg-a").unwrap();
        let root_version = PackageVersion::new("1.0.0").unwrap();

        let result = resolver.resolve(&root_id, &root_version).await.unwrap();

        // D should only be resolved once
        let d_count = result.resolved.iter().filter(|r| r.id.as_str() == "pkg-d").count();
        assert_eq!(d_count, 1);
    }

    #[tokio::test]
    async fn test_missing_package_error() {
        let registry = Registry::new(100).await;

        let resolver = DependencyResolver::new(registry);
        let root_id = PackageId::new("nonexistent").unwrap();
        let root_version = PackageVersion::new("1.0.0").unwrap();

        let result = resolver.resolve(&root_id, &root_version).await;

        assert!(result.is_err());
    }
}
