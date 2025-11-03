# Registry and CacheManager Architecture Design

## Document Version
- **Version**: 1.0.0
- **Date**: 2025-11-02
- **Status**: Implementation-Ready
- **Author**: System Architect

## Executive Summary

This document provides the complete architecture design for the ggen marketplace Registry and CacheManager systems. The design is based on existing ggen-core implementations and extended to support the CLI marketplace domain requirements.

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Registry Architecture](#registry-architecture)
3. [CacheManager Architecture](#cachemanager-architecture)
4. [Package Metadata Schema](#package-metadata-schema)
5. [Index File Format](#index-file-format)
6. [Version Resolution Logic](#version-resolution-logic)
7. [Dependency Graph Structure](#dependency-graph-structure)
8. [Error Handling Strategy](#error-handling-strategy)
9. [Performance Considerations](#performance-considerations)
10. [Implementation Checklist](#implementation-checklist)

---

## Architecture Overview

### System Components

```
┌─────────────────────────────────────────────────────────────┐
│                     Marketplace CLI                          │
│  (search, install, update, list, publish commands)          │
└────────────┬─────────────────────────────────┬──────────────┘
             │                                 │
             v                                 v
┌────────────────────────┐        ┌───────────────────────────┐
│   Registry             │◄───────┤   CacheManager            │
│   (Index Management)   │        │   (Local Storage + LRU)   │
└────────────┬───────────┘        └───────────┬───────────────┘
             │                                 │
             v                                 v
┌────────────────────────┐        ┌───────────────────────────┐
│   Remote Registry      │        │   Local Cache (~/.cache)  │
│   (JSON Index)         │        │   (Pack Storage)          │
└────────────────────────┘        └───────────────────────────┘
```

### Design Principles

1. **Separation of Concerns**: Registry handles metadata/discovery, CacheManager handles local storage
2. **Lazy Loading**: Fetch metadata only when needed
3. **Deterministic Caching**: SHA-256 verification for integrity
4. **Semantic Versioning**: Full semver support with constraint resolution
5. **Backward Compatibility**: Compatible with existing ggen-core registry

---

## Registry Architecture

### Core Struct Definition

```rust
/// Registry for managing marketplace package discovery and metadata
#[derive(Debug, Clone)]
pub struct Registry {
    /// HTTP client for fetching remote index
    client: RegistryClient,

    /// Local cache for index (optional, for performance)
    index_cache: Option<RegistryIndex>,

    /// Cache TTL (time-to-live) for index refresh
    cache_ttl: std::time::Duration,

    /// Last index fetch timestamp
    last_fetch: Option<std::time::Instant>,
}
```

### Complete Method Signatures

```rust
impl Registry {
    // ==== Construction ====

    /// Create a new Registry with default registry URL
    pub fn new() -> Result<Self>;

    /// Create Registry with custom base URL (for testing/private registries)
    pub fn with_url(base_url: url::Url) -> Result<Self>;

    /// Create Registry with custom cache TTL
    pub fn with_cache_ttl(ttl: std::time::Duration) -> Result<Self>;

    // ==== Index Management ====

    /// Fetch registry index (with caching)
    /// Returns cached index if within TTL, otherwise fetches from remote
    pub async fn fetch_index(&mut self) -> Result<&RegistryIndex>;

    /// Force refresh index from remote (ignores cache)
    pub async fn refresh_index(&mut self) -> Result<&RegistryIndex>;

    /// Clear cached index
    pub fn clear_cache(&mut self);

    /// Check if cached index is still valid (within TTL)
    pub fn is_cache_valid(&self) -> bool;

    // ==== Package Discovery ====

    /// Search for packages matching query
    /// Supports text search across name, description, tags, keywords
    pub async fn search(&mut self, query: &str) -> Result<Vec<SearchResult>>;

    /// Advanced search with filters
    pub async fn search_with_filters(
        &mut self,
        query: &str,
        filters: &SearchFilters,
    ) -> Result<Vec<SearchResult>>;

    /// List all packages in registry
    pub async fn list_all(&mut self) -> Result<Vec<PackageMetadata>>;

    /// List packages by category
    pub async fn list_by_category(&mut self, category: &str) -> Result<Vec<PackageMetadata>>;

    /// Get package metadata by ID
    pub async fn get_package(&mut self, package_id: &str) -> Result<Option<PackageMetadata>>;

    /// Check if package exists
    pub async fn package_exists(&mut self, package_id: &str) -> Result<bool>;

    // ==== Version Resolution ====

    /// Resolve package to specific version
    /// Version can be:
    /// - None: Latest version
    /// - Some("1.2.3"): Exact version
    /// - Some("^1.2.0"): Semver range
    /// - Some("latest"): Alias for latest
    pub async fn resolve(
        &mut self,
        package_id: &str,
        version: Option<&str>,
    ) -> Result<ResolvedPackage>;

    /// Resolve multiple packages (for dependency resolution)
    pub async fn resolve_many(
        &mut self,
        packages: &[(String, Option<String>)],
    ) -> Result<Vec<ResolvedPackage>>;

    /// Check for updates to a package
    pub async fn check_updates(
        &mut self,
        package_id: &str,
        current_version: &str,
    ) -> Result<Option<PackageUpdate>>;

    /// List all available versions for a package
    pub async fn list_versions(&mut self, package_id: &str) -> Result<Vec<String>>;

    /// Find latest version matching semver constraint
    pub async fn find_matching_version(
        &mut self,
        package_id: &str,
        constraint: &str,
    ) -> Result<Option<String>>;

    // ==== Dependency Resolution ====

    /// Resolve dependency graph for a package
    pub async fn resolve_dependencies(
        &mut self,
        package_id: &str,
        version: Option<&str>,
    ) -> Result<DependencyGraph>;

    /// Flatten dependency graph to installation order
    pub async fn flatten_dependencies(
        &mut self,
        graph: &DependencyGraph,
    ) -> Result<Vec<ResolvedPackage>>;

    /// Check for dependency conflicts
    pub async fn check_conflicts(
        &mut self,
        graph: &DependencyGraph,
    ) -> Result<Vec<DependencyConflict>>;

    // ==== Statistics & Analytics ====

    /// Get popular categories with counts
    pub async fn get_popular_categories(&mut self) -> Result<Vec<(String, u64)>>;

    /// Get popular keywords with counts
    pub async fn get_popular_keywords(&mut self) -> Result<Vec<(String, u64)>>;

    /// Get registry statistics
    pub async fn get_stats(&mut self) -> Result<RegistryStats>;
}
```

### Supporting Types

```rust
/// Resolved package ready for installation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedPackage {
    pub id: String,
    pub name: String,
    pub version: String,
    pub git_url: String,
    pub git_rev: String,
    pub sha256: String,
    pub dependencies: Vec<(String, String)>, // (package_id, version_constraint)
}

/// Package update information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageUpdate {
    pub package_id: String,
    pub current_version: String,
    pub latest_version: String,
    pub breaking_changes: bool, // true if major version changed
    pub changelog_url: Option<String>,
}

/// Dependency conflict detected during resolution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyConflict {
    pub package_id: String,
    pub required_by: Vec<String>, // List of packages requiring this
    pub conflicting_constraints: Vec<String>, // Incompatible version constraints
    pub resolution: Option<String>, // Suggested resolution version (if possible)
}

/// Registry statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegistryStats {
    pub total_packages: usize,
    pub total_versions: usize,
    pub categories: Vec<(String, u64)>,
    pub last_updated: DateTime<Utc>,
    pub registry_version: String,
}
```

---

## CacheManager Architecture

### Core Struct Definition

```rust
/// Local cache manager for marketplace packages with LRU eviction
#[derive(Debug, Clone)]
pub struct CacheManager {
    /// Base cache directory (~/.cache/ggen/marketplace)
    cache_dir: PathBuf,

    /// Maximum cache size in bytes (default: 5GB)
    max_size: u64,

    /// LRU tracker for cache entries
    lru: Arc<Mutex<LruCache<String, CacheEntry>>>,

    /// Current cache size in bytes
    current_size: Arc<AtomicU64>,
}
```

### Complete Method Signatures

```rust
impl CacheManager {
    // ==== Construction ====

    /// Create new CacheManager with default directory
    /// Default: ~/.cache/ggen/marketplace
    pub fn new() -> Result<Self>;

    /// Create CacheManager with custom directory (testing)
    pub fn with_dir(cache_dir: PathBuf) -> Result<Self>;

    /// Create CacheManager with custom max size (in bytes)
    pub fn with_max_size(cache_dir: PathBuf, max_size: u64) -> Result<Self>;

    /// Get cache directory path
    pub fn cache_dir(&self) -> &Path;

    // ==== Cache Operations ====

    /// Write package to cache
    /// Creates directory structure: cache_dir/package_id/version/
    pub async fn write(
        &self,
        package_id: &str,
        version: &str,
        content: &[u8],
    ) -> Result<CachedPackage>;

    /// Read package from cache
    /// Returns None if not cached or expired
    pub async fn read(
        &self,
        package_id: &str,
        version: &str,
    ) -> Result<Option<CachedPackage>>;

    /// Check if package is cached
    pub fn is_cached(&self, package_id: &str, version: &str) -> bool;

    /// Get cache entry path
    pub fn get_path(&self, package_id: &str, version: &str) -> PathBuf;

    /// Remove package from cache
    pub async fn remove(&self, package_id: &str, version: &str) -> Result<()>;

    /// Clear all cache entries
    pub async fn clear_all(&self) -> Result<()>;

    // ==== Cache Management ====

    /// List all cached packages
    pub fn list_cached(&self) -> Result<Vec<CachedPackage>>;

    /// Get current cache size in bytes
    pub fn current_size(&self) -> u64;

    /// Get cache usage statistics
    pub fn get_stats(&self) -> CacheStats;

    /// Touch cache entry (update access time for LRU)
    pub fn touch(&self, package_id: &str, version: &str) -> Result<()>;

    // ==== LRU Eviction ====

    /// Evict least recently used entries until under max_size
    pub async fn evict_lru(&self) -> Result<Vec<String>>;

    /// Evict specific number of entries
    pub async fn evict_count(&self, count: usize) -> Result<Vec<String>>;

    /// Evict entries older than duration
    pub async fn evict_older_than(&self, duration: std::time::Duration) -> Result<Vec<String>>;

    /// Calculate total cache size (walks all files)
    pub async fn calculate_size(&self) -> Result<u64>;

    /// Cleanup orphaned/corrupted entries
    pub async fn cleanup(&self) -> Result<CleanupReport>;

    // ==== Version Management ====

    /// List all versions of a package in cache
    pub fn list_versions(&self, package_id: &str) -> Result<Vec<String>>;

    /// Get latest cached version of a package
    pub fn get_latest_version(&self, package_id: &str) -> Result<Option<String>>;

    /// Remove all versions of a package except latest
    pub async fn cleanup_old_versions(&self, package_id: &str) -> Result<Vec<String>>;

    // ==== Integrity ====

    /// Verify package integrity (SHA-256)
    pub async fn verify(
        &self,
        package_id: &str,
        version: &str,
        expected_sha256: &str,
    ) -> Result<bool>;

    /// Recalculate SHA-256 for cached package
    pub async fn calculate_hash(
        &self,
        package_id: &str,
        version: &str,
    ) -> Result<String>;
}
```

### Supporting Types

```rust
/// Cached package entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CachedPackage {
    pub package_id: String,
    pub version: String,
    pub path: PathBuf,
    pub sha256: String,
    pub size: u64,
    pub cached_at: DateTime<Utc>,
    pub last_accessed: DateTime<Utc>,
    pub manifest: Option<GpackMetadata>,
}

/// Cache entry for LRU tracking (internal)
#[derive(Debug, Clone)]
struct CacheEntry {
    package_id: String,
    version: String,
    size: u64,
    last_accessed: std::time::Instant,
}

/// Cache statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheStats {
    pub total_packages: usize,
    pub total_versions: usize,
    pub total_size: u64,
    pub max_size: u64,
    pub usage_percent: f64,
    pub oldest_entry: Option<DateTime<Utc>>,
    pub newest_entry: Option<DateTime<Utc>>,
}

/// Cleanup report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CleanupReport {
    pub orphaned_removed: usize,
    pub corrupted_removed: usize,
    pub space_freed: u64,
    pub errors: Vec<String>,
}
```

### LRU Implementation Details

```rust
/// LRU Cache implementation using linked-hashmap
use lru::LruCache;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicU64, Ordering};

impl CacheManager {
    /// Initialize LRU cache with capacity
    fn init_lru(&self, capacity: usize) -> LruCache<String, CacheEntry> {
        LruCache::new(capacity.try_into().unwrap_or(NonZeroUsize::new(1000).unwrap()))
    }

    /// Update LRU on cache access
    fn update_lru(&self, key: &str, entry: CacheEntry) {
        let mut lru = self.lru.lock().unwrap();
        lru.put(key.to_string(), entry);
    }

    /// Get LRU entries for eviction
    fn get_lru_entries(&self, count: usize) -> Vec<String> {
        let lru = self.lru.lock().unwrap();
        lru.iter()
            .rev() // Start from least recently used
            .take(count)
            .map(|(k, _)| k.clone())
            .collect()
    }
}
```

---

## Package Metadata Schema

### Complete Package Metadata

```rust
/// Complete package metadata from registry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageMetadata {
    // === Core Identity ===
    pub id: String,                    // Unique identifier (e.g., "io.ggen.rust.cli")
    pub name: String,                  // Display name (e.g., "Rust CLI Template")
    pub version: String,               // Current/latest version

    // === Description ===
    pub description: String,           // Short description (< 200 chars)
    pub long_description: Option<String>, // Full description (markdown)
    pub tags: Vec<String>,             // Searchable tags
    pub keywords: Vec<String>,         // Keywords for search
    pub category: Option<String>,      // Primary category

    // === Authorship ===
    pub author: Option<String>,        // Author name
    pub author_email: Option<String>,  // Author contact
    pub maintainers: Vec<String>,      // List of maintainers
    pub license: Option<String>,       // SPDX license identifier

    // === Versioning ===
    pub latest_version: String,        // Latest stable version
    pub versions: HashMap<String, VersionMetadata>, // All versions

    // === Repository ===
    pub git_url: String,               // Git repository URL
    pub homepage: Option<String>,      // Project homepage
    pub repository: Option<String>,    // Repository URL (may differ from git_url)
    pub documentation: Option<String>, // Documentation URL

    // === Statistics ===
    pub downloads: Option<u64>,        // Total download count
    pub stars: Option<u32>,            // GitHub stars (if applicable)
    pub updated: Option<DateTime<Utc>>, // Last update timestamp
    pub created: Option<DateTime<Utc>>, // Creation timestamp

    // === Dependencies ===
    pub dependencies: Vec<Dependency>, // Runtime dependencies
    pub dev_dependencies: Vec<Dependency>, // Development dependencies

    // === Compatibility ===
    pub ggen_version: Option<String>,  // Required ggen version (semver)
    pub platforms: Vec<String>,        // Supported platforms (optional)

    // === Quality Indicators ===
    pub verified: bool,                // Verified by ggen team
    pub deprecated: bool,              // Deprecated package
    pub deprecation_message: Option<String>, // Deprecation notice
}

/// Version-specific metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VersionMetadata {
    pub version: String,               // Version string (semver)
    pub git_url: String,               // Repository URL
    pub git_rev: String,               // Git revision (commit, tag, branch)
    pub sha256: String,                // SHA-256 hash of package
    pub manifest_url: Option<String>,  // Direct manifest URL
    pub published: DateTime<Utc>,      // Publish timestamp
    pub yanked: bool,                  // Whether version is yanked
    pub yank_reason: Option<String>,   // Reason for yanking
}

/// Dependency specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependency {
    pub package_id: String,            // Package identifier
    pub version: String,               // Version constraint (semver)
    pub optional: bool,                // Optional dependency
    pub features: Vec<String>,         // Required features
}
```

---

## Index File Format

### JSON Structure

```json
{
  "version": "1.0.0",
  "updated": "2025-11-02T10:00:00Z",
  "packages": {
    "io.ggen.rust.cli": {
      "id": "io.ggen.rust.cli",
      "name": "Rust CLI Template",
      "version": "1.2.0",
      "description": "Production-ready Rust CLI template with clap",
      "long_description": "# Rust CLI Template\n\nA comprehensive CLI template...",
      "tags": ["rust", "cli", "template"],
      "keywords": ["command-line", "clap", "rust"],
      "category": "cli",
      "author": "ggen Contributors",
      "author_email": "maintainers@ggen.dev",
      "maintainers": ["user1", "user2"],
      "license": "MIT",
      "latest_version": "1.2.0",
      "versions": {
        "1.2.0": {
          "version": "1.2.0",
          "git_url": "https://github.com/ggen/templates.git",
          "git_rev": "v1.2.0",
          "sha256": "abc123...",
          "manifest_url": "https://example.com/manifest.json",
          "published": "2025-11-01T12:00:00Z",
          "yanked": false,
          "yank_reason": null
        },
        "1.1.0": {
          "version": "1.1.0",
          "git_url": "https://github.com/ggen/templates.git",
          "git_rev": "v1.1.0",
          "sha256": "def456...",
          "manifest_url": null,
          "published": "2025-10-01T12:00:00Z",
          "yanked": false,
          "yank_reason": null
        }
      },
      "git_url": "https://github.com/ggen/templates.git",
      "homepage": "https://ggen.dev/templates/rust-cli",
      "repository": "https://github.com/ggen/templates",
      "documentation": "https://docs.ggen.dev/templates/rust-cli",
      "downloads": 12500,
      "stars": 450,
      "updated": "2025-11-01T12:00:00Z",
      "created": "2024-01-15T10:00:00Z",
      "dependencies": [
        {
          "package_id": "io.ggen.rust.common",
          "version": "^1.0.0",
          "optional": false,
          "features": []
        }
      ],
      "dev_dependencies": [],
      "ggen_version": ">=2.0.0",
      "platforms": ["linux", "macos", "windows"],
      "verified": true,
      "deprecated": false,
      "deprecation_message": null
    }
  },
  "categories": {
    "cli": {
      "name": "Command-Line Tools",
      "description": "Templates for CLI applications",
      "count": 45
    },
    "web": {
      "name": "Web Development",
      "description": "Web application templates",
      "count": 78
    }
  },
  "metadata": {
    "total_packages": 150,
    "total_versions": 523,
    "total_downloads": 1500000
  }
}
```

### Index Validation Schema (JSON Schema)

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "required": ["version", "updated", "packages"],
  "properties": {
    "version": {
      "type": "string",
      "pattern": "^[0-9]+\\.[0-9]+\\.[0-9]+$"
    },
    "updated": {
      "type": "string",
      "format": "date-time"
    },
    "packages": {
      "type": "object",
      "patternProperties": {
        "^[a-z0-9\\-\\.]+$": {
          "$ref": "#/definitions/package"
        }
      }
    }
  },
  "definitions": {
    "package": {
      "type": "object",
      "required": ["id", "name", "version", "description", "latest_version", "versions"],
      "properties": {
        "id": { "type": "string" },
        "name": { "type": "string" },
        "version": { "type": "string" },
        "description": { "type": "string", "maxLength": 200 },
        "latest_version": { "type": "string" },
        "versions": {
          "type": "object",
          "patternProperties": {
            "^[0-9]+\\.[0-9]+\\.[0-9]+": {
              "$ref": "#/definitions/version"
            }
          }
        }
      }
    },
    "version": {
      "type": "object",
      "required": ["version", "git_url", "git_rev", "sha256"],
      "properties": {
        "version": { "type": "string" },
        "git_url": { "type": "string", "format": "uri" },
        "git_rev": { "type": "string" },
        "sha256": { "type": "string", "pattern": "^[a-f0-9]{64}$" }
      }
    }
  }
}
```

---

## Version Resolution Logic

### Semver Constraint Resolution

```rust
use semver::{Version, VersionReq};

impl Registry {
    /// Resolve version constraint to specific version
    pub async fn resolve_version_constraint(
        &mut self,
        package_id: &str,
        constraint: &str,
    ) -> Result<String> {
        // Get package metadata
        let package = self.get_package(package_id).await?
            .ok_or_else(|| Error::new(&format!("Package not found: {}", package_id)))?;

        // Handle special cases
        match constraint {
            "latest" | "*" => return Ok(package.latest_version.clone()),
            _ => {}
        }

        // Parse semver constraint
        let requirement = VersionReq::parse(constraint)
            .map_err(|e| Error::new(&format!("Invalid version constraint: {}", e)))?;

        // Collect all non-yanked versions
        let mut versions: Vec<Version> = package.versions
            .iter()
            .filter(|(_, meta)| !meta.yanked)
            .filter_map(|(ver, _)| Version::parse(ver).ok())
            .collect();

        // Sort in descending order (newest first)
        versions.sort_by(|a, b| b.cmp(a));

        // Find first matching version
        for version in versions {
            if requirement.matches(&version) {
                return Ok(version.to_string());
            }
        }

        Err(Error::new(&format!(
            "No version of '{}' matches constraint '{}'",
            package_id, constraint
        )))
    }
}
```

### Constraint Examples

| Constraint | Meaning | Example Match |
|------------|---------|---------------|
| `1.2.3` | Exact version | `1.2.3` |
| `^1.2.3` | Compatible (^) | `1.2.3`, `1.2.4`, `1.9.9` (not `2.0.0`) |
| `~1.2.3` | Tilde (~) | `1.2.3`, `1.2.4` (not `1.3.0`) |
| `>=1.2.0` | Greater or equal | `1.2.0`, `1.3.0`, `2.0.0` |
| `>1.0.0,<2.0.0` | Range | `1.0.1`, `1.9.9` (not `2.0.0`) |
| `*` or `latest` | Latest version | Most recent non-yanked |

---

## Dependency Graph Structure

### Graph Representation

```rust
use std::collections::{HashMap, HashSet};

/// Dependency graph for package resolution
#[derive(Debug, Clone)]
pub struct DependencyGraph {
    /// Root package being installed
    pub root: DependencyNode,

    /// All nodes in the graph (package_id -> node)
    pub nodes: HashMap<String, DependencyNode>,

    /// Resolved versions (package_id -> version)
    pub resolved: HashMap<String, String>,
}

/// Single node in dependency graph
#[derive(Debug, Clone)]
pub struct DependencyNode {
    pub package_id: String,
    pub version: String,
    pub constraint: String, // Original constraint requested
    pub dependencies: Vec<DependencyEdge>,
    pub depth: usize, // Distance from root
}

/// Edge in dependency graph
#[derive(Debug, Clone)]
pub struct DependencyEdge {
    pub target: String,      // Target package_id
    pub constraint: String,  // Version constraint
    pub optional: bool,
}

impl DependencyGraph {
    /// Create new empty graph
    pub fn new(root_package: String, root_version: String) -> Self {
        let root = DependencyNode {
            package_id: root_package.clone(),
            version: root_version.clone(),
            constraint: root_version.clone(),
            dependencies: vec![],
            depth: 0,
        };

        let mut nodes = HashMap::new();
        nodes.insert(root_package.clone(), root.clone());

        let mut resolved = HashMap::new();
        resolved.insert(root_package, root_version);

        Self { root, nodes, resolved }
    }

    /// Add node to graph
    pub fn add_node(&mut self, node: DependencyNode) {
        self.nodes.insert(node.package_id.clone(), node);
    }

    /// Get installation order (topological sort)
    pub fn topological_sort(&self) -> Result<Vec<String>> {
        let mut sorted = Vec::new();
        let mut visited = HashSet::new();
        let mut temp_mark = HashSet::new();

        // Visit all nodes
        for package_id in self.nodes.keys() {
            if !visited.contains(package_id) {
                self.visit(package_id, &mut visited, &mut temp_mark, &mut sorted)?;
            }
        }

        // Reverse to get correct order (dependencies first)
        sorted.reverse();
        Ok(sorted)
    }

    /// DFS visit for topological sort
    fn visit(
        &self,
        package_id: &str,
        visited: &mut HashSet<String>,
        temp_mark: &mut HashSet<String>,
        sorted: &mut Vec<String>,
    ) -> Result<()> {
        if temp_mark.contains(package_id) {
            return Err(Error::new(&format!(
                "Circular dependency detected: {}",
                package_id
            )));
        }

        if visited.contains(package_id) {
            return Ok(());
        }

        temp_mark.insert(package_id.to_string());

        // Visit dependencies
        if let Some(node) = self.nodes.get(package_id) {
            for edge in &node.dependencies {
                if !edge.optional {
                    self.visit(&edge.target, visited, temp_mark, sorted)?;
                }
            }
        }

        temp_mark.remove(package_id);
        visited.insert(package_id.to_string());
        sorted.push(package_id.to_string());

        Ok(())
    }

    /// Detect conflicts in resolved versions
    pub fn detect_conflicts(&self) -> Vec<DependencyConflict> {
        let mut conflicts = Vec::new();

        // For each package, check all constraints
        let mut constraints: HashMap<String, Vec<(String, String)>> = HashMap::new();

        for (package_id, node) in &self.nodes {
            for edge in &node.dependencies {
                constraints
                    .entry(edge.target.clone())
                    .or_insert_with(Vec::new)
                    .push((package_id.clone(), edge.constraint.clone()));
            }
        }

        // Check if all constraints are satisfied
        for (target, requirements) in constraints {
            if let Some(resolved_version) = self.resolved.get(&target) {
                let version = match Version::parse(resolved_version) {
                    Ok(v) => v,
                    Err(_) => continue,
                };

                let mut incompatible = Vec::new();
                let mut required_by = Vec::new();

                for (requirer, constraint) in requirements {
                    required_by.push(requirer.clone());

                    if let Ok(req) = VersionReq::parse(&constraint) {
                        if !req.matches(&version) {
                            incompatible.push(constraint);
                        }
                    }
                }

                if !incompatible.is_empty() {
                    conflicts.push(DependencyConflict {
                        package_id: target,
                        required_by,
                        conflicting_constraints: incompatible,
                        resolution: None,
                    });
                }
            }
        }

        conflicts
    }
}
```

### Dependency Resolution Algorithm

```rust
impl Registry {
    /// Resolve full dependency graph
    pub async fn resolve_dependencies(
        &mut self,
        package_id: &str,
        version: Option<&str>,
    ) -> Result<DependencyGraph> {
        // Resolve root package
        let resolved = self.resolve(package_id, version).await?;
        let mut graph = DependencyGraph::new(
            resolved.id.clone(),
            resolved.version.clone(),
        );

        // BFS to resolve all dependencies
        let mut queue = vec![(resolved.id.clone(), resolved.version.clone(), 0)];
        let mut processed = HashSet::new();

        while let Some((pkg_id, pkg_ver, depth)) = queue.pop() {
            let key = format!("{}@{}", pkg_id, pkg_ver);
            if processed.contains(&key) {
                continue;
            }
            processed.insert(key);

            // Get package metadata
            let package = match self.get_package(&pkg_id).await? {
                Some(p) => p,
                None => continue,
            };

            // Get version metadata
            let version_meta = match package.versions.get(&pkg_ver) {
                Some(v) => v,
                None => continue,
            };

            // Create node
            let mut node = DependencyNode {
                package_id: pkg_id.clone(),
                version: pkg_ver.clone(),
                constraint: pkg_ver.clone(),
                dependencies: vec![],
                depth,
            };

            // Process dependencies
            for dep in &package.dependencies {
                // Resolve dependency version
                let dep_version = self
                    .resolve_version_constraint(&dep.package_id, &dep.version)
                    .await?;

                // Add edge
                node.dependencies.push(DependencyEdge {
                    target: dep.package_id.clone(),
                    constraint: dep.version.clone(),
                    optional: dep.optional,
                });

                // Add to resolved set
                graph.resolved.insert(dep.package_id.clone(), dep_version.clone());

                // Queue for processing
                if !dep.optional {
                    queue.push((dep.package_id.clone(), dep_version, depth + 1));
                }
            }

            graph.add_node(node);
        }

        Ok(graph)
    }
}
```

---

## Error Handling Strategy

### Error Types

```rust
use thiserror::Error;

/// Registry and cache-specific errors
#[derive(Error, Debug)]
pub enum MarketplaceError {
    // === Registry Errors ===
    #[error("Registry unavailable: {0}")]
    RegistryUnavailable(String),

    #[error("Package not found: {0}")]
    PackageNotFound(String),

    #[error("Version not found: {package}@{version}")]
    VersionNotFound { package: String, version: String },

    #[error("Invalid version constraint: {0}")]
    InvalidVersionConstraint(String),

    #[error("Index parse error: {0}")]
    IndexParseError(String),

    // === Cache Errors ===
    #[error("Cache error: {0}")]
    CacheError(String),

    #[error("Cache full: {used}/{max} bytes")]
    CacheFull { used: u64, max: u64 },

    #[error("Cache corruption: {0}")]
    CacheCorruption(String),

    #[error("Hash mismatch: expected {expected}, got {actual}")]
    HashMismatch { expected: String, actual: String },

    // === Dependency Errors ===
    #[error("Circular dependency: {0}")]
    CircularDependency(String),

    #[error("Dependency conflict: {0}")]
    DependencyConflict(String),

    #[error("Unsatisfied dependency: {0}")]
    UnsatisfiedDependency(String),

    // === Network Errors ===
    #[error("Network error: {0}")]
    NetworkError(#[from] reqwest::Error),

    #[error("Timeout: {0}")]
    Timeout(String),

    // === I/O Errors ===
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),
}

/// Convert to ggen_utils::error::Error
impl From<MarketplaceError> for ggen_utils::error::Error {
    fn from(err: MarketplaceError) -> Self {
        ggen_utils::error::Error::new(&err.to_string())
    }
}
```

### Error Context Pattern

```rust
use anyhow::{Context, Result};

impl Registry {
    pub async fn example_method(&mut self) -> Result<()> {
        // Use .context() for better error messages
        self.fetch_index()
            .await
            .context("Failed to fetch registry index")?;

        // Use .with_context() for dynamic messages
        let package = self.get_package("my-package")
            .await
            .with_context(|| format!("Failed to get package metadata for 'my-package'"))?;

        Ok(())
    }
}
```

### Retry Logic

```rust
/// Retry with exponential backoff
async fn retry_with_backoff<F, Fut, T>(
    mut operation: F,
    max_attempts: u32,
) -> Result<T>
where
    F: FnMut() -> Fut,
    Fut: std::future::Future<Output = Result<T>>,
{
    let mut attempts = 0;
    let mut last_error = None;

    while attempts < max_attempts {
        match operation().await {
            Ok(result) => return Ok(result),
            Err(e) => {
                attempts += 1;
                last_error = Some(e);

                if attempts < max_attempts {
                    let backoff_ms = 100 * 2u64.pow(attempts - 1);
                    tokio::time::sleep(std::time::Duration::from_millis(backoff_ms)).await;
                }
            }
        }
    }

    Err(last_error.unwrap_or_else(|| anyhow::anyhow!("Operation failed")))
}
```

---

## Performance Considerations

### 1. Index Caching

**Strategy**: Cache index in memory with TTL to reduce network calls

```rust
// Cache for 5 minutes by default
const DEFAULT_INDEX_TTL: Duration = Duration::from_secs(300);

impl Registry {
    async fn fetch_index(&mut self) -> Result<&RegistryIndex> {
        // Check if cache is valid
        if self.is_cache_valid() {
            return Ok(self.index_cache.as_ref().unwrap());
        }

        // Fetch fresh index
        let index = self.client.fetch_index().await?;
        self.index_cache = Some(index);
        self.last_fetch = Some(std::time::Instant::now());

        Ok(self.index_cache.as_ref().unwrap())
    }
}
```

**Performance Impact**: Reduces latency from ~200ms to <1ms for cached requests

### 2. LRU Eviction

**Strategy**: Implement efficient LRU with linked hashmap

```rust
use lru::LruCache;

impl CacheManager {
    /// Evict using O(1) LRU cache
    pub async fn evict_lru(&self) -> Result<Vec<String>> {
        let mut evicted = Vec::new();
        let target_size = (self.max_size as f64 * 0.8) as u64; // Evict to 80%

        let mut lru = self.lru.lock().unwrap();

        while self.current_size.load(Ordering::Relaxed) > target_size {
            if let Some((key, entry)) = lru.pop_lru() {
                // Remove from filesystem
                self.remove_internal(&entry.package_id, &entry.version).await?;
                evicted.push(key);
            } else {
                break;
            }
        }

        Ok(evicted)
    }
}
```

**Performance Impact**: O(1) eviction vs O(n log n) for sorted approaches

### 3. Parallel Downloads

**Strategy**: Use tokio for concurrent package downloads

```rust
impl Registry {
    pub async fn download_multiple(
        &mut self,
        packages: Vec<ResolvedPackage>,
    ) -> Result<Vec<CachedPackage>> {
        use futures::stream::{self, StreamExt};

        // Download up to 4 packages concurrently
        let results: Vec<_> = stream::iter(packages)
            .map(|pkg| async move {
                // Download logic here
            })
            .buffer_unordered(4)
            .collect()
            .await;

        // Check for errors
        results.into_iter().collect()
    }
}
```

**Performance Impact**: 4x speedup for multi-package installations

### 4. Streaming Hash Calculation

**Strategy**: Stream files to avoid loading entire package in memory

```rust
use tokio::io::AsyncReadExt;
use sha2::{Digest, Sha256};

impl CacheManager {
    pub async fn calculate_hash_streaming(
        &self,
        path: &Path,
    ) -> Result<String> {
        let mut hasher = Sha256::new();
        let mut file = tokio::fs::File::open(path).await?;
        let mut buffer = vec![0u8; 8192]; // 8KB chunks

        loop {
            let n = file.read(&mut buffer).await?;
            if n == 0 {
                break;
            }
            hasher.update(&buffer[..n]);
        }

        Ok(format!("{:x}", hasher.finalize()))
    }
}
```

**Performance Impact**: Constant memory usage regardless of package size

### 5. Lazy Dependency Resolution

**Strategy**: Only resolve dependencies when needed (not during search)

```rust
impl Registry {
    // Search returns basic metadata (fast)
    pub async fn search(&mut self, query: &str) -> Result<Vec<SearchResult>> {
        // No dependency resolution here
    }

    // Resolve dependencies only during install (lazy)
    pub async fn install_with_deps(&mut self, package_id: &str) -> Result<()> {
        let graph = self.resolve_dependencies(package_id, None).await?;
        // ... install
    }
}
```

**Performance Impact**: Search operations ~10x faster

### Performance Benchmarks (Target)

| Operation | Target Latency | Notes |
|-----------|----------------|-------|
| Search (cached) | < 10ms | In-memory index search |
| Search (cold) | < 300ms | Network fetch + parse |
| Resolve version | < 5ms | Semver matching |
| Check cache | < 1ms | Filesystem stat |
| Download package (1MB) | < 2s | Network dependent |
| Hash calculation (1MB) | < 50ms | Streaming hash |
| LRU eviction (1 entry) | < 10ms | O(1) operation |
| Dependency resolution (10 deps) | < 100ms | BFS traversal |

---

## Implementation Checklist

### Phase 1: Core Registry (Priority: High)

- [ ] Implement `Registry` struct with `RegistryClient` delegation
- [ ] Add index caching with TTL
- [ ] Implement search methods
- [ ] Add version resolution logic
- [ ] Write unit tests for version constraints
- [ ] Add integration tests with mock index

### Phase 2: CacheManager (Priority: High)

- [ ] Implement `CacheManager` struct
- [ ] Add LRU cache using `lru` crate
- [ ] Implement cache read/write operations
- [ ] Add SHA-256 verification
- [ ] Implement eviction strategies
- [ ] Write unit tests for cache operations
- [ ] Add integration tests with temp directories

### Phase 3: Dependency Resolution (Priority: Medium)

- [ ] Implement `DependencyGraph` struct
- [ ] Add topological sort algorithm
- [ ] Implement conflict detection
- [ ] Add circular dependency detection
- [ ] Write tests for complex dependency scenarios
- [ ] Add benchmarks for graph operations

### Phase 4: Error Handling (Priority: Medium)

- [ ] Define `MarketplaceError` enum
- [ ] Add context to all error paths
- [ ] Implement retry logic for network operations
- [ ] Add error recovery strategies
- [ ] Write tests for error scenarios

### Phase 5: Performance Optimization (Priority: Low)

- [ ] Add parallel downloads
- [ ] Implement streaming hash calculation
- [ ] Optimize index parsing
- [ ] Add caching at multiple levels
- [ ] Run performance benchmarks
- [ ] Profile and optimize hot paths

### Phase 6: Documentation (Priority: Medium)

- [ ] Add rustdoc comments to all public APIs
- [ ] Create usage examples
- [ ] Write integration guide
- [ ] Document error handling patterns
- [ ] Add performance tuning guide

---

## Appendix: Dependencies Required

Add to `cli/Cargo.toml`:

```toml
[dependencies]
# Existing
ggen-core = { path = "../ggen-core", version = "2.2.0" }
ggen-utils = { path = "../utils", version = "2.2.0" }

# Semver for version resolution
semver = "1.0"

# LRU cache
lru = "0.12"

# SHA-256 hashing
sha2 = "0.10"

# Async utilities
tokio = { workspace = true }
futures = { workspace = true }

# HTTP client (from ggen-core's RegistryClient)
reqwest = { workspace = true }
url = "2.5"

# Serialization (existing)
serde = { workspace = true }
serde_json = { workspace = true }

# Error handling (existing)
anyhow = { workspace = true }
thiserror = { workspace = true }

# Date/time (from ggen-core)
chrono = { workspace = true }

# Logging
tracing = { workspace = true }
```

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2025-11-02 | System Architect | Initial architecture design |

---

**End of Architecture Design Document**
