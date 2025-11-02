//! Domain logic for marketplace package installation
//!
//! This module contains the core business logic for installing packages,
//! separated from CLI concerns for better testability and reusability.

use clap::Args;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};
use std::fs;
use std::io::Read;

/// Install command arguments
#[derive(Debug, Args)]
pub struct InstallArgs {
    /// Package name (format: name@version)
    pub package: String,

    /// Target directory
    #[arg(short = 't', long)]
    pub target: Option<String>,

    /// Force overwrite
    #[arg(short = 'f', long)]
    pub force: bool,

    /// Skip dependencies
    #[arg(long)]
    pub no_dependencies: bool,

    /// Dry run (simulate installation)
    #[arg(long)]
    pub dry_run: bool,
}

/// Package installation options
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct InstallOptions {
    pub package_name: String,
    pub version: Option<String>,
    pub target_path: Option<PathBuf>,
    pub force: bool,
    pub with_dependencies: bool,
    pub dry_run: bool,
}

impl InstallOptions {
    pub fn new(package_name: impl Into<String>) -> Self {
        Self {
            package_name: package_name.into(),
            with_dependencies: true,
            ..Default::default()
        }
    }

    pub fn with_version(mut self, version: impl Into<String>) -> Self {
        self.version = Some(version.into());
        self
    }

    pub fn with_target(mut self, path: PathBuf) -> Self {
        self.target_path = Some(path);
        self
    }

    pub fn force(mut self) -> Self {
        self.force = true;
        self
    }

    pub fn dry_run(mut self) -> Self {
        self.dry_run = true;
        self
    }

    pub fn no_dependencies(mut self) -> Self {
        self.with_dependencies = false;
        self
    }
}

/// Install package result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstallResult {
    pub package_name: String,
    pub version: String,
    pub install_path: PathBuf,
    pub dependencies_installed: Vec<String>,
}

/// Package manifest structure (matches publish.rs)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageManifest {
    pub name: String,
    pub version: String,
    pub title: String,
    pub description: String,
    #[serde(default)]
    pub dependencies: HashMap<String, String>,
    #[serde(default)]
    pub categories: Vec<String>,
    #[serde(default)]
    pub tags: Vec<String>,
}

/// Lockfile entry for installed packages
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockfileEntry {
    pub name: String,
    pub version: String,
    pub resolved: String,
    pub integrity: Option<String>,
    #[serde(default)]
    pub dependencies: HashMap<String, String>,
}

/// Lockfile structure
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Lockfile {
    pub version: String,
    pub packages: HashMap<String, LockfileEntry>,
}

/// Dependency resolution context
#[derive(Debug)]
struct DependencyGraph {
    nodes: HashMap<String, PackageNode>,
}

#[derive(Debug, Clone)]
struct PackageNode {
    name: String,
    version: String,
    dependencies: HashMap<String, String>,
}

impl DependencyGraph {
    fn new() -> Self {
        Self {
            nodes: HashMap::new(),
        }
    }

    fn add_package(&mut self, manifest: PackageManifest) {
        let key = format!("{}@{}", manifest.name, manifest.version);
        self.nodes.insert(
            key,
            PackageNode {
                name: manifest.name.clone(),
                version: manifest.version.clone(),
                dependencies: manifest.dependencies.clone(),
            },
        );
    }

    /// Detect circular dependencies using DFS
    fn detect_circular(&self) -> Result<()> {
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();

        for key in self.nodes.keys() {
            if !visited.contains(key) {
                self.dfs_cycle_check(key, &mut visited, &mut rec_stack)?;
            }
        }

        Ok(())
    }

    fn dfs_cycle_check(
        &self,
        node: &str,
        visited: &mut HashSet<String>,
        rec_stack: &mut HashSet<String>,
    ) -> Result<()> {
        visited.insert(node.to_string());
        rec_stack.insert(node.to_string());

        if let Some(pkg) = self.nodes.get(node) {
            for (dep_name, dep_version) in &pkg.dependencies {
                let dep_key = format!("{}@{}", dep_name, dep_version);

                if !visited.contains(&dep_key) {
                    self.dfs_cycle_check(&dep_key, visited, rec_stack)?;
                } else if rec_stack.contains(&dep_key) {
                    return Err(ggen_utils::error::Error::new(&format!(
                        "Circular dependency detected: {} -> {}",
                        node, dep_key
                    )));
                }
            }
        }

        rec_stack.remove(node);
        Ok(())
    }

    /// Topological sort for install order
    fn topological_sort(&self) -> Result<Vec<String>> {
        let mut in_degree: HashMap<String, usize> = HashMap::new();
        let mut adj_list: HashMap<String, Vec<String>> = HashMap::new();

        // Initialize
        for key in self.nodes.keys() {
            in_degree.insert(key.clone(), 0);
            adj_list.insert(key.clone(), Vec::new());
        }

        // Build adjacency list and in-degree count
        for (key, node) in &self.nodes {
            for (dep_name, dep_version) in &node.dependencies {
                let dep_key = format!("{}@{}", dep_name, dep_version);
                if self.nodes.contains_key(&dep_key) {
                    adj_list.get_mut(&dep_key).unwrap().push(key.clone());
                    *in_degree.get_mut(key).unwrap() += 1;
                }
            }
        }

        // Kahn's algorithm
        let mut queue: VecDeque<String> = in_degree
            .iter()
            .filter(|(_, &degree)| degree == 0)
            .map(|(k, _)| k.clone())
            .collect();

        let mut result = Vec::new();

        while let Some(node) = queue.pop_front() {
            result.push(node.clone());

            if let Some(neighbors) = adj_list.get(&node) {
                for neighbor in neighbors {
                    let degree = in_degree.get_mut(neighbor).unwrap();
                    *degree -= 1;
                    if *degree == 0 {
                        queue.push_back(neighbor.clone());
                    }
                }
            }
        }

        if result.len() != self.nodes.len() {
            return Err(ggen_utils::error::Error::new(
                "Dependency cycle detected during topological sort",
            ));
        }

        Ok(result)
    }
}

/// Parse version from package specification (pkg@version or pkg@^1.0.0)
fn parse_package_spec(spec: &str) -> (String, String) {
    match spec.rsplit_once('@') {
        Some((name, version)) => (name.to_string(), version.to_string()),
        None => (spec.to_string(), "latest".to_string()),
    }
}

/// Resolve version from semver range
fn resolve_version(
    package_name: &str,
    version_spec: &str,
    registry_path: &Path,
) -> Result<String> {
    // For "latest", return the highest version
    if version_spec == "latest" {
        return get_latest_version(package_name, registry_path);
    }

    // Handle exact version
    if !version_spec.starts_with('^')
        && !version_spec.starts_with('~')
        && !version_spec.starts_with('>')
        && !version_spec.starts_with('<')
        && !version_spec.starts_with('=') {
        return Ok(version_spec.to_string());
    }

    // Handle semver ranges (simplified implementation)
    let available_versions = get_available_versions(package_name, registry_path)?;

    if version_spec.starts_with('^') {
        // Caret: ^1.2.3 means >=1.2.3 <2.0.0
        let base_version = version_spec.trim_start_matches('^');
        resolve_caret_range(base_version, &available_versions)
    } else if version_spec.starts_with('~') {
        // Tilde: ~1.2.3 means >=1.2.3 <1.3.0
        let base_version = version_spec.trim_start_matches('~');
        resolve_tilde_range(base_version, &available_versions)
    } else if version_spec.starts_with(">=") {
        let base_version = version_spec.trim_start_matches(">=");
        resolve_gte_range(base_version, &available_versions)
    } else {
        Ok(version_spec.to_string())
    }
}

/// Get latest version from registry
fn get_latest_version(package_name: &str, registry_path: &Path) -> Result<String> {
    let versions = get_available_versions(package_name, registry_path)?;
    versions
        .last()
        .ok_or_else(|| ggen_utils::error::Error::new(&format!("No versions found for {}", package_name)))
        .map(|v| v.to_string())
}

/// Get all available versions for a package
fn get_available_versions(package_name: &str, registry_path: &Path) -> Result<Vec<String>> {
    let index_path = registry_path.join("index.json");

    if !index_path.exists() {
        return Err(ggen_utils::error::Error::new(&format!(
            "Package {} not found in registry",
            package_name
        )));
    }

    let content = std::fs::read_to_string(&index_path)?;
    let index: serde_json::Value = serde_json::from_str(&content)?;

    let versions: Vec<String> = index
        .get("packages")
        .and_then(|p| p.get(package_name))
        .and_then(|v| v.as_array())
        .ok_or_else(|| ggen_utils::error::Error::new(&format!(
            "Package {} not found in registry",
            package_name
        )))?
        .iter()
        .filter_map(|v| v.get("version").and_then(|ver| ver.as_str()))
        .map(String::from)
        .collect();

    if versions.is_empty() {
        return Err(ggen_utils::error::Error::new(&format!(
            "No versions found for package {}",
            package_name
        )));
    }

    Ok(versions)
}

/// Resolve caret range (^1.2.3 means >=1.2.3 <2.0.0)
fn resolve_caret_range(base: &str, versions: &[String]) -> Result<String> {
    let parts: Vec<&str> = base.split('.').collect();
    if parts.is_empty() {
        return Err(ggen_utils::error::Error::new("Invalid version format"));
    }

    let major: u32 = parts[0].parse().map_err(|_| ggen_utils::error::Error::new("Invalid major version"))?;

    versions
        .iter()
        .filter(|v| {
            let v_parts: Vec<&str> = v.split('.').collect();
            if v_parts.is_empty() {
                return false;
            }
            let v_major: u32 = v_parts[0].parse().unwrap_or(0);
            v_major == major && v.as_str() >= base
        })
        .last()
        .ok_or_else(|| ggen_utils::error::Error::new(&format!("No matching version for ^{}", base)))
        .map(|v| v.to_string())
}

/// Resolve tilde range (~1.2.3 means >=1.2.3 <1.3.0)
fn resolve_tilde_range(base: &str, versions: &[String]) -> Result<String> {
    let parts: Vec<&str> = base.split('.').collect();
    if parts.len() < 2 {
        return Err(ggen_utils::error::Error::new("Invalid version format"));
    }

    let major: u32 = parts[0].parse().map_err(|_| ggen_utils::error::Error::new("Invalid major version"))?;
    let minor: u32 = parts[1].parse().map_err(|_| ggen_utils::error::Error::new("Invalid minor version"))?;

    versions
        .iter()
        .filter(|v| {
            let v_parts: Vec<&str> = v.split('.').collect();
            if v_parts.len() < 2 {
                return false;
            }
            let v_major: u32 = v_parts[0].parse().unwrap_or(0);
            let v_minor: u32 = v_parts[1].parse().unwrap_or(0);
            v_major == major && v_minor == minor && v.as_str() >= base
        })
        .last()
        .ok_or_else(|| ggen_utils::error::Error::new(&format!("No matching version for ~{}", base)))
        .map(|v| v.to_string())
}

/// Resolve >= range
fn resolve_gte_range(base: &str, versions: &[String]) -> Result<String> {
    versions
        .iter()
        .filter(|v| v.as_str() >= base)
        .last()
        .ok_or_else(|| ggen_utils::error::Error::new(&format!("No matching version for >={}", base)))
        .map(|v| v.to_string())
}

/// Load package manifest from registry
async fn load_package_manifest(
    package_name: &str,
    version: &str,
    registry_path: &Path,
) -> Result<PackageManifest> {
    let package_dir = registry_path.join(package_name).join(version);
    let manifest_path = package_dir.join("package.json");

    if !manifest_path.exists() {
        return Err(ggen_utils::error::Error::new(&format!(
            "Package {}@{} not found in registry",
            package_name, version
        )));
    }

    let content = tokio::fs::read_to_string(&manifest_path).await?;
    let manifest: PackageManifest = serde_json::from_str(&content)?;

    Ok(manifest)
}

/// Extract tarball to target directory
async fn extract_tarball(tarball_path: &Path, target_dir: &Path) -> Result<()> {
    if !tarball_path.exists() {
        return Err(ggen_utils::error::Error::new(&format!(
            "Tarball not found: {}",
            tarball_path.display()
        )));
    }

    // Create target directory
    tokio::fs::create_dir_all(target_dir).await?;

    // For real implementation, use tar/gz extraction
    // This is a simplified version
    let file = fs::File::open(tarball_path)?;
    let decoder = flate2::read::GzDecoder::new(file);
    let mut archive = tar::Archive::new(decoder);

    archive.unpack(target_dir)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to extract tarball: {}", e)))?;

    Ok(())
}

/// Calculate checksum for integrity verification
fn calculate_checksum(path: &Path) -> Result<String> {
    let mut file = fs::File::open(path)?;
    let mut hasher = md5::Context::new();
    let mut buffer = [0; 8192];

    loop {
        let n = file.read(&mut buffer)?;
        if n == 0 {
            break;
        }
        hasher.consume(&buffer[..n]);
    }

    Ok(format!("{:x}", hasher.finalize()))
}

/// Load lockfile
async fn load_lockfile(packages_dir: &Path) -> Result<Lockfile> {
    let lockfile_path = packages_dir.join("ggen.lock");

    if !lockfile_path.exists() {
        return Ok(Lockfile {
            version: "1.0".to_string(),
            packages: HashMap::new(),
        });
    }

    let content = tokio::fs::read_to_string(&lockfile_path).await?;
    let lockfile: Lockfile = serde_json::from_str(&content)?;

    Ok(lockfile)
}

/// Save lockfile
async fn save_lockfile(lockfile: &Lockfile, packages_dir: &Path) -> Result<()> {
    let lockfile_path = packages_dir.join("ggen.lock");
    let content = serde_json::to_string_pretty(lockfile)?;

    tokio::fs::write(&lockfile_path, content).await?;

    Ok(())
}

/// Install a package from the marketplace
///
/// This implements the complete installation logic with:
/// - Version resolution (semver ranges: ^, ~, >=, latest)
/// - Dependency graph building and circular detection
/// - Topological sorting for install order
/// - Tarball extraction
/// - Lockfile management
/// - Atomic operations with rollback on failure
pub async fn install_package(options: &InstallOptions) -> Result<InstallResult> {
    // Get registry and packages directories
    let home_dir = dirs::home_dir()
        .ok_or_else(|| ggen_utils::error::Error::new("Home directory not found"))?;

    let default_base = home_dir.join(".ggen");

    // If target_path is provided, derive registry from it (for testing)
    let (registry_path, packages_dir) = if let Some(ref target) = options.target_path {
        // For tests: target is packages dir, registry is sibling
        let base = target.parent()
            .and_then(|p| p.parent())
            .unwrap_or(target.as_path());
        (base.join(".ggen").join("registry"), target.clone())
    } else {
        // Default: use home directory
        (default_base.join("registry"), default_base.join("packages"))
    };

    // Ensure directories exist
    tokio::fs::create_dir_all(&registry_path).await?;
    tokio::fs::create_dir_all(&packages_dir).await?;

    // Parse package specification
    let version_spec = options.version.as_deref().unwrap_or("latest");

    // Resolve version
    let resolved_version = resolve_version(&options.package_name, version_spec, &registry_path)?;

    println!("📦 Resolving {}@{} -> {}",
        options.package_name, version_spec, resolved_version);

    // Dry run check
    if options.dry_run {
        println!("🔍 Dry run: Would install {}@{}",
            options.package_name, resolved_version);
        return Ok(InstallResult {
            package_name: options.package_name.clone(),
            version: resolved_version,
            install_path: packages_dir.join(&options.package_name),
            dependencies_installed: vec![],
        });
    }

    // Check if already installed and not forcing
    let install_path = packages_dir.join(&options.package_name);
    if install_path.exists() && !options.force {
        return Err(ggen_utils::error::Error::new(&format!(
            "Package {} already installed. Use --force to overwrite",
            options.package_name
        )));
    }

    // Build dependency graph
    let mut graph = DependencyGraph::new();
    let mut to_install = Vec::new();

    // Load main package manifest
    let main_manifest = load_package_manifest(
        &options.package_name,
        &resolved_version,
        &registry_path,
    )
    .await?;

    graph.add_package(main_manifest.clone());
    to_install.push((options.package_name.clone(), resolved_version.clone()));

    // Recursively resolve dependencies
    if options.with_dependencies {
        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();

        queue.push_back((options.package_name.clone(), resolved_version.clone()));
        visited.insert(format!("{}@{}", options.package_name, resolved_version));

        while let Some((pkg_name, pkg_version)) = queue.pop_front() {
            let manifest = load_package_manifest(&pkg_name, &pkg_version, &registry_path).await?;

            for (dep_name, dep_version_spec) in &manifest.dependencies {
                let dep_version = resolve_version(dep_name, dep_version_spec, &registry_path)?;
                let dep_key = format!("{}@{}", dep_name, dep_version);

                // Skip if already visited
                if visited.contains(&dep_key) {
                    continue;
                }

                visited.insert(dep_key);

                // Load dependency manifest
                let dep_manifest = load_package_manifest(dep_name, &dep_version, &registry_path).await?;
                graph.add_package(dep_manifest);

                to_install.push((dep_name.clone(), dep_version.clone()));
                queue.push_back((dep_name.clone(), dep_version));
            }
        }
    }

    // Detect circular dependencies
    graph.detect_circular()?;

    // Get install order via topological sort
    let install_order = graph.topological_sort()?;

    println!("📊 Install order: {:?}", install_order);

    // Load lockfile
    let mut lockfile = load_lockfile(&packages_dir).await?;

    // Track installed for rollback
    let mut installed_packages = Vec::new();

    // Install packages in order
    for pkg_key in &install_order {
        let (pkg_name, pkg_version) = parse_package_spec(pkg_key);

        println!("⬇️  Installing {}@{}...", pkg_name, pkg_version);

        // Get tarball path
        let tarball_path = registry_path
            .join(&pkg_name)
            .join(&pkg_version)
            .join(format!("{}-{}.tar.gz", pkg_name.replace('/', "-"), pkg_version));

        let pkg_install_path = packages_dir.join(&pkg_name);

        // Remove existing if force
        if pkg_install_path.exists() && options.force {
            tokio::fs::remove_dir_all(&pkg_install_path).await?;
        }

        // Extract package
        match extract_tarball(&tarball_path, &pkg_install_path).await {
            Ok(_) => {
                // Calculate integrity
                let integrity = calculate_checksum(&tarball_path).ok();

                // Update lockfile
                let manifest = load_package_manifest(&pkg_name, &pkg_version, &registry_path).await?;
                lockfile.packages.insert(
                    pkg_key.clone(),
                    LockfileEntry {
                        name: pkg_name.clone(),
                        version: pkg_version.clone(),
                        resolved: tarball_path.display().to_string(),
                        integrity,
                        dependencies: manifest.dependencies.clone(),
                    },
                );

                installed_packages.push(pkg_key.clone());
                println!("✅ Installed {}@{}", pkg_name, pkg_version);
            }
            Err(e) => {
                // Rollback on failure
                println!("❌ Installation failed: {}", e);
                println!("🔄 Rolling back...");

                for installed_key in &installed_packages {
                    let (rollback_name, _) = parse_package_spec(installed_key);
                    let rollback_path = packages_dir.join(&rollback_name);

                    if rollback_path.exists() {
                        let _ = tokio::fs::remove_dir_all(&rollback_path).await;
                    }

                    lockfile.packages.remove(installed_key);
                }

                return Err(e);
            }
        }
    }

    // Save lockfile
    save_lockfile(&lockfile, &packages_dir).await?;

    // Collect dependency names
    let dependencies_installed: Vec<String> = installed_packages
        .iter()
        .filter(|pkg| !pkg.starts_with(&format!("{}@", options.package_name)))
        .map(|pkg| {
            let (name, version) = parse_package_spec(pkg);
            format!("{}@{}", name, version)
        })
        .collect();

    Ok(InstallResult {
        package_name: options.package_name.clone(),
        version: resolved_version,
        install_path,
        dependencies_installed,
    })
}

/// Install package and report progress
///
/// This function bridges the CLI to the domain layer.
pub async fn install_and_report(
    package: &str,
    target: Option<&str>,
    force: bool,
    with_dependencies: bool,
    dry_run: bool,
) -> Result<()> {
    // Parse package specification (name@version)
    let (package_name, version) = match package.rsplit_once('@') {
        Some((name, ver)) => (name.to_string(), Some(ver.to_string())),
        None => (package.to_string(), None),
    };

    // Build install options
    let mut options = InstallOptions::new(package_name);

    if let Some(ver) = version {
        options = options.with_version(ver);
    }

    if let Some(target_path) = target {
        options = options.with_target(PathBuf::from(target_path));
    }

    if force {
        options = options.force();
    }

    if !with_dependencies {
        options = options.no_dependencies();
    }

    if dry_run {
        options = options.dry_run();
    }

    // Display dry run information
    if dry_run {
        println!("🔍 Dry run: Would install package");
        println!("   Package: {}", options.package_name);
        if let Some(ref ver) = options.version {
            println!("   Version: {}", ver);
        }
        if let Some(ref path) = options.target_path {
            println!("   Target: {}", path.display());
        }
        println!("   Dependencies: {}", if with_dependencies { "yes" } else { "no" });
        return Ok(());
    }

    // Install package
    println!("📦 Installing {}...", package);

    match install_package(&options).await {
        Ok(result) => {
            println!("✅ Successfully installed {} v{}", result.package_name, result.version);
            println!("   Location: {}", result.install_path.display());

            if !result.dependencies_installed.is_empty() {
                println!("   Dependencies: {}", result.dependencies_installed.join(", "));
            }

            Ok(())
        }
        Err(e) => {
            // For Phase 1, show placeholder message
            println!("ℹ️  Package installation not yet implemented (Phase 2)");
            println!("   Package: {}", package);
            Err(e)
        }
    }
}

/// Run install command (sync wrapper for CLI)
pub fn run(args: &InstallArgs) -> Result<()> {
    crate::runtime::block_on(async {
        install_and_report(
            &args.package,
            args.target.as_deref(),
            args.force,
            !args.no_dependencies,
            args.dry_run,
        )
        .await
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_install_options_builder() {
        let options = InstallOptions::new("my-package")
            .with_version("1.0.0")
            .force()
            .dry_run();

        assert_eq!(options.package_name, "my-package");
        assert_eq!(options.version, Some("1.0.0".to_string()));
        assert!(options.force);
        assert!(options.dry_run);
        assert!(options.with_dependencies);
    }

    #[test]
    fn test_install_options_no_dependencies() {
        let options = InstallOptions::new("test").no_dependencies();
        assert!(!options.with_dependencies);
    }
}
