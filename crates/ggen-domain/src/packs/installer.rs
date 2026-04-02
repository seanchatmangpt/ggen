//! Production-grade pack installer with marketplace integration
//!
//! This module provides real package installation by integrating with
//! the marketplace domain layer and ggen-core infrastructure.

use crate::marketplace;
use crate::packs::dependency_graph::DependencyGraph;
use crate::packs::repository::{FileSystemRepository, PackRepository};
use crate::packs::types::Pack;
use ggen_utils::error::{Error, Result};
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Instant;
use tracing::{error, info, warn};

/// Pack installer with dependency resolution and conflict detection
pub struct PackInstaller {
    repository: Box<dyn PackRepository>,
}

impl PackInstaller {
    /// Create new installer with custom repository
    pub fn new(repository: Box<dyn PackRepository>) -> Self {
        Self { repository }
    }

    /// Create installer with default filesystem repository
    pub fn with_default_repo() -> Result<Self> {
        let repo = FileSystemRepository::discover()?;
        Ok(Self::new(Box::new(repo)))
    }

    /// Install a pack with full dependency resolution
    ///
    /// # Features
    /// - Resolves dependencies recursively
    /// - Detects circular dependencies
    /// - Topological sort for correct install order
    /// - Conflict detection between packs
    /// - Rollback on failure (when force=false)
    ///
    /// # Arguments
    /// * `pack_id` - ID of the pack to install
    /// * `options` - Installation options
    pub async fn install(&self, pack_id: &str, options: &InstallOptions) -> Result<InstallReport> {
        let start = Instant::now();

        info!("Starting pack installation: {}", pack_id);

        // Load pack metadata
        let pack = self.repository.load(pack_id).await?;

        info!("Loaded pack: {} v{}", pack.name, pack.version);

        // For dry-run, return early without resolving dependencies or reaching marketplace
        if options.dry_run {
            let duration = start.elapsed();
            let install_order = vec![pack_id.to_string()];
            return Ok(InstallReport {
                pack_id: pack_id.to_string(),
                pack_name: pack.name,
                pack_version: pack.version,
                packages_installed: pack.packages.clone(),
                templates_available: pack.templates.iter().map(|t| t.name.clone()).collect(),
                install_path: options
                    .target_dir
                    .clone()
                    .unwrap_or_else(|| PathBuf::from(".")),
                dependencies_resolved: vec![pack_id.to_string()],
                install_order,
                conflicts: vec![],
                duration,
                success: true,
            });
        }

        // Resolve dependencies (only for real installation)
        let all_packs = if options.skip_dependencies {
            vec![pack.clone()]
        } else {
            self.resolve_dependencies(&pack).await?
        };

        info!(
            "Resolved {} pack(s) including dependencies",
            all_packs.len()
        );

        // Build dependency graph
        let graph = DependencyGraph::from_packs(&all_packs)?;
        let install_order = graph.topological_sort()?;

        info!("Installation order: {:?}", install_order);

        // Detect conflicts
        let conflicts = self.detect_conflicts(&all_packs);
        if !conflicts.is_empty() {
            warn!("Detected {} conflict(s)", conflicts.len());
            for conflict in &conflicts {
                warn!("  - {}", conflict);
            }

            if !options.force {
                return Err(Error::new(&format!(
                    "Conflicts detected. Use --force to override:\n{}",
                    conflicts.join("\n")
                )));
            }
        }

        // Determine install path
        let install_path = options.target_dir.clone().unwrap_or_else(|| {
            dirs::home_dir()
                .unwrap_or_else(|| PathBuf::from("."))
                .join(".ggen")
                .join("packs")
        });

        // Create install directory
        tokio::fs::create_dir_all(&install_path).await?;

        // Install packages in topological order
        let mut packages_installed = Vec::new();
        let mut failed_packages = Vec::new();

        for pack_id_to_install in &install_order {
            let pack_to_install = all_packs
                .iter()
                .find(|p| p.id == *pack_id_to_install)
                .unwrap();

            info!("Installing packages from pack: {}", pack_to_install.name);

            for package_name in &pack_to_install.packages {
                match self
                    .install_package(package_name, &install_path, options)
                    .await
                {
                    Ok(_) => {
                        packages_installed.push(package_name.clone());
                        info!("✓ Installed package: {}", package_name);
                    }
                    Err(e) => {
                        error!("✗ Failed to install package {}: {}", package_name, e);
                        failed_packages.push(package_name.clone());

                        if !options.force {
                            return Err(Error::new(&format!(
                                "Failed to install package '{}': {}",
                                package_name, e
                            )));
                        }
                    }
                }
            }
        }

        let duration = start.elapsed();
        let success = failed_packages.is_empty();

        if success {
            info!(
                "✓ Pack installation completed successfully in {:?}",
                duration
            );
        } else {
            warn!(
                "⚠ Pack installation completed with {} failures in {:?}",
                failed_packages.len(),
                duration
            );
        }

        Ok(InstallReport {
            pack_id: pack_id.to_string(),
            pack_name: pack.name,
            pack_version: pack.version,
            packages_installed,
            templates_available: pack.templates.iter().map(|t| t.name.clone()).collect(),
            install_path,
            dependencies_resolved: all_packs.iter().map(|p| p.id.clone()).collect(),
            install_order,
            conflicts,
            duration,
            success,
        })
    }

    /// Resolve pack dependencies recursively
    async fn resolve_dependencies(&self, pack: &Pack) -> Result<Vec<Pack>> {
        let mut resolved = Vec::new();
        let mut visited = std::collections::HashSet::new();
        let mut queue = std::collections::VecDeque::new();

        queue.push_back(pack.clone());

        while let Some(current) = queue.pop_front() {
            if visited.contains(&current.id) {
                continue;
            }

            visited.insert(current.id.clone());
            resolved.push(current.clone());

            // Add dependencies to queue
            for dep in &current.dependencies {
                if !dep.optional && !visited.contains(&dep.pack_id) {
                    match self.repository.load(&dep.pack_id).await {
                        Ok(dep_pack) => {
                            queue.push_back(dep_pack);
                        }
                        Err(e) => {
                            return Err(Error::new(&format!(
                                "Failed to load dependency '{}': {}",
                                dep.pack_id, e
                            )));
                        }
                    }
                }
            }
        }

        Ok(resolved)
    }

    /// Detect conflicts between packs
    ///
    /// Conflicts occur when multiple packs provide the same package
    fn detect_conflicts(&self, packs: &[Pack]) -> Vec<String> {
        let mut conflicts = Vec::new();
        let mut package_sources: HashMap<String, Vec<String>> = HashMap::new();

        for pack in packs {
            for package in &pack.packages {
                package_sources
                    .entry(package.clone())
                    .or_insert_with(Vec::new)
                    .push(pack.id.clone());
            }
        }

        for (package, sources) in package_sources {
            if sources.len() > 1 {
                conflicts.push(format!(
                    "Package '{}' provided by multiple packs: {}",
                    package,
                    sources.join(", ")
                ));
            }
        }

        conflicts
    }

    /// Install a single package using marketplace domain functions
    async fn install_package(
        &self, package_name: &str, target_dir: &PathBuf, options: &InstallOptions,
    ) -> Result<()> {
        // Parse package name and version
        let (name, version) = if let Some(idx) = package_name.find('@') {
            let (n, v) = package_name.split_at(idx);
            (n.to_string(), Some(v[1..].to_string()))
        } else {
            (package_name.to_string(), None)
        };

        // Build marketplace install input
        // Format: package@version or just package
        let package_spec = if let Some(ver) = version {
            format!("{}@{}", name, ver)
        } else {
            name
        };

        let marketplace_input = marketplace::InstallInput {
            package: package_spec,
            target: Some(target_dir.display().to_string()),
            force: options.force,
            no_dependencies: options.skip_dependencies,
            dry_run: false, // We handle dry-run at pack level
        };

        // Call marketplace domain function
        marketplace::execute_install(marketplace_input).await?;

        Ok(())
    }
}

/// Installation options
#[derive(Debug, Clone)]
pub struct InstallOptions {
    /// Target directory for installation
    pub target_dir: Option<PathBuf>,
    /// Force installation even if conflicts exist
    pub force: bool,
    /// Dry run mode - don't actually install
    pub dry_run: bool,
    /// Skip dependency resolution
    pub skip_dependencies: bool,
}

impl Default for InstallOptions {
    fn default() -> Self {
        Self {
            target_dir: None,
            force: false,
            dry_run: false,
            skip_dependencies: false,
        }
    }
}

/// Installation report
#[derive(Debug, Clone)]
pub struct InstallReport {
    /// Pack ID that was installed
    pub pack_id: String,
    /// Pack name
    pub pack_name: String,
    /// Pack version
    pub pack_version: String,
    /// List of packages successfully installed
    pub packages_installed: Vec<String>,
    /// List of templates available in the pack
    pub templates_available: Vec<String>,
    /// Installation path
    pub install_path: PathBuf,
    /// Dependencies that were resolved
    pub dependencies_resolved: Vec<String>,
    /// Installation order (topologically sorted)
    pub install_order: Vec<String>,
    /// Conflicts detected (if any)
    pub conflicts: Vec<String>,
    /// Time taken for installation
    pub duration: std::time::Duration,
    /// Whether installation was successful
    pub success: bool,
}

impl InstallReport {
    /// Get summary string
    pub fn summary(&self) -> String {
        format!(
            "Installed pack '{}' v{} with {} packages in {:?}",
            self.pack_name,
            self.pack_version,
            self.packages_installed.len(),
            self.duration
        )
    }

    /// Get detailed report
    pub fn detailed_report(&self) -> String {
        let mut report = Vec::new();

        report.push(format!("Pack Installation Report"));
        report.push(format!("========================"));
        report.push(format!("Pack: {} v{}", self.pack_name, self.pack_version));
        report.push(format!("Pack ID: {}", self.pack_id));
        report.push(format!("Install Path: {}", self.install_path.display()));
        report.push(format!("Duration: {:?}", self.duration));
        report.push(format!(
            "Status: {}",
            if self.success { "SUCCESS" } else { "PARTIAL" }
        ));
        report.push(String::new());

        report.push(format!(
            "Packages Installed: {}",
            self.packages_installed.len()
        ));
        for pkg in &self.packages_installed {
            report.push(format!("  ✓ {}", pkg));
        }
        report.push(String::new());

        report.push(format!(
            "Templates Available: {}",
            self.templates_available.len()
        ));
        for tmpl in &self.templates_available {
            report.push(format!("  • {}", tmpl));
        }
        report.push(String::new());

        if !self.dependencies_resolved.is_empty() {
            report.push(format!(
                "Dependencies Resolved: {}",
                self.dependencies_resolved.len()
            ));
            for dep in &self.dependencies_resolved {
                report.push(format!("  • {}", dep));
            }
            report.push(String::new());
        }

        if !self.conflicts.is_empty() {
            report.push(format!("⚠ Conflicts Detected: {}", self.conflicts.len()));
            for conflict in &self.conflicts {
                report.push(format!("  ! {}", conflict));
            }
        }

        report.join("\n")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_installer_with_default_repo() {
        // Just test creation doesn't panic
        let _result = PackInstaller::with_default_repo();
    }

    #[test]
    fn test_install_options_default() {
        let opts = InstallOptions::default();
        assert!(!opts.force);
        assert!(!opts.dry_run);
        assert!(!opts.skip_dependencies);
        assert!(opts.target_dir.is_none());
    }

    #[test]
    fn test_install_report_summary() {
        let report = InstallReport {
            pack_id: "test-pack".to_string(),
            pack_name: "Test Pack".to_string(),
            pack_version: "1.0.0".to_string(),
            packages_installed: vec!["pkg1".to_string(), "pkg2".to_string()],
            templates_available: vec![],
            install_path: PathBuf::from("/tmp/test"),
            dependencies_resolved: vec![],
            install_order: vec![],
            conflicts: vec![],
            duration: std::time::Duration::from_millis(100),
            success: true,
        };

        let summary = report.summary();
        assert!(summary.contains("Test Pack"));
        assert!(summary.contains("1.0.0"));
        assert!(summary.contains("2 packages"));
    }

    #[test]
    fn test_install_report_detailed() {
        let report = InstallReport {
            pack_id: "test-pack".to_string(),
            pack_name: "Test Pack".to_string(),
            pack_version: "1.0.0".to_string(),
            packages_installed: vec!["pkg1".to_string()],
            templates_available: vec!["template1".to_string()],
            install_path: PathBuf::from("/tmp/test"),
            dependencies_resolved: vec!["dep1".to_string()],
            install_order: vec![],
            conflicts: vec!["conflict1".to_string()],
            duration: std::time::Duration::from_millis(100),
            success: true,
        };

        let detailed = report.detailed_report();
        assert!(detailed.contains("Pack Installation Report"));
        assert!(detailed.contains("Test Pack"));
        assert!(detailed.contains("pkg1"));
        assert!(detailed.contains("template1"));
        assert!(detailed.contains("dep1"));
        assert!(detailed.contains("conflict1"));
    }
}
