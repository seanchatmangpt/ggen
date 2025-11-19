//! Project generator for scaffolding new projects
//!
//! This module provides functionality for generating complete project structures
//! from templates. It supports multiple project types including Rust (web, CLI, library)
//! and JavaScript frameworks (Next.js, Nuxt).
//!
//! ## Features
//!
//! - **Multi-language support**: Generate Rust and JavaScript/TypeScript projects
//! - **Project type detection**: Automatically select appropriate generator
//! - **File system operations**: Create directories and write files
//! - **Git integration**: Initialize git repositories for new projects
//! - **Dependency management**: Install dependencies via cargo or npm
//!
//! ## Architecture
//!
//! The module uses a trait-based design with `ProjectGenerator` trait that allows
//! different generators for different project types. The `GeneratorFactory` creates
//! the appropriate generator based on project type.
//!
//! ## Examples
//!
//! ### Creating a Rust CLI Project
//!
//! ```rust,no_run
//! use ggen_core::project_generator::{ProjectConfig, ProjectType, create_new_project};
//! use std::path::PathBuf;
//!
//! # async fn example() -> anyhow::Result<()> {
//! let config = ProjectConfig {
//!     name: "my-cli".to_string(),
//!     project_type: ProjectType::RustCli,
//!     framework: None,
//!     path: PathBuf::from("."),
//! };
//!
//! create_new_project(&config).await?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Creating a Next.js Project
//!
//! ```rust,no_run
//! use ggen_core::project_generator::{ProjectConfig, ProjectType, create_new_project};
//! use std::path::PathBuf;
//!
//! # async fn example() -> anyhow::Result<()> {
//! let config = ProjectConfig {
//!     name: "my-app".to_string(),
//!     project_type: ProjectType::NextJs,
//!     framework: None,
//!     path: PathBuf::from("."),
//! };
//!
//! create_new_project(&config).await?;
//! # Ok(())
//! # }
//! ```

pub mod common;
pub mod nextjs;
pub mod rust;

use ggen_utils::error::{Error, Result};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProjectType {
    RustWeb,
    RustCli,
    RustLib,
    NextJs,
    Nuxt,
}

impl std::fmt::Display for ProjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProjectType::RustWeb => write!(f, "rust-web"),
            ProjectType::RustCli => write!(f, "rust-cli"),
            ProjectType::RustLib => write!(f, "rust-lib"),
            ProjectType::NextJs => write!(f, "nextjs"),
            ProjectType::Nuxt => write!(f, "nuxt"),
        }
    }
}

impl std::str::FromStr for ProjectType {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "rust-web" => Ok(ProjectType::RustWeb),
            "rust-cli" => Ok(ProjectType::RustCli),
            "rust-lib" => Ok(ProjectType::RustLib),
            "nextjs" => Ok(ProjectType::NextJs),
            "nuxt" => Ok(ProjectType::Nuxt),
            _ => Err(Error::new(&format!("Unsupported project type: {}", s))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ProjectConfig {
    pub name: String,
    pub project_type: ProjectType,
    pub framework: Option<String>,
    pub path: PathBuf,
}

#[derive(Debug, Clone, Default)]
pub struct ProjectStructure {
    pub files: Vec<(String, String)>,
    pub directories: Vec<String>,
}

/// Trait for project generators
pub trait ProjectGenerator: Send + Sync {
    fn generate(&self, config: &ProjectConfig) -> Result<ProjectStructure>;
    fn supported_types(&self) -> Vec<ProjectType>;
}

/// Factory for creating project generators
pub struct GeneratorFactory;

impl GeneratorFactory {
    pub fn create(project_type: &ProjectType) -> Result<Box<dyn ProjectGenerator>> {
        match project_type {
            ProjectType::RustWeb | ProjectType::RustCli | ProjectType::RustLib => {
                Ok(Box::new(rust::RustProjectGenerator::new()))
            }
            ProjectType::NextJs | ProjectType::Nuxt => Ok(Box::new(nextjs::NextJsGenerator::new())),
        }
    }
}

/// File system writer for creating files and directories
pub struct FileSystemWriter;

impl Default for FileSystemWriter {
    fn default() -> Self {
        Self
    }
}

impl FileSystemWriter {
    pub fn new() -> Self {
        Self
    }

    pub fn write_file(&self, path: &Path, content: &str) -> Result<()> {
        std::fs::write(path, content).map_err(|e| {
            Error::with_source(
                &format!("Failed to write file {}", path.display()),
                Box::new(e),
            )
        })
    }

    pub fn create_directory(&self, path: &Path) -> Result<()> {
        std::fs::create_dir_all(path).map_err(|e| {
            Error::with_source(
                &format!("Failed to create directory {}", path.display()),
                Box::new(e),
            )
        })
    }
}

/// Git repository initializer
pub struct GitInitializer;

impl Default for GitInitializer {
    fn default() -> Self {
        Self
    }
}

impl GitInitializer {
    pub fn new() -> Self {
        Self
    }

    pub fn initialize(&self, path: &Path) -> Result<()> {
        // SECURITY FIX (Week 4): Use SafeCommand instead of raw std::process::Command
        use crate::security::command::SafeCommand;

        let output = SafeCommand::new("git")?
            .arg("init")?
            .current_dir(path)?
            .execute()
            .map_err(|e| Error::with_source("Failed to run git init", Box::new(e)))?;

        if !output.status.success() {
            return Err(Error::new(&format!(
                "git init failed: {}",
                String::from_utf8_lossy(&output.stderr)
            )));
        }

        Ok(())
    }
}

/// Dependency installer for different project types
pub struct DependencyInstaller;

impl Default for DependencyInstaller {
    fn default() -> Self {
        Self
    }
}

impl DependencyInstaller {
    pub fn new() -> Self {
        Self
    }

    pub fn install(&self, path: &Path, project_type: &ProjectType) -> Result<()> {
        match project_type {
            ProjectType::RustWeb | ProjectType::RustCli | ProjectType::RustLib => {
                self.install_cargo_deps(path)
            }
            ProjectType::NextJs | ProjectType::Nuxt => self.install_npm_deps(path),
        }
    }

    fn install_cargo_deps(&self, path: &Path) -> Result<()> {
        // SECURITY FIX (Week 4): Use SafeCommand instead of raw std::process::Command
        use crate::security::command::SafeCommand;

        ggen_utils::alert_info!("Installing Cargo dependencies...");

        let output = SafeCommand::new("cargo")?
            .arg("fetch")?
            .current_dir(path)?
            .execute()
            .map_err(|e| Error::with_source("Failed to run cargo fetch", Box::new(e)))?;

        if !output.status.success() {
            // Non-critical failure - dependencies will be fetched on first build
            let error_msg = format!(
                "cargo fetch failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
            ggen_utils::alert_warning!(&error_msg);
        }

        Ok(())
    }

    fn install_npm_deps(&self, path: &Path) -> Result<()> {
        // SECURITY FIX (Week 4): Use SafeCommand instead of raw std::process::Command
        use crate::security::command::SafeCommand;

        ggen_utils::alert_info!("Installing npm dependencies...");

        let output = SafeCommand::new("npm")?
            .arg("install")?
            .current_dir(path)?
            .execute()
            .map_err(|e| Error::with_source("Failed to run npm install", Box::new(e)))?;

        if !output.status.success() {
            return Err(Error::new(&format!(
                "npm install failed: {}",
                String::from_utf8_lossy(&output.stderr)
            )));
        }

        Ok(())
    }
}

/// Main entry point for creating new projects
pub async fn create_new_project(config: &ProjectConfig) -> Result<()> {
    let project_path = config.path.join(&config.name);

    // Check if project already exists
    if project_path.exists() {
        return Err(Error::new(&format!(
            "Project directory '{}' already exists",
            config.name
        )));
    }

    // Create generator
    let generator = GeneratorFactory::create(&config.project_type)?;

    // Generate project structure
    let structure = generator.generate(config)?;

    // Create file system writer
    let fs_writer = FileSystemWriter::new();

    // Create main project directory
    fs_writer.create_directory(&project_path)?;

    // Create subdirectories
    for dir in &structure.directories {
        let dir_path = project_path.join(dir);
        fs_writer.create_directory(&dir_path)?;
    }

    // Write files
    for (file_path, content) in &structure.files {
        let full_path = project_path.join(file_path);

        // Ensure parent directory exists
        if let Some(parent) = full_path.parent() {
            fs_writer.create_directory(parent)?;
        }

        fs_writer.write_file(&full_path, content)?;
    }

    // Initialize git repository
    let git = GitInitializer::new();
    git.initialize(&project_path)?;

    // Install dependencies
    let deps = DependencyInstaller::new();
    deps.install(&project_path, &config.project_type)?;

    ggen_utils::alert_success!(&format!("Successfully created project: {}", config.name));
    ggen_utils::alert_info!(&format!("   Type: {}", config.project_type));
    ggen_utils::alert_info!(&format!("   Path: {}", project_path.display()));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_project_type_from_str() {
        assert_eq!(
            "rust-web".parse::<ProjectType>().unwrap(),
            ProjectType::RustWeb
        );
        assert_eq!(
            "nextjs".parse::<ProjectType>().unwrap(),
            ProjectType::NextJs
        );
    }

    #[test]
    fn test_project_type_display() {
        assert_eq!(ProjectType::RustWeb.to_string(), "rust-web");
        assert_eq!(ProjectType::NextJs.to_string(), "nextjs");
    }
}
