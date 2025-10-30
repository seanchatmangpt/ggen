// Project Generator Module
// Production-ready code with error handling

pub mod rust;
pub mod nextjs;
pub mod common;

use anyhow::Result;
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
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s.to_lowercase().as_str() {
            "rust-web" => Ok(ProjectType::RustWeb),
            "rust-cli" => Ok(ProjectType::RustCli),
            "rust-lib" => Ok(ProjectType::RustLib),
            "nextjs" => Ok(ProjectType::NextJs),
            "nuxt" => Ok(ProjectType::Nuxt),
            _ => anyhow::bail!("Unsupported project type: {}", s),
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
            ProjectType::NextJs | ProjectType::Nuxt => {
                Ok(Box::new(nextjs::NextJsGenerator::new()))
            }
        }
    }
}

/// File system writer for creating files and directories
pub struct FileSystemWriter;

impl FileSystemWriter {
    pub fn new() -> Self {
        Self
    }

    pub fn write_file(&self, path: &Path, content: &str) -> Result<()> {
        std::fs::write(path, content)
            .map_err(|e| anyhow::anyhow!("Failed to write file {}: {}", path.display(), e))
    }

    pub fn create_directory(&self, path: &Path) -> Result<()> {
        std::fs::create_dir_all(path)
            .map_err(|e| anyhow::anyhow!("Failed to create directory {}: {}", path.display(), e))
    }
}

/// Git repository initializer
pub struct GitInitializer;

impl GitInitializer {
    pub fn new() -> Self {
        Self
    }

    pub fn initialize(&self, path: &Path) -> Result<()> {
        use std::process::Command;

        let output = Command::new("git")
            .arg("init")
            .current_dir(path)
            .output()
            .map_err(|e| anyhow::anyhow!("Failed to run git init: {}", e))?;

        if !output.status.success() {
            anyhow::bail!(
                "git init failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }

        Ok(())
    }
}

/// Dependency installer for different project types
pub struct DependencyInstaller;

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
        use std::process::Command;

        println!("Installing Cargo dependencies...");

        let output = Command::new("cargo")
            .arg("fetch")
            .current_dir(path)
            .output()
            .map_err(|e| anyhow::anyhow!("Failed to run cargo fetch: {}", e))?;

        if !output.status.success() {
            // Non-critical failure - dependencies will be fetched on first build
            eprintln!(
                "Warning: cargo fetch failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }

        Ok(())
    }

    fn install_npm_deps(&self, path: &Path) -> Result<()> {
        use std::process::Command;

        println!("Installing npm dependencies...");

        let output = Command::new("npm")
            .arg("install")
            .current_dir(path)
            .output()
            .map_err(|e| anyhow::anyhow!("Failed to run npm install: {}", e))?;

        if !output.status.success() {
            anyhow::bail!(
                "npm install failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }

        Ok(())
    }
}

/// Main entry point for creating new projects
pub async fn create_new_project(config: &ProjectConfig) -> Result<()> {
    let project_path = config.path.join(&config.name);

    // Check if project already exists
    if project_path.exists() {
        anyhow::bail!("Project directory '{}' already exists", config.name);
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

    println!("✅ Successfully created project: {}", config.name);
    println!("   Type: {}", config.project_type);
    println!("   Path: {}", project_path.display());

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
