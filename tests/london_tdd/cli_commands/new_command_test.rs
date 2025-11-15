// London TDD - Test Suite for `ggen new` Command
// Outside-in testing approach: Start with acceptance tests, then unit tests

use anyhow::Result;
use mockall::predicate::*;
use std::path::{Path, PathBuf};
use tempfile::TempDir;
use tokio::fs;

// ============================================================================
// DOMAIN MODELS
// ============================================================================

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProjectType {
    RustWeb,
    #[allow(dead_code)]
    RustCli,
    #[allow(dead_code)]
    RustLib,
    NextJs,
    #[allow(dead_code)]
    Nuxt,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectConfig {
    pub name: String,
    pub project_type: ProjectType,
    pub framework: Option<String>,
    pub path: PathBuf,
}

#[derive(Debug, Clone, Default)]
pub struct ProjectStructure {
    pub files: Vec<(String, String)>, // (path, content)
    pub directories: Vec<String>,
}

// ============================================================================
// TRAIT DEFINITIONS (Design by Contract)
// ============================================================================

#[cfg_attr(test, mockall::automock)]
pub trait ProjectGenerator: Send + Sync {
    fn generate(&self, config: &ProjectConfig) -> Result<ProjectStructure>;
    #[allow(dead_code)]
    fn supported_types(&self) -> Vec<ProjectType>;
}

#[cfg_attr(test, mockall::automock)]
pub trait FileSystemWriter: Send + Sync {
    fn write_file(&self, path: &Path, content: &str) -> Result<()>;
    fn create_directory(&self, path: &Path) -> Result<()>;
}

#[cfg_attr(test, mockall::automock)]
pub trait GitInitializer: Send + Sync {
    fn initialize(&self, path: &Path) -> Result<()>;
}

#[cfg_attr(test, mockall::automock)]
pub trait DependencyInstaller: Send + Sync {
    fn install(&self, path: &Path, project_type: &ProjectType) -> Result<()>;
}

// ============================================================================
// ACCEPTANCE TESTS (Outside-In)
// ============================================================================

#[cfg(test)]
mod acceptance_tests {
    use super::*;

    #[tokio::test]
    async fn user_can_create_rust_web_project() -> Result<()> {
        // Given: User wants to create a Rust web project
        let temp_dir = TempDir::new()?;
        let project_name = "my-rust-web-app";

        let mut generator = MockProjectGenerator::new();
        generator
            .expect_generate()
            .with(function(|cfg: &ProjectConfig| {
                cfg.name == "my-rust-web-app" && cfg.project_type == ProjectType::RustWeb
            }))
            .returning(|_| {
                Ok(ProjectStructure {
                    files: vec![
                        (
                            "Cargo.toml".to_string(),
                            "[package]\nname = \"my-rust-web-app\"".to_string(),
                        ),
                        (
                            "src/main.rs".to_string(),
                            "fn main() { println!(\"Hello\"); }".to_string(),
                        ),
                    ],
                    directories: vec!["src".to_string()],
                })
            });

        let mut fs_writer = MockFileSystemWriter::new();
        fs_writer.expect_create_directory().returning(|_| Ok(()));
        fs_writer.expect_write_file().returning(|_, _| Ok(()));

        let mut git = MockGitInitializer::new();
        git.expect_initialize().returning(|_| Ok(()));

        let mut deps = MockDependencyInstaller::new();
        deps.expect_install().returning(|_, _| Ok(()));

        // When: User runs `ggen new my-rust-web-app --type rust-web`
        let config = ProjectConfig {
            name: project_name.to_string(),
            project_type: ProjectType::RustWeb,
            framework: None,
            path: temp_dir.path().to_path_buf(),
        };

        let result = create_new_project(&config, &generator, &fs_writer, &git, &deps).await;

        // Then: Project is created successfully
        assert!(result.is_ok());

        Ok(())
    }

    #[tokio::test]
    async fn user_can_create_nextjs_project() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let mut generator = MockProjectGenerator::new();
        generator
            .expect_generate()
            .with(function(|cfg: &ProjectConfig| {
                cfg.name == "my-nextjs-app" && cfg.project_type == ProjectType::NextJs
            }))
            .returning(|_| {
                Ok(ProjectStructure {
                    files: vec![
                        (
                            "package.json".to_string(),
                            r#"{"name": "my-nextjs-app"}"#.to_string(),
                        ),
                        (
                            "pages/index.tsx".to_string(),
                            "export default function Home() {}".to_string(),
                        ),
                    ],
                    directories: vec!["pages".to_string()],
                })
            });

        let mut fs_writer = MockFileSystemWriter::new();
        fs_writer.expect_create_directory().returning(|_| Ok(()));
        fs_writer.expect_write_file().returning(|_, _| Ok(()));

        let mut git = MockGitInitializer::new();
        git.expect_initialize().returning(|_| Ok(()));

        let mut deps = MockDependencyInstaller::new();
        deps.expect_install().returning(|_, _| Ok(()));

        let config = ProjectConfig {
            name: "my-nextjs-app".to_string(),
            project_type: ProjectType::NextJs,
            framework: None,
            path: temp_dir.path().to_path_buf(),
        };

        let result = create_new_project(&config, &generator, &fs_writer, &git, &deps).await;

        assert!(result.is_ok());
        Ok(())
    }

    #[tokio::test]
    async fn user_can_specify_custom_framework() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let mut generator = MockProjectGenerator::new();
        generator
            .expect_generate()
            .with(function(|cfg: &ProjectConfig| {
                cfg.framework == Some("axum".to_string())
            }))
            .returning(|_| Ok(ProjectStructure::default()));

        let mut fs_writer = MockFileSystemWriter::new();
        fs_writer.expect_create_directory().returning(|_| Ok(()));
        fs_writer.expect_write_file().returning(|_, _| Ok(()));

        let mut git = MockGitInitializer::new();
        git.expect_initialize().returning(|_| Ok(()));

        let mut deps = MockDependencyInstaller::new();
        deps.expect_install().returning(|_, _| Ok(()));

        let config = ProjectConfig {
            name: "my-app".to_string(),
            project_type: ProjectType::RustWeb,
            framework: Some("axum".to_string()),
            path: temp_dir.path().to_path_buf(),
        };

        let result = create_new_project(&config, &generator, &fs_writer, &git, &deps).await;

        assert!(result.is_ok());
        Ok(())
    }

    #[tokio::test]
    async fn fails_when_project_already_exists() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let project_path = temp_dir.path().join("existing-project");
        fs::create_dir_all(&project_path).await?;

        let generator = MockProjectGenerator::new();
        let fs_writer = MockFileSystemWriter::new();
        let git = MockGitInitializer::new();
        let deps = MockDependencyInstaller::new();

        let config = ProjectConfig {
            name: "existing-project".to_string(),
            project_type: ProjectType::RustWeb,
            framework: None,
            path: temp_dir.path().to_path_buf(),
        };

        let result = create_new_project(&config, &generator, &fs_writer, &git, &deps).await;

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("already exists"));
        Ok(())
    }
}

// ============================================================================
// UNIT TESTS (Project Generators)
// ============================================================================

#[cfg(test)]
mod rust_web_generator_tests {
    use super::*;

    #[test]
    fn generates_cargo_toml_with_correct_name() -> Result<()> {
        let generator = RustWebGenerator::new();

        let config = ProjectConfig {
            name: "test-project".to_string(),
            project_type: ProjectType::RustWeb,
            framework: None,
            path: PathBuf::from("/tmp"),
        };

        let structure = generator.generate(&config)?;

        let cargo_toml = structure
            .files
            .iter()
            .find(|(path, _)| path == "Cargo.toml")
            .map(|(_, content)| content);

        assert!(cargo_toml.is_some());
        assert!(cargo_toml.unwrap().contains("name = \"test-project\""));
        assert!(cargo_toml.unwrap().contains("version = \"0.1.0\""));

        Ok(())
    }

    #[test]
    fn generates_main_rs_with_working_code() -> Result<()> {
        let generator = RustWebGenerator::new();

        let config = ProjectConfig {
            name: "test-project".to_string(),
            project_type: ProjectType::RustWeb,
            framework: Some("axum".to_string()),
            path: PathBuf::from("/tmp"),
        };

        let structure = generator.generate(&config)?;

        let main_rs = structure
            .files
            .iter()
            .find(|(path, _)| path == "src/main.rs")
            .map(|(_, content)| content);

        assert!(main_rs.is_some());
        assert!(main_rs.unwrap().contains("async fn main"));
        assert!(main_rs.unwrap().contains("axum"));

        Ok(())
    }

    #[test]
    fn creates_required_directories() -> Result<()> {
        let generator = RustWebGenerator::new();

        let config = ProjectConfig {
            name: "test-project".to_string(),
            project_type: ProjectType::RustWeb,
            framework: None,
            path: PathBuf::from("/tmp"),
        };

        let structure = generator.generate(&config)?;

        assert!(structure.directories.contains(&"src".to_string()));
        assert!(structure.directories.contains(&"tests".to_string()));

        Ok(())
    }
}

#[cfg(test)]
mod nextjs_generator_tests {
    use super::*;

    #[test]
    fn generates_package_json_with_correct_name() -> Result<()> {
        let generator = NextJsGenerator::new();

        let config = ProjectConfig {
            name: "my-nextjs-app".to_string(),
            project_type: ProjectType::NextJs,
            framework: None,
            path: PathBuf::from("/tmp"),
        };

        let structure = generator.generate(&config)?;

        let package_json = structure
            .files
            .iter()
            .find(|(path, _)| path == "package.json")
            .map(|(_, content)| content);

        assert!(package_json.is_some());
        assert!(package_json.unwrap().contains(r#""name": "my-nextjs-app""#));

        Ok(())
    }

    #[test]
    fn generates_index_tsx_with_working_code() -> Result<()> {
        let generator = NextJsGenerator::new();

        let config = ProjectConfig {
            name: "my-nextjs-app".to_string(),
            project_type: ProjectType::NextJs,
            framework: None,
            path: PathBuf::from("/tmp"),
        };

        let structure = generator.generate(&config)?;

        let index_tsx = structure
            .files
            .iter()
            .find(|(path, _)| path == "pages/index.tsx")
            .map(|(_, content)| content);

        assert!(index_tsx.is_some());
        assert!(index_tsx.unwrap().contains("export default function"));

        Ok(())
    }
}

// ============================================================================
// IMPLEMENTATION UNDER TEST
// ============================================================================

pub async fn create_new_project(
    config: &ProjectConfig, generator: &impl ProjectGenerator, fs_writer: &impl FileSystemWriter,
    git: &impl GitInitializer, deps: &impl DependencyInstaller,
) -> Result<()> {
    let project_path = config.path.join(&config.name);

    // Check if project already exists
    if project_path.exists() {
        anyhow::bail!("Project directory '{}' already exists", config.name);
    }

    // Generate project structure
    let structure = generator.generate(config)?;

    // Create directories
    fs_writer.create_directory(&project_path)?;
    for dir in &structure.directories {
        let dir_path = project_path.join(dir);
        fs_writer.create_directory(&dir_path)?;
    }

    // Write files
    for (file_path, content) in &structure.files {
        let full_path = project_path.join(file_path);
        fs_writer.write_file(&full_path, content)?;
    }

    // Initialize git repository
    git.initialize(&project_path)?;

    // Install dependencies
    deps.install(&project_path, &config.project_type)?;

    Ok(())
}

// ============================================================================
// STUB IMPLEMENTATIONS (to be replaced with production code)
// ============================================================================

pub struct RustWebGenerator;

impl RustWebGenerator {
    pub fn new() -> Self {
        Self
    }
}

impl ProjectGenerator for RustWebGenerator {
    fn generate(&self, config: &ProjectConfig) -> Result<ProjectStructure> {
        let framework = config.framework.as_deref().unwrap_or("warp");

        let cargo_toml = format!(
            r#"[package]
name = "{}"
version = "0.1.0"
edition = "2021"

[dependencies]
tokio = {{ version = "1.0", features = ["full"] }}
{} = "0.3"
anyhow = "1.0"
"#,
            config.name, framework
        );

        let main_rs = format!(
            r#"use anyhow::Result;
use {}::*;

#[tokio::main]
async fn main() -> Result<()> {{
    println!("Starting {} server...");
    Ok(())
}}
"#,
            framework, config.name
        );

        Ok(ProjectStructure {
            files: vec![
                ("Cargo.toml".to_string(), cargo_toml),
                ("src/main.rs".to_string(), main_rs),
                (".gitignore".to_string(), "/target\n".to_string()),
                ("README.md".to_string(), format!("# {}\n", config.name)),
            ],
            directories: vec!["src".to_string(), "tests".to_string()],
        })
    }

    fn supported_types(&self) -> Vec<ProjectType> {
        vec![
            ProjectType::RustWeb,
            ProjectType::RustCli,
            ProjectType::RustLib,
        ]
    }
}

pub struct NextJsGenerator;

impl NextJsGenerator {
    pub fn new() -> Self {
        Self
    }
}

impl ProjectGenerator for NextJsGenerator {
    fn generate(&self, config: &ProjectConfig) -> Result<ProjectStructure> {
        let package_json = format!(
            r#"{{
  "name": "{}",
  "version": "0.1.0",
  "private": true,
  "scripts": {{
    "dev": "next dev",
    "build": "next build",
    "start": "next start"
  }},
  "dependencies": {{
    "next": "14.0.0",
    "react": "18.0.0",
    "react-dom": "18.0.0"
  }}
}}
"#,
            config.name
        );

        let index_tsx = r#"export default function Home() {
  return (
    <div>
      <h1>Welcome to Next.js!</h1>
    </div>
  );
}
"#;

        Ok(ProjectStructure {
            files: vec![
                ("package.json".to_string(), package_json),
                ("pages/index.tsx".to_string(), index_tsx.to_string()),
                (
                    ".gitignore".to_string(),
                    "node_modules\n.next\n".to_string(),
                ),
            ],
            directories: vec![
                "pages".to_string(),
                "public".to_string(),
                "styles".to_string(),
            ],
        })
    }

    fn supported_types(&self) -> Vec<ProjectType> {
        vec![ProjectType::NextJs, ProjectType::Nuxt]
    }
}
