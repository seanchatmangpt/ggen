//! Rust project generator
//!
//! This module generates Rust projects for web applications, CLI tools, and libraries.
//! It creates the complete project structure including Cargo.toml, source files,
//! and configuration files.
//!
//! ## Supported Project Types
//!
//! - **RustWeb**: Web applications using async frameworks (Axum, Actix, etc.)
//! - **RustCli**: Command-line interface applications using clap
//! - **RustLib**: Library crates for reusable code
//!
//! ## Features
//!
//! - **Framework support**: Configurable web frameworks (default: Axum)
//! - **Async runtime**: Tokio-based async/await support
//! - **Error handling**: anyhow for error propagation
//! - **Serialization**: serde for data serialization
//! - **Tracing**: Structured logging with tracing
//!
//! ## Examples
//!
//! ### Generating a Rust CLI Project
//!
//! ```rust,no_run
//! use ggen_core::project_generator::{ProjectConfig, ProjectType};
//! use ggen_core::project_generator::rust::RustProjectGenerator;
//! use std::path::PathBuf;
//!
//! # fn main() -> anyhow::Result<()> {
//! let config = ProjectConfig {
//!     name: "my-cli".to_string(),
//!     project_type: ProjectType::RustCli,
//!     framework: None,
//!     path: PathBuf::from("."),
//! };
//!
//! let generator = RustProjectGenerator::new();
//! let structure = generator.generate(&config)?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Generating a Rust Web Project
//!
//! ```rust,no_run
//! use ggen_core::project_generator::{ProjectConfig, ProjectType};
//! use ggen_core::project_generator::rust::RustProjectGenerator;
//! use std::path::PathBuf;
//!
//! # fn main() -> anyhow::Result<()> {
//! let config = ProjectConfig {
//!     name: "my-web".to_string(),
//!     project_type: ProjectType::RustWeb,
//!     framework: Some("axum".to_string()),
//!     path: PathBuf::from("."),
//! };
//!
//! let generator = RustProjectGenerator::new();
//! let structure = generator.generate(&config)?;
//! # Ok(())
//! # }
//! ```

use super::{ProjectConfig, ProjectGenerator, ProjectStructure, ProjectType};
use anyhow::Result;

pub struct RustProjectGenerator;

impl Default for RustProjectGenerator {
    fn default() -> Self {
        Self
    }
}

impl RustProjectGenerator {
    pub fn new() -> Self {
        Self
    }

    fn generate_cargo_toml(&self, config: &ProjectConfig) -> String {
        let dependencies = match config.project_type {
            ProjectType::RustWeb => {
                let framework = config.framework.as_deref().unwrap_or("axum");
                format!(
                    r#"tokio = {{ version = "1.0", features = ["full"] }}
{} = "0.7"
anyhow = "1.0"
serde = {{ version = "1.0", features = ["derive"] }}
serde_json = "1.0"
tracing = "0.1"
tracing-subscriber = "0.3"
"#,
                    framework
                )
            }
            ProjectType::RustCli => r#"clap = { version = "4.0", features = ["derive"] }
anyhow = "1.0"
tokio = { version = "1.0", features = ["full"] }
serde = { version = "1.0", features = ["derive"] }
"#
            .to_string(),
            ProjectType::RustLib => r#"anyhow = "1.0"
serde = { version = "1.0", features = ["derive"] }
"#
            .to_string(),
            _ => String::new(),
        };

        format!(
            r#"[package]
name = "{}"
version = "0.1.0"
edition = "2021"
authors = ["Your Name <you@example.com>"]

[dependencies]
{}

[dev-dependencies]
tokio-test = "0.4"
"#,
            config.name, dependencies
        )
    }

    fn generate_main_rs(&self, config: &ProjectConfig) -> String {
        match config.project_type {
            ProjectType::RustWeb => {
                let framework = config.framework.as_deref().unwrap_or("axum");
                if framework == "axum" {
                    format!(
                        r#"use anyhow::Result;
use axum::{{
    routing::get,
    Router,
}};
use std::net::SocketAddr;
use tracing::info;

#[tokio::main]
async fn main() -> Result<()> {{
    // Initialize tracing
    tracing_subscriber::fmt::init();

    // Build application
    let app = Router::new()
        .route("/", get(root))
        .route("/health", get(health));

    // Run server
    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    info!("Starting {} server on {{}}", addr);

    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}}

async fn root() -> &'static str {{
    "Welcome to {}!"
}}

async fn health() -> &'static str {{
    "OK"
}}
"#,
                        config.name, config.name
                    )
                } else {
                    format!(
                        r#"use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {{
    println!("Starting {} server...");
    Ok(())
}}
"#,
                        config.name
                    )
                }
            }
            ProjectType::RustCli => {
                format!(
                    r#"use anyhow::Result;
use clap::Parser;

#[derive(Parser)]
#[command(name = "{}")]
#[command(about = "A CLI tool", long_about = None)]
struct Cli {{
    #[arg(short, long)]
    verbose: bool,
}}

#[tokio::main]
async fn main() -> Result<()> {{
    let cli = Cli::parse();

    if cli.verbose {{
        println!("Running in verbose mode");
    }}

    println!("Hello from {}!");
    Ok(())
}}
"#,
                    config.name, config.name
                )
            }
            _ => String::new(),
        }
    }

    fn generate_lib_rs(&self, config: &ProjectConfig) -> String {
        format!(
            r#"//! {} - A Rust library
//!
//! This library provides...

use anyhow::Result;

/// Main library function
pub fn run() -> Result<()> {{
    Ok(())
}}

#[cfg(test)]
mod tests {{
    use super::*;

    #[test]
    fn test_run() {{
        assert!(run().is_ok());
    }}
}}
"#,
            config.name
        )
    }

    fn generate_gitignore(&self) -> String {
        r#"/target
/Cargo.lock
.env
.DS_Store
"#
        .to_string()
    }

    fn generate_readme(&self, config: &ProjectConfig) -> String {
        format!(
            r#"# {}

## Description

A {} project created with ggen.

## Installation

```bash
cargo build --release
```

## Usage

```bash
cargo run
```

## Testing

```bash
cargo test
```

## License

MIT
"#,
            config.name, config.project_type
        )
    }
}

impl ProjectGenerator for RustProjectGenerator {
    fn generate(&self, config: &ProjectConfig) -> Result<ProjectStructure> {
        let mut files = vec![
            ("Cargo.toml".to_string(), self.generate_cargo_toml(config)),
            (".gitignore".to_string(), self.generate_gitignore()),
            ("README.md".to_string(), self.generate_readme(config)),
        ];

        let directories = vec!["src".to_string(), "tests".to_string()];

        // Add main.rs or lib.rs based on project type
        match config.project_type {
            ProjectType::RustLib => {
                files.push(("src/lib.rs".to_string(), self.generate_lib_rs(config)));
            }
            _ => {
                files.push(("src/main.rs".to_string(), self.generate_main_rs(config)));
            }
        }

        Ok(ProjectStructure { files, directories })
    }

    fn supported_types(&self) -> Vec<ProjectType> {
        vec![
            ProjectType::RustWeb,
            ProjectType::RustCli,
            ProjectType::RustLib,
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_generate_rust_web_project() {
        let generator = RustProjectGenerator::new();
        let config = ProjectConfig {
            name: "test-web".to_string(),
            project_type: ProjectType::RustWeb,
            framework: Some("axum".to_string()),
            path: PathBuf::from("/tmp"),
        };

        let structure = generator.generate(&config).unwrap();

        // Check files
        assert!(structure.files.iter().any(|(path, _)| path == "Cargo.toml"));
        assert!(structure
            .files
            .iter()
            .any(|(path, _)| path == "src/main.rs"));

        // Check content
        let cargo_toml = structure
            .files
            .iter()
            .find(|(path, _)| path == "Cargo.toml")
            .map(|(_, content)| content)
            .unwrap();

        assert!(cargo_toml.contains("name = \"test-web\""));
        assert!(cargo_toml.contains("axum"));
    }

    #[test]
    fn test_generate_rust_cli_project() {
        let generator = RustProjectGenerator::new();
        let config = ProjectConfig {
            name: "test-cli".to_string(),
            project_type: ProjectType::RustCli,
            framework: None,
            path: PathBuf::from("/tmp"),
        };

        let structure = generator.generate(&config).unwrap();

        let main_rs = structure
            .files
            .iter()
            .find(|(path, _)| path == "src/main.rs")
            .map(|(_, content)| content)
            .unwrap();

        assert!(main_rs.contains("clap::Parser"));
    }

    #[test]
    fn test_generate_rust_lib_project() {
        let generator = RustProjectGenerator::new();
        let config = ProjectConfig {
            name: "test-lib".to_string(),
            project_type: ProjectType::RustLib,
            framework: None,
            path: PathBuf::from("/tmp"),
        };

        let structure = generator.generate(&config).unwrap();

        assert!(structure.files.iter().any(|(path, _)| path == "src/lib.rs"));
        assert!(!structure
            .files
            .iter()
            .any(|(path, _)| path == "src/main.rs"));
    }
}
