use anyhow::Result;
use chrono::Utc;
use serde_json::json;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

/// Mock GitHub Pages registry for testing
pub struct MockGitHubRegistry {
    pub temp_dir: TempDir,
    pub index_path: PathBuf,
}

impl MockGitHubRegistry {
    pub fn new() -> Result<Self> {
        let temp_dir = TempDir::new()?;
        let index_path = temp_dir.path().join("index.json");

        // Create mock registry index.json
        let index_data = create_mock_index();
        fs::write(&index_path, serde_json::to_string_pretty(&index_data)?)?;

        Ok(Self {
            temp_dir,
            index_path,
        })
    }

    /// Get the path to the mock index.json file
    pub fn index_path(&self) -> &PathBuf {
        &self.index_path
    }

    /// Get the content of the index.json file
    pub fn index_content(&self) -> Result<String> {
        Ok(fs::read_to_string(&self.index_path)?)
    }

    /// Update the index.json with new data
    pub fn update_index(&self, data: serde_json::Value) -> Result<()> {
        fs::write(&self.index_path, serde_json::to_string_pretty(&data)?)?;
        Ok(())
    }

    /// Create a mock gpack repository structure
    pub fn create_mock_gpack(&self, id: &str, version: &str) -> Result<PathBuf> {
        let gpack_dir = self.temp_dir.path().join("gpacks").join(id).join(version);
        fs::create_dir_all(&gpack_dir)?;

        // Create templates directory
        let templates_dir = gpack_dir.join("templates");
        fs::create_dir_all(&templates_dir)?;

        // Create mock ggen.toml manifest
        let manifest_content = create_mock_gpack_manifest(id, version);
        fs::write(templates_dir.join("ggen.toml"), manifest_content)?;

        // Create mock template file
        let template_content = create_mock_template(id);
        fs::write(templates_dir.join("main.tmpl"), template_content)?;

        Ok(gpack_dir)
    }
}

fn create_mock_index() -> serde_json::Value {
    json!({
        "updated": Utc::now().to_rfc3339(),
        "gpacks": [
            {
                "id": "io.ggen.rust.cli-subcommand",
                "name": "Rust CLI subcommand",
                "description": "Generate clap subcommands for Rust CLI applications",
                "tags": ["rust", "cli", "clap", "subcommand"],
                "versions": [
                    {
                        "version": "0.1.0",
                        "git_url": "https://github.com/mock/gpack-rust-cli.git",
                        "git_rev": "abc123def456",
                        "manifest_url": "https://raw.githubusercontent.com/mock/gpack-rust-cli/abc123def456/templates/ggen.toml",
                        "sha256": "mock_sha256_hash_1"
                    },
                    {
                        "version": "0.2.0",
                        "git_url": "https://github.com/mock/gpack-rust-cli.git",
                        "git_rev": "def456ghi789",
                        "manifest_url": "https://raw.githubusercontent.com/mock/gpack-rust-cli/def456ghi789/templates/ggen.toml",
                        "sha256": "mock_sha256_hash_2"
                    }
                ]
            },
            {
                "id": "io.ggen.python.fastapi",
                "name": "FastAPI endpoint",
                "description": "Generate FastAPI endpoints with Pydantic models",
                "tags": ["python", "fastapi", "api", "pydantic"],
                "versions": [
                    {
                        "version": "0.1.0",
                        "git_url": "https://github.com/mock/gpack-python-fastapi.git",
                        "git_rev": "ghi789jkl012",
                        "manifest_url": "https://raw.githubusercontent.com/mock/gpack-python-fastapi/ghi789jkl012/templates/ggen.toml",
                        "sha256": "mock_sha256_hash_3"
                    }
                ]
            },
            {
                "id": "io.ggen.macros.std",
                "name": "Standard macros",
                "description": "Common template macros and utilities",
                "tags": ["macros", "utilities", "common"],
                "versions": [
                    {
                        "version": "0.1.0",
                        "git_url": "https://github.com/mock/gpack-macros-std.git",
                        "git_rev": "jkl012mno345",
                        "manifest_url": "https://raw.githubusercontent.com/mock/gpack-macros-std/jkl012mno345/templates/ggen.toml",
                        "sha256": "mock_sha256_hash_4"
                    }
                ]
            }
        ]
    })
}

fn create_mock_gpack_manifest(id: &str, version: &str) -> String {
    format!(
        r#"[gpack]
id = "{}"
name = "Mock Gpack"
version = "{}"
description = "A mock gpack for testing"
license = "MIT"
ggen_compat = ">=0.1 <0.4"

[dependencies]

[templates]
entrypoints = ["main.tmpl"]
includes = []

[rdf]
base = "http://example.org/"
prefixes.ex = "http://example.org/"
files = []
inline = []

[queries]
files = []
aliases = {{}}

[shapes]
files = []

[preset]
config = null
vars = {{ name = "World", author = "Test Author" }}
"#,
        id, version
    )
}

fn create_mock_template(id: &str) -> String {
    format!(
        r#"---
to: "output/{{{{ name | lower }}}}.rs"
vars:
  - name
  - description
---

// Generated by gpack: {}
// Template: main.tmpl

use clap::{{Args, Parser, Subcommand}};

#[derive(Parser)]
#[command(name = "{}")]
#[command(about = "{}")]
struct Cli {{
    #[command(subcommands)]
    command: Commands,
}}

#[derive(Subcommand)]
enum Commands {{
    /// Example command
    Example {{
        /// Example argument
        #[arg(short, long)]
        name: Option<String>,
    }},
}}

fn main() {{
    let cli = Cli::parse();
    
    match cli.command {{
        Commands::Example {{ name }} => {{
            println!("Hello, {{}}!", name.unwrap_or_else(|| "World".to_string()));
        }},
    }}
}}
"#,
        id, id, "A mock CLI application"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mock_github_registry_creation() {
        let registry = MockGitHubRegistry::new().unwrap();

        // Test that index.json was created
        assert!(registry.index_path().exists());

        // Test that index content is valid JSON
        let content = registry.index_content().unwrap();
        let data: serde_json::Value = serde_json::from_str(&content).unwrap();
        assert!(data["gpacks"].is_array());
        assert_eq!(data["gpacks"].as_array().unwrap().len(), 3);
    }

    #[test]
    fn test_mock_gpack_creation() {
        let registry = MockGitHubRegistry::new().unwrap();

        // Create a mock gpack
        let gpack_dir = registry.create_mock_gpack("io.ggen.test", "0.1.0").unwrap();

        // Test that the structure was created
        assert!(gpack_dir.exists());
        assert!(gpack_dir.join("templates").exists());
        assert!(gpack_dir.join("templates").join("ggen.toml").exists());
        assert!(gpack_dir.join("templates").join("main.tmpl").exists());

        // Test manifest content
        let manifest_content =
            std::fs::read_to_string(gpack_dir.join("templates").join("ggen.toml")).unwrap();
        assert!(manifest_content.contains("id = \"io.ggen.test\""));
        assert!(manifest_content.contains("version = \"0.1.0\""));

        // Test template content
        let template_content =
            std::fs::read_to_string(gpack_dir.join("templates").join("main.tmpl")).unwrap();
        assert!(template_content.contains("Generated by gpack: io.ggen.test"));
    }

    #[test]
    fn test_mock_index_update() {
        let registry = MockGitHubRegistry::new().unwrap();

        // Update the index with new data
        let new_data = json!({
            "updated": Utc::now().to_rfc3339(),
            "gpacks": [
                {
                    "id": "io.ggen.test.new",
                    "name": "New Test Pack",
                    "description": "A new test pack",
                    "tags": ["test"],
                    "versions": [
                        {
                            "version": "1.0.0",
                            "git_url": "https://github.com/test/new.git",
                            "git_rev": "abc123",
                            "manifest_url": "https://raw.githubusercontent.com/test/new/abc123/templates/ggen.toml",
                            "sha256": "new_hash"
                        }
                    ]
                }
            ]
        });

        registry.update_index(new_data.clone()).unwrap();

        // Verify the update
        let content = registry.index_content().unwrap();
        let data: serde_json::Value = serde_json::from_str(&content).unwrap();
        assert_eq!(data["gpacks"].as_array().unwrap().len(), 1);
        assert_eq!(data["gpacks"][0]["id"], "io.ggen.test.new");
    }
}
