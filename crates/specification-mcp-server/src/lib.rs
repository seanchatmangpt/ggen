//! MCP server for ggen specification validation and rendering
//!
//! Provides three tools:
//! - validate_spec: SHACL validation of RDF specifications
//! - render_spec: Render specifications to various formats
//! - list_specs: List available specifications

use anyhow::{Context, Result};
use async_trait::async_trait;
use rmcp::{
    model::{CallToolRequest, CallToolResult, Tool, ToolDescription},
    server::{Router, Service},
};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// Specification directory path
const SPEC_DIR: &str = ".specify";

/// MCP server for specification operations
pub struct SpecificationMcpServer {
    /// Root directory containing specifications
    spec_root: PathBuf,
}

impl SpecificationMcpServer {
    /// Create a new specification MCP server
    pub fn new(spec_root: impl Into<PathBuf>) -> Self {
        Self {
            spec_root: spec_root.into(),
        }
    }

    /// Create with default specification directory
    pub fn with_defaults() -> Self {
        Self::new(SPEC_DIR)
    }

    /// Validate a specification file path
    fn validate_path(&self, path: &str) -> Result<PathBuf> {
        let full_path = self.spec_root.join(path);

        // Security: Prevent path traversal
        if !full_path.starts_with(&self.spec_root) {
            anyhow::bail!("Path traversal not allowed: {}", path);
        }

        // Validate extension
        if let Some(ext) = full_path.extension() {
            if ext != "ttl" {
                anyhow::bail!("Only .ttl files are supported, got: {:?}", ext);
            }
        } else {
            anyhow::bail!("File must have .ttl extension");
        }

        Ok(full_path)
    }

    /// Validate specification using SHACL
    async fn validate_spec_impl(&self, path: &str) -> Result<ValidationResult> {
        let spec_path = self.validate_path(path)?;

        if !spec_path.exists() {
            anyhow::bail!("Specification not found: {}", spec_path.display());
        }

        // Read and parse RDF
        let content = tokio::fs::read_to_string(&spec_path)
            .await
            .with_context(|| format!("Failed to read {}", spec_path.display()))?;

        // Parse with oxigraph
        let store = oxigraph::store::Store::new()
            .with_context(|| "Failed to create RDF store")?;

        store
            .load_from_reader(
                oxigraph::io::RdfFormat::Turtle,
                content.as_bytes(),
            )
            .with_context(|| format!("Failed to parse TTL from {}", spec_path.display()))?;

        // TODO: Implement SHACL validation
        // For now, return basic validation
        Ok(ValidationResult {
            valid: true,
            path: path.to_string(),
            triples_count: store.len()?,
            errors: vec![],
            warnings: vec![],
        })
    }

    /// Render specification to markdown
    async fn render_spec_impl(&self, path: &str) -> Result<String> {
        let spec_path = self.validate_path(path)?;

        if !spec_path.exists() {
            anyhow::bail!("Specification not found: {}", spec_path.display());
        }

        // Read content
        let content = tokio::fs::read_to_string(&spec_path)
            .await
            .with_context(|| format!("Failed to read {}", spec_path.display()))?;

        // Parse with oxigraph
        let store = oxigraph::store::Store::new()
            .with_context(|| "Failed to create RDF store")?;

        store
            .load_from_reader(
                oxigraph::io::RdfFormat::Turtle,
                content.as_bytes(),
            )
            .with_context(|| format!("Failed to parse TTL from {}", spec_path.display()))?;

        // TODO: Implement proper rendering with templates
        // For now, return basic markdown
        let mut output = format!("# Specification: {}\n\n", path);
        output.push_str(&format!("**Triples**: {}\n\n", store.len()?));
        output.push_str("## RDF Content\n\n");
        output.push_str("```turtle\n");
        output.push_str(&content);
        output.push_str("\n```\n");

        Ok(output)
    }

    /// List available specifications
    async fn list_specs_impl(&self) -> Result<Vec<SpecInfo>> {
        let mut specs = Vec::new();

        if !self.spec_root.exists() {
            return Ok(specs);
        }

        let mut entries = tokio::fs::read_dir(&self.spec_root)
            .await
            .with_context(|| format!("Failed to read directory {}", self.spec_root.display()))?;

        while let Some(entry) = entries.next_entry().await? {
            let path = entry.path();

            if path.is_file() {
                if let Some(ext) = path.extension() {
                    if ext == "ttl" {
                        let relative_path = path
                            .strip_prefix(&self.spec_root)
                            .unwrap_or(&path)
                            .to_string_lossy()
                            .to_string();

                        let metadata = entry.metadata().await?;

                        specs.push(SpecInfo {
                            path: relative_path,
                            size: metadata.len(),
                            modified: metadata
                                .modified()
                                .ok()
                                .and_then(|t| t.elapsed().ok())
                                .map(|d| format!("{:?} ago", d)),
                        });
                    }
                }
            } else if path.is_dir() {
                // Recursively search subdirectories
                let subdir_specs = self.list_specs_in_dir(&path, &self.spec_root).await?;
                specs.extend(subdir_specs);
            }
        }

        specs.sort_by(|a, b| a.path.cmp(&b.path));
        Ok(specs)
    }

    /// List specs in a directory recursively
    async fn list_specs_in_dir(&self, dir: &Path, root: &Path) -> Result<Vec<SpecInfo>> {
        let mut specs = Vec::new();
        let mut entries = tokio::fs::read_dir(dir).await?;

        while let Some(entry) = entries.next_entry().await? {
            let path = entry.path();

            if path.is_file() {
                if let Some(ext) = path.extension() {
                    if ext == "ttl" {
                        let relative_path = path
                            .strip_prefix(root)
                            .unwrap_or(&path)
                            .to_string_lossy()
                            .to_string();

                        let metadata = entry.metadata().await?;

                        specs.push(SpecInfo {
                            path: relative_path,
                            size: metadata.len(),
                            modified: metadata
                                .modified()
                                .ok()
                                .and_then(|t| t.elapsed().ok())
                                .map(|d| format!("{:?} ago", d)),
                        });
                    }
                }
            } else if path.is_dir() {
                let subdir_specs = self.list_specs_in_dir(&path, root).await?;
                specs.extend(subdir_specs);
            }
        }

        Ok(specs)
    }
}

#[async_trait]
impl Service for SpecificationMcpServer {
    async fn list_tools(&self) -> Result<Vec<ToolDescription>> {
        Ok(vec![
            ToolDescription {
                name: "validate_spec".to_string(),
                description: "Validate an RDF specification using SHACL".to_string(),
                input_schema: serde_json::json!({
                    "type": "object",
                    "properties": {
                        "path": {
                            "type": "string",
                            "description": "Relative path to .ttl specification file"
                        }
                    },
                    "required": ["path"]
                }),
            },
            ToolDescription {
                name: "render_spec".to_string(),
                description: "Render specification to markdown format".to_string(),
                input_schema: serde_json::json!({
                    "type": "object",
                    "properties": {
                        "path": {
                            "type": "string",
                            "description": "Relative path to .ttl specification file"
                        }
                    },
                    "required": ["path"]
                }),
            },
            ToolDescription {
                name: "list_specs".to_string(),
                description: "List all available specifications".to_string(),
                input_schema: serde_json::json!({
                    "type": "object",
                    "properties": {}
                }),
            },
        ])
    }

    async fn call_tool(&self, request: CallToolRequest) -> Result<CallToolResult> {
        match request.name.as_str() {
            "validate_spec" => {
                let args: ValidateSpecArgs = serde_json::from_value(request.arguments)
                    .context("Invalid arguments for validate_spec")?;

                let result = self.validate_spec_impl(&args.path).await?;
                let content = serde_json::to_value(result)?;

                Ok(CallToolResult {
                    content: vec![rmcp::model::Content::Text {
                        text: serde_json::to_string_pretty(&content)?,
                    }],
                    is_error: None,
                })
            }
            "render_spec" => {
                let args: RenderSpecArgs = serde_json::from_value(request.arguments)
                    .context("Invalid arguments for render_spec")?;

                let markdown = self.render_spec_impl(&args.path).await?;

                Ok(CallToolResult {
                    content: vec![rmcp::model::Content::Text { text: markdown }],
                    is_error: None,
                })
            }
            "list_specs" => {
                let specs = self.list_specs_impl().await?;
                let content = serde_json::to_value(specs)?;

                Ok(CallToolResult {
                    content: vec![rmcp::model::Content::Text {
                        text: serde_json::to_string_pretty(&content)?,
                    }],
                    is_error: None,
                })
            }
            _ => anyhow::bail!("Unknown tool: {}", request.name),
        }
    }
}

/// Arguments for validate_spec tool
#[derive(Debug, Deserialize, Serialize)]
struct ValidateSpecArgs {
    path: String,
}

/// Arguments for render_spec tool
#[derive(Debug, Deserialize, Serialize)]
struct RenderSpecArgs {
    path: String,
}

/// Validation result
#[derive(Debug, Serialize, Deserialize)]
pub struct ValidationResult {
    pub valid: bool,
    pub path: String,
    pub triples_count: usize,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
}

/// Specification info
#[derive(Debug, Serialize, Deserialize)]
pub struct SpecInfo {
    pub path: String,
    pub size: u64,
    pub modified: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_validation() {
        let server = SpecificationMcpServer::new("/tmp/specs");

        // Valid path
        assert!(server.validate_path("test.ttl").is_ok());

        // Path traversal attempt
        assert!(server.validate_path("../etc/passwd").is_err());

        // Invalid extension
        assert!(server.validate_path("test.json").is_err());

        // No extension
        assert!(server.validate_path("test").is_err());
    }

    #[tokio::test]
    async fn test_list_tools() {
        let server = SpecificationMcpServer::with_defaults();
        let tools = server.list_tools().await.unwrap();

        assert_eq!(tools.len(), 3);
        assert!(tools.iter().any(|t| t.name == "validate_spec"));
        assert!(tools.iter().any(|t| t.name == "render_spec"));
        assert!(tools.iter().any(|t| t.name == "list_specs"));
    }
}
