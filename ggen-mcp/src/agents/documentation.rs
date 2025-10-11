//! Documentation Agent - API Documentation and Architecture Docs
//!
//! This agent generates and maintains comprehensive documentation for the MCP server:
//! - API documentation with examples and schemas
//! - Architecture documentation with diagrams
//! - Integration guides and tutorials
//! - Living documentation that stays current
//!
//! # Documentation Patterns
//!
//! ## Living Documentation
//! - **Auto-generated** from code and schemas
//! - **Version-controlled** with the codebase
//! - **Always current** with latest changes
//! - **Testable** examples and snippets
//!
//! ## Documentation Types
//! - **API Reference** - Complete tool documentation
//! - **Architecture Guide** - System design and patterns
//! - **Integration Guide** - How to integrate with MCP
//! - **Tutorials** - Step-by-step guides
//! - **Examples** - Working code samples
//!
//! ## Documentation Quality
//! - **Clear and concise** language
//! - **Comprehensive examples** for each feature
//! - **Error handling** documentation
//! - **Performance considerations** and SLOs

use crate::agents::{Agent, AgentMetadata, AgentStatus, AgentId};
use crate::error::{GgenMcpError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use uuid::Uuid;
use chrono::{DateTime, Utc};

/// Documentation section
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DocumentationSection {
    pub id: String,
    pub title: String,
    pub content: String,
    pub section_type: SectionType,
    pub last_updated: chrono::DateTime<Utc>,
    pub version: String,
    pub tags: Vec<String>,
}

/// Documentation section types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum SectionType {
    ApiReference,
    Architecture,
    Integration,
    Tutorial,
    Example,
    Troubleshooting,
}

/// API documentation entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiDocumentation {
    pub tool_name: String,
    pub description: String,
    pub parameters: Vec<ParameterDocumentation>,
    pub examples: Vec<ExampleDocumentation>,
    pub error_codes: Vec<ErrorDocumentation>,
    pub performance_notes: String,
    pub last_updated: chrono::DateTime<Utc>,
}

/// Parameter documentation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParameterDocumentation {
    pub name: String,
    pub description: String,
    pub required: bool,
    pub data_type: String,
    pub default_value: Option<String>,
    pub validation_rules: Vec<String>,
    pub examples: Vec<String>,
}

/// Example documentation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExampleDocumentation {
    pub title: String,
    pub description: String,
    pub input: serde_json::Value,
    pub output: serde_json::Value,
    pub code_snippet: String,
    pub language: String,
}

/// Error documentation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorDocumentation {
    pub error_code: String,
    pub description: String,
    pub possible_causes: Vec<String>,
    pub solutions: Vec<String>,
    pub severity: String,
}

/// Documentation Agent implementation
pub struct DocumentationAgent {
    id: AgentId,
    documentation_sections: HashMap<String, DocumentationSection>,
    api_documentation: HashMap<String, ApiDocumentation>,
    generated_files: Vec<PathBuf>,
    last_generation_time: Option<chrono::DateTime<Utc>>,
}

impl DocumentationAgent {
    pub fn new() -> Self {
        let mut agent = Self {
            id: Uuid::new_v4(),
            documentation_sections: HashMap::new(),
            api_documentation: HashMap::new(),
            generated_files: Vec::new(),
            last_generation_time: None,
        };

        // Initialize documentation sections
        agent.initialize_documentation_sections();
        
        // Initialize API documentation
        agent.initialize_api_documentation();

        agent
    }

    /// Initialize documentation sections
    fn initialize_documentation_sections(&mut self) {
        self.documentation_sections.insert("overview".to_string(), DocumentationSection {
            id: "overview".to_string(),
            title: "GGen MCP Server Overview".to_string(),
            content: r#"
# GGen MCP Server

The GGen MCP (Model Context Protocol) server exposes ggen's graph-aware code generation capabilities to AI assistants through a standardized API.

## Features

- **40+ MCP Tools** for code generation
- **RDF/SPARQL Integration** for semantic queries
- **Marketplace Access** to template packages (gpacks)
- **Deterministic Generation** with reproducible outputs
- **Byzantine Fault Tolerance** for reliability

## Architecture

The server implements a 12-agent architecture with specialized agents for:
- Security and input validation
- Performance monitoring and optimization
- Error handling and fault recovery
- CLI integration and bridging
- Testing and quality assurance
- Documentation and API management

## Quick Start

```bash
# Install ggen (includes MCP server)
brew install seanchatmangpt/tap/ggen

# Start MCP server
ggen mcp start
```
"#.to_string(),
            section_type: SectionType::Architecture,
            last_updated: Utc::now(),
            version: "1.0.0".to_string(),
            tags: vec!["overview".to_string(), "architecture".to_string()],
        });

        self.documentation_sections.insert("security".to_string(), DocumentationSection {
            id: "security".to_string(),
            title: "Security Considerations".to_string(),
            content: r#"
# Security Considerations

The GGen MCP server implements comprehensive security measures to protect against common attacks.

## Input Validation

All inputs are validated for:
- **Path traversal** - Blocked attempts to access files outside allowed directories
- **Code injection** - Prevented execution of malicious code in templates
- **Resource exhaustion** - Limited memory and CPU usage per operation
- **Format validation** - Ensured proper data types and formats

## Rate Limiting

- **Per-client rate limiting** - Maximum 100 requests per minute per client
- **Global rate limiting** - System-wide request throttling
- **Exponential backoff** - Automatic retry with increasing delays

## Authentication

- **API key authentication** - Required for all operations
- **Session management** - Secure session handling
- **Audit logging** - Complete audit trail of all operations

## Best Practices

1. **Validate all inputs** before processing
2. **Use parameterized queries** for SPARQL operations
3. **Limit resource usage** per operation
4. **Monitor for suspicious activity**
5. **Keep dependencies updated**
"#.to_string(),
            section_type: SectionType::Architecture,
            last_updated: Utc::now(),
            version: "1.0.0".to_string(),
            tags: vec!["security".to_string(), "validation".to_string()],
        });
    }

    /// Initialize API documentation
    fn initialize_api_documentation(&mut self) {
        self.api_documentation.insert("project_gen".to_string(), ApiDocumentation {
            tool_name: "project_gen".to_string(),
            description: "Generate files from a template with variables".to_string(),
            parameters: vec![
                ParameterDocumentation {
                    name: "template".to_string(),
                    description: "Path to the template file or template name".to_string(),
                    required: true,
                    data_type: "string".to_string(),
                    default_value: None,
                    validation_rules: vec!["non_empty".to_string(), "path_safe".to_string()],
                    examples: vec!["rust-lib".to_string(), "./templates/my-template.tmpl".to_string()],
                },
                ParameterDocumentation {
                    name: "output".to_string(),
                    description: "Output directory for generated files".to_string(),
                    required: false,
                    data_type: "string".to_string(),
                    default_value: Some(".".to_string()),
                    validation_rules: vec!["path_safe".to_string()],
                    examples: vec!["./my-project".to_string(), "/tmp/generated".to_string()],
                },
                ParameterDocumentation {
                    name: "vars".to_string(),
                    description: "Variables to pass to the template".to_string(),
                    required: false,
                    data_type: "object".to_string(),
                    default_value: None,
                    validation_rules: vec!["json_format".to_string()],
                    examples: vec![r#"{"name": "my-project", "version": "1.0.0"}"#.to_string()],
                },
            ],
            examples: vec![
                ExampleDocumentation {
                    title: "Generate Rust Library".to_string(),
                    description: "Generate a new Rust library project".to_string(),
                    input: serde_json::json!({
                        "template": "rust-lib",
                        "output": "./my-rust-lib",
                        "vars": {
                            "name": "my-rust-lib",
                            "version": "0.1.0"
                        }
                    }),
                    output: serde_json::json!({
                        "status": "success",
                        "files_generated": [
                            "Cargo.toml",
                            "src/lib.rs",
                            "README.md"
                        ],
                        "output_dir": "./my-rust-lib"
                    }),
                    code_snippet: r#"// MCP client example
const result = await mcpClient.callTool("project_gen", {
    template: "rust-lib",
    output: "./my-rust-lib",
    vars: {
        name: "my-rust-lib",
        version: "0.1.0"
    }
});"#.to_string(),
                    language: "javascript".to_string(),
                },
            ],
            error_codes: vec![
                ErrorDocumentation {
                    error_code: "TEMPLATE_NOT_FOUND".to_string(),
                    description: "The specified template was not found".to_string(),
                    possible_causes: vec![
                        "Template name is incorrect".to_string(),
                        "Template file does not exist".to_string(),
                        "Template path is invalid".to_string(),
                    ],
                    solutions: vec![
                        "Check template name spelling".to_string(),
                        "Verify template file exists".to_string(),
                        "Use absolute path for template".to_string(),
                    ],
                    severity: "error".to_string(),
                },
            ],
            performance_notes: "Generation typically takes 1-5 seconds depending on template complexity and output size.".to_string(),
            last_updated: Utc::now(),
        });

        self.api_documentation.insert("graph_query".to_string(), ApiDocumentation {
            tool_name: "graph_query".to_string(),
            description: "Execute SPARQL query against RDF graph store".to_string(),
            parameters: vec![
                ParameterDocumentation {
                    name: "data".to_string(),
                    description: "RDF data file or inline RDF content".to_string(),
                    required: true,
                    data_type: "string".to_string(),
                    default_value: None,
                    validation_rules: vec!["non_empty".to_string(), "rdf_format".to_string()],
                    examples: vec!["./data.ttl".to_string(), "data.rdf".to_string()],
                },
                ParameterDocumentation {
                    name: "query".to_string(),
                    description: "SPARQL query to execute".to_string(),
                    required: true,
                    data_type: "string".to_string(),
                    default_value: None,
                    validation_rules: vec!["non_empty".to_string(), "sparql_safe".to_string()],
                    examples: vec![
                        "SELECT ?s ?p ?o WHERE { ?s ?p ?o }".to_string(),
                        "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }".to_string(),
                    ],
                },
                ParameterDocumentation {
                    name: "format".to_string(),
                    description: "Output format for query results".to_string(),
                    required: false,
                    data_type: "string".to_string(),
                    default_value: Some("json".to_string()),
                    validation_rules: vec!["enum:json,csv,table".to_string()],
                    examples: vec!["json".to_string(), "csv".to_string(), "table".to_string()],
                },
            ],
            examples: vec![
                ExampleDocumentation {
                    title: "Query RDF Data".to_string(),
                    description: "Execute a simple SPARQL SELECT query".to_string(),
                    input: serde_json::json!({
                        "data": "./ontology.ttl",
                        "query": "SELECT ?subject ?predicate ?object WHERE { ?subject ?predicate ?object }",
                        "format": "json"
                    }),
                    output: serde_json::json!({
                        "status": "success",
                        "results": {
                            "bindings": [
                                {
                                    "subject": "http://example.org/Person1",
                                    "predicate": "http://example.org/name",
                                    "object": "Alice"
                                }
                            ]
                        }
                    }),
                    code_snippet: r#"// MCP client example
const result = await mcpClient.callTool("graph_query", {
    data: "./ontology.ttl",
    query: "SELECT ?subject ?predicate ?object WHERE { ?subject ?predicate ?object }",
    format: "json"
});"#.to_string(),
                    language: "javascript".to_string(),
                },
            ],
            error_codes: vec![
                ErrorDocumentation {
                    error_code: "SPARQL_SYNTAX_ERROR".to_string(),
                    description: "Invalid SPARQL query syntax".to_string(),
                    possible_causes: vec![
                        "Malformed SPARQL query".to_string(),
                        "Invalid prefixes".to_string(),
                        "Syntax errors in query".to_string(),
                    ],
                    solutions: vec![
                        "Validate SPARQL syntax".to_string(),
                        "Check prefix declarations".to_string(),
                        "Use SPARQL validator".to_string(),
                    ],
                    severity: "error".to_string(),
                },
            ],
            performance_notes: "Query execution time depends on data size and query complexity. Large datasets may take several seconds.".to_string(),
            last_updated: Utc::now(),
        });
    }

    /// Generate documentation for a specific tool
    pub fn generate_tool_documentation(&self, tool_name: &str) -> Result<String> {
        if let Some(api_doc) = self.api_documentation.get(tool_name) {
            let mut doc = format!("# {}\n\n", api_doc.tool_name);
            doc.push_str(&format!("{}\n\n", api_doc.description));

            // Parameters section
            doc.push_str("## Parameters\n\n");
            for param in &api_doc.parameters {
                doc.push_str(&format!("### `{}`\n", param.name));
                doc.push_str(&format!("- **Type**: {}\n", param.data_type));
                doc.push_str(&format!("- **Required**: {}\n", param.required));
                doc.push_str(&format!("- **Description**: {}\n", param.description));
                
                if let Some(default) = &param.default_value {
                    doc.push_str(&format!("- **Default**: `{}`\n", default));
                }
                
                if !param.validation_rules.is_empty() {
                    doc.push_str(&format!("- **Validation**: {}\n", param.validation_rules.join(", ")));
                }
                
                if !param.examples.is_empty() {
                    doc.push_str(&format!("- **Examples**: {}\n", param.examples.join(", ")));
                }
                
                doc.push_str("\n");
            }

            // Examples section
            if !api_doc.examples.is_empty() {
                doc.push_str("## Examples\n\n");
                for example in &api_doc.examples {
                    doc.push_str(&format!("### {}\n", example.title));
                    doc.push_str(&format!("{}\n\n", example.description));
                    doc.push_str("**Input:**\n```json\n");
                    doc.push_str(&serde_json::to_string_pretty(&example.input).unwrap());
                    doc.push_str("\n```\n\n");
                    doc.push_str("**Output:**\n```json\n");
                    doc.push_str(&serde_json::to_string_pretty(&example.output).unwrap());
                    doc.push_str("\n```\n\n");
                    doc.push_str(&format!("**Code Example:**\n```{}\n", example.language));
                    doc.push_str(&example.code_snippet);
                    doc.push_str("\n```\n\n");
                }
            }

            // Error codes section
            if !api_doc.error_codes.is_empty() {
                doc.push_str("## Error Codes\n\n");
                for error in &api_doc.error_codes {
                    doc.push_str(&format!("### {}\n", error.error_code));
                    doc.push_str(&format!("- **Description**: {}\n", error.description));
                    doc.push_str(&format!("- **Severity**: {}\n", error.severity));
                    
                    if !error.possible_causes.is_empty() {
                        doc.push_str("- **Possible Causes**:\n");
                        for cause in &error.possible_causes {
                            doc.push_str(&format!("  - {}\n", cause));
                        }
                    }
                    
                    if !error.solutions.is_empty() {
                        doc.push_str("- **Solutions**:\n");
                        for solution in &error.solutions {
                            doc.push_str(&format!("  - {}\n", solution));
                        }
                    }
                    
                    doc.push_str("\n");
                }
            }

            // Performance notes
            if !api_doc.performance_notes.is_empty() {
                doc.push_str("## Performance Notes\n\n");
                doc.push_str(&api_doc.performance_notes);
                doc.push_str("\n\n");
            }

            Ok(doc)
        } else {
            Err(GgenMcpError::InvalidParameter(format!("Unknown tool: {}", tool_name)))
        }
    }

    /// Generate complete API documentation
    pub fn generate_api_documentation(&self) -> String {
        let mut doc = String::new();
        doc.push_str("# GGen MCP Server API Documentation\n\n");
        doc.push_str("This document provides comprehensive documentation for all MCP tools available in the GGen server.\n\n");

        // Table of contents
        doc.push_str("## Table of Contents\n\n");
        for tool_name in self.api_documentation.keys() {
            doc.push_str(&format!("- [{}](#{})\n", tool_name, tool_name.replace("_", "-")));
        }
        doc.push_str("\n");

        // Generate documentation for each tool
        for tool_name in self.api_documentation.keys() {
            if let Ok(tool_doc) = self.generate_tool_documentation(tool_name) {
                doc.push_str(&tool_doc);
                doc.push_str("\n---\n\n");
            }
        }

        doc
    }

    /// Generate architecture documentation
    pub fn generate_architecture_documentation(&self) -> String {
        let mut doc = String::new();
        doc.push_str("# GGen MCP Server Architecture\n\n");
        doc.push_str("This document describes the architecture and design of the GGen MCP server.\n\n");

        // Add all architecture sections
        for section in self.documentation_sections.values() {
            if section.section_type == SectionType::Architecture {
                doc.push_str(&format!("## {}\n\n", section.title));
                doc.push_str(&section.content);
                doc.push_str("\n\n");
            }
        }

        doc
    }

    /// Update documentation section
    pub fn update_documentation_section(&mut self, section_id: &str, content: String) -> Result<()> {
        if let Some(section) = self.documentation_sections.get_mut(section_id) {
            section.content = content;
            section.last_updated = Utc::now();
            Ok(())
        } else {
            Err(GgenMcpError::InvalidParameter(format!("Unknown section: {}", section_id)))
        }
    }

    /// Get documentation section
    pub fn get_documentation_section(&self, section_id: &str) -> Option<&DocumentationSection> {
        self.documentation_sections.get(section_id)
    }

    /// Get all documentation sections
    pub fn get_all_sections(&self) -> &HashMap<String, DocumentationSection> {
        &self.documentation_sections
    }

    /// Get API documentation
    pub fn get_api_documentation(&self) -> &HashMap<String, ApiDocumentation> {
        &self.api_documentation
    }
}

#[async_trait::async_trait]
impl Agent for DocumentationAgent {
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Documentation Agent initialized with ID: {}", self.id);
        tracing::info!("Loaded {} documentation sections", self.documentation_sections.len());
        tracing::info!("Loaded {} API documentation entries", self.api_documentation.len());
        Ok(())
    }

    async fn execute(&self, input: serde_json::Value) -> Result<serde_json::Value, Box<dyn std::error::Error>> {
        let operation = input.get("operation")
            .and_then(|v| v.as_str())
            .ok_or("Missing operation")?;

        let agent = DocumentationAgent::new();
        
        let result = match operation {
            "generate_tool_doc" => {
                let tool_name = input.get("tool_name")
                    .and_then(|v| v.as_str())
                    .ok_or("Missing tool_name")?;
                serde_json::to_value(agent.generate_tool_documentation(tool_name)?)?
            }
            "generate_api_doc" => {
                serde_json::to_value(agent.generate_api_documentation())?
            }
            "generate_architecture_doc" => {
                serde_json::to_value(agent.generate_architecture_documentation())?
            }
            "get_section" => {
                let section_id = input.get("section_id")
                    .and_then(|v| v.as_str())
                    .ok_or("Missing section_id")?;
                serde_json::to_value(agent.get_documentation_section(section_id))?
            }
            _ => return Err("Unknown operation".into()),
        };

        Ok(result)
    }

    fn metadata(&self) -> AgentMetadata {
        AgentMetadata {
            id: self.id,
            name: "DocumentationAgent".to_string(),
            version: "1.0.0".to_string(),
            status: AgentStatus::Healthy,
            capabilities: vec![
                "api_documentation".to_string(),
                "architecture_docs".to_string(),
                "living_documentation".to_string(),
                "example_generation".to_string(),
            ],
            last_heartbeat: Utc::now(),
        }
    }

    async fn health_check(&self) -> AgentStatus {
        // Documentation agent is always healthy unless explicitly failed
        AgentStatus::Healthy
    }

    async fn shutdown(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Documentation Agent shutting down");
        tracing::info!("Generated {} documentation files", self.generated_files.len());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_documentation_agent_creation() {
        let agent = DocumentationAgent::new();
        
        assert!(!agent.documentation_sections.is_empty());
        assert!(!agent.api_documentation.is_empty());
    }

    #[test]
    fn test_tool_documentation_generation() {
        let agent = DocumentationAgent::new();
        let doc = agent.generate_tool_documentation("project_gen").unwrap();
        
        assert!(doc.contains("project_gen"));
        assert!(doc.contains("Parameters"));
        assert!(doc.contains("Examples"));
    }

    #[test]
    fn test_api_documentation_generation() {
        let agent = DocumentationAgent::new();
        let doc = agent.generate_api_documentation();
        
        assert!(doc.contains("GGen MCP Server API Documentation"));
        assert!(doc.contains("Table of Contents"));
    }

    #[test]
    fn test_architecture_documentation_generation() {
        let agent = DocumentationAgent::new();
        let doc = agent.generate_architecture_documentation();
        
        assert!(doc.contains("GGen MCP Server Architecture"));
        assert!(doc.contains("Overview"));
    }

    #[test]
    fn test_documentation_section_access() {
        let agent = DocumentationAgent::new();
        let section = agent.get_documentation_section("overview");
        
        assert!(section.is_some());
        assert_eq!(section.unwrap().title, "GGen MCP Server Overview");
    }

    #[tokio::test]
    async fn test_agent_execution() {
        let mut agent = DocumentationAgent::new();
        agent.initialize().await.unwrap();
        
        let input = json!({
            "operation": "generate_tool_doc",
            "tool_name": "project_gen"
        });
        
        let result = agent.execute(input).await.unwrap();
        let doc: String = serde_json::from_value(result).unwrap();
        
        assert!(doc.contains("project_gen"));
    }
}
