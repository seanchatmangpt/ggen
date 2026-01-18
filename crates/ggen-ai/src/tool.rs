//! Tool metadata and definitions for agent tooling
//!
//! Provides type-safe representation of tools that can be registered in the
//! Tool Registry and exposed to agents (DSPy, MCP, GenAI, etc.)

use crate::dspy::Signature;
use crate::error::{GgenAiError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Metadata tags for tool discovery and categorization
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ToolTag {
    /// Tool performs code generation
    CodeGeneration,
    /// Tool performs data validation
    Validation,
    /// Tool performs query generation
    QueryGeneration,
    /// Tool performs ontology operations
    Ontology,
    /// Tool performs template operations
    Template,
    /// Tool performs caching operations
    Caching,
    /// Tool performs text summarization
    Summarization,
    /// Tool performs text analysis
    Analysis,
    /// Tool performs text generation
    Generation,
    /// Tool performs translation
    Translation,
    /// Tool performs financial domain operations
    Financial,
    /// Tool performs banking domain operations
    Banking,
    /// Tool performs insurance domain operations
    Insurance,
    /// Custom user-defined tag
    Custom(String),
}

impl std::fmt::Display for ToolTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CodeGeneration => write!(f, "code_generation"),
            Self::Validation => write!(f, "validation"),
            Self::QueryGeneration => write!(f, "query_generation"),
            Self::Ontology => write!(f, "ontology"),
            Self::Template => write!(f, "template"),
            Self::Caching => write!(f, "caching"),
            Self::Summarization => write!(f, "summarization"),
            Self::Analysis => write!(f, "analysis"),
            Self::Generation => write!(f, "generation"),
            Self::Translation => write!(f, "translation"),
            Self::Financial => write!(f, "financial"),
            Self::Banking => write!(f, "banking"),
            Self::Insurance => write!(f, "insurance"),
            Self::Custom(tag) => write!(f, "{}", tag),
        }
    }
}

/// SLO (Service Level Objective) requirements for tool execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolSlo {
    /// Maximum execution time in milliseconds
    pub timeout_ms: u64,
    /// Maximum number of retries on failure (0 = no retries)
    pub max_retries: u32,
    /// Whether tool execution should be cached
    pub cacheable: bool,
}

impl Default for ToolSlo {
    fn default() -> Self {
        Self {
            timeout_ms: 30_000, // 30 seconds default
            max_retries: 3,
            cacheable: true,
        }
    }
}

/// Authorization scope for tool access
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum AuthScope {
    /// Public - no authentication required
    Public,
    /// Authenticated - requires valid credentials
    Authenticated,
    /// Admin - requires admin credentials
    Admin,
    /// Custom scope
    Custom(String),
}

impl std::fmt::Display for AuthScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Public => write!(f, "public"),
            Self::Authenticated => write!(f, "authenticated"),
            Self::Admin => write!(f, "admin"),
            Self::Custom(scope) => write!(f, "{}", scope),
        }
    }
}

/// Complete tool definition with metadata
///
/// A Tool combines a DSPy Signature with operational metadata,
/// making it discoverable and executable by agents.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Tool {
    /// Unique identifier for the tool
    pub id: String,

    /// Human-readable tool name
    pub name: String,

    /// Version string (e.g., "1.0.0")
    pub version: String,

    /// Detailed description of tool functionality
    pub description: String,

    /// DSPy Signature defining input/output contract
    pub signature: Signature,

    /// Categories and discovery tags
    pub tags: Vec<ToolTag>,

    /// Author or organization that created the tool
    pub author: Option<String>,

    /// URL for documentation
    pub documentation_url: Option<String>,

    /// SLO requirements for execution
    pub slo: ToolSlo,

    /// Required authorization scope
    pub auth_scope: AuthScope,

    /// Additional metadata as key-value pairs
    pub metadata: HashMap<String, String>,

    /// Examples of tool usage (for agent learning)
    pub examples: Vec<ToolExample>,
}

impl Tool {
    /// Create a new tool with required fields
    ///
    /// # Arguments
    /// * `id` - Unique identifier
    /// * `name` - Human-readable name
    /// * `version` - Version string
    /// * `description` - Tool description
    /// * `signature` - DSPy Signature defining the interface
    pub fn new(
        id: impl Into<String>, name: impl Into<String>, version: impl Into<String>,
        description: impl Into<String>, signature: Signature,
    ) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            version: version.into(),
            description: description.into(),
            signature,
            tags: Vec::new(),
            author: None,
            documentation_url: None,
            slo: ToolSlo::default(),
            auth_scope: AuthScope::Public,
            metadata: HashMap::new(),
            examples: Vec::new(),
        }
    }

    /// Add a tag to this tool
    pub fn with_tag(mut self, tag: ToolTag) -> Self {
        self.tags.push(tag);
        self
    }

    /// Add multiple tags
    pub fn with_tags(mut self, tags: impl IntoIterator<Item = ToolTag>) -> Self {
        self.tags.extend(tags);
        self
    }

    /// Set the author
    pub fn with_author(mut self, author: impl Into<String>) -> Self {
        self.author = Some(author.into());
        self
    }

    /// Set documentation URL
    pub fn with_documentation_url(mut self, url: impl Into<String>) -> Self {
        self.documentation_url = Some(url.into());
        self
    }

    /// Set SLO requirements
    pub fn with_slo(mut self, slo: ToolSlo) -> Self {
        self.slo = slo;
        self
    }

    /// Set authorization scope
    pub fn with_auth_scope(mut self, scope: AuthScope) -> Self {
        self.auth_scope = scope;
        self
    }

    /// Add custom metadata
    pub fn with_metadata(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.metadata.insert(key.into(), value.into());
        self
    }

    /// Add usage example
    pub fn with_example(mut self, example: ToolExample) -> Self {
        self.examples.push(example);
        self
    }

    /// Get tool as OpenAPI-compatible tool definition
    ///
    /// Returns a JSON structure suitable for OpenAI function calls or similar APIs
    pub fn to_openapi(&self) -> serde_json::Value {
        use serde_json::json;

        let input_schema = self.signature.as_json_schema();

        json!({
            "type": "function",
            "function": {
                "name": self.id.clone(),
                "description": self.description.clone(),
                "parameters": input_schema
            }
        })
    }

    /// Get tool as MCP tool definition
    ///
    /// Returns a JSON structure suitable for Model Context Protocol tools
    pub fn to_mcp_tool(&self) -> serde_json::Value {
        use serde_json::json;

        let input_schema = self.signature.as_json_schema();

        json!({
            "name": self.id.clone(),
            "description": self.description.clone(),
            "inputSchema": input_schema
        })
    }

    /// Validate that tool is correctly configured
    ///
    /// Checks:
    /// - Non-empty id, name, description
    /// - Signature has at least one input and output
    /// - Version follows semantic versioning
    pub fn validate(&self) -> Result<()> {
        if self.id.trim().is_empty() {
            return Err(GgenAiError::ValidationError {
                message: "Tool id cannot be empty".to_string(),
            });
        }

        if self.name.trim().is_empty() {
            return Err(GgenAiError::ValidationError {
                message: "Tool name cannot be empty".to_string(),
            });
        }

        if self.description.trim().is_empty() {
            return Err(GgenAiError::ValidationError {
                message: "Tool description cannot be empty".to_string(),
            });
        }

        if self.signature.inputs.is_empty() {
            return Err(GgenAiError::ValidationError {
                message: "Tool signature must have at least one input field".to_string(),
            });
        }

        if self.signature.outputs.is_empty() {
            return Err(GgenAiError::ValidationError {
                message: "Tool signature must have at least one output field".to_string(),
            });
        }

        // Validate semantic version
        if !self.version.contains('.') {
            return Err(GgenAiError::ValidationError {
                message: "Tool version must be in semantic versioning format (e.g., 1.0.0)"
                    .to_string(),
            });
        }

        Ok(())
    }
}

/// Example of tool usage for agent learning
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolExample {
    /// Human-readable description of the example
    pub description: String,

    /// Input values for the example
    pub input: HashMap<String, serde_json::Value>,

    /// Expected output for the example
    pub output: HashMap<String, serde_json::Value>,
}

impl ToolExample {
    /// Create a new tool example
    pub fn new(
        description: impl Into<String>, input: HashMap<String, serde_json::Value>,
        output: HashMap<String, serde_json::Value>,
    ) -> Self {
        Self {
            description: description.into(),
            input,
            output,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dspy::field::{InputField, OutputField};

    fn create_test_signature() -> Signature {
        Signature::new("TestTool", "A test tool")
            .with_input(InputField::new("input", "Test input", "String"))
            .with_output(OutputField::new("output", "Test output", "String"))
    }

    #[test]
    fn test_tool_creation() {
        let sig = create_test_signature();
        let tool = Tool::new("test_tool", "Test Tool", "1.0.0", "A test tool", sig);

        assert_eq!(tool.id, "test_tool");
        assert_eq!(tool.name, "Test Tool");
        assert_eq!(tool.version, "1.0.0");
        assert_eq!(tool.auth_scope, AuthScope::Public);
    }

    #[test]
    fn test_tool_with_tags() {
        let sig = create_test_signature();
        let tool = Tool::new("test", "Test", "1.0.0", "Test", sig)
            .with_tag(ToolTag::CodeGeneration)
            .with_tag(ToolTag::Financial);

        assert_eq!(tool.tags.len(), 2);
        assert!(tool.tags.contains(&ToolTag::CodeGeneration));
        assert!(tool.tags.contains(&ToolTag::Financial));
    }

    #[test]
    fn test_tool_with_author() {
        let sig = create_test_signature();
        let tool = Tool::new("test", "Test", "1.0.0", "Test", sig).with_author("John Doe");

        assert_eq!(tool.author, Some("John Doe".to_string()));
    }

    #[test]
    fn test_tool_with_auth_scope() {
        let sig = create_test_signature();
        let tool =
            Tool::new("test", "Test", "1.0.0", "Test", sig).with_auth_scope(AuthScope::Admin);

        assert_eq!(tool.auth_scope, AuthScope::Admin);
    }

    #[test]
    fn test_tool_with_metadata() {
        let sig = create_test_signature();
        let tool = Tool::new("test", "Test", "1.0.0", "Test", sig)
            .with_metadata("key1", "value1")
            .with_metadata("key2", "value2");

        assert_eq!(tool.metadata.get("key1"), Some(&"value1".to_string()));
        assert_eq!(tool.metadata.get("key2"), Some(&"value2".to_string()));
    }

    #[test]
    fn test_tool_validation_success() {
        let sig = create_test_signature();
        let tool = Tool::new("test", "Test", "1.0.0", "Test", sig);

        assert!(tool.validate().is_ok());
    }

    #[test]
    fn test_tool_validation_empty_id() {
        let sig = create_test_signature();
        let tool = Tool::new("", "Test", "1.0.0", "Test", sig);

        assert!(tool.validate().is_err());
    }

    #[test]
    fn test_tool_validation_empty_name() {
        let sig = create_test_signature();
        let tool = Tool::new("test", "", "1.0.0", "Test", sig);

        assert!(tool.validate().is_err());
    }

    #[test]
    fn test_tool_validation_empty_description() {
        let sig = create_test_signature();
        let tool = Tool::new("test", "Test", "1.0.0", "", sig);

        assert!(tool.validate().is_err());
    }

    #[test]
    fn test_tool_validation_no_inputs() {
        let sig = Signature::new("TestTool", "A test tool").with_output(OutputField::new(
            "output",
            "Test output",
            "String",
        ));

        let tool = Tool::new("test", "Test", "1.0.0", "Test", sig);

        assert!(tool.validate().is_err());
    }

    #[test]
    fn test_tool_validation_no_outputs() {
        let sig = Signature::new("TestTool", "A test tool").with_input(InputField::new(
            "input",
            "Test input",
            "String",
        ));

        let tool = Tool::new("test", "Test", "1.0.0", "Test", sig);

        assert!(tool.validate().is_err());
    }

    #[test]
    fn test_tool_validation_invalid_version() {
        let sig = create_test_signature();
        let tool = Tool::new("test", "Test", "invalid", "Test", sig);

        assert!(tool.validate().is_err());
    }

    #[test]
    fn test_tool_to_openapi() {
        let sig = create_test_signature();
        let tool = Tool::new("test_tool", "Test", "1.0.0", "Test Tool", sig);

        let openapi = tool.to_openapi();

        assert_eq!(openapi["type"], "function");
        assert_eq!(openapi["function"]["name"], "test_tool");
        assert_eq!(openapi["function"]["description"], "Test Tool");
        assert!(openapi["function"]["parameters"].is_object());
    }

    #[test]
    fn test_tool_to_mcp_tool() {
        let sig = create_test_signature();
        let tool = Tool::new("test_tool", "Test", "1.0.0", "Test Tool", sig);

        let mcp = tool.to_mcp_tool();

        assert_eq!(mcp["name"], "test_tool");
        assert_eq!(mcp["description"], "Test Tool");
        assert!(mcp["inputSchema"].is_object());
    }

    #[test]
    fn test_tool_tag_display() {
        assert_eq!(ToolTag::CodeGeneration.to_string(), "code_generation");
        assert_eq!(ToolTag::Validation.to_string(), "validation");
        assert_eq!(ToolTag::Financial.to_string(), "financial");
        assert_eq!(
            ToolTag::Custom("custom_tag".to_string()).to_string(),
            "custom_tag"
        );
    }

    #[test]
    fn test_auth_scope_display() {
        assert_eq!(AuthScope::Public.to_string(), "public");
        assert_eq!(AuthScope::Authenticated.to_string(), "authenticated");
        assert_eq!(AuthScope::Admin.to_string(), "admin");
        assert_eq!(
            AuthScope::Custom("custom".to_string()).to_string(),
            "custom"
        );
    }

    #[test]
    fn test_tool_example_creation() {
        let mut input = HashMap::new();
        input.insert("text".to_string(), serde_json::json!("hello"));

        let mut output = HashMap::new();
        output.insert("result".to_string(), serde_json::json!("HELLO"));

        let example = ToolExample::new("Convert to uppercase", input, output);

        assert_eq!(example.description, "Convert to uppercase");
        assert_eq!(example.input.get("text"), Some(&serde_json::json!("hello")));
    }

    #[test]
    fn test_tool_with_multiple_features() {
        let sig = create_test_signature();
        let mut input_map = HashMap::new();
        input_map.insert("text".to_string(), serde_json::json!("test"));

        let mut output_map = HashMap::new();
        output_map.insert("result".to_string(), serde_json::json!("result"));

        let example = ToolExample::new("Test example", input_map, output_map);

        let tool = Tool::new(
            "complete_tool",
            "Complete Tool",
            "2.0.0",
            "A complete tool",
            sig,
        )
        .with_tag(ToolTag::CodeGeneration)
        .with_tag(ToolTag::Validation)
        .with_author("Test Author")
        .with_documentation_url("https://example.com/docs")
        .with_auth_scope(AuthScope::Authenticated)
        .with_metadata("domain", "finance")
        .with_example(example);

        assert_eq!(tool.tags.len(), 2);
        assert_eq!(tool.author, Some("Test Author".to_string()));
        assert_eq!(
            tool.documentation_url,
            Some("https://example.com/docs".to_string())
        );
        assert_eq!(tool.auth_scope, AuthScope::Authenticated);
        assert_eq!(tool.metadata.get("domain"), Some(&"finance".to_string()));
        assert_eq!(tool.examples.len(), 1);
        assert!(tool.validate().is_ok());
    }
}
