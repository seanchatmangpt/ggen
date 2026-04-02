# Agent Tooling Integration - Implementation Roadmap

**Status**: READY FOR DEVELOPMENT
**Priority**: HIGH (Unblocks LLM tool calling)
**Estimated Duration**: 4-6 weeks
**Owner**: Architecture Team

---

## Phase 1: Signature Validator (Week 1-2)

### Objective
Enable runtime validation of JSON arguments against Signature constraints.

### 1.1 Add `jsonschema` Dependency

```toml
# In crates/ggen-ai/Cargo.toml
[dependencies]
jsonschema = { version = "0.18", features = ["blocking"] }
```

### 1.2 Create Validator Module

**File**: `/home/user/ggen/crates/ggen-ai/src/dspy/validator.rs`

```rust
//! Signature validator - validate JSON against Signature constraints

use crate::dspy::Signature;
use crate::error::{GgenAiError, Result};
use serde_json::{json, Value};
use jsonschema::JSONSchema;

/// Validation error with rich context
#[derive(Debug, Clone)]
pub struct SignatureValidationError {
    pub field: String,
    pub value: Value,
    pub expected_type: String,
    pub constraint: Option<String>,
    pub message: String,
}

/// Validates JSON input against Signature constraints
pub struct SignatureValidator;

impl SignatureValidator {
    /// Validate JSON against Signature
    pub fn validate(input: &Value, signature: &Signature) -> Result<()> {
        let schema = signature.as_json_schema();

        // Create JSON Schema validator
        let validator = JSONSchema::compile(&schema)
            .map_err(|e| GgenAiError::Validation(format!("Invalid schema: {}", e)))?;

        // Validate input
        validator.validate(input)
            .map_err(|e| {
                let errors: Vec<_> = e.collect();
                let msg = errors.iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join("; ");
                GgenAiError::Validation(msg)
            })?;

        Ok(())
    }

    /// Validate and provide detailed error context
    pub fn validate_detailed(input: &Value, signature: &Signature)
        -> std::result::Result<(), Vec<SignatureValidationError>>
    {
        let schema = signature.as_json_schema();
        let validator = JSONSchema::compile(&schema)
            .map_err(|_| vec![])?;

        let mut errors = Vec::new();
        for error in validator.iter_errors(input) {
            let path = error.path().join(".");
            errors.push(SignatureValidationError {
                field: path.clone(),
                value: error.instance.clone(),
                expected_type: error.schema.get("type")
                    .and_then(|v| v.as_str())
                    .unwrap_or("unknown")
                    .to_string(),
                constraint: error.keyword.map(|k| k.to_string()),
                message: error.to_string(),
            });
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Coerce value to expected type
    pub fn coerce_to_type(value: &Value, target_type: &str) -> Result<Value> {
        match target_type {
            "string" => {
                if value.is_string() {
                    Ok(value.clone())
                } else if value.is_number() || value.is_boolean() {
                    Ok(Value::String(value.to_string()))
                } else if value.is_null() {
                    Err(GgenAiError::Validation("Cannot coerce null to string".into()))
                } else {
                    Ok(Value::String(serde_json::to_string(value)?))
                }
            }
            "integer" => {
                if value.is_i64() {
                    Ok(value.clone())
                } else if let Some(s) = value.as_str() {
                    let i: i64 = s.parse()
                        .map_err(|_| GgenAiError::Validation(format!("Cannot parse '{}' as integer", s)))?;
                    Ok(Value::Number(i.into()))
                } else {
                    Err(GgenAiError::Validation("Cannot coerce to integer".into()))
                }
            }
            "number" => {
                if value.is_number() {
                    Ok(value.clone())
                } else if let Some(s) = value.as_str() {
                    let f: f64 = s.parse()
                        .map_err(|_| GgenAiError::Validation(format!("Cannot parse '{}' as number", s)))?;
                    Ok(json!(f))
                } else {
                    Err(GgenAiError::Validation("Cannot coerce to number".into()))
                }
            }
            "boolean" => {
                if value.is_boolean() {
                    Ok(value.clone())
                } else if let Some(s) = value.as_str() {
                    let b = match s.to_lowercase().as_str() {
                        "true" | "yes" | "1" => true,
                        "false" | "no" | "0" => false,
                        _ => return Err(GgenAiError::Validation(format!("Cannot parse '{}' as boolean", s))),
                    };
                    Ok(Value::Bool(b))
                } else {
                    Err(GgenAiError::Validation("Cannot coerce to boolean".into()))
                }
            }
            "array" => {
                if value.is_array() {
                    Ok(value.clone())
                } else {
                    Err(GgenAiError::Validation("Cannot coerce to array".into()))
                }
            }
            "object" => {
                if value.is_object() {
                    Ok(value.clone())
                } else {
                    Err(GgenAiError::Validation("Cannot coerce to object".into()))
                }
            }
            _ => Ok(value.clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dspy::{InputField, OutputField, FieldConstraints};

    #[test]
    fn test_validate_valid_input() {
        let sig = Signature::new("Test", "Test signature")
            .with_input(InputField::new("name", "Name field", "String"));

        let input = json!({"name": "Alice"});
        assert!(SignatureValidator::validate(&input, &sig).is_ok());
    }

    #[test]
    fn test_validate_missing_required_field() {
        let mut field = InputField::new("name", "Name field", "String");
        field.constraints = FieldConstraints::new().required(true);

        let sig = Signature::new("Test", "Test").with_input(field);
        let input = json!({});

        assert!(SignatureValidator::validate(&input, &sig).is_err());
    }

    #[test]
    fn test_coerce_string_from_number() {
        let result = SignatureValidator::coerce_to_type(&json!(42), "string");
        assert_eq!(result.unwrap(), "42");
    }

    #[test]
    fn test_coerce_boolean_from_string() {
        let result = SignatureValidator::coerce_to_type(&json!("true"), "boolean");
        assert_eq!(result.unwrap(), true);
    }
}
```

### 1.3 Update dspy/mod.rs

```rust
// Add to dspy/mod.rs
pub mod validator;
pub use validator::{SignatureValidator, SignatureValidationError};
```

### 1.4 Update dspy/signature.rs

```rust
impl Signature {
    /// Validate JSON input against this signature
    pub fn validate(&self, input: &serde_json::Value) -> Result<()> {
        crate::dspy::SignatureValidator::validate(input, self)
    }

    /// Validate with detailed error information
    pub fn validate_detailed(&self, input: &serde_json::Value)
        -> std::result::Result<(), Vec<crate::dspy::SignatureValidationError>>
    {
        crate::dspy::SignatureValidator::validate_detailed(input, self)
    }

    /// Coerce input to match signature types
    pub fn coerce_input(&self, input: serde_json::Value) -> Result<serde_json::Value> {
        let mut result = serde_json::Map::new();

        if let Some(obj) = input.as_object() {
            for field in &self.inputs {
                if let Some(value) = obj.get(field.name()) {
                    let coerced = crate::dspy::SignatureValidator::coerce_to_type(
                        value,
                        field.type_annotation()
                    )?;
                    result.insert(field.name().to_string(), coerced);
                }
            }
        }

        Ok(serde_json::Value::Object(result))
    }
}
```

### 1.5 Tests

**File**: `/home/user/ggen/crates/ggen-ai/tests/signature_validator.rs`

```rust
//! Chicago TDD tests for Signature validation

use ggen_ai::dspy::{Signature, InputField, OutputField, FieldConstraints, SignatureValidator};
use serde_json::json;

#[test]
fn test_validate_simple_string_field() {
    // Arrange
    let sig = Signature::new("StringTest", "Test")
        .with_input(InputField::new("name", "Name", "String"));

    // Act
    let result = sig.validate(&json!({"name": "Alice"}));

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_validate_required_field_missing() {
    // Arrange
    let mut field = InputField::new("email", "Email", "String");
    field.constraints = FieldConstraints::new().required(true);
    let sig = Signature::new("EmailTest", "Test").with_input(field);

    // Act
    let result = sig.validate(&json!({}));

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_validate_enum_constraint() {
    // Arrange
    let mut field = InputField::new("domain", "Domain", "String");
    field.constraints = FieldConstraints::new()
        .enum_values(vec!["finops".into(), "banking".into()]);
    let sig = Signature::new("DomainTest", "Test").with_input(field);

    // Act - valid value
    assert!(sig.validate(&json!({"domain": "finops"})).is_ok());

    // Act - invalid value
    assert!(sig.validate(&json!({"domain": "invalid"})).is_err());
}

#[test]
fn test_coerce_string_from_number() {
    // Arrange
    let coerced = SignatureValidator::coerce_to_type(&json!(42), "string");

    // Assert
    assert!(coerced.is_ok());
    assert_eq!(coerced.unwrap(), json!("42"));
}

#[test]
fn test_validate_detailed_provides_field_names() {
    // Arrange
    let mut field = InputField::new("count", "Count", "i32");
    field.constraints = FieldConstraints::new().required(true);
    let sig = Signature::new("DetailedTest", "Test").with_input(field);

    // Act
    let result = sig.validate_detailed(&json!({}));

    // Assert
    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert!(!errors.is_empty());
}
```

---

## Phase 2: Agent-Signature Binding (Week 2-3)

### Objective
Enable agents to reference Signatures for tool definition.

### 2.1 Update Agent Traits

**File**: `/home/user/ggen/crates/ggen-ai/src/agents/mod.rs`

```rust
use crate::dspy::Signature;
use std::sync::Arc;

/// Enhanced agent configuration with signature binding
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentConfig {
    pub id: Uuid,
    pub name: String,
    pub role: AgentRole,
    pub capabilities: Vec<String>,
    pub max_concurrent_tasks: usize,
    // NEW
    pub signature: Option<Arc<Signature>>,
}

/// Unified agent trait - with signature support
#[async_trait]
pub trait Agent: Send + Sync + std::fmt::Debug {
    /// Initialize agent
    async fn initialize(&mut self)
        -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>>;

    /// Start agent execution
    async fn start(&mut self)
        -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>>;

    /// Stop agent execution
    async fn stop(&mut self)
        -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>>;

    /// Get agent status
    async fn status(&self) -> AgentStatus;

    /// Get agent configuration
    fn config(&self) -> &AgentConfig;

    // NEW: Get agent signature for tool registration
    fn signature(&self) -> Option<&Signature> {
        self.config().signature.as_ref().map(|s| s.as_ref())
    }

    // NEW: Validate input against signature
    async fn validate_input(&self, message: &AgentMessage)
        -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>>
    {
        if let Some(sig) = self.signature() {
            sig.validate(&message.data)
                .map_err(|e| Box::new(e) as Box<dyn std::error::Error + Send + Sync>)?;
        }
        Ok(())
    }

    /// Handle incoming messages
    async fn handle_message(&mut self, message: AgentMessage)
        -> std::result::Result<AgentMessage, Box<dyn std::error::Error + Send + Sync>>;
}

// New message type with validation support
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentMessage {
    pub id: Uuid,
    pub agent_id: Uuid,
    pub message_type: String,
    pub data: serde_json::Value,
    pub signature: Option<String>,  // Reference to Signature name
    pub validated: bool,  // Whether input was validated
}

impl AgentMessage {
    pub fn new(agent_id: Uuid, message_type: String, data: serde_json::Value) -> Self {
        Self {
            id: Uuid::new_v4(),
            agent_id,
            message_type,
            data,
            signature: None,
            validated: false,
        }
    }

    pub fn with_signature(mut self, sig_name: String) -> Self {
        self.signature = Some(sig_name);
        self
    }
}
```

### 2.2 Implement Tool Registration Trait

**File**: `/home/user/ggen/crates/ggen-ai/src/agents/tool_registry.rs`

```rust
//! Tool registration for agents

use crate::dspy::Signature;
use crate::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Tool definition for LLM integration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolDefinition {
    /// Tool ID (usually agent name)
    pub id: String,

    /// Tool name (human-readable)
    pub name: String,

    /// Tool description
    pub description: String,

    /// Input JSON Schema
    pub input_schema: serde_json::Value,

    /// Output JSON Schema
    pub output_schema: serde_json::Value,

    /// Signature this tool implements
    pub signature: Option<Signature>,
}

impl ToolDefinition {
    /// Create from Signature
    pub fn from_signature(sig: &Signature) -> Self {
        Self {
            id: sig.name.clone(),
            name: sig.name.clone(),
            description: sig.description.clone(),
            input_schema: sig.as_json_schema(),
            output_schema: {
                let mut output_sig = Signature::new(
                    format!("{}Output", sig.name),
                    format!("Output of {}", sig.name)
                );
                for output in &sig.outputs {
                    output_sig = output_sig.with_output(output.clone());
                }
                output_sig.as_json_schema()
            },
            signature: Some(sig.clone()),
        }
    }
}

/// Registry for agent tools
pub struct ToolRegistry {
    tools: HashMap<String, ToolDefinition>,
}

impl ToolRegistry {
    /// Create new tool registry
    pub fn new() -> Self {
        Self {
            tools: HashMap::new(),
        }
    }

    /// Register a tool from Signature
    pub fn register_from_signature(&mut self, sig: &Signature) -> Result<()> {
        let tool = ToolDefinition::from_signature(sig);
        self.tools.insert(tool.id.clone(), tool);
        Ok(())
    }

    /// Register a tool definition
    pub fn register(&mut self, tool: ToolDefinition) -> Result<()> {
        self.tools.insert(tool.id.clone(), tool);
        Ok(())
    }

    /// Get tool by ID
    pub fn get(&self, id: &str) -> Option<&ToolDefinition> {
        self.tools.get(id)
    }

    /// List all tools
    pub fn list(&self) -> Vec<&ToolDefinition> {
        self.tools.values().collect()
    }

    /// Get tools as JSON (for genai/MCP)
    pub fn to_json(&self) -> serde_json::Value {
        serde_json::json!({
            "tools": self.tools.values()
                .map(|t| serde_json::json!({
                    "name": t.name,
                    "description": t.description,
                    "input_schema": t.input_schema,
                }))
                .collect::<Vec<_>>()
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dspy::{InputField, OutputField};

    #[test]
    fn test_register_from_signature() {
        let sig = Signature::new("TestTool", "A test tool")
            .with_input(InputField::new("param", "A parameter", "String"));

        let mut registry = ToolRegistry::new();
        assert!(registry.register_from_signature(&sig).is_ok());
        assert!(registry.get("TestTool").is_some());
    }

    #[test]
    fn test_tool_definition_has_schema() {
        let sig = Signature::new("SchemaTool", "Tool with schema")
            .with_input(InputField::new("input", "Input", "String"));

        let tool = ToolDefinition::from_signature(&sig);
        assert!(tool.input_schema.get("properties").is_some());
    }
}
```

### 2.3 Update lib.rs Exports

```rust
// In src/lib.rs
pub use agents::{
    Agent, AgentConfig, AgentMessage, AgentStatus,
    AgentRole, AgentResult, AgentInput, AgentOutput,
};
pub use agents::tool_registry::{ToolDefinition, ToolRegistry};
pub use dspy::SignatureValidator;
```

---

## Phase 3: Validation Middleware (Week 3-4)

### Objective
Intercept and validate agent inputs before execution.

### 3.1 Create Middleware

**File**: `/home/user/ggen/crates/ggen-ai/src/agents/middleware.rs`

```rust
//! Agent input validation middleware

use crate::dspy::Signature;
use crate::agents::AgentMessage;
use crate::error::Result;
use async_trait::async_trait;

/// Middleware for agent message processing
#[async_trait]
pub trait AgentMiddleware: Send + Sync {
    /// Process message before agent receives it
    async fn process_input(&self, message: &mut AgentMessage) -> Result<()>;

    /// Process message after agent handles it
    async fn process_output(&self, message: &mut AgentMessage) -> Result<()>;
}

/// Validation middleware
pub struct ValidationMiddleware {
    signatures: std::collections::HashMap<String, Signature>,
}

impl ValidationMiddleware {
    pub fn new() -> Self {
        Self {
            signatures: std::collections::HashMap::new(),
        }
    }

    /// Register a signature
    pub fn register(&mut self, sig: Signature) {
        self.signatures.insert(sig.name.clone(), sig);
    }
}

#[async_trait]
impl AgentMiddleware for ValidationMiddleware {
    async fn process_input(&self, message: &mut AgentMessage) -> Result<()> {
        if let Some(sig_name) = &message.signature {
            if let Some(sig) = self.signatures.get(sig_name) {
                sig.validate(&message.data)?;
                message.validated = true;
            }
        }
        Ok(())
    }

    async fn process_output(&self, _message: &mut AgentMessage) -> Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dspy::{InputField, FieldConstraints};
    use serde_json::json;
    use uuid::Uuid;

    #[tokio::test]
    async fn test_validation_middleware_validates_input() {
        // Arrange
        let mut sig = Signature::new("TestSig", "Test");
        let mut field = InputField::new("required_field", "Required", "String");
        field.constraints = FieldConstraints::new().required(true);
        sig = sig.with_input(field);

        let mut middleware = ValidationMiddleware::new();
        middleware.register(sig);

        let mut message = AgentMessage::new(
            Uuid::new_v4(),
            "test".into(),
            json!({})
        ).with_signature("TestSig".into());

        // Act & Assert
        assert!(middleware.process_input(&mut message).await.is_err());
    }
}
```

---

## Phase 4: MCP Integration Stub (Week 4-5)

### Objective
Prepare for MCP server integration.

### 4.1 Create MCP Types

**File**: `/home/user/ggen/crates/ggen-ai/src/mcp/mod.rs`

```rust
//! Model Context Protocol (MCP) integration stub

use crate::dspy::Signature;
use crate::agents::ToolDefinition;
use serde::{Deserialize, Serialize};

/// MCP Tool Call request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpToolCall {
    pub tool_name: String,
    pub arguments: serde_json::Value,
}

/// MCP Tool Call result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpToolResult {
    pub tool_name: String,
    pub success: bool,
    pub result: Option<serde_json::Value>,
    pub error: Option<String>,
}

/// MCP Server interface (stub for future implementation)
pub trait McpServer: Send + Sync {
    fn register_tool(&mut self, tool: ToolDefinition);
    fn call_tool(&self, call: McpToolCall) -> crate::Result<McpToolResult>;
}

/// Convert Signature to MCP tool
pub fn signature_to_mcp_tool(sig: &Signature) -> ToolDefinition {
    ToolDefinition::from_signature(sig)
}
```

---

## Implementation Checklist

### Week 1-2: Signature Validator ✅
- [ ] Add `jsonschema` dependency
- [ ] Create `dspy/validator.rs` with validation logic
- [ ] Implement coercion functions
- [ ] Add detailed error handling
- [ ] Write Chicago TDD tests (12+ tests)
- [ ] Update `dspy/signature.rs` with validate/coerce methods
- [ ] Document validator usage

### Week 2-3: Agent-Signature Binding ✅
- [ ] Update `AgentConfig` with signature field
- [ ] Add `signature()` method to Agent trait
- [ ] Add `validate_input()` to Agent trait
- [ ] Create new `AgentMessage` type
- [ ] Create `agents/tool_registry.rs`
- [ ] Implement `ToolRegistry` and `ToolDefinition`
- [ ] Write integration tests

### Week 3-4: Validation Middleware ✅
- [ ] Create `agents/middleware.rs`
- [ ] Implement `AgentMiddleware` trait
- [ ] Create `ValidationMiddleware`
- [ ] Integration tests with real agents
- [ ] Document middleware usage

### Week 4-5: MCP Stub ✅
- [ ] Create `mcp/mod.rs`
- [ ] Define MCP types
- [ ] Create `McpServer` trait
- [ ] Add `signature_to_mcp_tool()` function
- [ ] Placeholder for future MCP server

### Week 5-6: Integration & Testing ✅
- [ ] End-to-end tests (Signature → Validation → Tool Call)
- [ ] Performance testing
- [ ] Documentation
- [ ] Enable agent modules in lib.rs

---

## Future Work

### Post-Phase 1: MCP Server Implementation
```rust
// Future: Complete MCP server
pub struct McpServerImpl {
    tools: ToolRegistry,
    middleware: ValidationMiddleware,
    agents: HashMap<String, Box<dyn Agent>>,
}

impl McpServer for McpServerImpl {
    fn call_tool(&self, call: McpToolCall) -> Result<McpToolResult> {
        // 1. Find tool
        let tool = self.tools.get(&call.tool_name)?;

        // 2. Validate input
        if let Some(sig) = &tool.signature {
            sig.validate(&call.arguments)?;
        }

        // 3. Call agent
        // 4. Return result
    }
}
```

### Post-Phase 1: Genai Integration
```rust
// Future: Use genai's tool support
use genai::chat::ToolCall;

pub fn signature_to_genai_tool(sig: &Signature) -> ToolCall {
    ToolCall {
        name: sig.name.clone(),
        description: sig.description.clone(),
        parameters: sig.as_json_schema(),
    }
}
```

---

## Testing Strategy

### Unit Tests
- Validator: Type mappings, constraints, coercion
- Registry: Register, lookup, listing
- Middleware: Input validation, error handling

### Integration Tests
- End-to-end: Signature → JSON Schema → Validation → Tool Call
- Real agents: CodeGenAgent, TemplateGeneratorAgent
- Chicago TDD: Real objects, no mocks

### Performance Tests
- Validation speed (< 100ms for typical schemas)
- Registry lookup (O(1) average)
- Schema generation caching

---

## Success Criteria

1. ✅ All validators pass tests
2. ✅ Agents can reference Signatures
3. ✅ Tool registry enables MCP integration
4. ✅ Validation middleware intercepts inputs
5. ✅ Zero breaking changes to existing code
6. ✅ Documentation complete
7. ✅ Performance benchmarks pass

---

**Next Action**: Assign Phase 1 developer, create feature branch, begin Week 1 implementation.
