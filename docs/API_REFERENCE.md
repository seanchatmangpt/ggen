# API Reference

**Complete Method Documentation for Agent Integration APIs**

## Table of Contents

1. [Signature API](#signature-api)
2. [InputField & OutputField](#inputfield--outputfield)
3. [FieldConstraints API](#fieldconstraints-api)
4. [Tool Registry API](#tool-registry-api)
5. [SignatureValidator API](#signaturevalidator-api)
6. [Error Types](#error-types)
7. [Examples](#examples)

---

## Signature API

### Overview

The `Signature` struct represents the type-safe interface contract between agents and tools.

```rust
pub struct Signature {
    pub name: String,
    pub description: String,
    pub inputs: Vec<InputField>,
    pub outputs: Vec<OutputField>,
    pub instructions: Option<String>,
}
```

### Methods

#### `new(name, description)`

Creates a new empty signature.

**Parameters**:
- `name: impl Into<String>` - Signature name (should be valid Rust type name)
- `description: impl Into<String>` - Human-readable description

**Returns**: `Signature`

**Example**:
```rust
let sig = Signature::new(
    "FinancialAnalyzer",
    "Analyzes stock data and provides recommendations"
);
```

---

#### `with_input(input_field)`

Adds a single input field to the signature.

**Parameters**:
- `input_field: InputField` - Input field definition

**Returns**: `Signature` (self, for chaining)

**Example**:
```rust
let sig = Signature::new("Calculator", "Simple math")
    .with_input(InputField::new("operand_a", "f64"))
    .with_input(InputField::new("operand_b", "f64"));
```

---

#### `with_inputs(inputs)`

Adds multiple input fields at once.

**Parameters**:
- `inputs: impl IntoIterator<Item = InputField>` - Iterator of input fields

**Returns**: `Signature` (self, for chaining)

**Example**:
```rust
let inputs = vec![
    InputField::new("operand_a", "f64"),
    InputField::new("operand_b", "f64"),
];
let sig = Signature::new("Calculator", "Math")
    .with_inputs(inputs);
```

---

#### `with_output(output_field)`

Adds a single output field.

**Parameters**:
- `output_field: OutputField` - Output field definition

**Returns**: `Signature` (self, for chaining)

**Example**:
```rust
let sig = Signature::new("Calculator", "Math")
    .with_output(OutputField::new("result", "f64"));
```

---

#### `with_outputs(outputs)`

Adds multiple output fields at once.

**Parameters**:
- `outputs: impl IntoIterator<Item = OutputField>` - Iterator of output fields

**Returns**: `Signature` (self, for chaining)

---

#### `with_instructions(instructions)`

Sets custom LLM instructions for this signature.

**Parameters**:
- `instructions: impl Into<String>` - Instructions for LLM

**Returns**: `Signature` (self, for chaining)

**Example**:
```rust
let sig = Signature::new("Analyzer", "Analyze data")
    .with_instructions("Always validate input before processing. Return confidence scores.")
    .with_input(InputField::new("data", "String"));
```

---

#### `input_names()`

Gets all input field names.

**Returns**: `Vec<&str>` - Field names in order

**Example**:
```rust
let names = sig.input_names();
assert_eq!(names, vec!["ticker_symbol", "period_days"]);
```

---

#### `output_names()`

Gets all output field names.

**Returns**: `Vec<&str>` - Field names in order

---

#### `get_input(name)`

Looks up an input field by name.

**Parameters**:
- `name: &str` - Field name

**Returns**: `Option<&InputField>`

**Example**:
```rust
if let Some(field) = sig.get_input("ticker_symbol") {
    println!("Found field: {} ({})", field.name(), field.type_annotation());
}
```

---

#### `get_output(name)`

Looks up an output field by name.

**Parameters**:
- `name: &str` - Field name

**Returns**: `Option<&OutputField>`

---

#### `as_rust_struct()`

Generates Rust struct code representing the signature.

**Returns**: `String` - Valid Rust code

**Example**:
```rust
let code = sig.as_rust_struct();
println!("{}", code);
// Output:
// #[derive(Debug, Clone, Serialize, Deserialize)]
// pub struct FinancialAnalyzer_Inputs {
//     /// Stock ticker symbol
//     pub ticker_symbol: String,
//     /// Analysis period in days
//     pub period_days: i32,
// }
```

---

#### `schema()`

Generates JSON-compatible schema representation.

**Returns**: `SignatureSchema`

**Example**:
```rust
let schema = sig.schema();
let json = serde_json::to_string_pretty(&schema)?;
```

---

## InputField & OutputField

### InputField

Represents an input to a tool.

```rust
pub struct InputField {
    name: String,
    type_annotation: String,
    description: String,
    constraints: Vec<FieldConstraint>,
    metadata: FieldMetadata,
}

pub struct FieldMetadata {
    pub required: bool,
    pub deprecated: bool,
    pub prefix: Option<String>,
    pub suffix: Option<String>,
}
```

#### `InputField::new(name, type_annotation)`

Creates a new input field.

**Parameters**:
- `name: impl Into<String>` - Field name (must be valid Rust identifier)
- `type_annotation: impl Into<String>` - Rust type (e.g., "String", "i32", "f64")

**Returns**: `InputField`

**Example**:
```rust
let field = InputField::new("ticker_symbol", "String");
```

---

#### `with_description(description)`

Sets the description.

**Parameters**:
- `description: impl Into<String>` - Human-readable description

**Returns**: `InputField` (self, for chaining)

**Example**:
```rust
let field = InputField::new("ticker", "String")
    .with_description("Stock ticker symbol (e.g., AAPL)");
```

---

#### `with_constraint(constraint)`

Adds a validation constraint.

**Parameters**:
- `constraint: FieldConstraint` - Validation rule

**Returns**: `InputField` (self, for chaining)

**Example**:
```rust
let field = InputField::new("ticker", "String")
    .with_constraint(FieldConstraint::pattern("^[A-Z]{1,5}$"))
    .with_constraint(FieldConstraint::required());
```

---

#### `with_constraints(constraints)`

Adds multiple constraints at once.

**Parameters**:
- `constraints: Vec<FieldConstraint>` - Validation rules

**Returns**: `InputField` (self, for chaining)

---

#### `name()`

Gets the field name.

**Returns**: `&str`

---

#### `type_annotation()`

Gets the Rust type annotation.

**Returns**: `&str`

---

#### `desc()`

Gets the description.

**Returns**: `&str`

---

#### `is_required()`

Checks if field is required.

**Returns**: `bool`

---

#### `is_deprecated()`

Checks if field is deprecated.

**Returns**: `bool`

---

### OutputField

Represents an output from a tool.

```rust
pub struct OutputField {
    name: String,
    type_annotation: String,
    description: String,
    constraints: Vec<FieldConstraint>,
}
```

OutputField has the same methods as InputField (new, with_description, with_constraint, etc.)

---

## FieldConstraints API

### Constraint Types

```rust
pub enum FieldConstraint {
    Required,
    MinLength(usize),
    MaxLength(usize),
    Pattern(String),
    MinValue(f64),
    MaxValue(f64),
    Enum(Vec<String>),
    Custom(String),
}
```

### Creating Constraints

#### `FieldConstraint::required()`

Field must be present (not null/empty).

```rust
let constraint = FieldConstraint::required();
```

---

#### `FieldConstraint::min_length(length)`

String must be at least `length` characters.

**Parameters**:
- `length: usize` - Minimum length

```rust
let constraint = FieldConstraint::min_length(1);
```

---

#### `FieldConstraint::max_length(length)`

String must be at most `length` characters.

**Parameters**:
- `length: usize` - Maximum length

```rust
let constraint = FieldConstraint::max_length(5);
```

---

#### `FieldConstraint::pattern(regex)`

String must match regex pattern.

**Parameters**:
- `regex: impl Into<String>` - Regular expression (PCRE syntax)

```rust
// Ticker symbol: 1-5 uppercase letters
let constraint = FieldConstraint::pattern("^[A-Z]{1,5}$");
```

---

#### `FieldConstraint::min_value(value)`

Numeric value must be >= `value`.

**Parameters**:
- `value: f64` - Minimum value

```rust
let constraint = FieldConstraint::min_value(0.0);
```

---

#### `FieldConstraint::max_value(value)`

Numeric value must be <= `value`.

**Parameters**:
- `value: f64` - Maximum value

```rust
let constraint = FieldConstraint::max_value(365.0);
```

---

#### `FieldConstraint::enum_values(values)`

Value must be one of the provided options.

**Parameters**:
- `values: Vec<String>` - Allowed values

```rust
let constraint = FieldConstraint::enum_values(
    vec!["BUY".to_string(), "SELL".to_string(), "HOLD".to_string()]
);
```

---

#### `FieldConstraint::custom(name, validator)`

Custom validation rule.

**Parameters**:
- `name: String` - Constraint name
- `validator: Box<dyn Fn(&str) -> bool>` - Validation function

```rust
let constraint = FieldConstraint::custom(
    "NonZero".to_string(),
    Box::new(|value| value.parse::<f64>().map(|v| v != 0.0).unwrap_or(false))
);
```

---

## Tool Registry API

### Overview

```rust
pub struct ToolRegistry {
    tools: Arc<RwLock<HashMap<String, Arc<dyn Tool>>>>,
    domain_index: Arc<RwLock<HashMap<String, Vec<String>>>>,
    validation_cache: Arc<RwLock<HashMap<String, ValidationResult>>>,
}
```

### Methods

#### `new()`

Creates a new empty registry.

**Returns**: `ToolRegistry`

**Example**:
```rust
let registry = ToolRegistry::new();
```

---

#### `register(tool)`

Registers a tool in the registry.

**Parameters**:
- `tool: Arc<dyn Tool>` - Tool implementation

**Returns**: `Result<()>`

**Errors**:
- `GgenAiError::ToolAlreadyRegistered` - Tool name already exists
- `GgenAiError::InvalidToolSignature` - Signature missing or invalid

**Example**:
```rust
let tool = Arc::new(MyFinancialTool { ... });
registry.register(tool).await?;
```

---

#### `get(name)`

Retrieves a tool by exact name.

**Parameters**:
- `name: &str` - Tool name (case-sensitive)

**Returns**: `Option<Arc<dyn Tool>>`

**Example**:
```rust
if let Some(tool) = registry.get("FinancialAnalyzer").await {
    println!("Found tool: {}", tool.name());
} else {
    println!("Tool not found");
}
```

---

#### `list_all()`

Lists all registered tool names.

**Returns**: `Vec<String>` - Tool names

**Example**:
```rust
let tools = registry.list_all().await;
for name in tools {
    println!("- {}", name);
}
```

---

#### `find_by_domain(domain)`

Finds tools by domain/category.

**Parameters**:
- `domain: &str` - Domain name (e.g., "finance", "weather")

**Returns**: `Vec<Arc<dyn Tool>>`

**Example**:
```rust
let market_tools = registry.find_by_domain("finance").await;
for tool in market_tools {
    println!("Tool: {}", tool.name());
}
```

---

#### `invoke(tool_name, input)`

Invokes a tool with automatic validation.

**Parameters**:
- `tool_name: &str` - Name of tool to invoke
- `input: &InputData` - Tool input (must match signature)

**Returns**: `Result<OutputData>`

**Errors**:
- `GgenAiError::ToolNotFound` - Tool doesn't exist
- `GgenAiError::Validation(error)` - Input validation failed
- `GgenAiError::ToolExecution(error)` - Tool execution failed

**Example**:
```rust
let input = InputData::from_json(serde_json::json!({
    "ticker_symbol": "AAPL",
    "period_days": 90,
}))?;

match registry.invoke("FinancialAnalyzer", &input).await {
    Ok(output) => println!("Success: {:?}", output),
    Err(e) => eprintln!("Error: {}", e),
}
```

---

#### `unregister(name)`

Removes a tool from the registry.

**Parameters**:
- `name: &str` - Tool name

**Returns**: `bool` - True if tool existed and was removed

**Example**:
```rust
if registry.unregister("OldTool").await {
    println!("Tool unregistered");
}
```

---

#### `count()`

Gets the number of registered tools.

**Returns**: `usize`

**Example**:
```rust
let count = registry.count().await;
println!("Total tools: {}", count);
```

---

## SignatureValidator API

### Overview

```rust
pub struct SignatureValidator {
    signature: Signature,
}
```

### Methods

#### `new(signature)`

Creates a validator for a signature.

**Parameters**:
- `signature: Signature` - Signature to validate against

**Returns**: `SignatureValidator`

**Example**:
```rust
let sig = Signature::new("Calculator", "...")...;
let validator = SignatureValidator::new(sig);
```

---

#### `validate_input(input_data)`

Validates input against signature's input fields.

**Parameters**:
- `input_data: &InputData` - Input to validate

**Returns**: `Result<ValidationResult>`

**Example**:
```rust
match validator.validate_input(&input) {
    Ok(result) => {
        if result.is_valid() {
            println!("Input is valid");
        } else {
            println!("Validation issues: {:?}", result.issues);
        }
    }
    Err(e) => eprintln!("Validation error: {}", e),
}
```

---

#### `validate_output(output_data)`

Validates output against signature's output fields.

**Parameters**:
- `output_data: &OutputData` - Output to validate

**Returns**: `Result<ValidationResult>`

**Example**:
```rust
match validator.validate_output(&output) {
    Ok(result) => println!("Output valid: {}", result.is_valid()),
    Err(e) => eprintln!("Validation error: {}", e),
}
```

---

#### `explain_error(error)`

Gets human-readable explanation of validation error.

**Parameters**:
- `error: &ValidationError` - Error to explain

**Returns**: `String` - Explanation with suggestions

**Example**:
```rust
match validator.validate_input(&input) {
    Err(e) => {
        let explanation = validator.explain_error(&e);
        eprintln!("{}", explanation);
        // Output:
        // Field 'ticker_symbol' failed validation:
        // - Expected pattern: ^[A-Z]{1,5}$
        // - Got: 'invalid123'
        // Suggestion: Use uppercase letters only (1-5 chars)
    }
    _ => {}
}
```

---

## Error Types

### GgenAiError

```rust
#[derive(Debug, thiserror::Error)]
pub enum GgenAiError {
    #[error("Tool '{0}' not found in registry")]
    ToolNotFound(String),

    #[error("Tool '{0}' already registered")]
    ToolAlreadyRegistered(String),

    #[error("Invalid tool signature: {0}")]
    InvalidToolSignature(String),

    #[error("Validation error: {0}")]
    Validation(#[from] ValidationError),

    #[error("Tool '{tool}' execution failed: {reason}")]
    ToolExecution { tool: String, reason: String },

    #[error("Tool '{tool}' execution timeout after {timeout_ms}ms")]
    ToolTimeout { tool: String, timeout_ms: u64 },

    #[error("Missing required field: {0}")]
    MissingField(String),

    #[error("Type mismatch: {0}")]
    TypeMismatch(String),

    #[error("SPARQL query error: {0}")]
    SparqlError(String),

    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    #[error("Internal error: {0}")]
    Internal(String),
}

pub type Result<T> = std::result::Result<T, GgenAiError>;
```

### ValidationError

```rust
#[derive(Debug, thiserror::Error)]
pub enum ValidationError {
    #[error("Field '{field}' validation failed: {reason}")]
    FieldValidation { field: String, reason: String },

    #[error("Missing required field: {0}")]
    MissingField(String),

    #[error("Type mismatch for field '{field}': expected {expected}, got {actual}")]
    TypeMismatch { field: String, expected: String, actual: String },

    #[error("Pattern mismatch for field '{field}': expected {pattern}")]
    PatternMismatch { field: String, pattern: String },

    #[error("Value {value} out of range for field '{field}': [{min}, {max}]")]
    OutOfRange {
        field: String,
        value: String,
        min: String,
        max: String,
    },

    #[error("Enum constraint violation for field '{field}': {message}")]
    EnumViolation { field: String, message: String },

    #[error("Custom constraint '{constraint}' failed for field '{field}'")]
    CustomConstraint { field: String, constraint: String },
}
```

---

## Examples

### Complete Tool Registration Example

```rust
use ggen_ai::{
    dspy::{Signature, InputField, OutputField},
    tools::{ToolRegistry, Tool, InputData, OutputData},
    generators::validator::SignatureValidator,
};
use async_trait::async_trait;
use std::sync::Arc;

// 1. Define tool
#[derive(Debug)]
struct WeatherTool {
    name: String,
    signature: Signature,
}

#[async_trait]
impl Tool for WeatherTool {
    fn name(&self) -> &str { &self.name }
    fn signature(&self) -> &Signature { &self.signature }

    async fn execute(&self, input: InputData) -> Result<OutputData> {
        let city = input.get("city").ok_or("Missing city")?;
        let mut output = OutputData::new();
        output.insert("temperature".to_string(), serde_json::json!(72.5));
        output.insert("condition".to_string(), serde_json::json!("Sunny"));
        Ok(output)
    }
}

// 2. Create signature
let signature = Signature::new("WeatherTool", "Gets current weather")
    .with_input(
        InputField::new("city", "String")
            .with_description("City name")
            .with_constraint(FieldConstraint::required())
            .with_constraint(FieldConstraint::min_length(1))
    )
    .with_output(
        OutputField::new("temperature", "f64")
            .with_description("Temperature in Fahrenheit")
    )
    .with_output(
        OutputField::new("condition", "String")
            .with_description("Weather condition")
    );

// 3. Create and register tool
let registry = ToolRegistry::new();
let tool = Arc::new(WeatherTool {
    name: "WeatherTool".to_string(),
    signature,
});
registry.register(tool).await?;

// 4. Invoke tool
let input = InputData::from_json(serde_json::json!({
    "city": "San Francisco",
}))?;

match registry.invoke("WeatherTool", &input).await {
    Ok(output) => {
        println!("Temperature: {}", output.get("temperature").unwrap());
        println!("Condition: {}", output.get("condition").unwrap());
    }
    Err(e) => eprintln!("Error: {}", e),
}
```

---

## Related Documentation

- **Agent Integration Guide**: [AGENT_INTEGRATION.md](AGENT_INTEGRATION.md)
- **MCP Interface Spec**: [MCP_INTERFACE_SPEC.md](MCP_INTERFACE_SPEC.md)
- **Testing Guide**: [TESTING.md](TESTING.md)
- **Code Examples**: `/docs/examples/`
