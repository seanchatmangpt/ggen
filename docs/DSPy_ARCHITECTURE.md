# DSPy Architecture: Rust Implementation

**Declarative Self-improving Prompting in Rust**

> Type-safe LLM prompting through RDF ontologies, compile-time guarantees, and zero-cost abstractions.

## Table of Contents

1. [Core Concepts](#core-concepts)
2. [Architecture Overview](#architecture-overview)
3. [Core Components](#core-components)
4. [RDF/SHACL Integration](#rdfshacl-integration)
5. [Type-Safe LLM Workflow](#type-safe-llm-workflow)
6. [Error Handling](#error-handling)
7. [Differences from Python DSPy](#differences-from-python-dspy)
8. [Integration with ggen](#integration-with-ggen)
9. [Common Patterns](#common-patterns)
10. [Testing & Examples](#testing--examples)

---

## Core Concepts

### What is DSPy?

DSPy (Declarative Self-improving Prompting) is a framework for building LLM applications through composable, type-safe signatures. Instead of manually crafting prompts, you define **what** the task is (inputs/outputs), and DSPy handles **how** to prompt the LLM.

### Rust DSPy Philosophy

```
RDF Ontology (Truth) → SHACL Constraints → Type-Safe Signature → LLM Prompt
     ↓                      ↓                      ↓                   ↓
  Source of Truth     Validation Rules      Compile-Time Safe    Runtime Safe
```

**Key Principles:**

- **Type-First Design**: Express constraints in types, not runtime checks
- **RDF as Truth**: Ontologies define task structure, code is generated
- **Zero Unwrap**: All operations return `Result<T, E>`
- **Compile-Time Guarantees**: Rust's type system prevents invalid signatures
- **Deterministic**: Same RDF → Same signature → Same prompt

---

## Architecture Overview

### High-Level Component Relationships

```
┌─────────────────────────────────────────────────────────────────────┐
│                         ggen Ecosystem                              │
│                                                                     │
│  ┌──────────────┐      ┌──────────────┐      ┌──────────────┐    │
│  │ RDF Store    │      │ TTL Files    │      │ SHACL Shapes │    │
│  │ (Oxigraph)   │◄─────┤ (.specify/)  │◄─────┤ (sh:NodeShape)│   │
│  └──────┬───────┘      └──────────────┘      └──────────────┘    │
│         │                                                          │
│         │ SPARQL Queries                                          │
│         ▼                                                          │
│  ┌──────────────────────────────────────────────┐                │
│  │   TTLToSignatureTranspiler                   │                │
│  │   - Find classes with sh:targetClass         │                │
│  │   - Extract property shapes                  │                │
│  │   - Classify input vs output fields          │                │
│  │   - Map XSD types → Rust types               │                │
│  │   - Handle naming collisions                 │                │
│  └──────────────┬───────────────────────────────┘                │
│                 │                                                  │
│                 │ build_signatures()                              │
│                 ▼                                                  │
│  ┌────────────────────────────────────────────────────────────┐  │
│  │                    Signature                                │  │
│  │  - name: String                                            │  │
│  │  - description: String                                     │  │
│  │  - inputs: Vec<InputField>                                │  │
│  │  - outputs: Vec<OutputField>                              │  │
│  │  - instructions: Option<String>                           │  │
│  └──────────────┬─────────────────────────────────────────────┘  │
│                 │                                                  │
│                 │ used by                                         │
│                 ▼                                                  │
│  ┌────────────────────────────────────────────────────────────┐  │
│  │                    Module Trait                             │  │
│  │  - signature() -> &Signature                               │  │
│  │  - forward(inputs) -> Result<outputs>                      │  │
│  │  - validate_inputs(inputs) -> Result<()>                   │  │
│  └──────────────┬─────────────────────────────────────────────┘  │
│                 │                                                  │
│                 │ implemented by                                  │
│                 ▼                                                  │
│  ┌────────────────────────────────────────────────────────────┐  │
│  │                    Predictor                                │  │
│  │  - Build prompt from Signature + inputs                    │  │
│  │  - Call LLM via genai Client                               │  │
│  │  - Parse structured output                                 │  │
│  │  - Validate outputs (optional)                             │  │
│  └──────────────┬─────────────────────────────────────────────┘  │
│                 │                                                  │
│                 │ calls                                           │
│                 ▼                                                  │
│  ┌────────────────────────────────────────────────────────────┐  │
│  │                LLM Backend (genai)                          │  │
│  │  - OpenAI, Anthropic, Cohere, etc.                         │  │
│  │  - Model routing via ggen.toml                             │  │
│  └────────────────────────────────────────────────────────────┘  │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

### Data Flow: RDF to LLM Response

```
1. RDF Ontology (.ttl)
   ↓
2. SHACL Shape Discovery (sh:targetClass)
   ↓
3. Property Extraction (sh:property, sh:path)
   ↓
4. Field Classification (cns:outputField)
   ↓
5. Type Mapping (xsd:string → String)
   ↓
6. Signature Assembly
   ↓
7. Module/Predictor Creation
   ↓
8. Prompt Construction
   ↓
9. LLM Call (genai::Client)
   ↓
10. Output Parsing
    ↓
11. Validation (optional)
    ↓
12. Result<HashMap<String, Value>>
```

---

## Core Components

### 1. Signature

**Purpose**: Type-safe specification of a module's interface (inputs → outputs).

**Definition**:
```rust
pub struct Signature {
    pub name: String,                    // Module name (valid Rust identifier)
    pub description: String,             // Human-readable task description
    pub inputs: Vec<InputField>,         // User-provided inputs
    pub outputs: Vec<OutputField>,       // Model-generated outputs
    pub instructions: Option<String>,    // Optional LLM instructions
}
```

**Key Methods**:
- `new(name, description)` - Create signature
- `with_input(field)` / `with_output(field)` - Add fields
- `with_instructions(text)` - Add LLM guidance
- `validator()` - Create runtime validator
- `as_json_schema()` - Generate JSON Schema for validation
- `as_rust_struct()` - Generate Rust code

**Example**:
```rust
use ggen_ai::dspy::{Signature, InputField, OutputField};

let sig = Signature::new("QuestionAnswering", "Answer questions based on context")
    .with_input(InputField::new("question", "User's question", "String"))
    .with_input(InputField::new("context", "Background context", "String"))
    .with_output(OutputField::new("answer", "Generated answer", "String"))
    .with_instructions("Provide concise, factual answers based only on the given context.");
```

**Philosophy**: Signature is the **contract** between user and LLM. It defines:
- What inputs are required
- What outputs are expected
- How to validate both
- How to construct prompts

---

### 2. Field Types

#### InputField

**Purpose**: Represents user-provided data with optional constraints.

```rust
pub struct InputField {
    pub metadata: FieldMetadata,        // Name, description, type
    pub constraints: FieldConstraints,  // Validation rules
}
```

**Builder Methods**:
```rust
InputField::new("email", "User email", "String")
    .required(true)
    .with_min_length(5)
    .with_max_length(254)
    .with_pattern(r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$")
    .with_enum_values(vec!["admin@example.com", "user@example.com"])
```

#### OutputField

**Purpose**: Represents LLM-generated data with validation rules.

```rust
pub struct OutputField {
    pub metadata: FieldMetadata,
    pub constraints: FieldConstraints,
}
```

**Same builder API as InputField**.

#### FieldConstraints

**Purpose**: Validation rules that apply to both inputs and outputs.

```rust
pub struct FieldConstraints {
    pub required: bool,                  // Non-null/empty
    pub min_items: Option<usize>,        // Array min length
    pub max_items: Option<usize>,        // Array max length
    pub min_length: Option<usize>,       // String min length
    pub max_length: Option<usize>,       // String max length
    pub pattern: Option<String>,         // Regex pattern
    pub enum_values: Option<Vec<String>>, // Allowed values
    pub semantic_type: Option<String>,   // RDF type (e.g., "fibo:Product")
    pub datatype: Option<String>,        // XSD datatype
}
```

**Validation**:
```rust
let constraints = FieldConstraints::new()
    .required(true)
    .min_length(10)
    .max_length(500)
    .pattern(r"^[A-Z]");

// Validate JSON value
let value = json!("This is a valid string starting with uppercase");
constraints.is_satisfied(&value)?; // Ok(())
```

---

### 3. Module Trait

**Purpose**: Composable unit of computation with signature-based interface.

```rust
#[async_trait::async_trait]
pub trait Module: Send + Sync {
    fn signature(&self) -> &Signature;
    async fn forward(&self, inputs: HashMap<String, Value>) -> Result<HashMap<String, Value>>;
    fn validate_inputs(&self, inputs: &HashMap<String, Value>) -> Result<()>;
}
```

**Analogy to Python DSPy**:
- Python: `class MyModule(dspy.Module)`
- Rust: `impl Module for MyModule`

**Key Properties**:
- **Composable**: Modules can call other modules
- **Async**: Uses `async_trait` for async LLM calls
- **Type-safe**: Signature defines expected inputs/outputs
- **Testable**: Forward method can be mocked/tested independently

---

### 4. Predictor

**Purpose**: Concrete Module implementation that uses an LLM backend.

```rust
pub struct Predictor {
    signature: Signature,
    model: String,        // e.g., "gpt-4o", "claude-sonnet-4"
    temperature: f32,     // 0.0-2.0, controls randomness
}
```

**Workflow**:
1. **Validate inputs** against signature
2. **Build prompt** from signature + inputs
3. **Call LLM** via genai Client
4. **Parse output** from LLM response
5. **Return** structured HashMap

**Example**:
```rust
use ggen_ai::dspy::{Signature, InputField, OutputField, Predictor};
use std::collections::HashMap;
use serde_json::json;

let sig = Signature::new("Summarize", "Summarize text")
    .with_input(InputField::new("text", "Text to summarize", "String"))
    .with_output(OutputField::new("summary", "Summary", "String"));

let predictor = Predictor::with_model(sig, "gpt-4o")
    .with_temperature(0.7);

let mut inputs = HashMap::new();
inputs.insert("text".to_string(), json!("Long text here..."));

let outputs = predictor.forward(inputs).await?;
let summary = outputs.get("summary").unwrap();
```

**Prompt Construction** (internal):
```
Summarize text

Input:
text: "Long text here..."

Output:
summary:
```

---

### 5. ChainOfThought

**Purpose**: Predictor variant that adds reasoning instructions.

```rust
pub struct ChainOfThought {
    predictor: Predictor,
}
```

**Automatically adds**:
```
Instructions: Think through this step-by-step before providing your answer.
```

**Example**:
```rust
let cot = ChainOfThought::new(sig);
// Automatically includes step-by-step reasoning prompt
```

---

### 6. SignatureValidator

**Purpose**: Runtime validation of JSON inputs against signature constraints.

```rust
pub struct SignatureValidator {
    signature: Signature,
}
```

**Usage**:
```rust
let validator = sig.validator();

let input = json!({
    "question": "What is Rust?",
    "context": "Rust is a systems programming language"
});

validator.validate(&input)?; // Ok(()) if valid

// Get detailed error info
let result = validator.validate_to_json(&input);
// { "valid": true, "errors": null }
```

**Error Types** (comprehensive):
- `FieldMissing` - Required field not present
- `NullValue` - Required field is null
- `TypeMismatch` - Wrong JSON type (string vs number)
- `StringTooShort` / `StringTooLong` - Length constraints
- `TooFewItems` / `TooManyItems` - Array size constraints
- `PatternMismatch` - Regex validation failed
- `EnumConstraintViolation` - Value not in allowed list
- `InvalidPattern` - Malformed regex

---

## RDF/SHACL Integration

### TTLToSignatureTranspiler

**Purpose**: Convert RDF ontologies with SHACL shapes into type-safe DSPy signatures.

**Core Equation**:
```
TTL (SHACL) + Transpiler → Signature (Rust) → Module → LLM
```

### Pipeline Stages

#### Stage 1: Class Discovery

Find all classes with `sh:targetClass` declarations:

```sparql
SELECT DISTINCT ?targetClass
WHERE {
    ?shape sh:targetClass ?targetClass .
}
```

#### Stage 2: Property Extraction

Extract property shapes (supports 2 patterns):

**Pattern 1: Direct Property Shapes**
```turtle
:NameProperty
    sh:targetClass :Person ;
    sh:path :name ;
    sh:datatype xsd:string .
```

**Pattern 2: Node Shapes with Properties**
```turtle
:PersonShape
    sh:targetClass :Person ;
    sh:property :NameProperty .

:NameProperty
    sh:path :name ;
    sh:datatype xsd:string .
```

#### Stage 3: Field Classification

Determine input vs output fields:

1. **Explicit marker**: `cns:outputField "true"`
2. **Description heuristic**: Contains "output" (case-insensitive)
3. **Default**: All unmarked fields are inputs

```turtle
:ResultProperty
    sh:path :result ;
    cns:outputField "true" ;        # ← Explicit output
    rdfs:comment "Generated result (output)" .  # ← Also matches heuristic
```

#### Stage 4: Type Mapping

XSD datatypes → Rust types:

| XSD Type         | Rust Type |
|------------------|-----------|
| xsd:string       | String    |
| xsd:integer      | i32       |
| xsd:int          | i32       |
| xsd:long         | i32       |
| xsd:boolean      | bool      |
| xsd:float        | f32       |
| xsd:double       | f32       |
| xsd:decimal      | f32       |
| (unknown)        | String    |

#### Stage 5: Name Normalization

**Safe Identifiers**:
1. Extract local name: `http://ex.com#Class` → `Class`
2. Convert to snake_case: `MyProperty` → `my_property`
3. Replace hyphens: `my-property` → `my_property`
4. Avoid reserved words: `class` → `custom_class`
5. Handle numeric prefixes: `1stField` → `field_1st_field`
6. Detect collisions: `field`, `field_1`, `field_2`

### Complete Example

**Input TTL**:
```turtle
@prefix : <http://example.com/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix cns: <http://cns.io/ontology#> .

:EmailValidatorShape
    a sh:NodeShape ;
    sh:targetClass :EmailValidator ;
    sh:property :EmailInputProperty ;
    sh:property :IsValidProperty .

:EmailInputProperty
    sh:path :email ;
    sh:datatype xsd:string ;
    sh:minLength 5 ;
    sh:maxLength 254 ;
    sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
    rdfs:comment "Email address to validate" .

:IsValidProperty
    sh:path :isValid ;
    sh:datatype xsd:boolean ;
    cns:outputField "true" ;
    rdfs:comment "Whether email is valid" .
```

**Generated Signature**:
```rust
Signature::new("EmailValidatorSignature", "DSPy Signature for EmailValidator")
    .with_input(
        InputField::new("email", "Email address to validate", "String")
            .with_min_length(5)
            .with_max_length(254)
            .with_pattern(r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$")
    )
    .with_output(
        OutputField::new("is_valid", "Whether email is valid", "bool")
    )
```

**Usage**:
```rust
use ggen_ai::codegen::TTLToSignatureTranspiler;
use oxigraph::store::Store;
use std::fs::File;

let mut store = Store::new()?;
let file = File::open("ontology.ttl")?;
store.load_from_reader(oxigraph::io::RdfFormat::Turtle, file)?;

let mut transpiler = TTLToSignatureTranspiler::new();
let signatures = transpiler.build_signatures(&store)?;

for sig in signatures {
    println!("Generated: {} with {} inputs, {} outputs",
        sig.name, sig.inputs.len(), sig.outputs.len());
}
```

---

## Type-Safe LLM Workflow

### End-to-End Example

**1. Define Ontology** (.specify/summarization.ttl):
```turtle
@prefix : <http://ggen.io/tasks/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix cns: <http://cns.io/ontology#> .

:SummarizationShape
    a sh:NodeShape ;
    sh:targetClass :Summarization ;
    sh:property :TextProperty ;
    sh:property :MaxLengthProperty ;
    sh:property :SummaryProperty .

:TextProperty
    sh:path :text ;
    sh:datatype xsd:string ;
    sh:minLength 10 ;
    rdfs:comment "Text to summarize" .

:MaxLengthProperty
    sh:path :maxLength ;
    sh:datatype xsd:integer ;
    rdfs:comment "Maximum summary length" .

:SummaryProperty
    sh:path :summary ;
    sh:datatype xsd:string ;
    cns:outputField "true" ;
    sh:maxLength 500 ;
    rdfs:comment "Generated summary" .
```

**2. Load & Transpile**:
```rust
use ggen_ai::codegen::TTLToSignatureTranspiler;
use ggen_ai::dspy::Predictor;
use oxigraph::store::Store;
use std::fs::File;

let mut store = Store::new()?;
let file = File::open(".specify/summarization.ttl")?;
store.load_from_reader(oxigraph::io::RdfFormat::Turtle, file)?;

let mut transpiler = TTLToSignatureTranspiler::new();
let signatures = transpiler.build_signatures(&store)?;
let sig = signatures.into_iter().next()
    .ok_or("No signature found")?;
```

**3. Create Predictor**:
```rust
let predictor = Predictor::with_model(sig, "gpt-4o")
    .with_temperature(0.5);
```

**4. Validate Input** (optional but recommended):
```rust
let input = json!({
    "text": "Long article about Rust programming...",
    "max_length": 100
});

let validator = predictor.signature().validator();
validator.validate(&input)?; // Compile-time type safety + runtime validation
```

**5. Execute**:
```rust
let mut inputs = HashMap::new();
inputs.insert("text".to_string(), json!("Long article..."));
inputs.insert("max_length".to_string(), json!(100));

let outputs = predictor.forward(inputs).await?;
let summary = outputs.get("summary")
    .ok_or("Missing summary")?
    .as_str()
    .ok_or("Summary not a string")?;

println!("Summary: {}", summary);
```

### Type Safety Guarantees

| Layer | Guarantee | Mechanism |
|-------|-----------|-----------|
| **Compile-Time** | Signature fields exist | Rust type system |
| **Compile-Time** | Result/Option handling | No unwrap in production |
| **Runtime** | Input validation | SignatureValidator |
| **Runtime** | Constraint satisfaction | FieldConstraints |
| **Runtime** | Type coercion | JSON Schema validation |

### Prompt Construction Details

**Predictor builds prompts as**:
```
{signature.description}

[Optional] Instructions: {signature.instructions}

Input:
{prefix}{field_name}: {value}
...

Output:
{field_name}:
...
```

**Example**:
```
Summarize text

Instructions: Provide concise, factual summaries.

Input:
text: "Rust is a systems programming language..."
max_length: 100

Output:
summary:
```

**LLM Response** (parsed):
```
summary: Rust is a fast, memory-safe systems language without garbage collection.
```

**Parsed Output**:
```rust
{
    "summary": "Rust is a fast, memory-safe systems language without garbage collection."
}
```

---

## Error Handling

### Error Philosophy

**Constitutional Rule**: Production code NEVER uses `unwrap()` or `expect()`.

**All operations return**:
```rust
pub type Result<T> = std::result::Result<T, GgenAiError>;
```

### Error Types

#### 1. ModuleError

```rust
pub enum ModuleError {
    MissingInput(String),              // Required input not provided
    InvalidInputType(String, String),  // Wrong type for field
    LlmError(String),                  // LLM call failed
    Other(String),                     // Generic error
}
```

**Example**:
```rust
// Missing input
let inputs = HashMap::new(); // Empty!
predictor.forward(inputs).await
    => Err(ModuleError::MissingInput("question"))

// Wrong type
inputs.insert("age", json!("not a number"));
predictor.forward(inputs).await
    => Err(ModuleError::InvalidInputType("age", "expected i32"))
```

#### 2. ValidationError

```rust
pub struct ValidationError {
    pub errors: Vec<ValidationErrorDetail>,
}

pub struct ValidationErrorDetail {
    pub field: String,
    pub error_type: ValidationErrorType,
    pub message: String,
}

pub enum ValidationErrorType {
    FieldMissing,
    NullValue,
    TypeMismatch,
    StringTooShort,
    StringTooLong,
    TooFewItems,
    TooManyItems,
    PatternMismatch,
    EnumConstraintViolation,
    InvalidPattern,
}
```

**Example**:
```rust
let validator = sig.validator();
let input = json!({
    "email": "invalid",   // Doesn't match pattern
    "age": "twenty"       // Wrong type
});

match validator.validate(&input) {
    Err(e) => {
        println!("Validation failed with {} errors:", e.error_count());
        for detail in e.errors {
            println!("  - {}: {}", detail.field, detail.message);
        }
    }
    Ok(_) => println!("Valid"),
}

// Output:
// Validation failed with 2 errors:
//   - email: value 'invalid' does not match pattern '^[a-z]+@[a-z]+\.[a-z]+$'
//   - age: expected integer or number, got string
```

#### 3. GgenAiError

**Top-level error type** for the entire crate:

```rust
pub enum GgenAiError {
    Validation(String),
    Io(std::io::Error),
    Rdf(String),
    Llm(String),
    Serialization(String),
    Other(String),
}
```

**Context Mapping**:
```rust
use ggen_ai::error::{GgenAiError, Result};

fn load_ontology(path: &str) -> Result<Store> {
    let file = File::open(path)
        .map_err(|e| GgenAiError::Io(e))?;

    let store = Store::new()
        .map_err(|e| GgenAiError::Rdf(e.to_string()))?;

    store.load_from_reader(RdfFormat::Turtle, BufReader::new(file))
        .map_err(|e| GgenAiError::Rdf(e.to_string()))?;

    Ok(store)
}
```

### Error Recovery Patterns

**1. Provide Defaults**:
```rust
let model = std::env::var("GGEN_LLM_MODEL")
    .unwrap_or_else(|_| "gpt-4o".to_string());
```

**2. Graceful Degradation**:
```rust
let description = property.description
    .unwrap_or_else(|| "No description provided".to_string());
```

**3. Collect All Errors**:
```rust
let mut errors = Vec::new();
for field in &signature.inputs {
    if let Err(e) = validate_field(field, &inputs) {
        errors.extend(e.errors);
    }
}
if !errors.is_empty() {
    return Err(ValidationError::multiple(errors));
}
```

---

## Differences from Python DSPy

### Conceptual Mapping

| Python DSPy | Rust DSPy | Notes |
|-------------|-----------|-------|
| `dspy.Signature` | `Signature` struct | Similar API, but typed |
| `dspy.InputField()` | `InputField::new()` | Builder pattern in Rust |
| `dspy.OutputField()` | `OutputField::new()` | Builder pattern in Rust |
| `dspy.Module` | `Module` trait | Trait vs base class |
| `dspy.Predict` | `Predictor` | Concrete struct |
| `dspy.ChainOfThought` | `ChainOfThought` | Wrapper around Predictor |
| Exceptions | `Result<T, E>` | Explicit error handling |
| Duck typing | Static types | Compile-time guarantees |
| Runtime validation | Compile + Runtime | Type system + validators |

### Key Differences

#### 1. Type System

**Python** (dynamic):
```python
class QA(dspy.Signature):
    """Answer questions."""
    question = dspy.InputField()
    answer = dspy.OutputField()

# No type checking until runtime
qa = dspy.Predict(QA)
result = qa(question=123)  # Accepted, might fail later
```

**Rust** (static):
```rust
let sig = Signature::new("QA", "Answer questions")
    .with_input(InputField::new("question", "Question", "String"))
    .with_output(OutputField::new("answer", "Answer", "String"));

let predictor = Predictor::with_model(sig, "gpt-4o");

// Compile error if wrong type
let inputs = HashMap::new();
inputs.insert("question", 123);  // Type mismatch caught at compile time
```

#### 2. Error Handling

**Python**:
```python
try:
    result = qa(question="What is Rust?")
except Exception as e:
    print(f"Error: {e}")
```

**Rust**:
```rust
match predictor.forward(inputs).await {
    Ok(outputs) => println!("Success: {:?}", outputs),
    Err(ModuleError::MissingInput(field)) => {
        eprintln!("Missing required field: {}", field);
    }
    Err(e) => eprintln!("Error: {}", e),
}
```

#### 3. Constraints

**Python** (runtime only):
```python
question = dspy.InputField(desc="Question",
                           constraint=lambda x: len(x) > 5)
```

**Rust** (compile-time types + runtime validation):
```rust
InputField::new("question", "Question", "String")
    .with_min_length(5)
    .with_pattern(r"^\w+")
```

#### 4. Async/Await

**Python** (optional):
```python
result = qa(question="...")  # Synchronous by default
```

**Rust** (explicit):
```rust
let result = predictor.forward(inputs).await?;  // Async required
```

#### 5. RDF Integration

**Python DSPy**: No built-in RDF support

**Rust DSPy**: First-class RDF/SHACL integration via `TTLToSignatureTranspiler`

```rust
// Signatures generated from .specify/*.ttl files
let signatures = transpiler.build_signatures(&store)?;
```

#### 6. Memory Safety

**Python**: Garbage collected, reference counting

**Rust**: Ownership system, no GC
- No data races (checked at compile time)
- No null pointer dereferences
- No use-after-free

---

## Integration with ggen

### ggen Ecosystem Context

**ggen** is a specification-driven code generation system. DSPy is one layer in the stack:

```
┌─────────────────────────────────────────┐
│   User Intent (natural language)        │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│   RDF Ontology (.specify/*.ttl)         │  ◄── Source of truth
│   - Domain model                        │
│   - SHACL constraints                   │
│   - Field specifications                │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│   TTLToSignatureTranspiler              │  ◄── DSPy layer
│   - Converts RDF → Signature            │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│   DSPy Modules (Predictor, etc.)        │  ◄── Execution layer
│   - Type-safe LLM prompting             │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│   LLM Backend (genai)                   │
│   - Model routing                       │
│   - API integration                     │
└─────────────────────────────────────────┘
```

### Configuration

**ggen.toml**:
```toml
[llm]
default_model = "gpt-4o"
temperature = 0.7
max_tokens = 4096

[llm.models.gpt-4o]
provider = "openai"
api_key_env = "OPENAI_API_KEY"

[llm.models.claude-sonnet-4]
provider = "anthropic"
api_key_env = "ANTHROPIC_API_KEY"
```

**Environment Variables**:
```bash
export GGEN_LLM_MODEL="gpt-4o"
export OPENAI_API_KEY="sk-..."
```

### Workflow Integration

**1. Define Domain in RDF**:
```bash
# .specify/qa_system.ttl
@prefix : <http://ggen.io/qa/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

:QuestionAnsweringShape
    sh:targetClass :QuestionAnswering ;
    sh:property :QuestionProperty ;
    sh:property :AnswerProperty .
```

**2. Generate Signatures**:
```bash
cargo run --bin ggen -- sync
# Reads .specify/*.ttl, generates Rust code
```

**3. Use in Application**:
```rust
use ggen_ai::codegen::TTLToSignatureTranspiler;
use ggen_ai::dspy::Predictor;

let signatures = load_signatures_from_spec()?;
let qa_sig = signatures.iter()
    .find(|s| s.name.contains("QuestionAnswering"))
    .ok_or("QA signature not found")?;

let qa_module = Predictor::with_model(qa_sig.clone(), "gpt-4o");
```

### Testing Strategy

**Chicago TDD Pattern** (as per CLAUDE.md):

```rust
#[tokio::test]
async fn test_qa_module_answers_rust_questions() {
    // Arrange: Load real RDF store
    let store = load_ttl_fixture("qa_system.ttl");
    let mut transpiler = TTLToSignatureTranspiler::new();
    let signatures = transpiler.build_signatures(&store)
        .expect("Failed to build signatures");
    let sig = signatures.into_iter().next().expect("No signature");

    // Arrange: Create real predictor (could use test LLM or mock)
    let predictor = Predictor::with_model(sig, "test-model");

    // Act: Call forward with real inputs
    let mut inputs = HashMap::new();
    inputs.insert("question".to_string(), json!("What is Rust?"));
    let outputs = predictor.forward(inputs).await
        .expect("Forward failed");

    // Assert: Verify observable state
    assert!(outputs.contains_key("answer"));
    let answer = outputs["answer"].as_str().unwrap();
    assert!(!answer.is_empty());
    assert!(answer.to_lowercase().contains("rust"));
}
```

**No mocks** - use real objects, verify observable state changes.

---

## Common Patterns

### Pattern 1: Question Answering

**Signature**:
```rust
let qa_sig = Signature::new("QA", "Answer questions based on context")
    .with_input(InputField::new("question", "User question", "String"))
    .with_input(InputField::new("context", "Background context", "String"))
    .with_output(OutputField::new("answer", "Answer to question", "String"))
    .with_instructions("Base your answer strictly on the provided context.");
```

**Usage**:
```rust
let qa = Predictor::with_model(qa_sig, "gpt-4o");
let mut inputs = HashMap::new();
inputs.insert("question".to_string(), json!("What is DSPy?"));
inputs.insert("context".to_string(), json!("DSPy is a framework for..."));
let outputs = qa.forward(inputs).await?;
```

### Pattern 2: Classification

**Signature**:
```rust
let classify_sig = Signature::new("Classify", "Classify text into categories")
    .with_input(InputField::new("text", "Text to classify", "String"))
    .with_output(
        OutputField::new("category", "Predicted category", "String")
            .with_enum_values(vec![
                "technology".to_string(),
                "science".to_string(),
                "politics".to_string()
            ])
    );
```

**Validation enforces enum constraint**:
```rust
let validator = classify_sig.validator();
let output = json!({"category": "sports"}); // Not in enum
assert!(validator.validate(&output).is_err()); // Fails validation
```

### Pattern 3: Multi-Step Reasoning

**Chain-of-Thought**:
```rust
let reasoning_sig = Signature::new("Math", "Solve math problems")
    .with_input(InputField::new("problem", "Math problem", "String"))
    .with_output(OutputField::new("reasoning", "Step-by-step reasoning", "String"))
    .with_output(OutputField::new("answer", "Final answer", "String"));

let cot = ChainOfThought::new(reasoning_sig);
// Automatically includes "Think through this step-by-step" instruction
```

### Pattern 4: Structured Extraction

**Signature with constraints**:
```rust
let extract_sig = Signature::new("ExtractEmails", "Extract email addresses")
    .with_input(InputField::new("text", "Text containing emails", "String"))
    .with_output(
        OutputField::new("emails", "Extracted emails", "Vec<String>")
            .with_min_items(1)
            .with_max_items(10)
    );
```

**Validates array constraints**:
```rust
let output = json!({"emails": []});
assert!(validator.validate(&output).is_err()); // Too few items
```

### Pattern 5: RDF-Driven Task

**TTL Definition**:
```turtle
:ProductReviewShape
    sh:targetClass :ProductReview ;
    sh:property [
        sh:path :review_text ;
        sh:datatype xsd:string ;
        sh:minLength 10
    ] ;
    sh:property [
        sh:path :sentiment ;
        sh:datatype xsd:string ;
        cns:outputField "true" ;
        sh:in ("positive" "negative" "neutral")
    ] .
```

**Generated Code**:
```rust
let sig = transpiler.build_signatures(&store)?
    .into_iter().next().unwrap();

// Signature has:
// - input: review_text (String, min_length=10)
// - output: sentiment (String, enum=["positive", "negative", "neutral"])
```

### Pattern 6: Validation Pipeline

**Pre-execution validation**:
```rust
fn execute_with_validation(
    predictor: &Predictor,
    inputs: HashMap<String, Value>
) -> Result<HashMap<String, Value>> {
    // 1. Validate inputs
    let validator = predictor.signature().validator();
    validator.validate(&serde_json::to_value(&inputs)?)?;

    // 2. Execute
    let outputs = predictor.forward(inputs).await?;

    // 3. Validate outputs (optional)
    // Could validate outputs against output field constraints

    Ok(outputs)
}
```

---

## Testing & Examples

### Test Files

**Location**: `crates/ggen-ai/tests/`

1. **ttl_to_signature.rs** - TTL transpilation integration tests
   - SHACL shape extraction
   - Field naming transformations
   - Type mapping
   - Collision detection
   - Multi-class support

2. **signature_validation.rs** - Validation tests
   - Constraint checking
   - Error aggregation
   - JSON Schema generation

### Example Files

**Location**: `crates/ggen-ai/examples/`

1. **ttl_to_signature_demo.rs**
   - Complete workflow: TTL → Signature
   - Demonstrates all transpiler methods
   - Shows field naming, type mapping

**Run**:
```bash
cargo run --example ttl_to_signature_demo --package ggen-ai
```

### Test Fixtures

**Location**: `crates/ggen-ai/tests/fixtures/`

Example fixtures:
- `simple_shape.ttl` - Basic SHACL shape
- `shape_with_constraints.ttl` - Min/max/pattern constraints
- `shape_with_output_fields.ttl` - Output field classification
- `shape_with_multiple_classes.ttl` - Multiple shapes in one file

### Running Tests

```bash
# All tests
cargo test --package ggen-ai

# Specific test suite
cargo test --test ttl_to_signature

# With output
cargo test --package ggen-ai -- --nocapture

# Specific test
cargo test test_basic_shape_extraction_finds_all_properties
```

### Documentation Tests

**Location**: Inline in source files

```rust
/// Validate JSON input
///
/// # Example
/// ```
/// use ggen_ai::dspy::{Signature, InputField, SignatureValidator};
/// use serde_json::json;
///
/// let sig = Signature::new("Test", "Test")
///     .with_input(InputField::new("name", "Name", "String"));
/// let validator = sig.validator();
/// assert!(validator.validate(&json!({"name": "Alice"})).is_ok());
/// ```
```

**Run doc tests**:
```bash
cargo test --doc --package ggen-ai
```

---

## Performance Considerations

### Memory Budget

**Target**: ≤ 100MB for typical workloads

**Optimizations**:
- Stack allocation where possible (Signature, Field structs)
- Minimal heap allocations (HashMap, Vec only when necessary)
- No cloning unless required (use references)

### Time Complexity

| Operation | Complexity | Notes |
|-----------|------------|-------|
| Signature creation | O(n) | n = fields |
| Validation | O(n*m) | n = fields, m = avg constraints |
| TTL transpilation | O(c*p) | c = classes, p = properties |
| Prompt building | O(n) | n = fields |
| LLM call | O(1) network | Dominated by network/LLM latency |

### Async Performance

**genai** uses Tokio for async LLM calls:
- Non-blocking I/O
- Concurrent requests possible
- No thread-per-request overhead

---

## Advanced Topics

### Custom Validators

**Extend FieldConstraints**:
```rust
impl FieldConstraints {
    pub fn custom_validator<F>(&self, f: F) -> Result<()>
    where
        F: Fn(&Value) -> Result<()>,
    {
        // Apply custom validation logic
        f(value)
    }
}
```

### Signature Composition

**Combine multiple signatures**:
```rust
let sig1 = Signature::new("Part1", "First part")
    .with_input(InputField::new("a", "A", "String"));

let sig2 = Signature::new("Part2", "Second part")
    .with_input(InputField::new("b", "B", "String"));

let combined = Signature::new("Combined", "Combined")
    .with_inputs(sig1.inputs)
    .with_inputs(sig2.inputs);
```

### Dynamic Model Selection

**Route based on task**:
```rust
fn select_model(task_complexity: &str) -> &'static str {
    match task_complexity {
        "simple" => "gpt-4o-mini",
        "complex" => "gpt-4o",
        "reasoning" => "claude-sonnet-4",
        _ => "gpt-4o",
    }
}

let model = select_model("reasoning");
let predictor = Predictor::with_model(sig, model);
```

---

## Appendix: Quick Reference

### Signature Creation

```rust
Signature::new(name, description)
    .with_input(InputField::new(name, desc, type))
    .with_output(OutputField::new(name, desc, type))
    .with_instructions(text)
```

### Field Constraints

```rust
InputField::new(name, desc, type)
    .required(bool)
    .with_min_length(n)
    .with_max_length(n)
    .with_min_items(n)
    .with_max_items(n)
    .with_pattern(regex)
    .with_enum_values(vec)
    .with_semantic_type(uri)
```

### Validation

```rust
let validator = sig.validator();
validator.validate(&json_input)?;
let result = validator.validate_to_json(&json_input);
```

### Predictor

```rust
let predictor = Predictor::with_model(sig, model)
    .with_temperature(0.7);
let outputs = predictor.forward(inputs).await?;
```

### TTL Transpilation

```rust
let mut store = Store::new()?;
store.load_from_reader(RdfFormat::Turtle, reader)?;
let mut transpiler = TTLToSignatureTranspiler::new();
let signatures = transpiler.build_signatures(&store)?;
```

### Error Handling

```rust
match result {
    Ok(value) => { /* success */ }
    Err(ModuleError::MissingInput(field)) => { /* handle */ }
    Err(e) => { /* other errors */ }
}
```

---

## Resources

### Documentation

- **API Docs**: `cargo doc --open --package ggen-ai`
- **CLAUDE.md**: Project conventions and rules
- **TTL_TO_SIGNATURE.md**: Detailed transpiler documentation

### Examples

- `crates/ggen-ai/examples/ttl_to_signature_demo.rs`
- `crates/ggen-ai/tests/ttl_to_signature.rs`

### External References

- [SHACL Specification](https://www.w3.org/TR/shacl/)
- [DSPy (Python)](https://github.com/stanfordnlp/dspy)
- [genai-rs](https://github.com/jeremychone/rust-genai)
- [Oxigraph RDF Store](https://github.com/oxigraph/oxigraph)

---

**Version**: 1.0.0
**Last Updated**: 2026-01-11
**Maintainers**: ggen contributors
**License**: Same as ggen project
