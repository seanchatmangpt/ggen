<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Agent Integration Guide](#agent-integration-guide)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
    - [Big Bang 80/20 Principle](#big-bang-8020-principle)
  - [Architecture](#architecture)
    - [Key Components](#key-components)
  - [Core Concepts](#core-concepts)
    - [1. Signatures: The Contract Layer](#1-signatures-the-contract-layer)
    - [2. Tool Registry: The Discovery Engine](#2-tool-registry-the-discovery-engine)
    - [3. Agent-Tool Binding: The Execution Model](#3-agent-tool-binding-the-execution-model)
  - [Tool Registry Pattern](#tool-registry-pattern)
    - [Registering a Tool](#registering-a-tool)
    - [Tool Discovery](#tool-discovery)
    - [Tool Invocation with Automatic Validation](#tool-invocation-with-automatic-validation)
  - [Signature Integration](#signature-integration)
    - [Creating Signatures from RDF (TTL)](#creating-signatures-from-rdf-ttl)
    - [Converting TTL to Signatures](#converting-ttl-to-signatures)
  - [Agent Execution Flow](#agent-execution-flow)
    - [Complete Agent-Tool Interaction Cycle](#complete-agent-tool-interaction-cycle)
    - [Registering Agent with Registry](#registering-agent-with-registry)
  - [Error Handling](#error-handling)
    - [Validation Error Propagation](#validation-error-propagation)
    - [Handling Tool Invocation Errors](#handling-tool-invocation-errors)
    - [Error Context Mapping](#error-context-mapping)
  - [Performance Tuning](#performance-tuning)
    - [SLO Targets](#slo-targets)
    - [Caching Validation Results](#caching-validation-results)
    - [Parallel Tool Execution](#parallel-tool-execution)
  - [Security Considerations](#security-considerations)
    - [Input Sanitization](#input-sanitization)
    - [SPARQL Injection Prevention](#sparql-injection-prevention)
    - [Signature Validation Security](#signature-validation-security)
  - [Troubleshooting](#troubleshooting)
    - [Common Issues and Solutions](#common-issues-and-solutions)
      - [Issue: "Tool not found" error](#issue-tool-not-found-error)
      - [Issue: Input validation failures](#issue-input-validation-failures)
      - [Issue: Output validation failures](#issue-output-validation-failures)
      - [Issue: Performance degradation](#issue-performance-degradation)
      - [Issue: Concurrent access panics](#issue-concurrent-access-panics)
  - [Complete Example: Weather Analysis Domain](#complete-example-weather-analysis-domain)
  - [Next Steps](#next-steps)
  - [Reference](#reference)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Agent Integration Guide

**Complete Pattern for Binding Signatures to Agents and Tool Registries**

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Core Concepts](#core-concepts)
4. [Tool Registry Pattern](#tool-registry-pattern)
5. [Signature Integration](#signature-integration)
6. [Agent Execution Flow](#agent-execution-flow)
7. [Error Handling](#error-handling)
8. [Performance Tuning](#performance-tuning)
9. [Security Considerations](#security-considerations)
10. [Troubleshooting](#troubleshooting)

---

## Overview

This guide documents the complete pattern for integrating custom agents with the ggen ecosystem. Agents bind to **Signatures** (type-safe interface specifications) through a **Tool Registry** that enables:

- **Discovery**: Agents find tools by type, domain, or capability
- **Validation**: Signatures enforce input/output constraints
- **Invocation**: Agents call tools with type-safe arguments
- **Error Propagation**: Validation errors bubble up through Result types

### Big Bang 80/20 Principle

The core pattern follows the 80/20 rule:

- **20% Core**: Agent trait + Registry + Signature binding
- **80% Variation**: Domain-specific tools, validators, error handlers

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     Agent Integration Layer                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│  ┌──────────────────┐         ┌──────────────────┐              │
│  │    Signature     │         │    Agent Config  │              │
│  │  ┌─────────────┐ │         │  ┌────────────┐  │              │
│  │  │ InputField  │ │         │  │ role       │  │              │
│  │  │ OutputField │ │         │  │ capabilities   │              │
│  │  │ Constraints │ │         │  │ max_tasks  │  │              │
│  │  └─────────────┘ │         │  └────────────┘  │              │
│  └──────────────────┘         └──────────────────┘              │
│           ▲                             ▲                        │
│           │                             │                        │
│     ┌─────┴─────────┬──────────────────┴─────┐                  │
│     │               │                        │                  │
│  ┌──▼──────────┐ ┌─▼───────────┐ ┌────────▼──┐                │
│  │   Tool      │ │  Validator  │ │  Registry │                │
│  │  Registry   │ │             │ │           │                │
│  └─────────────┘ └─────────────┘ └───────────┘                │
│        ▲              ▲                 ▲                       │
│        └──────────────┼─────────────────┘                       │
│                       │                                         │
│              ┌────────▼────────┐                               │
│              │   Agent Trait   │                               │
│              │  ┌────────────┐ │                               │
│              │  │ execute()  │ │                               │
│              │  │ validate() │ │                               │
│              │  └────────────┘ │                               │
│              └─────────────────┘                               │
│                                                                   │
└─────────────────────────────────────────────────────────────────┘
```

### Key Components

| Component | Purpose | Responsibility |
|-----------|---------|-----------------|
| **Signature** | Type-safe interface contract | Defines inputs/outputs/constraints |
| **Agent** | Autonomous executor | Implements Agent trait, calls tools |
| **Tool Registry** | Tool discovery & management | Register, query, invoke tools |
| **SignatureValidator** | Input/output validation | Apply constraints, report errors |
| **FieldConstraints** | Validation rules | Min/max length, patterns, custom rules |

---

## Core Concepts

### 1. Signatures: The Contract Layer

Signatures define the **interface** between agents and tools. They're the specification equivalent of function signatures:

```rust
// Conceptual example
pub struct Signature {
    pub name: String,               // e.g., "FinancialAnalyzer"
    pub description: String,        // Agent-readable docs
    pub inputs: Vec<InputField>,    // What agent provides
    pub outputs: Vec<OutputField>,  // What tool produces
    pub instructions: Option<String>, // LLM instructions (if ML-backed)
}

pub struct InputField {
    pub name: String,               // e.g., "ticker_symbol"
    pub type_annotation: String,    // e.g., "String"
    pub description: String,        // e.g., "Stock ticker (e.g., AAPL)"
    pub constraints: FieldConstraints, // Validation rules
}
```

**Example: Financial Analyzer Signature**

```rust
let financial_analyzer = Signature::new(
    "FinancialAnalyzer",
    "Analyzes stock data and returns investment recommendations"
)
.with_input(
    InputField::new("ticker_symbol", "String")
        .with_description("Stock ticker symbol")
        .with_constraint(FieldConstraint::pattern("^[A-Z]{1,5}$"))
        .with_constraint(FieldConstraint::required())
)
.with_input(
    InputField::new("period_days", "i32")
        .with_description("Historical analysis period in days")
        .with_constraint(FieldConstraint::min_value(1))
        .with_constraint(FieldConstraint::max_value(365))
)
.with_output(
    OutputField::new("recommendation", "String")
        .with_description("Investment recommendation (BUY/SELL/HOLD)")
)
.with_output(
    OutputField::new("confidence_score", "f64")
        .with_description("Confidence level (0.0-1.0)")
);
```

### 2. Tool Registry: The Discovery Engine

The Tool Registry manages the lifecycle of available tools:

```rust
pub struct ToolRegistry {
    // Map tool name -> implementation
    tools: Arc<RwLock<HashMap<String, Arc<dyn Tool>>>>,

    // Map domain -> tool names (for discovery)
    domain_index: Arc<RwLock<HashMap<String, Vec<String>>>>,

    // Validation results cache
    validation_cache: Arc<RwLock<HashMap<String, ValidationResult>>>,
}

// Core operations
impl ToolRegistry {
    pub async fn register(&self, tool: Arc<dyn Tool>) -> Result<()> { }
    pub async fn get(&self, name: &str) -> Option<Arc<dyn Tool>> { }
    pub async fn find_by_domain(&self, domain: &str) -> Vec<Arc<dyn Tool>> { }
    pub async fn list_all(&self) -> Vec<String> { }
    pub async fn invoke(&self, tool: &str, input: &InputData) -> Result<OutputData> { }
}
```

### 3. Agent-Tool Binding: The Execution Model

Agents invoke tools through the registry by name, with automatic validation:

```
Agent:
1. Get InputData from external source (user, event, other agent)
2. Look up Signature by tool name in registry
3. Validate InputData against Signature's InputFields
4. If validation fails → return ValidationError
5. If validation passes → Call tool via registry.invoke()
6. Get OutputData
7. Validate OutputData against Signature's OutputFields
8. If validation fails → log and optionally retry
9. Return Result<OutputData> to caller
```

---

## Tool Registry Pattern

### Registering a Tool

```rust
use ggen_ai::tools::{Tool, InputData, OutputData};
use ggen_ai::dspy::Signature;
use std::sync::Arc;

// 1. Define the tool implementation
#[derive(Debug)]
struct MyFinancialTool {
    name: String,
    signature: Signature,
}

#[async_trait]
impl Tool for MyFinancialTool {
    fn name(&self) -> &str {
        &self.name
    }

    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn execute(
        &self,
        input: InputData,
    ) -> Result<OutputData> {
        // Validate input using signature
        let validator = SignatureValidator::new(self.signature().clone());
        validator.validate_input(&input)?;

        // Execute business logic
        let ticker = input.get("ticker_symbol")
            .ok_or(GgenAiError::MissingField("ticker_symbol".into()))?;

        // Produce output
        let mut output = OutputData::new();
        output.insert("recommendation".into(), serde_json::json!("BUY"));
        output.insert("confidence_score".into(), serde_json::json!(0.85));

        // Validate output before returning
        validator.validate_output(&output)?;

        Ok(output)
    }
}

// 2. Create registry instance
let registry = ToolRegistry::new();

// 3. Register the tool
let tool = Arc::new(MyFinancialTool {
    name: "FinancialAnalyzer".to_string(),
    signature: financial_analyzer, // Signature defined earlier
});
registry.register(tool).await?;
```

### Tool Discovery

```rust
// By exact name
if let Some(tool) = registry.get("FinancialAnalyzer").await {
    println!("Found tool: {}", tool.name());
}

// By domain
let market_tools = registry.find_by_domain("finance").await;
for tool in market_tools {
    println!("Domain tool: {}", tool.name());
}

// List all
let all_tools = registry.list_all().await;
println!("Available tools: {:?}", all_tools);
```

### Tool Invocation with Automatic Validation

```rust
let input_data = InputData::from_json(serde_json::json!({
    "ticker_symbol": "AAPL",
    "period_days": 90,
}))?;

// Registry handles validation + execution
let result = registry.invoke("FinancialAnalyzer", &input_data).await;

match result {
    Ok(output) => {
        let recommendation = output.get("recommendation").unwrap();
        let confidence = output.get("confidence_score").unwrap();
        println!("Recommendation: {}, Confidence: {}", recommendation, confidence);
    }
    Err(e) => eprintln!("Tool invocation failed: {}", e),
}
```

---

## Signature Integration

### Creating Signatures from RDF (TTL)

Signatures can be derived from RDF ontologies using SHACL shapes:

```ttl
@prefix : <http://example.com/finance/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix cns: <http://cns.io/ontology#> .

# Financial Analysis Tool Shape
:FinancialAnalyzerShape
    a sh:NodeShape ;
    sh:targetClass :FinancialAnalyzer ;
    sh:property :TickerProperty ;
    sh:property :PeriodProperty ;
    sh:property :RecommendationProperty ;
    sh:property :ConfidenceProperty .

:TickerProperty
    sh:path :ticker_symbol ;
    sh:datatype xsd:string ;
    sh:minLength 1 ;
    sh:maxLength 5 ;
    rdfs:comment "Stock ticker symbol (e.g., AAPL)" .

:PeriodProperty
    sh:path :period_days ;
    sh:datatype xsd:integer ;
    sh:minInclusive 1 ;
    sh:maxInclusive 365 ;
    rdfs:comment "Historical analysis period" .

:RecommendationProperty
    sh:path :recommendation ;
    sh:datatype xsd:string ;
    cns:outputField "true" ;
    rdfs:comment "Investment recommendation (BUY/SELL/HOLD)" .

:ConfidenceProperty
    sh:path :confidence_score ;
    sh:datatype xsd:double ;
    cns:outputField "true" ;
    rdfs:comment "Confidence level (0.0-1.0)" .
```

### Converting TTL to Signatures

```rust
use ggen_ai::codegen::TTLToSignatureTranspiler;
use oxigraph::store::Store;
use oxigraph::io::RdfFormat;
use std::io::BufReader;
use std::fs::File;

// Load TTL ontology
let store = Store::new()?;
let file = File::open("finance.ttl")?;
let reader = BufReader::new(file);
store.load_graph(
    reader,
    RdfFormat::Turtle,
    None,
)?;

// Convert to Signatures
let transpiler = TTLToSignatureTranspiler::new();
let signatures = transpiler.transpile(&store)?;

// Register tools based on signatures
for signature in signatures {
    println!("Generated signature: {}", signature.name);
    // Create tool implementation for this signature...
}
```

---

## Agent Execution Flow

### Complete Agent-Tool Interaction Cycle

```rust
use ggen_ai::agents::{Agent, AgentConfig, AgentMessage, TaskDefinition};
use std::sync::Arc;
use uuid::Uuid;

#[derive(Debug)]
struct FinancialAgent {
    config: AgentConfig,
    registry: Arc<ToolRegistry>,
}

#[async_trait]
impl Agent for FinancialAgent {
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        println!("Initializing financial agent...");
        Ok(())
    }

    async fn start(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        println!("Starting financial agent");
        Ok(())
    }

    async fn stop(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        println!("Stopping financial agent");
        Ok(())
    }

    async fn status(&self) -> AgentStatus {
        AgentStatus::Healthy
    }

    fn config(&self) -> &AgentConfig {
        &self.config
    }

    async fn handle_message(
        &mut self,
        message: AgentMessage,
    ) -> Result<AgentMessage, Box<dyn std::error::Error + Send + Sync>> {
        match message {
            AgentMessage::TaskAssignment { task_id, task } => {
                // Execute the task
                let result = self.execute_task(&task).await?;

                Ok(AgentMessage::TaskCompletion {
                    task_id,
                    result,
                })
            }
            _ => Ok(message),
        }
    }
}

impl FinancialAgent {
    async fn execute_task(&self, task: &TaskDefinition) -> Result<TaskResult> {
        let start = std::time::Instant::now();

        // Extract parameters from task
        let ticker = task.parameters
            .get("ticker_symbol")
            .ok_or("Missing ticker_symbol")?;

        // Build input data
        let input = InputData::from_json(serde_json::json!({
            "ticker_symbol": ticker,
            "period_days": 90,
        }))?;

        // Execute tool through registry
        let output = self.registry.invoke("FinancialAnalyzer", &input).await?;

        let duration_ms = start.elapsed().as_millis() as u64;

        Ok(TaskResult {
            task_id: task.id,
            success: true,
            result: Some(serde_json::to_value(output)?),
            error: None,
            duration_ms,
            metrics: None,
        })
    }
}
```

### Registering Agent with Registry

```rust
// Create agent registry
let agent_registry = AgentRegistry::new();

// Create financial agent
let financial_agent = Arc::new(FinancialAgent {
    config: AgentConfig {
        id: Uuid::new_v4(),
        name: "FinancialAnalyst".to_string(),
        role: AgentRole::Validator,
        capabilities: vec!["stock_analysis".to_string()],
        max_concurrent_tasks: 5,
    },
    registry: tool_registry.clone(),
});

// Register agent
agent_registry.register(financial_agent).await;

// Query agent by name
if let Some(agent) = agent_registry.get("FinancialAnalyst").await {
    println!("Agent config: {:?}", agent.config());
}

// List all agents
let all_agents = agent_registry.list().await;
println!("Available agents: {:?}", all_agents);

// Check health of all agents
let health_status = agent_registry.health_check_all().await;
for (agent_name, health) in health_status {
    println!("{}: {:?}", agent_name, health);
}
```

---

## Error Handling

### Validation Error Propagation

```rust
#[derive(Debug, thiserror::Error)]
pub enum ValidationError {
    #[error("Field '{field}' validation failed: {reason}")]
    FieldValidation { field: String, reason: String },

    #[error("Missing required field: {0}")]
    MissingField(String),

    #[error("Type mismatch for field '{field}': expected {expected}, got {actual}")]
    TypeMismatch {
        field: String,
        expected: String,
        actual: String,
    },

    #[error("Pattern mismatch for field '{field}': expected pattern {pattern}")]
    PatternMismatch { field: String, pattern: String },

    #[error("Value {value} out of range for field '{field}': [{min}, {max}]")]
    OutOfRange {
        field: String,
        value: String,
        min: String,
        max: String,
    },
}

pub type Result<T> = std::result::Result<T, GgenAiError>;
```

### Handling Tool Invocation Errors

```rust
match registry.invoke("FinancialAnalyzer", &input).await {
    Ok(output) => {
        // Success: validate and use output
        match validator.validate_output(&output) {
            Ok(_) => println!("Output is valid"),
            Err(e) => eprintln!("Output validation failed: {}", e),
        }
    }
    Err(GgenAiError::Validation(e)) => {
        // Input validation failed - don't retry
        eprintln!("Invalid input: {}", e);
    }
    Err(GgenAiError::ToolNotFound(name)) => {
        // Tool missing - check registry
        eprintln!("Tool '{}' not found. Available: {:?}", name, registry.list_all().await);
    }
    Err(e) => {
        // Other errors (network, timeout, etc) - may retry
        eprintln!("Tool execution error: {}", e);
    }
}
```

### Error Context Mapping

```rust
// Map domain-specific errors to Result
let result = async {
    let tool = self.registry.get("FinancialAnalyzer")
        .await
        .ok_or_else(|| GgenAiError::ToolNotFound("FinancialAnalyzer".into()))?;

    let output = tool.execute(input)
        .await
        .map_err(|e| GgenAiError::ToolExecution(
            "FinancialAnalyzer".into(),
            e.to_string(),
        ))?;

    Ok::<_, GgenAiError>(output)
}.await;
```

---

## Performance Tuning

### SLO Targets

All integration operations must meet these SLOs:

| Operation | Target | Mechanism |
|-----------|--------|-----------|
| Tool lookup (exact) | <1ms | HashMap |
| Tool discovery (by domain) | <5ms | Index |
| Input validation | <10ms | Parallel constraints |
| Output validation | <10ms | Parallel constraints |
| Tool execution | <100ms | Domain-dependent |
| Complete invoke() cycle | <150ms | Registry orchestration |

### Caching Validation Results

```rust
pub struct ToolRegistry {
    // ... other fields ...

    // Cache: (tool_name, input_hash) -> validation_result
    validation_cache: Arc<RwLock<LruCache<String, ValidationResult>>>,
}

impl ToolRegistry {
    pub async fn invoke_cached(
        &self,
        tool: &str,
        input: &InputData,
    ) -> Result<OutputData> {
        let cache_key = format!("{}::{:?}", tool, input);

        // Check cache
        if let Some(cached_result) = self.validation_cache.read().await.get(&cache_key) {
            if cached_result.is_valid() {
                // Validation was successful in past - skip for identical input
                return self.execute_tool_unchecked(tool, input).await;
            }
        }

        // Full validation path
        self.invoke(tool, input).await
    }
}
```

### Parallel Tool Execution

```rust
// Execute multiple tools concurrently
let futures = vec![
    registry.invoke("FinancialAnalyzer", &input1),
    registry.invoke("RiskCalculator", &input2),
    registry.invoke("PortfolioOptimizer", &input3),
];

let results = futures::future::try_join_all(futures).await?;
```

---

## Security Considerations

### Input Sanitization

```rust
// Sanitize inputs before passing to tools
fn sanitize_input(input: &mut InputData) -> Result<()> {
    for (key, value) in input.iter_mut() {
        // Remove potentially dangerous characters
        if let Some(s) = value.as_str_mut() {
            *s = s.trim_matches(|c: char| !c.is_ascii_alphanumeric() && c != '-' && c != '_').to_string();
        }
    }
    Ok(())
}
```

### SPARQL Injection Prevention

```rust
// Never directly interpolate SPARQL queries
// ❌ WRONG:
// let query = format!("SELECT * WHERE {{ ?s ?p '{}' }}", user_input);

// ✅ CORRECT: Use parameterized queries
fn build_safe_query(parameter: &str) -> String {
    let escaped = parameter
        .replace("\\", "\\\\")
        .replace("'", "\\'");
    format!("SELECT * WHERE {{ ?s ?p '{}' }}", escaped)
}
```

### Signature Validation Security

```rust
// Validate signatures come from trusted sources
pub struct SignatureCertificate {
    signature: Signature,
    signer: PublicKey,
    timestamp: SystemTime,
    ttl: Duration,
}

impl SignatureCertificate {
    pub fn is_trusted(&self, trusted_signers: &[PublicKey]) -> bool {
        trusted_signers.contains(&self.signer)
            && SystemTime::now().duration_since(self.timestamp).unwrap_or(Duration::MAX) < self.ttl
    }
}
```

---

## Troubleshooting

### Common Issues and Solutions

#### Issue: "Tool not found" error

**Symptom**: `GgenAiError::ToolNotFound("FinancialAnalyzer")`

**Solutions**:
1. Verify tool is registered: `registry.list_all().await`
2. Check tool name matches exactly (case-sensitive)
3. Ensure registration completed before invocation
4. Check tool registry is properly shared across agents

```rust
// Debug: List all registered tools
let tools = registry.list_all().await;
println!("Registered tools: {:?}", tools);

// Debug: Check specific tool
if let Some(tool) = registry.get("FinancialAnalyzer").await {
    println!("Tool found: {} (Signature: {})", tool.name(), tool.signature().name);
} else {
    println!("Tool not found!");
}
```

#### Issue: Input validation failures

**Symptom**: `ValidationError::FieldValidation { field, reason }`

**Solutions**:
1. Check input data against signature: `signature.input_names()`
2. Verify data types match field annotations
3. Check constraints (min/max, patterns, etc.)
4. Use `validator.explain_error()` for details

```rust
let validator = SignatureValidator::new(signature.clone());

match validator.validate_input(&input) {
    Ok(_) => println!("Input valid"),
    Err(e) => {
        // Print detailed explanation
        eprintln!("Validation failed: {}", e);
        eprintln!("Expected fields: {:?}", signature.input_names());
        eprintln!("Input data: {:?}", input);
    }
}
```

#### Issue: Output validation failures

**Symptom**: Tool executes but output doesn't match signature

**Solutions**:
1. Verify tool's OutputField definitions in signature
2. Check tool implementation produces correct fields
3. Verify field types match (especially numeric precision)
4. Use `validator.explain_error()` to see exact mismatch

```rust
match validator.validate_output(&output) {
    Ok(_) => println!("Output valid"),
    Err(e) => {
        eprintln!("Output validation failed: {}", e);
        eprintln!("Expected output fields: {:?}", signature.output_names());
        eprintln!("Actual output: {:?}", output);
    }
}
```

#### Issue: Performance degradation

**Symptom**: Tool invocation takes >150ms for simple operations

**Solutions**:
1. Check tool execution time: `result.duration_ms`
2. Profile validation separately: disable caching and measure
3. Use validation caching for repeated inputs
4. Reduce validation complexity (fewer constraints if safe)

```rust
// Measure components separately
let start_validate = Instant::now();
validator.validate_input(&input)?;
println!("Input validation: {}ms", start_validate.elapsed().as_millis());

let start_exec = Instant::now();
let output = registry.invoke("FinancialAnalyzer", &input).await?;
println!("Tool execution: {}ms", start_exec.elapsed().as_millis());

let start_output = Instant::now();
validator.validate_output(&output)?;
println!("Output validation: {}ms", start_output.elapsed().as_millis());
```

#### Issue: Concurrent access panics

**Symptom**: Registry panics under concurrent load

**Solutions**:
1. Use `Arc<RwLock<>>` for shared mutable state
2. Minimize lock duration (don't hold locks across awaits)
3. Use read locks where possible
4. Consider lock-free structures (e.g., `DashMap`)

```rust
// ✅ CORRECT: Release lock before await
let tool = {
    let registry = self.registry.read().await;
    registry.get("FinancialAnalyzer").cloned().ok_or("Not found")?
};
let result = tool.execute(input).await?;

// ❌ WRONG: Hold lock across await
let registry = self.registry.write().await;
let result = registry.invoke("FinancialAnalyzer", input).await?;
```

---

## Complete Example: Weather Analysis Domain

See `/docs/examples/agent_integration_basic.rs` for a complete, runnable example integrating:
- Custom WeatherAgent
- Tool Registry with weather domain tools
- Signature-based validation
- Error handling
- Performance monitoring

---

## Next Steps

1. **Tool Registry API** - Full method reference in [API_REFERENCE.md](API_REFERENCE.md)
2. **MCP Interface** - Model Context Protocol integration in [MCP_INTERFACE_SPEC.md](MCP_INTERFACE_SPEC.md)
3. **Testing** - Comprehensive testing patterns in [TESTING.md](TESTING.md)
4. **Examples** - Runnable examples in `/docs/examples/`

---

## Reference

- **Agent Module**: `crates/ggen-ai/src/agents/`
- **Tool Registry**: `crates/ggen-ai/src/generators/validator/`
- **Signature Types**: `crates/ggen-ai/src/dspy/`
- **TTL to Signature Transpiler**: `crates/ggen-ai/src/codegen/ttl_to_signature.rs`
