# Agent-Schema Layer Architecture Reference

**Purpose**: Document the complete architecture for agent-signature integration
**Status**: Partially Implemented
**Last Updated**: 2026-01-09

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                      LLM Client (genai)                      │
│                                                              │
│  Anthropic  │  OpenAI  │  Ollama  │  DeepSeek  │  Groq     │
└────────────────────────────┬────────────────────────────────┘
                             │
                    ┌────────▼────────┐
                    │  Tool Calling   │ ← NEW: Phase 4
                    │  Middleware     │
                    └────────┬────────┘
                             │
              ┌──────────────┼──────────────┐
              │                             │
    ┌─────────▼──────────┐      ┌──────────▼──────────┐
    │   MCP Server       │      │   Tool Registry     │ ← NEW: Phase 2
    │   (Future)         │      │   (ToolRegistry)    │
    └─────────┬──────────┘      └──────────┬──────────┘
              │                             │
              │        ┌────────────────────┘
              │        │
              │    ┌───▼──────────────┐
              │    │ Validation       │ ← NEW: Phase 1
              │    │ Middleware       │
              │    └───┬──────────────┘
              │        │
              └────────┼────────┐
                       │        │
           ┌───────────▼──┐  ┌──▼──────────┐
           │   Agents     │  │   Executor  │
           │              │  │             │
           │ - CodeGen    │  │ - Validate  │
           │ - Template   │  │ - Execute   │
           │ - Validator  │  │ - Return    │
           └───────────┬──┘  └──────────────┘
                       │
        ┌──────────────┴──────────────┐
        │                             │
    ┌───▼────────────┐      ┌────────▼────┐
    │ Signature      │      │ JSON Schema  │ (ACTIVE ✅)
    │ (DSPy module)  │      │ Export       │
    └───┬────────────┘      └────────┬─────┘
        │                            │
        │ Inputs/Outputs       Type Mapping
        │ Constraints          Constraint Conv.
        │ Instructions         Schema Validation
        │
    ┌───▼──────────────┐
    │ TTL-to-Signature │ (ACTIVE ✅)
    │ Transpiler       │
    └───┬──────────────┘
        │
        │ SPARQL Queries
        │ SHACL Extraction
        │ Type Inference
        │
    ┌───▼──────────────┐
    │ RDF Store        │
    │ (Oxigraph)       │
    └──────────────────┘
```

---

## Component Definitions

### Layer 1: Schema Layer (Production ✅)

#### Signature Struct
```
Signature
├── name: String               (e.g., "FinanceDomainSelector")
├── description: String        (e.g., "Select financial domain")
├── inputs: Vec<InputField>    (User-provided data)
│   ├── name: String
│   ├── description: String
│   ├── type_annotation: String
│   └── constraints: FieldConstraints
│       ├── required: bool
│       ├── min_length: Option<usize>
│       ├── max_length: Option<usize>
│       ├── pattern: Option<String>
│       ├── enum_values: Option<Vec<String>>
│       ├── min_items: Option<usize>
│       └── max_items: Option<usize>
├── outputs: Vec<OutputField>  (Model-generated data)
└── instructions: Option<String>
```

**Key Methods**:
- `as_json_schema()` → `serde_json::Value` (JSON Schema Draft 7)
- `schema()` → `SignatureSchema` (structured metadata)
- `as_rust_struct()` → `String` (Rust struct definition)

**Supported Types**:
- Primitives: String, i32, i64, u32, u64, f32, f64, bool
- Collections: Vec<T>, Option<T>
- Containers: Vec<Vec<T>>, Option<Vec<T>>

#### JSON Schema Export
```
Signature
  ↓
Type Mapping (Rust → JSON Schema)
  ├── String → "string"
  ├── i32/i64/u32/u64 → "integer"
  ├── f32/f64 → "number"
  ├── bool → "boolean"
  └── Vec<T> → { "type": "array", "items": ... }
  ↓
Constraint Mapping
  ├── required → {"required": ["field"]}
  ├── min_length → {"minLength": 5}
  ├── max_length → {"maxLength": 100}
  ├── pattern → {"pattern": "^[a-z]+$"}
  ├── enum → {"enum": ["opt1", "opt2"]}
  ├── min_items → {"minItems": 1}
  └── max_items → {"maxItems": 10}
  ↓
Valid JSON Schema
```

### Layer 2: Code Generation (Production ✅)

#### TTL-to-Signature Pipeline
```
TTL File (Turtle)
  ↓
Parse SHACL Shapes
  ├── Find sh:targetClass
  ├── Extract sh:property shapes
  └── Load sh:description
  ↓
Extract Properties
  ├── Local name extraction (IRI → identifier)
  ├── Type inference (XSD → Rust)
  ├── Constraint parsing (sh:minLength, etc.)
  └── Description normalization
  ↓
Build Signatures
  ├── Create InputField for each property
  ├── Set constraints from SHACL
  ├── Generate default output field
  └── Combine into Signature
  ↓
Signature Objects
```

**Type Mappings** (XSD → Rust):
```
XSD DataType              → Rust Type
xsd:string               → String
xsd:integer, xsd:int     → i32
xsd:long                 → i32
xsd:float, xsd:double    → f32
xsd:boolean              → bool
[unknown]                → String (default)
```

### Layer 3: Validation (IN DEVELOPMENT 🔄)

#### SignatureValidator (New - Phase 1)
```
JSON Input
  ↓
Compare against Signature
  ├── Check required fields
  ├── Validate field types
  ├── Check constraints
  │   ├── Length constraints (strings)
  │   ├── Item constraints (arrays)
  │   ├── Pattern constraints (regex)
  │   └── Enum constraints
  └── Type coercion (if enabled)
  ↓
Result: Valid ✅ or Validation Errors ❌
```

**Validation Flow**:
```
Input JSON
  ↓
Generate JSON Schema (from Signature)
  ↓
Create JSONSchema Validator
  ↓
Validate Input against Schema
  ↓
Return Result or Errors
```

**Error Context**:
```
ValidationError {
  field: String,           // Path to error (e.g., "domain")
  value: serde_json::Value, // Actual value
  expected_type: String,    // Expected type (e.g., "string")
  constraint: Option<String>, // Constraint (e.g., "enum")
  message: String,          // Error message
}
```

### Layer 4: Tool Registry (IN DEVELOPMENT 🔄)

#### ToolRegistry (New - Phase 2)
```
ToolRegistry
├── tools: HashMap<String, ToolDefinition>
└── Methods:
    ├── register(signature) → ToolDefinition
    ├── register(tool_def) → Success
    ├── get(id) → Option<ToolDefinition>
    ├── list() → Vec<ToolDefinition>
    └── to_json() → MCP-compatible JSON
```

#### ToolDefinition
```
ToolDefinition {
  id: String,                        // Tool identifier (usually Signature name)
  name: String,                      // Human-readable name
  description: String,               // Tool description
  input_schema: serde_json::Value,   // JSON Schema for inputs
  output_schema: serde_json::Value,  // JSON Schema for outputs
  signature: Option<Signature>,      // Reference to Signature
}
```

### Layer 5: Agents (DISABLED ⚠️)

#### Agent Trait (Needs Update)
```rust
trait Agent {
  // Lifecycle
  async fn initialize()
  async fn start()
  async fn stop()
  async fn status() -> AgentStatus

  // NEW: Signature support
  fn signature() -> Option<&Signature>
  async fn validate_input(message: &AgentMessage) -> Result<()>

  // Core execution
  async fn handle_message(message: AgentMessage) -> Result<AgentMessage>
}
```

#### AgentMessage (New Type)
```
AgentMessage {
  id: Uuid,
  agent_id: Uuid,
  message_type: String,
  data: serde_json::Value,        // Tool arguments
  signature: Option<String>,       // Signature name (for validation)
  validated: bool,                 // Whether input was validated
}
```

### Layer 6: Middleware (IN DEVELOPMENT 🔄)

#### ValidationMiddleware (New - Phase 3)
```
AgentMiddleware
├── async fn process_input(message) -> Result<()>
│   ├── Find signature by name
│   ├── Validate message.data against signature
│   ├── Set validated flag
│   └── Return result or error
└── async fn process_output(message) -> Result<()>
```

**Middleware Pipeline**:
```
Incoming Request
  ↓
ValidationMiddleware (1)
  ├── Validate against Signature
  ├── Type coercion
  └── Rich error context
  ↓
Agent Execution
  ├── Process validated input
  ├── Generate output
  └── Return result
  ↓
Optional: Serialization Middleware
  ├── Convert Rust types → JSON
  └── Return JSON response
```

### Layer 7: MCP Integration (FUTURE 🔮)

#### McpServer Trait (New - Phase 4)
```rust
trait McpServer {
  fn register_tool(tool: ToolDefinition)
  fn call_tool(call: McpToolCall) -> Result<McpToolResult>
}
```

#### Tool Call Flow
```
LLM Decision: "Call FinanceSelector"
  ↓
MCP Client sends ToolCall
  {
    "tool_name": "FinanceSelector",
    "arguments": {"domain": "finops"}
  }
  ↓
MCP Server receives call
  ├── Look up tool in registry
  ├── Get associated signature
  ├── Validate arguments (ValidationMiddleware)
  ├── Coerce types
  ├── Call underlying agent
  └── Return result
  ↓
Return McpToolResult
  {
    "tool_name": "FinanceSelector",
    "success": true,
    "result": {...}
  }
  ↓
LLM processes result
```

---

## Data Flow Diagrams

### Flow 1: Signature → JSON Schema → Tool Registration

```
┌─────────────────────────────┐
│ Define Signature            │
│                             │
│ sig = Signature::new(       │
│   "MyTool",                 │
│   "Tool description"        │
│ )                           │
│ .with_input(InputField(...))│
│ .with_output(OutputField(...))
│                             │
└──────────────┬──────────────┘
               │
               │ sig.as_json_schema()
               ▼
┌──────────────────────────────┐
│ JSON Schema                  │
│                              │
│ {                            │
│   "type": "object",          │
│   "properties": {...},       │
│   "required": [...],         │
│   "description": "..."       │
│ }                            │
│                              │
└──────────────┬───────────────┘
               │
               │ ToolDefinition::from_signature()
               ▼
┌──────────────────────────────┐
│ Tool Definition              │
│                              │
│ {                            │
│   "id": "MyTool",            │
│   "name": "MyTool",          │
│   "description": "...",      │
│   "input_schema": {...},     │
│   "output_schema": {...}     │
│ }                            │
│                              │
└──────────────┬───────────────┘
               │
               │ registry.register(tool)
               ▼
┌──────────────────────────────┐
│ Tool Registry                │
│                              │
│ tools: {                     │
│   "MyTool": ToolDefinition   │
│ }                            │
│                              │
└──────────────┬───────────────┘
               │
               │ mcp_server.register_tools()
               ▼
┌──────────────────────────────┐
│ MCP Server                   │
│                              │
│ Ready for LLM tool calling   │
│                              │
└──────────────────────────────┘
```

### Flow 2: Agent Execution with Validation

```
┌─────────────────────────────┐
│ LLM Tool Call               │
│                             │
│ {                           │
│   "tool": "MyAgent",        │
│   "args": {...}             │
│ }                           │
│                             │
└──────────────┬──────────────┘
               │
               │
               ▼
┌──────────────────────────────┐
│ ValidationMiddleware         │
│ process_input()              │
│                              │
│ 1. Find Signature "MyAgent"  │
│ 2. Validate args vs schema   │
│ 3. Coerce types if needed    │
│ 4. Set validated = true      │
│                              │
└──────────────┬───────────────┘
               │
               │ ✅ Validation passed
               ▼
┌──────────────────────────────┐
│ Agent::handle_message()      │
│                              │
│ 1. Check validated flag      │
│ 2. Process input             │
│ 3. Generate output           │
│ 4. Return AgentMessage       │
│                              │
└──────────────┬───────────────┘
               │
               │
               ▼
┌──────────────────────────────┐
│ Return Result                │
│                              │
│ McpToolResult {              │
│   success: true,             │
│   result: {...}              │
│ }                            │
│                              │
└──────────────────────────────┘
```

---

## File Organization

```
crates/ggen-ai/src/
│
├── dspy/
│   ├── mod.rs                    (Module exports)
│   ├── signature.rs       ✅     (Signature struct - 741 lines)
│   ├── field.rs           ✅     (InputField, OutputField - 1300+ lines)
│   ├── validator.rs       🔄     (NEW: SignatureValidator)
│   ├── module.rs          (DSPy Module trait)
│   └── predictor.rs       (Predictor implementations)
│
├── codegen/
│   ├── mod.rs                    (Module exports)
│   ├── ttl_to_signature.rs ✅    (Legacy transpiler)
│   ├── transpiler.rs      ✅     (New transpiler with caching)
│   ├── shacl_parser.rs    ✅     (SHACL constraint parser)
│   └── metrics.rs         ✅     (Process metrics)
│
├── agents/
│   ├── mod.rs              ⚠️     (Agent traits - needs update)
│   ├── registry.rs         ⚠️     (Agent registry)
│   ├── tool_registry.rs    🔄     (NEW: Tool registration)
│   ├── middleware.rs       🔄     (NEW: Validation middleware)
│   ├── core/
│   │   └── graph_evolution.rs
│   └── (other agent implementations)
│
├── swarm/
│   ├── mod.rs              ⚠️     (Swarm framework - disabled)
│   ├── agents/             ⚠️     (Swarm agent implementations)
│   ├── coordinator.rs      ⚠️     (Orchestration)
│   └── ...
│
├── mcp/
│   └── mod.rs              🔄     (NEW: MCP types and traits)
│
└── lib.rs                   (Module exports)

tests/
├── json_schema.rs         ✅     (1,023 JSON Schema tests)
├── ttl_to_signature.rs    ✅     (40+ TTL transpiler tests)
├── signature_validator.rs 🔄     (NEW: Validator tests)
└── integration/           (NEW: End-to-end tests)
```

**Legend**:
- ✅ Production/tested
- 🔄 In development
- ⚠️ Disabled/needs fixing

---

## Type Hierarchy

```
Signature
├── inputs: Vec<InputField>
│   ├── name: String
│   ├── description: String
│   ├── type_annotation: String
│   ├── constraints: FieldConstraints
│   │   ├── required: bool
│   │   ├── min_length: Option<usize>
│   │   ├── max_length: Option<usize>
│   │   ├── min_items: Option<usize>
│   │   ├── max_items: Option<usize>
│   │   ├── pattern: Option<String>
│   │   └── enum_values: Option<Vec<String>>
│   └── metadata: FieldMetadata
│       ├── default: Option<String>
│       ├── prefix: Option<String>
│       └── examples: Option<Vec<String>>
│
└── outputs: Vec<OutputField>
    ├── (same structure as InputField)
```

---

## Deployment Architecture

### Current (Phase 0)
```
User Code
  ↓
ggen-ai Crate
  ├── Signature ✅
  ├── JSON Schema ✅
  └── TTL-to-Signature ✅
  ↓
RDF Store (Oxigraph)
```

### Phase 1 (After Validators)
```
User Code
  ↓
ggen-ai Crate
  ├── Signature ✅
  ├── Validator 🔄
  ├── JSON Schema ✅
  ├── TTL-to-Signature ✅
  └── Agents ⚠️
  ↓
RDF Store (Oxigraph)
```

### Phase 2 (Full Integration)
```
LLM Client (genai)
  ↓
Tool Calling Middleware 🔄
  ↓
MCP Server 🔄
  ├── Tool Registry 🔄
  ├── Validator 🔄
  └── Agents ✅
  ↓
ggen-ai Crate
  ├── Signature ✅
  ├── JSON Schema ✅
  ├── TTL-to-Signature ✅
  └── Codegen
  ↓
RDF Store (Oxigraph)
```

---

## Technology Stack

| Layer | Technology | Version | Status |
|-------|-----------|---------|--------|
| Schema | DSPy Signatures | Rust equiv | ✅ |
| JSON Schema | jsonschema crate | 0.18+ | 🔄 |
| Validation | serde_json | 1.0 | ✅ |
| Code Gen | Oxigraph | 0.5 | ✅ |
| Async | Tokio | 1.47 | ✅ |
| Serialization | serde | 1.0 | ✅ |
| Testing | Chicago TDD tools | 1.4.0 | ✅ |
| MCP (Future) | mcp-rs | TBD | 🔮 |

---

## Integration Points

### With genai
```rust
use genai::client::ClientBase;

// Future: Use genai's tool support
let tools = registry.list()
    .iter()
    .map(|t| signature_to_genai_tool(&t.signature))
    .collect();

let response = client.call_with_tools(prompt, tools).await?;
```

### With Anthropic SDK
```rust
// Future: Use Claude's tool_use feature
let tools = registry.list()
    .iter()
    .map(|t| create_anthropic_tool(&t.signature))
    .collect();

let response = client.create_message(messages, tools).await?;
```

### With RDF Stores
```rust
// Current: Load signatures from TTL
let store = Store::new()?;
store.load_from_file("ontology.ttl")?;

let mut transpiler = TTLToSignatureTranspiler::new();
let sigs = transpiler.build_signatures(&store)?;

for sig in sigs {
    registry.register_from_signature(&sig)?;
}
```

---

## Summary Table

| Component | Location | Status | Tests | Lines |
|-----------|----------|--------|-------|-------|
| Signature | dspy/signature.rs | ✅ | 1,023 | 741 |
| InputField | dspy/field.rs | ✅ | 100+ | 1,300+ |
| JSON Schema | dspy/signature.rs | ✅ | 1,023 | 200 |
| TTL Transpiler | codegen/ttl_to_signature.rs | ✅ | 40+ | 507 |
| SHACL Parser | codegen/shacl_parser.rs | ✅ | - | 290 |
| Agent Trait | agents/mod.rs | ⚠️ | Disabled | 293 |
| Swarm Framework | swarm/mod.rs | ⚠️ | Disabled | 1,500+ |
| SignatureValidator | dspy/validator.rs | 🔄 | TBD | ~300 |
| ToolRegistry | agents/tool_registry.rs | 🔄 | TBD | ~200 |
| ValidationMiddleware | agents/middleware.rs | 🔄 | TBD | ~150 |
| MCP Types | mcp/mod.rs | 🔄 | TBD | ~100 |
| **TOTAL** | | | | 5,294+ |

---

**Next**: Review AGENT_TOOLING_IMPLEMENTATION_ROADMAP.md for detailed implementation plan.
