# A2A-RS Integration Guide

## Overview

A2A-RS (Agent-to-Agent Rust) is ggen's code generation pipeline that produces type-safe Rust code from RDF ontology specifications. It enables you to define autonomous agents, their capabilities, communication protocols, and task execution patterns in declarative RDF/Turtle format, then automatically generate production-ready Rust implementations.

### Why Generate Code from RDF?

| Aspect | Hand-written Code | RDF-Generated Code |
|--------|------------------|-------------------|
| **Consistency** | Manual synchronization required | Guaranteed by single source of truth |
| **Documentation** | Separate from code | Embedded in ontology, auto-generated |
| **Type Safety** | Manual maintenance | Derived from ontology constraints |
| **Cross-Language** | Rewrite per language | One ontology, multiple targets |
| **Validation** | Runtime errors | SHACL shapes validate at ontology level |
| **Traceability** | Code comments only | Full provenance via RDF graphs |

The formula: **A = mu(O)** - Code precipitates from Ontology through a five-stage pipeline (mu_1 to mu_5).

## The mu_1-mu_5 Pipeline

The A2A-RS code generation follows a deterministic five-stage pipeline:

```
                    .---.  mu_1  .---------.  mu_2  .---------.  mu_3  .---------.  mu_4  .----------.  mu_5  .-------.
RDF Ontology (O) -->| μ1 | ------> | μ2     | ------> | μ3     | ------> | μ4     | ------> | μ5       | ------> | Code |
                    '---'  CONSTRUCT  SELECT   Templates  Canonical Receipt Receipt Verify   (A)
                           Transform   Extract   Render    Format   Generate   Sign
```

### mu_1: CONSTRUCT - RDF Normalization

**File**: `crates/ggen-core/queries/a2a/construct-agents.rq`

The first stage transforms your RDF ontology into a normalized A2A structure. SPARQL CONSTRUCT queries create new triples that:

- Normalize agent identifiers and names
- Extract and structure skills with optional properties
- Map transports and endpoints to A2A interfaces
- Create proper relationships between agents and capabilities

**Key transformation patterns**:
```sparql
# Input: Various RDF schemas
?agent a ggen:Agent ;
    ggen:name "DataProcessor" ;
    ggen:skill ?skill .

# Output: Normalized A2A structure
?normalizedAgent a a2a:Agent ;
    a2a:agentId "abc123" ;
    a2a:name "DataProcessor" ;
    a2a:skill ?normalizedSkill .
```

### mu_2: SELECT - Data Extraction

**Files**:
- `crates/ggen-core/queries/a2a/extract-tasks.rq`
- `crates/ggen-core/queries/a2a/extract-messages.rq`
- `crates/ggen-core/queries/a2a/extract-skills.rq`
- `crates/ggen-core/queries/a2a/extract-transports.rq`

The second stage extracts structured data as JSON bindings for template rendering:

```sparql
SELECT DISTINCT
    ?taskName
    ?taskDescription
    ?taskInputType
    ?taskOutputType
    ?taskCapability
    ?taskExecutor
WHERE {
    ?task a a2a:Task .
    ?task a2a:name ?taskName .
    # ... additional bindings
}
```

**Output format**: JSON bindings compatible with Tera templates:
```json
{
  "bindings": [
    {
      "taskName": { "value": "ProcessData" },
      "taskDescription": { "value": "Processes input data" },
      "taskExecutor": { "value": "DataAgent" }
    }
  ]
}
```

### mu_3: Templates - Code Generation

**Directory**: `crates/ggen-core/templates/a2a-rs/`

Templates use the Tera templating engine to render Rust code:

| Template | Purpose | Output |
|----------|---------|--------|
| `agent.tera` | Agent domain types | `agent.rs` |
| `message.tera` | Message types | `message.rs` |
| `task.tera` | Task execution | `task.rs` |
| `transport.tera` | Communication protocols | `transport.rs` |
| `adapter.tera` | Port/Adapter pattern | `adapter.rs` |
| `lib.rs.tera` | Module exports | `lib.rs` |

**Template example** (simplified from `types.rs.tera`):
```tera
{% for type in types %}
/// {{ type.description | trim }}
#[derive({{ type.derive | join(", ") }})]
pub struct {{ type.name }} {
    {% for field in type.fields %}
    pub {{ field.name }}: {{ field.type }},
    {% endfor %}
}
{% endfor %}
```

### mu_4: Canonicalization - Format Enforcement

Ensures generated code follows project standards:
- Rustfmt formatting
- Consistent naming conventions
- Import ordering
- Documentation standards

### mu_5: Receipt - Reproducibility

Generates a cryptographic receipt for verification:

```toml
[receipt]
version = "1.0.0"
timestamp = "2025-02-08T12:00:00Z"
ontology_hash = "sha256:abc123..."
template_versions = { agent = "v1.0.0", message = "v1.0.0" }
output_hash = "sha256:def456..."
```

## Ontology Design

### Core A2A Classes

The A2A ontology defines the following core classes:

#### a2a:Agent

Represents an autonomous agent with capabilities and communication interfaces.

```turtle
@prefix a2a: <https://ggen.dev/ontology/a2a#> .

:MyAgent a a2a:Agent ;
    a2a:id "agent-001" ;
    a2a:name "DataProcessor" ;
    a2a:description "Processes CSV data and outputs JSON" ;
    a2a:version "1.0.0" ;
    a2a:protocolVersion "0.3.0" ;
    a2a:preferredTransport "HTTP" ;
    a2a:defaultInputModes "text/csv" ;
    a2a:defaultOutputModes "application/json" ;
    a2a:skills (:ProcessCSV :ValidateSchema) ;
    a2a:interface (:HttpEndpoint) ;
    a2a:capabilities (:StreamingCap) .
```

#### a2a:AgentSkill

Defines a specific capability or skill an agent can perform.

```turtle
:ProcessCSV a a2a:AgentSkill ;
    a2a:id "skill-csv-process" ;
    a2a:name "ProcessCSV" ;
    a2a:description "Reads CSV, performs transformations, outputs JSON" ;
    a2a:tags "data,transformation,etl" ;
    a2a:inputModes "text/csv" ;
    a2a:outputModes "application/json" ;
    a2a:examples "input.csv,process,output.json" .
```

#### a2a:Task

Represents a unit of work that can be assigned to an agent.

```turtle
:DataTransformTask a a2a:Task ;
    a2a:id "task-data-xform" ;
    a2a:name "DataTransformTask" ;
    a2a:description "Transform input CSV to output JSON" ;
    a2a:inputType "CSVInput" ;
    a2a:outputType "JSONOutput" ;
    a2a:capability :ProcessCSV ;
    a2a:executedBy :MyAgent .
```

#### a2a:Message

Defines message types for agent communication.

```turtle
:TaskRequestMessage a a2a:MessageType ;
    a2a:name "TaskRequest" ;
    a2a:description "Request to execute a task" ;
    a2a:inputType "TaskRequestInput" ;
    a2a:outputType "TaskResponse" ;
    a2a:mode "request-response" ;
    a2a:schema <https://schema.org/TaskRequest> .
```

#### a2a:AgentInterface

Defines communication endpoints.

```turtle
:HttpEndpoint a a2a:AgentInterface ;
    a2a:url "http://localhost:8080" ;
    a2a:transport "HTTP" .
```

#### a2a:AgentCapabilities

Feature flags and extensions.

```turtle
:StreamingCap a a2a:AgentCapabilities ;
    a2a:streaming true ;
    a2a:pushNotifications true ;
    a2a:stateTransitionHistory true ;
    a2a:extension (:StreamExt) .
```

### Property Reference

| Property | Domain | Range | Description |
|----------|--------|-------|-------------|
| `a2a:id` | Agent, Skill, Task | xsd:string | Unique identifier |
| `a2a:name` | All | xsd:string | Human-readable name |
| `a2a:description` | All | xsd:string | Detailed description |
| `a2a:version` | Agent | xsd:string | Semantic version |
| `a2a:skills` | Agent | Skill+ | Agent capabilities |
| `a2a:interface` | Agent | AgentInterface+ | Communication endpoints |
| `a2a:capabilities` | Agent | AgentCapabilities | Feature flags |
| `a2a:executesTask` | Agent | Task+ | Tasks agent can run |
| `a2a:executedBy` | Task | Agent | Task executor |

## Defining Agents in RDF/TTL

### Basic Agent Definition

Create a file at `.specify/specs/014-a2a-integration/agents/my-agent.ttl`:

```turtle
@prefix a2a: <https://ggen.dev/ontology/a2a#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# ============================================================================
# AGENT DEFINITION
# ============================================================================

:DataAnalysisAgent a a2a:Agent ;
    a2a:id "da-agent-001" ;
    a2a:name "DataAnalysisAgent" ;
    a2a:description "Performs statistical analysis on tabular data" ;
    a2a:version "1.0.0" ;
    a2a:url "https://agents.example.com/data-analysis" ;
    a2a:preferredTransport "HTTP" ;
    a2a:defaultInputModes "application/json" ;
    a2a:defaultOutputModes "application/json" ;

    # Capabilities
    a2a:capabilities :AnalysisCapabilities ;

    # Skills (what this agent can do)
    a2a:skills (
        :CalculateStatistics
        :GenerateReport
        :DetectAnomalies
    ) ;

    # Communication endpoints
    a2a:interface (
        :HttpEndpoint
        :WebSocketEndpoint
    ) ;

    # Tasks this agent can execute
    a2a:executesTask (
        :StatsAnalysisTask
        :ReportGenerationTask
    ) ;

    # Security
    a2a:securityScheme :BearerAuth .

# ============================================================================
# CAPABILITIES
# ============================================================================

:AnalysisCapabilities a a2a:AgentCapabilities ;
    a2a:streaming true ;
    a2a:pushNotifications false ;
    a2a:stateTransitionHistory true .

# ============================================================================
# SKILLS
# ============================================================================

:CalculateStatistics a a2a:AgentSkill ;
    a2a:id "skill-calc-stats" ;
    a2a:name "CalculateStatistics" ;
    a2a:description "Calculate mean, median, mode, standard deviation" ;
    a2a:tags "statistics,math,analysis" ;
    a2a:inputModes "application/json" ;
    a2a:outputModes "application/json" ;
    a2a:examples "mean,median,stddev" .

:GenerateReport a a2a:AgentSkill ;
    a2a:id "skill-gen-report" ;
    a2a:name "GenerateReport" ;
    a2a:description "Generate formatted analysis reports" ;
    a2a:inputModes "application/json" ;
    a2a:outputModes "application/pdf,text/html" ;
    a2a:examples "report.pdf,report.html" .

:DetectAnomalies a a2a:AgentSkill ;
    a2a:id "skill-detect-anomalies" ;
    a2a:name "DetectAnomalies" ;
    a2a:description "Detect outliers and anomalies using statistical methods" ;
    a2a:tags "anomaly-detection,outliers" ;
    a2a:inputModes "application/json" ;
    a2a:outputModes "application/json" .

# ============================================================================
# INTERFACES
# ============================================================================

:HttpEndpoint a a2a:AgentInterface ;
    a2a:url "http://localhost:8080/api/v1" ;
    a2a:transport "HTTP" .

:WebSocketEndpoint a a2a:AgentInterface ;
    a2a:url "ws://localhost:8080/ws" ;
    a2a:transport "WebSocket" .

# ============================================================================
# TASKS
# ============================================================================

:StatsAnalysisTask a a2a:Task ;
    a2a:id "task-stats-analysis" ;
    a2a:name "StatsAnalysisTask" ;
    a2a:description "Perform statistical analysis on dataset" ;
    a2a:inputType "Dataset" ;
    a2a:outputType "StatisticsReport" ;
    a2a:capability :CalculateStatistics ;
    a2a:executedBy :DataAnalysisAgent .

:ReportGenerationTask a a2a:Task ;
    a2a:id "task-report-gen" ;
    a2a:name "ReportGenerationTask" ;
    a2a:description "Generate formatted report from analysis results" ;
    a2a:inputType "StatisticsReport" ;
    a2a:outputType "FormattedReport" ;
    a2a:capability :GenerateReport ;
    a2a:executedBy :DataAnalysisAgent .

# ============================================================================
# SECURITY
# ============================================================================

:BearerAuth a a2a:SecurityScheme ;
    a2a:name "bearerAuth" ;
    a2a:type "httpBearer" ;
    a2a:description "Bearer token authentication" .
```

### Advanced: Agent Composition

```turtle
# ============================================================================
# AGENT COMPOSITION - Multi-skill agent with workflow
# ============================================================================

:ETLPipelineAgent a a2a:Agent ;
    a2a:id "etl-pipeline-001" ;
    a2a:name "ETLPipelineAgent" ;
    a2a:description "End-to-end ETL pipeline with validation and error handling" ;

    # Workflow definition (sequential execution)
    a2a:workflow (
        :ExtractData
        :ValidateSchema
        :TransformData
        :LoadData
    ) ;

    # Error handling
    a2a:errorHandling :RetryWithBackoff ;

    # Dependencies (skills that must complete first)
    a2a:dependencies (
        :DatabaseConnector
        :SchemaRegistry
    ) .

# Workflow steps
:ExtractData a a2a:AgentSkill ;
    a2a:name "Extract" ;
    a2a:timeout 300 ;
    a2a:retryPolicy "exponential-backoff" .

:ValidateSchema a a2a:AgentSkill ;
    a2a:name "Validate" ;
    a2a:dependsOn :ExtractData .
```

## Template Customization Guide

### Template Structure

Templates are located in `crates/ggen-core/templates/a2a-rs/`:

```
templates/a2a-rs/
├── agent.tera          # Agent domain types
├── message.tera        # Message types
├── task.tera           # Task execution
├── transport.tera      # Transport protocols
├── adapter.tera        # Port/Adapter implementations
├── port.tera           # Port trait definitions
├── types.rs.tera       # Generic type generation
├── module.rs.tera      # Module structure
└── lib.rs.tera         # Library exports
```

### Customizing Generated Types

To add custom fields to generated types, modify the corresponding template:

**Example**: Add a `metadata` field to all agents

Edit `agent.tera`:

```tera
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Agent {
    pub id: String,
    pub name: String,
    pub description: String,
    // ... existing fields ...

    // Custom field
    {% if agent.metadata %}
    pub metadata: HashMap<String, serde_json::Value>,
    {% endif %}
}
```

### Creating Custom Templates

1. Create a new template file: `crates/ggen-core/templates/a2a-rs/custom.tera`

2. Define the template structure:
```tera
// Generated by ggen from A2A ontology
// Do not edit - regenerate with: ggen sync

{% for entity in entities %}
pub struct {{ entity.name }} {
    {% for field in entity.fields %}
    pub {{ field.name }}: {{ field.rust_type }},
    {% endfor %}
}

impl {{ entity.name }} {
    pub fn new({% for field in entity.fields %}{{ field.name }}: {{ field.rust_type }}, {% endfor %}) -> Self {
        Self {
            {% for field in entity.fields %}
            {{ field.name }},
            {% endfor %}
        }
    }
}
{% endfor %}
```

3. Add a generation rule in `ggen.toml`:
```toml
[[generation.rules]]
name = "a2a-custom"
query = "crates/ggen-core/queries/a2a/custom.rq"
template = "crates/ggen-core/templates/a2a-rs/custom.tera"
output_file = "crates/a2a-generated/src/custom.rs"
mode = "Overwrite"
```

### Tera Template Syntax Reference

| Syntax | Description | Example |
|--------|-------------|---------|
| `{{ variable }}` | Print variable | `{{ agent.name }}` |
| `{% if condition %}` | Conditional | `{% if agent.streaming %}` |
| `{% for item in items %}` | Loop | `{% for skill in skills %}` |
| `{{ filter \| name }}` | Apply filter | `{{ description \| trim }}` |
| `{# comment #}` | Comment | `{# This is a comment #}` |

## Example Workflow: From Spec to Working Agent

### Step 1: Define the Ontology

Create `.specify/specs/014-a2a-integration/agents/weather-agent.ttl`:

```turtle
@prefix a2a: <https://ggen.dev/ontology/a2a#> .

:WeatherAgent a a2a:Agent ;
    a2a:id "weather-001" ;
    a2a:name "WeatherAgent" ;
    a2a:description "Fetches weather data from external APIs" ;
    a2a:skills (:GetCurrentWeather :GetForecast) ;
    a2a:interface :WeatherHttpInterface .

:GetCurrentWeather a a2a:AgentSkill ;
    a2a:name "GetCurrentWeather" ;
    a2a:description "Get current weather for a location" ;
    a2a:inputModes "application/json" ;
    a2a:outputModes "application/json" .

:WeatherHttpInterface a a2a:AgentInterface ;
    a2a:url "http://localhost:9000/weather" ;
    a2a:transport "HTTP" .
```

### Step 2: Validate the Ontology

```bash
ggen validate .specify/specs/014-a2a-integration/agents/weather-agent.ttl
```

### Step 3: Generate Code

```bash
ggen sync --audit true
```

This executes the full mu_1-mu_5 pipeline and generates:
- `crates/a2a-generated/src/agent.rs` - Agent types
- `crates/a2a-generated/src/task.rs` - Task execution types
- `crates/a2a-generated/src/message.rs` - Message types
- `crates/a2a-generated/src/lib.rs` - Module exports

### Step 4: Use the Generated Code

```rust
use a2a_generated::prelude::*;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create agent from generated factory
    let agent = AgentFactory::create_agent(
        "weather",
        "weather-001",
        "WeatherAgent"
    );

    // Create a task
    let task = Task::new(
        "task-001".to_string(),
        "GetCurrentWeather".to_string(),
        "weather".to_string(),
        serde_json::json!({
            "location": "San Francisco, CA",
            "units": "metric"
        }),
    );

    // Execute task
    let executor = DefaultTaskExecutor::new();
    let result = executor.execute(&task).await?;

    println!("Weather result: {:?}", result);

    Ok(())
}
```

### Step 5: Verify the Receipt

```bash
# Check the generation receipt
cat .ggen/receipts/latest.toml

# Verify reproducibility
ggen verify --receipt .ggen/receipts/latest.toml
```

## Troubleshooting Common Issues

### Issue: Generated Code Has Compilation Errors

**Symptom**: `cargo check` fails after `ggen sync`

**Solutions**:
1. Check that your ontology validates: `ggen validate <spec-file>`
2. Ensure all required properties are present in RDF
3. Check for duplicate IRIs in your ontology
4. Verify template syntax is valid

```bash
# Debug template rendering
ggen sync --dry-run --debug
```

### Issue: Missing Generated Types

**Symptom**: `use a2a_generated::SomeType;` fails with "not found"

**Solution**: Check that the type is defined in your ontology and the extraction query includes it.

```sparql
# Verify the type in your query
SELECT ?typeName WHERE {
    ?type a a2a:YourType .
    ?type a2a:name ?typeName .
}
```

### Issue: SPARQL Query Returns No Results

**Symptom**: Generated files are empty or missing data

**Solutions**:
1. Test your SPARQL query against the ontology directly:
```bash
ggen sparql --query crates/ggen-core/queries/a2a/extract-tasks.rq
```

2. Check prefix declarations in your query match the ontology
3. Verify property names match exactly (case-sensitive)

### Issue: Template Variables Not Populated

**Symptom**: Template renders with empty values

**Solution**: Ensure SELECT query variable names match template variable names:

```sparql
# Query must use these exact names
SELECT ?taskName ?taskDescription ...
```

```tera
{# Template must use same names #}
{{ taskName }} - {{ taskDescription }}
```

### Issue: Receipt Verification Fails

**Symptom**: `ggen verify` reports mismatches

**Solutions**:
1. Check that ontology files haven't changed
2. Verify template versions match
3. Re-run generation: `ggen sync --audit true`

```bash
# Full audit trail
ggen sync --audit true --debug 2>&1 | tee .ggen/audit.log
```

## Receipt Verification for Reproducibility

Each code generation produces a receipt that verifies reproducibility:

### Receipt Structure

```toml
[receipt]
version = "1.0.0"
timestamp = "2025-02-08T12:00:00Z"
ggen_version = "6.0.0"

[input]
ontology_files = [
    ".specify/specs/014-a2a-integration/agents/agent1.ttl",
    ".specify/specs/014-a2a-integration/agents/agent2.ttl"
]
ontology_hash = "sha256:abc123..."

[pipeline]
[[pipeline.stages]]
name = "mu1-construct"
query = "crates/ggen-core/queries/a2a/construct-agents.rq"
query_hash = "sha256:def456..."
output_triples = 1234

[[pipeline.stages]]
name = "mu2-select"
query = "crates/ggen-core/queries/a2a/extract-tasks.rq"
query_hash = "sha256:789abc..."
bindings_count = 42

[[pipeline.stages]]
name = "mu3-templates"
templates = [
    { name = "agent.tera", version = "v1.0.0", hash = "sha256:..." },
    { name = "task.tera", version = "v1.0.0", hash = "sha256:..." }
]

[[pipeline.stages]]
name = "mu4-canonical"
rustfmt_version = "1.77.0"

[[pipeline.stages]]
name = "mu5-receipt"
receipt_signature = "BASE64_ENCODED_SIGNATURE"

[output]
files = [
    "crates/a2a-generated/src/agent.rs",
    "crates/a2a-generated/src/task.rs",
    # ...
]
output_hash = "sha256:xyz789..."
```

### Verification Commands

```bash
# Verify latest generation
ggen verify

# Verify specific receipt
ggen verify --receipt .ggen/receipts/2025-02-08T12:00:00Z.toml

# Compare two receipts
ggen diff --receipt-a .ggen/receipts/v1.toml --receipt-b .ggen/receipts/v2.toml

# Re-generate from receipt (reproducibility test)
ggen sync --from-receipt .ggen/receipts/latest.toml
```

### Reproducibility Workflow

```bash
# 1. Initial generation
ggen sync --audit true
# Creates: .ggen/receipts/$(date -u +%Y-%m-%dT%H:%M:%SZ).toml

# 2. Commit receipt to version control
git add .ggen/receipts/*.toml
git commit -m "Add generation receipt for v1.0.0"

# 3. Later, verify reproduction
ggen verify --receipt .ggen/receipts/2025-02-08T12:00:00Z.toml

# 4. If verification passes, code is reproducible
# If verification fails, investigate:
# - Have ontology files changed?
# - Have templates been modified?
# - Is ggen version different?
```

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        ggen A2A-RS Pipeline                                  │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌──────────┐     ┌─────────────┐     ┌─────────────┐                     │
│  │  RDF     │────▶│  μ₁ CONSTRUCT│────▶│ Normalized  │                     │
│  │Ontology  │     │   Query     │     │    RDF      │                     │
│  └──────────┘     └─────────────┘     └─────────────┘                     │
│       │                  │                                                     │
│       │                  ▼                                                     │
│       │           ┌─────────────┐     ┌─────────────┐                     │
│       │           │   μ₂ SELECT │────▶│   JSON      │                     │
│       │           │   Query     │     │  Bindings   │                     │
│       │           └─────────────┘     └─────────────┘                     │
│       │                  │                                                     │
│       │                  ▼                                                     │
│       │           ┌─────────────┐     ┌─────────────┐                     │
│       │           │  μ₃ Tera    │────▶│   Rust      │                     │
│       │           │  Templates  │     │    Code     │                     │
│       │           └─────────────┘     └─────────────┘                     │
│       │                  │                                                     │
│       │                  ▼                                                     │
│       │           ┌─────────────┐     ┌─────────────┐                     │
│       │           │  μ₄ Format  │────▶│ Canonical   │                     │
│       │           │  (rustfmt) │     │    Code     │                     │
│       │           └─────────────┘     └─────────────┘                     │
│       │                  │                                                     │
│       │                  ▼                                                     │
│       │           ┌─────────────┐     ┌─────────────┐                     │
│       └──────────▶│  μ₅ Receipt │────▶│   Signed    │                     │
│                  │  Generate  │     │   Output    │                     │
│                  └─────────────┘     └─────────────┘                     │
│                                                                              │
│  Generated Output:                                                           │
│  ┌──────────────────────────────────────────────────────────────────┐      │
│  │ crates/a2a-generated/src/                                         │      │
│  │  ├── agent.rs      (Agent, AgentStatus, AgentCapability...)       │      │
│  │  ├── message.rs    (Message, MessageType, MessageBuilder...)      │      │
│  │  ├── task.rs       (Task, TaskStatus, TaskExecutor...)           │      │
│  │  ├── adapter.rs    (AgentAdapter, TaskAdapter, MessageAdapter...) │      │
│  │  └── lib.rs        (Module exports, prelude...)                  │      │
│  └──────────────────────────────────────────────────────────────────┘      │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Quick Reference

### Essential Commands

| Command | Purpose |
|---------|---------|
| `ggen validate <spec.ttl>` | Validate ontology |
| `ggen sync` | Run full pipeline |
| `ggen sync --audit true` | Generate with receipt |
| `ggen verify` | Verify reproducibility |
| `ggen sparql --query <file.rq>` | Test SPARQL query |
| `cargo make check` | Compile generated code |
| `cargo make test` | Run tests |

### File Locations

| Type | Path |
|------|------|
| Ontology specs | `.specify/specs/014-a2a-integration/` |
| SPARQL queries | `crates/ggen-core/queries/a2a/` |
| Templates | `crates/ggen-core/templates/a2a-rs/` |
| Generated code | `crates/a2a-generated/src/` |
| Receipts | `.ggen/receipts/` |

### Key Ontology Classes

| Class | Description |
|-------|-------------|
| `a2a:Agent` | Autonomous agent definition |
| `a2a:AgentSkill` | Agent capability/skill |
| `a2a:Task` | Executable task unit |
| `a2a:MessageType` | Communication message type |
| `a2a:AgentInterface` | Communication endpoint |
| `a2a:AgentCapabilities` | Feature flags |

## Further Reading

- [ggen User Guide](../README.md)
- [SPARQL Query Reference](./sparql-reference.md)
- [Template Development Guide](./template-guide.md)
- [A2A Ontology Specification](./ontology-spec.md)
- [RDF/Turtle Primer](https://www.w3.org/TR/turtle/)
