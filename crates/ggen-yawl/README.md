# ggen-yawl: YAWL Workflow Generation from Industry Ontologies

`ggen-yawl` is a Rust crate that generates YAWL (Yet Another Workflow Language) workflow specifications from industry ontologies (FIBO, HL7, ISO standards) using SPARQL CONSTRUCT queries and the ggen five-stage pipeline.

## Overview

The crate implements the transformation equation `A = μ(O)` where code precipitates from RDF ontology via a five-stage deterministic pipeline:

- **μ₁ (Normalize)**: RDF validation, SHACL shapes, dependency resolution
- **μ₂ (Extract)**: SPARQL CONSTRUCT queries with 6 transformation patterns
- **μ₃ (Emit)**: Tera template rendering for YAWL XML and Erlang
- **μ₄ (Canonicalize)**: Deterministic formatting, content hashing
- **μ₅ (Receipt)**: Cryptographic proof generation, audit trail

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         Industry Ontology Layer                              │
│                     (FIBO, HL7, ISO 20022, etc.)                            │
└───────────────────────────────────────┬─────────────────────────────────────┘
                                        │ SPARQL CONSTRUCT
                                        ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                      Transformation Patterns (6)                            │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐       │
│  │ Class→Task  │  │ Property→   │  │ Cardinality │  │ Rules→      │       │
│  │             │  │ Flow        │  │ Split/Join  │  │ Conditions  │       │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘       │
│  ┌─────────────┐  ┌─────────────┐                                          │
│  │ Multiple    │  │ Composite   │                                          │
│  │ Instance    │  │ Task        │                                          │
│  └─────────────┘  └─────────────┘                                          │
└───────────────────────────────────────┬─────────────────────────────────────┘
                                        │ YAWL RDF
                                        ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         Intermediate Representation                           │
│                      (YAWL RDF Graph + Metadata)                            │
└───────────────────────────────────────┬─────────────────────────────────────┘
                                        │ Tera Templates
                                        ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                              Output Generation                              │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐                      │
│  │  YAWL XML    │  │  Erlang      │  │  Validation  │                      │
│  │  (.yawl)     │  │  (.erl)      │  │  Reports     │                      │
│  └──────────────┘  └──────────────┘  └──────────────┘                      │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Features

- **RDF Format Support**: Turtle (.ttl), RDF/XML (.rdf, .owl), N-Triples (.nt), N-Quads (.nq), TriG (.trig)
- **6 Transformation Patterns**: Complete mapping from ontology constructs to YAWL workflow patterns
- **FIBO Integration**: Specialized support for Financial Industry Business Ontology
- **Template Rendering**: Tera-based code generation with custom filters
- **CLI Integration**: Full-featured CLI commands via `ggen yawl`
- **Validation**: XML and YAWL schema validation
- **SLO Compliance**: Performance targets enforced (RDF processing <5s/1k triples)

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
ggen-yawl = "0.1.0"
```

## Quick Start

### Basic Usage

```rust
use ggen_yawl::{YawlGenerator, OntologyLoader};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load industry ontology
    let ontology = OntologyLoader::new()
        .load_from_file("fibo.ttl")?;

    // Generate YAWL workflow
    let generator = YawlGenerator::new()
        .with_validation(true);

    let yawl_xml = generator.generate_from_graph(&ontology)?;

    // Write output
    std::fs::write("output.yawl", yawl_xml)?;

    Ok(())
}
```

### CLI Usage

```bash
# Generate YAWL workflow from ontology
ggen yawl generate --ontology schema/domain.ttl --output-dir .ggen/yawl/

# Validate generated workflow
ggen yawl validate --file .ggen/yawl/workflow.yawl.xml

# Deploy to gen_yawl engine
ggen yawl deploy --workflow .ggen/yawl/workflow.yawl.xml --target vendors/gen_yawl/

# Watch mode for development
ggen yawl watch --ontology schema/domain.ttl
```

## FIBO to YAWL Transformation

### Pattern 1: Class to Task

Maps FIBO business classes to YAWL atomic tasks:

**Input (FIBO Turtle):**
```turtle
@prefix fibo: <https://spec.edmcouncil.org/fibo/ontology/> .

fibo:LoanAgreement a owl:Class ;
    rdfs:label "Loan Agreement" ;
    rdfs:comment "A contract for lending funds" .
```

**Output (YAWL XML):**
```xml
<task id="task-Loan_Agreement" name="Loan Agreement">
  <split type="XOR"/>
  <join type="XOR"/>
</task>
```

### Pattern 2: Lifecycle to Sequence

FIBO entity lifecycle maps to YAWL sequence pattern (WP1):

**Input:**
```turtle
fibo:hasStatus a owl:ObjectProperty ;
    rdfs:domain fibo:LoanAgreement ;
    rdfs:range fibo:LoanStatus .

fibo:LoanStatus a owl:Class ;
    owl:oneOf (fibo:Pending fibo:Active fibo:Closed) .
```

**Output:**
```xml
<task id="validate_loan" name="Validate Loan"/>
<flow into="validate_loan" from="input">
  <predicate>/status = 'Pending'</predicate>
</flow>
```

### Pattern 3: Multi-Obligation to Parallel Split

Multiple obligations map to AND split (WP2):

**Input:**
```turtle
fibo:requiresObligation a owl:ObjectProperty ;
    rdfs:domain fibo:LoanAgreement ;
    rdfs:range fibo:Obligation .

fibo:CreditCheck a owl:Class ;
    rdfs:subClassOf fibo:Obligation .

fibo:IdentityVerification a owl:Class ;
    rdfs:subClassOf fibo:Obligation .
```

**Output:**
```xml
<task id="credit_check" name="Credit Check">
  <split type="AND"/>
  <join type="AND"/>
</task>
<task id="identity_verification" name="Identity Verification">
  <split type="AND"/>
  <join type="AND"/>
</task>
```

### Pattern 4: Conditional to Exclusive Choice

Conditional requirements map to XOR split (WP4):

**Input:**
```turtle
fibo:condition a owl:AnnotationProperty .
fibo:LoanAgreement fibo:condition "amount > 100000"^^xsd:string .
```

**Output:**
```xml
<flow into="high_value_review" from="application_review">
  <predicate>/amount > 100000</predicate>
</flow>
<flow into="standard_processing" from="application_review"/>
```

## YAWL Workflow Patterns

The crate supports all 20 YAWL workflow patterns:

| Pattern | ID | Description | FIBO Mapping |
|---------|-----|-------------|--------------|
| Sequence | WP1 | Tasks execute in order | Entity lifecycle states |
| Parallel Split | WP2 | Split into parallel branches | Multiple obligations |
| Synchronization | WP3 | Join parallel branches | Obligation completion |
| Exclusive Choice | WP4 | Choose one branch | Conditional requirements |
| Simple Merge | WP5 | Merge exclusive branches | Default path |
| Multi-Choice | WP6 | Choose multiple branches | Alternative paths |
| Synchronizing Merge | WP7 | Sync after multi-choice | Convergence point |
| Multi-Merge | WP8 | Merge multiple branches | Aggregation |
| Discriminator | WP9 | Continue after N branches | Partial completion |
| Arbitrary Cycles | WP10 | Loop back to previous state | Retry logic |
| Implicit Termination | WP11 | End when no work remains | Process completion |
| Multiple Instances | WP12 | Create multiple task instances | Collection iteration |
| Multiple Instances w/o Sync | WP13 | Independent instances | Parallel processing |
| Deferred Choice | WP14 | Runtime choice | Ad-hoc decisions |
| State-Based Choice | WP15 | Choice based on state | State machines |
| Cancel Task | WP16 | Cancel active task | Exception handling |
| Cancel Case | WP17 | Cancel entire case | Process cancellation |

## API Documentation

### Core Types

#### `YawlGenerator`

Main workflow generator orchestrating the full pipeline.

```rust
use ggen_yawl::YawlGenerator;

let generator = YawlGenerator::new()
    .with_validation(true);

let yawl_xml = generator.generate(ontology_content)?;
```

**Methods:**
- `new()` - Create generator with default configuration
- `with_validation(bool)` - Enable/disable output validation
- `generate(&str) -> Result<String>` - Generate YAWL XML from ontology string
- `generate_from_graph(&Graph) -> Result<String>` - Generate from loaded graph

#### `OntologyLoader`

Loader for industry ontologies with format auto-detection.

```rust
use ggen_yawl::OntologyLoader;
use ggen_yawl::OntologyFormat;

let loader = OntologyLoader::new()
    .with_base_iri("http://example.org/".to_string())
    .with_flatten_imports(true);

// From file
let graph = loader.load_from_file("fibo.ttl")?;

// From string
let graph = loader.load_from_str(turtle_content, OntologyFormat::Turtle)?;
```

**Methods:**
- `new()` - Create loader with default settings
- `with_base_iri(String)` - Set base IRI for resolving references
- `with_flatten_imports(bool)` - Enable flattening of owl:imports
- `load_from_file<P: AsRef<Path>>(P) -> Result<Graph>` - Load from file path
- `load_from_str(&str, OntologyFormat) -> Result<Graph>` - Load from string

#### `ConstructExecutor`

Executes SPARQL CONSTRUCT queries in dependency order.

```rust
use ggen_yawl::ConstructExecutor;

let executor = ConstructExecutor::new();

// Execute all registered queries
let result_graph = executor.execute_all(&source_graph)?;

// Execute single query
let result = executor.execute_query(&graph, &query)?;
```

**Methods:**
- `new()` - Create executor with default queries
- `register_query(Query)` - Add a custom CONSTRUCT query
- `execute_all(&Graph) -> Result<Graph>` - Execute all queries in order
- `execute_query(&Graph, &Query) -> Result<Graph>` - Execute single query
- `topological_sort() -> Result<Vec<String>>` - Get execution order

#### `TemplateRenderer`

Renders YAWL XML and Erlang from template context.

```rust
use ggen_yawl::TemplateRenderer;
use ggen_yawl::template::TemplateContext;

let renderer = TemplateRenderer::new();

// Render from context
let xml = renderer.render_yawl_xml(&context)?;

// Render from graph
let xml = renderer.render_yawl_xml_from_graph(&graph)?;

// Render Erlang module
let erlang = renderer.render_erlang_module(&context)?;
```

**Methods:**
- `new()` - Create renderer with default templates
- `with_template_dir<P: AsRef<Path>>(P) -> Result<Self>` - Custom template directory
- `render_yawl_xml(&TemplateContext) -> Result<String>` - Render YAWL XML
- `render_yawl_xml_from_graph(&Graph) -> Result<String>` - Extract and render
- `render_erlang_module(&TemplateContext) -> Result<String>` - Render Erlang
- `tera() -> &Tera` - Access underlying Tera instance

#### `TemplateContext`

Context structure for template rendering.

```rust
use ggen_yawl::template::{TemplateContext, TaskContext, FlowContext};

let context = TemplateContext {
    workflow_name: "LoanApproval".to_string(),
    description: "Loan approval workflow".to_string(),
    version: "1.0.0".to_string(),
    tasks: vec![
        TaskContext {
            id: "t1".to_string(),
            name: "Application Review".to_string(),
            split_type: "XOR".to_string(),
            join_type: "XOR".to_string(),
            is_auto: true,
            decomposition_id: None,
        },
    ],
    flows: vec![
        FlowContext {
            source: "input".to_string(),
            target: "t1".to_string(),
            condition: None,
            predicate: None,
            is_default: true,
        },
    ],
    input_condition: None,
    output_condition: None,
    variables: vec![],
};
```

**Fields:**
- `workflow_name: String` - Workflow identifier
- `description: String` - Human-readable description
- `version: String` - Workflow version
- `tasks: Vec<TaskContext>` - Workflow tasks
- `flows: Vec<FlowContext>` - Task connections
- `input_condition: Option<ConditionContext>` - Starting condition
- `output_condition: Option<ConditionContext>` - Ending condition
- `variables: Vec<VariableContext>` - Workflow variables

### Error Handling

All operations return `Result<T, Error>`:

```rust
use ggen_yawl::Error;

match result {
    Ok(xml) => println!("Generated: {}", xml),
    Err(Error::OntologyLoad(msg)) => eprintln!("Load error: {}", msg),
    Err(Error::Sparql(msg)) => eprintln!("Query error: {}", msg),
    Err(Error::Template(msg)) => eprintln!("Template error: {}", msg),
    Err(Error::Validation(msg)) => eprintln!("Validation error: {}", msg),
    Err(e) => eprintln!("Other error: {}", e),
}
```

## CLI Reference

### ggen yawl generate

Generate YAWL workflows from an ontology.

```
USAGE:
    ggen yawl generate [OPTIONS]

OPTIONS:
    --ontology <PATH>        Ontology file path (default: schema/domain.ttl)
    --output-dir <PATH>      Output directory (default: .ggen/yawl/)
    --format <FORMAT>        Output format: xml, erlang (default: xml)
    --verbose                Show detailed generation progress
    --validate               Validate output after generation
    --watch                  Watch mode: regenerate on file changes
    --timeout <MS>           Maximum execution time in milliseconds (default: 30000)
```

**Exit Codes:**
- `0` - Success
- `1` - Ontology load error
- `3` - Generation error

### ggen yawl validate

Validate YAWL XML workflow specifications.

```
USAGE:
    ggen yawl validate [OPTIONS]

OPTIONS:
    --file <PATH>            YAWL file to validate (default: .ggen/yawl/*.yawl.xml)
    --strict                 Enable strict validation mode
    --verbose                Show detailed validation output
```

**Exit Codes:**
- `0` - Valid
- `2` - Validation error

### ggen yawl deploy

Deploy YAWL workflows to the gen_yawl Erlang engine.

```
USAGE:
    ggen yawl deploy [OPTIONS]

OPTIONS:
    --workflow <PATH>        Workflow file to deploy (default: .ggen/yawl/*.yawl.xml)
    --target <PATH>          gen_yawl installation path (default: vendors/gen_yawl/)
    --restart                Restart gen_yawl service after deployment
    --compile                Compile Erlang modules after deployment
    --verbose                Show detailed deployment output
```

**Exit Codes:**
- `0` - Deployed
- `4` - Deployment error

### ggen yawl watch

Watch ontology files for changes and auto-regenerate.

```
USAGE:
    ggen yawl watch [OPTIONS]

OPTIONS:
    --ontology <PATH>        Ontology file to watch (default: schema/domain.ttl)
    --output-dir <PATH>      Output directory (default: .ggen/yawl/)
    --debounce <MS>          Debounce delay in milliseconds (default: 500)
    --verbose                Show detailed regeneration output
```

**Exit Codes:**
- `0` - Watching stopped normally
- `5` - Watch interrupted

## Examples

### Example 1: Simple FIBO Loan Approval

```rust
use ggen_yawl::{YawlGenerator, OntologyLoader};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load FIBO loan ontology
    let ontology = r#"
        @prefix fibo: <https://spec.edmcouncil.org/fibo/ontology/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        fibo:LoanApplication a owl:Class ;
            rdfs:label "Loan Application" ;
            rdfs:comment "Customer loan application" .

        fibo:CreditCheck a owl:Class ;
            rdfs:label "Credit Check" ;
            rdfs:subClassOf fibo:LoanApplication .

        fibo:hasNextStep a owl:ObjectProperty ;
            rdfs:domain fibo:LoanApplication ;
            rdfs:range fibo:CreditCheck .
    "#;

    let loader = OntologyLoader::new();
    let graph = loader.load_from_str(ontology, ggen_yawl::OntologyFormat::Turtle)?;

    let generator = YawlGenerator::new();
    let yawl_xml = generator.generate_from_graph(&graph)?;

    println!("{}", yawl_xml);
    Ok(())
}
```

### Example 2: Custom CONSTRUCT Query

```rust
use ggen_yawl::transform::{Query, ConstructExecutor};
use ggen_core::Graph;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let graph = Graph::new()?;

    // Define custom transformation
    let query = Query::new(
        "custom_transform",
        r#"
            PREFIX fibo: <https://spec.edmcouncil.org/fibo/ontology/>
            PREFIX yawl: <http://unrdf.org/yawl#>

            CONSTRUCT {
                ?task a yawl:AtomicTask ;
                    yawl:taskId ?taskId ;
                    yawl:taskName ?name .
            }
            WHERE {
                ?cls a owl:Class ;
                    rdfs:label ?name .
                BIND(SHA256(?name) AS ?taskId)
                BIND(IRI(CONCAT(STR(yawl:), "task/", ?taskId)) AS ?task)
            }
        "#
    );

    let mut executor = ConstructExecutor::new();
    executor.register_query(query);

    let result = executor.execute_all(&graph)?;
    Ok(())
}
```

### Example 3: Custom Template Rendering

```rust
use ggen_yawl::template::{TemplateContext, ContextBuilder};
use ggen_yawl::TemplateRenderer;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let graph = ggen_core::Graph::new()?;

    // Build context from graph
    let context = ContextBuilder::new(&graph)
        .with_workflow_name("CustomWorkflow")
        .with_description("My custom workflow")
        .with_version("2.0.0")
        .build()?;

    // Render with custom template directory
    let renderer = TemplateRenderer::with_template_dir("./custom_templates")?;
    let xml = renderer.render_yawl_xml(&context)?;

    Ok(())
}
```

## Performance

SLO targets (Service Level Objectives):

| Operation | Target | Notes |
|-----------|--------|-------|
| RDF Loading (1k triples) | <5s | Includes parsing and validation |
| CONSTRUCT Execution | <2s | All 6 queries |
| Template Rendering | <1s | YAWL XML generation |
| Full Pipeline | <10s | End-to-end generation |
| Incremental Update | <2s | Cached query results |

Benchmarks are run with:

```bash
cargo bench --bench yawl_workflow_slo
```

## Testing

Run the test suite:

```bash
# Unit tests
cargo test -p ggen-yawl

# Integration tests
cargo test -p ggen-yawl --test '*'

# With output
cargo test -p ggen-yawl -- --nocapture

# Specific test
cargo test -p ggen-yawl test_extract_tasks
```

## License

MIT

## Contributing

Contributions are welcome! Please ensure:

1. All tests pass (`cargo test`)
2. Clippy is clean (`cargo clippy`)
3. Code is formatted (`cargo fmt`)
4. Documentation is updated
5. Commit messages follow conventional commits

## See Also

- [ggen-core](../ggen-core/README.md) - Core RDF and SPARQL functionality
- [ggen-cli](../ggen-cli/README.md) - CLI tool with YAWL commands
- [YAWL Specification](https://www.yawlfoundation.org/)
- [FIBO Ontology](https://spec.edmcouncil.org/fibo/)
