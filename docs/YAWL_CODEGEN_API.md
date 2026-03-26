# YAWL Codegen API Reference

Complete API documentation for ggen-yawl with code examples.

**Version**: 0.1.0
**Last Updated**: 2026-03-26

## Table of Contents

1. [Core Types](#core-types)
2. [Main Generator](#main-generator)
3. [Ontology Loading](#ontology-loading)
4. [SPARQL Execution](#sparql-execution)
5. [Template Rendering](#template-rendering)
6. [Code Generation](#code-generation)
7. [Error Handling](#error-handling)
8. [Examples](#examples)

---

## Core Types

### YawlGenerator

Main orchestrator for the five-stage pipeline.

```rust
pub struct YawlGenerator {
    executor: ConstructExecutor,
    renderer: TemplateRenderer,
    validate_output: bool,
}
```

**Methods**:

#### `new() -> Self`

Create a generator with default configuration.

```rust
use ggen_yawl::YawlGenerator;

let generator = YawlGenerator::new();
assert!(generator.validate_output);
```

#### `with_validation(bool) -> Self`

Enable or disable output validation (chaining).

```rust
let generator = YawlGenerator::new()
    .with_validation(false);
```

#### `generate(&str) -> Result<String>`

Generate YAWL XML from ontology string.

```rust
use ggen_yawl::YawlGenerator;

let ontology = std::fs::read_to_string("schema/domain.ttl")?;
let generator = YawlGenerator::new();
let yawl_xml = generator.generate(&ontology)?;
std::fs::write("output.yawl", yawl_xml)?;
```

**Errors**:
- `Error::OntologyLoad` - Failed to parse ontology
- `Error::Sparql` - CONSTRUCT query failed
- `Error::Template` - Template rendering failed
- `Error::Validation` - Output validation failed

#### `generate_from_graph(&Graph) -> Result<String>`

Generate YAWL XML from pre-loaded RDF graph.

```rust
use ggen_yawl::{YawlGenerator, load_ontology};

let ontology_text = std::fs::read_to_string("schema/domain.ttl")?;
let graph = load_ontology(&ontology_text)?;
let generator = YawlGenerator::new();
let xml = generator.generate_from_graph(&graph)?;
```

---

## Ontology Loading

### OntologyLoader

Loads industry ontologies in multiple RDF formats.

```rust
pub struct OntologyLoader {
    base_iri: String,
    flatten_imports: bool,
}
```

**Methods**:

#### `new() -> Self`

Create loader with default settings.

```rust
use ggen_yawl::OntologyLoader;

let loader = OntologyLoader::new();
```

#### `with_base_iri(String) -> Self`

Set base IRI for resolving relative references.

```rust
let loader = OntologyLoader::new()
    .with_base_iri("http://mycompany.com/ontology/".to_string());
```

#### `with_flatten_imports(bool) -> Self`

Enable flattening of owl:imports (experimental).

```rust
let loader = OntologyLoader::new()
    .with_flatten_imports(true);
```

#### `load_from_file<P: AsRef<Path>>(P) -> Result<Graph>`

Load ontology from file path.

```rust
use ggen_yawl::{OntologyLoader, OntologyFormat};

let loader = OntologyLoader::new();
let graph = loader.load_from_file("schema/domain.ttl")?;
```

**Supported Formats** (auto-detected by extension):
- `.ttl` → Turtle
- `.rdf`, `.owl` → RDF/XML
- `.nt` → N-Triples
- `.nq` → N-Quads
- `.trig` → TriG

#### `load_from_str(&str, OntologyFormat) -> Result<Graph>`

Load ontology from string with explicit format.

```rust
use ggen_yawl::{OntologyLoader, OntologyFormat};

let ttl_content = r#"
@prefix ex: <http://example.org/> .
ex:Class1 a owl:Class .
"#;

let loader = OntologyLoader::new();
let graph = loader.load_from_str(ttl_content, OntologyFormat::Turtle)?;
```

### OntologyFormat

Supported RDF formats.

```rust
pub enum OntologyFormat {
    Turtle,
    RdfXml,
    NTriples,
    NQuads,
    TriG,
}
```

### Error Types

```rust
pub enum Error {
    OntologyLoad(String),
    Sparql(String),
    Template(String),
    Validation(String),
    Io(std::io::Error),
    // ...
}

pub type Result<T> = std::result::Result<T, Error>;
```

---

## SPARQL Execution

### ConstructExecutor

Executes SPARQL CONSTRUCT queries in dependency order.

```rust
pub struct ConstructExecutor {
    queries: Vec<Query>,
}
```

**Methods**:

#### `new() -> Self`

Create executor with default CONSTRUCT queries (6 patterns).

```rust
use ggen_yawl::ConstructExecutor;

let executor = ConstructExecutor::new();
```

#### `register_query(Query) -> &mut Self`

Add a custom CONSTRUCT query.

```rust
use ggen_yawl::ConstructExecutor;

let mut executor = ConstructExecutor::new();
executor.register_query(custom_query);
```

#### `execute_all(&Graph) -> Result<Graph>`

Execute all registered queries in dependency order.

```rust
use ggen_yawl::{ConstructExecutor, load_ontology};

let graph = load_ontology(&ontology_text)?;
let executor = ConstructExecutor::new();
let result = executor.execute_all(&graph)?;

// Result contains YAWL RDF graph
```

#### `execute_query(&Graph, &Query) -> Result<Graph>`

Execute a single CONSTRUCT query.

```rust
let result = executor.execute_query(&graph, &query)?;
```

#### `topological_sort() -> Result<Vec<String>>`

Get execution order of queries (for debugging).

```rust
let order = executor.topological_sort()?;
for (i, query_name) in order.iter().enumerate() {
    println!("{}: {}", i + 1, query_name);
}
// Output:
// 1: extract-tasks
// 2: extract-flows
// 3: cardinality-splitjoin
// 4: rules-to-conditions
// 5: multiple-instance
// 6: composite-task
```

### Query

Represents a SPARQL CONSTRUCT query.

```rust
pub struct Query {
    pub id: String,
    pub sparql: String,
    pub depends_on: Vec<String>,
}
```

**Example**:

```rust
use ggen_yawl::transform::Query;

let query = Query {
    id: "custom-task-extraction".to_string(),
    sparql: r#"
        PREFIX owl: <http://www.w3.org/2002/07/owl#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

        CONSTRUCT {
            ?task a yawl:Task ;
                yawl:taskId ?taskId ;
                yawl:taskName ?name .
        }
        WHERE {
            ?class a owl:Class ;
                rdfs:label ?name .
            BIND(SHA256(CONCAT(STR(?class), ?name)) AS ?taskId)
            BIND(IRI(CONCAT("http://unrdf.org/yawl#task/", ?taskId)) AS ?task)
        }
    "#.to_string(),
    depends_on: vec![],
};
```

---

## Template Rendering

### TemplateRenderer

Renders YAWL XML and code templates.

```rust
pub struct TemplateRenderer {
    tera: Tera,
}
```

**Methods**:

#### `new() -> Self`

Create renderer with embedded default templates.

```rust
use ggen_yawl::TemplateRenderer;

let renderer = TemplateRenderer::new();
```

#### `with_template_dir<P: AsRef<Path>>(P) -> Result<Self>`

Create renderer with custom template directory.

```rust
use ggen_yawl::TemplateRenderer;

let renderer = TemplateRenderer::with_template_dir("./custom_templates")?;
```

#### `render_yawl_xml(&TemplateContext) -> Result<String>`

Render YAWL XML from template context.

```rust
use ggen_yawl::{TemplateRenderer, template::TemplateContext};

let renderer = TemplateRenderer::new();
let context = TemplateContext {
    workflow_name: "LoanApproval".to_string(),
    ..Default::default()
};
let xml = renderer.render_yawl_xml(&context)?;
```

#### `render_yawl_xml_from_graph(&Graph) -> Result<String>`

Render YAWL XML directly from RDF graph (auto-extract context).

```rust
let xml = renderer.render_yawl_xml_from_graph(&graph)?;
```

#### `render_erlang_module(&TemplateContext) -> Result<String>`

Render Erlang module for gen_yawl engine (optional).

```rust
let erlang = renderer.render_erlang_module(&context)?;
```

#### `tera() -> &Tera`

Access underlying Tera instance for advanced customization.

```rust
let tera = renderer.tera();
// Use tera for custom template operations
```

### TemplateContext

Context data passed to templates.

```rust
pub struct TemplateContext {
    pub workflow_name: String,
    pub description: String,
    pub version: String,
    pub tasks: Vec<TaskContext>,
    pub flows: Vec<FlowContext>,
    pub input_condition: Option<ConditionContext>,
    pub output_condition: Option<ConditionContext>,
    pub variables: Vec<VariableContext>,
}
```

**Fields**:

- `workflow_name: String` - Workflow identifier
- `description: String` - Human-readable description
- `version: String` - Workflow version (e.g., "1.0.0")
- `tasks: Vec<TaskContext>` - List of workflow tasks
- `flows: Vec<FlowContext>` - Control flow edges
- `input_condition: Option<ConditionContext>` - Starting condition
- `output_condition: Option<ConditionContext>` - Ending condition
- `variables: Vec<VariableContext>` - Workflow variables

**Example**:

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
            is_auto: false,
            decomposition_id: None,
        },
        TaskContext {
            id: "t2".to_string(),
            name: "Credit Check".to_string(),
            split_type: "AND".to_string(),
            join_type: "AND".to_string(),
            is_auto: false,
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
        FlowContext {
            source: "t1".to_string(),
            target: "t2".to_string(),
            condition: None,
            predicate: None,
            is_default: false,
        },
        FlowContext {
            source: "t2".to_string(),
            target: "output".to_string(),
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

### TaskContext

Information about a single YAWL task.

```rust
pub struct TaskContext {
    pub id: String,
    pub name: String,
    pub split_type: String,      // XOR, AND, OR
    pub join_type: String,        // XOR, AND, OR
    pub is_auto: bool,            // Automatic task
    pub decomposition_id: Option<String>,
}
```

### FlowContext

Control flow connection between tasks.

```rust
pub struct FlowContext {
    pub source: String,
    pub target: String,
    pub condition: Option<String>,
    pub predicate: Option<String>,
    pub is_default: bool,
}
```

---

## Code Generation

### Rule Trait

Generic code generation rule.

```rust
pub trait Rule<Q: Queryable, T: Renderable> {
    fn execute(&self) -> Result<Vec<GeneratedFile>>;
}
```

### Queryable Trait

Executes SPARQL queries and returns bindings.

```rust
pub trait Queryable {
    fn execute(&self) -> Result<Vec<HashMap<String, String>>>;
}
```

**Example**:

```rust
use ggen_codegen::Queryable;
use std::collections::HashMap;

struct MyQuery;

impl Queryable for MyQuery {
    fn execute(&self) -> Result<Vec<HashMap<String, String>>> {
        let mut results = Vec::new();

        let mut binding = HashMap::new();
        binding.insert("className".to_string(), "MyClass".to_string());
        binding.insert("fieldCount".to_string(), "5".to_string());
        results.push(binding);

        Ok(results)
    }
}
```

### Renderable Trait

Renders code using bindings and templates.

```rust
pub trait Renderable {
    fn render(&self, bindings: &HashMap<String, String>) -> Result<String>;
}
```

**Example**:

```rust
use ggen_codegen::Renderable;
use std::collections::HashMap;

struct MyTemplate;

impl Renderable for MyTemplate {
    fn render(&self, bindings: &HashMap<String, String>) -> Result<String> {
        let class_name = bindings.get("className")
            .ok_or("Missing className")?;

        Ok(format!(
            "public class {} {{\n  // Generated class\n}}\n",
            class_name
        ))
    }
}
```

### GeneratedFile

Represents a single generated file.

```rust
pub struct GeneratedFile {
    pub path: PathBuf,
    pub content: String,
}
```

**Example**:

```rust
use ggen_codegen::GeneratedFile;
use std::path::PathBuf;

let file = GeneratedFile {
    path: PathBuf::from("src/main/java/com/example/MyClass.java"),
    content: "public class MyClass { }".to_string(),
};
```

### Rule Factory Functions

Helper functions to create rules.

#### Rule 3: JPA Entity

```rust
use ggen_yawl::codegen::create_jpa_entity_rule;

let rule = create_jpa_entity_rule()?;
let files = rule.execute()?;
for file in files {
    println!("Generated: {}", file.path.display());
    // entity/YWorkItem.java
    // entity/YTask.java
    // ...
}
```

#### Rule 4: Repository

```rust
use ggen_yawl::codegen::create_repository_rule;

let rule = create_repository_rule()?;
let files = rule.execute()?;
```

#### Rule 5: DTO

```rust
use ggen_yawl::codegen::create_dto_rule;

let rule = create_dto_rule()?;
let files = rule.execute()?;
```

#### Rule 6: Controller

```rust
use ggen_yawl::codegen::create_controller_rule;

let rule = create_controller_rule()?;
let files = rule.execute()?;
```

#### Rule 7: Enum

```rust
use ggen_yawl::codegen::create_enum_rule;

let rule = create_enum_rule()?;
let files = rule.execute()?;
```

#### Rule 8: Service

```rust
use ggen_yawl::codegen::create_service_rule;

let rule = create_service_rule()?;
let files = rule.execute()?;
```

#### Rule 9: HBM Mapping

```rust
use ggen_yawl::codegen::create_hbm_mapping_rule;

let rule = create_hbm_mapping_rule()?;
let files = rule.execute()?;
```

#### Rule 10: Jackson Serializer

```rust
use ggen_yawl::codegen::create_jackson_serializer_rule;

let rule = create_jackson_serializer_rule()?;
let files = rule.execute()?;
```

---

## Error Handling

### Result Type

All fallible operations return `Result<T>`.

```rust
pub type Result<T> = std::result::Result<T, Error>;
```

### Error Variants

```rust
pub enum Error {
    OntologyLoad(String),
    Sparql(String),
    Template(String),
    Validation(String),
    Io(std::io::Error),
    Other(String),
}

impl std::error::Error for Error { ... }
impl std::fmt::Display for Error { ... }
```

### Error Handling Patterns

**Pattern 1: Use `?` operator**

```rust
fn generate_workflow() -> Result<String> {
    let ontology = std::fs::read_to_string("schema/domain.ttl")?;
    let generator = YawlGenerator::new();
    let xml = generator.generate(&ontology)?;
    Ok(xml)
}
```

**Pattern 2: Match on errors**

```rust
use ggen_yawl::Error;

match generator.generate(&ontology) {
    Ok(xml) => println!("{}", xml),
    Err(Error::OntologyLoad(msg)) => eprintln!("Load error: {}", msg),
    Err(Error::Sparql(msg)) => eprintln!("Query error: {}", msg),
    Err(Error::Template(msg)) => eprintln!("Template error: {}", msg),
    Err(Error::Validation(msg)) => eprintln!("Validation error: {}", msg),
    Err(e) => eprintln!("Other error: {}", e),
}
```

**Pattern 3: Map errors**

```rust
let xml = generator.generate(&ontology)
    .map_err(|e| format!("Generation failed: {}", e))?;
```

---

## Examples

### Example 1: Basic YAWL Generation

```rust
use ggen_yawl::YawlGenerator;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let ontology = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:ProcessA a owl:Class ;
            rdfs:label "Process A" .

        ex:ProcessB a owl:Class ;
            rdfs:label "Process B" .

        ex:followedBy a owl:ObjectProperty ;
            rdfs:domain ex:ProcessA ;
            rdfs:range ex:ProcessB .
    "#;

    let generator = YawlGenerator::new();
    let xml = generator.generate(ontology)?;
    println!("{}", xml);

    Ok(())
}
```

### Example 2: Load Ontology File

```rust
use ggen_yawl::{YawlGenerator, OntologyLoader};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load from file
    let loader = OntologyLoader::new()
        .with_base_iri("http://mycompany.com/ontology/".to_string());

    let graph = loader.load_from_file("schema/domain.ttl")?;

    // Generate from graph
    let generator = YawlGenerator::new()
        .with_validation(true);

    let xml = generator.generate_from_graph(&graph)?;

    // Write output
    std::fs::write("output.yawl", xml)?;

    Ok(())
}
```

### Example 3: Generate Java Code (Rules 3-10)

```rust
use ggen_yawl::codegen::{
    create_jpa_entity_rule, create_repository_rule,
    create_dto_rule, create_controller_rule,
    create_enum_rule, create_service_rule,
    create_hbm_mapping_rule, create_jackson_serializer_rule,
};
use std::fs;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let output_dir = "src/main/java";
    fs::create_dir_all(output_dir)?;

    // Execute all rules
    let rules = vec![
        ("Rule 3: JPA Entity", create_jpa_entity_rule()?),
        ("Rule 4: Repository", create_repository_rule()?),
        ("Rule 5: DTO", create_dto_rule()?),
        ("Rule 6: Controller", create_controller_rule()?),
        ("Rule 7: Enum", create_enum_rule()?),
        ("Rule 8: Service", create_service_rule()?),
        ("Rule 9: HBM Mapping", create_hbm_mapping_rule()?),
        ("Rule 10: Serializer", create_jackson_serializer_rule()?),
    ];

    for (rule_name, rule) in rules {
        let files = rule.execute()?;
        println!("{}: {} files", rule_name, files.len());

        for file in files {
            let path = Path::new(output_dir).join(&file.path);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(path, file.content)?;
        }
    }

    Ok(())
}
```

### Example 4: Custom Template Rendering

```rust
use ggen_yawl::{TemplateRenderer, template::TemplateContext};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Use custom templates
    let renderer = TemplateRenderer::with_template_dir("./templates")?;

    // Build context
    let context = TemplateContext {
        workflow_name: "CustomWorkflow".to_string(),
        description: "My custom workflow".to_string(),
        version: "2.0.0".to_string(),
        tasks: vec![],
        flows: vec![],
        input_condition: None,
        output_condition: None,
        variables: vec![],
    };

    // Render
    let xml = renderer.render_yawl_xml(&context)?;
    println!("{}", xml);

    Ok(())
}
```

---

**See Also**:
- [YAWL_CODEGEN_ARCHITECTURE.md](./YAWL_CODEGEN_ARCHITECTURE.md) - Technical architecture
- [YAWL_CODEGEN_USER_GUIDE.md](./YAWL_CODEGEN_USER_GUIDE.md) - User guide and examples
- `docs/rules/` - Individual rule specifications
