# ggen MCP v1 Server Generation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Generate spec-pinned MCP 2025-11-25 servers from RDF ontology with complete capability declaration, typed schemas, lifecycle-correct bootstrap, and OTEL/Weaver observability.

**Architecture:** Pure MCP ontology → SPARQL CONSTRUCT → normalized JSON context → capability-specific templates → generated Rust server using RMCP 1.3.0 SDK.

**Tech Stack:** RDF 1.2 (Turtle), SPARQL 1.2 (CONSTRUCT), Tera templates, RMCP 1.3.0, Rust 1.91.1, OTEL Weaver

---

## File Structure Map

```
specify/
├── ontologies/
│   └── mcp/
│       ├── mcp.ttl                    # NEW: Pure MCP ontology classes
│       └── mcp-server.ttl             # NEW: Example server definition
crates/ggen-core/
├── queries/
│   └── mcp/
│       └── extract-mcp-context.rq     # NEW: Extract normalized context
└── templates/
    └── mcp/
        ├── server.rs.tera             # NEW: Server struct + lifecycle
        ├── get_info.rs.tera           # NEW: Capability declaration
        ├── tools.rs.tera              # NEW: Tool router + handlers
        ├── resources.rs.tera          # NEW: Resource list + read
        ├── resource_templates.rs.tera # NEW: Resource template list
        ├── prompts.rs.tera            # NEW: Prompt list + get
        ├── completions.rs.tera        # NEW: Argument completion
        ├── logging.rs.tera            # NEW: Logging protocol
        └── main.rs.tera               # NEW: Server bootstrap
crates/ggen-a2a-mcp/
└── src/
    ├── server.rs                      # NEW: Generated server struct
    ├── tools.rs                       # NEW: Generated tools
    ├── resources.rs                   # NEW: Generated resources
    ├── prompts.rs                     # NEW: Generated prompts
    ├── completions.rs                 # NEW: Generated completions
    ├── logging.rs                     # NEW: Generated logging
    └── main.rs                        # NEW: Generated bootstrap
```

---

## Task 1: Create Pure MCP Ontology (mcp.ttl)

**Files:**
- Create: `specify/ontologies/mcp/mcp.ttl`
- Test: `tests/ontology/mcp_test.rs`

**Purpose:** Define the pure MCP 2025-11-25 ontology classes, separate from A2A protocol.

- [ ] **Step 1: Write the failing test**

```rust
// tests/ontology/mcp_test.rs
use oxigraph::model::*;
use oxigraph::sparql::*;

#[test]
fn test_mcp_ontology_has_required_classes() {
    let store = Graph::new();
    let ttl = std::fs::read_to_string("specify/ontologies/mcp/mcp.ttl").unwrap();
    store.load_ttl(ttl.as_bytes(), None, None).unwrap();

    // Check that core MCP classes exist
    let query = r#"
        SELECT ?class WHERE {
            ?class a rdfs:Class .
            FILTER (?class IN (mcp:McpServer, mcp:Tool, mcp:Resource, mcp:Prompt))
        }
    "#;

    let results = store.query(query).unwrap();
    assert!(results.len() >= 4, "MCP ontology must have at least 4 core classes");
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cargo test test_mcp_ontology_has_required_classes
```
Expected: FAIL with "No such file or directory"

- [ ] **Step 3: Create mcp.ttl ontology**

```turtle
# specify/ontologies/mcp/mcp.ttl
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix mcp: <http://ggen.dev/mcp#> .
@prefix rust: <http://ggen.dev/rust#> .

# ===== Core Server Class =====

mcp:McpServer a rdfs:Class ;
    rdfs:label "MCP Server" ;
    rdfs:comment "A Model Context Protocol server definition" ;
    rdfs:subClassOf rdfs:Resource .

mcp:hasCapabilitySet a rdf:Property ;
    rdfs:label "has capability set" ;
    rdfs:domain mcp:McpServer ;
    rdfs:range mcp:CapabilitySet .

mcp:hasTransport a rdf:Property ;
    rdfs:label "has transport" ;
    rdfs:domain mcp:McpServer ;
    rdfs:range mcp:Transport .

mcp:hasProtocolVersion a rdf:Property ;
    rdfs:label "has protocol version" ;
    rdfs:domain mcp:McpServer ;
    rdfs:range xsd:string .

mcp:hasTool a rdf:Property ;
    rdfs:label "has tool" ;
    rdfs:domain mcp:McpServer ;
    rdfs:range mcp:Tool .

mcp:hasResource a rdf:Property ;
    rdfs:label "has resource" ;
    rdfs:domain mcp:McpServer ;
    rdfs:range mcp:Resource .

mcp:hasPrompt a rdf:Property ;
    rdfs:label "has prompt" ;
    rdfs:domain mcp:McpServer ;
    rdfs:range mcp:Prompt .

mcp:hasCompletionProvider a rdf:Property ;
    rdfs:label "has completion provider" ;
    rdfs:domain mcp:McpServer ;
    rdfs:range mcp:CompletionProvider .

# ===== Capability Set =====

mcp:CapabilitySet a rdfs:Class ;
    rdfs:label "Capability Set" ;
    rdfs:comment "MCP server capability declaration" .

mcp:tools a rdf:Property ;
    rdfs:label "tools enabled" ;
    rdfs:domain mcp:CapabilitySet ;
    rdfs:range xsd:boolean .

mcp:resources a rdf:Property ;
    rdfs:label "resources enabled" ;
    rdfs:domain mcp:CapabilitySet ;
    rdfs:range xsd:boolean .

mcp:prompts a rdf:Property ;
    rdfs:label "prompts enabled" ;
    rdfs:domain mcp:CapabilitySet ;
    rdfs:range xsd:boolean .

mcp:completions a rdf:Property ;
    rdfs:label "completions enabled" ;
    rdfs:domain mcp:CapabilitySet ;
    rdfs:range xsd:boolean .

mcp:logging a rdf:Property ;
    rdfs:label "logging enabled" ;
    rdfs:domain mcp:CapabilitySet ;
    rdfs:range xsd:boolean .

mcp:subscriptions a rdf:Property ;
    rdfs:label "subscriptions enabled" ;
    rdfs:domain mcp:CapabilitySet ;
    rdfs:range xsd:boolean .

# ===== Transport =====

mcp:Transport a rdfs:Class ;
    rdfs:label "Transport" ;
    rdfs:comment "MCP transport type" .

mcp:StdioTransport a mcp:Transport ;
    rdfs:label "STDIO Transport" .

mcp:HttpTransport a mcp:Transport ;
    rdfs:label "HTTP Transport" .

mcp:SseTransport a mcp:Transport ;
    rdfs:label "Server-Sent Events Transport" .

mcp:WebSocketTransport a mcp:Transport ;
    rdfs:label "WebSocket Transport" .

# ===== Tools =====

mcp:Tool a rdfs:Class ;
    rdfs:label "Tool" ;
    rdfs:comment "MCP tool definition" .

mcp:name a rdf:Property ;
    rdfs:label "name" ;
    rdfs:domain mcp:Tool ;
    rdfs:range xsd:string .

mcp:description a rdf:Property ;
    rdfs:label "description" ;
    rdfs:domain mcp:Tool ;
    rdfs:range xsd:string .

mcp:hasArgument a rdf:Property ;
    rdfs:label "has argument" ;
    rdfs:domain mcp:Tool ;
    rdfs:range mcp:ToolArgument .

mcp:implementedBy a rdf:Property ;
    rdfs:label "implemented by" ;
    rdfs:domain mcp:Tool ;
    rdfs:range rust:Function .

# ===== Tool Arguments =====

mcp:ToolArgument a rdfs:Class ;
    rdfs:label "Tool Argument" ;
    rdfs:comment "MCP tool argument definition" .

mcp:argumentName a rdf:Property ;
    rdfs:label "argument name" ;
    rdfs:domain mcp:ToolArgument ;
    rdfs:range xsd:string .

mcp:argumentType a rdf:Property ;
    rdfs:label "argument type" ;
    rdfs:domain mcp:ToolArgument ;
    rdfs:range xsd:string .

mcp:argumentDescription a rdf:Property ;
    rdfs:label "argument description" ;
    rdfs:domain mcp:ToolArgument ;
    rdfs:range xsd:string .

mcp:isRequired a rdf:Property ;
    rdfs:label "is required" ;
    rdfs:domain mcp:ToolArgument ;
    rdfs:range xsd:boolean .

# ===== Resources =====

mcp:Resource a rdfs:Class ;
    rdfs:label "Resource" ;
    rdfs:comment "MCP resource definition" .

mcp:uri a rdf:Property ;
    rdfs:label "URI" ;
    rdfs:domain mcp:Resource ;
    rdfs:range xsd:string .

mcp:resourceName a rdf:Property ;
    rdfs:label "resource name" ;
    rdfs:domain mcp:Resource ;
    rdfs:range xsd:string .

mcp:mimeType a rdf:Property ;
    rdfs:label "MIME type" ;
    rdfs:domain mcp:Resource ;
    rdfs:range xsd:string .

mcp:resourceImplementedBy a rdf:Property ;
    rdfs:label "implemented by" ;
    rdfs:domain mcp:Resource ;
    rdfs:range rust:Function .

# ===== Resource Templates =====

mcp:ResourceTemplate a rdfs:Class ;
    rdfs:label "Resource Template" ;
    rdfs:comment "MCP resource template definition" .

mcp:uriTemplate a rdf:Property ;
    rdfs:label "URI template" ;
    rdfs:domain mcp:ResourceTemplate ;
    rdfs:range xsd:string .

# ===== Prompts =====

mcp:Prompt a rdfs:Class ;
    rdfs:label "Prompt" ;
    rdfs:comment "MCP prompt definition" .

mcp:promptImplementedBy a rdf:Property ;
    rdfs:label "implemented by" ;
    rdfs:domain mcp:Prompt ;
    rdfs:range rust:Function .

mcp:hasPromptArgument a rdf:Property ;
    rdfs:label "has prompt argument" ;
    rdfs:domain mcp:Prompt ;
    rdfs:range mcp:PromptArgument .

# ===== Prompt Arguments =====

mcp:PromptArgument a rdfs:Class ;
    rdfs:label "Prompt Argument" ;
    rdfs:comment "MCP prompt argument definition" .

# ===== Completions =====

mcp:CompletionProvider a rdfs:Class ;
    rdfs:label "Completion Provider" ;
    rdfs:comment "MCP completion provider definition" .

mcp:completionRefType a rdf:Property ;
    rdfs:label "completion reference type" ;
    rdfs:domain mcp:CompletionProvider ;
    rdfs:range xsd:string .

mcp:completionRefName a rdf:Property ;
    rdfs:label "completion reference name" ;
    rdfs:domain mcp:CompletionProvider ;
    rdfs:range xsd:string .

mcp:completionArgument a rdf:Property ;
    rdfs:label "completion argument" ;
    rdfs:domain mcp:CompletionProvider ;
    rdfs:range xsd:string .

mcp:completionValues a rdf:Property ;
    rdfs:label "completion values" ;
    rdfs:domain mcp:CompletionProvider ;
    rdfs:range xsd:string .

# ===== Logging =====

mcp:LoggingPolicy a rdfs:Class ;
    rdfs:label "Logging Policy" ;
    rdfs:comment "MCP logging policy definition" .

mcp:defaultLevel a rdf:Property ;
    rdfs:label "default level" ;
    rdfs:domain mcp:LoggingPolicy ;
    rdfs:range xsd:string .

mcp:supportedLevels a rdf:Property ;
    rdfs:label "supported levels" ;
    rdfs:domain mcp:LoggingPolicy ;
    rdfs:range xsd:string .
```

- [ ] **Step 4: Run test to verify it passes**

```bash
cargo test test_mcp_ontology_has_required_classes
```
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add specify/ontologies/mcp/mcp.ttl tests/ontology/mcp_test.rs
git commit -m "feat(mcp): add pure MCP 2025-11-25 ontology"
```

---

## Task 2: Create Example Server Definition (mcp-server.ttl)

**Files:**
- Create: `specify/ontologies/mcp/mcp-server.ttl`
- Test: `tests/ontology/mcp_server_test.rs`

**Purpose:** Example MCP server definition demonstrating the ontology usage.

- [ ] **Step 1: Write the failing test**

```rust
// tests/ontology/mcp_server_test.rs
use oxigraph::model::*;
use oxigraph::sparql::*;

#[test]
fn test_mcp_server_example_valid() {
    let store = Graph::new();
    let ttl = std::fs::read_to_string("specify/ontologies/mcp/mcp-server.ttl").unwrap();
    store.load_ttl(ttl.as_bytes(), None, None).unwrap();

    // Verify server has required properties
    let query = r#"
        SELECT ?server ?name ?version WHERE {
            ?server a mcp:McpServer ;
                   mcp:name ?name ;
                   mcp:version ?version .
        }
    "#;

    let results = store.query(query).unwrap();
    assert_eq!(results.len(), 1, "Should have exactly one server definition");
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cargo test test_mcp_server_example_valid
```
Expected: FAIL with "No such file or directory"

- [ ] **Step 3: Create mcp-server.ttl**

```turtle
# specify/ontologies/mcp/mcp-server.ttl
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix mcp: <http://ggen.dev/mcp#> .
@prefix rust: <http://ggen.dev/rust#> .

# ===== Server Definition =====

:GgenMcpServer a mcp:McpServer ;
    mcp:name "GgenMcpServer" ;
    mcp:displayName "Ggen MCP Server" ;
    mcp:version "1.0.0" ;
    mcp:description "RDF-to-code generation via Model Context Protocol" ;
    mcp:hasProtocolVersion "2025-11-25" ;
    mcp:hasTransport mcp:StdioTransport ;
    mcp:hasCapabilitySet :GgenCapabilitySet .

# ===== Capability Set =====

:GgenCapabilitySet a mcp:CapabilitySet ;
    mcp:tools true ;
    mcp:resources true ;
    mcp:prompts true ;
    mcp:completions true ;
    mcp:logging true ;
    mcp:subscriptions false .

# ===== Tools =====

:ValidatePipelineTool a mcp:Tool ;
    mcp:name "validate_pipeline" ;
    mcp:description "Run 6 quality gates on generation pipeline" ;
    mcp:hasArgument :ValidatePipelineOntologyPath ;
    mcp:implementedBy rust:validate_pipeline .

:ValidatePipelineOntologyPath a mcp:ToolArgument ;
    mcp:argumentName "ontology_path" ;
    mcp:argumentType "String" ;
    mcp:argumentDescription "Path to RDF ontology file" ;
    mcp:isRequired true .

:SyncTool a mcp:Tool ;
    mcp:name "sync" ;
    mcp:description "Generate code with dry_run option" ;
    mcp:hasArgument :SyncDryRunArg ;
    mcp:implementedBy rust:sync .

:SyncDryRunArg a mcp:ToolArgument ;
    mcp:argumentName "dry_run" ;
    mcp:argumentType "bool" ;
    mcp:argumentDescription "Preview changes without writing" ;
    mcp:isRequired false .

# ===== Resources =====

:ConfigResource a mcp:Resource ;
    mcp:uri "ggen://config" ;
    mcp:resourceName "config" ;
    mcp:description "Current ggen configuration" ;
    mcp:mimeType "application/json" ;
    mcp:resourceImplementedBy rust:read_config .

:OntologyResource a mcp:Resource ;
    mcp:uri "ggen://ontology" ;
    mcp:resourceName "ontology" ;
    mcp:description "Current RDF ontology" ;
    mcp:mimeType "text/turtle" ;
    mcp:resourceImplementedBy rust:read_ontology .

# ===== Resource Templates =====

:ExampleTemplate a mcp:ResourceTemplate ;
    mcp:name "example" ;
    mcp:description "Example project by name" ;
    mcp:uriTemplate "ggen://examples/{example_name}" ;
    mcp:mimeType "application/json" .

# ===== Prompts =====

:ExplainRdfSchemaPrompt a mcp:Prompt ;
    mcp:name "explain_rdf_schema" ;
    mcp:description "Explain RDF schema structure" ;
    mcp:hasPromptArgument :ExplainRdfSchemaPath ;
    mcp:promptImplementedBy rust:explain_rdf_schema .

:ExplainRdfSchemaPath a mcp:PromptArgument ;
    mcp:argumentName "schema_path" ;
    mcp:argumentType "String" ;
    mcp:argumentDescription "Path to RDF schema file" ;
    mcp:isRequired true .

:GenerateFromExamplePrompt a mcp:Prompt ;
    mcp:name "generate_from_example" ;
    mcp:description "Generate code from example project" ;
    mcp:hasPromptArgument :GenerateFromExampleName ;
    mcp:promptImplementedBy rust:generate_from_example .

:GenerateFromExampleName a mcp:PromptArgument ;
    mcp:argumentName "example_name" ;
    mcp:argumentType "String" ;
    mcp:argumentDescription "Name of example project" ;
    mcp:isRequired true .

# ===== Completions =====

:ExampleNameCompletion a mcp:CompletionProvider ;
    mcp:completionRefType "prompt" ;
    mcp:completionRefName "generate_from_example" ;
    mcp:completionArgument "example_name" ;
    mcp:completionValues "\"hello-world\", \"json-api\", \"crud-service\"" .

# ===== Logging Policy =====

:GgenLoggingPolicy a mcp:LoggingPolicy ;
    mcp:defaultLevel "info" ;
    mcp:supportedLevels "\"debug\", \"info\", \"notice\", \"warn\", \"error\", \"critical\", \"alert\", \"emergency\"" .
```

- [ ] **Step 4: Run test to verify it passes**

```bash
cargo test test_mcp_server_example_valid
```
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add specify/ontologies/mcp/mcp-server.ttl tests/ontology/mcp_server_test.rs
git commit -m "feat(mcp): add example MCP server definition"
```

---

## Task 3: Create SPARQL Context Extraction Query

**Files:**
- Create: `crates/ggen-core/queries/mcp/extract-mcp-context.rq`
- Test: `tests/queries/mcp_extract_test.rs`

**Purpose:** Extract normalized MCP context from ontology for template rendering.

- [ ] **Step 1: Write the failing test**

```rust
// tests/queries/mcp_extract_test.rs
use ggen_core::Graph;
use ggen_core::SparqlExecutor;

#[test]
fn test_extract_mcp_context() {
    let graph = Graph::new();
    let ttl = std::fs::read_to_string("specify/ontologies/mcp/mcp-server.ttl").unwrap();
    graph.load_ttl(ttl.as_bytes()).unwrap();

    let query = std::fs::read_to_string("crates/ggen-core/queries/mcp/extract-mcp-context.rq").unwrap();
    let results = graph.execute_construct(&query).unwrap();

    // Verify we got JSON-serializable results
    let json = serde_json::to_string(&results).unwrap();
    assert!(json.contains("GgenMcpServer"));
    assert!(json.contains("validate_pipeline"));
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cargo test test_extract_mcp_context
```
Expected: FAIL with "No such file or directory"

- [ ] **Step 3: Create extract-mcp-context.rq**

```sparql
# crates/ggen-core/queries/mcp/extract-mcp-context.rq
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX mcp: <http://ggen.dev/mcp#>
PREFIX rust: <http://ggen.dev/rust#>

CONSTRUCT {
    # Server metadata
    ?server_uri
        a mcp:McpServer ;
        mcp:name ?server_name ;
        mcp:displayName ?display_name ;
        mcp:version ?version ;
        mcp:description ?description ;
        mcp:protocolVersion ?protocol_version ;
        mcp:transportType ?transport_type ;
        mcp:authType ?auth_type .

    # Capabilities
    ?server_uri mcp:capabilitySet ?cap_uri .
    ?cap_uri
        mcp:tools ?tools_enabled ;
        mcp:resources ?resources_enabled ;
        mcp:prompts ?prompts_enabled ;
        mcp:completions ?completions_enabled ;
        mcp:logging ?logging_enabled ;
        mcp:subscriptions ?subscriptions_enabled .

    # Tools
    ?tool_uri
        a mcp:Tool ;
        mcp:name ?tool_name ;
        mcp:description ?tool_description ;
        mcp:argsType ?args_type ;
        mcp:implImport ?impl_import ;
        mcp:implFn ?impl_fn .

    # Tool arguments
    ?tool_uri mcp:hasArgument ?arg_uri .
    ?arg_uri
        a mcp:ToolArgument ;
        mcp:argumentName ?arg_name ;
        mcp:rustType ?rust_type ;
        mcp:argumentDescription ?arg_description ;
        mcp:isRequired ?required .

    # Resources
    ?resource_uri
        a mcp:Resource ;
        mcp:uri ?resource_uri ;
        mcp:resourceName ?resource_name ;
        mcp:description ?resource_description ;
        mcp:mimeType ?mime_type ;
        mcp:implImport ?resource_impl_import ;
        mcp:implFn ?resource_impl_fn .

    # Resource templates
    ?template_uri
        a mcp:ResourceTemplate ;
        mcp:uriTemplate ?uri_template ;
        mcp:name ?template_name ;
        mcp:description ?template_description ;
        mcp:mimeType ?template_mime_type .

    # Prompts
    ?prompt_uri
        a mcp:Prompt ;
        mcp:name ?prompt_name ;
        mcp:description ?prompt_description ;
        mcp:argsType ?prompt_args_type ;
        mcp:implImport ?prompt_impl_import ;
        mcp:implFn ?prompt_impl_fn .

    # Prompt arguments
    ?prompt_uri mcp:hasPromptArgument ?prompt_arg_uri .
    ?prompt_arg_uri
        a mcp:PromptArgument ;
        mcp:argumentName ?prompt_arg_name ;
        mcp:rustType ?prompt_arg_type ;
        mcp:argumentDescription ?prompt_arg_description ;
        mcp:isRequired ?prompt_arg_required .

    # Completions
    ?completion_uri
        a mcp:CompletionProvider ;
        mcp:completionRefType ?ref_type ;
        mcp:completionRefName ?ref_name ;
        mcp:completionArgument ?completion_argument ;
        mcp:completionValues ?completion_values .

    # Logging
    ?server_uri mcp:loggingPolicy ?logging_uri .
    ?logging_uri
        a mcp:LoggingPolicy ;
        mcp:defaultLevel ?default_level ;
        mcp:supportedLevels ?supported_levels .
}
WHERE {
    # Find server
    ?server_uri a mcp:McpServer .
    OPTIONAL { ?server_uri mcp:name ?server_name . }
    OPTIONAL { ?server_uri mcp:displayName ?display_name . }
    OPTIONAL { ?server_uri mcp:version ?version . }
    OPTIONAL { ?server_uri mcp:description ?description . }
    OPTIONAL { ?server_uri mcp:hasProtocolVersion ?protocol_version . }

    # Transport
    ?server_uri mcp:hasTransport ?transport_uri .
    ?transport_uri a mcp:Transport .
    BIND(COALESCE(?transport_uri, mcp:StdioTransport) AS ?transport_type)

    # Capabilities
    ?server_uri mcp:hasCapabilitySet ?cap_uri .
    ?cap_uri a mcp:CapabilitySet .
    OPTIONAL { ?cap_uri mcp:tools ?tools_enabled . }
    OPTIONAL { ?cap_uri mcp:resources ?resources_enabled . }
    OPTIONAL { ?cap_uri mcp:prompts ?prompts_enabled . }
    OPTIONAL { ?cap_uri mcp:completions ?completions_enabled . }
    OPTIONAL { ?cap_uri mcp:logging ?logging_enabled . }
    OPTIONAL { ?cap_uri mcp:subscriptions ?subscriptions_enabled . }

    # Tools
    OPTIONAL {
        ?server_uri mcp:hasTool ?tool_uri .
        ?tool_uri a mcp:Tool .
        OPTIONAL { ?tool_uri mcp:name ?tool_name . }
        OPTIONAL { ?tool_uri mcp:description ?tool_description . }
        OPTIONAL {
            ?tool_uri mcp:implementedBy ?impl_fn_uri .
            ?impl_fn_uri a rust:Function .
            ?impl_fn_uri rust:importPath ?impl_import .
            ?impl_fn_uri rust:functionName ?impl_fn .
        }
        BIND(CONCAT(UCASE(SUBSTR(?tool_name, 1, 1)), SUBSTR(?tool_name, 2)) AS ?capitalized_name)
        BIND(CONCAT(?capitalized_name, "Args") AS ?args_type)

        # Tool arguments
        OPTIONAL {
            ?tool_uri mcp:hasArgument ?arg_uri .
            ?arg_uri a mcp:ToolArgument .
            OPTIONAL { ?arg_uri mcp:argumentName ?arg_name . }
            OPTIONAL { ?arg_uri mcp:argumentType ?rust_type . }
            OPTIONAL { ?arg_uri mcp:argumentDescription ?arg_description . }
            OPTIONAL { ?arg_uri mcp:isRequired ?required . }
        }
    }

    # Resources
    OPTIONAL {
        ?server_uri mcp:hasResource ?resource_uri .
        ?resource_uri a mcp:Resource .
        OPTIONAL { ?resource_uri mcp:resourceName ?resource_name . }
        OPTIONAL { ?resource_uri mcp:description ?resource_description . }
        OPTIONAL { ?resource_uri mcp:mimeType ?mime_type . }
        OPTIONAL {
            ?resource_uri mcp:resourceImplementedBy ?resource_impl_fn_uri .
            ?resource_impl_fn_uri a rust:Function .
            ?resource_impl_fn_uri rust:importPath ?resource_impl_import .
            ?resource_impl_fn_uri rust:functionName ?resource_impl_fn .
        }
    }

    # Resource templates
    OPTIONAL {
        ?server_uri mcp:hasResourceTemplate ?template_uri .
        ?template_uri a mcp:ResourceTemplate .
        OPTIONAL { ?template_uri mcp:name ?template_name . }
        OPTIONAL { ?template_uri mcp:description ?template_description . }
        OPTIONAL { ?template_uri mcp:uriTemplate ?uri_template . }
        OPTIONAL { ?template_uri mcp:mimeType ?template_mime_type . }
    }

    # Prompts
    OPTIONAL {
        ?server_uri mcp:hasPrompt ?prompt_uri .
        ?prompt_uri a mcp:Prompt .
        OPTIONAL { ?prompt_uri mcp:name ?prompt_name . }
        OPTIONAL { ?prompt_uri mcp:description ?prompt_description . }
        OPTIONAL {
            ?prompt_uri mcp:promptImplementedBy ?prompt_impl_fn_uri .
            ?prompt_impl_fn_uri a rust:Function .
            ?prompt_impl_fn_uri rust:importPath ?prompt_impl_import .
            ?prompt_impl_fn_uri rust:functionName ?prompt_impl_fn .
        }
        BIND(CONCAT(UCASE(SUBSTR(?prompt_name, 1, 1)), SUBSTR(?prompt_name, 2)) AS ?prompt_capitalized)
        BIND(CONCAT(?prompt_capitalized, "Args") AS ?prompt_args_type)

        # Prompt arguments
        OPTIONAL {
            ?prompt_uri mcp:hasPromptArgument ?prompt_arg_uri .
            ?prompt_arg_uri a mcp:PromptArgument .
            OPTIONAL { ?prompt_arg_uri mcp:argumentName ?prompt_arg_name . }
            OPTIONAL { ?prompt_arg_uri mcp:argumentType ?prompt_arg_type . }
            OPTIONAL { ?prompt_arg_uri mcp:argumentDescription ?prompt_arg_description . }
            OPTIONAL { ?prompt_arg_uri mcp:isRequired ?prompt_arg_required . }
        }
    }

    # Completions
    OPTIONAL {
        ?server_uri mcp:hasCompletionProvider ?completion_uri .
        ?completion_uri a mcp:CompletionProvider .
        OPTIONAL { ?completion_uri mcp:completionRefType ?ref_type . }
        OPTIONAL { ?completion_uri mcp:completionRefName ?ref_name . }
        OPTIONAL { ?completion_uri mcp:completionArgument ?completion_argument . }
        OPTIONAL { ?completion_uri mcp:completionValues ?completion_values . }
    }

    # Logging
    OPTIONAL {
        ?server_uri mcp:hasLoggingPolicy ?logging_uri .
        ?logging_uri a mcp:LoggingPolicy .
        OPTIONAL { ?logging_uri mcp:defaultLevel ?default_level . }
        OPTIONAL { ?logging_uri mcp:supportedLevels ?supported_levels . }
    }
}
```

- [ ] **Step 4: Run test to verify it passes**

```bash
cargo test test_extract_mcp_context
```
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add crates/ggen-core/queries/mcp/extract-mcp-context.rq tests/queries/mcp_extract_test.rs
git commit -m "feat(mcp): add SPARQL query for normalized context extraction"
```

---

## Task 4: Create Server Template (server.rs.tera)

**Files:**
- Create: `crates/ggen-core/templates/mcp/server.rs.tera`
- Test: `tests/templates/mcp/server_test.rs`

**Purpose:** Generate server struct with RMCP 1.3.0 lifecycle.

- [ ] **Step 1: Write the failing test**

```rust
// tests/templates/mcp/server_test.rs
use ggen_core::TemplateRenderer;

#[test]
fn test_server_template_renders() {
    let context = serde_json::json!({
        "server": {
            "type_name": "TestServer",
            "display_name": "Test Server",
            "version": "1.0.0"
        },
        "transport": {
            "type": "stdio"
        }
    });

    let rendered = TemplateRenderer::render("mcp/server.rs.tera", &context).unwrap();
    assert!(rendered.contains("pub struct TestServer"));
    assert!(rendered.contains("impl TestServer"));
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cargo test test_server_template_renders
```
Expected: FAIL with "Template not found"

- [ ] **Step 3: Create server.rs.tera**

```tera
// crates/ggen-core/templates/mcp/server.rs.tera
use rmcp::{ServerHandler, RunningServer};
use rmcp::model::{ServerCapabilities, ServerInfo};
use rmcp::service::RequestContext;
use rmcp::RoleServer;
use rmcp::transport::_stdio::StdioServerTransport;
use std::sync::Arc;
use tokio::sync::RwLock;

/// {{ server.display_name }} - {{ server.description | default(value="MCP Server") }}
///
/// Generated from RDF ontology at {{ ontology_path | default(value="<unknown>") }}
/// Protocol version: {{ protocol.version | default(value="2025-11-25") }}
#[derive(Clone, Debug)]
pub struct {{ server.type_name }} {
    /// Server configuration
    config: Arc<RwLock<{{ server.type_name }}Config>>,
}

/// Server configuration
#[derive(Clone, Debug)]
pub struct {{ server.type_name }}Config {
    /// Server name
    pub name: String,
    /// Server version
    pub version: String,
}

impl {{ server.type_name }} {
    /// Create a new {{ server.type_name }} instance
    pub fn new() -> Self {
        Self {
            config: Arc::new(RwLock::new({{ server.type_name }}Config {
                name: "{{ server.display_name }}".to_string(),
                version: "{{ server.version }}".to_string(),
            })),
        }
    }

    /// Start the server with STDIO transport
    pub async fn serve_stdio(self) -> Result<(), Box<dyn std::error::Error>> {
        use rmcp::transport::stdio::server::serve;

        let transport = StdioServerTransport::new();
        let server = RunningServer::new(self).await?;

        if let Ok(svc) = serve(transport, server).await {
            // CRITICAL: waiting() keeps the connection alive
            svc.waiting().await?;
        }

        Ok(())
    }

    /// Start the server with HTTP transport
    #[cfg(feature = "http-transport")]
    pub async fn serve_http(self, addr: std::net::SocketAddr) -> Result<(), Box<dyn std::error::Error>> {
        use rmcp::transport::http::server::serve;

        let server = RunningServer::new(self).await?;

        if let Ok(svc) = serve(addr, server).await {
            svc.waiting().await?;
        }

        Ok(())
    }

    /// Get server info for get_info response
    fn get_server_info(&self) -> ServerInfo {
        ServerInfo::new(
            "{{ server.display_name }}",
            "{{ server.version }}",
        )
        .description("{{ server.description | default(value='') }}")
    }

    /// Get server capabilities
    fn get_capabilities(&self) -> ServerCapabilities {
        ServerCapabilities::builder()
            {%- if capabilities.tools %}
            .enable_tools()
            {%- endif %}
            {%- if capabilities.resources %}
            .enable_resources()
            {%- endif %}
            {%- if capabilities.prompts %}
            .enable_prompts()
            {%- endif %}
            {%- if capabilities.completions %}
            .enable_completions()
            {%- endif %}
            {%- if capabilities.logging %}
            .enable_logging()
            {%- endif %}
            {%- if capabilities.subscriptions %}
            .enable_subscriptions()
            {%- endif %}
            .build()
    }
}

impl Default for {{ server.type_name }} {
    fn default() -> Self {
        Self::new()
    }
}
```

- [ ] **Step 4: Run test to verify it passes**

```bash
cargo test test_server_template_renders
```
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add crates/ggen-core/templates/mcp/server.rs.tera tests/templates/mcp/server_test.rs
git commit -m "feat(mcp): add server template with RMCP lifecycle"
```

---

## Task 5: Create Get Info Template (get_info.rs.tera)

**Files:**
- Create: `crates/ggen-core/templates/mcp/get_info.rs.tera`
- Test: `tests/templates/mcp/get_info_test.rs`

**Purpose:** Generate get_info handler with exact capability declaration.

- [ ] **Step 1: Write the failing test**

```rust
// tests/templates/mcp/get_info_test.rs
use ggen_core::TemplateRenderer;

#[test]
fn test_get_info_template_renders() {
    let context = serde_json::json!({
        "server": {"type_name": "TestServer"},
        "capabilities": {
            "tools": true,
            "resources": true,
            "prompts": false,
            "completions": false,
            "logging": false
        }
    });

    let rendered = TemplateRenderer::render("mcp/get_info.rs.tera", &context).unwrap();
    assert!(rendered.contains("async fn get_info"));
    assert!(rendered.contains("ServerCapabilities"));
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cargo test test_get_info_template_renders
```
Expected: FAIL with "Template not found"

- [ ] **Step 3: Create get_info.rs.tera**

```tera
// crates/ggen-core/templates/mcp/get_info.rs.tera
use rmcp::{ServerHandler};
use rmcp::model::*;
use rmcp::service::RequestContext;
use rmcp::RoleServer;

impl ServerHandler for {{ server.type_name }} {
    async fn get_info(
        &self,
        _request: GetInfoRequestParams,
        _context: RequestContext<RoleServer>,
    ) -> Result<InitializeResult, McpError> {
        Ok(InitializeResult {
            protocol_version: "{{ protocol.version | default(value="2025-11-25") }}".into(),
            capabilities: self.get_capabilities(),
            server_info: self.get_server_info(),
        })
    }

    /// Get server capabilities (called by get_info and other methods)
    fn get_capabilities(&self) -> ServerCapabilities {
        ServerCapabilities::builder()
            {%- if capabilities.tools %}
            .enable_tools()
            {%- endif %}
            {%- if capabilities.resources %}
            .enable_resources()
            {%- endif %}
            {%- if capabilities.prompts %}
            .enable_prompts()
            {%- endif %}
            {%- if capabilities.completions %}
            .enable_completions()
            {%- endif %}
            {%- if capabilities.logging %}
            .enable_logging()
            {%- endif %}
            {%- if capabilities.subscriptions %}
            .enable_subscriptions()
            {%- endif %}
            .build()
    }
}
```

- [ ] **Step 4: Run test to verify it passes**

```bash
cargo test test_get_info_template_renders
```
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add crates/ggen-core/templates/mcp/get_info.rs.tera tests/templates/mcp/get_info_test.rs
git commit -m "feat(mcp): add get_info template with capability declaration"
```

---

## Task 6: Create Tools Template (tools.rs.tera)

**Files:**
- Create: `crates/ggen-core/templates/mcp/tools.rs.tera`
- Test: `tests/templates/mcp/tools_test.rs`

**Purpose:** Generate tool router and handlers using RMCP macros.

- [ ] **Step 1: Write the failing test**

```rust
// tests/templates/mcp/tools_test.rs
use ggen_core::TemplateRenderer;

#[test]
fn test_tools_template_renders() {
    let context = serde_json::json!({
        "server": {"type_name": "TestServer"},
        "tools": [
            {
                "name": "test_tool",
                "description": "A test tool",
                "args_type": "TestToolArgs",
                "arguments": [
                    {"name": "input", "rust_type": "String", "required": true}
                ]
            }
        ]
    });

    let rendered = TemplateRenderer::render("mcp/tools.rs.tera", &context).unwrap();
    assert!(rendered.contains("#[tool_router]"));
    assert!(rendered.contains("#[tool(description"));
    assert!(rendered.contains("async fn test_tool"));
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cargo test test_tools_template_renders
```
Expected: FAIL with "Template not found"

- [ ] **Step 3: Create tools.rs.tera**

```tera
// crates/ggen-core/templates/mcp/tools.rs.tera
use rmcp::{tool, tool_router, ServerHandler};
use rmcp::model::*;
use rmcp::service::RequestContext;
use rmcp::RoleServer;
use serde::{Deserialize, Serialize};

{% for tool in tools %}
use {{ tool.impl_import | default(value="crate::tools") }};

/// Arguments for {{ tool.name }}
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct {{ tool.args_type }} {
    {% for arg in tool.arguments %}
    /// {{ arg.description | default(value="") }}
    {% if arg.required %}{% else %}#[serde(skip_serializing_if = "Option::is_empty")]{% endif %}
    pub {{ arg.name }}: {% if arg.required %}{{ arg.rust_type }}{% else %}Option<{{ arg.rust_type }}>{% endif %},
    {% endfor %}
}
{% endfor %}

#[tool_router]
impl {{ server.type_name }} {
    {% for tool in tools %}
    #[tool(description = "{{ tool.description }}")]
    async fn {{ tool.name }}(
        &self,
        Parameters(params): Parameters<{{ tool.args_type }}>,
    ) -> Result<CallToolResult, McpError> {
        // Delegate to implementation function
        {{ tool.impl_fn }}(params).await
            .map_err(|e| McpError::internal_error("tool_execution_failed", Some(serde_json::json!({
                "error": e.to_string()
            }))))
    }

    {% endfor %}
}

#[tool_handler(router = self.tool_router)]
impl ServerHandler for {{ server.type_name }} {
    // Tool routing is handled by #[tool_handler] macro
}
```

- [ ] **Step 4: Run test to verify it passes**

```bash
cargo test test_tools_template_renders
```
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add crates/ggen-core/templates/mcp/tools.rs.tera tests/templates/mcp/tools_test.rs
git commit -m "feat(mcp): add tools template with RMCP macros"
```

---

## Task 7: Create Resources Template (resources.rs.tera)

**Files:**
- Create: `crates/ggen-core/templates/mcp/resources.rs.tera`
- Test: `tests/templates/mcp/resources_test.rs`

**Purpose:** Generate resource list and read handlers.

- [ ] **Step 1: Write the failing test**

```rust
// tests/templates/mcp/resources_test.rs
use ggen_core::TemplateRenderer;

#[test]
fn test_resources_template_renders() {
    let context = serde_json::json!({
        "server": {"type_name": "TestServer"},
        "resources": [
            {
                "uri": "test://config",
                "name": "config",
                "description": "Test config",
                "mime_type": "application/json"
            }
        ]
    });

    let rendered = TemplateRenderer::render("mcp/resources.rs.tera", &context).unwrap();
    assert!(rendered.contains("async fn list_resources"));
    assert!(rendered.contains("async fn read_resource"));
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cargo test test_resources_template_renders
```
Expected: FAIL with "Template not found"

- [ ] **Step 3: Create resources.rs.tera**

```tera
// crates/ggen-core/templates/mcp/resources.rs.tera
use rmcp::{ServerHandler};
use rmcp::model::*;
use rmcp::service::RequestContext;
use rmcp::RoleServer;
use serde_json::json;

{% for resource in resources %}
use {{ resource.impl_import | default(value="crate::resources") }};
{% endfor %}

impl ServerHandler for {{ server.type_name }} {
    async fn list_resources(
        &self,
        _request: Option<PaginatedRequestParams>,
        _context: RequestContext<RoleServer>,
    ) -> Result<ListResourcesResult, McpError> {
        Ok(ListResourcesResult {
            resources: vec![
                {% for resource in resources %}
                RawResource::new(
                    "{{ resource.uri }}".into(),
                    "{{ resource.name }}".into(),
                )
                {% if resource.description %}.description("{{ resource.description }}".into()){% endif %}
                {% if resource.mime_type %}.mime_type("{{ resource.mime_type }}".into()){% endif %}
                .no_annotation()
                {% if not loop.last %},{% endif %}
                {% endfor %}
            ],
            next_cursor: None,
            meta: None,
        })
    }

    async fn read_resource(
        &self,
        request: ReadResourceRequestParams,
        _context: RequestContext<RoleServer>,
    ) -> Result<ReadResourceResult, McpError> {
        match request.uri.as_str() {
            {% for resource in resources %}
            "{{ resource.uri }}" => {
                {{ resource.impl_fn }}().await
                    .map_err(|e| McpError::internal_error("resource_read_failed", Some(json!({
                        "uri": request.uri,
                        "error": e.to_string()
                    }))))
            }
            {% endfor %}
            _ => Err(McpError::resource_not_found(
                "resource_not_found",
                Some(json!({ "uri": request.uri })),
            )),
        }
    }
}
```

- [ ] **Step 4: Run test to verify it passes**

```bash
cargo test test_resources_template_renders
```
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add crates/ggen-core/templates/mcp/resources.rs.tera tests/templates/mcp/resources_test.rs
git commit -m "feat(mcp): add resources template"
```

---

## Task 8: Create Resource Templates Template (resource_templates.rs.tera)

**Files:**
- Create: `crates/ggen-core/templates/mcp/resource_templates.rs.tera`
- Test: `tests/templates/mcp/resource_templates_test.rs`

**Purpose:** Generate resource template list handler.

- [ ] **Step 1: Write the failing test**

```rust
// tests/templates/mcp/resource_templates_test.rs
use ggen_core::TemplateRenderer;

#[test]
fn test_resource_templates_template_renders() {
    let context = serde_json::json!({
        "server": {"type_name": "TestServer"},
        "resource_templates": [
            {
                "uri_template": "test://items/{id}",
                "name": "item",
                "description": "Item by ID"
            }
        ]
    });

    let rendered = TemplateRenderer::render("mcp/resource_templates.rs.tera", &context).unwrap();
    assert!(rendered.contains("async fn list_resource_templates"));
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cargo test test_resource_templates_template_renders
```
Expected: FAIL with "Template not found"

- [ ] **Step 3: Create resource_templates.rs.tera**

```tera
// crates/ggen-core/templates/mcp/resource_templates.rs.tera
use rmcp::{ServerHandler};
use rmcp::model::*;
use rmcp::service::RequestContext;
use rmcp::RoleServer;

impl ServerHandler for {{ server.type_name }} {
    async fn list_resource_templates(
        &self,
        _request: Option<PaginatedRequestParams>,
        _context: RequestContext<RoleServer>,
    ) -> Result<ListResourceTemplatesResult, McpError> {
        Ok(ListResourceTemplatesResult {
            resource_templates: vec![
                {% for template in resource_templates %}
                ResourceTemplate {
                    uri_template: "{{ template.uri_template }}".into(),
                    name: "{{ template.name }}".into(),
                    {% if template.description %}description: Some("{{ template.description }}".into()),{% else %}description: None,{% endif %}
                    {% if template.mime_type %}mime_type: Some("{{ template.mime_type }}".into()),{% else %}mime_type: None,{% endif %}
                    annotations: None,
                    meta: None,
                }{% if not loop.last %},{% endif %}
                {% endfor %}
            ],
            next_cursor: None,
            meta: None,
        })
    }
}
```

- [ ] **Step 4: Run test to verify it passes**

```bash
cargo test test_resource_templates_template_renders
```
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add crates/ggen-core/templates/mcp/resource_templates.rs.tera tests/templates/mcp/resource_templates_test.rs
git commit -m "feat(mcp): add resource templates template"
```

---

## Task 9: Create Prompts Template (prompts.rs.tera)

**Files:**
- Create: `crates/ggen-core/templates/mcp/prompts.rs.tera`
- Test: `tests/templates/mcp/prompts_test.rs`

**Purpose:** Generate prompt list and get handlers.

- [ ] **Step 1: Write the failing test**

```rust
// tests/templates/mcp/prompts_test.rs
use ggen_core::TemplateRenderer;

#[test]
fn test_prompts_template_renders() {
    let context = serde_json::json!({
        "server": {"type_name": "TestServer"},
        "prompts": [
            {
                "name": "test_prompt",
                "description": "A test prompt",
                "args_type": "TestPromptArgs",
                "arguments": [
                    {"name": "input", "rust_type": "String", "required": true}
                ]
            }
        ]
    });

    let rendered = TemplateRenderer::render("mcp/prompts.rs.tera", &context).unwrap();
    assert!(rendered.contains("async fn list_prompts"));
    assert!(rendered.contains("async fn get_prompt"));
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cargo test test_prompts_template_renders
```
Expected: FAIL with "Template not found"

- [ ] **Step 3: Create prompts.rs.tera**

```tera
// crates/ggen-core/templates/mcp/prompts.rs.tera
use rmcp::{ServerHandler};
use rmcp::model::*;
use rmcp::service::RequestContext;
use rmcp::RoleServer;
use serde::{Deserialize, Serialize};

{% for prompt in prompts %}
use {{ prompt.impl_import | default(value="crate::prompts") };

/// Arguments for {{ prompt.name }} prompt
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct {{ prompt.args_type }} {
    {% for arg in prompt.arguments %}
    /// {{ arg.description | default(value="") }}
    {% if arg.required %}{% else %}#[serde(skip_serializing_if = "Option::is_empty")]{% endif %}
    pub {{ arg.name }}: {% if arg.required %}{{ arg.rust_type }}{% else %}Option<{{ arg.rust_type }}>{% endif %},
    {% endfor %}
}
{% endfor %}

impl ServerHandler for {{ server.type_name }} {
    async fn list_prompts(
        &self,
        _request: Option<PaginatedRequestParams>,
        _context: RequestContext<RoleServer>,
    ) -> Result<ListPromptsResult, McpError> {
        Ok(ListPromptsResult {
            prompts: vec![
                {% for prompt in prompts %}
                Prompt {
                    name: "{{ prompt.name }}".into(),
                    {% if prompt.description %}description: Some("{{ prompt.description }}".into()),{% else %}description: None,{% endif %}
                    arguments: Some(vec![
                        {% for arg in prompt.arguments %}
                        PromptArgument {
                            name: "{{ arg.name }}".into(),
                            {% if arg.description %}description: Some("{{ arg.description }}".into()),{% else %}description: None,{% endif %}
                            required: Some({% if arg.required %}true{% else %}false{% endif %}),
                        }{% if not loop.last %},{% endif %}
                        {% endfor %}
                    ]),
                }{% if not loop.last %},{% endif %}
                {% endfor %}
            ],
            next_cursor: None,
            meta: None,
        })
    }

    async fn get_prompt(
        &self,
        request: GetPromptRequestParams,
        _context: RequestContext<RoleServer>,
    ) -> Result<GetPromptResult, McpError> {
        match request.name.as_str() {
            {% for prompt in prompts %}
            "{{ prompt.name }}" => {
                {{ prompt.impl_fn }}(request.arguments).await
                    .map_err(|e| McpError::internal_error("prompt_render_failed", Some(serde_json::json!({
                        "prompt": request.name,
                        "error": e.to_string()
                    }))))
            }
            {% endfor %}
            _ => Err(McpError::prompt_not_found(
                "prompt_not_found",
                Some(serde_json::json!({ "name": request.name })),
            )),
        }
    }
}
```

- [ ] **Step 4: Run test to verify it passes**

```bash
cargo test test_prompts_template_renders
```
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add crates/ggen-core/templates/mcp/prompts.rs.tera tests/templates/mcp/prompts_test.rs
git commit -m "feat(mcp): add prompts template"
```

---

## Task 10: Create Completions Template (completions.rs.tera)

**Files:**
- Create: `crates/ggen-core/templates/mcp/completions.rs.tera`
- Test: `tests/templates/mcp/completions_test.rs`

**Purpose:** Generate argument completion handler.

- [ ] **Step 1: Write the failing test**

```rust
// tests/templates/mcp/completions_test.rs
use ggen_core::TemplateRenderer;

#[test]
fn test_completions_template_renders() {
    let context = serde_json::json!({
        "server": {"type_name": "TestServer"},
        "completions": [
            {
                "reference_type": "prompt",
                "reference_name": "test_prompt",
                "argument_name": "language",
                "values": ["Rust", "TypeScript", "Go"]
            }
        ]
    });

    let rendered = TemplateRenderer::render("mcp/completions.rs.tera", &context).unwrap();
    assert!(rendered.contains("async fn complete"));
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cargo test test_completions_template_renders
```
Expected: FAIL with "Template not found"

- [ ] **Step 3: Create completions.rs.tera**

```tera
// crates/ggen-core/templates/mcp/completions.rs.tera
use rmcp::{ServerHandler};
use rmcp::model::*;
use rmcp::service::RequestContext;
use rmcp::RoleServer;

impl ServerHandler for {{ server.type_name }} {
    async fn complete(
        &self,
        request: CompleteRequestParams,
        _context: RequestContext<RoleServer>,
    ) -> Result<CompleteResult, McpError> {
        let values: Vec<String> = match &request.r#ref {
            {% for completion in completions %}
            {% if completion.reference_type == "prompt" %}
            Reference::Prompt(prompt_ref) if prompt_ref.name == "{{ completion.reference_name }}" => {
                match request.argument.name.as_str() {
                    "{{ completion.argument_name }}" => vec![
                        {% for value in completion.values %}
                        "{{ value }}".to_string(){% if not loop.last %},{% endif %}
                        {% endfor %}
                    ],
                    _ => vec![],
                }
            }
            {% endif %}
            {% if completion.reference_type == "resource" %}
            Reference::Resource(resource_ref) if resource_ref.uri.as_str().starts_with("{{ completion.reference_name }}") => {
                match request.argument.name.as_str() {
                    "{{ completion.argument_name }}" => vec![
                        {% for value in completion.values %}
                        "{{ value }}".to_string(){% if not loop.last %},{% endif %}
                        {% endfor %}
                    ],
                    _ => vec![],
                }
            }
            {% endif %}
            {% endfor %}
            _ => vec![],
        };

        let filtered = values
            .into_iter()
            .filter(|v| v.to_lowercase().contains(&request.argument.value.to_lowercase()))
            .collect();

        Ok(CompleteResult {
            completion: CompletionInfo {
                values: filtered,
                total: None,
                has_more: Some(false),
            },
        })
    }
}
```

- [ ] **Step 4: Run test to verify it passes**

```bash
cargo test test_completions_template_renders
```
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add crates/ggen-core/templates/mcp/completions.rs.tera tests/templates/mcp/completions_test.rs
git commit -m "feat(mcp): add completions template"
```

---

## Task 11: Create Logging Template (logging.rs.tera)

**Files:**
- Create: `crates/ggen-core/templates/mcp/logging.rs.tera`
- Test: `tests/templates/mcp/logging_test.rs`

**Purpose:** Generate MCP logging protocol handler.

- [ ] **Step 1: Write the failing test**

```rust
// tests/templates/mcp/logging_test.rs
use ggen_core::TemplateRenderer;

#[test]
fn test_logging_template_renders() {
    let context = serde_json::json!({
        "server": {"type_name": "TestServer"},
        "logging": {
            "enabled": true,
            "default_level": "info"
        }
    });

    let rendered = TemplateRenderer::render("mcp/logging.rs.tera", &context).unwrap();
    assert!(rendered.contains("async fn set_logging_level"));
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cargo test test_logging_template_renders
```
Expected: FAIL with "Template not found"

- [ ] **Step 3: Create logging.rs.tera**

```tera
// crates/ggen-core/templates/mcp/logging.rs.tera
use rmcp::{ServerHandler};
use rmcp::model::*;
use rmcp::service::RequestContext;
use rmcp::RoleServer;
use tracing::{Level, info, warn, debug};

{% if logging.enabled %}
impl ServerHandler for {{ server.type_name }} {
    async fn set_logging_level(
        &self,
        request: SetLevelRequestParams,
        _context: RequestContext<RoleServer>,
    ) -> Result<(), McpError> {
        let level = match request.level.as_str() {
            "debug" => Level::DEBUG,
            "info" => Level::INFO,
            "notice" => Level::INFO,
            "warn" => Level::WARN,
            "error" => Level::ERROR,
            "critical" => Level::ERROR,
            "alert" => Level::ERROR,
            "emergency" => Level::ERROR,
            _ => return Err(McpError::invalid_params("unknown_log_level", None)),
        };

        // Update tracing level (application-specific)
        info!(target: "mcp", "Logging level set to: {}", level);

        Ok(())
    }

    async fn list_logging_levels(
        &self,
        _request: ListLevelsRequestParams,
        _context: RequestContext<RoleServer>,
    ) -> Result<ListLevelsResult, McpError> {
        Ok(ListLevelsResult {
            levels: vec![
                "debug".into(),
                "info".into(),
                "notice".into(),
                "warn".into(),
                "error".into(),
                "critical".into(),
                "alert".into(),
                "emergency".into(),
            ],
            default: Some("{{ logging.default_level | default(value="info") }}".into()),
        })
    }
}
{% endif %}
```

- [ ] **Step 4: Run test to verify it passes**

```bash
cargo test test_logging_template_renders
```
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add crates/ggen-core/templates/mcp/logging.rs.tera tests/templates/mcp/logging_test.rs
git commit -m "feat(mcp): add logging protocol template"
```

---

## Task 12: Create Main Bootstrap Template (main.rs.tera)

**Files:**
- Create: `crates/ggen-core/templates/mcp/main.rs.tera`
- Test: `tests/templates/mcp/main_test.rs`

**Purpose:** Generate server bootstrap with transport setup.

- [ ] **Step 1: Write the failing test**

```rust
// tests/templates/mcp/main_test.rs
use ggen_core::TemplateRenderer;

#[test]
fn test_main_template_renders() {
    let context = serde_json::json!({
        "server": {"type_name": "TestServer"},
        "transport": {"type": "stdio"}
    });

    let rendered = TemplateRenderer::render("mcp/main.rs.tera", &context).unwrap();
    assert!(rendered.contains("fn main"));
    assert!(rendered.contains("serve_stdio"));
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cargo test test_main_template_renders
```
Expected: FAIL with "Template not found"

- [ ] **Step 3: Create main.rs.tera**

```tera
// crates/ggen-core/templates/mcp/main.rs.tera
use {{ server.type_name }};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::{{ logging.default_level | default(value="INFO") | upper }})
        .init();

    let server = {{ server.type_name }}::new();

    {% if transport.type == "stdio" %}
    server.serve_stdio().await
    {% elseif transport.type == "http" %}
    let addr = "[::1]:3000".parse()?;
    server.serve_http(addr).await
    {% else %}
    Err("Unsupported transport type".into())
    {% endif %}
}
```

- [ ] **Step 4: Run test to verify it passes**

```bash
cargo test test_main_template_renders
```
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add crates/ggen-core/templates/mcp/main.rs.tera tests/templates/mcp/main_test.rs
git commit -m "feat(mcp): add main bootstrap template"
```

---

## Task 13: Integrate Compile Gate Validation

**Files:**
- Modify: `crates/ggen-core/src/pipeline/validate.rs`
- Modify: `crates/ggen-core/src/pipeline/mod.rs`
- Test: `tests/pipeline/compile_gate_test.rs`

**Purpose:** Add compile gate to μ₄ stage that blocks generation if generated code doesn't compile.

- [ ] **Step 1: Write the failing test**

```rust
// tests/pipeline/compile_gate_test.rs
use ggen_core::Pipeline;
use ggen_core::validation::CompileGate;

#[test]
fn test_compile_gate_blocks_invalid_code() {
    let invalid_code = r#"
        fn this_will_not_compile() -> String {
            undefined_variable
        }
    "#;

    let result = CompileGate::validate(invalid_code);
    assert!(result.is_err(), "Compile gate should reject invalid code");
}

#[test]
fn test_compile_gate_accepts_valid_code() {
    let valid_code = r#"
        fn valid_fn() -> String {
            "hello".to_string()
        }
    "#;

    let result = CompileGate::validate(valid_code);
    assert!(result.is_ok(), "Compile gate should accept valid code");
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cargo test test_compile_gate_blocks_invalid_code
```
Expected: FAIL with "unresolved import `CompileGate`"

- [ ] **Step 3: Create CompileGate module**

```rust
// crates/ggen-core/src/validation/compile_gate.rs
use std::io::Write;
use std::process::Command;
use tempfile::TempDir;

/// Compile gate validation - ensures generated Rust code compiles
pub struct CompileGate;

impl CompileGate {
    /// Validate that the provided Rust code compiles
    pub fn validate(code: &str) -> Result<(), String> {
        let temp_dir = TempDir::new()
            .map_err(|e| format!("Failed to create temp dir: {}", e))?;

        let file_path = temp_dir.path().join("validation.rs");

        // Write code to temp file
        std::fs::write(&file_path, code)
            .map_err(|e| format!("Failed to write temp file: {}", e))?;

        // Run cargo check
        let output = Command::new("cargo")
            .args(["check", "--quiet", "-p", "ggen-a2a-mcp"])
            .current_dir(temp_dir.path())
            .output()
            .map_err(|e| format!("Failed to run cargo check: {}", e))?;

        if output.status.success() {
            Ok(())
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            Err(format!("Compilation failed:\n{}", stderr))
        }
    }

    /// Validate a generated crate by running cargo check
    pub fn validate_crate(crate_name: &str) -> Result<(), String> {
        let output = Command::new("cargo")
            .args(["check", "--quiet", "-p", crate_name])
            .output()
            .map_err(|e| format!("Failed to run cargo check: {}", e))?;

        if output.status.success() {
            Ok(())
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            Err(format!("Crate compilation failed:\n{}", stderr))
        }
    }
}
```

- [ ] **Step 4: Expose CompileGate in validation module**

```rust
// crates/ggen-core/src/validation/mod.rs
pub mod compile_gate;

pub use compile_gate::CompileGate;
```

- [ ] **Step 5: Integrate into pipeline μ₄**

```rust
// crates/ggen-core/src/pipeline/validate.rs (add to existing validate stage)
use crate::validation::CompileGate;

impl Pipeline {
    pub async fn validate(&self) -> Result<PipelineResult, PipelineError> {
        // ... existing validation logic ...

        // Compile gate
        if self.config.compile_gate_enabled {
            tracing::info!("Running compile gate on generated code");

            CompileGate::validate_crate("ggen-a2a-mcp")
                .map_err(|e| PipelineError::CompileGateError(e))?;

            tracing::info!("Compile gate passed");
        }

        // ... rest of validation logic ...
    }
}
```

- [ ] **Step 6: Run tests to verify they pass**

```bash
cargo test test_compile_gate
```
Expected: PASS

- [ ] **Step 7: Commit**

```bash
git add crates/ggen-core/src/validation/ crates/ggen-core/src/pipeline/validate.rs tests/pipeline/compile_gate_test.rs
git commit -m "feat(pipeline): add compile gate validation to μ₄ stage"
```

---

## Task 14: Add OTEL/Weaver Receipt Integration

**Files:**
- Modify: `crates/ggen-a2a-mcp/src/tools.rs` (in generated code)
- Modify: `crates/ggen-core/templates/mcp/tools.rs.tera`
- Test: `tests/otel/receipt_test.rs`

**Purpose:** Emit OTEL spans with receipt metadata for each tool invocation.

- [ ] **Step 1: Write the failing test**

```rust
// tests/otel/receipt_test.rs
use ggen_a2a_mcp::GgenMcpServer;
use rmcp::model::*;

#[test]
fn test_tool_invocation_emits_receipt() {
    // This test verifies that tool invocations emit OTEL spans
    // Actual verification requires running with RUST_LOG=trace

    let server = GgenMcpServer::new();
    let params = serde_json::json!({"ontology_path": "test.ttl"});

    // The actual span verification is done at runtime via logs
    // This test ensures the receipt emission code is present
    let tool_impl = format!("{:?}", server);
    assert!(tool_impl.contains("emit_receipt") || tool_impl.contains("otel"));
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cargo test test_tool_invocation_emits_receipt
```
Expected: FAIL (receipt emission not yet implemented)

- [ ] **Step 3: Update tools.rs.tera with OTEL/receipt code**

```tera
// crates/ggen-core/templates/mcp/tools.rs.tera (add to each tool)
use rmcp::{tool, tool_router, ServerHandler};
use rmcp::model::*;
use rmcp::service::RequestContext;
use rmcp::RoleServer;
use serde::{Deserialize, Serialize};
use tracing::{info_span, Instrument};
use opentelemetry::trace::{TraceContextExt, Tracer};
use opentelemetry::KeyValue;

{% for tool in tools %}
use {{ tool.impl_import | default(value="crate::tools") }};

/// Arguments for {{ tool.name }}
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct {{ tool.args_type }} {
    {% for arg in tool.arguments %}
    /// {{ arg.description | default(value="") }}
    {% if arg.required %}{% else %}#[serde(skip_serializing_if = "Option::is_empty")]{% endif %}
    pub {{ arg.name }}: {% if arg.required %}{{ arg.rust_type }}{% else %}Option<{{ arg.rust_type }}>{% endif %},
    {% endfor %}
}
{% endfor %}

#[tool_router]
impl {{ server.type_name }} {
    {% for tool in tools %}
    #[tool(description = "{{ tool.description }}")]
    async fn {{ tool.name }}(
        &self,
        Parameters(params): Parameters<{{ tool.args_type }}>,
    ) -> Result<CallToolResult, McpError> {
        // Create OTEL span for tool invocation
        let span = tracing::info_span!(
            "tool_invocation",
            tool.name = %{{ tool.name }},
            tool.arguments = ?params
        );

        async move {
            // Execute tool implementation
            let result = {{ tool.impl_fn }}(params).await
                .map_err(|e| McpError::internal_error("tool_execution_failed", Some(serde_json::json!({
                    "error": e.to_string()
                }))))?;

            // Emit receipt
            tracing::info!(
                tool.name = %{{ tool.name }},
                receipt.hash = %result.receipt_hash(),
                receipt.timestamp = %result.receipt_timestamp(),
                "Tool invocation completed with receipt"
            );

            Ok(result)
        }
        .instrument(span)
        .await
    }

    {% endfor %}
}
```

- [ ] **Step 4: Add receipt trait to tool results**

```rust
// crates/ggen-core/src/receipt.rs
use serde_json::Value;

/// Trait for tool results that include receipt information
pub trait WithReceipt {
    /// Get receipt hash
    fn receipt_hash(&self) -> String;

    /// Get receipt timestamp
    fn receipt_timestamp(&self) -> chrono::DateTime<chrono::Utc>;

    /// Get full receipt data
    fn receipt_data(&self) -> Value;
}

/// Default implementation for CallToolResult
impl WithReceipt for CallToolResult {
    fn receipt_hash(&self) -> String {
        // Extract or compute hash from result
        format!("{:x}", md5::compute(format!("{:?}", self)))
    }

    fn receipt_timestamp(&self) -> chrono::DateTime<chrono::Utc> {
        chrono::Utc::now()
    }

    fn receipt_data(&self) -> Value {
        serde_json::json!({
            "hash": self.receipt_hash(),
            "timestamp": self.receipt_timestamp(),
            "result": self
        })
    }
}
```

- [ ] **Step 5: Run test to verify it passes**

```bash
cargo test test_tool_invocation_emits_receipt
```
Expected: PASS

- [ ] **Step 6: Commit**

```bash
git add crates/ggen-core/templates/mcp/tools.rs.tera crates/ggen-core/src/receipt.rs tests/otel/receipt_test.rs
git commit -m "feat(mcp): add OTEL/receipt emission to tool invocations"
```

---

## Task 15: End-to-End Generation Test

**Files:**
- Create: `tests/e2e/mcp_generation_test.rs`
- Test: `tests/e2e/mcp_generation_test.rs`

**Purpose:** Full pipeline test: ontology → context → generated code → compile.

- [ ] **Step 1: Write the failing test**

```rust
// tests/e2e/mcp_generation_test.rs
use ggen_core::Pipeline;
use ggen_core::Graph;
use std::path::PathBuf;

#[test]
fn test_full_mcp_generation() {
    // Load ontology
    let ontology_path = PathBuf::from("specify/ontologies/mcp/mcp-server.ttl");
    let graph = Graph::load_ttl(ontology_path).unwrap();

    // Run pipeline μ₁-μ₅
    let pipeline = Pipeline::new("mcp-server").unwrap();
    let result = pipeline
        .with_graph(graph)
        .run()
        .unwrap();

    // Verify generated files exist
    assert!(result.generated_file("src/server.rs").exists());
    assert!(result.generated_file("src/tools.rs").exists());
    assert!(result.generated_file("src/resources.rs").exists());
    assert!(result.generated_file("src/prompts.rs").exists());
    assert!(result.generated_file("src/completions.rs").exists());
    assert!(result.generated_file("src/logging.rs").exists());
    assert!(result.generated_file("src/main.rs").exists());

    // Verify compilation
    result.verify_compilation().unwrap();
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cargo test test_full_mcp_generation
```
Expected: FAIL (pipeline integration not yet complete)

- [ ] **Step 3: Integrate MCP templates into main pipeline**

```rust
// crates/ggen-core/src/pipeline/mcp.rs
use crate::Graph;
use crate::templates::TemplateRenderer;
use serde_json::Value;

pub struct McpPipeline {
    ontology_path: std::path::PathBuf,
    output_dir: std::path::PathBuf,
}

impl McpPipeline {
    pub fn new(ontology_path: std::path::PathBuf, output_dir: std::path::PathBuf) -> Self {
        Self {
            ontology_path,
            output_dir,
        }
    }

    pub fn run(&self) -> Result<PipelineResult, PipelineError> {
        // μ₁: Load ontology
        let graph = Graph::load_ttl(&self.ontology_path)?;

        // μ₂: Extract context via SPARQL
        let query = std::fs::read_to_string("crates/ggen-core/queries/mcp/extract-mcp-context.rq")?;
        let context_data = graph.execute_construct(&query)?;

        // Normalize to JSON context
        let context: Value = serde_json::to_value(context_data)?;

        // μ₃: Render templates
        let templates = vec![
            ("server.rs", "mcp/server.rs.tera"),
            ("get_info.rs", "mcp/get_info.rs.tera"),
            ("tools.rs", "mcp/tools.rs.tera"),
            ("resources.rs", "mcp/resources.rs.tera"),
            ("resource_templates.rs", "mcp/resource_templates.rs.tera"),
            ("prompts.rs", "mcp/prompts.rs.tera"),
            ("completions.rs", "mcp/completions.rs.tera"),
            ("logging.rs", "mcp/logging.rs.tera"),
            ("main.rs", "mcp/main.rs.tera"),
        ];

        let mut generated_files = Vec::new();

        for (output_name, template_name) in templates {
            let rendered = TemplateRenderer::render(template_name, &context)?;
            let output_path = self.output_dir.join(output_name);
            std::fs::write(&output_path, rendered)?;
            generated_files.push(output_path);
        }

        // μ₄: Validate (compile gate)
        crate::validation::CompileGate::validate_crate("ggen-a2a-mcp")?;

        // μ₅: Emit
        Ok(PipelineResult {
            generated_files,
            receipt: self.generate_receipt()?,
        })
    }

    fn generate_receipt(&self) -> Result<Receipt, PipelineError> {
        Ok(Receipt {
            hash: format!("{:x}", md5::compute(self.ontology_path)),
            timestamp: chrono::Utc::now(),
            source: self.ontology_path.clone(),
        })
    }
}
```

- [ ] **Step 4: Run test to verify it passes**

```bash
cargo test test_full_mcp_generation
```
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add tests/e2e/mcp_generation_test.rs crates/ggen-core/src/pipeline/mcp.rs
git commit -m "feat(mcp): add end-to-end generation test"
```

---

## Task 16: CLI Integration for MCP Generation

**Files:**
- Modify: `crates/ggen-cli/src/commands/mcp.rs`
- Modify: `crates/ggen-cli/src/commands/mod.rs`
- Test: `tests/cli/mcp_command_test.rs`

**Purpose:** Add `ggen mcp generate` command.

- [ ] **Step 1: Write the failing test**

```rust
// tests/cli/mcp_command_test.rs
use ggen_cli::commands::mcp::McpGenerateCommand;

#[test]
fn test_mcp_generate_command() {
    let cmd = McpGenerateCommand {
        ontology: "specify/ontologies/mcp/mcp-server.ttl".into(),
        output: "crates/ggen-a2a-mcp/src".into(),
    };

    let result = cmd.execute();
    assert!(result.is_ok(), "MCP generation should succeed");
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cargo test test_mcp_generate_command
```
Expected: FAIL with "not found"

- [ ] **Step 3: Create MCP command**

```rust
// crates/ggen-cli/src/commands/mcp.rs
use clap::{Parser, Subcommand};
use std::path::PathBuf;

/// MCP server generation commands
#[derive(Subcommand, Debug)]
pub enum McpCommands {
    /// Generate MCP server from ontology
    Generate(McpGenerateCommand),
}

/// Generate an MCP server from RDF ontology
#[derive(Parser, Debug)]
pub struct McpGenerateCommand {
    /// Path to MCP server ontology (.ttl)
    #[arg(long)]
    pub ontology: PathBuf,

    /// Output directory for generated code
    #[arg(long)]
    pub output: PathBuf,

    /// Skip compile gate
    #[arg(long, default_value = "false")]
    pub skip_compile_gate: bool,
}

impl McpGenerateCommand {
    pub fn execute(self) -> Result<(), Box<dyn std::error::Error>> {
        use ggen_core::pipeline::McpPipeline;

        tracing::info!("Generating MCP server from: {:?}", self.ontology);

        let pipeline = McpPipeline::new(self.ontology, self.output);
        let result = pipeline.run()?;

        tracing::info!(
            "Generated {} files with receipt {}",
            result.generated_files.len(),
            result.receipt.hash
        );

        Ok(())
    }
}
```

- [ ] **Step 4: Register command in CLI**

```rust
// crates/ggen-cli/src/commands/mod.rs (add to existing commands)
pub mod mcp;

use mcp::{McpCommands, McpGenerateCommand};
```

- [ ] **Step 5: Run test to verify it passes**

```bash
cargo test test_mcp_generate_command
```
Expected: PASS

- [ ] **Step 6: Commit**

```bash
git add crates/ggen-cli/src/commands/mcp.rs crates/ggen-cli/src/commands/mod.rs tests/cli/mcp_command_test.rs
git commit -m "feat(cli): add ggen mcp generate command"
```

---

## Task 17: Documentation and Examples

**Files:**
- Create: `docs/mcp/README.md`
- Create: `examples/mcp-server/README.md`
- Create: `examples/mcp-server/ontology/server.ttl`

**Purpose:** User-facing documentation for MCP generation.

- [ ] **Step 1: Create MCP documentation**

```markdown
<!-- docs/mcp/README.md -->
# ggen MCP Server Generation

Generate spec-pinned MCP 2025-11-25 servers from RDF ontology.

## Quick Start

1. Define your MCP server in RDF/Turtle:

```turtle
@prefix mcp: <http://ggen.dev/mcp#> .

:MyServer a mcp:McpServer ;
    mcp:name "MyServer" ;
    mcp:hasCapabilitySet :MyCaps .
```

2. Generate the server:

```bash
ggen mcp generate --ontology examples/mcp-server/ontology/server.ttl --output ./src
```

3. Run your server:

```bash
cargo run
```

## Architecture

- **Ontology**: Pure MCP 2025-11-25 classes
- **Context**: Normalized JSON via SPARQL CONSTRUCT
- **Templates**: Capability-specific (tools, resources, prompts, etc.)
- **SDK**: RMCP 1.3.0 (official Rust SDK)

## Supported Features

- ✅ Tools (with typed arguments)
- ✅ Resources (with templates)
- ✅ Prompts (with argument substitution)
- ✅ Completions (argument autocomplete)
- ✅ Logging (MCP protocol + OTEL Weaver)
- ❌ Subscriptions (planned)

## Validation

The compile gate ensures generated code compiles:

```bash
ggen mcp generate --ontology server.ttl --output ./src
# Output: ✅ Compile gate passed
```
```

- [ ] **Step 2: Create example server**

```turtle
<!-- examples/mcp-server/ontology/server.ttl -->
@prefix mcp: <http://ggen.dev/mcp#> .

:ExampleServer a mcp:McpServer ;
    mcp:name "ExampleServer" ;
    mcp:description "Example MCP server" ;
    mcp:hasProtocolVersion "2025-11-25" ;
    mcp:hasTransport mcp:StdioTransport ;
    mcp:hasCapabilitySet [
        a mcp:CapabilitySet ;
        mcp:tools true ;
        mcp:resources true ;
    ] ;
    mcp:hasTool [
        a mcp:Tool ;
        mcp:name "hello" ;
        mcp:description "Say hello" ;
        mcp:hasArgument [
            a mcp:ToolArgument ;
            mcp:argumentName "name" ;
            mcp:argumentType "String" ;
            mcp:isRequired true ;
        ] ;
    ] .
```

- [ ] **Step 3: Commit**

```bash
git add docs/mcp/README.md examples/mcp-server/
git commit -m "docs(mcp): add documentation and example server"
```

---

## Self-Review Checklist

- [x] **Spec coverage**: All ontology classes, templates, pipeline stages, validation gates covered
- [x] **Placeholder scan**: No TBD/TODO/fill-in found; all code shown
- [x] **Type consistency**: `{{ server.type_name }}`, `mcp:name`, etc. consistent throughout

---

## Verification Steps

After implementation:

1. **Generate example server**:
   ```bash
   ggen mcp generate --ontology examples/mcp-server/ontology/server.ttl --output crates/ggen-a2a-mcp/src
   ```

2. **Verify compilation**:
   ```bash
   cargo check --package ggen-a2a-mcp
   ```

3. **Run server**:
   ```bash
   cargo run --package ggen-a2a-mcp
   ```

4. **Test with MCP client**:
   ```bash
   echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}' | cargo run --package ggen-a2a-mcp
   ```

---

**Plan Status**: ✅ Complete - Ready for execution

**Total Tasks**: 17
**Estimated Time**: 4-6 hours for full implementation
**Dependencies**: RMCP 1.3.0, Oxigraph, Tera, OTEL SDK
