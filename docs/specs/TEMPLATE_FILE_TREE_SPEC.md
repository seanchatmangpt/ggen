# Template-to-File-Tree Generation Specification
## SPARC Phase 1: Specification

**Version:** 1.0.0
**Date:** 2025-11-01
**Status:** Draft for Review
**Author:** Hive Queen Specification Agent (Beta)

---

## Executive Summary

This specification defines a comprehensive system for generating **entire file trees from a single template** using RDF metadata integration in the ggen code generation framework. The system enables developers to scaffold complete project structures (microservices, full-stack apps, multi-module systems) from declarative templates with semantic relationships encoded in RDF graphs.

**Key Innovation:** Transform a single `.tmpl` file into dozens of interconnected source files using SPARQL queries to define file relationships, structure, and content variations.

---

## 1. Functional Requirements

### FR-001: Multi-File Template Declaration
**Priority:** High
**Description:** Templates shall support declaring multiple output files within a single template using RDF-based file tree metadata.

**Acceptance Criteria:**
- Template frontmatter includes `file_tree` section defining multiple outputs
- Each file node has: `path`, `content_template`, `dependencies`, `metadata`
- SPARQL queries can reference file nodes and relationships
- File generation order respects dependency graph

**Example:**
```yaml
---
file_tree:
  nodes:
    - id: "service_main"
      path: "src/services/{{ name }}.rs"
      template: "service_template"
      dependencies: ["models", "handlers"]
    - id: "models"
      path: "src/models/{{ name }}.rs"
      template: "model_template"
    - id: "handlers"
      path: "src/handlers/{{ name }}.rs"
      template: "handler_template"
      dependencies: ["models"]
---
```

### FR-002: RDF-Based File Tree Metadata
**Priority:** High
**Description:** File tree structure shall be queryable via RDF/SPARQL for dynamic file generation.

**Acceptance Criteria:**
- Files represented as RDF nodes with properties (path, type, language)
- Relationships (imports, depends_on, generates) encoded as RDF predicates
- SPARQL queries extract file patterns from domain ontology
- Template can query graph to determine which files to generate

**RDF Schema:**
```turtle
@prefix ggen: <http://ggen.io/ontology/> .
@prefix fs: <http://ggen.io/ontology/filesystem/> .

fs:File a rdfs:Class ;
    rdfs:label "Generated File" .

fs:path a rdf:Property ;
    rdfs:domain fs:File ;
    rdfs:range xsd:string .

fs:language a rdf:Property ;
    rdfs:domain fs:File ;
    rdfs:range xsd:string .

fs:dependsOn a rdf:Property ;
    rdfs:domain fs:File ;
    rdfs:range fs:File .

fs:imports a rdf:Property ;
    rdfs:domain fs:File ;
    rdfs:range fs:File .
```

### FR-003: Template Composition and Inheritance
**Priority:** Medium
**Description:** Templates shall support composition via includes and inheritance patterns.

**Acceptance Criteria:**
- `extends` keyword inherits file tree from base template
- `includes` keyword composes partial templates
- Variable scoping: global, file-level, template-level
- Override mechanisms for paths and content

**Example:**
```yaml
---
extends: "base-microservice.tmpl"
includes:
  - "common/docker.tmpl"
  - "common/ci.tmpl"
file_tree:
  override:
    - id: "service_main"
      path: "custom/path/{{ name }}.rs"
---
```

### FR-004: Dynamic File Generation from SPARQL
**Priority:** High
**Description:** File tree shall be dynamically generated based on SPARQL query results.

**Acceptance Criteria:**
- SPARQL queries return file specifications (path, content type)
- Each query result row generates one file
- Template variables populated from RDF properties
- Support for conditional file generation based on graph patterns

**Example:**
```yaml
---
sparql:
  file_spec: |
    SELECT ?entity ?type ?path WHERE {
      ?entity a ex:Entity ;
              ex:type ?type ;
              ex:outputPath ?path .
    }
file_tree:
  dynamic:
    source: "file_spec"
    template: "entity_template"
    path: "{{ result.path }}"
    vars:
      type: "{{ result.type }}"
      entity: "{{ result.entity }}"
---
```

### FR-005: File Tree Validation
**Priority:** Medium
**Description:** System shall validate file tree structure before generation.

**Acceptance Criteria:**
- Detect circular dependencies in file graph
- Validate path uniqueness (no duplicate outputs)
- Check template references exist
- Verify required variables present
- Validate RDF graph integrity

### FR-006: Incremental File Tree Updates
**Priority:** Low
**Description:** Support updating existing file trees without full regeneration.

**Acceptance Criteria:**
- Detect changed files via checksums
- Only regenerate modified dependencies
- Preserve manual edits in designated sections
- Generate diff reports before applying changes

---

## 2. Non-Functional Requirements

### NFR-001: Performance
**Category:** Performance
**Description:** File tree generation shall complete in reasonable time for typical projects.

**Metrics:**
- Generate 50-file project in <10 seconds
- Generate 200-file monorepo in <60 seconds
- RDF graph queries <100ms for 1000+ triples
- Memory usage <500MB for typical projects

### NFR-002: Determinism
**Category:** Reliability
**Description:** File tree generation shall be deterministic and reproducible.

**Metrics:**
- Identical inputs produce byte-identical outputs
- File generation order consistent across runs
- SPARQL result ordering deterministic (ORDER BY required)
- RNG seeding via `determinism` frontmatter field

### NFR-003: Error Handling
**Category:** Reliability
**Description:** System shall gracefully handle all error conditions.

**Requirements:**
- No `.expect()` or `.unwrap()` in production paths
- Detailed error messages with context
- Partial generation with rollback on error
- Validation errors before file writes

### NFR-004: Backwards Compatibility
**Category:** Compatibility
**Description:** New file tree features shall not break existing single-file templates.

**Requirements:**
- Single-file templates continue working unchanged
- `file_tree` section optional
- Default behavior: single file output (current system)
- Migration path from single to multi-file

---

## 3. Use Cases

### UC-001: Generate Microservices Architecture
**Actor:** Developer
**Goal:** Scaffold complete microservice from single template

**Preconditions:**
- Template defines service structure
- RDF graph contains domain entities
- Variables provided: service_name, entities

**Main Flow:**
1. Developer runs: `ggen generate microservice.tmpl --vars service_name=users`
2. Template queries RDF graph for entities (User, Role, Permission)
3. For each entity, generates:
   - `src/models/{entity}.rs` - Data models
   - `src/handlers/{entity}.rs` - HTTP handlers
   - `src/repositories/{entity}.rs` - Database access
   - `src/services/{entity}.rs` - Business logic
4. Generates common files:
   - `Cargo.toml` - Dependencies
   - `src/main.rs` - Entry point
   - `src/config.rs` - Configuration
   - `Dockerfile` - Container image
   - `.github/workflows/ci.yml` - CI/CD
5. All files written with correct imports and relationships
6. Summary report shows generated file tree

**Postconditions:**
- 15-30 files generated in proper structure
- All imports reference correct modules
- Compiles without errors (if domain model valid)

**Exceptions:**
- Invalid entity in RDF graph: Skip entity, log warning
- Circular dependency: Error before generation
- Duplicate file paths: Error with conflicting templates

### UC-002: Generate Full-Stack Application
**Actor:** Developer
**Goal:** Create complete web app (frontend + backend + infrastructure)

**Preconditions:**
- Template extends `fullstack-base.tmpl`
- RDF defines API endpoints and UI components
- Variables: app_name, database_type, ui_framework

**Main Flow:**
1. Developer runs: `ggen generate fullstack.tmpl --vars app_name=blog database_type=postgres ui_framework=react`
2. Template queries RDF for:
   - API endpoints (GET /posts, POST /posts, etc.)
   - UI components (PostList, PostDetail, Editor)
   - Database schema (posts, users, comments tables)
3. Backend generation:
   - `backend/src/routes/*.rs` - API routes
   - `backend/src/models/*.rs` - ORM models
   - `backend/migrations/*.sql` - Database migrations
4. Frontend generation:
   - `frontend/src/components/*.tsx` - React components
   - `frontend/src/api/*.ts` - API client
   - `frontend/src/hooks/*.ts` - Custom hooks
5. Infrastructure:
   - `docker-compose.yml` - Local dev stack
   - `kubernetes/*.yaml` - K8s manifests
   - `.github/workflows/*.yml` - CI/CD pipelines
6. Configuration:
   - `backend/.env.example` - Backend config template
   - `frontend/.env.example` - Frontend config template

**Postconditions:**
- 60-100 files generated
- Backend compiles and starts
- Frontend builds and runs
- Docker compose stack works

### UC-003: Generate Multi-Module Monorepo
**Actor:** Developer
**Goal:** Scaffold Cargo workspace with multiple crates

**Preconditions:**
- Template defines workspace structure
- RDF graph specifies crate dependencies
- Variables: workspace_name, crates[]

**Main Flow:**
1. Developer runs: `ggen generate monorepo.tmpl --vars workspace_name=my_project`
2. Template queries RDF for crate dependency graph
3. Generates workspace:
   - `Cargo.toml` - Workspace config with members
   - `.cargo/config.toml` - Build config
4. For each crate in dependency order:
   - `crates/{name}/Cargo.toml` - Crate manifest
   - `crates/{name}/src/lib.rs` - Library entry
   - `crates/{name}/tests/*.rs` - Integration tests
   - `crates/{name}/benches/*.rs` - Benchmarks
5. Generates shared infrastructure:
   - `xtask/src/main.rs` - Build automation
   - `.github/workflows/ci.yml` - Workspace CI
   - `Makefile.toml` - Cargo-make tasks

**Postconditions:**
- Valid Cargo workspace
- All crates build in correct order
- Cross-crate dependencies properly configured

### UC-004: Generate Component Library
**Actor:** UI Developer
**Goal:** Create design system components from specifications

**Preconditions:**
- RDF graph contains component specifications
- Design tokens defined (colors, spacing, typography)
- UI framework selected (React, Vue, Svelte)

**Main Flow:**
1. Developer runs: `ggen generate components.tmpl --vars framework=react`
2. Template queries RDF for:
   - Component definitions (Button, Input, Card)
   - Props and variants for each component
   - Accessibility requirements
3. For each component:
   - `src/components/{Name}/{Name}.tsx` - Component
   - `src/components/{Name}/{Name}.stories.tsx` - Storybook
   - `src/components/{Name}/{Name}.test.tsx` - Tests
   - `src/components/{Name}/{Name}.module.css` - Styles
4. Generates infrastructure:
   - `src/components/index.ts` - Barrel exports
   - `.storybook/main.ts` - Storybook config
   - `package.json` - Dependencies
   - `tsconfig.json` - TypeScript config

**Postconditions:**
- Component library builds
- Storybook runs showing all components
- Tests pass
- Exports available for consumption

---

## 4. Technical Requirements

### TR-001: Template Format Extension

**Current Single-File Format:**
```yaml
---
to: "src/output.rs"
vars:
  name: "example"
---
// Single file content
fn main() { println!("{{ name }}"); }
```

**New Multi-File Format:**
```yaml
---
# File tree definition (new!)
file_tree:
  # Static files (explicit paths)
  static:
    - id: "main"
      path: "src/main.rs"
      template: |
        fn main() {
            println!("Starting {{ name }}...");
            {{ name | snake }}::run();
        }

    - id: "lib"
      path: "src/lib.rs"
      template: |
        pub mod handlers;
        pub mod models;

        pub fn run() {
            println!("Running {{ name }}");
        }

  # Dynamic files (generated from SPARQL)
  dynamic:
    - id: "entity_models"
      source: "entities_query"  # References SPARQL query below
      path: "src/models/{{ entity.name | snake }}.rs"
      template: |
        /// {{ entity.description }}
        #[derive(Debug, Clone)]
        pub struct {{ entity.name | pascal }} {
            pub id: i64,
            {% for prop in entity.properties %}
            pub {{ prop.name }}: {{ prop.type }},
            {% endfor %}
        }

    - id: "entity_handlers"
      source: "entities_query"
      path: "src/handlers/{{ entity.name | snake }}.rs"
      template: |
        use crate::models::{{ entity.name | pascal }};

        pub async fn get_{{ entity.name | snake }}(id: i64) -> Result<{{ entity.name | pascal }}> {
            todo!("Implement get")
        }

# RDF and SPARQL (existing)
rdf:
  - "domain.ttl"

sparql:
  entities_query: |
    SELECT ?entity ?name ?description WHERE {
      ?entity a ex:Entity ;
              ex:name ?name ;
              ex:description ?description .
      OPTIONAL {
        ?entity ex:properties ?props .
      }
    }

# Global variables (existing)
vars:
  name: "my_service"
  author: "Developer"
---
# Optional: Main template content for metadata/docs
# This section used for README or documentation generation
# Generated by ggen on {{ "now" | date(format="%Y-%m-%d") }}
# Project: {{ name }}
```

### TR-002: RDF Ontology for File Trees

**Namespace:**
```turtle
@prefix ggen: <http://ggen.io/ontology/> .
@prefix fs: <http://ggen.io/ontology/filesystem/> .
@prefix proj: <http://ggen.io/ontology/project/> .
```

**Core Classes:**
```turtle
fs:File a rdfs:Class ;
    rdfs:label "Generated File" ;
    rdfs:comment "Represents a file to be generated" .

fs:Directory a rdfs:Class ;
    rdfs:label "Generated Directory" ;
    rdfs:comment "Represents a directory structure" .

proj:Project a rdfs:Class ;
    rdfs:label "Generated Project" ;
    rdfs:comment "Root of file tree" .

proj:Module a rdfs:Class ;
    rdfs:subClassOf fs:Directory ;
    rdfs:label "Code Module" .

proj:Component a rdfs:Class ;
    rdfs:label "Software Component" ;
    rdfs:comment "Abstract component generating files" .
```

**Properties:**
```turtle
fs:path a rdf:Property ;
    rdfs:domain fs:File ;
    rdfs:range xsd:string ;
    rdfs:comment "Relative path from project root" .

fs:language a rdf:Property ;
    rdfs:domain fs:File ;
    rdfs:range xsd:string ;
    rdfs:comment "Programming language (rust, typescript, python)" .

fs:fileType a rdf:Property ;
    rdfs:domain fs:File ;
    rdfs:range xsd:string ;
    rdfs:comment "File type (source, test, config, docs)" .

fs:dependsOn a rdf:Property ;
    rdfs:domain fs:File ;
    rdfs:range fs:File ;
    rdfs:comment "File dependency relationship" .

fs:imports a rdf:Property ;
    rdfs:domain fs:File ;
    rdfs:range fs:File ;
    rdfs:comment "Import/include relationship" .

proj:generates a rdf:Property ;
    rdfs:domain proj:Component ;
    rdfs:range fs:File ;
    rdfs:comment "Component generates file" .

proj:hasModule a rdf:Property ;
    rdfs:domain proj:Project ;
    rdfs:range proj:Module .
```

**Example Instance:**
```turtle
@prefix ex: <http://example.org/myproject/> .
@prefix fs: <http://ggen.io/ontology/filesystem/> .

ex:myproject a proj:Project ;
    rdfs:label "My Project" ;
    proj:hasModule ex:api_module, ex:models_module .

ex:api_module a proj:Module ;
    rdfs:label "API Module" ;
    proj:generates ex:api_file .

ex:api_file a fs:File ;
    fs:path "src/api/handlers.rs" ;
    fs:language "rust" ;
    fs:fileType "source" ;
    fs:imports ex:models_file .

ex:models_file a fs:File ;
    fs:path "src/models/user.rs" ;
    fs:language "rust" ;
    fs:fileType "source" .
```

### TR-003: File Generation Algorithm

**Input:**
- Template with `file_tree` section
- User-provided variables
- RDF graph (optional)

**Output:**
- Map of file paths to content
- Dependency graph
- Generation metadata

**Algorithm:**

```rust
fn generate_file_tree(template: &Template, vars: &Context, graph: &Graph) -> Result<FileTree> {
    let mut file_tree = FileTree::new();

    // Phase 1: Parse and validate file tree definition
    let file_tree_spec = parse_file_tree_from_frontmatter(&template.front)?;
    validate_file_tree_spec(&file_tree_spec)?;

    // Phase 2: Generate static files
    for file_spec in &file_tree_spec.static_files {
        let path = render_path(&file_spec.path, vars)?;
        let content = render_template(&file_spec.template, vars)?;
        file_tree.add_file(path, content, file_spec.metadata)?;
    }

    // Phase 3: Generate dynamic files from SPARQL
    for dynamic_spec in &file_tree_spec.dynamic_files {
        // Get SPARQL query results
        let query = template.front.sparql.get(&dynamic_spec.source)
            .ok_or_else(|| anyhow!("Missing SPARQL query: {}", dynamic_spec.source))?;

        let results = graph.query_cached(query)?;

        // Generate one file per result row
        for row in results.to_json().as_array() {
            let mut file_vars = vars.clone();
            file_vars.insert("result", row);

            let path = render_path(&dynamic_spec.path, &file_vars)?;
            let content = render_template(&dynamic_spec.template, &file_vars)?;
            file_tree.add_file(path, content, dynamic_spec.metadata)?;
        }
    }

    // Phase 4: Build dependency graph
    let dep_graph = build_dependency_graph(&file_tree, graph)?;
    validate_no_cycles(&dep_graph)?;

    // Phase 5: Sort files by dependency order
    let sorted_files = topological_sort(&dep_graph)?;

    Ok(FileTree {
        files: sorted_files,
        graph: dep_graph,
        metadata: template.front.clone(),
    })
}
```

### TR-004: Integration with Existing System

**Minimal Changes Required:**

1. **Template Struct Extension** (`ggen-core/src/template.rs`):
```rust
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Frontmatter {
    // ... existing fields ...

    // NEW: File tree specification
    #[serde(default)]
    pub file_tree: Option<FileTreeSpec>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileTreeSpec {
    #[serde(default)]
    pub static_files: Vec<StaticFileSpec>,

    #[serde(default)]
    pub dynamic_files: Vec<DynamicFileSpec>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StaticFileSpec {
    pub id: String,
    pub path: String,
    pub template: String,
    #[serde(default)]
    pub metadata: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DynamicFileSpec {
    pub id: String,
    pub source: String,  // Reference to SPARQL query
    pub path: String,    // Template path with {{ result.field }}
    pub template: String,
    #[serde(default)]
    pub metadata: BTreeMap<String, String>,
}
```

2. **Generator Extension** (`ggen-core/src/generator.rs`):
```rust
impl Generator {
    pub fn generate(&mut self) -> Result<Vec<PathBuf>> {
        let input = fs::read_to_string(&self.ctx.template_path)?;
        let mut tmpl = Template::parse(&input)?;

        // ... existing frontmatter/graph processing ...

        // NEW: Check for file tree generation
        if let Some(file_tree_spec) = &tmpl.front.file_tree {
            return self.generate_file_tree(&tmpl, file_tree_spec);
        }

        // Fallback: Single file generation (existing behavior)
        self.generate_single_file(&tmpl)
    }

    fn generate_file_tree(
        &mut self,
        tmpl: &Template,
        spec: &FileTreeSpec
    ) -> Result<Vec<PathBuf>> {
        // Implementation of TR-003 algorithm
    }
}
```

3. **CLI Enhancement** (`cli/src/commands/generate.rs`):
```rust
// NEW: Add flag for file tree mode
#[derive(Parser)]
pub struct GenerateArgs {
    // ... existing args ...

    /// Generate entire file tree (multi-file output)
    #[arg(long, short = 't')]
    pub tree: bool,

    /// Show file tree preview without writing files
    #[arg(long)]
    pub preview: bool,
}
```

---

## 5. RDF Schema Requirements

### SR-001: File System Ontology

**File:** `ggen-core/ontologies/filesystem.ttl`

```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ggen: <http://ggen.io/ontology/> .
@prefix fs: <http://ggen.io/ontology/filesystem/> .

# Core Classes
fs:File a rdfs:Class ;
    rdfs:label "Generated File"@en ;
    rdfs:comment "Represents a file to be generated from a template"@en .

fs:Directory a rdfs:Class ;
    rdfs:label "Directory"@en ;
    rdfs:comment "Represents a directory in the file tree"@en .

fs:SymbolicLink a rdfs:Class ;
    rdfs:label "Symbolic Link"@en ;
    rdfs:comment "Symbolic link to another file or directory"@en .

# File Properties
fs:path a rdf:Property ;
    rdfs:label "file path"@en ;
    rdfs:domain fs:File ;
    rdfs:range xsd:string ;
    rdfs:comment "Relative path from project root"@en .

fs:absolutePath a rdf:Property ;
    rdfs:label "absolute path"@en ;
    rdfs:domain fs:File ;
    rdfs:range xsd:string .

fs:language a rdf:Property ;
    rdfs:label "programming language"@en ;
    rdfs:domain fs:File ;
    rdfs:range xsd:string ;
    rdfs:comment "rust, typescript, python, go, java, etc."@en .

fs:fileType a rdf:Property ;
    rdfs:label "file type"@en ;
    rdfs:domain fs:File ;
    rdfs:range fs:FileType .

fs:FileType a rdfs:Class ;
    rdfs:label "File Type"@en .

fs:Source a fs:FileType ;
    rdfs:label "Source Code"@en .

fs:Test a fs:FileType ;
    rdfs:label "Test Code"@en .

fs:Configuration a fs:FileType ;
    rdfs:label "Configuration"@en .

fs:Documentation a fs:FileType ;
    rdfs:label "Documentation"@en .

fs:Build a fs:FileType ;
    rdfs:label "Build Script"@en .

# Relationships
fs:dependsOn a rdf:Property ;
    rdfs:label "depends on"@en ;
    rdfs:domain fs:File ;
    rdfs:range fs:File ;
    rdfs:comment "File has a dependency on another file"@en .

fs:imports a rdf:Property ;
    rdfs:label "imports"@en ;
    rdfs:domain fs:File ;
    rdfs:range fs:File ;
    rdfs:comment "File imports/includes another file"@en .

fs:exports a rdf:Property ;
    rdfs:label "exports"@en ;
    rdfs:domain fs:File ;
    rdfs:range xsd:string ;
    rdfs:comment "Symbols exported by this file"@en .

fs:contains a rdf:Property ;
    rdfs:label "contains"@en ;
    rdfs:domain fs:Directory ;
    rdfs:range fs:File ;
    rdfs:comment "Directory contains file"@en .

# Metadata
fs:checksum a rdf:Property ;
    rdfs:label "checksum"@en ;
    rdfs:domain fs:File ;
    rdfs:range xsd:string ;
    rdfs:comment "SHA256 checksum of file content"@en .

fs:size a rdf:Property ;
    rdfs:label "size"@en ;
    rdfs:domain fs:File ;
    rdfs:range xsd:integer ;
    rdfs:comment "File size in bytes"@en .

fs:permissions a rdf:Property ;
    rdfs:label "permissions"@en ;
    rdfs:domain fs:File ;
    rdfs:range xsd:string ;
    rdfs:comment "Unix-style permissions (e.g., '0755')"@en .

fs:generatedAt a rdf:Property ;
    rdfs:label "generated at"@en ;
    rdfs:domain fs:File ;
    rdfs:range xsd:dateTime .

fs:generatedBy a rdf:Property ;
    rdfs:label "generated by"@en ;
    rdfs:domain fs:File ;
    rdfs:range xsd:string ;
    rdfs:comment "Template that generated this file"@en .
```

### SR-002: Project Structure Ontology

**File:** `ggen-core/ontologies/project.ttl`

```turtle
@prefix proj: <http://ggen.io/ontology/project/> .
@prefix fs: <http://ggen.io/ontology/filesystem/> .

# Project Structure
proj:Project a rdfs:Class ;
    rdfs:label "Software Project"@en ;
    rdfs:comment "Root of a generated project"@en .

proj:Module a rdfs:Class ;
    rdfs:subClassOf fs:Directory ;
    rdfs:label "Code Module"@en ;
    rdfs:comment "Logical module or package in the project"@en .

proj:Component a rdfs:Class ;
    rdfs:label "Software Component"@en ;
    rdfs:comment "Abstract software component that generates files"@en .

# Properties
proj:hasModule a rdf:Property ;
    rdfs:domain proj:Project ;
    rdfs:range proj:Module .

proj:generates a rdf:Property ;
    rdfs:domain proj:Component ;
    rdfs:range fs:File ;
    rdfs:comment "Component generates these files"@en .

proj:version a rdf:Property ;
    rdfs:domain proj:Project ;
    rdfs:range xsd:string ;
    rdfs:comment "Semantic version (e.g., '1.0.0')"@en .

proj:license a rdf:Property ;
    rdfs:domain proj:Project ;
    rdfs:range xsd:string ;
    rdfs:comment "License identifier (e.g., 'MIT', 'Apache-2.0')"@en .

proj:repository a rdf:Property ;
    rdfs:domain proj:Project ;
    rdfs:range xsd:anyURI ;
    rdfs:comment "Git repository URL"@en .

# Project Types
proj:MicroserviceProject a rdfs:Class ;
    rdfs:subClassOf proj:Project ;
    rdfs:label "Microservice Project"@en .

proj:MonorepoProject a rdfs:Class ;
    rdfs:subClassOf proj:Project ;
    rdfs:label "Monorepo Project"@en .

proj:LibraryProject a rdfs:Class ;
    rdfs:subClassOf proj:Project ;
    rdfs:label "Library Project"@en .

proj:FullStackProject a rdfs:Class ;
    rdfs:subClassOf proj:Project ;
    rdfs:label "Full-Stack Project"@en .
```

---

## 6. Constraints and Boundaries

### Design Constraints

**DC-001: Backwards Compatibility**
- All existing single-file templates must continue working
- `file_tree` section is optional
- Default behavior unchanged (single file output)

**DC-002: Performance**
- File tree generation must not impact single-file generation performance
- SPARQL query caching essential for large graphs
- Lazy evaluation where possible

**DC-003: Complexity Management**
- Maximum file tree depth: 10 levels
- Maximum files per template: 1000
- Maximum SPARQL result rows: 5000
- Circular dependency detection required

### Technical Boundaries

**TB-001: Supported Template Engines**
- Primary: Tera (existing)
- No support for other engines initially
- Tera features: filters, functions, includes, macros

**TB-002: RDF Format Support**
- Turtle (.ttl) - Primary
- RDF/XML (.rdf) - Secondary
- JSON-LD (.jsonld) - Future
- N-Triples (.nt) - Future

**TB-003: File System Limitations**
- Platform-specific path restrictions apply
- No absolute paths in templates (security)
- Path traversal (../) blocked
- Maximum path length: 255 characters

---

## 7. Validation Checklist

Before implementation, all requirements must be:

### Functional Requirements
- [x] FR-001: Multi-file template declaration defined
- [x] FR-002: RDF-based file tree metadata specified
- [x] FR-003: Template composition and inheritance designed
- [x] FR-004: Dynamic file generation from SPARQL specified
- [x] FR-005: File tree validation requirements listed
- [ ] FR-006: Incremental updates (deferred to v2)

### Non-Functional Requirements
- [x] NFR-001: Performance targets defined
- [x] NFR-002: Determinism requirements specified
- [x] NFR-003: Error handling strategy documented
- [x] NFR-004: Backwards compatibility ensured

### Use Cases
- [x] UC-001: Microservices architecture use case complete
- [x] UC-002: Full-stack application use case complete
- [x] UC-003: Multi-module monorepo use case complete
- [x] UC-004: Component library use case complete

### Technical Requirements
- [x] TR-001: Template format extension designed
- [x] TR-002: RDF ontology defined
- [x] TR-003: File generation algorithm specified
- [x] TR-004: Integration points identified

### Schema Requirements
- [x] SR-001: File system ontology complete
- [x] SR-002: Project structure ontology complete

---

## 8. Success Metrics

### Adoption Metrics
- **Target:** 50% of new templates use file tree generation within 6 months
- **Measure:** Template analytics in marketplace

### Performance Metrics
- **Baseline:** Single file generation <3s
- **Target:** 50-file tree <10s, 200-file tree <60s
- **Measure:** Built-in benchmarking harness

### Quality Metrics
- **Target:** 90% of generated file trees compile without errors
- **Target:** Zero security vulnerabilities in path handling
- **Measure:** Automated test suite + security audits

### Developer Experience
- **Target:** Documentation read time <15 minutes
- **Target:** First successful file tree generation <30 minutes
- **Measure:** User testing + feedback surveys

---

## 9. Open Questions & Future Work

### Open Questions
1. **Q:** Should file tree templates support conditional includes based on RDF queries?
   - **Status:** Research needed - may enable powerful patterns but increases complexity

2. **Q:** How to handle conflicting files from composed templates?
   - **Proposed:** Last-wins strategy with explicit override syntax

3. **Q:** Support for binary file generation (images, fonts)?
   - **Proposed:** Base64 encoding in templates, or reference external files

### Future Enhancements (v2+)

**FE-001: Visual File Tree Editor**
- GUI tool to design file trees
- Drag-and-drop file relationships
- Live SPARQL query testing

**FE-002: Template Inheritance Chains**
- Multi-level extends (A extends B extends C)
- Mixin composition patterns
- Override conflict resolution

**FE-003: Incremental Regeneration**
- Track file checksums
- Only regenerate changed files
- Preserve manual edits in freeze zones

**FE-004: Remote RDF Graphs**
- Query external SPARQL endpoints
- Federated queries across graphs
- Schema.org integration

**FE-005: AI-Assisted File Tree Design**
- LLM generates file tree from description
- Automatic RDF ontology creation
- Best practice recommendations

---

## 10. Appendix: Example Templates

### Example A: Microservice Scaffold

**File:** `templates/microservice.tmpl`

```yaml
---
# Microservice scaffold with Rust + Axum + PostgreSQL
file_tree:
  static:
    - id: "cargo_toml"
      path: "Cargo.toml"
      template: |
        [package]
        name = "{{ name }}"
        version = "{{ version | default(value='0.1.0') }}"
        edition = "2021"

        [dependencies]
        axum = "0.7"
        tokio = { version = "1", features = ["full"] }
        sqlx = { version = "0.7", features = ["postgres", "runtime-tokio-rustls"] }
        serde = { version = "1.0", features = ["derive"] }
        serde_json = "1.0"
        anyhow = "1.0"

    - id: "main_rs"
      path: "src/main.rs"
      template: |
        use axum::Router;
        use std::net::SocketAddr;

        mod config;
        mod routes;
        mod models;
        mod handlers;

        #[tokio::main]
        async fn main() -> anyhow::Result<()> {
            let config = config::load()?;
            let app = Router::new()
                .nest("/api", routes::api_routes());

            let addr = SocketAddr::from(([0, 0, 0, 0], 8080));
            println!("🚀 {{ name }} listening on {}", addr);

            axum::Server::bind(&addr)
                .serve(app.into_make_service())
                .await?;

            Ok(())
        }

    - id: "config_rs"
      path: "src/config.rs"
      template: |
        use serde::Deserialize;

        #[derive(Debug, Clone, Deserialize)]
        pub struct Config {
            pub database_url: String,
            pub port: u16,
        }

        pub fn load() -> anyhow::Result<Config> {
            Ok(Config {
                database_url: std::env::var("DATABASE_URL")?,
                port: std::env::var("PORT")?.parse()?,
            })
        }

  dynamic:
    - id: "entity_models"
      source: "entities"
      path: "src/models/{{ entity.name | snake }}.rs"
      template: |
        //! {{ entity.description }}

        use serde::{Deserialize, Serialize};
        use sqlx::FromRow;

        #[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
        pub struct {{ entity.name | pascal }} {
            pub id: i64,
            {% for prop in entity.properties %}
            pub {{ prop.name | snake }}: {{ prop.rust_type }},
            {% endfor %}
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Create{{ entity.name | pascal }} {
            {% for prop in entity.properties %}
            pub {{ prop.name | snake }}: {{ prop.rust_type }},
            {% endfor %}
        }

    - id: "entity_handlers"
      source: "entities"
      path: "src/handlers/{{ entity.name | snake }}.rs"
      template: |
        use axum::{extract::{Path, State}, Json};
        use crate::models::{{ entity.name | snake }}::*;

        pub async fn list(
            State(pool): State<sqlx::PgPool>,
        ) -> Result<Json<Vec<{{ entity.name | pascal }}>>, StatusCode> {
            let {{ entity.name | snake }}s = sqlx::query_as!(
                {{ entity.name | pascal }},
                "SELECT * FROM {{ entity.table_name }}"
            )
            .fetch_all(&pool)
            .await
            .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

            Ok(Json({{ entity.name | snake }}s))
        }

        pub async fn get(
            State(pool): State<sqlx::PgPool>,
            Path(id): Path<i64>,
        ) -> Result<Json<{{ entity.name | pascal }}>, StatusCode> {
            let {{ entity.name | snake }} = sqlx::query_as!(
                {{ entity.name | pascal }},
                "SELECT * FROM {{ entity.table_name }} WHERE id = $1",
                id
            )
            .fetch_one(&pool)
            .await
            .map_err(|_| StatusCode::NOT_FOUND)?;

            Ok(Json({{ entity.name | snake }}))
        }

# RDF domain model
rdf:
  - "domain/entities.ttl"

# SPARQL queries
sparql:
  entities: |
    PREFIX ex: <http://example.org/>
    PREFIX schema: <http://schema.org/>

    SELECT ?entity ?name ?description ?table_name WHERE {
      ?entity a ex:Entity ;
              schema:name ?name ;
              schema:description ?description ;
              ex:tableName ?table_name .

      OPTIONAL {
        ?entity ex:property ?prop .
        ?prop schema:name ?prop_name ;
              ex:rustType ?prop_type .
      }
    }

# Variables
vars:
  name: "my_service"
  version: "0.1.0"
  author: "Developer"
---
# README content (optional main template)
# {{ name }}

Microservice generated by ggen on {{ "now" | date(format="%Y-%m-%d") }}.

## Getting Started

```bash
cargo run
```
```

**Domain Model:** `domain/entities.ttl`

```turtle
@prefix ex: <http://example.org/> .
@prefix schema: <http://schema.org/> .

ex:User a ex:Entity ;
    schema:name "User" ;
    schema:description "User account entity" ;
    ex:tableName "users" ;
    ex:property ex:UserName, ex:UserEmail .

ex:UserName a ex:Property ;
    schema:name "name" ;
    ex:rustType "String" .

ex:UserEmail a ex:Property ;
    schema:name "email" ;
    ex:rustType "String" .

ex:Post a ex:Entity ;
    schema:name "Post" ;
    schema:description "Blog post entity" ;
    ex:tableName "posts" ;
    ex:property ex:PostTitle, ex:PostContent .

ex:PostTitle a ex:Property ;
    schema:name "title" ;
    ex:rustType "String" .

ex:PostContent a ex:Property ;
    schema:name "content" ;
    ex:rustType "String" .
```

**Generated File Tree:**
```
my_service/
├── Cargo.toml
├── src/
│   ├── main.rs
│   ├── config.rs
│   ├── models/
│   │   ├── user.rs
│   │   └── post.rs
│   └── handlers/
│       ├── user.rs
│       └── post.rs
└── README.md
```

---

## 11. References

### Internal Documentation
- [ggen-core/src/template.rs](../../ggen-core/src/template.rs) - Current template system
- [ggen-core/src/graph.rs](../../ggen-core/src/graph.rs) - RDF/SPARQL integration
- [ggen-core/src/generator.rs](../../ggen-core/src/generator.rs) - File generation
- [ggen-core/src/gpack.rs](../../ggen-core/src/gpack.rs) - Package structure

### External Standards
- **RDF 1.1:** https://www.w3.org/TR/rdf11-concepts/
- **SPARQL 1.1:** https://www.w3.org/TR/sparql11-query/
- **Turtle:** https://www.w3.org/TR/turtle/
- **Tera Templates:** https://tera.netlify.app/

### Similar Systems
- **Yeoman:** https://yeoman.io/ (JavaScript scaffolding)
- **Cookiecutter:** https://cookiecutter.readthedocs.io/ (Python templating)
- **Copier:** https://copier.readthedocs.io/ (Template copying)
- **Hygen:** http://www.hygen.io/ (Code generation)

---

## Document Metadata

**Version History:**
- v1.0.0 (2025-11-01): Initial specification
- Author: Hive Queen Specification Agent (Beta)
- Review Status: Pending Architect + Coder + Tester review
- Implementation Status: Not started
- Target Release: ggen v1.3.0

**Approval Chain:**
- [ ] Specification Agent (Beta) - This document
- [ ] Architect Agent - System design review
- [ ] Code Analyzer - Feasibility review
- [ ] Performance Benchmarker - Performance analysis
- [ ] Production Validator - Production readiness
- [ ] Project Owner - Final approval

---

**End of Specification Document**
