# The 2029 Architecture Paradigm: Why Ontologies Must Be Packs

**Case Study: The Evolution from Direct Code Generation to Template-Driven Composition**

## Executive Summary

This document analyzes the critical architectural mistake made in commit `74362e9d` and extracts universal principles for building extensible systems in 2029. The core lesson: **Direct code generation is an anti-pattern. Templates are foundational, not optional.**

---

## 1. The Anti-Pattern: Direct Code Generation

### What Was Built (Commit 74362e9d)

```rust
// ggen-cli/src/cmds/ontology.rs - THE WRONG APPROACH
#[verb]
fn generate(schema_file: String, language: String, ...) -> VerbResult<GenerateOutput> {
    // Read schema
    let schema: OntologySchema = serde_json::from_str(&schema_content)?;

    // PROBLEM: Direct code generation in CLI layer
    if language == "typescript" {
        let interfaces = TypeScriptGenerator::generate_interfaces(&schema)?;
        let zod_schemas = TypeScriptGenerator::generate_zod_schemas(&schema)?;
        // Write files directly
        std::fs::write("types.ts", interfaces)?;
        std::fs::write("schemas.ts", zod_schemas)?;
    } else {
        bail!("Unsupported language") // PROBLEM: Hardcoded limitation
    }
}
```

### Problems Identified

#### 1.1 Hardcoded Language Support
- **Symptom**: Only TypeScript supported, Python/Go/Rust require CLI code changes
- **Root Cause**: Code generation logic lives in the CLI layer
- **Impact**: Cannot scale to multi-language ecosystems

#### 1.2 No Marketplace Integration
- **Symptom**: Cannot discover, install, or compose ontology templates
- **Root Cause**: Ontologies treated as special case, not as first-class packs
- **Impact**: Users can't leverage marketplace infrastructure (versioning, validation, composition)

#### 1.3 Violates Single Responsibility Principle
- **Symptom**: CLI knows about TypeScript interfaces, Zod schemas, file layouts
- **Root Cause**: Domain logic mixed with presentation logic
- **Impact**: Every output format requires CLI changes

#### 1.4 No Template Reuse
- **Symptom**: Duplicate logic across ggen (templates) and ontology (code generation)
- **Root Cause**: Ontology pipeline bypasses template engine
- **Impact**: Cannot leverage existing template infrastructure

#### 1.5 No Composition or Discovery
- **Symptom**: Cannot combine ontology packs, search marketplace, or install from registry
- **Root Cause**: Ontologies are files, not packs
- **Impact**: Users reinvent the wheel for every project

---

## 2. The 2029 Approach: Ontologies as First-Class Packs

### Architecture Overview

```
┌─────────────────────────────────────────────────────────┐
│                    MARKETPLACE LAYER                     │
│  - Discovery (search, browse, trending)                 │
│  - Installation (fetch, validate, cache)                │
│  - Composition (dependency resolution)                   │
│  - Versioning (semver, compatibility)                    │
└─────────────────────────────────────────────────────────┘
                            ▼
┌─────────────────────────────────────────────────────────┐
│                      PACK LAYER                          │
│  - gpack.toml manifest                                   │
│  - Template discovery (conventions + patterns)           │
│  - RDF integration (graphs, queries, shapes)             │
│  - Dependency management                                 │
└─────────────────────────────────────────────────────────┘
                            ▼
┌─────────────────────────────────────────────────────────┐
│                   TEMPLATE LAYER                         │
│  - Tera templates (.tmpl files)                         │
│  - SPARQL queries (semantic data access)                │
│  - Macros (reusable components)                          │
│  - Context injection (OntologySchema → template vars)    │
└─────────────────────────────────────────────────────────┘
                            ▼
┌─────────────────────────────────────────────────────────┐
│                   GENERATION LAYER                       │
│  - Template rendering (Tera engine)                      │
│  - File tree generation                                  │
│  - Post-processing (formatting, validation)              │
└─────────────────────────────────────────────────────────┘
```

### Example: Ontology Pack Structure

```
schema-org-typescript/
├── gpack.toml                      # Pack manifest
├── templates/
│   ├── ontology/
│   │   ├── graphs/
│   │   │   └── schema-org.ttl      # RDF ontology data
│   │   ├── queries/
│   │   │   ├── extract-classes.sparql
│   │   │   └── extract-properties.sparql
│   │   └── types.ts.tmpl           # TypeScript template
│   ├── zod/
│   │   └── schemas.ts.tmpl         # Zod validation template
│   └── utilities/
│       └── crud.ts.tmpl            # CRUD utilities template
└── README.md
```

### Pack Manifest (`gpack.toml`)

```toml
[gpack]
id = "org.schema.typescript"
name = "Schema.org TypeScript Pack"
version = "1.0.0"
description = "Generate TypeScript types from Schema.org ontologies"
license = "MIT"
ggen_compat = ">=3.0.0"

[dependencies]
"org.schema.core" = "^1.0"      # Base Schema.org definitions

[templates]
patterns = [
    "templates/**/*.tmpl",
    "templates/**/*.tera"
]

[rdf]
base = "https://schema.org/"
patterns = ["templates/**/graphs/*.ttl"]

[rdf.prefixes]
schema = "https://schema.org/"
xsd = "http://www.w3.org/2001/XMLSchema#"

[queries]
patterns = ["templates/**/queries/*.sparql"]
aliases = { classes = "extract-classes.sparql" }

[preset.typescript]
output_dir = "src/types"
zod = true
utilities = true
```

### Template Example (`types.ts.tmpl`)

```typescript
/**
 * Auto-generated TypeScript types from ontology: {{ ontology.label }}
 * Version: {{ ontology.version }}
 * Generated: {{ timestamp }}
 */

{% for class in ontology.classes %}
/**
 * {{ class.description | default(value="") }}
 * @see {{ class.uri }}
 */
export interface {{ class.name }} {
  {% for prop in class | properties_for_class %}
  /**
   * {{ prop.description | default(value="") }}
   */
  {{ prop.name }}{% if prop.cardinality == "ZeroOrOne" or prop.cardinality == "Many" %}?{% endif %}: {{ prop.range | to_typescript_type }};
  {% endfor %}
}
{% endfor %}

// Type exports
export type {
  {% for class in ontology.classes %}
  {{ class.name }},
  {% endfor %}
};
```

---

## 3. Why Templates Are Foundational (Not Optional)

### 3.1 Separation of Concerns

**Template-Driven Architecture**:
```
CLI Layer        → Orchestrates workflow, handles user interaction
Pack Layer       → Defines structure, dependencies, metadata
Template Layer   → Implements output format logic
Domain Layer     → Provides semantic data (OntologySchema, RDF graph)
```

**Benefits**:
- CLI never knows about TypeScript syntax or Zod schemas
- Adding new languages = create new pack, not modify CLI
- Template authors are independent from core developers

### 3.2 Reusability and Composition

**Without Templates**:
```rust
// Every output format requires new Rust code
TypeScriptGenerator::generate_interfaces(&schema)?;
PythonGenerator::generate_dataclasses(&schema)?;
GoGenerator::generate_structs(&schema)?;
RustGenerator::generate_types(&schema)?;
```

**With Templates**:
```bash
# Install packs from marketplace
ggen pack install schema-org-typescript
ggen pack install schema-org-python
ggen pack install schema-org-go

# Generate for all languages
ggen generate schema.ttl --preset typescript
ggen generate schema.ttl --preset python
ggen generate schema.ttl --preset go
```

### 3.3 Community Extension

**Closed System (Old Approach)**:
- Core team must implement every language/framework
- Users wait for PRs to be merged
- Innovation bottlenecked by maintainers

**Open System (Template-Driven)**:
- Users create custom packs for niche needs
- Publish to marketplace for others
- Core team focuses on engine, not every output format

**Example**: A user needs GraphQL schema generation from ontologies:
```bash
# Old approach: Wait for core team or fork repo
# New approach: Create pack and share
mkdir schema-org-graphql && cd schema-org-graphql
ggen pack init --template ontology
# Edit templates/graphql/schema.graphql.tmpl
ggen pack publish
```

### 3.4 Versioning and Stability

**Templates Enable**:
```toml
[dependencies]
"org.schema.typescript" = "^1.0"  # Stable interface
"org.schema.python" = "^2.0"      # Different evolution
```

**Without Templates**:
- CLI version 4.0 breaks TypeScript generation
- All users affected, no rollback
- Must maintain backward compatibility forever

---

## 4. Why Composition and Discovery Matter

### 4.1 The Network Effect

**Marketplace Integration**:
```bash
# Discover ontology packs
ggen search "schema.org typescript"
# → schema-org-typescript (1,234 downloads)
# → schema-org-zod (856 downloads)
# → schema-org-react-hooks (432 downloads)

# Install and use
ggen pack install schema-org-typescript
ggen generate my-ontology.ttl --preset typescript

# Compose multiple packs
ggen pack install schema-org-typescript
ggen pack install graphql-federation
ggen generate my-ontology.ttl --preset typescript-graphql
```

**Benefits**:
- Users find proven solutions instead of starting from scratch
- Download counts signal quality
- Dependency resolution ensures compatibility

### 4.2 Dependency Management

**Example: E-commerce Ontology**
```toml
[gpack]
id = "com.example.ecommerce"
name = "E-commerce Ontology Pack"

[dependencies]
"org.schema.product" = "^1.0"        # Schema.org Product
"org.schema.organization" = "^1.0"   # Schema.org Organization
"com.stripe.payment" = "^2.0"        # Stripe payment ontology
```

**Automatic Resolution**:
```bash
ggen pack install com.example.ecommerce
# Automatically fetches:
# - org.schema.product v1.3.0
# - org.schema.organization v1.2.1
# - com.stripe.payment v2.1.5
```

### 4.3 Quality and Validation

**Marketplace Guards**:
- **Schema validation**: Ensures pack conforms to gpack spec
- **Dependency resolution**: Prevents circular dependencies
- **License compatibility**: Warns about GPL in MIT project
- **Quality scoring**: Downloads, stars, maintenance status

---

## 5. Separating Concerns: CLI vs Domain vs Templates

### 5.1 CLI Layer Responsibilities

**What CLI Should Do**:
```rust
// ggen-cli/src/cmds/ontology.rs - THE RIGHT APPROACH
#[verb]
fn generate(schema_file: String, preset: String) -> VerbResult<GenerateOutput> {
    // 1. Load ontology (domain logic)
    let schema = OntologyExtractor::extract_from_file(&schema_file)?;

    // 2. Resolve pack (pack logic)
    let pack = PackResolver::resolve_preset(&preset)?;

    // 3. Create context (integration logic)
    let context = create_template_context(&schema);

    // 4. Generate (template logic)
    let output = TemplateEngine::render(&pack, &context)?;

    // 5. Write (file I/O)
    FileTree::write(output.files, output.directory)?;

    Ok(GenerateOutput {
        preset,
        files_generated: output.files.len(),
        output_directory: output.directory,
    })
}
```

**What CLI Should NOT Do**:
- ❌ Know about TypeScript syntax
- ❌ Implement code generation logic
- ❌ Define output file structure
- ❌ Parse ontology formats

### 5.2 Domain Layer Responsibilities

**What Domain Should Do**:
```rust
// ggen-core/src/ontology/extractor.rs
pub struct OntologyExtractor;

impl OntologyExtractor {
    /// Extract structured schema from RDF/OWL ontology
    pub fn extract(graph: &Graph, namespace: &str) -> Result<OntologySchema> {
        // Parse RDF triples
        // Query SPARQL for classes, properties
        // Build relationships
        // Return structured OntologySchema
    }
}

// ggen-core/src/ontology/schema.rs
pub struct OntologySchema {
    pub classes: Vec<OntClass>,
    pub properties: Vec<OntProperty>,
    pub relationships: Vec<OntRelationship>,
    // ... semantic structure
}
```

**What Domain Should NOT Do**:
- ❌ Generate TypeScript code
- ❌ Manage files and directories
- ❌ Know about template engines
- ❌ Handle user interaction

### 5.3 Template Layer Responsibilities

**What Templates Should Do**:
```jinja2
{# templates/typescript/types.ts.tmpl #}
{% for class in ontology.classes %}
export interface {{ class.name }} {
  {% for prop in class | properties_for_class %}
  {{ prop.name }}: {{ prop.range | to_typescript_type }};
  {% endfor %}
}
{% endfor %}
```

**What Templates Should NOT Do**:
- ❌ Parse RDF/OWL files
- ❌ Resolve dependencies
- ❌ Execute SPARQL queries (use pre-computed data)
- ❌ Validate ontology structure

---

## 6. Identifying "Old Thinking" in Other Parts of ggen

### 6.1 Red Flags to Watch For

#### Code Generation in CLI
```rust
// ❌ WRONG: CLI generates code directly
#[verb]
fn create_api(name: String) -> Result<()> {
    let code = format!("
        pub fn {}() {{
            // API implementation
        }}
    ", name);
    std::fs::write("api.rs", code)?;
}

// ✅ RIGHT: CLI uses templates
#[verb]
fn create_api(name: String) -> Result<()> {
    let pack = PackResolver::resolve("api-template")?;
    let context = json!({ "api_name": name });
    TemplateEngine::render(&pack, &context)?;
}
```

#### Hardcoded Output Formats
```rust
// ❌ WRONG: Match on format strings
match format.as_str() {
    "json" => serialize_json(&data)?,
    "yaml" => serialize_yaml(&data)?,
    "toml" => serialize_toml(&data)?,
    _ => bail!("Unsupported format"),
}

// ✅ RIGHT: Use serializer packs
let serializer = SerializerResolver::resolve(&format)?;
serializer.serialize(&data)?;
```

#### Business Logic in Templates
```jinja2
{# ❌ WRONG: Complex logic in template #}
{% if database == "postgres" %}
  {% set driver = "tokio-postgres" %}
  {% set connection = "postgresql://..." %}
{% elsif database == "mysql" %}
  {% set driver = "tokio-mysql" %}
  {% set connection = "mysql://..." %}
{% endif %}

{# ✅ RIGHT: Logic in domain, data in template #}
Database: {{ database.driver }}
Connection: {{ database.connection_string }}
```

### 6.2 Audit Checklist

For each subsystem, ask:

1. **Is this generating code/text directly?**
   - If yes → Should it use templates instead?

2. **Is this hardcoding formats/languages?**
   - If yes → Should it use packs/plugins instead?

3. **Is this duplicating template engine logic?**
   - If yes → Should it leverage Tera/template infrastructure?

4. **Can users extend this without changing Rust code?**
   - If no → Should it be template-driven?

5. **Is this tied to marketplace/discovery?**
   - If no → Should it be a pack instead of built-in?

---

## 7. The Philosophy Behind "Ontologies as Packs"

### 7.1 First-Class Citizenship

**Principle**: If users care about it, make it a first-class entity.

**Applied to Ontologies**:
- Users want to share ontology templates → Make ontologies packs
- Users want to discover ontology solutions → Integrate with marketplace
- Users want to compose ontologies → Support pack dependencies

### 7.2 Convention Over Configuration (with Escape Hatches)

**Pack Conventions**:
```
pack/
├── gpack.toml                 # Manifest (minimal required)
├── templates/                 # Convention: templates here
│   ├── **/*.tmpl             # Convention: .tmpl extension
│   └── graphs/               # Convention: RDF data location
│       └── ontology.ttl
└── README.md                  # Convention: documentation
```

**Escape Hatches**:
```toml
[templates]
patterns = ["custom/**/*.template"]  # Override convention

[rdf]
patterns = ["data/**/*.rdf"]         # Override convention
```

### 7.3 Composition Over Inheritance

**Inheritance (Brittle)**:
```rust
trait OntologyGenerator {
    fn generate_interfaces(&self) -> Result<String>;
    fn generate_schemas(&self) -> Result<String>;
}

struct TypeScriptGenerator: OntologyGenerator { ... }
struct PythonGenerator: OntologyGenerator { ... }
```

**Composition (Flexible)**:
```toml
[dependencies]
"org.schema.core" = "^1.0"        # Base ontology
"ts.types" = "^2.0"               # TypeScript types
"ts.zod" = "^1.0"                 # Zod validation
"react.hooks" = "^3.0"            # React hooks
```

Users compose packs like Lego blocks.

### 7.4 Data-Driven Over Code-Driven

**Code-Driven (Inflexible)**:
```rust
impl TypeScriptGenerator {
    fn to_typescript_type(range: &PropertyRange) -> String {
        match range {
            PropertyRange::String => "string",
            PropertyRange::Integer => "number",
            // ... 20 more cases
        }
    }
}
```

**Data-Driven (Configurable)**:
```toml
[type_mappings.typescript]
String = "string"
Integer = "number"
Float = "number"
Boolean = "boolean"
DateTime = "Date"
Reference = "{ id: string }"
```

Templates consume data, not code.

---

## 8. Scaling to Other Extensible Systems

### 8.1 Database Migrations

**Old Thinking**:
```bash
ggen database create-migration "add_users_table" --database postgres
# Hardcoded SQL generation for Postgres
```

**New Thinking**:
```bash
ggen pack install postgres-migrations
ggen pack install flyway-formatter
ggen generate schema.ttl --preset postgres-migration
```

### 8.2 API Generators

**Old Thinking**:
```rust
fn generate_rest_api(schema: &Schema) -> Result<String> {
    // Hardcoded REST logic in Rust
}
```

**New Thinking**:
```bash
ggen pack install rest-api-axum
ggen pack install graphql-federation
ggen pack install grpc-tonic

ggen generate schema.ttl --preset rest-api
ggen generate schema.ttl --preset graphql
ggen generate schema.ttl --preset grpc
```

### 8.3 Documentation Generators

**Old Thinking**:
```rust
match format {
    "markdown" => generate_markdown(&docs)?,
    "html" => generate_html(&docs)?,
}
```

**New Thinking**:
```bash
ggen pack install mdbook-templates
ggen pack install docusaurus-templates
ggen pack install sphinx-templates
```

### 8.4 Universal Pattern

For any extensible system:

1. **Identify the extension point**
   - What do users want to customize?
   - What formats/outputs are needed?

2. **Create pack interface**
   - Define manifest schema (gpack.toml)
   - Define template conventions
   - Define context data structure

3. **Implement marketplace integration**
   - Discovery (search, browse)
   - Installation (fetch, cache)
   - Composition (dependencies)

4. **Document best practices**
   - Template authoring guide
   - Pack publishing guide
   - Example packs

---

## 9. Implementation Roadmap

### Phase 1: Core Infrastructure (Week 1)
1. Define `OntologyContext` struct for template rendering
2. Implement `OntologyExtractor::to_template_context()` converter
3. Create base ontology pack structure (`templates/ontology/base/`)
4. Add `ggen ontology init --pack` command

### Phase 2: Template Packs (Week 2)
1. Create `schema-org-typescript` pack
2. Create `schema-org-python` pack
3. Create `schema-org-graphql` pack
4. Publish packs to local registry

### Phase 3: Marketplace Integration (Week 3)
1. Add ontology category to marketplace
2. Implement `ggen search --category ontology`
3. Implement `ggen pack install <ontology-pack>`
4. Add quality scoring for ontology packs

### Phase 4: Migration (Week 4)
1. Deprecate `ggen ontology generate --language typescript`
2. Create migration guide: old commands → new pack-based workflow
3. Archive `TypeScriptGenerator` with deprecation notice
4. Update documentation

### Phase 5: Community Enablement (Ongoing)
1. Publish pack authoring guide
2. Create example ontology packs
3. Host pack creation workshops
4. Monitor marketplace for community packs

---

## 10. Lessons Learned: Universal Principles

### 10.1 Architecture Principles

1. **Inversion of Control**
   - Don't call code generation → Let templates consume data
   - Don't hardcode formats → Let packs define formats

2. **Open-Closed Principle**
   - Open for extension (create new packs)
   - Closed for modification (don't change CLI/core)

3. **Separation of Concerns**
   - CLI orchestrates, doesn't generate
   - Domain extracts, doesn't format
   - Templates format, don't extract

4. **Data-Driven Design**
   - Configuration over code
   - Templates over generators
   - Composition over inheritance

### 10.2 Product Principles

1. **User Empowerment**
   - Users shouldn't wait for core team
   - Users can solve niche problems
   - Users can share solutions

2. **Network Effects**
   - Marketplace creates discovery
   - Downloads signal quality
   - Community creates value

3. **Graceful Evolution**
   - Old packs still work
   - New packs add features
   - Breaking changes are opt-in

### 10.3 Code Quality Principles

1. **Testability**
   - Templates are easily tested (input → output)
   - Domain logic is pure functions
   - CLI is thin integration layer

2. **Maintainability**
   - Template bugs don't require Rust changes
   - New formats don't require code review
   - Community maintains packs, core maintains engine

3. **Performance**
   - Template rendering is cached
   - Pack resolution is lazy
   - Parallel generation possible

---

## 11. Conclusion

### The Core Lesson

**Direct code generation is an anti-pattern because**:
1. It hardcodes business logic in presentation layer
2. It prevents user extension without core changes
3. It duplicates infrastructure already built (templates)
4. It ignores existing marketplace/discovery systems
5. It violates separation of concerns

**Templates are foundational because**:
1. They separate "what" (data) from "how" (format)
2. They enable composition and reuse
3. They empower community extension
4. They leverage proven infrastructure
5. They scale to unlimited formats

### The 2029 Mindset

In 2029, successful systems are:
- **Data-driven**: Logic in data, not code
- **Composable**: Packs combine like Lego blocks
- **Discoverable**: Marketplace enables finding solutions
- **Extensible**: Users add features without forking
- **Maintainable**: Core focuses on engine, community on content

### Call to Action

**For ggen developers**:
1. Audit every code generator → Convert to templates
2. Audit every hardcoded format → Convert to packs
3. Audit every special case → Make first-class if users care

**For ggen users**:
1. Don't fork to add features → Create packs
2. Don't wait for core team → Build and share packs
3. Don't reinvent → Search marketplace first

### Final Thought

The ontology CLI was built with good intentions—generate TypeScript types from ontologies. But by implementing it as direct code generation instead of template-driven packs, it:
- Limited itself to one language
- Bypassed proven infrastructure
- Required core changes for extensions
- Isolated itself from marketplace

By rebuilding as ontology packs, we:
- Enable unlimited languages/frameworks
- Reuse template/marketplace infrastructure
- Empower community to extend
- Create network effects through discovery

**This is the 2029 way: Composition over code, templates over generators, packs over built-ins.**

---

**Document Version**: 1.0.0
**Last Updated**: 2025-11-18
**Author**: ggen Architecture Team
**Status**: Reference Architecture
