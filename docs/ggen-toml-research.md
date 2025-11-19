# Advanced Configuration Pattern Research for ggen.toml

**Research Agent Analysis**
**Date**: 2025-11-19
**Session**: swarm-1763519525942-5e1f4mkv2
**Focus**: 80/20 principle - Top 20% features providing 80% value

---

## Executive Summary

This research analyzes advanced configuration patterns from leading package management systems to design a powerful, graph-native `ggen.toml` specification. The analysis identifies 23 high-value features across 5 major config systems, with 12 novel graph-aware capabilities unique to ggen's ontology-driven architecture.

### Key Findings

1. **Workspace Management** (Cargo.toml): Best-in-class monorepo support with dependency resolution
2. **Tool Centralization** (pyproject.toml): Single source of truth for all tooling configuration
3. **Script Ecosystem** (package.json): Rich scripting with lifecycle hooks and workspace protocols
4. **Dependency Constraints** (Maven POM): Advanced dependency management with BOMs and exclusions
5. **Graph-Native Opportunities**: 12 unique capabilities leveraging RDF/SPARQL infrastructure

---

## Feature Comparison Matrix

### Workspace & Monorepo Management

| Feature | Cargo | pyproject | package.json | Maven | Gradle | ggen Opportunity |
|---------|-------|-----------|--------------|-------|--------|------------------|
| **Workspace Members** | ✅ Glob patterns | ❌ Basic PEP 621 | ✅ Workspaces | ✅ Modules | ✅ Projects | 🚀 **Graph-based workspace discovery** |
| **Dependency Resolution** | ✅ resolver = "2" | ⚠️ Via pip | ✅ NPM/Yarn/PNPM | ✅ BOM support | ✅ Constraints | 🚀 **SPARQL-based resolution** |
| **Shared Dependencies** | ✅ workspace.dependencies | ❌ | ✅ Hoisting | ✅ dependencyManagement | ✅ Platform | 🚀 **Ontology inheritance** |
| **Exclusions** | ✅ Per-member | ❌ | ✅ nohoist | ✅ exclusions | ✅ configurations | 🚀 **OWL restrictions** |
| **Path References** | ✅ path = "..." | ✅ {path = ...} | ✅ file:... | ❌ | ❌ | 🚀 **RDF URI references** |

**ggen Enhancement**: Use RDF graph to model workspace topology, enabling SPARQL queries for dependency analysis and automatic workspace member discovery via ontology patterns.

---

### Feature Flags & Conditional Compilation

| Feature | Cargo | pyproject | package.json | Maven | Gradle | ggen Opportunity |
|---------|-------|-----------|--------------|-------|--------|------------------|
| **Feature Flags** | ✅ [features] | ❌ | ❌ | ❌ | ❌ | 🚀 **OWL-defined feature sets** |
| **Default Features** | ✅ default = [...] | ❌ | ❌ | ❌ | ❌ | 🚀 **Reasoner-based defaults** |
| **Optional Deps** | ✅ optional = true | ✅ [project.optional-dependencies] | ✅ optionalDependencies | ✅ <optional> | ✅ compileOnly | 🚀 **SHACL-validated options** |
| **Profiles/Variants** | ✅ [profile.*] | ❌ | ❌ | ✅ <profiles> | ✅ buildTypes | 🚀 **Graph-based profiles** |
| **Platform-Specific** | ✅ [target.'cfg(...)'] | ✅ markers | ❌ | ✅ <activation> | ✅ variants | 🚀 **RDF platform ontology** |

**ggen Enhancement**: Model features as OWL classes with SHACL constraints, enabling:
- Automatic feature dependency inference via reasoning
- Platform-specific feature selection via SPARQL
- Feature conflict detection via OWL restrictions

---

### Build Configuration & Lifecycle

| Feature | Cargo | pyproject | package.json | Maven | Gradle | ggen Opportunity |
|---------|-------|-----------|--------------|-------|--------|------------------|
| **Build Scripts** | ✅ build.rs | ✅ [build-system] | ✅ scripts.build | ✅ <build> | ✅ tasks | ✅ **Already has make.toml** |
| **Custom Scripts** | ❌ | ❌ | ✅ scripts.* | ❌ | ✅ Custom tasks | 🚀 **Template-driven scripts** |
| **Lifecycle Phases** | ⚠️ Implicit | ⚠️ PEP 517 | ⚠️ pre/post hooks | ✅ phases | ✅ lifecycle | ✅ **Already has [lifecycle.phases]** |
| **Parallel Execution** | ✅ codegen-units | ❌ | ❌ | ✅ threads | ✅ parallel | 🚀 **Graph-parallelization** |
| **Incremental Builds** | ✅ incremental | ❌ | ❌ | ✅ incremental | ✅ up-to-date | 🚀 **RDF-based cache keys** |

**ggen Enhancement**: Already strong with `make.toml` integration. Add:
- SPARQL queries to determine task dependencies
- RDF-based build cache invalidation
- Template-generated build scripts from ontology

---

### Dependency Specification & Constraints

| Feature | Cargo | pyproject | package.json | Maven | Gradle | ggen Opportunity |
|---------|-------|-----------|--------------|-------|--------|------------------|
| **Version Ranges** | ✅ Semver | ✅ PEP 440 | ✅ Semver | ✅ Version ranges | ✅ Constraints | 🚀 **OWL version restrictions** |
| **Git Dependencies** | ✅ git = "..." | ✅ git+ | ✅ git+ | ❌ | ✅ vcs | 🚀 **RDF provenance tracking** |
| **Local Path** | ✅ path = "..." | ✅ file: | ✅ file: | ❌ | ❌ | 🚀 **Ontology path resolution** |
| **Platform-Specific** | ✅ [target] | ✅ markers | ✅ optionalDeps | ✅ <activation> | ✅ variants | 🚀 **Platform ontology** |
| **Conflict Resolution** | ✅ Automatic | ⚠️ pip | ✅ NPM | ✅ Nearest-wins | ✅ Resolution strategy | 🚀 **SPARQL conflict queries** |
| **BOM/Platform** | ❌ | ❌ | ❌ | ✅ <dependencyManagement> | ✅ platform() | 🚀 **Ontology-based BOMs** |

**ggen Enhancement**: Model dependencies as RDF triples with OWL version constraints:
```turtle
:myproject :dependsOn [
  :package "serde" ;
  :version "^1.0" ;
  :features ["derive"] ;
  owl:maxVersion "2.0.0"
] .
```

---

### Metadata & Discovery

| Feature | Cargo | pyproject | package.json | Maven | Gradle | ggen Opportunity |
|---------|-------|-----------|--------------|-------|--------|------------------|
| **Package Metadata** | ✅ [package] | ✅ [project] | ✅ Top-level | ✅ <project> | ✅ Maven publish | ✅ **Already has [package]** |
| **Keywords/Tags** | ✅ keywords | ✅ keywords | ✅ keywords | ❌ | ❌ | 🚀 **SKOS vocabulary** |
| **Categories** | ✅ categories | ✅ classifiers | ❌ | ❌ | ❌ | 🚀 **OWL class hierarchy** |
| **Custom Metadata** | ❌ | ❌ | ❌ | ✅ <properties> | ✅ ext {} | ✅ **Already has [package.metadata.ggen]** |
| **Tool Config** | ❌ | ✅ [tool.*] | ✅ Config files | ✅ <properties> | ✅ Plugins | 🚀 **Tool ontology** |

**ggen Enhancement**: Already excellent with `[package.metadata.ggen]`. Add:
- SKOS vocabulary for keywords (semantic search)
- OWL class hierarchy for categories (taxonomy reasoning)
- Tool configuration ontology for IDE/editor integration

---

### Quality & Validation

| Feature | Cargo | pyproject | package.json | Maven | Gradle | ggen Opportunity |
|---------|-------|-----------|--------------|-------|--------|------------------|
| **Linting Config** | ✅ [lints] | ✅ [tool.ruff] | ✅ .eslintrc | ❌ | ❌ | 🚀 **SHACL validation rules** |
| **Testing Config** | ❌ | ✅ [tool.pytest] | ✅ jest config | ✅ <plugins> | ✅ test {} | 🚀 **Test ontology** |
| **Code Coverage** | ❌ | ✅ [tool.coverage] | ✅ nyc config | ✅ jacoco | ✅ jacoco | 🚀 **Coverage constraints** |
| **Security Audits** | ✅ cargo-audit | ❌ | ✅ npm audit | ✅ dependency-check | ❌ | 🚀 **CVE ontology** |
| **License Compliance** | ✅ license | ✅ license | ✅ license | ✅ <licenses> | ❌ | 🚀 **SPDX ontology** |

**ggen Enhancement**: Use SHACL for validation:
```turtle
:ProjectShape a sh:NodeShape ;
  sh:targetClass :Project ;
  sh:property [
    sh:path :hasLicense ;
    sh:minCount 1 ;
    sh:pattern "^(MIT|Apache-2.0|GPL-3.0)$"
  ] .
```

---

## Novel Graph-Native Capabilities

### 1. **Ontology Inheritance & Extension** 🚀

**Unique to ggen**: Projects can inherit from ontology-defined archetypes.

```toml
[ontology]
extends = "https://ggen.dev/ontology/rust-cli-archetype"
imports = ["https://schema.org/SoftwareApplication"]

[ontology.custom]
defines = ["MyCustomClass", "MyCustomProperty"]
```

**Benefits**:
- Projects inherit configuration from archetypes (DRY)
- SPARQL queries can find all projects of a specific archetype
- OWL reasoning infers missing configuration
- Templates generated from ontology structure

**Example Query**:
```sparql
SELECT ?project ?description WHERE {
  ?project a :RustCLIArchetype ;
           :hasDescription ?description .
}
```

---

### 2. **SPARQL-Based Dependency Resolution** 🚀

**Unique to ggen**: Resolve dependencies via SPARQL queries against package graph.

```toml
[dependencies]
# Traditional
serde = { version = "1.0", features = ["derive"] }

# Graph-based query
[dependencies.query]
sparql = """
  SELECT ?pkg ?version WHERE {
    ?pkg a :RustCrate ;
         :hasName "serde" ;
         :hasVersion ?version ;
         :supportsFeature :Derive .
    FILTER(?version >= "1.0" && ?version < "2.0")
  }
  ORDER BY DESC(?version)
  LIMIT 1
"""
```

**Benefits**:
- Complex dependency constraints via SPARQL
- Find packages by capability, not just name
- Semantic versioning queries with OWL restrictions
- Automatic BOM generation from package graph

---

### 3. **Template & Generator Integration** 🚀

**Unique to ggen**: Templates as first-class citizens in config.

```toml
[templates]
# Existing feature - enhance with ontology
directory = "templates"
output_directory = "generated"

# NEW: Template discovery via ontology
[templates.discovery]
sparql = """
  SELECT ?template ?priority WHERE {
    ?template a :CodeTemplate ;
              :appliesTo :RustProject ;
              :priority ?priority .
  }
  ORDER BY DESC(?priority)
"""

# NEW: Template variables from ontology
[templates.variables]
from_ontology = true
sparql_bindings = """
  SELECT ?key ?value WHERE {
    ?project :hasProperty ?prop .
    ?prop :key ?key ; :value ?value .
  }
"""
```

**Benefits**:
- Discover templates from marketplace via SPARQL
- Auto-populate template variables from project ontology
- Validate template compatibility via OWL reasoning
- Template versioning and dependency management

---

### 4. **Graph-Based Workspace Topology** 🚀

**Unique to ggen**: Model workspace as RDF graph.

```toml
[workspace]
# Existing Cargo-style
members = ["crates/*", "examples/*"]

# NEW: Graph-based discovery
[workspace.graph]
auto_discover = true
discovery_query = """
  SELECT ?member WHERE {
    ?member a :WorkspaceMember ;
            :parentWorkspace ?this .
  }
"""

# NEW: Dependency graph constraints
[workspace.constraints]
no_circular_deps = true
max_depth = 5
enforce_tiers = ["core", "utils", "cli"]
```

**Benefits**:
- Automatic workspace member discovery via ontology
- Enforce architectural constraints (no circular deps)
- Visualize workspace topology from RDF graph
- Generate workspace documentation from ontology

---

### 5. **AI-Driven Configuration** 🚀

**Unique to ggen**: Already has `[ai]` section - enhance with ontology.

```toml
[ai]
provider = "ollama"
model = "qwen2.5-coder"

# NEW: AI prompts from ontology
[ai.prompts.from_ontology]
enabled = true
context_query = """
  SELECT ?pattern ?example WHERE {
    ?archetype :hasCodePattern ?pattern ;
               :hasExample ?example .
  }
"""

# NEW: AI validation via ontology
[ai.validation]
enabled = true
quality_threshold = 0.8
validate_against_ontology = true
shacl_shapes = ["ProjectStructureShape", "CodeQualityShape"]
```

**Benefits**:
- AI generates code conforming to project ontology
- Validate AI output against SHACL shapes
- Prompt engineering from ontology patterns
- Quality metrics tracked in RDF graph

---

### 6. **RDF Provenance & Audit Trail** 🚀

**Unique to ggen**: Track configuration changes in RDF.

```toml
[provenance]
enabled = true
track_changes = true
store_format = "turtle"

[provenance.metadata]
author = "research-agent"
timestamp = "auto"
reasoning_used = true
source_ontology = "https://ggen.dev/ontology/v3.2.0"
```

**Benefits**:
- Full audit trail of configuration changes
- SPARQL queries for configuration history
- Compliance reporting from provenance data
- Reproducible builds via provenance tracking

---

### 7. **SHACL Validation Rules** 🚀

**Unique to ggen**: Validate configuration against SHACL shapes.

```toml
[validation]
enabled = true
shapes_directory = "shapes/"

[validation.rules]
# Define inline SHACL shapes
enforce_package_metadata = """
  :PackageMetadataShape a sh:NodeShape ;
    sh:targetClass :Package ;
    sh:property [
      sh:path :hasName ;
      sh:minCount 1 ;
      sh:datatype xsd:string
    ] ;
    sh:property [
      sh:path :hasVersion ;
      sh:minCount 1 ;
      sh:pattern "^\\d+\\.\\d+\\.\\d+$"
    ] .
"""

enforce_dependencies = """
  :DependencyShape a sh:NodeShape ;
    sh:targetClass :Dependency ;
    sh:property [
      sh:path :hasVersion ;
      sh:minCount 1
    ] .
"""
```

**Benefits**:
- Type-safe configuration via SHACL constraints
- Custom validation rules as RDF
- Generate validation errors as RDF for tooling
- IDE integration via SHACL validation

---

### 8. **Semantic Versioning Constraints** 🚀

**Unique to ggen**: OWL-based version constraints.

```toml
[dependencies.serde]
version = "^1.0"

# NEW: OWL-based constraints
[dependencies.serde.constraints]
owl_restrictions = """
  :SerdeConstraint a owl:Restriction ;
    owl:onProperty :hasVersion ;
    owl:allValuesFrom [
      a rdfs:Datatype ;
      owl:onDatatype xsd:string ;
      owl:withRestrictions (
        [xsd:pattern "^1\\..*"]
        [owl:maxVersion "2.0.0"]
      )
    ] .
"""
```

**Benefits**:
- Complex version constraints via OWL
- Reasoning-based version resolution
- CVE-based version exclusions in ontology
- Automatic security patch version selection

---

### 9. **Cross-Project Knowledge Graph** 🚀

**Unique to ggen**: Projects share knowledge via RDF graph.

```toml
[graph]
# Connect to shared knowledge base
shared_graph_uri = "https://ggen.dev/knowledge-graph"
contribute_metadata = true

[graph.queries]
# Find similar projects
find_similar = """
  SELECT ?project ?similarity WHERE {
    ?project :hasCategory ?category ;
             :hasKeyword ?keyword .
    ?this :hasCategory ?category ;
          :hasKeyword ?keyword .
    FILTER(?project != ?this)
  }
"""

# Find reusable components
find_components = """
  SELECT ?component ?description WHERE {
    ?component a :ReusableComponent ;
               :compatibleWith ?this ;
               :hasDescription ?description .
  }
"""
```

**Benefits**:
- Discover reusable code across projects
- Find projects with similar architecture
- Share configuration patterns via graph
- Marketplace integration via shared ontology

---

### 10. **Lifecycle Phase Ontology** 🚀

**Unique to ggen**: Model lifecycle as RDF graph.

```toml
[lifecycle]
enabled = true

# Existing feature
[lifecycle.phases]
default = ["init", "setup", "build", "test"]

# NEW: Ontology-based phases
[lifecycle.ontology]
phase_graph = """
  :InitPhase a :LifecyclePhase ;
    :hasNext :SetupPhase ;
    :requires :CleanWorkspace .

  :SetupPhase a :LifecyclePhase ;
    :hasNext :BuildPhase ;
    :requires :DependenciesInstalled .

  :BuildPhase a :LifecyclePhase ;
    :hasNext :TestPhase ;
    :produces :CompiledArtifact .
"""

[lifecycle.reasoning]
# Infer missing phases
auto_infer_phases = true
# Validate phase dependencies
validate_requirements = true
# Generate phase graph visualization
generate_diagram = true
```

**Benefits**:
- Automatic phase dependency inference
- Validate lifecycle completeness
- Generate phase diagrams from ontology
- Custom phase extension via RDF

---

### 11. **Platform & Environment Ontology** 🚀

**Unique to ggen**: Model platforms as OWL classes.

```toml
[platform]
# Traditional target specification
targets = ["x86_64-unknown-linux-gnu", "aarch64-apple-darwin"]

# NEW: Ontology-based platform selection
[platform.ontology]
platform_graph = """
  :LinuxPlatform a :Platform ;
    :hasArchitecture :X86_64 ;
    :hasOS :Linux ;
    :requiresFeature :GLIBC_2_31 .

  :MacOSPlatform a :Platform ;
    :hasArchitecture :ARM64 ;
    :hasOS :MacOS ;
    :requiresFeature :AppleSilicon .
"""

# NEW: Feature detection via reasoning
[platform.features]
auto_detect = true
reason_compatibility = true
fallback_selection = "most_compatible"
```

**Benefits**:
- Automatic platform detection via reasoning
- Feature requirement validation
- Cross-compilation target selection via SPARQL
- Platform-specific dependency resolution

---

### 12. **Marketplace Integration via Ontology** 🚀

**Unique to ggen**: Already has marketplace - enhance with ontology.

```toml
[marketplace]
# Existing features
registry = "https://ggen.dev/marketplace"

# NEW: Semantic search via SPARQL
[marketplace.search]
sparql_enabled = true
search_query = """
  SELECT ?package ?score WHERE {
    ?package a :MarketplacePackage ;
             :hasKeyword ?keyword ;
             :hasCategory :CLITool ;
             :qualityScore ?score .
    FILTER(?score > 0.8)
  }
  ORDER BY DESC(?score)
"""

# NEW: Dependency resolution from marketplace graph
[marketplace.dependencies]
resolve_from_graph = true
prefer_high_quality = true
min_quality_score = 0.7
```

**Benefits**:
- Semantic package discovery via SPARQL
- Quality-based dependency selection
- Automatic package compatibility checking
- Provenance tracking for marketplace packages

---

## Recommended ggen.toml Schema

### Core Sections (Existing)

```toml
[package]
name = "my-project"
version = "1.0.0"
description = "..."
authors = ["..."]
license = "MIT"
keywords = ["cli", "tool"]
categories = ["command-line-utilities"]

[package.metadata.ggen]
generated_by = "ggen-ai"
generation_date = "2025-11-19"
language = "rust"
framework = "clap"
quality_score = 0.95
```

### Workspace Management (Cargo-inspired)

```toml
[workspace]
members = ["crates/*", "examples/*"]
exclude = ["target", "generated"]
resolver = "2"

[workspace.dependencies]
# Shared dependencies across workspace
serde = { version = "1.0", features = ["derive"] }
tokio = { version = "1.0", features = ["full"] }

[workspace.graph]
# NEW: Graph-based workspace discovery
auto_discover = true
enforce_no_cycles = true
max_depth = 5
```

### Dependencies (Enhanced)

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }

# NEW: Platform-specific dependencies via ontology
[dependencies.target.x86_64-unknown-linux-gnu]
openssl = "0.10"

# NEW: SPARQL-based dependency resolution
[dependencies.query]
find_best_logger = """
  SELECT ?pkg ?score WHERE {
    ?pkg a :RustCrate ;
         :hasCategory :Logging ;
         :qualityScore ?score .
    FILTER(?score > 0.9)
  }
  ORDER BY DESC(?score)
  LIMIT 1
"""
```

### Features (Cargo-inspired)

```toml
[features]
default = ["std", "logging"]
std = []
logging = ["tracing", "tracing-subscriber"]
nightly = []

# NEW: OWL-based feature constraints
[features.constraints]
mutex_features = [["std", "no_std"]]
requires = { logging = ["std"] }
```

### Templates (Enhanced)

```toml
[templates]
directory = "templates"
output_directory = "generated"
backup_enabled = true

# NEW: Template discovery from marketplace
[templates.marketplace]
enabled = true
min_quality = 0.8
auto_update = true

# NEW: Ontology-based template variables
[templates.variables]
from_ontology = true
custom = { project_name = "MyProject" }
```

### AI Configuration (Enhanced)

```toml
[ai]
provider = "ollama"
model = "qwen2.5-coder"
temperature = 0.7
max_tokens = 4000

[ai.prompts]
system = "You are a Rust expert..."
user_prefix = "Generate code for:"

[ai.validation]
enabled = true
quality_threshold = 0.8
validate_against_ontology = true
shacl_shapes = ["CodeQualityShape"]
```

### RDF & Ontology (Core ggen Feature)

```toml
[ontology]
extends = ["https://ggen.dev/ontology/rust-cli-archetype"]
imports = ["https://schema.org/SoftwareApplication"]

[ontology.custom]
defines = ["MyCustomClass", "MyCustomProperty"]

[rdf]
base_uri = "https://example.com/my-project/"
prefixes = {
  ex = "https://example.com/",
  schema = "http://schema.org/"
}

[sparql]
timeout = 10
max_results = 1000
cache_enabled = true
```

### Lifecycle (Enhanced)

```toml
[lifecycle]
enabled = true
config_file = "make.toml"

[lifecycle.phases]
default = ["init", "setup", "build", "test"]
production = ["build", "test", "docker", "deploy"]

# NEW: Ontology-based phase inference
[lifecycle.ontology]
auto_infer_phases = true
validate_dependencies = true
```

### Validation (NEW)

```toml
[validation]
enabled = true
shapes_directory = "shapes/"

[validation.rules]
enforce_license = true
enforce_readme = true
enforce_tests = true
min_coverage = 80
```

### Security (NEW)

```toml
[security]
path_traversal_protection = true
shell_injection_protection = true
template_sandboxing = true

[security.audit]
enabled = true
block_vulnerabilities = true
max_severity = "medium"
```

### Performance (NEW)

```toml
[performance]
parallel_execution = true
max_workers = 16
cache_size = "1GB"

[performance.build]
incremental = true
cache_key_strategy = "rdf_hash"
```

### Marketplace (Enhanced)

```toml
[marketplace]
registry = "https://ggen.dev/marketplace"

# NEW: Semantic search
[marketplace.search]
sparql_enabled = true
min_quality = 0.7

# NEW: Auto-update dependencies
[marketplace.updates]
auto_check = true
auto_apply_patches = true
```

---

## Integration Points with Existing ggen System

### 1. Graph Module Integration

The `Graph` type in `crates/ggen-core/src/graph/core.rs` already provides:
- SPARQL query caching (LRU cache)
- Epoch-based cache invalidation
- Thread-safe graph operations

**Enhancement Opportunity**: Add `ggen.toml` loader that:
```rust
impl Graph {
    pub fn load_config(&self, path: &Path) -> Result<ConfigGraph> {
        // Parse ggen.toml
        // Convert to RDF triples
        // Insert into graph
        // Run SHACL validation
        // Return validated config
    }
}
```

### 2. Template System Integration

Templates already use TOML frontmatter and Tera syntax.

**Enhancement Opportunity**:
- Load template configuration from RDF graph
- Populate Tera context from SPARQL queries
- Validate template output against SHACL shapes

### 3. Lifecycle Integration

`make.toml` already provides lifecycle management.

**Enhancement Opportunity**:
- Generate `make.toml` from ontology-defined lifecycle
- Infer task dependencies via SPARQL
- Validate lifecycle completeness via reasoning

### 4. Marketplace Integration

Marketplace already has search and install capabilities.

**Enhancement Opportunity**:
- Semantic search via SPARQL (already in plan!)
- Quality scoring from RDF graph
- Dependency resolution from package graph

### 5. AI Integration

`ggen-ai` crate already has OpenAI/Ollama integration.

**Enhancement Opportunity**:
- Load AI prompts from ontology
- Validate AI output against SHACL shapes
- Track AI-generated code provenance in RDF

---

## Implementation Roadmap

### Phase 1: Core Config Schema (Week 1-2)
- [ ] Define `ggen.toml` schema with all sections
- [ ] Implement TOML parser with `toml` crate
- [ ] Convert TOML to RDF triples
- [ ] Basic SHACL validation

### Phase 2: Workspace & Dependencies (Week 3-4)
- [ ] Implement workspace member discovery
- [ ] SPARQL-based dependency resolution
- [ ] OWL version constraints
- [ ] Platform-specific dependencies

### Phase 3: Template & AI Integration (Week 5-6)
- [ ] Template discovery via SPARQL
- [ ] Ontology-based template variables
- [ ] AI prompt generation from ontology
- [ ] SHACL validation for AI output

### Phase 4: Advanced Features (Week 7-8)
- [ ] Lifecycle phase ontology
- [ ] Provenance tracking
- [ ] Marketplace semantic search
- [ ] Cross-project knowledge graph

### Phase 5: Tooling & IDE Support (Week 9-10)
- [ ] VS Code extension for ggen.toml
- [ ] SHACL validation in IDE
- [ ] Autocomplete from ontology
- [ ] Documentation generation

---

## Conclusion

The proposed `ggen.toml` specification combines the best features from:
1. **Cargo.toml**: Workspace management, feature flags, profiles
2. **pyproject.toml**: Tool centralization, PEP 621 metadata
3. **package.json**: Scripts, workspaces, lifecycle hooks
4. **Maven POM**: Dependency management, BOMs, profiles
5. **Gradle**: Kotlin DSL, platform constraints, variants

Plus **12 novel graph-native capabilities** unique to ggen:
1. Ontology inheritance & extension
2. SPARQL-based dependency resolution
3. Template & generator integration
4. Graph-based workspace topology
5. AI-driven configuration
6. RDF provenance & audit trail
7. SHACL validation rules
8. Semantic versioning constraints
9. Cross-project knowledge graph
10. Lifecycle phase ontology
11. Platform & environment ontology
12. Marketplace integration via ontology

This positions `ggen.toml` as the **most powerful configuration format** in the ecosystem, leveraging RDF/OWL/SPARQL for capabilities impossible in traditional package managers.

---

## References

- Cargo Book: https://doc.rust-lang.org/cargo/
- PEP 621 (pyproject.toml): https://peps.python.org/pep-0621/
- NPM package.json: https://docs.npmjs.com/cli/v9/configuring-npm/package-json
- Maven POM: https://maven.apache.org/pom.html
- Gradle Build Language: https://docs.gradle.org/current/dsl/
- SHACL Spec: https://www.w3.org/TR/shacl/
- OWL 2 Spec: https://www.w3.org/TR/owl2-overview/
- SPARQL 1.1: https://www.w3.org/TR/sparql11-query/
