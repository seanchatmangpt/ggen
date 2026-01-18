# ggen v2.0.0 Architecture Diagrams

Quick reference for the v2.0.0 architecture design.

---

## 1. High-Level System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    ggen v2.0.0 Architecture                  │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌─────────────┐    ┌──────────────┐    ┌───────────────┐  │
│  │   CLI Layer │───>│ Domain Layer │───>│ Infrastructure│  │
│  │ (Commands)  │    │  (Business)  │    │   (Storage)   │  │
│  └─────────────┘    └──────────────┘    └───────────────┘  │
│         │                   │                     │          │
│         v                   v                     v          │
│  Auto-Discovery      Clean Logic         RDF/Templates      │
│  #[verb] attrs       No CLI deps         Frozen Sections    │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

---

## 2. Module Structure

```
ggen/
├── cli/src/
│   ├── lib.rs                  # Auto-discovery entry point
│   ├── commands/               # CLI layer (thin wrappers)
│   │   ├── utils/
│   │   │   └── doctor.rs      # #[verb] → domain::utils::doctor
│   │   ├── project/
│   │   │   ├── new.rs         # #[verb] → domain::project::new
│   │   │   └── gen.rs
│   │   └── market/
│   │       └── search.rs
│   │
│   └── domain/                # Business layer (pure logic)
│       ├── utils/
│       │   └── doctor.rs      # No clap imports!
│       ├── project/
│       │   ├── new.rs
│       │   └── gen.rs
│       └── market/
│           └── search.rs
│
└── ggen-core/src/
    ├── template/
    │   ├── engine.rs          # Tera + frozen sections
    │   └── frozen.rs          # Freeze block parser
    └── rdf/
        └── graph.rs           # Oxigraph integration
```

---

## 3. Command Execution Flow

```
User: ggen project new my-app --type rust-web
     │
     v
┌─────────────────────────────────────────────────┐
│ CLI Layer (clap-noun-verb)                      │
│ 1. Parse: noun="project", verb="new"            │
│ 2. Route: commands::project::new::run()         │
└────────────────┬────────────────────────────────┘
                 │ Validated arguments
                 v
┌─────────────────────────────────────────────────┐
│ Command Handler (commands/project/new.rs)       │
│ 1. Extract args: name, type                     │
│ 2. Call: domain::project::create_new_project()  │
└────────────────┬────────────────────────────────┘
                 │ Domain function call
                 v
┌─────────────────────────────────────────────────┐
│ Domain Layer (domain/project/new.rs)            │
│ 1. Validate business rules                      │
│ 2. Load template for "rust-web"                │
│ 3. Generate files                               │
└────────────────┬────────────────────────────────┘
                 │ Use infrastructure
                 v
┌─────────────────────────────────────────────────┐
│ Infrastructure (ggen-core)                      │
│ 1. Template engine renders                      │
│ 2. RDF graph validates                          │
│ 3. Write to filesystem                          │
└────────────────┬────────────────────────────────┘
                 │ Success
                 v
         ✅ Created project: my-app
```

---

## 4. Auto-Discovery Process

```
Startup: ggen project new my-app
     │
     v
┌──────────────────────────────────────────┐
│ auto_discover("cli/src/commands")        │
│ 1. Scan filesystem                       │
│ 2. Find all #[verb] functions            │
│ 3. Parse #[noun] from mod.rs files       │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Build Routing Table                      │
│ {                                         │
│   "utils": {                              │
│     "doctor": utils::doctor::run         │
│   },                                      │
│   "project": {                            │
│     "new": project::new::run,            │
│     "gen": project::gen::run             │
│   }                                       │
│ }                                         │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Generate Clap Subcommands                │
│ • ggen utils doctor                      │
│ • ggen project new                       │
│ • ggen project gen                       │
└──────┬───────────────────────────────────┘
       │
       v Runtime execution
       │
┌──────────────────────────────────────────┐
│ Route: "project new" → run()             │
└──────────────────────────────────────────┘
```

---

## 5. Template Generation with Frozen Sections

```
User: ggen project gen rust-service.tmpl --var name=auth
     │
     v
┌──────────────────────────────────────────┐
│ Load Template                             │
│ frozen_sections: ["impl", "tests"]       │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Check Existing File: src/auth.rs         │
│ Parse frozen blocks:                     │
│   // FREEZE START: impl                  │
│   // ... user code ...                   │
│   // FREEZE END: impl                    │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Render Template (new output)             │
│ Generate fresh struct/functions          │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Merge Frozen Sections                     │
│ Replace "impl" block in new output       │
│ with preserved user code from old file   │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Final Output: src/auth.rs                │
│ ✅ Template structure updated            │
│ ✅ User modifications preserved          │
└──────────────────────────────────────────┘
```

---

## 6. Layer Separation

```
┌────────────────────────────────────────────────────────┐
│                   CLI Layer                             │
│  • Argument parsing (clap)                             │
│  • Input validation                                     │
│  • Error formatting                                     │
│  • Routing to domain                                    │
└────────────────┬───────────────────────────────────────┘
                 │ Pure function calls
                 │ No business logic!
                 v
┌────────────────────────────────────────────────────────┐
│                 Domain Layer                            │
│  • Business logic                                       │
│  • No CLI dependencies (no clap!)                       │
│  • Testable in isolation                                │
│  • Returns DomainResult<T>                              │
└────────────────┬───────────────────────────────────────┘
                 │ Uses infrastructure
                 │
                 v
┌────────────────────────────────────────────────────────┐
│              Infrastructure Layer                       │
│  • ggen-core (template engine)                         │
│  • ggen-marketplace (registry)                         │
│  • ggen-ai (LLM providers)                             │
│  • utils (config, logging)                             │
└────────────────────────────────────────────────────────┘
```

---

## 7. RDF Integration Flow

```
Template with RDF
     │
     v
┌──────────────────────────────────────────┐
│ Parse Frontmatter                         │
│ rdf_inline: ["@prefix ...", "..."]       │
│ schema: "schema.ttl"                     │
│ validation: { shacl: "rules.ttl" }       │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Load RDF Schema                           │
│ Parse ontology definitions                │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Build RDF Graph (Oxigraph)                │
│ Insert rdf_inline triples                │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Validate RDF (SHACL)                      │
│ Check structure constraints               │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Execute SPARQL Queries                    │
│ Return bindings for template vars        │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Template Rendering (Tera)                 │
│ Inject SPARQL results                     │
│ Generate final output                     │
└──────────────────────────────────────────┘
```

---

## 8. Migration Path (v1.2.0 → v2.0.0)

```
v1.2.0 (Current)
     │
     v
┌──────────────────────────────────────────┐
│ Phase 1: Foundation                       │
│ • Create domain/ directory               │
│ • Migrate utils/doctor as POC            │
│ • Both cmds/ and domain/ active          │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Phase 2: Core Commands                    │
│ • Migrate project, market, template      │
│ • Implement frozen sections              │
│ • Test dual-layer execution              │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Phase 3: Auto-Discovery                   │
│ • Integrate clap-noun-verb               │
│ • Add #[verb] attributes                 │
│ • Test filesystem routing                │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Phase 4: Full Migration                   │
│ • Migrate remaining commands             │
│ • Add deprecation warnings               │
│ • Update documentation                   │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Phase 5: Cleanup                          │
│ • Remove cmds/ directory                 │
│ • Final testing                          │
│ • Release v2.0.0                         │
└──────────────────────────────────────────┘
```

---

## 9. Backwards Compatibility

```
v2.0.0 CLI Entry Point
     │
     v
┌──────────────────────────────────────────┐
│ enum Command {                            │
│   V2(NounVerb),        // New auto-disc  │
│   Legacy(cmds::Cmd)    // Old manual     │
│ }                                         │
└──────┬───────────────────────────────────┘
       │
       ├──> User runs: ggen project new
       │         │
       │         v Routes to V2
       │
       └──> User runs: ggen doctor (legacy)
                 │
                 v Shows deprecation warning
                 │
                 v Routes to Legacy
```

---

## 10. Frozen Section Example

**Template:**
```yaml
---
frozen_sections: ["impl", "business_logic"]
---
pub struct {{name}} { ... }

// FREEZE START: impl
impl {{name}} {
    // User modifications preserved here
}
// FREEZE END: impl
```

**Regeneration Flow:**
```
Old File (user-modified)        Template Output (fresh)
     │                                  │
     v                                  v
┌─────────────────┐           ┌─────────────────┐
│ pub struct Auth │           │ pub struct Auth │
│                 │           │                 │
│ // FREEZE: impl │           │ // FREEZE: impl │
│ impl Auth {     │           │ impl Auth {     │
│   // USER CODE  │  MERGE    │   // TEMPLATE   │
│   custom_fn()   │  ─────>   │   ...           │
│ }               │           │ }               │
│ // FREEZE END   │           │ // FREEZE END   │
└─────────────────┘           └─────────────────┘
         │                             │
         └─────────┬───────────────────┘
                   v
         ┌─────────────────────┐
         │ Final Output:        │
         │ Template structure + │
         │ User custom_fn()     │
         └─────────────────────┘
```

---

## Quick Reference

### Key Patterns

**Command Pattern:**
```rust
// commands/{noun}/{verb}.rs
#[derive(Args)]
pub struct VerbArgs { ... }

#[verb]
pub async fn run(args: &VerbArgs) -> Result<()> {
    domain::{noun}::{verb}(args).await
}
```

**Domain Pattern:**
```rust
// domain/{noun}/{verb}.rs
pub async fn business_function(...) -> DomainResult<T> {
    // No clap imports!
    // Pure business logic
}
```

**Frozen Section Pattern:**
```rust
// FREEZE START: marker
// User code preserved across regenerations
// FREEZE END: marker
```

---

**End of Diagrams Reference**
