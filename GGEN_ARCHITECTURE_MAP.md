# ggen Architecture Map: Complete Internal Structure

## Overview

ggen is a **specification-driven code generation framework** built on:
- **87 Rust crates** (monorepo architecture)
- **Layered design** from ontology → generation → execution
- **Manufacturing systems principles** (TPS: Toyota Production System)
- **Multi-agent orchestration** (A2A protocol)

---

## Core Layer Architecture (What We're Fixing)

### Layer 1: RDF/Ontology Foundation
```
ggen-ontology-core
├─ RDF loading (Turtle, RDF/XML)
├─ SPARQL query engine (Oxigraph)
└─ Entity mapping
```

### Layer 2: Generation Pipeline (ggen-core)
**File**: `crates/ggen-core/src/`

```
pipeline.rs (LINES 409-423) ✅ ALREADY IMPLEMENTS:
├─ RDF graph loading
├─ SPARQL query execution
│  └─ Returns Vec<BTreeMap<String, String>>
├─ Tera template context creation
│  └─ Passes sparql_results to templates
└─ Output file writing

Other modules:
├─ generator.rs      → Code generation orchestration
├─ parallel_generator.rs → Parallel execution
├─ template.rs       → Template rendering wrapper
├─ tera_env.rs       → Tera environment setup
├─ template_cache.rs → Template caching layer
├─ registry.rs       → Rule/template registry
└─ resolver.rs       → Dependency resolution
```

**KEY FINDING**: pipeline.rs already has working SPARQL execution! (Agent 3 discovered this)

### Layer 3: CLI Wrapper (ggen-cli)
**File**: `crates/ggen-cli/src/`

```
Currently implements:
└─ ggen sync          → Generic class-to-template rendering (NOT rule-based)

Missing implementation:
└─ ggen apply         → Rule-based selective generation (Agents 4 needs to add)
```

### Layer 4: Configuration (ggen-config, ggen-config-clap)
```
Exists: ggen-config crate
├─ Config parsing (TOML support)
├─ Clap integration (CLI arg parsing)
└─ Validation

Missing: ggen.toml actual implementation in pipeline
```

---

## Agent Task Clarification (Clear Diataxis)

### ✅ AGENT RESPONSIBILITY MATRIX

| Agent | Task | Files | Purpose | Dependencies |
|-------|------|-------|---------|--------------|
| **Agent 1** | Enhance SPARQL Executor | `ggen-core/src/pipeline.rs` | Expose query execution for rule-based generation | None (foundational) |
| **Agent 2** | Config Parser | `ggen-config/` + CLI integration | Load ggen.toml rules into memory | Uses Agent 1's interface |
| **Agent 3** | Template Binding | `ggen-core/src/template_renderer.rs` (new) | Pass SPARQL results to templates per-row | Uses Agent 1 results |
| **Agent 4** | ggen apply CLI | `ggen-cli/src/commands/apply.rs` | Implement `ggen apply --rule X` subcommand | Uses Agents 1-3 |
| **Agent 5** | Integration Tests | `ggen-core/tests/integration_test.rs` | Validate full pipeline works | Validates Agents 1-4 |

**NO OVERLAP**: Each agent has distinct files and concerns
**CLEAR FLOW**: Agent 1 → 2,3 → 4 → 5

---

## What Agents Are NOT Changing

### Off-Limits (Don't Touch):
- ❌ Workflow orchestration (ggen-workflow, ggen-workflow-43)
- ❌ AI/LLM layer (ggen-ai, ggen-dspy)
- ❌ TPS implementation (ggen-kaizen, ggen-jidoka, etc.)
- ❌ Manufacturing systems (KNHK stack)
- ❌ A2A/MCP integration (ggen-a2a-mcp)
- ❌ Business layers (API, Auth, Payments, Marketplace)
- ❌ TAI stack (Kubernetes, Resilience, etc.)

### On-Limits (Agents Will Modify):
- ✅ ggen-core (generation pipeline)
- ✅ ggen-config (configuration)
- ✅ ggen-cli (command implementation)
- ✅ ggen-ontology-core (if needed for SPARQL)

---

## Real Problem (Discovered by Agent 3)

**The infrastructure is 80% there. We just need to:**

1. **Agent 1**: Expose existing SPARQL execution as reusable function
   - `pipeline.rs` already does this (lines 409-423)
   - Just needs extraction into `sparql_executor.rs`
   - Agents 2-4 will call it

2. **Agent 2**: Parse ggen.toml and make rules available
   - Config parsing exists in ggen-config
   - Just needs to be integrated into pipeline

3. **Agent 3**: Adjust template context to pass one row at a time
   - Currently passes all rows at once
   - Need: `sparql_results = [current_row]` pattern
   - Will make templates consistent

4. **Agent 4**: Wrap it in a CLI subcommand
   - `ggen sync` already works (somewhat)
   - `ggen apply --rule X` will be selective variant

5. **Agent 5**: Validate everything works together

---

## Current State Assessment

### ✅ What Works
- SPARQL query execution (pipeline.rs lines 409-423)
- Tera template rendering
- Basic ontology loading
- File output writing

### ⚠️ What's Incomplete
- ggen.toml not actually used by CLI
- Template context passes ALL rows, not individual rows
- No rule filtering (ggen apply doesn't exist)
- No per-rule execution logic

### ❌ What's Broken
- `ggen sync` doesn't read ggen.toml rules
- Templates get empty sparql_results (because query results aren't properly passed)
- No `ggen apply` command
- Agent instructions assume work that's already done (discovery phase needed)

---

## Documentation Structure (Diataxis Framework)

### Tutorials (How to use ggen)
- Currently: None formal
- Need: "Getting started with ggen.toml"

### How-To Guides (Specific tasks)
- "Write a SPARQL query for code generation"
- "Create a Tera template that uses SPARQL results"
- "Use ggen apply to generate specific rules only"

### Reference (Architecture & API)
- **THIS DOCUMENT**: Architecture map
- ggen-core modules API docs
- SPARQL query guidelines
- Template variable reference

### Explanations (Concepts & Design)
- "Why rule-based generation matters"
- "How SPARQL results flow through templates"
- "Correct by construction philosophy"

---

## Critical Discovery

**Agent 3's Finding**: The pipeline ALREADY executes SPARQL queries and passes results to templates!

This means:
- Agents were planning to implement features that partially exist
- The real work is **integration and exposure**, not reimplementation
- `ggen sync` mostly works; it just doesn't use ggen.toml
- Fixing this is SMALLER scope than originally thought

**Revised Scope**:
- Extract/expose existing functionality (not build from scratch)
- Add configuration integration
- Create CLI wrapper for selective rule execution
- Validate with tests

---

## Next Steps for Agents

### When Agents Resume:

1. **Agent 1**: Read `ggen-core/src/pipeline.rs` lines 409-423
   - Extract SPARQL execution into reusable function
   - Don't rewrite existing code, refactor it

2. **Agent 2**: Check ggen-config crate
   - Reuse existing config parsing if it works
   - Just integrate into pipeline

3. **Agent 3**: Check how pipeline currently passes results to templates
   - Small fix to context creation
   - Make it pass one row per render (not all rows)

4. **Agent 4**: Wrap everything in CLI
   - `ggen apply` command that filters rules
   - Uses Agents 1-3

5. **Agent 5**: Test the integration
   - Full pipeline from ggen.toml → SPARQL → templates → output

**IMPORTANT**: Agents should investigate what's already there before implementing!
