# Fixture Setup Quick Reference

## What's Broken (TL;DR)

❌ **1 blocker found**: `crates/ggen-core/examples/validate_example_project.rs` references missing `examples/basic-template-generation/`

## Recommended Fixtures (Use These)

### For Simple Tests (Copy-Paste Ready)
```
examples/simple-project/        ← Simplest, fastest
examples/mcp-server-definition/ ← Single ontology
examples/rust-structs/          ← Type generation
```

### For Complex Tests
```
examples/advanced-rust-project/ ← Full Rust patterns
examples/openapi/               ← API generation
examples/database-schema/       ← Schema generation
examples/factory-paas/          ← Enterprise patterns
```

## How to Use Fixtures in Tests

### Option A: Relative Path (Simple, PWD-Dependent)
```rust
#[test]
fn test_project() {
    let manifest = std::fs::read_to_string(
        "examples/simple-project/ggen.toml"
    ).expect("fixture not found");
}
```
⚠️ Only works if test runs from workspace root

### Option B: Absolute Path (Portable, Recommended)
```rust
use std::path::PathBuf;

#[test]
fn test_project() {
    let base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture = base.join("../../examples/simple-project/ggen.toml");
    let manifest = std::fs::read_to_string(fixture)
        .expect("fixture not found");
}
```
✓ Works from any directory

## Fixture Completeness Check

Every example fixture **MUST** have:
```
examples/NAME/
├── ggen.toml              ← Required
├── ontology.ttl OR        ← Required (either file or dir)
├── ontology/
│   └── *.ttl
└── templates/             ← Required
    └── *.tera
```

## All 30 Complete Fixtures (Ready to Use)

```
a2a-agent-definition
a2a-groq-agent
advanced-rust-project
ai-code-generation
ai-microservice
cli-noun-verb
comprehensive-rust-showcase
config-generator
database-schema
factory-paas
gcp-erlang-autonomics
graphql-schema
grpc-service
llm-full-integration
mcp-a2a-self-hosting
mcp-server-definition
microservices-architecture
middleware-stack
nextjs-openapi-sqlite-shadcn-vitest
observable-agent
openapi-variants
openapi
rest-api-advanced
rust-structs
self-play
simple-project
thesis-gen
validation-schemas
weaver-semantic-conventions
workspace-project
```

## Incomplete Fixtures (Avoid)

These 20 examples are **incomplete** and will be skipped by tests:

```
_archive
_shared_templates
7-agent-validation
a2a-agent-lifecycle (missing ggen.toml)
a2a-rs-agents
advanced-ai-usage
advanced-cache-registry
advanced-cli-tool
advanced-error-handling
advanced-fullstack-integration
advanced-pipeline
advanced-rust-api-8020
ai-template-project
archive_2025
bree-semantic-scheduler
clap-noun-verb-demo
cli-advanced
demo-project (missing ggen.toml)
distributed-consensus (missing ggen.toml)
e2e-agent-workflow
... (20 total)
```

## Playground Fixtures

Stable test fixtures in `/playground/`:
- `playground/sync-foundation/` ← Used by `gall_sync_actuation` test
- `playground/thesis-ontology.ttl` ← Thesis generation example
- `playground/proof/broken-construct.rq` ← Negative test case

## Fixing the Blocker

### Issue
File: `crates/ggen-core/examples/validate_example_project.rs:11-12`
```rust
let manifest_path = PathBuf::from("examples/basic-template-generation/ggen.toml");
```

### Fix
Replace with:
```rust
let manifest_path = PathBuf::from("examples/simple-project/ggen.toml");
let base_path = PathBuf::from("examples/simple-project");
```

Or use absolute path:
```rust
let base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
let manifest_path = base.join("../../examples/simple-project/ggen.toml");
```

## Testing Fixture Paths

Run this to verify fixtures are accessible:
```bash
# From workspace root
cargo test --lib 2>&1 | grep -E "examples/|playground/"

# Validate specific fixture
ls -la examples/simple-project/ggen.toml
ls -la playground/sync-foundation/
```

## Checklist for New Tests

When writing a new test that uses fixtures:

- [ ] Pick a complete fixture from the 30-example list above
- [ ] Use absolute path with `env!("CARGO_MANIFEST_DIR")`
- [ ] Test has skip logic if fixture is missing
- [ ] Test documentation mentions fixture name
- [ ] Test runs successfully: `cargo test --lib`

## Common Paths

| Fixture | Path | Status |
|---------|------|--------|
| Simple | `examples/simple-project/` | ✓ |
| MCP | `examples/mcp-server-definition/` | ✓ |
| OpenAPI | `examples/openapi/` | ✓ |
| Sync Foundation | `playground/sync-foundation/` | ✓ |
| **BROKEN** | `examples/basic-template-generation/` | ❌ FIX THIS |

---

**Last Updated**: 2026-05-29  
**Status**: 1 blocker identified, 30 fixtures ready for use
