# Research: Fix Template Rendering Engine

**Branch**: `005-fix-rendering-engine`
**Date**: 2025-12-13

## Root Cause Analysis

### Problem Statement
When running `ggen template generate --template template.tmpl --output output.txt`, templates with SPARQL frontmatter report `sparql_queries_executed: 0` and fail to render `{{ sparql_results.* }}` variables.

### Root Cause
**The `template generate` CLI command uses `GenerateFileOptions` → `generate_file()` which does NOT support RDF/SPARQL processing.**

The correct path that DOES support SPARQL is `RenderWithRdfOptions` → `render_with_rdf()`, which is only used by `project generate`.

## Key Findings

### Finding 1: Two Separate Rendering Paths

| Command | Options Struct | Domain Function | RDF Support |
|---------|---------------|-----------------|-------------|
| `template generate` | `GenerateFileOptions` | `generate_file()` | **NONE** |
| `project generate` | `RenderWithRdfOptions` | `render_with_rdf()` | **FULL** |

**Decision**: Modify `template generate` to use `RenderWithRdfOptions` when template contains RDF/SPARQL frontmatter.

**Rationale**: This provides backward compatibility for simple templates while enabling RDF for advanced ones.

**Alternatives Considered**:
1. ❌ Create new `template generate-rdf` command - Fragment user experience
2. ❌ Always use `render_with_rdf()` - May have performance overhead for simple templates
3. ✅ Auto-detect and route - Best of both worlds

### Finding 2: SPARQL Execution Path Is Complete

The SPARQL execution infrastructure is fully implemented:

| Component | File | Lines | Status |
|-----------|------|-------|--------|
| Frontmatter Struct | `crates/ggen-core/src/template.rs` | 45-132 | ✅ Complete |
| SPARQL field parsing | `crates/ggen-core/src/template.rs` | 95-100 | ✅ Complete |
| Graph processing | `crates/ggen-core/src/template.rs` | 393-505 | ✅ Complete |
| Query execution | `crates/ggen-core/src/graph/core.rs` | 537-584 | ✅ Complete |
| Result injection | `crates/ggen-core/src/template.rs` | 561-576 | ✅ Complete |
| Helper functions | `crates/ggen-core/src/register.rs` | 172-190 | ✅ Complete |

**Decision**: No changes needed to core SPARQL infrastructure.

**Rationale**: Existing tests prove SPARQL works (`rdf_insert_and_select_visible` at line 941).

### Finding 3: Force Flag Propagation Is Correct

Both commands properly propagate `--force` to `force_overwrite`:

- `template generate`: line 258 → `GenerateFileOptions { force_overwrite: force }`
- `project generate`: line 708 → `RenderWithRdfOptions { force_overwrite: force }`

**Decision**: No changes needed for force flag.

**Rationale**: Both paths check the flag before file operations.

### Finding 4: Template Detection Pattern

Lazy RDF loading check (template.rs:412-417):
```rust
if self.front.rdf_inline.is_empty()
    && self.front.rdf.is_empty()
    && self.front.sparql.is_empty()
{
    return Ok(());  // Early return, no SPARQL
}
```

**Decision**: Use this same pattern to detect if template needs RDF processing in CLI.

**Rationale**: Consistent with existing optimization pattern.

## Implementation Strategy

### Option A: Modify `template generate` to auto-detect RDF (RECOMMENDED)

1. Parse template to check for `rdf`, `rdf_inline`, or `sparql` fields
2. If RDF detected → use `render_with_rdf()` path
3. If no RDF → use existing `generate_file()` path (backward compatible)

**Pros**: Single command, backward compatible, auto-routing
**Cons**: Slight complexity in detection

### Option B: Add `--rdf` flag to `template generate`

1. Add `--rdf <path>` CLI parameter
2. When provided, use `render_with_rdf()` path
3. Otherwise use `generate_file()` path

**Pros**: Explicit user control
**Cons**: Extra flag required, may confuse users

### Option C: Always use `render_with_rdf()`

1. Refactor `template generate` to always use `RenderWithRdfOptions`
2. Empty RDF list triggers lazy loading optimization

**Pros**: Simplest code path
**Cons**: May have minor performance impact on simple templates

**Selected**: Option A (auto-detect) for best user experience.

## Files to Modify

| File | Change | Reason |
|------|--------|--------|
| `crates/ggen-cli/src/cmds/template.rs` | Modify `generate()` function (lines 228-272) | Route to `render_with_rdf()` when RDF detected |
| `crates/ggen-domain/src/template/mod.rs` | Add detection helper if needed | Centralize RDF detection logic |

## Test Plan

1. **Unit Test**: Template with `sparql:` frontmatter → `sparql_queries_executed > 0`
2. **Unit Test**: Template without RDF → uses fast path, works as before
3. **Integration Test**: clap-noun-verb calculator.ttl generates valid Rust
4. **Regression Test**: Existing templates still work without changes
