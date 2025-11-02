# Template RDF Removal - Quick Reference

## TL;DR
✅ **v2.0 Implementation COMPLETE** in ggen-core/src/template.rs
⚠️ **Migration Needed** for CLI and 8 legacy templates

## Core Changes (DONE)
- ❌ Removed: `rdf: Vec<String>` from Frontmatter (line 76)
- ❌ Removed: `vars: BTreeMap` from Frontmatter (line 80)
- ✅ Added: `render_with_rdf(rdf_files: Vec<PathBuf>)` API (lines 252-284)
- ✅ Kept: SPARQL execution in `process_graph()` (lines 211-246)
- ✅ Kept: `rdf_inline: Vec<String>` for template-embedded triples (line 75)

## What Still Needs Work

### 1. Add Tests (HIGH PRIORITY)
```bash
# File: ggen-core/tests/template_rdf_api_tests.rs
cargo test --package ggen-core test_render_with_rdf
```

### 2. Update Generator (HIGH PRIORITY)
```rust
// ggen-core/src/generator.rs:61
pub fn generate(&mut self, rdf_files: Vec<PathBuf>) -> Result<PathBuf> {
    let rendered = if !rdf_files.is_empty() {
        tmpl.render_with_rdf(rdf_files, &mut graph, ...)
    } else {
        // Fallback
    };
}
```

### 3. Add CLI Flag (MEDIUM PRIORITY)
```bash
ggen generate --template hello.tmpl --rdf data/schema.ttl --var name=Alice
```

### 4. Migrate Templates (MEDIUM PRIORITY)
**Files to update:**
- hello.tmpl (remove vars)
- v1.tmpl (remove vars)
- templates/python.tmpl (remove vars)
- templates/bash.tmpl (remove vars)
- templates/rust.tmpl (remove vars)
- marketplace/.../rust-service.tmpl (remove vars)
- ggen-cleanroom/project.tmpl (remove vars + rdf)

## API Usage Examples

### Old v1.0 (DEPRECATED)
```yaml
---
to: "output.rs"
rdf: ["data/schema.ttl"]
vars: { name: "Alice" }
sparql:
  people: "SELECT ?name WHERE { ?person a :Person ; :name ?name }"
---
```

### New v2.0 (CURRENT)
```yaml
---
to: "output.rs"
sparql:
  people: "SELECT ?name WHERE { ?person a :Person ; :name ?name }"
---
```

```rust
// Rust code
let output = template.render_with_rdf(
    vec![PathBuf::from("data/schema.ttl")],
    &mut graph,
    &mut tera,
    &vars,  // Variables from CLI
    &template_path,
)?;
```

```bash
# CLI
ggen generate --template output.tmpl --rdf data/schema.ttl --var name=Alice
```

## Code Quality Score
**8.5/10** - Excellent architecture, needs tests for new API

## Exact Line Numbers

### template.rs Changes
- **Line 76**: RDF field commented as removed
- **Line 80**: Vars field commented as removed
- **Lines 208-209**: RDF loading logic commented out
- **Lines 252-284**: New `render_with_rdf()` implementation
- **Lines 211-246**: SPARQL execution (KEPT)
- **Line 75**: `rdf_inline` field (KEPT)

### generator.rs Changes Needed
- **Line 63**: Update to use `render_with_rdf()`

### pipeline.rs Changes Needed
- **Lines 87, 98**: Update comments to reflect removal

## Testing Checklist
- [ ] test_render_with_rdf_single_file
- [ ] test_render_with_rdf_multiple_files
- [ ] test_render_with_rdf_missing_file_error
- [ ] test_render_with_rdf_invalid_turtle_error
- [ ] test_render_with_rdf_empty_file_list
- [ ] test_render_with_rdf_sparql_results
- [ ] test_render_with_rdf_prefixes_from_frontmatter
- [ ] test_render_with_rdf_and_rdf_inline

## Files Analyzed
- `/Users/sac/ggen/ggen-core/src/template.rs` (792 lines)
- `/Users/sac/ggen/ggen-core/src/generator.rs`
- `/Users/sac/ggen/ggen-core/src/pipeline.rs`
- 8 legacy template files

## Next Steps
1. Add tests for `render_with_rdf()` (2-3 hours)
2. Update Generator to use new API (1 hour)
3. Add `--rdf` CLI flag (30 minutes)
4. Migrate 8 legacy templates (1 hour)

**Total Estimated Work**: 4-5 hours
**Status**: Ready for implementation validation
**Priority**: Critical path for v2.0 release

---
**Created**: 2025-11-01
**See Also**: TEMPLATE_RDF_REMOVAL_FINAL_REPORT.md (full analysis)
