# Template RDF Removal Analysis
**Critical Path Implementation Report**

## Executive Summary

Analysis of `/Users/sac/ggen/ggen-core/src/template.rs` reveals that **THE RDF FIELD HAS ALREADY BEEN REMOVED** from the Frontmatter struct and all related loading logic has been refactored. The v2.0 architecture is **ALREADY IMPLEMENTED**.

## Current State (v2.0 Implementation)

### ✅ ALREADY COMPLETED

1. **Line 76**: Comment explicitly states removal:
   ```rust
   // ❌ REMOVED: rdf: Vec<String> - RDF files now loaded via CLI/API only
   ```

2. **Lines 208-209**: RDF file loading logic removed from `process_graph()`:
   ```rust
   // ❌ REMOVED: RDF file loading from frontmatter
   // RDF files are now loaded via CLI/API using render_with_rdf() method
   ```

3. **Lines 252-284**: NEW `render_with_rdf()` API implemented:
   ```rust
   pub fn render_with_rdf(
       &mut self,
       rdf_files: Vec<std::path::PathBuf>,
       graph: &mut Graph,
       tera: &mut Tera,
       vars: &Context,
       template_path: &std::path::Path,
   ) -> Result<String>
   ```

4. **Line 75**: `rdf_inline` field KEPT for convenience (template-embedded triples)
   ```rust
   #[serde(default, deserialize_with = "string_or_seq")]
   pub rdf_inline: Vec<String>,
   ```

5. **Lines 211-246**: SPARQL execution logic PRESERVED and working

## Architecture Review

### Two-Phase Rendering (Lines 1-29 Documentation)

```text
Template String → Parse → Render Frontmatter → Load RDF (CLI/API) → Process Graph → Render Body
```

**Key Design Decisions:**
1. RDF files: CLI/API via `render_with_rdf()` ✅
2. Inline RDF: `rdf_inline:` frontmatter field ✅
3. SPARQL: Preserved in `process_graph()` ✅
4. Variables: CLI/API only (removed from frontmatter) ✅

### API Surface Analysis

#### Public Methods (All Working)

1. **`Template::parse(input: &str)`** (Lines 114-123)
   - Parses YAML frontmatter + body
   - No rendering yet

2. **`Template::parse_with_preprocessor()`** (Lines 126-142)
   - Runs preprocessor pipeline (freeze stages)
   - Then parses

3. **`Template::render_frontmatter()`** (Lines 173-178)
   - Resolves `{{vars}}` in frontmatter YAML
   - Populates `self.front` struct

4. **`Template::process_graph()`** (Lines 181-248)
   - Loads inline RDF from `rdf_inline:` field
   - Executes SPARQL queries
   - Stores results in `self.front.sparql_results`

5. **`Template::render_with_rdf()`** (Lines 252-284) ⭐ NEW v2.0 API
   - Accepts RDF files from CLI/API
   - Calls `process_graph()` internally
   - Returns rendered output

6. **`Template::render()`** (Lines 286-315)
   - Renders template body
   - Injects SPARQL results via `sparql_results` context

### Migration Path Analysis

#### Callers Need to Change:

**OLD v1.0 Pattern (DEPRECATED):**
```yaml
---
to: "output.rs"
rdf: ["data/schema.ttl", "data/instances.ttl"]
sparql:
  people: "SELECT ?name WHERE { ?person a :Person ; :name ?name }"
---
{{ sparql_results.people | length }} people
```

**NEW v2.0 Pattern (CURRENT):**
```rust
// CLI passes RDF files via --rdf flag
let mut template = Template::parse(template_str)?;
let rdf_files = vec![
    PathBuf::from("data/schema.ttl"),
    PathBuf::from("data/instances.ttl"),
];
let output = template.render_with_rdf(
    rdf_files,
    &mut graph,
    &mut tera,
    &vars,
    &template_path,
)?;
```

**Template frontmatter (v2.0):**
```yaml
---
to: "output.rs"
# NO rdf: field anymore!
sparql:
  people: "SELECT ?name WHERE { ?person a :Person ; :name ?name }"
---
{{ sparql_results.people | length }} people
```

### Test Coverage Analysis

#### Existing Tests (Lines 397-791)

1. **Parsing tests** (Lines 449-523)
   - ✅ All passing
   - ✅ No `rdf:` field in test cases
   - ✅ Using `rdf_inline:` correctly

2. **Rendering tests** (Lines 557-584)
   - ✅ Inline body rendering
   - ✅ `from:` file rendering

3. **Graph + SPARQL tests** (Lines 588-646)
   - ✅ `rdf_insert_and_select_visible` (Lines 588-602)
   - ✅ Boolean ASK queries (Lines 605-623)
   - ✅ Multiple queries + projection helpers (Lines 626-646)

4. **Preprocessor integration** (Lines 649-691)
   - ✅ Freeze stage support

5. **Property tests** (Lines 693-790)
   - ✅ Template parsing idempotency
   - ✅ Path validation
   - ❌ REMOVED: `frontmatter_vars_roundtrip` (Line 739)

#### Missing Tests

1. **`render_with_rdf()` method** - No direct tests found!
2. **RDF file loading from API** - Need integration test
3. **Error handling for missing RDF files** - Need test

## Code Quality Analysis

### Strengths

1. **Clear separation of concerns**
   - Frontmatter parsing ≠ rendering
   - Graph operations isolated in `process_graph()`
   - Clean API with `render_with_rdf()`

2. **Comprehensive documentation**
   - Lines 1-29: Excellent module-level docs
   - Clear migration notes (Lines 21-22)
   - API usage examples

3. **Test coverage**
   - 21 test functions
   - Property-based tests with `proptest`
   - Integration tests for preprocessor

4. **Error handling**
   - All functions return `Result<T>`
   - Clear error messages (e.g., Line 269)

### Code Smells (NONE CRITICAL)

1. **Line 186-192**: Defensive check for rendered frontmatter
   - Could be assertion instead of conditional
   - Low severity

2. **Lines 320-395**: Helper deserializers
   - `string_or_seq`: 75 lines
   - Could extract to separate module
   - Medium complexity but working

3. **No direct tests for `render_with_rdf()`**
   - High-priority gap
   - Should add integration test

## Implementation Status

### ✅ COMPLETED (No Changes Needed)

- [x] Remove `rdf: Vec<String>` from Frontmatter (Line 76)
- [x] Remove RDF loading logic from `process_graph()` (Lines 208-209)
- [x] Keep SPARQL execution (Lines 211-246)
- [x] Add `render_with_rdf()` API (Lines 252-284)
- [x] Update documentation (Lines 1-29)
- [x] Remove vars field from Frontmatter (Line 80)

### ⚠️ RECOMMENDED ENHANCEMENTS

1. **Add test for `render_with_rdf()`**
   ```rust
   #[test]
   fn render_with_rdf_from_files() -> Result<()> {
       let template_str = r#"---
   to: "output.rs"
   sparql:
     people: "SELECT ?name WHERE { ?person a ex:Person ; ex:name ?name }"
   ---
   People: {{ sparql_results.people | length }}"#;

       let mut temp_rdf = NamedTempFile::new()?;
       writeln!(temp_rdf, "@prefix ex: <http://example.org/> .")?;
       writeln!(temp_rdf, "ex:alice a ex:Person ; ex:name 'Alice' .")?;

       let mut template = Template::parse(template_str)?;
       let mut graph = Graph::new()?;
       let mut tera = mk_tera();
       let vars = Context::new();

       let output = template.render_with_rdf(
           vec![temp_rdf.path().to_path_buf()],
           &mut graph,
           &mut tera,
           &vars,
           Path::new("test.tmpl"),
       )?;

       assert_contains!(output, "People: 1");
       Ok(())
   }
   ```

2. **Add error handling test**
   ```rust
   #[test]
   fn render_with_rdf_missing_file_error() {
       let template_str = "---\nto: 'x'\n---\nBody";
       let mut template = Template::parse(template_str).unwrap();
       let mut graph = Graph::new().unwrap();
       let mut tera = mk_tera();
       let vars = Context::new();

       let result = template.render_with_rdf(
           vec![PathBuf::from("/nonexistent/file.ttl")],
           &mut graph,
           &mut tera,
           &vars,
           Path::new("test.tmpl"),
       );

       assert!(result.is_err());
       assert!(result.unwrap_err().to_string().contains("Failed to read RDF file"));
   }
   ```

## Migration Impact Analysis

### Files That Call Template Methods

Need to search for:
1. `Template::parse()` → May need `render_with_rdf()`
2. `process_graph()` → Check if calling with RDF files
3. YAML templates with `rdf:` field → Update to use CLI flags

### CLI Integration Points

1. **`cli/src/domain/template/`** - Check for `--rdf` flag handling
2. **`cli/src/domain/project/`** - Project generation with RDF
3. **`cli/src/commands/template/`** - Template command implementation

## Recommendations

### HIGH Priority

1. ✅ **No code changes needed** - Implementation complete
2. ⚠️ **Add tests** for `render_with_rdf()` method
3. 🔍 **Search codebase** for YAML templates with `rdf:` field
4. 📝 **Update migration docs** with examples

### MEDIUM Priority

1. Extract deserializer helpers to `template/deserializers.rs`
2. Add performance benchmarks for graph operations
3. Document SPARQL helper functions in module docs

### LOW Priority

1. Consider assertion instead of defensive check (Lines 186-192)
2. Add property tests for `render_with_rdf()`
3. Document error variants with examples

## Next Steps

### 1. Validate CLI Integration
```bash
# Search for templates with rdf: field
rg "rdf:" --type yaml --glob '**/*.md.t'

# Search for Template::parse usage
rg "Template::parse" --type rust

# Check for process_graph calls
rg "process_graph" --type rust
```

### 2. Add Missing Tests
- Create `tests/template_rdf_api.rs`
- Test `render_with_rdf()` with real RDF files
- Test error cases (missing files, invalid RDF)

### 3. Update Documentation
- Migration guide with before/after examples
- CLI usage examples with `--rdf` flag
- API reference for `render_with_rdf()`

### 4. Search for Deprecated Patterns
- Find YAML templates using old `rdf:` field
- Update to CLI-based RDF loading
- Add deprecation warnings if needed

## Conclusion

**STATUS: ✅ IMPLEMENTATION COMPLETE**

The RDF removal from frontmatter is **ALREADY DONE** in v2.0. The codebase shows:
- Clean API with `render_with_rdf()`
- Preserved SPARQL functionality
- Well-documented migration path
- Comprehensive test coverage (except `render_with_rdf()`)

**NO CHANGES NEEDED TO CORE LOGIC** - Focus on:
1. Adding tests for new API
2. Finding/updating deprecated YAML templates
3. Validating CLI integration

This is NOT a blocking issue - it's a completed migration that needs validation.

---

**Analysis Complete**: 2025-11-01
**File**: `/Users/sac/ggen/ggen-core/src/template.rs` (792 lines)
**Status**: v2.0 Architecture Implemented ✅
