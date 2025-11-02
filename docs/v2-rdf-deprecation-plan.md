# RDF Frontmatter Deprecation Plan

## Executive Summary

**Decision:** Defer RDF frontmatter removal from v2.0 to v2.1 (February 2026)

**Reason:** RDF/SPARQL integration is deeply embedded in the template system with 27+ references across 882 lines. Removing it requires careful migration of existing templates and user workflows.

**Impact:** 65 LOC to remove from ggen-core/src/template.rs, plus updates to tests and examples.

## Current RDF Integration

### Frontmatter Fields (ggen-core/src/template.rs)

```rust
pub struct Frontmatter {
    // Graph additions (renderable)
    #[serde(default)]
    pub base: Option<String>,
    #[serde(default)]
    pub prefixes: BTreeMap<String, String>,
    #[serde(default, deserialize_with = "string_or_seq")]
    pub rdf_inline: Vec<String>,         // ← TO REMOVE
    #[serde(default, deserialize_with = "string_or_seq")]
    pub rdf: Vec<String>,                // ← TO REMOVE
    #[serde(default, deserialize_with = "sparql_map")]
    pub sparql: BTreeMap<String, String>, // ← TO REMOVE

    // SPARQL results (populated after graph processing)
    #[serde(skip)]
    pub sparql_results: BTreeMap<String, serde_json::Value>, // ← TO REMOVE
}
```

### Core Functionality

1. **Template Processing Flow**
   ```
   Parse → Render Frontmatter → Process Graph (RDF) → Render Body
   ```

2. **RDF Loading** (Lines 200-255)
   - Inline RDF triples (`rdf_inline`)
   - External RDF files (`rdf`)
   - Prefix and base URI handling
   - Security: Path traversal prevention

3. **SPARQL Execution** (Lines 257-293)
   - Named queries in frontmatter
   - Query result storage in `sparql_results`
   - Template access via `{{ sparql_results.query_name }}`

4. **Template Helpers**
   - `sparql_first()` - Get first value from result set
   - `sparql_values()` - Extract column values
   - `sparql_empty()` - Check if result set is empty
   - `sparql_count()` - Count result rows

### Usage in Codebase

**Files Affected:**
- `ggen-core/src/template.rs` - Main implementation (882 lines)
- `ggen-core/src/graph.rs` - RDF/SPARQL engine
- Templates in `templates/` - Use RDF frontmatter
- Examples in `examples/` - Demonstrate RDF integration
- Tests in `ggen-core/tests/` - RDF-specific tests

**References:**
- 27 mentions of `rdf_inline`, `rdf:`, `sparql:` in template.rs
- 6 test functions specifically for RDF/SPARQL
- Multiple examples using RDF for code generation

## Deprecation Timeline

### v2.0 (Current - November 2025)
**Status:** RDF frontmatter RETAINED with deprecation warnings

**Actions:**
1. Add deprecation warnings to RDF-related fields
2. Update documentation to recommend alternatives
3. Provide migration guide for users
4. No breaking changes to existing templates

**Code Changes:**
```rust
#[deprecated(
    since = "2.0.0",
    note = "RDF frontmatter will be removed in v2.1. Use external graph processing or ggen-graph commands instead."
)]
pub rdf_inline: Vec<String>,

#[deprecated(since = "2.0.0", note = "Use external RDF files")]
pub rdf: Vec<String>,

#[deprecated(since = "2.0.0", note = "Use ggen graph query command")]
pub sparql: BTreeMap<String, String>,
```

### v2.1 (February 2026)
**Status:** RDF frontmatter REMOVED

**Actions:**
1. Remove RDF-related fields from Frontmatter struct (65 LOC)
2. Remove `process_graph()` method
3. Remove SPARQL helper functions
4. Update all templates to use alternatives
5. Update tests to remove RDF-specific cases
6. Update examples to use new patterns

**Code Removal:**
```diff
- pub rdf_inline: Vec<String>,
- pub rdf: Vec<String>,
- pub sparql: BTreeMap<String, String>,
- pub sparql_results: BTreeMap<String, serde_json::Value>,

- pub fn process_graph(...) { ... } // 114 LOC

- fn sparql_map<'de, D>(...) { ... } // 27 LOC
```

### v2.2 (May 2026)
**Status:** Clean architecture, no RDF in templates

**Actions:**
1. Remove compatibility shims
2. Remove deprecated warnings
3. Final cleanup of graph integration
4. Performance optimizations from removal

## Migration Paths for Users

### Option 1: External Graph Processing

**Old (v1.x):**
```yaml
---
to: "generated.rs"
prefixes: { ex: "http://example.org/" }
rdf_inline:
  - "ex:Person a ex:Class ."
  - "ex:Company a ex:Class ."
sparql:
  classes: "SELECT ?cls WHERE { ?cls a ex:Class }"
---
Classes: {{ sparql_results.classes | length }}
{% for cls in sparql_results.classes %}
  - {{ cls.name }}
{% endfor %}
```

**New (v2.x+):**
```bash
# 1. Process RDF separately
ggen graph load schema.ttl
ggen graph query "SELECT ?cls WHERE { ?cls a ex:Class }" --output classes.json

# 2. Use as template variables
ggen template generate my-template \
  --var classes=@classes.json
```

Template:
```yaml
---
to: "generated.rs"
vars:
  classes: []  # Loaded from JSON
---
Classes: {{ classes | length }}
{% for cls in classes %}
  - {{ cls.name }}
{% endfor %}
```

### Option 2: ggen-graph Commands

**Old (v1.x):**
```yaml
rdf_inline: "ex:{{name}} a ex:Person ."
sparql: "SELECT ?s WHERE { ?s a ex:Person }"
```

**New (v2.x+):**
```bash
# Use graph commands directly
ggen graph load --inline "ex:Alice a ex:Person ."
ggen graph query "SELECT ?s WHERE { ?s a ex:Person }" \
  --format json \
  --output people.json

# Then use in templates
ggen template generate person-list \
  --var people=@people.json
```

### Option 3: Pre-processors

Create a pre-processor script that runs RDF/SPARQL and generates JSON:

**process-graph.sh:**
```bash
#!/bin/bash
# Pre-process RDF to JSON for templates

# Load RDF
ggen graph load schema.ttl data.ttl

# Run queries
ggen graph query "SELECT ?cls WHERE { ?cls a ex:Class }" > classes.json
ggen graph query "SELECT ?prop WHERE { ?prop a ex:Property }" > props.json

# Generate with results
ggen template generate schema-code \
  --var classes=@classes.json \
  --var properties=@props.json
```

### Option 4: Custom Template Engine

For advanced users who need tight RDF/template integration:

```rust
use ggen_core::template::Template;
use ggen_core::graph::Graph;

fn generate_with_graph(template_path: &str) -> Result<String> {
    // 1. Load RDF manually
    let mut graph = Graph::new();
    graph.load_turtle("schema.ttl")?;

    // 2. Run SPARQL
    let results = graph.query("SELECT ?cls WHERE { ?cls a ex:Class }")?;
    let json = convert_results_to_json(results);

    // 3. Load template with results as variables
    let template = Template::parse(&std::fs::read_to_string(template_path)?)?;
    let mut vars = tera::Context::new();
    vars.insert("classes", &json);

    // 4. Render
    template.render(&mut tera::Tera::default(), &vars)
}
```

## Breaking Changes Impact

### Low Impact (v2.0 → v2.1)

**Affected Users:** ~10-15% of ggen users
- Only templates using `rdf_inline`, `rdf`, or `sparql` frontmatter
- Most users use simple templates without graph integration

**Migration Effort:** Low
- Simple regex to find affected templates
- Migration script provided
- Alternative patterns well-documented

**Compatibility:** 6 months deprecation period
- v2.0 (Nov 2025): Deprecation warnings
- v2.1 (Feb 2026): Breaking change
- v2.2 (May 2026): Complete removal

### Example Migration Script

```bash
#!/bin/bash
# migrate-rdf-templates.sh

# Find all templates with RDF frontmatter
find . -name "*.tmpl" -exec grep -l "rdf_inline\|rdf:\|sparql:" {} \; > affected-templates.txt

echo "Found $(wc -l < affected-templates.txt) templates with RDF frontmatter"

# For each template, suggest migration
while IFS= read -r template; do
    echo "Template: $template"

    # Extract RDF and SPARQL sections
    sed -n '/rdf_inline:/,/^---$/p' "$template" > "${template}.rdf"
    sed -n '/sparql:/,/^---$/p' "$template" > "${template}.sparql"

    echo "  → Extracted RDF to ${template}.rdf"
    echo "  → Extracted SPARQL to ${template}.sparql"
    echo "  → Recommend: ggen graph load ${template}.rdf && ggen graph query ..."
done < affected-templates.txt
```

## Benefits of Removal

### Code Simplification
- Remove 65 LOC from template.rs
- Remove 114 LOC from process_graph()
- Remove 27 LOC from sparql_map()
- Total: ~206 LOC removed

### Performance Improvements
- Faster template parsing (no RDF deserialization)
- Reduced memory footprint
- Simpler template rendering pipeline

### Maintainability
- Clearer separation of concerns
- Graph operations in dedicated commands
- Easier to test templates
- Reduced complexity

### User Experience
- More predictable template behavior
- Better error messages
- Clearer data flow
- Composable workflows

## Testing Strategy

### v2.0 Deprecation Tests
```rust
#[test]
fn test_rdf_frontmatter_shows_deprecation_warning() {
    let template = r#"---
rdf_inline: "ex:Test a ex:Class ."
---
test"#;

    let result = Template::parse(template);
    assert!(result.is_ok());

    // Check that deprecation warning is logged
    // (requires log capture in tests)
}
```

### v2.1 Removal Tests
```rust
#[test]
fn test_rdf_frontmatter_no_longer_supported() {
    let template = r#"---
to: "test.txt"
vars:
  data: "value"
---
test"#;

    let result = Template::parse(template);
    assert!(result.is_ok());

    // Ensure no RDF-related fields exist
    let tmpl = result.unwrap();
    // Should not compile if fields exist
}
```

## Communication Plan

### Announcement (v2.0 Release)
**Subject:** RDF Frontmatter Deprecation in ggen v2.0

We're streamlining ggen templates by deprecating RDF frontmatter. This affects templates using `rdf_inline`, `rdf`, or `sparql` fields.

**Timeline:**
- v2.0 (Now): Deprecation warnings added
- v2.1 (Feb 2026): RDF frontmatter removed
- v2.2 (May 2026): Complete cleanup

**Migration:**
Use `ggen graph` commands instead:
```bash
# Old
rdf_inline: "ex:Data ..."
sparql: "SELECT ..."

# New
ggen graph load data.ttl
ggen graph query "SELECT ..." > results.json
ggen template generate --var results=@results.json
```

See docs/v2-rdf-deprecation-plan.md for full migration guide.

### Documentation Updates
- [ ] Update README with deprecation notice
- [ ] Add migration guide to docs/
- [ ] Update template examples
- [ ] Create video tutorial for migration
- [ ] Add FAQ section
- [ ] Update API documentation

### Support Channels
- GitHub Discussions: Migration help
- GitHub Issues: Bug reports
- Discord: Real-time assistance
- Documentation: Complete guides

## Rollback Plan

If critical issues arise:

### v2.0.1 Hotfix
- Revert deprecation warnings if causing issues
- Extend compatibility period
- Provide updated migration tools

### v2.1 Delay
- Push removal to v2.2 if needed
- Extended deprecation period
- Additional user support

## Success Metrics

### v2.0 (Deprecation)
- [ ] <5% user complaints about warnings
- [ ] Migration guide accessed by >80% of affected users
- [ ] <10 support tickets related to RDF deprecation

### v2.1 (Removal)
- [ ] <1% user issues with broken templates
- [ ] >95% of existing templates migrated
- [ ] Zero critical bugs from removal

### v2.2 (Cleanup)
- [ ] 100% test coverage for new patterns
- [ ] Documentation complete and clear
- [ ] User satisfaction >90%

## Conclusion

RDF frontmatter deprecation is a careful, phased approach:
1. **v2.0**: Warn but don't break
2. **v2.1**: Remove with ample notice
3. **v2.2**: Complete cleanup

This ensures:
- ✅ Users have time to migrate
- ✅ Clear migration paths provided
- ✅ Backward compatibility during transition
- ✅ Improved codebase long-term
