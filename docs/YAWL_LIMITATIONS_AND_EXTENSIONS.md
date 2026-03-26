# YAWL v6: Known Limitations, Workarounds, and Extension Points

**Version:** 6.0.0
**Date:** 2026-03-26

---

## Known Limitations

### 1. RDF Store Size Limit (~100MB)

**Limitation:** Oxigraph stores RDF in memory

**Impact:**
- Ontologies > 100MB will exhaust memory and crash
- Typical ontologies: 1-10MB (well within limit)
- Enterprise ontologies: 10-50MB (usually acceptable)

**When This Matters:**
- Large government/pharma ontologies
- Aggregated multi-domain specifications
- Historical data included in ontology

**Workaround: Ontology Partitioning**

Split large ontologies into focused domains:

```bash
# Large: entities.ttl (100MB) → Cannot load
# Solution: Partition by domain
fibo/
├─ fibo-core.ttl        (10MB)
├─ fibo-accounts.ttl    (8MB)
├─ fibo-loans.ttl       (9MB)
└─ fibo-instruments.ttl (12MB)

# Run separate pipelines
ggen sync --manifest ggen-core.toml     # Uses fibo-core.ttl
ggen sync --manifest ggen-accounts.toml # Uses fibo-accounts.ttl
# (etc.)

# Merge results in CI/CD
```

**Workaround: External SPARQL Endpoint**

Use SPARQL 1.1 federation (future feature):

```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?entity WHERE {
  SERVICE <http://fuseki.internal/fibo> {
    ?entity rdf:type fibo:Account .
  }
}
```

**Status:** Federation not yet implemented (v6.0.0)

---

### 2. No Recursive Tera Macros

**Limitation:** Tera templates cannot define recursive macros

**Impact:**
- Cannot generate recursive data structures directly
- Tree-shaped code requires iteration + helper macros

**When This Matters:**
- AST-like code generation
- Hierarchical document generation
- Recursive type definitions

**Example Problem:**

```jinja2
{# Recursive macro (NOT SUPPORTED) #}
{% macro render_tree(node) %}
  {% if node.children %}
    {% for child in node.children %}
      {{ render_tree(child) }}  {# Can't recurse #}
    {% endfor %}
  {% endif %}
{% endmacro %}
```

**Workaround 1: Use SPARQL Query to Flatten**

Transform recursive structure into flat list in SPARQL:

```sparql
# Query: Get all tree nodes with depth pre-computed
SELECT ?node ?depth ?parentId WHERE {
  BIND(0 AS ?depth) .
  ?node a :TreeNode .
  OPTIONAL { ?node :parent ?parentId . }
}
ORDER BY ?depth ?parentId ?node
```

Then use iteration + depth field in template:

```jinja2
{% for item in items %}
  {{ "  " | repeat(item.depth) }}{{ item.node_name }}
{% endfor %}
```

**Workaround 2: Delegate Recursion to Generated Code**

Generate initial tree structure, let generated code handle recursion:

```jinja2
impl {{ node_name }} {
    pub fn render(&self, depth: usize) -> String {
        // Template generates non-recursive method
        let indent = " ".repeat(depth * 2);
        format!("{}{}",
            indent,
            self.value
        )
    }
}
```

Generated code then recurses:

```rust
impl {{ node_name }} {
    pub fn render_tree(&self, depth: usize) -> String {
        let mut result = self.render(depth);
        for child in &self.children {
            result.push_str(&child.render_tree(depth + 1));
        }
        result
    }
}
```

**Workaround 3: Custom Tera Filter (Rust)**

Register custom Rust closure as Tera filter:

```rust
// In ggen-yawl/src/template/renderer.rs
tera.register_filter("render_tree", |value, _| {
    // Custom Rust code to handle recursion
    let tree = serde_json::from_value::<TreeNode>(value)?;
    Ok(json!(render_tree_recursive(&tree, 0)))
});

fn render_tree_recursive(node: &TreeNode, depth: usize) -> String {
    // Custom Rust recursion logic
    // ...
}
```

**Status:** Workarounds available for 99% of use cases

---

### 3. SPARQL Federation Not Supported

**Limitation:** Cannot query across multiple RDF endpoints using SERVICE clause

**Impact:**
- Cannot integrate external SPARQL services
- Must merge all data before generation
- No real-time querying of live endpoints

**When This Matters:**
- Distributed knowledge graphs
- Querying government open data
- Real-time market data integration

**Workaround 1: Pre-Merge Ontologies**

Fetch remote data, merge locally:

```bash
# fetch-ontologies.sh
curl http://govdata.org/legal.ttl > legal.ttl
curl http://schema.org/schema.ttl > schema.ttl
cat legal.ttl schema.ttl > merged.ttl

# Use merged.ttl in ggen.toml
```

**Workaround 2: Use External Tool to Materialize**

Use Virtuoso/Fuseki to materialize federation results:

```bash
# Query external SPARQL endpoint
curl -G http://fuseki/fibo/sparql \
  --data-urlencode "query=
    CONSTRUCT { ?s ?p ?o . }
    WHERE { ?s ?p ?o . }" \
  > fibo.ttl

# Now use fibo.ttl in ggen.toml
```

**Future:** Federation support planned for v7.0 (requires oxigraph enhancement)

---

### 4. Sequential Rule Execution (No Parallelism)

**Limitation:** Rules execute sequentially, not in parallel

**Impact:**
- No multi-core speedup
- Generation time grows linearly with rule count
- Cannot hide I/O latency

**When This Matters:**
- Projects with 100+ generation rules
- Generating 10,000+ files (very rare)
- Real-time code generation (non-existent in practice)

**Typical Impact:**
- 10 rules: ~5s (acceptable)
- 100 rules: ~50s (noticeable)
- 1000 rules: ~500s (unacceptable)

**Workaround 1: Split into Multiple Pipelines**

Instead of one large manifest, use multiple:

```bash
# ggen-core.toml (generates core classes)
ggen sync --manifest ggen-core.toml

# ggen-api.toml (generates API endpoints)
ggen sync --manifest ggen-api.toml

# ggen-tests.toml (generates tests)
ggen sync --manifest ggen-tests.toml

# Run in parallel (CI/CD)
parallel ggen sync --manifest {} :::
  ggen-core.toml ggen-api.toml ggen-tests.toml
```

**Workaround 2: Consolidate Rules**

Merge multiple simple rules into single complex rule:

```toml
# Before (10 separate rules)
[[generation.rules]]
name = "generate_entity_1"
query = { inline = "SELECT ... WHERE { ?entity = ex:Entity1 . }" }
# ... 9 more similar rules

# After (1 rule generating all 10)
[[generation.rules]]
name = "generate_entities"
query = { inline = "SELECT ... WHERE { ?entity a ex:Entity . }" }
template = { file = "entity.tera" }
output_file = "src/entities/{{ entity_name }}.rs"
```

**Status:** Parallelism possible but not implemented (would sacrifice determinism guarantee)

---

### 5. Limited SPARQL Feature Support

**Limitation:** Oxigraph doesn't support all SPARQL 1.1 features

**Missing Features:**
- SPARQL FEDERATION (SERVICE clause)
- Some functions (e.g., complex date arithmetic)
- Some path expressions (complex transitive closure)
- Graph datasets (NAMED graphs limited)

**When This Matters:**
- Complex semantic reasoning
- Advanced RDF inference
- Querying multiple graphs

**Workaround 1: Implement as Inference Rule**

Materialize missing functionality in SPARQL CONSTRUCT:

```sparql
# Instead of: SELECT ?date WHERE { ?date > ?startDate }
# Use: CONSTRUCT to materialize all comparisons

PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
CONSTRUCT {
  ?entity fibo:isAfter ?otherEntity .
}
WHERE {
  ?entity fibo:date ?date1 .
  ?otherEntity fibo:date ?date2 .
  FILTER (?date1 > ?date2)
}
# Now SELECT uses materialized fibo:isAfter relationship
```

**Workaround 2: Use Intermediate Processing**

Add pre/post processing step:

```bash
# Step 1: Export data from RDF
ggen extract --manifest export.toml --output data.json

# Step 2: Process with external tool
process-complex-logic.py data.json > processed.json

# Step 3: Import back to RDF
import-json-to-rdf.py processed.json > enriched.ttl

# Step 4: Generate code from enriched RDF
ggen sync --manifest ggen.toml
```

**Workaround 3: Custom SPARQL Filters**

Register Rust function as SPARQL filter:

```rust
// In query executor initialization
query.register_filter("complex_comparison", |arg1, arg2| {
    // Custom Rust logic
    // ...
});
```

**Status:** Covers ~95% of practical code generation needs

---

## Workarounds Matrix

| Limitation | Severity | Impact | Workaround | Effort |
|-----------|----------|--------|-----------|--------|
| RDF size limit | Medium | Crashes on >100MB | Partition ontology | Low |
| Recursive macros | Medium | Complex types hard | Use iteration + SPARQL | Medium |
| No federation | Low | Multi-endpoint hard | Pre-merge data | Low |
| Sequential execution | Low | Slow with 100+ rules | Split manifests | Low |
| Limited SPARQL | Low | Some features missing | Use CONSTRUCT inference | Medium |

---

## Extension Points

### 1. Custom SPARQL Filters

**Purpose:** Add domain-specific SPARQL functions

**Implementation:**

```rust
// File: crates/ggen-yawl/src/extensions/custom_filters.rs

pub fn register_custom_filters(graph: &mut Graph) {
    graph.register_function("customFunction", |arg1, arg2| {
        // Custom logic in Rust
        let result = custom_logic(arg1, arg2);
        Ok(Term::Literal(result))
    });
}

// Usage in SPARQL:
// SELECT ?result WHERE {
//   ?entity :value ?val .
//   BIND(customFunction(?val, ?other) AS ?result)
// }
```

**Examples:**
- Custom date calculations
- Domain-specific string formatting
- Financial calculations
- Cryptographic hashing

---

### 2. Custom Tera Filters

**Purpose:** Add domain-specific template functions

**Implementation:**

```rust
// File: crates/ggen-yawl/src/template/custom_filters.rs

pub fn register_custom_filters(tera: &mut Tera) {
    tera.register_filter("to_camel_case", |s, _| {
        Ok(Value::String(to_camel_case(&s.as_str().unwrap())))
    });

    tera.register_filter("to_financial_notation", |amount, _| {
        let formatted = format!("${:.2}", amount.as_f64().unwrap());
        Ok(Value::String(formatted))
    });
}

// Usage in templates:
// {{ entity_name | to_camel_case }}
// {{ transaction_amount | to_financial_notation }}
```

**Examples:**
- Language-specific naming conventions
- Domain-specific formatting
- Complex type conversions
- Multi-language support

---

### 3. Validation Rules

**Purpose:** Add custom validation beyond SHACL

**Implementation:**

```toml
# In ggen.toml
[validation]
# Built-in SHACL
shacl_shapes = ["ontology/shapes.ttl"]

# Custom SPARQL constraints
sparql_constraints = [
  "validation/no_duplicate_names.sparql",
  "validation/required_fields.sparql"
]

# Custom Rust validators (future)
custom_validators = ["crate::validators::FinancialValidator"]
```

**Examples:**
- Cross-rule consistency checks
- Code quality metrics
- Security policy enforcement
- Performance constraints

---

### 4. Custom RDF Stores

**Purpose:** Use different RDF backend (future)

**Current:** Oxigraph in-memory

**Potential:**
- Persistent stores (RocksDB, SQLite)
- Remote SPARQL endpoints
- Graph databases (Neo4j integration)

**Extension Point:**

```rust
// Future trait-based store selection
pub trait RdfStore {
    fn query(&self, sparql: &str) -> Result<QueryResults>;
    fn insert_triples(&mut self, triples: Vec<Triple>);
    // ...
}

impl RdfStore for OxigraphStore { ... }
impl RdfStore for FusekiRemoteStore { ... }
impl RdfStore for Neo4jStore { ... }
```

**Status:** Planned for v7.0 (as plugin architecture)

---

### 5. Template Engine Plugins

**Purpose:** Support other template engines

**Current:** Tera only

**Potential:**
- Handlebars
- Askama (compile-time)
- Custom DSLs

**Extension Point:**

```rust
// Future trait-based template engine selection
pub trait TemplateEngine {
    fn render(&self, template: &str, context: &Context) -> Result<String>;
    fn register_filter(&mut self, name: &str, f: Box<dyn Fn(Value) -> Result<Value>>);
    // ...
}

impl TemplateEngine for TeraEngine { ... }
impl TemplateEngine for AskamaEngine { ... }
impl TemplateEngine for HandlersEngine { ... }
```

**Status:** Planned for v7.0 (as plugin architecture)

---

### 6. Lifecycle Hooks

**Purpose:** Execute code before/after generation stages

**Planned:**

```toml
[lifecycle]
# Hook before entire pipeline
pre_generation = "scripts/pre-gen.sh"

# Hook after each stage
post_stage_normalize = "scripts/after-normalize.sh"
post_stage_extract = "scripts/after-extract.sh"
post_stage_emit = "scripts/after-emit.sh"
post_stage_canonicalize = "scripts/after-canonicalize.sh"
post_stage_receipt = "scripts/after-receipt.sh"

# Hook after entire pipeline
post_generation = "scripts/post-gen.sh"
```

**Use Cases:**
- Format generated code (rustfmt, gofmt)
- Run tests
- Generate documentation
- Deploy artifacts
- Notify systems

**Status:** Basic support in v6.0.0, full support planned for v6.1

---

### 7. Manifest Composition

**Purpose:** Include and extend other manifests

**Planned:**

```toml
# Base manifest: ggen-base.toml
[project]
name = "ggen"
version = "6.0.0"

[ontology]
source = "fibo.ttl"

[[inference.rules]]
name = "infer_accounts"
construct = "..."

# Extending manifest: ggen-extended.toml
include = "ggen-base.toml"

[[inference.rules]]
name = "infer_workflows"
construct = "..."

[[generation.rules]]
name = "generate_erlang"
query = "..."
```

**Use Cases:**
- Shared rule libraries
- Domain layering
- Version-specific behaviors
- Multi-project manifests

**Status:** Planned for v6.1

---

### 8. External Rule Plugins

**Purpose:** Load rules from external packages

**Planned:**

```toml
[plugins]
# Load rules from crate
fibo_rules = { crate = "ggen-fibo-rules", version = "1.0" }
hl7_rules = { crate = "ggen-hl7-rules", version = "2.0" }

# Use imported rules
[[inference.rules]]
inherit_from = "fibo_rules::infer_accounts"

[[generation.rules]]
inherit_from = "hl7_rules::generate_clinical_workflow"
```

**Use Cases:**
- Packaged domain rules
- Third-party integrations
- Organization-specific patterns
- Community contributions

**Status:** Planned for v7.0

---

## Feature Evolution Roadmap

### v6.1 (Q2 2026)
- [ ] Lifecycle hooks (pre/post generation)
- [ ] Manifest composition (include support)
- [ ] Custom validation rules
- [ ] Incremental caching improvements

### v6.2 (Q3 2026)
- [ ] SPARQL parameterized queries
- [ ] Template import/include
- [ ] Multi-output rules
- [ ] Streaming large files

### v7.0 (Q4 2026)
- [ ] Plugin architecture (RDF stores, template engines)
- [ ] SPARQL federation support
- [ ] Parallel rule execution (with explicit DAG)
- [ ] External rule packages
- [ ] Template compilation (Tera → Rust)

### v8.0 (2027)
- [ ] AI-assisted generation (LLM rule suggestions)
- [ ] Real-time streaming generation
- [ ] Distributed execution
- [ ] Advanced compliance automation

---

## Migration Guide: Working Around Limitations

### Problem: Ontology is 150MB (too large)

**Solution:**

```bash
# Step 1: Identify domains
# - Legal (30MB)
# - Financial (40MB)
# - Technical (35MB)
# - Regulatory (25MB)
# - Admin (20MB)

# Step 2: Split manifest
mkdir ggen-manifests/
cat > ggen-manifests/legal.toml << EOF
[ontology]
source = "ontologies/legal.ttl"

[[generation.rules]]
name = "generate_legal_types"
# ...
EOF

# Step 3: Run in sequence (or parallel)
for manifest in ggen-manifests/*.toml; do
  ggen sync --manifest "$manifest"
done

# Step 4: Merge outputs
cat src/generated/*/legal*.rs > src/generated/legal_module.rs
```

### Problem: Complex recursive code structure

**Solution:**

See "Workaround 2: Use SPARQL Query to Flatten" above.

### Problem: Need integration with external API

**Solution:**

```bash
# Step 1: Fetch external data
curl https://api.example.com/schema > external_schema.json

# Step 2: Convert to RDF
jq -r '@json' external_schema.json > external.ttl

# Step 3: Merge with existing ontology
cat ontologies/main.ttl external.ttl > merged.ttl

# Step 4: Use merged ontology
# (Update ggen.toml to reference merged.ttl)
```

---

## Reporting Limitations

Found a limitation not listed here?

1. Check existing [GitHub Issues](https://github.com/seanchatmangpt/ggen/issues)
2. Create issue with:
   - Use case description
   - Current error message
   - Desired behavior
   - Workaround (if found)

3. Include minimal reproducible example

---

## Related Documentation

- **[YAWL_ARCHITECTURE.md](./YAWL_ARCHITECTURE.md)** - How system works
- **[YAWL_DESIGN_DECISIONS.md](./YAWL_DESIGN_DECISIONS.md)** - Trade-offs explained
- **[YAWL_DIAGRAMS.md](./YAWL_DIAGRAMS.md)** - Visual architecture
