# Current RDF Integration in Ggen

## Executive Summary

Ggen has **mature RDF/SPARQL integration** that loads RDF data, executes SPARQL queries, and exposes results to Tera templates. **The limitation**: each template generates **ONE output file**. To generate **multiple files** from SPARQL results, you need an **external orchestrator** that iterates query results and invokes ggen for each file.

## Current Architecture

### Data Flow

```
Template with frontmatter
↓
Step 1: Parse YAML frontmatter
↓
Step 2: Render frontmatter with Tera (resolve {{ vars }})
↓
Step 3: Process RDF Graph
  ├─ Load rdf_inline: [...] into Oxigraph store
  ├─ Load rdf: ["file.ttl"] from filesystem
  ├─ Execute sparql: {...} queries
  └─ Store results in frontmatter.sparql_results
↓
Step 4: Render template body with Tera
  ├─ SPARQL results available as {{ sparql_results.query_name }}
  ├─ Helper functions: sparql_first, sparql_values, sparql_count, etc.
  └─ Render to string
↓
Step 5: Write to ONE file (frontmatter.to)
```

### Key Components

#### 1. Template Frontmatter (`ggen-core/src/template.rs`)

```yaml
---
to: "output.rs"                    # ONE output path
vars:
  name: "User"
prefixes:
  ex: "http://example.org/"
base: "http://example.org/base/"
rdf_inline:                         # Inline Turtle triples
  - "@prefix ex: <http://example.org/> . ex:Alice a ex:Person ."
rdf:                                # External Turtle files
  - "data/domain.ttl"
sparql:                             # Named SPARQL queries
  find_users: "SELECT ?user ?name WHERE { ?user a ex:User ; ex:name ?name }"
  find_tables: "SELECT ?table WHERE { ?table a ex:Table }"
---
Body renders here using {{ sparql_results.find_users }}
```

#### 2. RDF Graph Processing (`ggen-core/src/graph.rs`)

**Graph.rs provides**:
- **Oxigraph wrapper** with thread-safe store
- **SPARQL caching** (LRU cache for queries and results)
- **Multiple RDF formats**: Turtle, N-Triples, RDF/XML
- **Quad insertion and pattern matching**

**Key methods**:
```rust
impl Graph {
    pub fn new() -> Result<Self>
    pub fn insert_turtle(&self, turtle: &str) -> Result<()>
    pub fn load_path<P: AsRef<Path>>(&self, path: P) -> Result<()>
    pub fn query<'a>(&'a self, sparql: &str) -> Result<QueryResults<'a>>
    pub fn query_cached(&self, sparql: &str) -> Result<CachedResult>
}
```

#### 3. Template Processing (`ggen-core/src/template.rs`)

**Process flow**:
```rust
impl Template {
    // 1. Parse frontmatter + body
    pub fn parse(input: &str) -> Result<Self>

    // 2. Render frontmatter (resolve {{ vars }} in YAML)
    pub fn render_frontmatter(&mut self, tera: &mut Tera, vars: &Context) -> Result<()>

    // 3. Process RDF graph and execute SPARQL
    pub fn process_graph(
        &mut self, graph: &mut Graph, tera: &mut Tera, vars: &Context,
        template_path: &std::path::Path,
    ) -> Result<()> {
        // Insert inline RDF
        for ttl in &self.front.rdf_inline {
            let ttl_rendered = tera.render_str(ttl, vars)?;
            graph.insert_turtle(&ttl_rendered)?;
        }

        // Load RDF files (relative to template directory)
        for rdf_file in &self.front.rdf {
            let rdf_path = template_dir.join(&rendered_path);
            graph.load_path(&rdf_path)?;
        }

        // Execute SPARQL queries and store results
        for (name, query) in &self.front.sparql {
            let results = graph.query(&query)?;
            let json_result = materialize_to_json(results)?;
            self.front.sparql_results.insert(name.clone(), json_result);
        }
    }

    // 4. Render body with SPARQL results available
    pub fn render(&self, tera: &mut Tera, vars: &Context) -> Result<String> {
        // Inject sparql_results into Tera context
        final_vars.insert("sparql_results", &self.front.sparql_results);
        tera.render_str(&body_source, &final_vars)
    }
}
```

**Frontmatter fields**:
```rust
pub struct Frontmatter {
    pub to: Option<String>,              // Output file path
    pub rdf_inline: Vec<String>,         // Inline Turtle triples
    pub rdf: Vec<String>,                // External RDF file paths
    pub sparql: BTreeMap<String, String>, // Named SPARQL queries
    pub sparql_results: BTreeMap<String, serde_json::Value>, // Query results
    pub prefixes: BTreeMap<String, String>, // SPARQL prefixes
    pub base: Option<String>,            // Base URI
    // ... other fields
}
```

#### 4. SPARQL Result Helpers (`ggen-core/src/register.rs`)

**Available in Tera templates**:
```rust
// Extract first value from a column
sparql_first(results=sparql_results.query_name, column="columnName")

// Extract all values from a column as array
sparql_values(results=sparql_results.query_name, column="columnName")

// Check if results are empty
sparql_empty(results=sparql_results.query_name)

// Count results
sparql_count(results=sparql_results.query_name)

// Get specific column
sparql_column(results=sparql_results.query_name, column="columnName")

// Get specific row
sparql_row(results=sparql_results.query_name, index=0)
```

### Real-World Examples

#### Example 1: Single File with SPARQL Results

**Template**: `examples/microservices-architecture/templates/user-service.tmpl`

```yaml
---
to: "services/user-service/src/main.rs"
rdf_inline:
  - "@prefix ex: <https://example.com/microservices/> . ex:UserService ex:manages ex:User ."
sparql:
  user_operations: "SELECT ?operation WHERE { ex:UserService ex:handles ?operation }"
---
//! User Service
//! Operations: {{ sparql_count(results=sparql_results.user_operations) }}
```

**Result**: ONE file `services/user-service/src/main.rs` with SPARQL results embedded in code.

#### Example 2: Iterating SPARQL Results (WORKS - but still ONE file)

**Template**: `marketplace/packages/advanced-rust-project/templates/database-schema.tmpl`

```yaml
---
to: "generated/src/database/{{ name | snake }}.rs"
rdf:
  - "data/domain.ttl"
sparql:
  find_tables: "SELECT ?table WHERE { ?table a ex:Table }"
  find_columns: "SELECT ?column WHERE { ?column a ex:Column }"
---
{% for table in sparql_results.find_tables %}
#[derive(Debug, Clone)]
pub struct {{ table.table | pascal }} {
    {% for column in sparql_results.find_columns %}
    {% if column.table == table.table %}
    pub {{ column.column | snake }}: String,
    {% endif %}
    {% endfor %}
}
{% endfor %}
```

**Result**: ONE file with multiple structs (one per table). **Not** multiple files!

#### Example 3: Query-Only Templates

**Template**: `examples/microservices-architecture/templates/sparql-queries.tmpl`

```yaml
---
to: "data/queries.sparql"
rdf_inline:
  - "@prefix ex: <https://example.com/microservices/> ."
sparql:
  service_dependencies: "SELECT ?service ?dependsOn WHERE { ?service ex:dependsOn ?dependsOn }"
---
# Generated SPARQL Queries
# Service count: {{ sparql_count(results=sparql_results.service_dependencies) }}

PREFIX ex: <https://example.com/microservices/>
SELECT ?service ?dependsOn WHERE { ?service ex:dependsOn ?dependsOn }
```

**Result**: ONE documentation file listing SPARQL queries and statistics.

## Current Capabilities

### ✅ What Works Today

1. **Load RDF data**
   - Inline Turtle: `rdf_inline: ["triples..."]`
   - External files: `rdf: ["data/domain.ttl"]`
   - Multiple formats: Turtle, N-Triples, RDF/XML

2. **Execute SPARQL queries**
   - Named queries: `sparql: { query_name: "SELECT ..." }`
   - Prefix/base support: `prefixes: { ex: "http://..." }, base: "..."`
   - Query types: SELECT, ASK, CONSTRUCT, DESCRIBE

3. **Access results in templates**
   - Direct access: `{{ sparql_results.query_name }}`
   - Helper functions: `sparql_first()`, `sparql_values()`, etc.
   - Array iteration: `{% for row in sparql_results.query_name %}`

4. **Generate ONE output file**
   - Path templating: `to: "{{ name }}.rs"`
   - Variable substitution in frontmatter
   - Template iteration over results (multiple structs in one file)

5. **Performance optimizations**
   - Query result caching (LRU cache)
   - Plan caching (parsed queries)
   - Epoch-based cache invalidation

### ❌ What Doesn't Work

1. **Generate MULTIPLE files from SPARQL results**
   - **No fan-out mechanism**: One template = one `to:` path = one output file
   - **Can't do**: `to: "models/{{ row.table }}.rs"` for each SPARQL row
   - **Workaround**: External loop calling ggen N times

2. **Dynamic file tree generation**
   - Can't create `N` files where `N` comes from SPARQL query
   - Can't generate directory structure from RDF graph

3. **Cross-file references**
   - Each template invocation is isolated
   - No shared state between multiple template runs

## The Gap: One Template → Multiple Files

### Current Limitation

```yaml
---
to: "models/{{ table }}.rs"  # ❌ This resolves to ONE path
sparql:
  find_tables: "SELECT ?table WHERE { ?table a ex:Table }"
---
# This generates ONE file, not N files (one per table)
```

### What We Need

**Desired behavior**:
```yaml
---
to_each: "models/{{ row.table | snake }}.rs"  # ❌ Doesn't exist
for_each: sparql_results.find_tables          # ❌ Doesn't exist
sparql:
  find_tables: "SELECT ?table WHERE { ?table a ex:Table }"
---
// Generate THIS file for EACH table in SPARQL results
pub struct {{ row.table | pascal }} { ... }
```

**Current workaround** (external orchestration):
```bash
# Query ggen for SPARQL results
ggen query --sparql "SELECT ?table WHERE { ?table a ex:Table }" > tables.json

# Loop and invoke ggen for each table
jq -r '.[] | .table' tables.json | while read table; do
  ggen template generate table.tmpl \
    --vars "table=$table" \
    --output "models/${table}.rs"
done
```

## Proposed Extensions (Simplest Bridges)

### Option 1: `to_many` with `for_each` (Template-level)

**Add to frontmatter**:
```yaml
---
to_many: "models/{{ item.table | snake }}.rs"
for_each: sparql_results.find_tables
sparql:
  find_tables: "SELECT ?table WHERE { ?table a ex:Table }"
---
// Each file gets {{ item.table }} in scope
pub struct {{ item.table | pascal }} { ... }
```

**Implementation**: Modify `Template::render()` to return `Vec<(PathBuf, String)>` instead of `String`.

### Option 2: `ggen batch` command (CLI-level)

**Usage**:
```bash
ggen batch \
  --template table.tmpl \
  --sparql "SELECT ?table WHERE { ?table a ex:Table }" \
  --for-each "table" \
  --output "models/{{ table }}.rs"
```

**Implementation**: New CLI command that:
1. Executes SPARQL query
2. Iterates results
3. Invokes template generator for each row with row context

### Option 3: `ggen pipeline` (External YAML)

**pipeline.yaml**:
```yaml
steps:
  - name: Load RDF
    rdf: ["data/domain.ttl"]

  - name: Query tables
    sparql: "SELECT ?table WHERE { ?table a ex:Table }"
    store_as: tables

  - name: Generate table files
    for_each: tables
    template: table.tmpl
    output: "models/{{ item.table }}.rs"
    vars:
      table: "{{ item.table }}"
```

**Implementation**: New pipeline runner that orchestrates multiple template invocations.

## Comparison: Current vs. Needed

| Feature | Current | Needed |
|---------|---------|--------|
| **RDF loading** | ✅ rdf_inline, rdf files | ✅ Same |
| **SPARQL queries** | ✅ Named queries in frontmatter | ✅ Same |
| **Results access** | ✅ sparql_results.name | ✅ Same |
| **Single file output** | ✅ to: "path.rs" | ✅ Same |
| **Multiple file output** | ❌ No mechanism | ✅ to_many + for_each |
| **Iteration** | ✅ {% for %} in ONE file | ✅ Separate files per row |
| **Helper functions** | ✅ sparql_first, etc. | ✅ Same |
| **Performance** | ✅ Cached queries | ✅ Same + batch optimization |

## Simplest Bridge Strategy

### Recommendation: **Option 1 (to_many + for_each)**

**Why**:
1. **Minimal code changes**: Modify `Template::render()` and output writer
2. **Backward compatible**: Existing `to:` templates unchanged
3. **Natural syntax**: Feels like native ggen feature
4. **No external dependencies**: Pure Rust, no shell scripting

**Implementation sketch**:
```rust
// In template.rs
pub enum OutputMode {
    Single { path: String, content: String },
    Multiple { files: Vec<(PathBuf, String)> },
}

impl Template {
    pub fn render_outputs(&self, tera: &mut Tera, vars: &Context) -> Result<OutputMode> {
        if let Some(to_many) = &self.front.to_many {
            let for_each = self.front.for_each.as_ref()
                .ok_or_else(|| anyhow!("to_many requires for_each"))?;

            let results = self.front.sparql_results.get(for_each)
                .ok_or_else(|| anyhow!("for_each references unknown query"))?;

            let mut files = Vec::new();
            for row in results.as_array().unwrap() {
                let mut item_vars = vars.clone();
                item_vars.insert("item", row);

                let path = tera.render_str(to_many, &item_vars)?;
                let content = tera.render_str(&self.body, &item_vars)?;
                files.push((PathBuf::from(path), content));
            }
            Ok(OutputMode::Multiple { files })
        } else {
            // Existing single-file logic
            let path = self.front.to.clone().unwrap_or_default();
            let content = self.render(tera, vars)?;
            Ok(OutputMode::Single { path, content })
        }
    }
}
```

## Conclusion

**Ggen's RDF/SPARQL integration is production-ready** for single-file generation with semantic data. The gap is **file fan-out**: generating N files from N SPARQL result rows.

**Simplest extension**: Add `to_many` + `for_each` to frontmatter, modify renderer to return multiple outputs. This preserves all existing functionality while enabling the multi-file use case.

**No reinvention needed**: Graph loading, SPARQL execution, result caching, and helper functions all work perfectly. Just need the final mile: output multiplexing.
