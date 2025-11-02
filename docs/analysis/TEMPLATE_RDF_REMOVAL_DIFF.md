# Template RDF Removal - Minimal Implementation Diff

## Status: ✅ CORE CHANGES COMPLETE - INTEGRATION NEEDED

This document shows the **exact diffs** needed to complete the RDF removal migration.

---

## Part 1: Core Library Changes (DONE ✅)

### File: ggen-core/src/template.rs

**All changes already implemented in v2.0**

#### Change 1: Line 76 - Remove RDF field
```diff
 #[serde(default, deserialize_with = "string_or_seq")]
 pub rdf_inline: Vec<String>,
-pub rdf: Vec<String>,
+// ❌ REMOVED: rdf: Vec<String> - RDF files now loaded via CLI/API only
 #[serde(default, deserialize_with = "sparql_map")]
```
**Status**: ✅ DONE

#### Change 2: Line 80 - Remove vars field
```diff
 #[serde(default, deserialize_with = "sparql_map")]
 pub sparql: BTreeMap<String, String>,
-pub vars: BTreeMap<String, serde_yaml::Value>,
+// ❌ REMOVED: vars: BTreeMap - Variables now come from CLI/API, not frontmatter
 #[serde(default)]
```
**Status**: ✅ DONE

#### Change 3: Lines 208-209 - Remove RDF loading logic
```diff
     graph.insert_turtle(&final_ttl)?;
 }

-// Load RDF files from frontmatter
-for rdf_path in &self.front.rdf {
-    let ttl_content = std::fs::read_to_string(rdf_path)?;
-    let final_ttl = if prolog.is_empty() {
-        ttl_content
-    } else {
-        format!("{prolog}\n{ttl_content}")
-    };
-    graph.insert_turtle(&final_ttl)?;
-}
+// ❌ REMOVED: RDF file loading from frontmatter
+// RDF files are now loaded via CLI/API using render_with_rdf() method

 // Execute SPARQL (prepend PREFIX/BASE prolog) and capture results
```
**Status**: ✅ DONE

#### Change 4: Lines 252-284 - Add render_with_rdf() API
```diff
+/// Render template with RDF data from external sources (CLI/API).
+/// This is the v2.0 method for loading RDF - frontmatter no longer supports rdf: field.
+pub fn render_with_rdf(
+    &mut self,
+    rdf_files: Vec<std::path::PathBuf>,
+    graph: &mut Graph,
+    tera: &mut Tera,
+    vars: &Context,
+    template_path: &std::path::Path,
+) -> Result<String> {
+    // Render frontmatter first
+    self.render_frontmatter(tera, vars)?;
+
+    // Build prolog from frontmatter prefixes
+    let prolog = crate::graph::build_prolog(&self.front.prefixes, self.front.base.as_deref());
+
+    // Load RDF files from CLI/API (not frontmatter)
+    for rdf_path in rdf_files {
+        let ttl_content = std::fs::read_to_string(&rdf_path).map_err(|e| {
+            anyhow::anyhow!("Failed to read RDF file '{}': {}", rdf_path.display(), e)
+        })?;
+        let final_ttl = if prolog.is_empty() {
+            ttl_content
+        } else {
+            format!("{prolog}\n{ttl_content}")
+        };
+        graph.insert_turtle(&final_ttl)?;
+    }
+
+    // Process graph with inline RDF and SPARQL
+    self.process_graph(graph, tera, vars, template_path)?;
+
+    // Render body
+    self.render(tera, vars)
+}
```
**Status**: ✅ DONE

---

## Part 2: Generator Integration (TODO ⚠️)

### File: ggen-core/src/generator.rs

#### Change: Lines 61-101 - Use render_with_rdf()

**Current Code**:
```rust
pub fn generate(&mut self) -> Result<PathBuf> {
    let input = fs::read_to_string(&self.ctx.template_path)?;
    let mut tmpl = Template::parse(&input)?;

    // Context
    let mut tctx = Context::from_serialize(&self.ctx.vars)?;
    insert_env(&mut tctx);

    // Render frontmatter
    tmpl.render_frontmatter(&mut self.pipeline.tera, &tctx)?;

    // Process graph
    tmpl.process_graph(
        &mut self.pipeline.graph,
        &mut self.pipeline.tera,
        &tctx,
        &self.ctx.template_path,
    )?;

    // Render body
    let rendered = tmpl.render(&mut self.pipeline.tera, &tctx)?;

    // ... rest of method
}
```

**New Code** (with RDF support):
```diff
+pub fn generate(&mut self, rdf_files: Vec<PathBuf>) -> Result<PathBuf> {
-pub fn generate(&mut self) -> Result<PathBuf> {
     let input = fs::read_to_string(&self.ctx.template_path)?;
     let mut tmpl = Template::parse(&input)?;

     // Context
     let mut tctx = Context::from_serialize(&self.ctx.vars)?;
     insert_env(&mut tctx);

-    // Render frontmatter
-    tmpl.render_frontmatter(&mut self.pipeline.tera, &tctx)?;
-
-    // Process graph
-    tmpl.process_graph(
-        &mut self.pipeline.graph,
-        &mut self.pipeline.tera,
-        &tctx,
-        &self.ctx.template_path,
-    )?;
-
-    // Render body
-    let rendered = tmpl.render(&mut self.pipeline.tera, &tctx)?;
+    // Use render_with_rdf() if RDF files provided, otherwise fallback
+    let rendered = if !rdf_files.is_empty() {
+        tmpl.render_with_rdf(
+            rdf_files,
+            &mut self.pipeline.graph,
+            &mut self.pipeline.tera,
+            &tctx,
+            &self.ctx.template_path,
+        )?
+    } else {
+        // Fallback to old workflow (no RDF files)
+        tmpl.render_frontmatter(&mut self.pipeline.tera, &tctx)?;
+        tmpl.process_graph(
+            &mut self.pipeline.graph,
+            &mut self.pipeline.tera,
+            &tctx,
+            &self.ctx.template_path,
+        )?;
+        tmpl.render(&mut self.pipeline.tera, &tctx)?
+    };

     // Determine output path
     let output_path = if let Some(to_path) = &tmpl.front.to {
         // ... rest of method unchanged
     }
 }
```

**Status**: ⚠️ TODO

---

## Part 3: CLI Changes (TODO ⚠️)

### Location: cli/src/commands/template/ or cli/src/domain/template/

#### Add --rdf flag to GenerateCmd

**New Code**:
```rust
use clap::Parser;
use std::path::PathBuf;

#[derive(Parser, Debug)]
pub struct GenerateCmd {
    /// Path to template file
    #[arg(long)]
    template: PathBuf,

    /// Variables in KEY=VALUE format
    #[arg(long, value_name = "KEY=VALUE")]
    var: Vec<String>,

    /// RDF files to load (NEW in v2.0)
    #[arg(long, value_name = "FILE")]
    rdf: Vec<PathBuf>,

    /// Output directory
    #[arg(long, default_value = ".")]
    output: PathBuf,

    /// Dry run mode
    #[arg(long)]
    dry_run: bool,
}

impl GenerateCmd {
    pub fn run(&self) -> anyhow::Result<()> {
        // Parse variables
        let vars = parse_vars(&self.var)?;

        // Create generator context
        let ctx = GenContext::new(self.template.clone(), self.output.clone())
            .with_vars(vars)
            .dry(self.dry_run);

        let pipeline = Pipeline::new()?;
        let mut generator = Generator::new(pipeline, ctx);

        // Generate with RDF files
        let output_path = generator.generate(self.rdf.clone())?;

        println!("Generated: {}", output_path.display());
        Ok(())
    }
}
```

**Status**: ⚠️ TODO

---

## Part 4: Template Migration (TODO ⚠️)

### Template Files to Update

#### 1. hello.tmpl
```diff
 ---
 to: hello.rs
-vars: { name: "hello" }
 ---
 fn main() {
     println!("Hello, {name}!");
 }
```
**CLI Command**: `ggen generate --template hello.tmpl --var name=hello`

#### 2. v1.tmpl
```diff
 ---
 to: hello.rs
-vars: { name: "hello", version: "version: 1.0" }
 ---
 fn main() {
     println!("Hello, {name}!");
     println!("{version}");
 }
```
**CLI Command**: `ggen generate --template v1.tmpl --var name=hello --var version="version: 1.0"`

#### 3. templates/python.tmpl
```diff
 ---
 to: hello.py
-vars: { name: "hello" }
 ---
 def main():
     print(f"Hello, {name}!")
```

#### 4. templates/bash.tmpl
```diff
 ---
 to: hello.sh
-vars: { name: "hello" }
 ---
 #!/bin/bash
 echo "Hello, {name}!"
```

#### 5. templates/rust.tmpl
```diff
 ---
 to: hello.rs
-vars: { name: "hello" }
 ---
 fn main() {
     println!("Hello, {name}!");
 }
```

#### 6. ggen-cleanroom/project.tmpl (CRITICAL)
```diff
 ---
 to: src/main.rs
-rdf: ["schemas/test_schema.ttl"]
 sparql:
   test_containers: "SELECT ?container ?image ?port WHERE { ... }"
   test_lifecycle: "SELECT ?hook ?phase ?action WHERE { ... }"
   test_specs: "SELECT ?spec ?description ?feature ?scenario WHERE { ... }"
-vars:
-  var0:
-    type: "string"
-    name: "project_name"
-    description: "Name of the project"
-    default: "ggen-cleanroom"
-  var1:
-    type: "string"
-    name: "version"
-    description: "Project version"
-    default: "0.1.0"
 ---
 // Template body unchanged
```
**CLI Command**:
```bash
ggen generate \
  --template ggen-cleanroom/project.tmpl \
  --rdf schemas/test_schema.ttl \
  --var project_name=ggen-cleanroom \
  --var version=0.1.0
```

#### 7. marketplace/.../rust-service.tmpl
```diff
 ---
 to: generated/src/services/{{name}}.rs
-vars:
-  name: "user-service"
-  description: "User management service"
-  version: "1.0.0"
-  author: "ggen-examples"
 sparql:
   find_entities: "SELECT ?entity ?label WHERE { ... }"
   find_properties: "SELECT ?entity ?property ?label WHERE { ... }"
   find_relationships: "SELECT ?entity ?relationship ?label WHERE { ... }"
 freeze_policy: "checksum"
 freeze_slots_dir: "generated/.ggen/freeze"
 ---
 // Template body unchanged
```

**Status**: ⚠️ TODO (7 files)

---

## Part 5: Test Cases (TODO ⚠️)

### File: ggen-core/tests/template_rdf_api_tests.rs (NEW FILE)

```rust
use anyhow::Result;
use ggen_core::{graph::Graph, template::Template};
use std::io::Write;
use std::path::Path;
use tempfile::NamedTempFile;
use tera::Context;

fn mk_tera() -> tera::Tera {
    let mut tera = tera::Tera::default();
    ggen_core::register::register_all(&mut tera);
    tera
}

#[test]
fn test_render_with_rdf_single_file() -> Result<()> {
    // Create temporary RDF file
    let mut temp_rdf = NamedTempFile::new()?;
    writeln!(temp_rdf, "@prefix ex: <http://example.org/> .")?;
    writeln!(temp_rdf, "ex:alice a ex:Person ; ex:name 'Alice' .")?;
    writeln!(temp_rdf, "ex:bob a ex:Person ; ex:name 'Bob' .")?;

    // Template with SPARQL
    let template_str = r#"---
to: "output.rs"
sparql:
  people: "SELECT ?name WHERE { ?person a ex:Person ; ex:name ?name } ORDER BY ?name"
---
People: {{ sparql_results.people | length }}"#;

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

    assert!(output.contains("People: 2"));
    Ok(())
}

#[test]
fn test_render_with_rdf_multiple_files() -> Result<()> {
    // Create two RDF files
    let mut rdf1 = NamedTempFile::new()?;
    writeln!(rdf1, "@prefix ex: <http://example.org/> .")?;
    writeln!(rdf1, "ex:alice a ex:Person ; ex:name 'Alice' .")?;

    let mut rdf2 = NamedTempFile::new()?;
    writeln!(rdf2, "@prefix ex: <http://example.org/> .")?;
    writeln!(rdf2, "ex:bob a ex:Person ; ex:name 'Bob' .")?;

    let template_str = r#"---
to: "output.rs"
sparql:
  people: "SELECT ?name WHERE { ?person a ex:Person ; ex:name ?name } ORDER BY ?name"
---
Count: {{ sparql_results.people | length }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    let output = template.render_with_rdf(
        vec![rdf1.path().to_path_buf(), rdf2.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    assert!(output.contains("Count: 2"));
    Ok(())
}

#[test]
fn test_render_with_rdf_missing_file_error() {
    let template_str = "---\nto: 'x'\n---\nBody";
    let mut template = Template::parse(template_str).unwrap();
    let mut graph = Graph::new().unwrap();
    let mut tera = mk_tera();
    let vars = Context::new();

    let result = template.render_with_rdf(
        vec![std::path::PathBuf::from("/nonexistent/file.ttl")],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    );

    assert!(result.is_err());
    let err_msg = result.unwrap_err().to_string();
    assert!(err_msg.contains("Failed to read RDF file"));
}

#[test]
fn test_render_with_rdf_empty_file_list() -> Result<()> {
    let template_str = r#"---
to: "output.rs"
---
Hello, World!"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    let output = template.render_with_rdf(
        vec![],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    assert!(output.contains("Hello, World!"));
    Ok(())
}

#[test]
fn test_render_with_rdf_prefixes_from_frontmatter() -> Result<()> {
    let mut temp_rdf = NamedTempFile::new()?;
    writeln!(temp_rdf, "ex:alice a ex:Person ; ex:name 'Alice' .")?;

    let template_str = r#"---
to: "output.rs"
prefixes: { ex: "http://example.org/" }
sparql:
  people: "SELECT ?name WHERE { ?person a ex:Person ; ex:name ?name }"
---
First: {{ sparql_first(results=sparql_results.people, column="name") }}"#;

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

    assert!(output.contains("First:"));
    Ok(())
}

#[test]
fn test_render_with_rdf_and_rdf_inline() -> Result<()> {
    // External RDF file
    let mut temp_rdf = NamedTempFile::new()?;
    writeln!(temp_rdf, "@prefix ex: <http://example.org/> .")?;
    writeln!(temp_rdf, "ex:alice a ex:Person ; ex:name 'Alice' .")?;

    // Template with inline RDF
    let template_str = r#"---
to: "output.rs"
prefixes: { ex: "http://example.org/" }
rdf_inline: "ex:bob a ex:Person ; ex:name 'Bob' ."
sparql:
  people: "SELECT ?name WHERE { ?person a ex:Person ; ex:name ?name } ORDER BY ?name"
---
Count: {{ sparql_results.people | length }}"#;

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

    // Should have both external (Alice) and inline (Bob)
    assert!(output.contains("Count: 2"));
    Ok(())
}
```

**Status**: ⚠️ TODO (create new test file)

---

## Part 6: Implementation Checklist

### Core Library ✅
- [x] Remove `rdf` field from Frontmatter struct
- [x] Remove `vars` field from Frontmatter struct
- [x] Remove RDF loading logic from `process_graph()`
- [x] Add `render_with_rdf()` method
- [x] Keep SPARQL execution
- [x] Keep `rdf_inline` field
- [x] Update documentation

### Generator Integration ⚠️
- [ ] Update `Generator::generate()` signature
- [ ] Add `rdf_files` parameter
- [ ] Use `render_with_rdf()` when RDF files provided
- [ ] Keep backward compatibility (fallback)

### CLI Changes ⚠️
- [ ] Add `--rdf` flag to GenerateCmd
- [ ] Parse RDF file paths
- [ ] Pass to Generator
- [ ] Update CLI documentation

### Template Migration ⚠️
- [ ] hello.tmpl - remove vars
- [ ] v1.tmpl - remove vars
- [ ] templates/python.tmpl - remove vars
- [ ] templates/bash.tmpl - remove vars
- [ ] templates/rust.tmpl - remove vars
- [ ] marketplace/.../rust-service.tmpl - remove vars
- [ ] ggen-cleanroom/project.tmpl - remove vars + rdf

### Testing ⚠️
- [ ] test_render_with_rdf_single_file
- [ ] test_render_with_rdf_multiple_files
- [ ] test_render_with_rdf_missing_file_error
- [ ] test_render_with_rdf_empty_file_list
- [ ] test_render_with_rdf_prefixes_from_frontmatter
- [ ] test_render_with_rdf_and_rdf_inline
- [ ] Integration test with CLI

### Documentation ⚠️
- [ ] Update README with new CLI usage
- [ ] Update migration guide
- [ ] Add examples for --rdf flag
- [ ] Update CHANGELOG

---

## Estimated Work

| Task | Time | Priority |
|------|------|----------|
| Add test file | 2-3 hours | HIGH |
| Update Generator | 1 hour | HIGH |
| Add CLI flag | 30 min | MEDIUM |
| Migrate templates | 1 hour | MEDIUM |
| Update docs | 1 hour | LOW |
| **TOTAL** | **5-6 hours** | - |

---

## Validation Commands

```bash
# Run all tests
cargo test --package ggen-core

# Run specific test
cargo test --package ggen-core test_render_with_rdf

# Build CLI
cargo build --package ggen

# Test CLI with new flag
./target/debug/ggen generate \
  --template hello.tmpl \
  --var name=Alice \
  --rdf data/schema.ttl

# Verify no regressions
cargo test --all

# Check for deprecated patterns
rg "vars:" --type yaml --glob '**/*.tmpl'
rg "rdf:" --type yaml --glob '**/*.tmpl'
```

---

**Status**: Ready for implementation
**Priority**: Critical path for v2.0 release
**Blockers**: None - core changes complete
**Next Step**: Add tests for `render_with_rdf()`

---
**Created**: 2025-11-01
**See Also**:
- TEMPLATE_RDF_REMOVAL_FINAL_REPORT.md (full analysis)
- TEMPLATE_RDF_QUICK_REFERENCE.md (quick lookup)
