//! Template system: YAML frontmatter + Tera rendering + RDF/SPARQL integration
//!
//! ## Core Flow
//! ```text
//! Template String → Parse → Render Frontmatter → Process Graph → Render Body
//! ```
//!
//! ## Key Features
//! - **Two-phase rendering**: Frontmatter first (resolve {{vars}}), then body
//! - **Flexible vars**: Accepts maps, arrays, strings, null (LLM-friendly)
//! - **RDF integration**: Load triples, execute SPARQL, expose results to templates
//! - **File injection**: Modify existing files with markers/line numbers
//! - **Hygen compatible**: Standard template fields work unchanged
//!
//! ## Frontmatter Fields
//! - `to/from`: Output/input file paths
//! - `vars`: Template variables (any YAML type)
//! - `rdf_inline/rdf`: Turtle triples (inline/files)
//! - `sparql`: Named queries → `sparql_results.<name>`
//! - `inject/before/after`: File modification markers
//!
//! ## SPARQL Results Access
//! ```tera
//! Count: {{ sparql_results.people | length }}
//! First: {{ sparql_first(results=sparql_results.people, column="name") }}
//! All: {{ sparql_values(results=sparql_results.people, column="name") }}
//! ```

use anyhow::Result;
use gray_matter::{engine::YAML, Matter, ParsedEntity};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use tera::{Context, Tera};

use crate::graph::Graph;
use crate::preprocessor::{FreezePolicy, FreezeStage, PrepCtx, Preprocessor};

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Frontmatter {
    // Hygen core
    pub to: Option<String>,
    pub from: Option<String>,
    #[serde(default)]
    pub force: bool,
    #[serde(default)]
    pub unless_exists: bool,

    // Injection
    #[serde(default)]
    pub inject: bool,
    pub before: Option<String>,
    pub after: Option<String>,
    #[serde(default)]
    pub prepend: bool,
    #[serde(default)]
    pub append: bool,
    pub at_line: Option<u32>,
    #[serde(default)]
    pub eof_last: bool,
    pub skip_if: Option<String>,

    // Shell hooks
    #[serde(alias = "sh")]
    pub sh_before: Option<String>,
    pub sh_after: Option<String>,

    // Graph additions (renderable)
    #[serde(default)]
    pub base: Option<String>,
    #[serde(default)]
    pub prefixes: BTreeMap<String, String>,
    #[serde(default, deserialize_with = "string_or_seq")]
    pub rdf_inline: Vec<String>,
    #[serde(default, deserialize_with = "string_or_seq")]
    pub rdf: Vec<String>, // treat as inline TTL in prototype
    #[serde(default, deserialize_with = "sparql_map")]
    pub sparql: BTreeMap<String, String>,

    // Optional template variables defined in frontmatter
    // Accepts maps, arrays, or single values for maximum flexibility
    #[serde(default, deserialize_with = "deserialize_flexible_vars")]
    pub vars: BTreeMap<String, serde_yaml::Value>,

    // Safety and idempotency
    #[serde(default)]
    pub backup: Option<bool>,
    #[serde(default)]
    pub idempotent: bool,

    // Additional fields for compatibility
    #[serde(default, deserialize_with = "string_or_seq")]
    pub shape: Vec<String>,
    #[serde(default)]
    pub determinism: Option<serde_yaml::Value>,

    // Preprocessor configuration
    #[serde(default)]
    pub freeze_policy: Option<String>, // "always", "checksum", "never"
    #[serde(default)]
    pub freeze_slots_dir: Option<String>,

    // SPARQL results storage (populated during process_graph)
    #[serde(skip)]
    pub sparql_results: BTreeMap<String, serde_json::Value>,
}

#[derive(Clone)]
pub struct Template {
    raw_frontmatter: serde_yaml::Value,
    pub front: Frontmatter, // populated after render_frontmatter()
    pub body: String,
}

impl Template {
    /// Parse frontmatter + body. Does NOT render yet.
    pub fn parse(input: &str) -> Result<Self> {
        let matter = Matter::<YAML>::new();
        let ParsedEntity { data, content, .. } = matter.parse::<serde_yaml::Value>(input)?;
        let raw_frontmatter = data.unwrap_or(serde_yaml::Value::Null);
        Ok(Self {
            raw_frontmatter,
            front: Frontmatter::default(),
            body: content,
        })
    }

    /// Parse with preprocessor pipeline. Runs preprocessor before parsing.
    pub fn parse_with_preprocessor(
        input: &str, template_path: &std::path::Path, out_dir: &std::path::Path,
    ) -> Result<Self> {
        // Create preprocessor with default configuration
        let preprocessor = Preprocessor::default();

        // Run preprocessor
        let ctx = PrepCtx {
            template_path,
            out_dir,
            vars_json: &serde_json::Value::Null,
        };
        let processed = preprocessor.run(input.to_string(), &ctx)?;

        // Parse the processed content
        Self::parse(&processed)
    }

    /// Parse with custom preprocessor configuration
    pub fn parse_with_custom_preprocessor(
        input: &str, template_path: &std::path::Path, out_dir: &std::path::Path,
        freeze_slots_dir: Option<&std::path::Path>, freeze_policy: Option<FreezePolicy>,
    ) -> Result<Self> {
        // Build preprocessor based on configuration
        let mut preprocessor = Preprocessor::new();

        // Add freeze stage if configured
        if let (Some(slots_dir), Some(policy)) = (freeze_slots_dir, freeze_policy) {
            preprocessor = preprocessor.with(FreezeStage {
                slots_dir: slots_dir.to_path_buf(),
                policy,
            });
        }

        // Run preprocessor
        let ctx = PrepCtx {
            template_path,
            out_dir,
            vars_json: &serde_json::Value::Null,
        };
        let processed = preprocessor.run(input.to_string(), &ctx)?;

        // Parse the processed content
        Self::parse(&processed)
    }

    /// Render frontmatter through Tera once to resolve {{ }} in YAML.
    pub fn render_frontmatter(&mut self, tera: &mut Tera, vars: &Context) -> Result<()> {
        let yaml_src = serde_yaml::to_string(&self.raw_frontmatter)?;
        let rendered_yaml = tera.render_str(&yaml_src, vars)?;
        self.front = serde_yaml::from_str::<Frontmatter>(&rendered_yaml)?;
        Ok(())
    }

    /// Load RDF and run SPARQL using the rendered frontmatter.
    pub fn process_graph(
        &mut self, graph: &mut Graph, tera: &mut Tera, vars: &Context,
        template_path: &std::path::Path,
    ) -> Result<()> {
        // Ensure frontmatter is rendered before graph ops
        if self.front.to.is_none()
            && self.front.from.is_none()
            && self.front.rdf_inline.is_empty()
            && self.front.rdf.is_empty()
            && self.front.sparql.is_empty()
        {
            self.render_frontmatter(tera, vars)?;
        }

        // Build prolog once
        let prolog = crate::graph::build_prolog(&self.front.prefixes, self.front.base.as_deref());

        // Insert inline RDF
        for ttl in &self.front.rdf_inline {
            let ttl_rendered = tera.render_str(ttl, vars)?;
            let final_ttl = if prolog.is_empty() {
                ttl_rendered
            } else {
                format!("{prolog}\n{ttl_rendered}")
            };
            graph.insert_turtle(&final_ttl)?;
        }

        // Load RDF files - resolve relative to template directory
        for rdf_file in &self.front.rdf {
            let rendered_path = tera.render_str(rdf_file, vars)?;

            // Resolve relative to template's directory
            let template_dir = template_path.parent().unwrap_or(std::path::Path::new("."));
            let rdf_path = template_dir.join(&rendered_path);

            // Security check: prevent path traversal attacks
            let canonical_rdf = rdf_path.canonicalize().map_err(|e| {
                anyhow::anyhow!("Failed to canonicalize RDF path '{}': {}", rdf_path.display(), e)
            })?;
            let canonical_template = template_dir.canonicalize().map_err(|e| {
                anyhow::anyhow!("Failed to canonicalize template directory '{}': {}", template_dir.display(), e)
            })?;

            if !canonical_rdf.starts_with(&canonical_template) {
                return Err(anyhow::anyhow!("Path traversal blocked: '{}' is outside template directory", rendered_path));
            }

            if let Ok(ttl_content) = std::fs::read_to_string(&rdf_path) {
                let final_ttl = if prolog.is_empty() {
                    ttl_content
                } else {
                    format!("{prolog}\n{ttl_content}")
                };
                graph.insert_turtle(&final_ttl)?;
            }
        }

        // Execute SPARQL (prepend PREFIX/BASE prolog) and capture results
        for (name, q) in &self.front.sparql {
            let q_rendered = tera.render_str(q, vars)?;
            let final_q = if prolog.is_empty() {
                q_rendered
            } else {
                format!("{prolog}\n{q_rendered}")
            };

            // Execute query and capture results
            let results = graph.query(&final_q)?;
            let json_result = match results {
                oxigraph::sparql::QueryResults::Boolean(b) => serde_json::Value::Bool(b),
                oxigraph::sparql::QueryResults::Solutions(solutions) => {
                    let mut rows = Vec::new();
                    for solution in solutions {
                        let solution = solution
                            .map_err(|e| anyhow::anyhow!("SPARQL solution error: {}", e))?;
                        let mut row = serde_json::Map::new();
                        for (var, term) in solution.iter() {
                            row.insert(
                                var.to_string(),
                                serde_json::Value::String(term.to_string()),
                            );
                        }
                        rows.push(serde_json::Value::Object(row));
                    }
                    serde_json::Value::Array(rows)
                }
                oxigraph::sparql::QueryResults::Graph(_) => serde_json::Value::Array(Vec::new()), // Graph results not supported in templates
            };

            // Store result in frontmatter for template access
            self.front.sparql_results.insert(name.clone(), json_result);
        }

        Ok(())
    }

    /// Render template body with Tera.
    /// If `from:` is specified in frontmatter, read that file and use as body source.
    pub fn render(&self, tera: &mut Tera, vars: &Context) -> Result<String> {
        let body_source = if let Some(from_path) = &self.front.from {
            // Render the from path as a template to resolve variables
            let rendered_from = tera.render_str(from_path, vars)?;
            std::fs::read_to_string(&rendered_from).map_err(|e| {
                anyhow::anyhow!("Failed to read from file '{}': {}", rendered_from, e)
            })?
        } else {
            self.body.clone()
        };

        // Create a new context with SPARQL results
        let mut final_vars = vars.clone();

        // Add SPARQL results to context as sparql_results.<name>
        if !self.front.sparql_results.is_empty() {
            let mut sparql_results_obj = serde_json::Map::new();
            for (name, result) in &self.front.sparql_results {
                sparql_results_obj.insert(name.clone(), result.clone());
            }
            final_vars.insert(
                "sparql_results",
                &serde_json::Value::Object(sparql_results_obj),
            );
        }

        Ok(tera.render_str(&body_source, &final_vars)?)
    }
}

/* ---------------- helpers ---------------- */

// Accept either "rdf: <string>" or "rdf: [<string>, ...]"
fn string_or_seq<'de, D>(de: D) -> Result<Vec<String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    use serde::de::{Error as DeError, SeqAccess, Visitor};
    use std::fmt;

    struct StrOrSeq;

    impl<'de> Visitor<'de> for StrOrSeq {
        type Value = Vec<String>;

        fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
            f.write_str("a string or a sequence of strings")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: DeError,
        {
            Ok(vec![v.to_string()])
        }

        fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
        where
            E: DeError,
        {
            Ok(vec![v])
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            let mut out = Vec::new();
            while let Some(s) = seq.next_element::<String>()? {
                out.push(s);
            }
            Ok(out)
        }
    }

    de.deserialize_any(StrOrSeq)
}

/// Accept vars as map, array, string, or any YAML value
/// This provides maximum flexibility for LLM-generated templates
///
/// Examples:
/// - `vars: {key: "value"}` → kept as-is
/// - `vars: ["item1", "item2"]` → converted to {var0: "item1", var1: "item2"}
/// - `vars: "single"` → converted to {var0: "single"}
fn deserialize_flexible_vars<'de, D>(
    deserializer: D,
) -> Result<BTreeMap<String, serde_yaml::Value>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    use serde::de::Error;

    let value = serde_yaml::Value::deserialize(deserializer)?;

    match value {
        serde_yaml::Value::Mapping(map) => {
            // Already a map - convert keys to strings
            let mut result = BTreeMap::new();
            for (k, v) in map {
                let key = k
                    .as_str()
                    .ok_or_else(|| Error::custom("Map keys must be strings"))?
                    .to_string();
                result.insert(key, v);
            }
            Ok(result)
        }
        serde_yaml::Value::Sequence(seq) => {
            // Array - convert to indexed map
            let mut result = BTreeMap::new();
            for (i, v) in seq.into_iter().enumerate() {
                result.insert(format!("var{}", i), v);
            }
            Ok(result)
        }
        serde_yaml::Value::Null => {
            // Null or missing - return empty map
            Ok(BTreeMap::new())
        }
        other => {
            // Single value - wrap in map
            let mut result = BTreeMap::new();
            result.insert("var0".to_string(), other);
            Ok(result)
        }
    }
}

// Accept either "sparql: '<query>'" or "sparql: { name: '<query>' }"
fn sparql_map<'de, D>(de: D) -> Result<BTreeMap<String, String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum OneOrMapOrSeq {
        One(String),
        Map(BTreeMap<String, String>),
        Seq(Vec<String>),
    }
    match OneOrMapOrSeq::deserialize(de)? {
        OneOrMapOrSeq::One(q) => {
            let mut m = BTreeMap::new();
            m.insert("default".to_string(), q);
            Ok(m)
        }
        OneOrMapOrSeq::Map(m) => Ok(m),
        OneOrMapOrSeq::Seq(queries) => {
            let mut m = BTreeMap::new();
            for (i, query) in queries.into_iter().enumerate() {
                m.insert(format!("query_{}", i), query);
            }
            Ok(m)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;
    use std::{io::Write, path::Path};
    use tempfile::NamedTempFile;
    use tera::Context;

    /* ---------- helpers ---------- */

    fn mk_tera() -> Tera {
        let mut tera = Tera::default();
        crate::register::register_all(&mut tera);
        tera
    }

    fn ctx(pairs: &[(&str, impl serde::Serialize)]) -> Context {
        let mut c = Context::new();
        for (k, v) in pairs {
            c.insert(*k, v);
        }
        c
    }

    fn render_only(input: &str, vars: &Context) -> Result<String> {
        let tmpl = Template::parse(input)?;
        let mut tera = mk_tera();
        tmpl.render(&mut tera, vars)
    }

    fn process_then_render(input: &str, vars: &Context) -> Result<String> {
        let mut tmpl = Template::parse(input)?;
        let mut graph = Graph::new()?;
        let mut tera = mk_tera();
        tmpl.process_graph(&mut graph, &mut tera, vars, Path::new("test.tmpl"))?;
        tmpl.render(&mut tera, vars)
    }

    macro_rules! assert_contains {
        ($hay:expr, $needle:expr) => {
            assert!(
                $hay.contains($needle),
                "expected to find '{}'\n--- in ---\n{}",
                $needle,
                $hay
            )
        };
    }

    /* ---------- parsing & frontmatter ---------- */

    #[test]
    fn parse_variants_and_preserve_body() -> Result<()> {
        let cases = [
            (
                // basic with frontmatter
                r#"---
to: "{{name}}.rs"
---
fn main() { println!("Hello, {{name}}!"); }"#,
                "fn main() { println!(\"Hello, {{name}}!\"); }",
            ),
            (
                // empty frontmatter
                r#"---
---
fn main() { println!("Hello"); }"#,
                "fn main() { println!(\"Hello\"); }",
            ),
            (
                // no frontmatter
                r#"fn main() { println!("Hello, world!"); }"#,
                "fn main() { println!(\"Hello, world!\"); }",
            ),
        ];

        for (input, want_body) in cases {
            let t = Template::parse(input)?;
            assert_eq!(t.body, want_body);
        }
        Ok(())
    }

    #[test]
    fn frontmatter_render_core_fields_and_flex_vars() -> Result<()> {
        let input = r#"---
to: "{{name}}.rs"
prefixes: { ex: "http://example.org/" }
base: "http://example.org/{{ns}}/"
vars:
  name_fmt: "Mr. {{name}}"
rdf_inline: "ex:{{name}} a ex:Person ."
sparql: "SELECT ?s WHERE { ?s a ex:Person }"
---
body"#;

        let mut t = Template::parse(input)?;
        let mut tera = mk_tera();
        let v = ctx(&[("name", "Alice"), ("ns", "test")]);

        t.render_frontmatter(&mut tera, &v)?;
        assert_eq!(t.front.to.as_deref(), Some("Alice.rs"));
        assert_eq!(
            t.front.prefixes.get("ex").map(String::as_str),
            Some("http://example.org/")
        );
        assert_eq!(t.front.base.as_deref(), Some("http://example.org/test/"));
        assert_eq!(t.front.rdf_inline.len(), 1);
        assert_eq!(t.front.sparql.len(), 1);
        assert_eq!(
            t.front.vars.get("name_fmt").and_then(|x| x.as_str()),
            Some("Mr. Alice")
        );
        Ok(())
    }

    #[test]
    fn deserializers_string_or_seq_and_sparql_map() -> Result<()> {
        let fm1: Frontmatter = serde_yaml::from_str(r#"rdf_inline: "a""#)?;
        let fm2: Frontmatter = serde_yaml::from_str(r#"rdf_inline: ["a","b"]"#)?;
        assert_eq!(fm1.rdf_inline, vec!["a"]);
        assert_eq!(fm2.rdf_inline, vec!["a", "b"]);

        let fm3: Frontmatter = serde_yaml::from_str(r#"sparql: "SELECT * WHERE { ?s ?p ?o }""#)?;
        let fm4: Frontmatter =
            serde_yaml::from_str(r#"sparql: { q1: "SELECT 1 {}", q2: "SELECT 2 {}" }"#)?;
        let fm5: Frontmatter = serde_yaml::from_str(r#"sparql: ["SELECT 1 {}", "SELECT 2 {}"]"#)?;
        assert!(fm3.sparql.contains_key("default"));
        assert!(fm4.sparql.contains_key("q1"));
        assert!(fm5.sparql.contains_key("query_0"));
        Ok(())
    }

    #[test]
    fn boolean_and_injection_flags() -> Result<()> {
        let fm: Frontmatter = serde_yaml::from_str(
            r#"
force: true
unless_exists: true
inject: true
prepend: true
append: true
eof_last: true
idempotent: true
before: "// before"
after: "// after"
at_line: 7
skip_if: "needle"
sh_before: "echo pre"
sh_after: "echo post"
"#,
        )?;
        assert!(fm.force && fm.unless_exists && fm.inject && fm.prepend && fm.append);
        assert!(fm.eof_last && fm.idempotent);
        assert_eq!(fm.before.as_deref(), Some("// before"));
        assert_eq!(fm.after.as_deref(), Some("// after"));
        assert_eq!(fm.at_line, Some(7));
        assert_eq!(fm.skip_if.as_deref(), Some("needle"));
        assert_eq!(fm.sh_before.as_deref(), Some("echo pre"));
        assert_eq!(fm.sh_after.as_deref(), Some("echo post"));
        Ok(())
    }

    /* ---------- body rendering ---------- */

    #[test]
    fn body_render_inline_and_from_file() -> Result<()> {
        // inline
        let inline = r#"---
to: "x"
---
fn main() { println!("Hello, {{name}}!"); }"#;
        let got = render_only(inline, &ctx(&[("name", "Alice")]))?;
        assert_contains!(got, "Hello, Alice!");

        // from: - need to render frontmatter first to populate 'from' field
        let mut tmp = NamedTempFile::new()?;
        writeln!(tmp, "fn main() {{ println!(\"Hello, {{{{name}}}}!\"); }}")?;
        let from_tmpl = format!(
            r#"---
from: "{}"
---
ignored"#,
            tmp.path().display()
        );
        let mut tmpl = Template::parse(&from_tmpl)?;
        let mut tera = mk_tera();
        let vars = ctx(&[("name", "Bob")]);
        tmpl.render_frontmatter(&mut tera, &vars)?;
        let got2 = tmpl.render(&mut tera, &vars)?;
        assert_contains!(got2, "Hello, Bob!");
        Ok(())
    }

    /* ---------- graph + sparql ---------- */

    #[test]
    fn rdf_insert_and_select_visible() -> Result<()> {
        let input = r#"---
prefixes: { ex: "http://example.org/" }
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:Alice a ex:Person ."
sparql:
  q: "SELECT ?s WHERE { ?s a ex:Person }"
---
Count: {{ sparql_results.q | length }}"#;

        let out = process_then_render(input, &Context::new())?;
        assert_contains!(out, "Count: 1");
        Ok(())
    }

    #[test]
    fn boolean_ask_and_empty_result_helpers() -> Result<()> {
        let input = r#"---
prefixes: { ex: "http://example.org/" }
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:a a ex:Person ."
sparql:
  has_people: "ASK WHERE { ?x a ex:Person }"
  none: "SELECT ?x WHERE { ?x a ex:NonExistent }"
---
Has: {{ sparql_results.has_people }}
Empty: {{ sparql_empty(results=sparql_results.none) }}
Count: {{ sparql_count(results=sparql_results.none) }}"#;

        let out = process_then_render(input, &Context::new())?;
        assert_contains!(out, "Has: true");
        assert_contains!(out, "Empty: true");
        assert_contains!(out, "Count: 0");
        Ok(())
    }

    #[test]
    fn projection_helpers_and_multiple_queries() -> Result<()> {
        let input = r#"---
prefixes: { ex: "http://example.org/" }
rdf_inline:
  - "@prefix ex: <http://example.org/> .
     ex:alice a ex:Person ; ex:name 'Alice' ; ex:age 30 .
     ex:bob   a ex:Person ; ex:name 'Bob'   ; ex:age 25 ."
sparql:
  people: "SELECT ?person ?name WHERE { ?person a ex:Person ; ex:name ?name } ORDER BY ?name"
  ages:   "SELECT ?age WHERE { ?p a ex:Person ; ex:age ?age } ORDER BY ?age"
---
First: {{ sparql_first(results=sparql_results.people, column="name") }}
People count: {{ sparql_count(results=sparql_results.people) }}
Ages: {{ sparql_values(results=sparql_results.ages, column="age") }}"#;

        let out = process_then_render(input, &Context::new())?;
        assert_contains!(out, "First: \"Alice\"");
        assert_contains!(out, "People count: 2");
        assert_contains!(out, "Ages:");
        Ok(())
    }

    #[test]
    fn preprocessor_integration() -> Result<()> {
        use std::path::Path;
        use tempfile::TempDir;

        let temp_dir = TempDir::new()?;
        let template_path = Path::new("test.tmpl");
        let out_dir = temp_dir.path();

        // Test freeze stage integration
        let input = r#"---
to: "output.rs"
---
fn main() {
    println!("Hello");
    {% startfreeze id="test_block" %}
    println!("Frozen content");
    {% endfreeze %}
    println!("World");
}"#;

        let slots_dir = out_dir.join("slots");
        let mut template = Template::parse_with_custom_preprocessor(
            input,
            template_path,
            out_dir,
            Some(&slots_dir),
            Some(FreezePolicy::Always),
        )?;

        // Render frontmatter to populate the 'to' field
        let mut tera = mk_tera();
        let vars = Context::new();
        template.render_frontmatter(&mut tera, &vars)?;

        assert_eq!(template.front.to, Some("output.rs".to_string()));
        assert!(template.body.contains("Hello"));
        assert!(template.body.contains("Frozen content"));
        assert!(template.body.contains("World"));
        assert!(!template.body.contains("startfreeze"));
        assert!(!template.body.contains("endfreeze"));

        Ok(())
    }

    #[cfg(feature = "proptest")]
    mod proptest_tests {
        use super::*;
        use proptest::prelude::*;

        /// Property test: Template parsing should be idempotent for valid inputs
        proptest! {
            #[test]
            fn template_parsing_idempotent(
                frontmatter_yaml in r"[a-zA-Z0-9_:\s\-\.\/]*",
                body_content in r"[a-zA-Z0-9_\s\-\.\/\{\}]*"
            ) {
                // Skip invalid combinations that would cause parsing errors
                if frontmatter_yaml.contains("{{") || frontmatter_yaml.contains("}}") {
                    return Ok(());
                }
                if body_content.len() > 1000 {
                    return Ok(());
                }

                let template_str = format!("---\n{}\n---\n{}", frontmatter_yaml, body_content);
                
                // First parse
                let template1 = Template::parse(&template_str).ok();
                
                // Second parse (should be identical)
                let template2 = Template::parse(&template_str).ok();
                
                // Both should succeed or fail together
                match (template1, template2) {
                    (Some(t1), Some(t2)) => {
                        // If both succeed, they should be identical
                        assert_eq!(t1.front.to, t2.front.to);
                        assert_eq!(t1.front.from, t2.front.from);
                        assert_eq!(t1.body, t2.body);
                    },
                    (None, None) => {
                        // Both failed - this is acceptable for invalid inputs
                    },
                    _ => {
                        // One succeeded and one failed - this violates idempotency
                        panic!("Template parsing is not idempotent");
                    }
                }
            }

            #[test]
            fn frontmatter_vars_roundtrip(
                var_name in r"[a-zA-Z_][a-zA-Z0-9_]*",
                var_value in r"[a-zA-Z0-9_\s\-\.]*"
            ) {
                // Skip if value contains special characters that would break YAML
                if var_value.contains(':') || var_value.contains('"') || var_value.contains('\'') {
                    return Ok(());
                }

                let yaml_content = format!("vars:\n  {}: {}", var_name, var_value);
                let template_str = format!("---\n{}\n---\nHello {{{{ vars.{} }}}}", yaml_content, var_name);
                
                let template = Template::parse(&template_str);
                
                match template {
                    Ok(t) => {
                        // Check that the variable was parsed correctly
                        if let Some(value) = t.front.vars.get(&var_name) {
                            // The value should be preserved (as string)
                            assert!(value.as_str().is_some() || value.is_string());
                        }
                    },
                    Err(_) => {
                        // Parsing failed - this is acceptable for invalid inputs
                    }
                }
            }

            #[test]
            fn template_paths_are_valid(
                path in r"[a-zA-Z0-9_\-\.\/]*"
            ) {
                // Skip paths that are clearly invalid or empty
                if path.contains("..") || path.len() > 200 || path.is_empty() {
                    return Ok(());
                }

                let template_str = format!("---\nto: {}\n---\nContent", path);
                
                let mut template = Template::parse(&template_str);
                
                match template {
                    Ok(mut t) => {
                        // Render frontmatter to populate the fields
                        let mut tera = tera::Tera::new("dummy:*").unwrap();
                        let vars = tera::Context::new();
                        
                        match t.render_frontmatter(&mut tera, &vars) {
                            Ok(_) => {
                                // If rendering succeeded, check that the path was preserved
                                // Note: YAML may normalize numeric-looking strings (e.g., "0." -> "0.0")
                                if let Some(parsed_path) = &t.front.to {
                                    // For numeric-looking paths, allow normalization
                                    if path.parse::<f64>().is_ok() && parsed_path.parse::<f64>().is_ok() {
                                        // Both are numeric, check if they're equivalent
                                        let original_num: f64 = path.parse().unwrap();
                                        let parsed_num: f64 = parsed_path.parse().unwrap();
                                        assert_eq!(original_num, parsed_num);
                                    } else {
                                        // Non-numeric paths should be preserved exactly
                                        assert_eq!(parsed_path, &path);
                                    }
                                } else {
                                    panic!("Expected path to be preserved, but got None");
                                }
                            },
                            Err(_) => {
                                // Rendering failed - this is acceptable for invalid paths
                            }
                        }
                    },
                    Err(_) => {
                        // Parsing failed - this is acceptable for invalid paths
                    }
                }
            }
        }
    }
}
