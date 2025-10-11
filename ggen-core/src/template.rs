use anyhow::Result;
use gray_matter::{engine::YAML, Matter, ParsedEntity};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use tera::{Context, Tera};

use crate::graph::Graph;

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

            if let Ok(ttl_content) = std::fs::read_to_string(&rdf_path) {
                let final_ttl = if prolog.is_empty() {
                    ttl_content
                } else {
                    format!("{prolog}\n{ttl_content}")
                };
                graph.insert_turtle(&final_ttl)?;
            }
        }

        // Execute SPARQL (prepend PREFIX/BASE prolog)
        for q in self.front.sparql.values() {
            let q_rendered = tera.render_str(q, vars)?;
            let final_q = if prolog.is_empty() {
                q_rendered
            } else {
                format!("{prolog}\n{q_rendered}")
            };
            let _ = graph.query(&final_q)?;
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

        Ok(tera.render_str(&body_source, vars)?)
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
                let key = k.as_str()
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
    use std::io::Write;
    use tera::Context;

    #[test]
    fn test_template_parse_basic() -> Result<()> {
        let input = r#"---
to: "{{name}}.rs"
---
fn main() {
    println!("Hello, {{name}}!");
}"#;
        let template = Template::parse(input)?;

        assert_eq!(
            template.body,
            "fn main() {\n    println!(\"Hello, {{name}}!\");\n}"
        );
        assert!(template.front.to.is_none()); // Not rendered yet

        Ok(())
    }

    #[test]
    fn test_template_parse_no_frontmatter() -> Result<()> {
        let input = r#"fn main() {
    println!("Hello, world!");
}"#;
        let template = Template::parse(input)?;

        assert_eq!(
            template.body,
            "fn main() {\n    println!(\"Hello, world!\");\n}"
        );

        Ok(())
    }

    #[test]
    fn test_template_parse_empty_frontmatter() -> Result<()> {
        let input = r#"---
---
fn main() {
    println!("Hello, world!");
}"#;
        let template = Template::parse(input)?;

        assert_eq!(
            template.body,
            "fn main() {\n    println!(\"Hello, world!\");\n}"
        );

        Ok(())
    }

    #[test]
    fn test_render_frontmatter() -> Result<()> {
        let input = r#"---
to: "{{name}}.rs"
vars:
  greeting: "Hello"
---
fn main() {
    println!("{{greeting}}, {{name}}!");
}"#;
        let mut template = Template::parse(input)?;

        let mut tera = Tera::default();
        let mut vars = Context::new();
        vars.insert("name", "Alice");

        template.render_frontmatter(&mut tera, &vars)?;

        assert_eq!(template.front.to, Some("Alice.rs".to_string()));
        let greeting = template.front.vars.get("greeting")
            .and_then(|v| v.as_str())
            .expect("greeting should be a string");
        assert_eq!(greeting, "Hello");

        Ok(())
    }

    #[test]
    fn test_render_frontmatter_with_prefixes() -> Result<()> {
        let input = r#"---
prefixes:
  ex: "http://example.org/"
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
base: "http://example.org/{{namespace}}/"
---
fn main() {
    println!("Hello, world!");
}"#;
        let mut template = Template::parse(input)?;

        let mut tera = Tera::default();
        let mut vars = Context::new();
        vars.insert("namespace", "test");

        template.render_frontmatter(&mut tera, &vars)?;

        assert_eq!(
            template.front.prefixes.get("ex"),
            Some(&"http://example.org/".to_string())
        );
        assert_eq!(
            template.front.prefixes.get("rdf"),
            Some(&"http://www.w3.org/1999/02/22-rdf-syntax-ns#".to_string())
        );
        assert_eq!(
            template.front.base,
            Some("http://example.org/test/".to_string())
        );

        Ok(())
    }

    #[test]
    fn test_render_frontmatter_with_rdf_inline() -> Result<()> {
        let input = r#"---
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:{{name}} a ex:Person ."
---
fn main() {
    println!("Hello, world!");
}"#;
        let mut template = Template::parse(input)?;

        let mut tera = Tera::default();
        let mut vars = Context::new();
        vars.insert("name", "Alice");

        template.render_frontmatter(&mut tera, &vars)?;

        assert_eq!(template.front.rdf_inline.len(), 1);
        assert_eq!(
            template.front.rdf_inline[0],
            "@prefix ex: <http://example.org/> . ex:Alice a ex:Person ."
        );

        Ok(())
    }

    #[test]
    fn test_render_frontmatter_with_sparql() -> Result<()> {
        let input = r#"---
sparql:
  people: "SELECT ?person WHERE { ?person a ex:Person . ?person ex:name '{{name}}' }"
---
fn main() {
    println!("Hello, world!");
}"#;
        let mut template = Template::parse(input)?;

        let mut tera = Tera::default();
        let mut vars = Context::new();
        vars.insert("name", "Alice");

        template.render_frontmatter(&mut tera, &vars)?;

        assert_eq!(template.front.sparql.len(), 1);
        assert_eq!(
            template.front.sparql.get("people"),
            Some(
                &"SELECT ?person WHERE { ?person a ex:Person . ?person ex:name 'Alice' }"
                    .to_string()
            )
        );

        Ok(())
    }

    #[test]
    fn test_render_template_body() -> Result<()> {
        let input = r#"---
to: "{{name}}.rs"
---
fn main() {
    println!("Hello, {{name}}!");
}"#;
        let template = Template::parse(input)?;

        let mut tera = Tera::default();
        let mut vars = Context::new();
        vars.insert("name", "Alice");

        let rendered = template.render(&mut tera, &vars)?;

        assert_eq!(rendered, "fn main() {\n    println!(\"Hello, Alice!\");\n}");

        Ok(())
    }

    #[test]
    fn test_render_template_with_from() -> Result<()> {
        use tempfile::NamedTempFile;

        // Create a temporary file with template content
        let mut temp_file = NamedTempFile::new()?;
        writeln!(temp_file, "fn main() {{")?;
        writeln!(temp_file, "    println!(\"Hello, {{{{name}}}}!\");")?;
        writeln!(temp_file, "}}")?;
        temp_file.flush()?;

        let input = format!(
            r#"---
from: "{}"
---
This should be ignored"#,
            temp_file.path().to_str().unwrap()
        );

        let mut template = Template::parse(&input)?;

        let mut tera = Tera::default();
        let mut vars = Context::new();
        vars.insert("name", "Alice");

        // Render frontmatter first to populate the 'from' field
        template.render_frontmatter(&mut tera, &vars)?;

        let rendered = template.render(&mut tera, &vars)?;

        assert!(rendered.contains("fn main() {"));
        assert!(rendered.contains("println!(\"Hello, Alice!\");"));
        assert!(!rendered.contains("This should be ignored"));

        Ok(())
    }

    #[test]
    fn test_process_graph_with_rdf_inline() -> Result<()> {
        let input = r#"---
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:{{name}} a ex:Person ."
---
fn main() {
    println!("Hello, world!");
}"#;
        let mut template = Template::parse(input)?;
        let mut graph = Graph::new()?;

        let mut tera = Tera::default();
        let mut vars = Context::new();
        vars.insert("name", "Alice");

        template.process_graph(
            &mut graph,
            &mut tera,
            &vars,
            std::path::Path::new("test.tmpl"),
        )?;

        // Check that the RDF was inserted
        assert!(!graph.is_empty());

        // Query for the inserted data
        let results = graph.query("SELECT ?s WHERE { ?s a <http://example.org/Person> }")?;
        if let oxigraph::sparql::QueryResults::Solutions(mut it) = results {
            let first = it.next().unwrap().unwrap();
            let s = first.get("s").unwrap().to_string();
            assert_eq!(s, "<http://example.org/Alice>");
        } else {
            return Err(anyhow::anyhow!("Expected Solutions results"));
        }

        Ok(())
    }

    #[test]
    fn test_process_graph_with_sparql() -> Result<()> {
        let input = r#"---
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:alice a ex:Person . ex:bob a ex:Person ."
sparql:
  count_people: "SELECT (COUNT(?person) AS ?count) WHERE { ?person a ex:Person }"
---
fn main() {
    println!("Hello, world!");
}"#;
        let mut template = Template::parse(input)?;
        let mut graph = Graph::new()?;

        let mut tera = Tera::default();
        let vars = Context::new();

        template.process_graph(
            &mut graph,
            &mut tera,
            &vars,
            std::path::Path::new("test.tmpl"),
        )?;

        // Check that the RDF was inserted
        assert!(!graph.is_empty());

        // The SPARQL query should have been executed (though we can't easily test the result)
        // But we can verify the graph has the expected data
        let results = graph.query("SELECT ?s WHERE { ?s a <http://example.org/Person> }")?;
        if let oxigraph::sparql::QueryResults::Solutions(it) = results {
            let count = it.count();
            assert_eq!(count, 2); // alice and bob
        } else {
            return Err(anyhow::anyhow!("Expected Solutions results"));
        }

        Ok(())
    }

    #[test]
    fn test_string_or_seq_deserializer() -> Result<()> {
        // Test string input
        let yaml_str = r#"rdf_inline: "single string""#;
        let frontmatter: Frontmatter = serde_yaml::from_str(yaml_str)?;
        assert_eq!(frontmatter.rdf_inline, vec!["single string"]);

        // Test array input
        let yaml_array = r#"rdf_inline: ["string1", "string2"]"#;
        let frontmatter: Frontmatter = serde_yaml::from_str(yaml_array)?;
        assert_eq!(frontmatter.rdf_inline, vec!["string1", "string2"]);

        Ok(())
    }

    #[test]
    fn test_sparql_map_deserializer() -> Result<()> {
        // Test single string input
        let yaml_str = r#"sparql: "SELECT ?s WHERE { ?s ?p ?o }""#;
        let frontmatter: Frontmatter = serde_yaml::from_str(yaml_str)?;
        assert_eq!(
            frontmatter.sparql.get("default"),
            Some(&"SELECT ?s WHERE { ?s ?p ?o }".to_string())
        );

        // Test map input
        let yaml_map = r#"sparql:
  query1: "SELECT ?s WHERE { ?s ?p ?o }"
  query2: "SELECT ?o WHERE { ?s ?p ?o }""#;
        let frontmatter: Frontmatter = serde_yaml::from_str(yaml_map)?;
        assert_eq!(
            frontmatter.sparql.get("query1"),
            Some(&"SELECT ?s WHERE { ?s ?p ?o }".to_string())
        );
        assert_eq!(
            frontmatter.sparql.get("query2"),
            Some(&"SELECT ?o WHERE { ?s ?p ?o }".to_string())
        );

        // Test array input
        let yaml_array =
            r#"sparql: ["SELECT ?s WHERE { ?s ?p ?o }", "SELECT ?o WHERE { ?s ?p ?o }"]"#;
        let frontmatter: Frontmatter = serde_yaml::from_str(yaml_array)?;
        assert_eq!(
            frontmatter.sparql.get("query_0"),
            Some(&"SELECT ?s WHERE { ?s ?p ?o }".to_string())
        );
        assert_eq!(
            frontmatter.sparql.get("query_1"),
            Some(&"SELECT ?o WHERE { ?s ?p ?o }".to_string())
        );

        Ok(())
    }

    #[test]
    fn test_frontmatter_defaults() -> Result<()> {
        let yaml_str = r#"to: "test.rs""#;
        let frontmatter: Frontmatter = serde_yaml::from_str(yaml_str)?;

        // Test default values
        assert_eq!(frontmatter.force, false);
        assert_eq!(frontmatter.unless_exists, false);
        assert_eq!(frontmatter.inject, false);
        assert_eq!(frontmatter.prepend, false);
        assert_eq!(frontmatter.append, false);
        assert_eq!(frontmatter.eof_last, false);
        assert_eq!(frontmatter.idempotent, false);
        assert!(frontmatter.prefixes.is_empty());
        assert!(frontmatter.rdf_inline.is_empty());
        assert!(frontmatter.rdf.is_empty());
        assert!(frontmatter.sparql.is_empty());
        assert!(frontmatter.vars.is_empty());
        assert!(frontmatter.shape.is_empty());

        Ok(())
    }

    #[test]
    fn test_flexible_vars_map() -> Result<()> {
        let yaml_str = r#"vars:
  name: "Alice"
  age: 30"#;
        let frontmatter: Frontmatter = serde_yaml::from_str(yaml_str)?;

        assert_eq!(frontmatter.vars.len(), 2);
        assert_eq!(frontmatter.vars.get("name").and_then(|v| v.as_str()), Some("Alice"));
        assert_eq!(frontmatter.vars.get("age").and_then(|v| v.as_i64()), Some(30));

        Ok(())
    }

    #[test]
    fn test_flexible_vars_array() -> Result<()> {
        let yaml_str = r#"vars:
  - "item1"
  - "item2"
  - "item3""#;
        let frontmatter: Frontmatter = serde_yaml::from_str(yaml_str)?;

        assert_eq!(frontmatter.vars.len(), 3);
        assert_eq!(frontmatter.vars.get("var0").and_then(|v| v.as_str()), Some("item1"));
        assert_eq!(frontmatter.vars.get("var1").and_then(|v| v.as_str()), Some("item2"));
        assert_eq!(frontmatter.vars.get("var2").and_then(|v| v.as_str()), Some("item3"));

        Ok(())
    }

    #[test]
    fn test_flexible_vars_string() -> Result<()> {
        let yaml_str = r#"vars: "single_value""#;
        let frontmatter: Frontmatter = serde_yaml::from_str(yaml_str)?;

        assert_eq!(frontmatter.vars.len(), 1);
        assert_eq!(frontmatter.vars.get("var0").and_then(|v| v.as_str()), Some("single_value"));

        Ok(())
    }

    #[test]
    fn test_flexible_vars_null() -> Result<()> {
        let yaml_str = r#"vars: null"#;
        let frontmatter: Frontmatter = serde_yaml::from_str(yaml_str)?;

        assert!(frontmatter.vars.is_empty());

        Ok(())
    }

    #[test]
    fn test_frontmatter_boolean_fields() -> Result<()> {
        let yaml_str = r#"force: true
unless_exists: true
inject: true
prepend: true
append: true
eof_last: true
idempotent: true"#;
        let frontmatter: Frontmatter = serde_yaml::from_str(yaml_str)?;

        assert_eq!(frontmatter.force, true);
        assert_eq!(frontmatter.unless_exists, true);
        assert_eq!(frontmatter.inject, true);
        assert_eq!(frontmatter.prepend, true);
        assert_eq!(frontmatter.append, true);
        assert_eq!(frontmatter.eof_last, true);
        assert_eq!(frontmatter.idempotent, true);

        Ok(())
    }

    #[test]
    fn test_frontmatter_injection_fields() -> Result<()> {
        let yaml_str = r#"inject: true
before: "// Before comment"
after: "// After comment"
at_line: 5
skip_if: "existing code""#;
        let frontmatter: Frontmatter = serde_yaml::from_str(yaml_str)?;

        assert_eq!(frontmatter.inject, true);
        assert_eq!(frontmatter.before, Some("// Before comment".to_string()));
        assert_eq!(frontmatter.after, Some("// After comment".to_string()));
        assert_eq!(frontmatter.at_line, Some(5));
        assert_eq!(frontmatter.skip_if, Some("existing code".to_string()));

        Ok(())
    }

    #[test]
    fn test_frontmatter_shell_hooks() -> Result<()> {
        let yaml_str = r#"sh_before: "echo Before generation"
sh_after: "echo After generation""#;
        let frontmatter: Frontmatter = serde_yaml::from_str(yaml_str)?;

        assert_eq!(
            frontmatter.sh_before,
            Some("echo Before generation".to_string())
        );
        assert_eq!(
            frontmatter.sh_after,
            Some("echo After generation".to_string())
        );

        Ok(())
    }

    #[test]
    fn test_frontmatter_sh_alias() -> Result<()> {
        let yaml_str = r#"sh: "echo Shell hook""#;
        let frontmatter: Frontmatter = serde_yaml::from_str(yaml_str)?;

        assert_eq!(frontmatter.sh_before, Some("echo Shell hook".to_string()));

        Ok(())
    }
}
