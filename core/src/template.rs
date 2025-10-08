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
    #[serde(default)]
    pub vars: BTreeMap<String, String>,

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
    pub fn process_graph(&mut self, graph: &mut Graph, tera: &mut Tera, vars: &Context) -> Result<()> {
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
        
        // Load RDF files
        for rdf_file in &self.front.rdf {
            let rendered_path = tera.render_str(rdf_file, vars)?;
            if let Ok(ttl_content) = std::fs::read_to_string(&rendered_path) {
                let final_ttl = if prolog.is_empty() {
                    ttl_content
                } else {
                    format!("{prolog}\n{ttl_content}")
                };
                graph.insert_turtle(&final_ttl)?;
            }
        }

        // Execute SPARQL (prepend PREFIX/BASE prolog)
        for (_name, q) in &self.front.sparql {
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
            std::fs::read_to_string(&rendered_from)
                .map_err(|e| anyhow::anyhow!("Failed to read from file '{}': {}", rendered_from, e))?
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
