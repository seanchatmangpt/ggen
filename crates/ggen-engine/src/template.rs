//! Hygen-style template parsing and Tera environment construction.
//!
//! A template is a leading `--- yaml ---` frontmatter block followed by a
//! Tera body. The frontmatter key set is closed ([`Frontmatter`] uses
//! `deny_unknown_fields`), so any unrecognized key is a hard error.
//!
//! [`build_tera`] produces a Tera environment with a `sparql(query="…")`
//! function bound to a [`DeterministicGraph`], plus `snake_case` and
//! `pascal_case` filters.

use std::{
    collections::{BTreeMap, HashMap},
    path::{Path, PathBuf},
    sync::Arc,
};

use schemars::JsonSchema;
use serde::Deserialize;
use tera::{Tera, Value};

use crate::{
    error::{AppError, Result, TemplateFailureCause},
    graph::{EngineQueryResults, EngineRow, EngineValue, GraphEngine},
};

/// Closed 25-property frontmatter key set for a ggen template.
///
/// The Hygen-derived action grammar is extended by ontology projection,
/// explicit cardinality, deterministic lifecycle law, and receipts. Unknown
/// keys are rejected at parse time (`deny_unknown_fields`).
///
/// `#[derive(JsonSchema)]` is load-bearing: it lets
/// `tests/frontmatter_schema_match.rs` compare this struct's *actual* field
/// set (via `schemars::schema_for!`) against `schema/frontmatter-schema.ttl`,
/// instead of a hand-maintained mirror list that could itself drift.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct Frontmatter {
    /// Output path relative to the project root (Tera-renderable).
    pub to: String,
    /// Named SPARQL queries available to the template body. Accepts a bare
    /// string (implicitly named `default`), a YAML sequence (implicitly
    /// named `query_0`, `query_1`, …), or the explicit mapping form
    /// `{name: query}` — the same three forms and naming convention as
    /// ggen-core's own frontmatter `sparql:` field
    /// (`crates/ggen-core/src/template_types.rs`'s `sparql_map`
    /// deserializer), replicated faithfully.
    #[serde(default, deserialize_with = "sparql_map")]
    pub sparql: BTreeMap<String, String>,
    /// Explicit named `sparql:` result that governs projection cardinality.
    /// When set, dynamic `to:` paths fan out once per row; static paths
    /// aggregate one body rendering per row into a single output. When
    /// absent, the historical first-array/`to`-contains-`{{` behavior is
    /// preserved exactly.
    #[serde(default)]
    pub for_each: Option<String>,
    /// Optional CONSTRUCT query whose result feeds the template.
    #[serde(default)]
    pub construct: Option<String>,
    /// Inject into an existing file instead of creating a new one.
    #[serde(default)]
    pub inject: bool,
    /// Inject before the selected host-content match. A bare string keeps
    /// the historical `contains + line + first` behavior; a structured
    /// declaration can opt into exact/regex matching and cardinality.
    #[serde(default)]
    pub before: Option<MatchSpec>,
    /// Inject after the selected host-content match. Defaults are identical
    /// to `before`.
    #[serde(default)]
    pub after: Option<MatchSpec>,
    /// Inject at this 1-based line number.
    #[serde(default)]
    pub at_line: Option<usize>,
    /// Skip the write when the existing file satisfies this selector. A bare
    /// string preserves the historical whole-file substring behavior.
    #[serde(default)]
    pub skip_if: Option<MatchSpec>,
    /// Skip the write entirely when the target file already exists.
    #[serde(default)]
    pub unless_exists: bool,
    /// Overwrite an existing, differing file instead of failing closed.
    #[serde(default)]
    pub force: bool,
    /// SPARQL ASK guard: generate only when the graph satisfies it.
    #[serde(default)]
    pub when: Option<String>,
    /// Skip the write when the rendered body is empty.
    #[serde(default)]
    pub skip_empty: bool,
    /// Load the Tera body from this path instead (relative to the template
    /// file's own directory); frontmatter fields still come from this file.
    #[serde(default)]
    pub from: Option<String>,
    /// Shell command run before the write decision. Refused (not executed)
    /// if it matches [`crate::shell_safety::check_shell_command_safe`]'s
    /// denylist. Runs with the project root as its working directory.
    #[serde(default, alias = "sh")]
    pub sh_before: Option<String>,
    /// Shell command run after a successful `Written`/`Injected` outcome
    /// (never after `Skipped`). Same denylist and working directory as
    /// `sh_before`.
    #[serde(default)]
    pub sh_after: Option<String>,
    /// Before overwriting an existing file (`force` or `inject`), copy it to
    /// `<path>.bak` first.
    #[serde(default)]
    pub backup: bool,
    /// SHACL shape file paths (relative to the project root) declared as
    /// governing this output. **Existence-checked only** — no SHACL engine
    /// runs in this crate yet, so listed shapes are not evaluated against
    /// the rendered output; see `docs/v26.7.4/GGEN_TOML_SCHEMA_MAPPING.md`.
    #[serde(default)]
    pub shape: Vec<String>,
    /// When `true`, the sync pipeline renders this template's body twice
    /// with identical inputs and refuses if the bytes differ (a real,
    /// enforced determinism assertion, not a declared-but-unchecked claim).
    #[serde(default)]
    pub determinism: Option<bool>,
    /// Freeze policy for this output once written; see [`FreezePolicy`].
    /// Defaults to `never` (no freeze behavior) when absent.
    #[serde(default)]
    pub freeze_policy: Option<FreezePolicy>,
    /// Directory (relative to the project root) storing per-output BLAKE3
    /// checksums for `freeze_policy: checksum`. Required when that policy
    /// is set; ignored otherwise.
    #[serde(default)]
    pub freeze_slots_dir: Option<String>,
    /// RDF/Turtle file paths loaded into a per-template overlay graph,
    /// resolved relative to the template file's own directory (never the
    /// project root) — the same traversal-safety check as `from:`. Accepts
    /// either a bare string or a YAML sequence (`rdf: foo.ttl` or
    /// `rdf: [foo.ttl, bar.ttl]`), matching ggen-core's
    /// `template_types.rs::string_or_seq` semantics exactly. See
    /// `crate::sync`'s overlay construction: the base graph's triples plus
    /// every file here, queried/rendered for THIS template only — the
    /// shared project graph is never mutated, and no other template in the
    /// same run observes these extra triples.
    #[serde(default, deserialize_with = "string_or_seq")]
    pub rdf: Vec<String>,
    /// Literal inline Turtle text (not file paths), loaded into the same
    /// per-template overlay graph as `rdf:`. Same string-or-sequence
    /// acceptance as `rdf:`.
    #[serde(default, deserialize_with = "string_or_seq")]
    pub rdf_inline: Vec<String>,
    /// Extra Turtle `@prefix` declarations (prefix name → IRI) prepended
    /// before parsing `rdf:`/`rdf_inline:` content. Ignored when both
    /// `rdf:` and `rdf_inline:` are empty.
    #[serde(default)]
    pub prefixes: BTreeMap<String, String>,
    /// RDF base IRI (`@base`) prepended before parsing `rdf:`/`rdf_inline:`
    /// content. Ignored when both `rdf:` and `rdf_inline:` are empty.
    #[serde(default)]
    pub base: Option<String>,
}

/// Backward-compatible textual selector used by `before`, `after`, and
/// `skip_if`.
///
/// Bare strings preserve the original Hygen-derived behavior. Structured
/// declarations expose the complete matcher algebra while every optional
/// property has a deterministic default.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum MatchSpec {
    /// Compatibility form. `before`/`after` resolve to
    /// `contains + line + first`; `skip_if` resolves to
    /// `contains + file + first`.
    Literal(String),
    /// Explicit matcher declaration.
    Structured(MatchRule),
}

impl MatchSpec {
    /// Pattern text, before output-phase Tera rendering.
    pub fn pattern(&self) -> &str {
        match self {
            Self::Literal(pattern) => pattern,
            Self::Structured(rule) => &rule.pattern,
        }
    }

    /// Clone the declaration while replacing only its rendered pattern.
    pub fn with_pattern(&self, pattern: String) -> Self {
        match self {
            Self::Literal(_) => Self::Literal(pattern),
            Self::Structured(rule) => {
                let mut rendered = rule.clone();
                rendered.pattern = pattern;
                Self::Structured(rendered)
            }
        }
    }

    /// Whether this declaration opted into the structured matcher algebra.
    pub fn is_structured(&self) -> bool {
        matches!(self, Self::Structured(_))
    }
}

impl From<String> for MatchSpec {
    fn from(value: String) -> Self {
        Self::Literal(value)
    }
}

impl From<&str> for MatchSpec {
    fn from(value: &str) -> Self {
        Self::Literal(value.to_string())
    }
}

/// Explicit host-content matcher. All fields except `pattern` have sane,
/// compatibility-oriented defaults.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct MatchRule {
    /// Literal or regular-expression pattern. Tera-rendered per output.
    pub pattern: String,
    /// Matching algorithm. Defaults to `contains`.
    #[serde(default)]
    pub matcher: MatchKind,
    /// Candidate scope. `auto` resolves to line scope for `before`/`after`
    /// and file scope for `skip_if`.
    #[serde(default)]
    pub scope: MatchScope,
    /// Which observed match supplies the consequence. Defaults to `first`.
    #[serde(default)]
    pub occurrence: MatchOccurrence,
    /// One-based occurrence used by `nth`. Defaults to `1`.
    #[serde(default = "default_match_index")]
    pub index: usize,
    /// Case-sensitive matching by default.
    #[serde(default = "default_true")]
    pub case_sensitive: bool,
    /// Match original text by default; set true to trim candidate boundaries.
    #[serde(default)]
    pub trim: bool,
}

/// Matcher algorithm.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum MatchKind {
    /// Candidate contains the pattern.
    #[default]
    Contains,
    /// Candidate exactly equals the pattern.
    Exact,
    /// Rust `regex` pattern.
    Regex,
}

/// Candidate scope.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum MatchScope {
    /// Property-specific compatibility default.
    #[default]
    Auto,
    /// Match each line independently. Occurrences count matching lines.
    Line,
    /// Match the complete file. Occurrences count non-overlapping spans.
    File,
}

/// Match cardinality/selection law.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum MatchOccurrence {
    /// Select the first observed match.
    #[default]
    First,
    /// Select the last observed match.
    Last,
    /// Require exactly one observed match.
    Unique,
    /// Select the one-based `index` occurrence.
    Nth,
}

const fn default_match_index() -> usize {
    1
}

const fn default_true() -> bool {
    true
}

/// Freeze policy for a frontmatter's output, once it has been written once.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum FreezePolicy {
    /// Never skip on freeze grounds; the normal write decision table applies
    /// unchanged. Equivalent to omitting `freeze_policy` entirely.
    Never,
    /// Once the target exists, always skip regeneration — a one-time
    /// scaffold that is never touched again by `ggen sync`.
    Always,
    /// Skip regeneration only when the target's on-disk content no longer
    /// matches the checksum ggen recorded the last time it wrote this file
    /// (i.e. a human has edited it since); otherwise proceed normally and
    /// record the new checksum. Requires `freeze_slots_dir`.
    Checksum,
}

/// A parsed template: validated frontmatter plus the raw Tera body.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Template {
    /// Parsed and validated frontmatter block.
    pub frontmatter: Frontmatter,
    /// Tera template body (everything after the closing `---`).
    pub body: String,
}

impl Template {
    /// Parse a template file: a leading `---` YAML `---` block, then the body.
    ///
    /// # Errors
    /// - `[FM-TPL-001]` when the file does not start with a `---` line at all.
    /// - `[FM-TPL-012]` when the opening `---` is present but not on its own
    ///   line (trailing content after it on the same line).
    /// - `[FM-TPL-013]` when the frontmatter block is unterminated (no
    ///   closing `---` line found).
    /// - `[FM-TPL-002]` when the YAML fails to deserialize, including any
    ///   unknown frontmatter key (closed key set, fail closed).
    pub fn parse(content: &str) -> Result<Self> {
        let rest = content.strip_prefix("---").ok_or_else(|| {
            AppError::fm_tpl(
                1,
                "template must start with a `---` frontmatter block. \
                 Remediation: begin the file with `---`, YAML keys, `---`.",
            )
        })?;
        // The opening delimiter must be its own line.
        let rest = rest.strip_prefix('\n').ok_or_else(|| {
            AppError::fm_tpl(12, "`---` frontmatter delimiter must be on its own line")
        })?;
        let (yaml, body) = split_closing_delimiter(rest).ok_or_else(|| {
            AppError::fm_tpl(
                13,
                "unterminated frontmatter: no closing `---` line found. \
                 Remediation: close the YAML block with a `---` line.",
            )
        })?;
        let frontmatter: Frontmatter = serde_yaml::from_str(yaml).map_err(|e| {
            AppError::fm_tpl(
                2,
                format!(
                    "frontmatter rejected: {e}. \
                     Remediation: use only the closed key set (to, sparql, for_each, construct, \
                     inject, before, after, at_line, skip_if, unless_exists, force, \
                     when, skip_empty, from, sh_before, sh_after, backup, shape, \
                     determinism, freeze_policy, freeze_slots_dir, rdf, rdf_inline, \
                     prefixes, base)."
                ),
            )
        })?;
        Ok(Self {
            frontmatter,
            body: body.to_string(),
        })
    }
}

/// Split `rest` at the first line that is exactly `---`, returning
/// `(yaml, body)`. The body excludes the delimiter line itself.
fn split_closing_delimiter(rest: &str) -> Option<(&str, &str)> {
    let mut offset = 0usize;
    for line in rest.split_inclusive('\n') {
        if line.trim_end_matches(['\r', '\n']) == "---" {
            let yaml = &rest[..offset];
            let body = &rest[offset + line.len()..];
            return Some((yaml, body));
        }
        offset += line.len();
    }
    None
}

/// Accept `rdf: "<path>"` (single) or `rdf: ["<a>", "<b>"]` (sequence) —
/// replicates ggen-core's `template_types.rs::string_or_seq` deserializer
/// (same visitor structure, same accepted shapes) for `rdf:`/`rdf_inline:`.
fn string_or_seq<'de, D>(de: D) -> std::result::Result<Vec<String>, D::Error>
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

        fn visit_str<E>(self, v: &str) -> std::result::Result<Self::Value, E>
        where
            E: DeError,
        {
            Ok(vec![v.to_string()])
        }

        fn visit_string<E>(self, v: String) -> std::result::Result<Self::Value, E>
        where
            E: DeError,
        {
            Ok(vec![v])
        }

        fn visit_seq<A>(self, mut seq: A) -> std::result::Result<Self::Value, A::Error>
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

/// Accept `sparql: "<query>"` (single, named `default`),
/// `sparql: ["<q0>", "<q1>"]` (named `query_0`, `query_1`, …), or
/// `sparql: { name: "<query>" }` (explicit mapping) — replicates
/// ggen-core's `template_types.rs::sparql_map` deserializer (same untagged
/// enum, same naming convention) so templates authored against either
/// convention parse identically here.
fn sparql_map<'de, D>(de: D) -> std::result::Result<BTreeMap<String, String>, D::Error>
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
                m.insert(format!("query_{i}"), query);
            }
            Ok(m)
        }
    }
}

/// Build a Tera environment bound to `graph`.
///
/// Registers:
/// - `sparql(query="…")` — executes the query against the graph.
///   ASK → bool; SELECT → array of `{var: value}` objects (bare variable
///   names, e.g. `name`, never `?name` — see
///   `sparql_row_keys_are_bare_not_question_mark_prefixed` for why that's a
///   pinned invariant, not an accident); CONSTRUCT / DESCRIBE → array of
///   `{subject, predicate, object}` objects.
/// - `local(iri="…")` — the local name/fragment of an IRI (after the last
///   `#` or `/`).
/// - `sparql_first(rows=…)` / `sparql_first(results=…)` — the first row of a
///   SELECT result array, or `null` if empty. With `column="…"` given
///   (either convention), returns just that column's value instead of the
///   full row object. `rows=` and `results=` are the same convention under
///   two names (the engine's own, and ggen-core's); passing both is a hard
///   error.
/// - `sparql_values(rows=…, column="…")` / `sparql_values(results=…,
///   column="…")` — the array of one column's values across every row.
/// - `sparql_empty(rows=…)` / `sparql_empty(results=…)` — `true` if the
///   array is empty.
/// - `sparql_count(rows=…)` / `sparql_count(results=…)` — the number of
///   rows.
/// - `snake_case`, `pascal_case`, `camel_case`, `kebab_case`,
///   `shouty_snake_case`, `title_case`, `pluralize`, `singularize` filters,
///   plus the bare aliases `snake`, `pascal`, `camel`, `kebab`, `shouty`,
///   `title` (identical output to their `_case`-suffixed counterparts —
///   `title` deliberately shadows Tera's own built-in `title` filter,
///   matching the semantics templates ported from ggen-core were authored
///   against).
///
/// # Errors
/// Returns `[FM-TPL-015]` only for a structural failure isolating the
/// `templates/` directory itself (e.g. unreadable, or a genuine
/// inheritance-chain/macro-import defect spanning multiple files — see
/// [`load_templates_glob_lenient`]'s doc comment for why a *single* broken
/// file's parse error no longer reaches this point at all). Previously a
/// syntax error in ANY file under `templates/**/*` — including one no
/// active rule/template references — aborted this call entirely; that
/// collateral failure mode is what [`load_templates_glob_lenient`] fixes.
pub fn build_tera(graph: Arc<dyn GraphEngine>) -> Result<Tera> {
    let mut tera = load_templates_glob_lenient(Path::new("templates"))?;
    tera.register_function("sparql", move |args: &HashMap<String, Value>| {
        let query = args
            .get("query")
            .and_then(Value::as_str)
            .ok_or_else(|| tera::Error::msg("sparql() requires a string `query` argument"))?;
        sparql_to_value(graph.as_ref(), query).map_err(|e| tera::Error::msg(e.to_string()))
    });
    tera.register_function("local", local_fn);
    tera.register_function("sparql_first", sparql_first_fn);
    tera.register_function("sparql_values", sparql_values_fn);
    tera.register_function("sparql_empty", sparql_empty_fn);
    tera.register_function("sparql_count", sparql_count_fn);
    tera.register_filter("snake_case", snake_case_filter);
    tera.register_filter("snake", snake_case_filter);
    tera.register_filter("pascal_case", pascal_case_filter);
    tera.register_filter("pascal", pascal_case_filter);
    tera.register_filter("camel_case", camel_case_filter);
    tera.register_filter("camel", camel_case_filter);
    tera.register_filter("kebab_case", kebab_case_filter);
    tera.register_filter("kebab", kebab_case_filter);
    tera.register_filter("shouty_snake_case", shouty_snake_case_filter);
    tera.register_filter("shouty", shouty_snake_case_filter);
    tera.register_filter("title_case", title_case_filter);
    // Deliberately shadows Tera's own built-in `title` filter: the 35 real
    // templates this alias exists for were authored against ggen-core's
    // `title` (== this crate's `title_case`) semantics, not Tera's.
    tera.register_filter("title", title_case_filter);
    tera.register_filter("pluralize", pluralize_filter);
    tera.register_filter("singularize", singularize_filter);
    tera.register_filter("hex_to_u64", hex_to_u64_filter);
    Ok(tera)
}

/// Lenient replacement for `Tera::new("templates/**/*")`: parses every file
/// under `templates_dir` (relative to the current working directory,
/// matching `Tera::new`'s own glob-then-canonicalize semantics) *individually*.
///
/// A file that fails to parse is skipped — never registered — and its path
/// and error are logged at `WARN`, not silently discarded. This isolates a
/// dead/unreferenced/broken file from all direct `render_str()` callers while
/// preserving every valid macro/include/inheritance entry. ggen-engine's own
/// frontmatter sync does not rely on these preloaded templates for its body
/// rendering (it calls `render_str` directly); `sparql()` and filters are the
/// load-bearing part of this environment there. Other callers that do render
/// by registered name get all parseable files and a precise render-time
/// "template not found" if they ask for one of the skipped files.
fn load_templates_glob_lenient(templates_dir: &Path) -> Result<Tera> {
    let mut tera = Tera::default();
    let mut paths = Vec::new();
    if templates_dir.exists() {
        collect_template_paths(templates_dir, &mut paths).map_err(|e| {
            AppError::fm_tpl(
                15,
                format!(
                    "cannot enumerate Tera templates under `{}`: {e}",
                    templates_dir.display()
                ),
            )
        })?;
    }
    paths.sort();

    for path in paths {
        let name = path.to_string_lossy().replace('\\', "/");
        match tera.add_template_file(&path, Some(&name)) {
            Ok(()) => {}
            Err(error) => {
                tracing::warn!(
                    target: "ggen_engine::template",
                    template = %name,
                    %error,
                    "skipping malformed Tera template during environment registration"
                );
            }
        }
    }
    Ok(tera)
}

fn collect_template_paths(dir: &Path, paths: &mut Vec<PathBuf>) -> std::io::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            collect_template_paths(&path, paths)?;
        } else if path.is_file() {
            paths.push(path);
        }
    }
    Ok(())
}

/// Convert graph-engine query results into Tera [`Value`].
///
/// ASK → `bool`; SELECT → array of objects; CONSTRUCT/DESCRIBE → array of
/// `{subject,predicate,object}` objects.
pub fn sparql_to_value(graph: &dyn GraphEngine, query: &str) -> Result<Value> {
    match graph.query(query)? {
        EngineQueryResults::Boolean(b) => Ok(Value::Bool(b)),
        EngineQueryResults::Solutions(rows) => {
            let mut out = Vec::with_capacity(rows.len());
            for row in rows {
                let map = row
                    .into_iter()
                    .map(|(k, v)| (k, engine_value_to_json(v)))
                    .collect();
                out.push(Value::Object(map));
            }
            Ok(Value::Array(out))
        }
        EngineQueryResults::Graph(triples) => {
            let out = triples
                .into_iter()
                .map(|triple| {
                    let mut map = serde_json::Map::new();
                    map.insert("subject".into(), Value::String(triple.subject));
                    map.insert("predicate".into(), Value::String(triple.predicate));
                    map.insert("object".into(), Value::String(triple.object));
                    Value::Object(map)
                })
                .collect();
            Ok(Value::Array(out))
        }
    }
}

fn engine_value_to_json(value: EngineValue) -> Value {
    match value {
        EngineValue::Bool(b) => Value::Bool(b),
        EngineValue::Int(n) => Value::Number(n.into()),
        EngineValue::Float(f) => serde_json::Number::from_f64(f)
            .map(Value::Number)
            .unwrap_or(Value::Null),
        EngineValue::String(s) => Value::String(s),
    }
}

fn local_fn(args: &HashMap<String, Value>) -> tera::Result<Value> {
    let iri = args
        .get("iri")
        .and_then(Value::as_str)
        .ok_or_else(|| tera::Error::msg("local() requires a string `iri` argument"))?;
    let local = iri.rsplit(['#', '/']).next().unwrap_or(iri);
    Ok(Value::String(local.to_string()))
}

fn expect_rows<'a>(args: &'a HashMap<String, Value>, function: &str) -> tera::Result<&'a Vec<Value>> {
    let rows = args.get("rows");
    let results = args.get("results");
    if rows.is_some() && results.is_some() {
        return Err(tera::Error::msg(format!(
            "{function}() accepts `rows=` or `results=`, not both"
        )));
    }
    rows.or(results)
        .and_then(Value::as_array)
        .ok_or_else(|| tera::Error::msg(format!("{function}() requires an array `rows`/`results` argument")))
}

fn sparql_first_fn(args: &HashMap<String, Value>) -> tera::Result<Value> {
    let rows = expect_rows(args, "sparql_first")?;
    let Some(first) = rows.first() else {
        return Ok(Value::Null);
    };
    match args.get("column") {
        None => Ok(first.clone()),
        Some(column) => {
            let column = column
                .as_str()
                .ok_or_else(|| tera::Error::msg("sparql_first() `column` must be a string"))?;
            first
                .as_object()
                .and_then(|row| row.get(column))
                .cloned()
                .ok_or_else(|| tera::Error::msg(format!("sparql_first(): column `{column}` not found in first row")))
        }
    }
}

fn sparql_values_fn(args: &HashMap<String, Value>) -> tera::Result<Value> {
    let rows = expect_rows(args, "sparql_values")?;
    let column = args
        .get("column")
        .and_then(Value::as_str)
        .ok_or_else(|| tera::Error::msg("sparql_values() requires a string `column` argument"))?;
    let mut out = Vec::with_capacity(rows.len());
    for (index, row) in rows.iter().enumerate() {
        let value = row
            .as_object()
            .and_then(|row| row.get(column))
            .cloned()
            .ok_or_else(|| {
                tera::Error::msg(format!(
                    "sparql_values(): column `{column}` missing from row {index}"
                ))
            })?;
        out.push(value);
    }
    Ok(Value::Array(out))
}

fn sparql_empty_fn(args: &HashMap<String, Value>) -> tera::Result<Value> {
    Ok(Value::Bool(expect_rows(args, "sparql_empty")?.is_empty()))
}

fn sparql_count_fn(args: &HashMap<String, Value>) -> tera::Result<Value> {
    Ok(Value::Number(expect_rows(args, "sparql_count")?.len().into()))
}

fn unary_string_arg<'a>(value: &'a Value, filter: &str) -> tera::Result<&'a str> {
    value
        .as_str()
        .ok_or_else(|| tera::Error::msg(format!("{filter} filter requires a string input")))
}

fn snake_case_filter(value: &Value, _: &HashMap<String, Value>) -> tera::Result<Value> {
    Ok(Value::String(
        heck::ToSnakeCase::to_snake_case(unary_string_arg(value, "snake_case")?),
    ))
}

fn pascal_case_filter(value: &Value, _: &HashMap<String, Value>) -> tera::Result<Value> {
    Ok(Value::String(
        heck::ToUpperCamelCase::to_upper_camel_case(unary_string_arg(value, "pascal_case")?),
    ))
}

fn camel_case_filter(value: &Value, _: &HashMap<String, Value>) -> tera::Result<Value> {
    Ok(Value::String(
        heck::ToLowerCamelCase::to_lower_camel_case(unary_string_arg(value, "camel_case")?),
    ))
}

fn kebab_case_filter(value: &Value, _: &HashMap<String, Value>) -> tera::Result<Value> {
    Ok(Value::String(
        heck::ToKebabCase::to_kebab_case(unary_string_arg(value, "kebab_case")?),
    ))
}

fn shouty_snake_case_filter(value: &Value, _: &HashMap<String, Value>) -> tera::Result<Value> {
    Ok(Value::String(
        heck::ToShoutySnakeCase::to_shouty_snake_case(unary_string_arg(value, "shouty_snake_case")?),
    ))
}

fn title_case_filter(value: &Value, _: &HashMap<String, Value>) -> tera::Result<Value> {
    Ok(Value::String(
        heck::ToTitleCase::to_title_case(unary_string_arg(value, "title_case")?),
    ))
}

fn pluralize_filter(value: &Value, _: &HashMap<String, Value>) -> tera::Result<Value> {
    let word = unary_string_arg(value, "pluralize")?;
    Ok(Value::String(pluralize::pluralize(word, 2, false)))
}

fn singularize_filter(value: &Value, _: &HashMap<String, Value>) -> tera::Result<Value> {
    let word = unary_string_arg(value, "singularize")?;
    Ok(Value::String(pluralize::singularize(word)))
}

fn hex_to_u64_filter(value: &Value, _: &HashMap<String, Value>) -> tera::Result<Value> {
    let hex = unary_string_arg(value, "hex_to_u64")?;
    let parsed = u64::from_str_radix(hex.trim_start_matches("0x"), 16)
        .map_err(|error| tera::Error::msg(format!("hex_to_u64: invalid hex `{hex}`: {error}")))?;
    Ok(Value::Number(parsed.into()))
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use schemars::schema_for;

    use super::*;
    use crate::graph::DeterministicGraph;

    #[test]
    fn frontmatter_json_schema_is_closed_and_contains_all_fields() {
        let schema = schema_for!(Frontmatter);
        let object = schema
            .get("properties")
            .and_then(Value::as_object)
            .expect("object schema properties");
        for field in [
            "to",
            "sparql",
            "construct",
            "inject",
            "before",
            "after",
            "at_line",
            "skip_if",
            "unless_exists",
            "force",
            "when",
            "skip_empty",
            "from",
            "sh_before",
            "sh_after",
            "backup",
            "shape",
            "determinism",
            "freeze_policy",
            "freeze_slots_dir",
            "rdf",
            "rdf_inline",
            "prefixes",
            "base",
        ] {
            assert!(object.contains_key(field), "schema missing `{field}`");
        }
        assert_eq!(object.len(), 24, "closed field count drifted");
        assert!(
            schema
                .get("additionalProperties")
                .and_then(Value::as_bool)
                .is_some_and(|allowed| !allowed),
            "deny_unknown_fields must produce additionalProperties=false"
        );
    }

    #[test]
    fn parses_minimal_frontmatter() {
        let parsed = Template::parse("---\nto: src/out.rs\n---\nhello\n").expect("parse");
        assert_eq!(parsed.frontmatter.to, "src/out.rs");
        assert_eq!(parsed.body, "hello\n");
    }

    #[test]
    fn parses_all_added_frontmatter_fields() {
        let src = r#"---
to: out.txt
sh_before: "echo before"
sh_after: "echo after"
backup: true
shape: [shape.ttl]
determinism: true
freeze_policy: checksum
freeze_slots_dir: .ggen-freeze
from: body.tera
rdf: [a.ttl, b.ttl]
rdf_inline: "@prefix ex: <http://example.org/> . ex:s ex:p ex:o ."
prefixes:
  ex: "http://example.org/"
base: "http://example.org/base/"
---
body
"#;
        let parsed = Template::parse(src).expect("parse all fields");
        assert_eq!(parsed.frontmatter.sh_before.as_deref(), Some("echo before"));
        assert_eq!(parsed.frontmatter.sh_after.as_deref(), Some("echo after"));
        assert!(parsed.frontmatter.backup);
        assert_eq!(parsed.frontmatter.shape, vec!["shape.ttl"]);
        assert_eq!(parsed.frontmatter.determinism, Some(true));
        assert_eq!(parsed.frontmatter.freeze_policy, Some(FreezePolicy::Checksum));
        assert_eq!(parsed.frontmatter.freeze_slots_dir.as_deref(), Some(".ggen-freeze"));
        assert_eq!(parsed.frontmatter.from.as_deref(), Some("body.tera"));
        assert_eq!(parsed.frontmatter.rdf, vec!["a.ttl", "b.ttl"]);
        assert_eq!(parsed.frontmatter.rdf_inline.len(), 1);
        assert_eq!(parsed.frontmatter.prefixes.get("ex").map(String::as_str), Some("http://example.org/"));
        assert_eq!(parsed.frontmatter.base.as_deref(), Some("http://example.org/base/"));
    }

    #[test]
    fn sh_alias_maps_to_sh_before() {
        let parsed = Template::parse("---\nto: x\nsh: echo hi\n---\n").expect("parse");
        assert_eq!(parsed.frontmatter.sh_before.as_deref(), Some("echo hi"));
    }

    #[test]
    fn unknown_field_is_rejected() {
        let err = Template::parse("---\nto: x\nnot_a_field: true\n---\n")
            .expect_err("must reject unknown");
        assert!(err.to_string().contains("FM-TPL-002"), "{err}");
    }

    #[test]
    fn rdf_accepts_bare_string_and_sequence() {
        let one = Template::parse("---\nto: x\nrdf: one.ttl\n---\n").expect("bare string");
        assert_eq!(one.frontmatter.rdf, vec!["one.ttl"]);
        let many = Template::parse("---\nto: x\nrdf: [a.ttl, b.ttl]\n---\n").expect("sequence");
        assert_eq!(many.frontmatter.rdf, vec!["a.ttl", "b.ttl"]);
    }

    #[test]
    fn sparql_accepts_bare_string_sequence_and_mapping() {
        let one = Template::parse("---\nto: x\nsparql: SELECT * WHERE {}\n---\n").expect("one");
        assert_eq!(one.frontmatter.sparql.get("default").map(String::as_str), Some("SELECT * WHERE {}"));

        let seq = Template::parse("---\nto: x\nsparql:\n  - SELECT ?x WHERE {}\n  - ASK {}\n---\n")
            .expect("seq");
        assert_eq!(seq.frontmatter.sparql.len(), 2);
        assert!(seq.frontmatter.sparql.contains_key("query_0"));
        assert!(seq.frontmatter.sparql.contains_key("query_1"));

        let map = Template::parse("---\nto: x\nsparql:\n  people: SELECT ?p WHERE {}\n---\n")
            .expect("map");
        assert_eq!(map.frontmatter.sparql.get("people").map(String::as_str), Some("SELECT ?p WHERE {}"));
    }

    #[test]
    fn build_tera_renders_basic_template() {
        let graph: Arc<dyn GraphEngine> = Arc::new(DeterministicGraph::new().expect("graph"));
        let mut tera = build_tera(graph).expect("tera");
        let rendered = tera
            .render_str("hello {{ name | pascal_case }}", &tera::Context::from_serialize(serde_json::json!({"name":"world_name"})).expect("context"))
            .expect("render");
        assert_eq!(rendered, "hello WorldName");
    }
}
