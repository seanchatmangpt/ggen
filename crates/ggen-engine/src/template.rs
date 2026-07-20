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

/// Closed frontmatter key set for a ggen template (Hygen semantics).
///
/// Unknown keys are rejected at parse time (`deny_unknown_fields`).
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
    /// Optional CONSTRUCT query whose result feeds the template.
    #[serde(default)]
    pub construct: Option<String>,
    /// Inject into an existing file instead of creating a new one.
    #[serde(default)]
    pub inject: bool,
    /// Inject before the first line containing this marker.
    #[serde(default)]
    pub before: Option<String>,
    /// Inject after the first line containing this marker.
    #[serde(default)]
    pub after: Option<String>,
    /// Inject at this 1-based line number.
    #[serde(default)]
    pub at_line: Option<usize>,
    /// Skip the write when the existing file already contains this substring.
    #[serde(default)]
    pub skip_if: Option<String>,
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
                     Remediation: use only the closed key set (to, sparql, construct, \
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
/// broken, unreferenced template file from every unrelated
/// `[[generation.rules]]` rule or frontmatter template that doesn't
/// `{% include %}`/`{% extends %}` it: previously `Tera::new`'s own
/// `load_from_glob` aggregated every file's parse error into one `Err` and
/// aborted registration entirely — one orphaned file with a syntax error
/// (e.g. Jinja2-style `default("x")` instead of Tera's `default(value="x")`)
/// collaterally blocked every active rule in the same project. A file that
/// something DOES reference still fails loudly, just at the point it's
/// actually used (Tera's own `TemplateNotFound`/`MissingParent`), not
/// collaterally for the entire sync.
///
/// Returns an empty [`Tera::default`] (matching `Tera::new`'s own behavior
/// for a glob that matches nothing) when `templates_dir` doesn't exist at
/// all.
///
/// # Errors
/// `[FM-TPL-015]` if `templates_dir` exists but isn't readable, or if
/// building the inheritance chain / checking macro imports over the
/// *successfully-parsed* subset fails (a defect spanning multiple files
/// that all parsed fine individually — e.g. a child `{% extends %}`ing a
/// parent that itself failed to parse, or a real circular extend).
pub(crate) fn load_templates_glob_lenient(templates_dir: &Path) -> Result<Tera> {
    if !templates_dir.is_dir() {
        return Ok(Tera::default());
    }
    let canonical_root = templates_dir.canonicalize().map_err(|e| {
        AppError::fm_tpl(
            15,
            format!(
                "`{}` directory unreadable: {e}. Remediation: check its permissions.",
                templates_dir.display()
            ),
        )
    })?;

    let mut files: Vec<PathBuf> = Vec::new();
    collect_files_recursive(&canonical_root, &mut files).map_err(|e| {
        AppError::fm_tpl(
            15,
            format!(
                "walking `{}` failed: {e}. Remediation: check its permissions.",
                templates_dir.display()
            ),
        )
    })?;
    // Deterministic registration order (matters for reproducible error
    // ordering, not for correctness — Tera's inheritance build is a
    // separate, order-independent pass below).
    files.sort();

    let mut tera = Tera::default();
    for path in files {
        let rel_name = path
            .strip_prefix(&canonical_root)
            .unwrap_or(&path)
            .to_string_lossy()
            .replace('\\', "/");
        let content = match std::fs::read_to_string(&path) {
            Ok(c) => c,
            Err(e) => {
                tracing::warn!(
                    template.path = %rel_name,
                    template.error = %e,
                    "templates/**/* glob: skipping unreadable template file \
                     (it will still fail loudly if a rule or include actually uses it)",
                );
                continue;
            }
        }; // continue above is deliberate: no further code in this loop body reads `content`
           // before falling to the next file if it wasn't produced.
        match tera::Template::new(
            &rel_name,
            Some(path.to_string_lossy().to_string()),
            &content,
        ) {
            Ok(tpl) => {
                tera.templates.insert(rel_name, tpl);
            }
            Err(e) => {
                tracing::warn!(
                    template.path = %rel_name,
                    template.error = %e,
                    "templates/**/* glob: skipping unparseable template file \
                     (it will still fail loudly if a rule or include actually uses it)",
                );
            }
        }
    }

    tera.build_inheritance_chains().map_err(|e| {
        AppError::fm_tpl(
            15,
            format!(
                "Tera inheritance chain build failed across `{}`: {e}. \
                 Remediation: fix the reported `extends`/parent relationship.",
                templates_dir.display()
            ),
        )
    })?;
    tera.check_macro_files().map_err(|e| {
        AppError::fm_tpl(
            15,
            format!(
                "Tera macro import check failed across `{}`: {e}. \
                 Remediation: fix the reported macro import path.",
                templates_dir.display()
            ),
        )
    })?;
    Ok(tera)
}

/// Recursively collect every regular file under `dir` (mirrors
/// `Tera::new`'s own `templates/**/*` glob scope: every file at any depth,
/// no extension filter, symlinks followed via `Path::is_dir`/`is_file`
/// which resolve through them).
fn collect_files_recursive(dir: &Path, out: &mut Vec<PathBuf>) -> std::io::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let path = entry?.path();
        if path.is_dir() {
            collect_files_recursive(&path, out)?;
        } else if path.is_file() {
            out.push(path);
        }
    }
    Ok(())
}

/// Walk a `tera::Error`'s `source()` chain and join every level's `Display`
/// text with a colon separator. Tera's own `Display` impl prints only the
/// outermost wrapper message (something like "Failed to render 'x'") and
/// drops the actual cause (something like "Variable sparql_results not
/// found in context") — this crate's own
/// `sparql_functions_reject_both_rows_and_results` test (below) already had
/// to reach for `{:?}` instead of `{}` for exactly this reason. Used by
/// `crate::generation_rules` so an `[FM-GEN-008]` message carries the real
/// cause, not just the wrapper.
pub(crate) fn tera_error_full_chain(err: &tera::Error) -> String {
    use std::error::Error as StdError;
    let mut parts = vec![err.to_string()];
    let mut cause = StdError::source(err);
    while let Some(e) = cause {
        parts.push(e.to_string());
        cause = e.source();
    }
    parts.join(": ")
}

/// Best-effort extraction of a `<line> | <code>` gutter (Tera's own
/// pest-derived parse-error formatting) from a Tera error's full chain.
/// `None` for the overwhelming majority of `[FM-GEN-008]` failures —
/// render-time semantic errors like "Variable ... not found" carry no
/// line/col at all; only a genuine parse failure (e.g.
/// `render_output_file`'s own `render_str` hitting a syntax error inside
/// `output_file` itself) does.
pub(crate) fn tera_error_location(err: &tera::Error) -> Option<String> {
    tera_error_full_chain(err).lines().find_map(|line| {
        let trimmed = line.trim_start();
        let digits: String = trimmed.chars().take_while(char::is_ascii_digit).collect();
        if digits.is_empty() {
            return None;
        }
        let rest = trimmed[digits.len()..].trim_start();
        rest.starts_with('|').then(|| format!("line {digits}"))
    })
}

/// Classify a Tera render/parse failure into a [`TemplateFailureCause`],
/// combining `tera::ErrorKind` matching (where Tera gives a structured
/// variant) with message substring matching on the `Msg` catch-all (the
/// only signal Tera's 1.20.1 error model exposes for the rest — see
/// [`TemplateFailureCause`]'s own doc comment). `own_template_name` is the
/// name the caller registered/is rendering (e.g. `generation_rule::agent-rs`)
/// — used to tell a top-level `TemplateNotFound` (this template itself was
/// never registered) apart from one raised for a *different* name (an
/// `{% include %}`/`{% extends %}` target that's missing).
///
/// Tera 1.20.1's `Processor::render` (`renderer/processor.rs::render`)
/// unconditionally wraps EVERY render-time error one level deep —
/// `Error::chain(self.get_error_location(), e)`, i.e. an outer
/// `ErrorKind::Msg("Failed to render '<name>'")` whose `source()` is the
/// real cause — confirmed empirically: even a bare "Variable ... not found"
/// or `FilterNotFound` arrives wrapped this way. Classifying on `err.kind`
/// directly (the first version of this function did) therefore always saw
/// the generic wrapper and always fell through to
/// [`TemplateFailureCause::TemplateRenderInternal`]. This walks past that
/// one (or more) wrapper layer(s) to the real cause before classifying.
pub(crate) fn classify_tera_render_error(
    err: &tera::Error, own_template_name: &str,
) -> TemplateFailureCause {
    use std::error::Error as StdError;
    let mut current = err;
    loop {
        match &current.kind {
            tera::ErrorKind::Msg(_) => {
                match StdError::source(current).and_then(|s| s.downcast_ref::<tera::Error>()) {
                    // A generic wrapper around a further tera::Error --
                    // unwrap one level and re-classify on the real cause.
                    Some(inner) => {
                        current = inner;
                        continue;
                    }
                    // No further tera::Error inside: THIS Msg text is the
                    // real leaf cause (e.g. a bare
                    // `Error::msg("Variable ... not found ...")`, which
                    // carries no source at all).
                    None => return classify_msg(&current.to_string()),
                }
            }
            other => return classify_kind(other, own_template_name),
        }
    }
}

/// The non-`Msg` half of [`classify_tera_render_error`]'s match, extracted
/// so the unwrap loop above can re-invoke it at whatever depth the real
/// cause turns out to live at.
fn classify_kind(kind: &tera::ErrorKind, own_template_name: &str) -> TemplateFailureCause {
    match kind {
        tera::ErrorKind::TemplateNotFound(name) => {
            if name == own_template_name {
                TemplateFailureCause::TemplateNotFound
            } else {
                TemplateFailureCause::TemplateIncludeNotFound
            }
        }
        tera::ErrorKind::MissingParent { .. } | tera::ErrorKind::CircularExtend { .. } => {
            TemplateFailureCause::TemplateIncludeNotFound
        }
        tera::ErrorKind::FilterNotFound(_)
        | tera::ErrorKind::FunctionNotFound(_)
        | tera::ErrorKind::TestNotFound(_) => TemplateFailureCause::TemplateFilterUnknown,
        // The filter/function/test name WAS found; it failed when invoked
        // (wrong argument shape, wrong value type) — a context problem,
        // not an unknown-identifier problem. `Json` (a context value that
        // failed to serialize) is the same class of problem.
        tera::ErrorKind::CallFunction(_)
        | tera::ErrorKind::CallFilter(_)
        | tera::ErrorKind::CallTest(_)
        | tera::ErrorKind::Json(_) => TemplateFailureCause::TemplateContextInvalid,
        tera::ErrorKind::Io(_) | tera::ErrorKind::Utf8Conversion { .. } => {
            TemplateFailureCause::TemplateRenderInternal
        }
        tera::ErrorKind::InvalidMacroDefinition(_) => TemplateFailureCause::TemplateParseFailed,
        // Reached only if a `Msg` variant's `source()` downcast to
        // `tera::Error` but that inner error's OWN kind is again `Msg` with
        // no further tera::Error source — handled by the loop above, never
        // this arm directly; kept for exhaustiveness against Tera's
        // `#[non_exhaustive]`-style `ErrorKind`.
        tera::ErrorKind::Msg(msg) => classify_msg(msg),
        _ => TemplateFailureCause::TemplateRenderInternal,
    }
}

/// Substring classification for `tera::ErrorKind::Msg` — the catch-all
/// variant every render-time semantic error (variable lookup, math
/// operation, parse-error wrapper text) actually arrives as in Tera 1.20.1.
fn classify_msg(msg: &str) -> TemplateFailureCause {
    if msg.contains("not found in context") {
        TemplateFailureCause::TemplateVariableMissing
    } else if msg.contains("Failed to parse") || msg.contains("expected") {
        TemplateFailureCause::TemplateParseFailed
    } else if msg.contains("is not a number") || msg.contains("can not be evaluated") {
        TemplateFailureCause::TemplateContextInvalid
    } else {
        TemplateFailureCause::TemplateRenderInternal
    }
}

/// Execute `query` and convert the engine-neutral results into a Tera
/// [`Value`] (behavior-preserving: identical JSON shapes to the previous
/// oxigraph-typed implementation).
pub(crate) fn sparql_to_value(graph: &dyn GraphEngine, query: &str) -> Result<Value> {
    match graph.query(query)? {
        EngineQueryResults::Boolean(b) => Ok(Value::Bool(b)),
        EngineQueryResults::Solutions(solutions) => {
            Ok(Value::Array(solutions_to_values(solutions)))
        }
        EngineQueryResults::Graph(triples) => {
            let rows = triples
                .into_iter()
                .map(|triple| {
                    let mut row = tera::Map::new();
                    row.insert("subject".to_string(), Value::String(triple.subject));
                    row.insert("predicate".to_string(), Value::String(triple.predicate));
                    row.insert("object".to_string(), Value::String(triple.object_value));
                    Value::Object(row)
                })
                .collect();
            Ok(Value::Array(rows))
        }
    }
}

/// Convert engine-neutral SELECT rows into Tera `Value::Object`s (bare
/// variable-name keys, e.g. `name`, never `?name`). Shared by
/// [`sparql_to_value`]'s `Solutions` branch and
/// `crate::generation_rules`'s per-row context building, so both call sites
/// agree on the exact same row shape.
///
/// Datatype-aware: each cell's [`EngineValue`] (produced by
/// `graph::term_to_engine_value`) is mapped 1:1 to the matching Tera/JSON
/// `Value` variant by [`engine_value_to_tera`] — `xsd:boolean` literals
/// become `Value::Bool`, integer/decimal/float-family literals become
/// `Value::Number`, everything else stays `Value::String` exactly as
/// before. See [`EngineValue`]'s doc comment for the full datatype table
/// and the documented lossy-string scope boundary (language tags,
/// `xsd:dateTime`/`xsd:date`).
pub(crate) fn solutions_to_values(rows: Vec<EngineRow>) -> Vec<Value> {
    rows.into_iter()
        .map(|solution| {
            let mut row = tera::Map::new();
            for (var, value) in solution {
                row.insert(var, engine_value_to_tera(value));
            }
            Value::Object(row)
        })
        .collect()
}

/// Map one [`EngineValue`] cell to its Tera/JSON `Value` — the single
/// conversion point [`solutions_to_values`] uses for every row cell.
fn engine_value_to_tera(value: EngineValue) -> Value {
    match value {
        EngineValue::Bool(b) => Value::Bool(b),
        EngineValue::Int(n) => Value::Number(n.into()),
        // `term_to_engine_value` already filters out non-finite floats
        // (falls back to `EngineValue::String` with a warning), so
        // `from_f64` here is infallible in practice; the `unwrap_or`
        // fallback exists only as defense-in-depth, never expected to fire.
        EngineValue::Float(f) => serde_json::Number::from_f64(f)
            .map(Value::Number)
            .unwrap_or(Value::Null),
        EngineValue::String(s) => Value::String(s),
    }
}

/// `local(iri="…")` — the local name/fragment of an IRI: the substring
/// after the last `#`, or after the last `/` if there is no `#`, or the
/// whole string if neither separator is present.
fn local_fn(args: &HashMap<String, Value>) -> tera::Result<Value> {
    let iri = args
        .get("iri")
        .and_then(Value::as_str)
        .ok_or_else(|| tera::Error::msg("local() requires a string `iri` argument"))?;
    let local = iri
        .rsplit_once('#')
        .map(|(_, frag)| frag)
        .or_else(|| iri.rsplit_once('/').map(|(_, seg)| seg))
        .unwrap_or(iri);
    Ok(Value::String(local.to_string()))
}

/// Extract the rows array from either the engine's own `rows=` convention or
/// ggen-core's `results=` convention (`crates/ggen-core/src/register.rs`'s
/// `SparqlFirstFn`/`SparqlValuesFn`/`SparqlEmptyFn`/`SparqlCountFn`) — both
/// name the identical SELECT-result-array argument. Supplying both in the
/// same call is a hard error (never a silent pick of one); supplying
/// neither names both accepted argument names in the error.
fn rows_or_results_arg<'a>(
    args: &'a HashMap<String, Value>, fn_name: &str,
) -> tera::Result<&'a Vec<Value>> {
    match (args.get("rows"), args.get("results")) {
        (Some(_), Some(_)) => Err(tera::Error::msg(format!(
            "{fn_name}() accepts either `rows` or `results`, not both — pick one convention"
        ))),
        (Some(v), None) => v.as_array().ok_or_else(|| {
            tera::Error::msg(format!("{fn_name}() requires an array `rows` argument"))
        }),
        (None, Some(v)) => v.as_array().ok_or_else(|| {
            tera::Error::msg(format!("{fn_name}() requires an array `results` argument"))
        }),
        (None, None) => Err(tera::Error::msg(format!(
            "{fn_name}() requires an array `rows` or `results` argument"
        ))),
    }
}

/// `sparql_first(rows=…)` / `sparql_first(results=…)` — the first row, or
/// `null` if empty. With `column="…"` given (under either convention),
/// returns just that column's value from the first row instead of the full
/// row object — `null` when the first row is not an object or lacks that
/// column, matching ggen-core's `SparqlFirstFn` result-shape semantics.
fn sparql_first_fn(args: &HashMap<String, Value>) -> tera::Result<Value> {
    let rows = rows_or_results_arg(args, "sparql_first")?;
    let Some(first) = rows.first() else {
        return Ok(Value::Null);
    };
    match args.get("column").and_then(Value::as_str) {
        Some(column) => Ok(first
            .as_object()
            .and_then(|o| o.get(column))
            .cloned()
            .unwrap_or(Value::Null)),
        None => Ok(first.clone()),
    }
}

/// `sparql_values(rows=…, column="…")` / `sparql_values(results=…,
/// column="…")` — the array of `column`'s value across every row
/// (missing/non-object rows contribute `null`). `column` is required under
/// either convention (unchanged from the engine's original `rows=`-only
/// behavior).
fn sparql_values_fn(args: &HashMap<String, Value>) -> tera::Result<Value> {
    let rows = rows_or_results_arg(args, "sparql_values")?;
    let column = args
        .get("column")
        .and_then(Value::as_str)
        .ok_or_else(|| tera::Error::msg("sparql_values() requires a string `column` argument"))?;
    let values: Vec<Value> = rows
        .iter()
        .map(|row| {
            row.as_object()
                .and_then(|o| o.get(column))
                .cloned()
                .unwrap_or(Value::Null)
        })
        .collect();
    Ok(Value::Array(values))
}

/// `sparql_empty(rows=…)` / `sparql_empty(results=…)` — `true` if the array
/// has no elements.
fn sparql_empty_fn(args: &HashMap<String, Value>) -> tera::Result<Value> {
    let rows = rows_or_results_arg(args, "sparql_empty")?;
    Ok(Value::Bool(rows.is_empty()))
}

/// `sparql_count(rows=…)` / `sparql_count(results=…)` — the number of rows.
fn sparql_count_fn(args: &HashMap<String, Value>) -> tera::Result<Value> {
    let rows = rows_or_results_arg(args, "sparql_count")?;
    Ok(Value::Number(rows.len().into()))
}

/// `hex_to_u64` filter: `"0x560b48233d2e635f"` (or `"560b48233d2e635f"`,
/// case-insensitive, optional `0x` prefix) → the JSON number `6197033906917782879`.
///
/// For emitting hex-encoded ontology literals (e.g. `ceng:caseSeed`) as a
/// plain integer where the consuming Rust type is numeric (`u64`), not a
/// string — the template layer converts once here instead of asking every
/// numeric-seed consumer to accept a hex-string encoding.
fn hex_to_u64_filter(value: &Value, _args: &HashMap<String, Value>) -> tera::Result<Value> {
    let s = value
        .as_str()
        .ok_or_else(|| tera::Error::msg("hex_to_u64 filter requires a string"))?;
    let trimmed = s
        .strip_prefix("0x")
        .or_else(|| s.strip_prefix("0X"))
        .unwrap_or(s);
    let n = u64::from_str_radix(trimmed, 16)
        .map_err(|e| tera::Error::msg(format!("hex_to_u64: {s:?} is not valid hex: {e}")))?;
    Ok(Value::Number(n.into()))
}

/// `snake_case` filter: `FooBar`, `foo-bar`, `foo bar` → `foo_bar`.
#[allow(clippy::unnecessary_wraps)]
fn snake_case_filter(value: &Value, _args: &HashMap<String, Value>) -> tera::Result<Value> {
    let s = value
        .as_str()
        .ok_or_else(|| tera::Error::msg("snake_case filter requires a string"))?;
    let mut out = String::with_capacity(s.len() + 4);
    let mut prev_lower = false;
    for ch in s.chars() {
        if ch == '-' || ch == ' ' || ch == '_' {
            if !out.ends_with('_') && !out.is_empty() {
                out.push('_');
            }
            prev_lower = false;
        } else if ch.is_uppercase() {
            if prev_lower && !out.ends_with('_') {
                out.push('_');
            }
            out.extend(ch.to_lowercase());
            prev_lower = false;
        } else {
            out.push(ch);
            prev_lower = ch.is_lowercase() || ch.is_ascii_digit();
        }
    }
    Ok(Value::String(out))
}

/// `pascal_case` filter: `foo_bar`, `foo-bar`, `foo bar` → `FooBar`.
#[allow(clippy::unnecessary_wraps)]
fn pascal_case_filter(value: &Value, _args: &HashMap<String, Value>) -> tera::Result<Value> {
    let s = value
        .as_str()
        .ok_or_else(|| tera::Error::msg("pascal_case filter requires a string"))?;
    let mut out = String::with_capacity(s.len());
    let mut upper_next = true;
    for ch in s.chars() {
        if ch == '_' || ch == '-' || ch == ' ' {
            upper_next = true;
        } else if upper_next {
            out.extend(ch.to_uppercase());
            upper_next = false;
        } else {
            out.push(ch);
        }
    }
    Ok(Value::String(out))
}

/// Split `s` into lowercase words on `_`/`-`/` ` separators and
/// lower-to-upper case boundaries (shared by the case/inflection filters
/// below, so each filter only decides how to rejoin the words).
fn split_words(s: &str) -> Vec<String> {
    let mut words = Vec::new();
    let mut current = String::new();
    let mut prev_lower = false;
    for ch in s.chars() {
        if ch == '_' || ch == '-' || ch == ' ' {
            if !current.is_empty() {
                words.push(std::mem::take(&mut current));
            }
            prev_lower = false;
        } else if ch.is_uppercase() && prev_lower {
            if !current.is_empty() {
                words.push(std::mem::take(&mut current));
            }
            current.extend(ch.to_lowercase());
            prev_lower = false;
        } else {
            current.extend(ch.to_lowercase());
            prev_lower = ch.is_lowercase() || ch.is_ascii_digit();
        }
    }
    if !current.is_empty() {
        words.push(current);
    }
    words
}

/// `camel_case` filter: `foo_bar`, `FooBar`, `foo-bar` → `fooBar`
/// (lowerCamelCase — first word lowercase, subsequent words capitalized, no
/// separators).
#[allow(clippy::unnecessary_wraps)]
fn camel_case_filter(value: &Value, _args: &HashMap<String, Value>) -> tera::Result<Value> {
    let s = value
        .as_str()
        .ok_or_else(|| tera::Error::msg("camel_case filter requires a string"))?;
    let words = split_words(s);
    let mut out = String::with_capacity(s.len());
    for (i, word) in words.iter().enumerate() {
        if i == 0 {
            out.push_str(word);
        } else {
            let mut chars = word.chars();
            if let Some(first) = chars.next() {
                out.extend(first.to_uppercase());
                out.push_str(chars.as_str());
            }
        }
    }
    Ok(Value::String(out))
}

/// `kebab_case` filter: `foo_bar`, `FooBar`, `foo bar` → `foo-bar`.
#[allow(clippy::unnecessary_wraps)]
fn kebab_case_filter(value: &Value, _args: &HashMap<String, Value>) -> tera::Result<Value> {
    let s = value
        .as_str()
        .ok_or_else(|| tera::Error::msg("kebab_case filter requires a string"))?;
    Ok(Value::String(split_words(s).join("-")))
}

/// `shouty_snake_case` filter: `foo_bar`, `FooBar`, `foo-bar` → `FOO_BAR`.
#[allow(clippy::unnecessary_wraps)]
fn shouty_snake_case_filter(value: &Value, _args: &HashMap<String, Value>) -> tera::Result<Value> {
    let s = value
        .as_str()
        .ok_or_else(|| tera::Error::msg("shouty_snake_case filter requires a string"))?;
    Ok(Value::String(split_words(s).join("_").to_uppercase()))
}

/// `title_case` filter: `foo_bar`, `FooBar`, `foo-bar` → `Foo Bar`.
#[allow(clippy::unnecessary_wraps)]
fn title_case_filter(value: &Value, _args: &HashMap<String, Value>) -> tera::Result<Value> {
    let s = value
        .as_str()
        .ok_or_else(|| tera::Error::msg("title_case filter requires a string"))?;
    let titled: Vec<String> = split_words(s)
        .into_iter()
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
                None => word,
            }
        })
        .collect();
    Ok(Value::String(titled.join(" ")))
}

/// `pluralize` filter: naive English pluralization (`bus`→`buses`,
/// `city`→`cities`, `cat`→`cats`). Operates on the whole input string
/// as-is (no word-splitting) since it's meant for a single noun, not a
/// multi-word identifier.
#[allow(clippy::unnecessary_wraps)]
fn pluralize_filter(value: &Value, _args: &HashMap<String, Value>) -> tera::Result<Value> {
    let s = value
        .as_str()
        .ok_or_else(|| tera::Error::msg("pluralize filter requires a string"))?;
    Ok(Value::String(pluralize_word(s)))
}

fn pluralize_word(s: &str) -> String {
    if s.is_empty() {
        return String::new();
    }
    let lower = s.to_ascii_lowercase();
    if lower.ends_with('y')
        && !lower.ends_with("ay")
        && !lower.ends_with("ey")
        && !lower.ends_with("oy")
        && !lower.ends_with("uy")
    {
        format!("{}ies", &s[..s.len() - 1])
    } else if lower.ends_with('s')
        || lower.ends_with('x')
        || lower.ends_with('z')
        || lower.ends_with("ch")
        || lower.ends_with("sh")
    {
        format!("{s}es")
    } else {
        format!("{s}s")
    }
}

/// `singularize` filter: reverse of [`pluralize_word`]'s heuristic
/// (`buses`→`bus`, `cities`→`city`, `cats`→`cat`). Best-effort, not a
/// dictionary — irregular plurals (`people`, `children`) are unaffected.
#[allow(clippy::unnecessary_wraps)]
fn singularize_filter(value: &Value, _args: &HashMap<String, Value>) -> tera::Result<Value> {
    let s = value
        .as_str()
        .ok_or_else(|| tera::Error::msg("singularize filter requires a string"))?;
    let lower = s.to_ascii_lowercase();
    let out = if lower.ends_with("ies") && s.len() > 3 {
        format!("{}y", &s[..s.len() - 3])
    } else if lower.ends_with("ches") || lower.ends_with("shes") {
        s[..s.len() - 2].to_string()
    } else if (lower.ends_with("ses") || lower.ends_with("xes") || lower.ends_with("zes"))
        && s.len() > 3
    {
        s[..s.len() - 2].to_string()
    } else if lower.ends_with('s') && !lower.ends_with("ss") {
        s[..s.len() - 1].to_string()
    } else {
        s.to_string()
    };
    Ok(Value::String(out))
}

#[cfg(test)]
mod tests {
    use super::*;

    const TTL: &str = r#"
        @prefix ex: <http://example.org/> .
        ex:alice ex:name "alice_smith" .
        ex:bob   ex:name "bob_jones" .
    "#;

    fn graph() -> Arc<dyn GraphEngine> {
        let g = crate::graph::DeterministicGraph::new().expect("graph");
        g.insert_turtle(TTL).expect("ttl");
        Arc::new(g)
    }

    #[test]
    fn sparql_row_keys_are_bare_not_question_mark_prefixed() {
        // Direct regression guard for a confirmed bug in ~/ggen's rendering
        // engine (ggen-core/src/pipeline.rs builds row keys with
        // `var.to_string()`, whose `Display` impl is `write!(f, "?{name}")`,
        // forcing template authors to write `row["?name"]`). This crate's
        // `sparql_to_value` must keep using `Variable::as_str()` (bare name,
        // no `?`), so `row.name` works directly. If this test ever fails
        // after touching `sparql_to_value`, that's this exact regression.
        let value = sparql_to_value(
            graph().as_ref(),
            "SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name",
        )
        .expect("query");
        let rows = value.as_array().expect("array");
        let first = rows[0].as_object().expect("object");
        assert!(
            first.contains_key("name"),
            "row must have bare key `name`: {first:?}"
        );
        assert!(
            !first.contains_key("?name"),
            "row must NOT have `?`-prefixed key `?name`: {first:?}"
        );
    }

    #[test]
    fn parse_and_render_with_sparql_against_graph() {
        let content = "---\nto: out.rs\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in sparql(query=q) %}{{ row.name | pascal_case }};{% endfor %}";
        let tpl = Template::parse(content).expect("parse");
        assert_eq!(tpl.frontmatter.to, "out.rs");
        let q = tpl.frontmatter.sparql.get("people").expect("query").clone();

        let mut tera = build_tera(graph()).expect("build tera");
        let mut ctx = tera::Context::new();
        ctx.insert("q", &q);
        let rendered = tera.render_str(&tpl.body, &ctx).expect("render");
        assert_eq!(rendered, "AliceSmith;BobJones;");
    }

    #[test]
    fn ask_query_returns_bool() {
        let tera = build_tera(graph()).expect("build tera");
        let mut t = tera;
        let rendered = t
            .render_str(
                "{% if sparql(query=\"ASK { ?s ?p ?o }\") %}yes{% else %}no{% endif %}",
                &tera::Context::new(),
            )
            .expect("render");
        assert_eq!(rendered, "yes");
    }

    #[test]
    fn construct_returns_triples() {
        let mut tera = build_tera(graph()).expect("build tera");
        let rendered = tera
            .render_str(
                "{% set ts = sparql(query=\"CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\") %}{{ ts | length }}",
                &tera::Context::new(),
            )
            .expect("render");
        assert_eq!(rendered, "2");
    }

    #[test]
    fn unknown_frontmatter_key_is_err() {
        let content = "---\nto: out.rs\nvars:\n  x: 1\n---\nbody";
        let err = Template::parse(content).expect_err("must reject `vars:`");
        assert!(err.to_string().contains("FM-TPL-002"), "{err}");
    }

    #[test]
    fn missing_frontmatter_is_err() {
        let err = Template::parse("no frontmatter here").expect_err("must reject");
        assert!(err.to_string().contains("FM-TPL-001"), "{err}");
    }

    #[test]
    fn snake_and_pascal_filters() {
        let mut tera = build_tera(graph()).expect("build tera");
        let rendered = tera
            .render_str(
                "{{ \"FooBarBaz\" | snake_case }} {{ \"foo_bar-baz qux\" | pascal_case }}",
                &tera::Context::new(),
            )
            .expect("render");
        assert_eq!(rendered, "foo_bar_baz FooBarBazQux");
    }

    #[test]
    fn camel_kebab_shouty_title_filters() {
        let mut tera = build_tera(graph()).expect("build tera");
        let rendered = tera
            .render_str(
                "{{ \"foo_bar-baz\" | camel_case }} \
                 {{ \"FooBarBaz\" | kebab_case }} \
                 {{ \"foo-bar baz\" | shouty_snake_case }} \
                 {{ \"foo_bar-baz\" | title_case }}",
                &tera::Context::new(),
            )
            .expect("render");
        assert_eq!(rendered, "fooBarBaz foo-bar-baz FOO_BAR_BAZ Foo Bar Baz");
    }

    #[test]
    fn pluralize_and_singularize_filters() {
        let mut tera = build_tera(graph()).expect("build tera");
        let rendered = tera
            .render_str(
                "{{ \"cat\" | pluralize }} {{ \"city\" | pluralize }} {{ \"bus\" | pluralize }} \
                 {{ \"cats\" | singularize }} {{ \"cities\" | singularize }} {{ \"buses\" | singularize }}",
                &tera::Context::new(),
            )
            .expect("render");
        assert_eq!(rendered, "cats cities buses cat city bus");
    }

    #[test]
    fn local_function_strips_namespace() {
        let mut tera = build_tera(graph()).expect("build tera");
        let rendered = tera
            .render_str(
                "{{ local(iri=\"http://example.org/name\") }} \
                 {{ local(iri=\"http://example.org/ns#Widget\") }} \
                 {{ local(iri=\"plain\") }}",
                &tera::Context::new(),
            )
            .expect("render");
        assert_eq!(rendered, "name Widget plain");
    }

    #[test]
    fn sparql_first_values_empty_count_functions() {
        let mut tera = build_tera(graph()).expect("build tera");
        let rendered = tera
            .render_str(
                "{% set rows = sparql(query=\"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\") %}\
                 {% set first = sparql_first(rows=rows) %}\
                 {{ first.name }};\
                 {{ sparql_values(rows=rows, column=\"name\") | join(sep=\",\") }};\
                 {{ sparql_count(rows=rows) }};\
                 {{ sparql_empty(rows=rows) }};\
                 {{ sparql_empty(rows=[]) }}",
                &tera::Context::new(),
            )
            .expect("render");
        assert_eq!(rendered, "alice_smith;alice_smith,bob_jones;2;false;true");
    }

    #[test]
    fn sparql_first_on_empty_rows_is_null() {
        let mut tera = build_tera(graph()).expect("build tera");
        let rendered = tera
            .render_str("{{ sparql_first(rows=[]) }}", &tera::Context::new())
            .expect("render");
        assert_eq!(
            rendered, "",
            "Tera renders a Value::Null function result as empty output"
        );
    }

    // ---- Gap 2: bare filter aliases (snake, pascal, camel, kebab, title) ----

    #[test]
    fn filter_aliases_match_full_names_byte_identical() {
        let mut tera = build_tera(graph()).expect("build tera");
        for s in ["hello_world_example", "HelloWorldExample"] {
            let mut ctx = tera::Context::new();
            ctx.insert("v", s);
            let rendered = tera
                .render_str(
                    "{{ v | snake }}|{{ v | snake_case }}|\
                     {{ v | pascal }}|{{ v | pascal_case }}|\
                     {{ v | camel }}|{{ v | camel_case }}|\
                     {{ v | kebab }}|{{ v | kebab_case }}|\
                     {{ v | title }}|{{ v | title_case }}|\
                     {{ v | shouty }}|{{ v | shouty_snake_case }}",
                    &ctx,
                )
                .expect("render");
            let parts: Vec<&str> = rendered.split('|').collect();
            assert_eq!(parts.len(), 12, "unexpected render shape: {rendered:?}");
            assert_eq!(parts[0], parts[1], "snake alias mismatch for {s}");
            assert_eq!(parts[2], parts[3], "pascal alias mismatch for {s}");
            assert_eq!(parts[4], parts[5], "camel alias mismatch for {s}");
            assert_eq!(parts[6], parts[7], "kebab alias mismatch for {s}");
            assert_eq!(parts[8], parts[9], "title alias mismatch for {s}");
            assert_eq!(parts[10], parts[11], "shouty alias mismatch for {s}");
        }
    }

    // ---- Gap 3: sparql_* rows=/results= reconciliation ----

    #[test]
    fn sparql_first_values_accept_results_alias_and_column_extraction() {
        let mut tera = build_tera(graph()).expect("build tera");
        let rendered = tera
            .render_str(
                "{% set rows = sparql(query=\"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\") %}\
                 {% set first_full = sparql_first(results=rows) %}\
                 {{ first_full.name }};\
                 {{ sparql_first(results=rows, column=\"name\") }};\
                 {{ sparql_values(results=rows, column=\"name\") | join(sep=\",\") }};\
                 {{ sparql_empty(results=rows) }};\
                 {{ sparql_count(results=rows) }}",
                &tera::Context::new(),
            )
            .expect("render");
        assert_eq!(
            rendered, "alice_smith;alice_smith;alice_smith,bob_jones;false;2",
            "results= must behave identically to rows=, and results=+column= \
             must extract the scalar/list for that column"
        );
    }

    #[test]
    fn sparql_functions_reject_both_rows_and_results() {
        let mut tera = build_tera(graph()).expect("build tera");
        for call in [
            "sparql_first(rows=[], results=[])",
            "sparql_values(rows=[], results=[], column=\"x\")",
            "sparql_empty(rows=[], results=[])",
            "sparql_count(rows=[], results=[])",
        ] {
            let err = tera
                .render_str(&format!("{{{{ {call} }}}}"), &tera::Context::new())
                .unwrap_err();
            // Tera's `Display` only prints the outer "Failed to render"
            // wrapper, not the chained function-call cause — `{:?}` walks
            // the full `source` chain (`Error { kind, source: Some(..) }`),
            // which is where our own message actually lands.
            assert!(
                format!("{err:?}").contains("not both"),
                "{call} must refuse rows=+results= loudly: {err:?}"
            );
        }
    }

    // ---- Gap 4: flexible `sparql:` frontmatter deserializer ----

    #[test]
    fn sparql_field_accepts_bare_string_and_sequence_forms() {
        const QUERY: &str =
            "SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name";
        let map_tpl = Template::parse(&format!(
            "---\nto: out.rs\nsparql:\n  people: {QUERY}\n---\nbody"
        ))
        .expect("map form parses");
        let bare_tpl = Template::parse(&format!("---\nto: out.rs\nsparql: {QUERY}\n---\nbody"))
            .expect("bare form parses");
        let seq_tpl = Template::parse(&format!("---\nto: out.rs\nsparql:\n  - {QUERY}\n---\nbody"))
            .expect("sequence form parses");

        assert_eq!(
            map_tpl.frontmatter.sparql.get("people").map(String::as_str),
            Some(QUERY)
        );
        assert_eq!(
            bare_tpl
                .frontmatter
                .sparql
                .get("default")
                .map(String::as_str),
            Some(QUERY),
            "bare string must be named `default`, matching ggen-core's convention"
        );
        assert_eq!(
            seq_tpl.frontmatter.sparql.get("query_0").map(String::as_str),
            Some(QUERY),
            "sequence entries must be named `query_0`, `query_1`, …, matching ggen-core's convention"
        );
    }

    #[test]
    fn bare_string_sparql_field_is_queryable_same_as_map_form() {
        let content = "---\nto: out.rs\nsparql: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in sparql(query=q) %}{{ row.name | pascal_case }};{% endfor %}";
        let tpl = Template::parse(content).expect("parse");
        let q = tpl
            .frontmatter
            .sparql
            .get("default")
            .expect("default query")
            .clone();

        let mut tera = build_tera(graph()).expect("build tera");
        let mut ctx = tera::Context::new();
        ctx.insert("q", &q);
        let rendered = tera.render_str(&tpl.body, &ctx).expect("render");
        assert_eq!(
            rendered, "AliceSmith;BobJones;",
            "identical to the mapping-form test `parse_and_render_with_sparql_against_graph`"
        );
    }
}
