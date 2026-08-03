//! `ggen_frontmatter_lint` — parse one template and report its
//! SPARQL-projected variables, its Tera-consumed variables, and the
//! difference between them, WITHOUT running the pipeline.
//!
//! Closes two verified friction points at once:
//!   1. A Tera body that cannot parse (e.g. Jinja2 `{{ x if y else z }}`
//!      ternary syntax, which Tera has no such construct for) was only
//!      discoverable by running a full `sync run` and reading a parse error.
//!   2. The SELECT-vars ∩ template-vars diff -- the check that catches "my
//!      template uses `{{ row.name }}` but my SELECT never binds `?name`"
//!      -- existed only inside analyzer internals, reachable by no tool.
//!
//! Reuses `ggen_engine::lint`'s own `consumed_vars`/`projected_vars`/
//! `lint_template`, which is where this logic already lives -- not a
//! reimplementation.

use std::collections::BTreeSet;

use ggen_engine::lint::{consumed_vars, lint_template, projected_vars, Projection};
use ggen_engine::template::Template;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::error::{ErrorCategory, McpError};
use crate::limits::MAX_QUERY_TEXT_BYTES;
use crate::project_root::{resolve_relative, resolve_root};

#[derive(Debug, Deserialize, JsonSchema)]
pub struct FrontmatterLintParams {
    /// Project root directory.
    pub root: String,
    /// Template path, relative to `root`.
    pub template_path: String,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct FrontmatterLintResult {
    pub ok: bool,
    /// Diagnostics from `ggen_engine::lint::lint_template` -- the same
    /// author-time checks the engine itself applies.
    pub diagnostics: Vec<String>,
    /// Variables the template body actually references.
    pub consumed_vars: BTreeSet<String>,
    /// Variables the frontmatter's SPARQL queries project into the Tera
    /// context. `None` when a query uses `SELECT *`, which makes the
    /// projected set unknowable and disables the unbound-variable checks
    /// (reported via `projection_is_wildcard`).
    pub projected_vars: Option<BTreeSet<String>>,
    /// `true` when at least one query used `SELECT *`. In that case
    /// `undefined_vars` is empty NOT because everything is bound, but
    /// because the check could not run -- reported explicitly rather than
    /// silently returning a clean result.
    pub projection_is_wildcard: bool,
    /// Consumed but not projected -- the likely-bug set.
    pub undefined_vars: BTreeSet<String>,
    /// Projected but never consumed -- dead query columns.
    pub unused_vars: BTreeSet<String>,
    /// Which projection mode this template will use at sync time
    /// (`fan_out` / `aggregate` / `single`) -- see
    /// `ggen_frontmatter_schema`'s `projection_modes` for what each means.
    pub projection_mode: String,
}

/// Lint one template file.
///
/// # Errors
/// `ErrorCategory::PathTraversal` for an unresolvable `root`/`template_path`;
/// `ErrorCategory::NotFound` if the template is unreadable;
/// `ErrorCategory::SyntaxError` if the frontmatter block itself will not
/// parse (an unknown key, a malformed YAML block, a missing `---`) -- the
/// caller gets the engine's own typed `[FM-TPL-*]` message, not a generic
/// failure.
pub fn frontmatter_lint(params: &FrontmatterLintParams) -> Result<FrontmatterLintResult, McpError> {
    let root = resolve_root(&params.root)?;
    let path = resolve_relative(&root, &params.template_path)?;
    let content = std::fs::read_to_string(&path).map_err(|e| {
        McpError::new(
            ErrorCategory::NotFound,
            format!("{} unreadable: {e}", path.display()),
        )
    })?;
    if content.len() > MAX_QUERY_TEXT_BYTES {
        return Err(McpError::new(
            ErrorCategory::InputTooLarge,
            format!("template exceeds {MAX_QUERY_TEXT_BYTES} bytes"),
        ));
    }

    // A frontmatter that will not parse is a SyntaxError, distinct from a
    // template that parses but has unbound variables -- never merged.
    let template = Template::parse(&content)
        .map_err(|e| McpError::new(ErrorCategory::SyntaxError, e.to_string()))?;

    let consumed = consumed_vars(&template.body);
    let (projected, is_wildcard) = match projected_vars(&template.frontmatter) {
        Projection::Wildcard => (None, true),
        Projection::Vars(v) => (Some(v), false),
    };

    let (undefined_vars, unused_vars) = match projected.as_ref() {
        Some(p) => (
            consumed.difference(p).cloned().collect(),
            p.difference(&consumed).cloned().collect(),
        ),
        // SELECT * -- the diff is not computable. Empty sets here mean
        // "not checked", which `projection_is_wildcard` states explicitly.
        None => (BTreeSet::new(), BTreeSet::new()),
    };

    let projection_mode = if template.frontmatter.to.contains("{{") {
        "fan_out"
    } else if template.frontmatter.for_each.is_some() {
        "aggregate"
    } else {
        "single"
    };

    let diagnostics: Vec<String> = lint_template(&path, &template)
        .iter()
        .map(ToString::to_string)
        .collect();

    Ok(FrontmatterLintResult {
        ok: true,
        diagnostics,
        consumed_vars: consumed,
        projected_vars: projected,
        projection_is_wildcard: is_wildcard,
        undefined_vars,
        unused_vars,
        projection_mode: projection_mode.to_string(),
    })
}
