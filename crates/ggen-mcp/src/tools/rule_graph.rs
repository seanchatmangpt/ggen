//! `ggen_rule_graph` — expose the rule → query → template → output wiring
//! for a whole project.
//!
//! This is the project map an agent most wants when orienting in an
//! unfamiliar ggen project ("what rules exist, what does each one read, and
//! what does it write") and nothing surfaced it. `ggen_lsp::ProjectIndex`
//! already resolves exactly this, including each rule's `selected_vars`, so
//! this tool is a projection of that index, not new analysis.
//!
//! Only meaningful for the declarative-rules `ggen.toml` schema (the
//! frontmatter schema has no `[[generation.rules]]`); a frontmatter project
//! yields an empty rule list, which the result states explicitly rather
//! than leaving the caller to guess.

use ggen_lsp::project_index::ProjectIndex;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::error::{ErrorCategory, McpError};
use crate::project_root::resolve_root;

/// Server-enforced page size cap, applied regardless of what the caller
/// requests -- the same "clamp server-side" idiom used for query rows.
const MAX_PAGE_SIZE: usize = 200;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct RuleGraphParams {
    /// Project root directory.
    pub root: String,
    /// Optional: return only the rule with this `name`.
    #[serde(default)]
    pub rule_name: Option<String>,
    /// Zero-based offset for paging. Defaults to 0.
    #[serde(default)]
    pub offset: Option<usize>,
    /// Page size. Clamped to a server maximum regardless of value.
    #[serde(default)]
    pub limit: Option<usize>,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct RuleEdge {
    pub rule_id: String,
    /// `true` when the SPARQL was inline in the manifest rather than a file.
    pub query_inline: bool,
    /// SELECT variables this rule's query binds, without the leading `?`.
    /// These are exactly the names its template may consume.
    pub selected_vars: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub template_path: Option<String>,
    /// `true` when the template's content could not be resolved (missing
    /// file, or an unimplemented source such as pack/git/package -- see
    /// `ggen_capability_status`).
    pub template_unresolved: bool,
    pub output_file: String,
    /// Non-fatal resolution problems for this rule, e.g. a missing query or
    /// template file. Preserved rather than dropped.
    pub issues: Vec<String>,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct RuleGraphResult {
    pub ok: bool,
    /// Total rules in the project, before paging/filtering.
    pub total_rules: usize,
    pub returned: usize,
    pub offset: usize,
    pub has_more: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub next_offset: Option<usize>,
    pub rules: Vec<RuleEdge>,
}

/// Project the rule index.
///
/// # Errors
/// `ErrorCategory::PathTraversal` for an unresolvable `root`;
/// `ErrorCategory::ConfigError` when the manifest is missing or unparseable;
/// `ErrorCategory::NotFound` when `rule_name` matches no rule.
pub fn rule_graph(params: &RuleGraphParams) -> Result<RuleGraphResult, McpError> {
    let root = resolve_root(&params.root)?;
    let index = ProjectIndex::from_root(&root)
        .map_err(|e| McpError::new(ErrorCategory::ConfigError, e.to_string()))?;

    let mut entries: Vec<_> = index.rule_entries.iter().collect();
    if let Some(wanted) = params.rule_name.as_deref() {
        entries.retain(|e| e.rule_id == wanted);
        if entries.is_empty() {
            let available: Vec<&str> = index
                .rule_entries
                .iter()
                .map(|e| e.rule_id.as_str())
                .collect();
            return Err(McpError::new(
                ErrorCategory::NotFound,
                format!(
                    "no rule named {wanted:?}. Rules in this project: {}",
                    if available.is_empty() {
                        "(none -- this may be a frontmatter-schema project, which has \
                         no [[generation.rules]])"
                            .to_string()
                    } else {
                        available.join(", ")
                    }
                ),
            ));
        }
    }

    let total_rules = entries.len();
    let offset = params.offset.unwrap_or(0).min(total_rules);
    let limit = params.limit.unwrap_or(MAX_PAGE_SIZE).min(MAX_PAGE_SIZE);
    let page: Vec<RuleEdge> = entries
        .iter()
        .skip(offset)
        .take(limit)
        .map(|e| RuleEdge {
            rule_id: e.rule_id.clone(),
            query_inline: e.query_inline,
            selected_vars: e.selected_vars.iter().cloned().collect(),
            template_path: e.template_path.as_ref().map(|p| p.display().to_string()),
            template_unresolved: e.template_content.is_none(),
            output_file: e.output_file.clone(),
            issues: e.issues.clone(),
        })
        .collect();

    let returned = page.len();
    let has_more = offset + returned < total_rules;
    Ok(RuleGraphResult {
        ok: true,
        total_rules,
        returned,
        offset,
        has_more,
        next_offset: has_more.then_some(offset + returned),
        rules: page,
    })
}
