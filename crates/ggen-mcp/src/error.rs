//! One structured error taxonomy, used at every tool call site.
//!
//! A prior Rust MCP server (`~/ggen-mcp`, unrelated to this crate, kept only
//! as a design-idiom reference) built a rich `ErrorCode` enum with
//! retry/category metadata and then only wired it through at 2 of 21 call
//! sites -- the type existed, the discipline didn't. This module exists so
//! that mistake is structurally harder to repeat: every tool's pure `fn`
//! returns `Result<T, McpError>`, and the `#[tool]` adapter's only job is to
//! convert that into `rmcp`'s `CallToolResult`/`ErrorData`, never to invent
//! its own ad-hoc error string.

use serde::Serialize;

/// Closed set of error categories a ggen-mcp tool can report. Adding a new
/// tool means picking from this set or, if genuinely novel, adding a new
/// variant here -- never stringly-typing a category inline at a call site.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ErrorCategory {
    /// Input exceeded a declared size/row cap before any graph work began.
    InputTooLarge,
    /// The SPARQL text itself is malformed (caught before graph load).
    SyntaxError,
    /// The project's ontology/imports could not be loaded into a graph.
    GraphLoadError,
    /// `root` (or another path-shaped parameter) failed traversal-safety
    /// resolution -- escapes the project root, absolute, or a symlink
    /// escape. See `project_root::resolve_root`.
    PathTraversal,
    /// `ggen.toml` itself could not be classified/parsed.
    ConfigError,
    /// A referenced file (template, rule, ontology) does not exist.
    NotFound,
    /// A capability that is intentionally out of scope for this tool
    /// (e.g. `TemplateSource::Pack` resolution) was requested.
    Unsupported,
    /// Anything else -- a real bug, not a modeled refusal. Should be rare;
    /// every anticipated failure mode gets its own category above instead.
    Internal,
}

impl ErrorCategory {
    /// Whether retrying the exact same call could plausibly succeed. Every
    /// category here is a function of the input/project state, not of
    /// transient infrastructure, so all are `false` today -- kept as a
    /// method (not a bare `false` at call sites) so a future category with
    /// real retry semantics (e.g. a network-backed pack fetch) doesn't
    /// require an audit of every caller.
    #[must_use]
    pub fn retryable(self) -> bool {
        false
    }
}

/// The one error type every ggen-mcp tool's pure logic function returns.
#[derive(Debug, Clone, Serialize, thiserror::Error)]
#[error("{category:?}: {message}")]
pub struct McpError {
    pub category: ErrorCategory,
    pub message: String,
    /// Byte offset into the offending input, when known (e.g. a SPARQL
    /// parse error's position). Never fabricated -- absent unless the
    /// underlying failure actually carries a position.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub offset: Option<usize>,
}

impl McpError {
    #[must_use]
    pub fn new(category: ErrorCategory, message: impl Into<String>) -> Self {
        Self {
            category,
            message: message.into(),
            offset: None,
        }
    }

    #[must_use]
    pub fn with_offset(mut self, offset: usize) -> Self {
        self.offset = Some(offset);
        self
    }

    #[must_use]
    pub fn retryable(&self) -> bool {
        self.category.retryable()
    }
}

/// Convert a tool's typed `McpError` into an `rmcp` tool-level failure
/// (`CallToolResult::error`, not a protocol-level `ErrorData`) -- the tool
/// ran, it failed, and the caller should see the structured reason, per
/// `rmcp`'s own distinction between "tool failed" and "protocol failed".
impl From<McpError> for rmcp::model::CallToolResult {
    fn from(err: McpError) -> Self {
        let body = serde_json::to_string(&err).unwrap_or_else(|_| {
            format!(
                "{{\"category\":\"internal\",\"message\":{:?}}}",
                err.message
            )
        });
        rmcp::model::CallToolResult::error(vec![rmcp::model::Content::text(body)])
    }
}
