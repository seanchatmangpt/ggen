//! `ggen-mcp` — an MCP server exposing ggen's SPARQL/frontmatter/diagnostic
//! introspection surface as tool calls.
//!
//! ggen's entire CLI is ten whole-pipeline verbs, none of which can run a
//! SPARQL query, enumerate the legal frontmatter keys, or report why a
//! template was skipped. This crate is that missing introspection surface:
//! nine tools, read-only except one, over stdio.
//!
//! Design notes that are load-bearing rather than incidental:
//!
//! - **Tools only.** `rmcp` 1.8 also supports Resources, Prompts, Sampling
//!   and Tasks, and none of them are used here. Every friction point this
//!   crate addresses is a question an agent asks mid-session, not something
//!   it browses or subscribes to, and tool-call support is the one part of
//!   the protocol every MCP client implements today.
//! - **Read/write split by tool, never by flag.** `ggen_sync_dry_run` is
//!   `readOnlyHint`; `ggen_write_apply` is `destructiveHint` and demands an
//!   explicit `confirm: true`. A client can gate on the annotation alone.
//! - **Pure functions, thin adapters.** Every tool's logic is a plain
//!   `fn(&Params) -> Result<T, McpError>` in `tools::*`, independently
//!   testable without an rmcp context; the dispatch layer below only
//!   deserializes and serializes.

pub mod error;
pub mod limits;
pub mod project_root;
pub mod selfplay;
pub mod tools;

use std::sync::Arc;

use rmcp::{model::*, service::RequestContext, ErrorData as McpError, RoleServer, ServerHandler};

/// Build an rmcp `Tool` from a name, description, and schemars-derived
/// input schema. Mirrors `ggen-lsp`'s own MCP server construction.
fn make_tool(
    name: &'static str, description: &'static str, schema: serde_json::Value,
    annotations: ToolAnnotations,
) -> Tool {
    // Runs once, at server startup, over a schemars-derived schema for a
    // concrete Rust params type -- a non-object schema here is a real
    // regression in that type, not an input-dependent runtime condition.
    // Fail loudly at startup rather than silently registering a tool with
    // an empty `{}` input schema (which `unwrap_or_default` would do,
    // making the tool accept -- or reject -- arguments with no real
    // validation).
    let object = schema.as_object().cloned().unwrap_or_else(|| {
        panic!("tool {name:?}'s schemars-derived schema must be a JSON object, got {schema:?}")
    });
    let mut tool = Tool::new(name, description, Arc::new(object));
    tool.annotations = Some(annotations);
    tool
}

fn read_only(title: &'static str) -> ToolAnnotations {
    ToolAnnotations::with_title(title)
        .read_only(true)
        .destructive(false)
}

fn destructive(title: &'static str) -> ToolAnnotations {
    ToolAnnotations::with_title(title)
        .read_only(false)
        .destructive(true)
}

/// Register a tool: name, description, params type, annotations.
macro_rules! tool_defs {
    ($( $name:literal => ($params:ty, $ann:expr, $desc:literal) ),* $(,)?) => {
        fn tool_list() -> Vec<Tool> {
            vec![$(
                make_tool(
                    $name,
                    $desc,
                    // Runs once at server startup over a concrete, unchanging
                    // Rust type's schemars derive -- a serialization failure
                    // here is a real regression in that type. Fail loudly
                    // rather than silently registering a tool with `{}` (no
                    // real input schema) via `unwrap_or_default`.
                    serde_json::to_value(schemars::schema_for!($params))
                        .unwrap_or_else(|e| panic!(
                            "tool {:?}'s schemars schema for {} must serialize to JSON: {e}",
                            $name, stringify!($params)
                        )),
                    $ann,
                )
            ),*]
        }
    };
}

tool_defs! {
    "ggen_query_preview" => (
        tools::query_preview::QueryPreviewParams,
        read_only("Preview a SPARQL query"),
        "Execute an ad-hoc SPARQL query against a ggen project's loaded graph and \
         report the TRUE row count before any truncation. A zero-row SELECT is not an \
         error -- it is reported loudly (ok:true, row_count:0) so a mandatory triple \
         pattern that matches nothing is never silently mistaken for success. Call \
         this before relying on a query in a template."
    ),
    "ggen_config_classify" => (
        tools::config_classify::ConfigClassifyParams,
        read_only("Classify ggen.toml schema"),
        "Report which of ggen.toml's two incompatible schemas (declarative-rules vs \
         frontmatter) a project will be parsed as. Reads exactly one file: runs no \
         pipeline stage, resolves no packs, clones no git repositories."
    ),
    "ggen_frontmatter_schema" => (
        tools::frontmatter_schema::FrontmatterSchemaParams,
        read_only("List template frontmatter keys"),
        "Enumerate every legal template frontmatter key (derived live from the engine's \
         own schema, so it cannot drift), plus the projection-mode rule that decides \
         whether a template writes ONE file or one file PER ROW -- which is control \
         flow, not schema, and is therefore invisible in the key list alone."
    ),
    "ggen_frontmatter_lint" => (
        tools::frontmatter_lint::FrontmatterLintParams,
        read_only("Lint one template"),
        "Parse one template and report its SPARQL-projected variables, its \
         Tera-consumed variables, and the diff -- catching both an unparseable Tera \
         body and a template that consumes a variable its SELECT never binds, without \
         running the pipeline."
    ),
    "ggen_sync_dry_run" => (
        tools::sync_dry_run::SyncDryRunParams,
        read_only("Preview what sync would write"),
        "Run the pipeline in dry-run mode (writes nothing) and report what would be \
         written, with TYPED skip reasons -- so 'skipped because the when: guard was \
         false' and 'skipped because the query returned zero rows' are distinguishable \
         rather than both being opaque strings."
    ),
    "ggen_check_project" => (
        tools::check_project::CheckProjectParams,
        read_only("Run project diagnostics"),
        "Run ggen's cross-surface diagnostic pass over a whole project, surfacing the \
         GGEN-*/E00xx codes (unbound template variable, output-path escape, competing \
         authority, SELECT * blindspot, ...)."
    ),
    "ggen_rule_graph" => (
        tools::rule_graph::RuleGraphParams,
        read_only("Map rules to queries, templates, outputs"),
        "Expose the rule -> query -> template -> output wiring for a project, including \
         each rule's SELECT variables. The orientation map for an unfamiliar project. \
         Declarative-rules schema only; a frontmatter project has no such rules."
    ),
    "ggen_capability_status" => (
        tools::capability_status::CapabilityStatusParams,
        read_only("Report inert ggen.toml fields"),
        "Report ggen.toml fields that are structurally accepted but not implemented \
         (TemplateSource::Pack/Git/Package), AND whether this specific project already \
         depends on one -- so an author learns up front instead of at sync time."
    ),
    "ggen_write_apply" => (
        tools::write_apply::WriteApplyParams,
        destructive("Apply sync (WRITES FILES)"),
        "Run a real sync and WRITE its outputs. Requires confirm:true. Returns the \
         BLAKE3 of each file read back after writing, as evidence of what actually \
         landed. Run ggen_sync_dry_run first."
    ),
}

/// The ggen-mcp server.
#[derive(Clone)]
pub struct GgenMcpServer {
    tools: Arc<Vec<Tool>>,
}

impl Default for GgenMcpServer {
    fn default() -> Self {
        Self::new()
    }
}

impl GgenMcpServer {
    #[must_use]
    pub fn new() -> Self {
        Self {
            tools: Arc::new(tool_list()),
        }
    }

    /// Serve over stdio. Bare invocation of the `ggen-mcp` binary lands
    /// here -- clients spawn it with no arguments and speak JSON-RPC on its
    /// stdio immediately, so nothing may write to stdout before this point
    /// (see `bin/ggen-mcp.rs`'s stderr-only tracing setup).
    ///
    /// # Errors
    /// Returns an error if the stdio transport fails to initialize or serve.
    pub async fn start_stdio() -> anyhow::Result<()> {
        use rmcp::ServiceExt;
        let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
        let running = Self::new().serve((stdin, stdout)).await?;
        running.waiting().await?;
        Ok(())
    }
}

/// Deserialize `arguments` into `P`, run `f`, and render either side into a
/// `CallToolResult`. One definition, used by every tool, so error shaping
/// can never drift per-tool.
fn dispatch<P, T>(
    arguments: Option<serde_json::Map<String, serde_json::Value>>,
    f: fn(&P) -> Result<T, crate::error::McpError>,
) -> CallToolResult
where
    P: serde::de::DeserializeOwned,
    T: serde::Serialize,
{
    let value = serde_json::Value::Object(arguments.unwrap_or_default());
    let params: P = match serde_json::from_value(value) {
        Ok(p) => p,
        Err(e) => {
            return crate::error::McpError::new(
                crate::error::ErrorCategory::Internal,
                format!("invalid params: {e}"),
            )
            .into()
        }
    };
    match f(&params) {
        Ok(result) => match serde_json::to_string_pretty(&result) {
            Ok(text) => CallToolResult::success(vec![Content::text(text)]),
            // Per-request, not startup-time -- unlike the schema-building
            // sites above, a panic here would kill the whole server on one
            // bad response instead of surfacing to just this caller. Route
            // through the same typed-error shape every other failure uses
            // (`error.rs`'s `From<McpError> for CallToolResult`) rather than
            // `unwrap_or_default`, which would report `CallToolResult::
            // success` with an empty body -- indistinguishable from a tool
            // that genuinely succeeded with nothing to report.
            Err(e) => crate::error::McpError::new(
                crate::error::ErrorCategory::Internal,
                format!("tool result failed to serialize: {e}"),
            )
            .into(),
        },
        Err(e) => e.into(),
    }
}

impl ServerHandler for GgenMcpServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo::new(ServerCapabilities::builder().enable_tools().build())
            .with_protocol_version(ProtocolVersion::V_2024_11_05)
            .with_server_info(Implementation::new("ggen-mcp", env!("CARGO_PKG_VERSION")))
            .with_instructions(
                "ggen-mcp: introspection tools for authoring ggen projects (RDF ontology \
                 + SPARQL + Tera code generation).\n\n\
                 Suggested order when working in an unfamiliar project: \
                 ggen_config_classify (which schema?) -> ggen_rule_graph (what exists?) \
                 -> ggen_query_preview (does my query return rows?) -> \
                 ggen_frontmatter_lint (are my template vars bound?) -> \
                 ggen_sync_dry_run (what would be written?) -> ggen_write_apply.\n\n\
                 ggen_query_preview is the one to reach for first when a generated file \
                 comes out empty or wrong: it reports the true row count, including \
                 zero, before you commit to a query. All tools are read-only except \
                 ggen_write_apply, which requires confirm:true.",
            )
    }

    fn list_tools(
        &self, _request: Option<PaginatedRequestParams>, _ctx: RequestContext<RoleServer>,
    ) -> impl std::future::Future<Output = Result<ListToolsResult, McpError>> + Send + '_ {
        std::future::ready(Ok(ListToolsResult {
            tools: (*self.tools).clone(),
            next_cursor: None,
            meta: None,
        }))
    }

    fn call_tool(
        &self,
        CallToolRequestParams {
            name, arguments, ..
        }: CallToolRequestParams,
        _ctx: RequestContext<RoleServer>,
    ) -> impl std::future::Future<Output = Result<CallToolResult, McpError>> + Send + '_ {
        std::future::ready(match name.as_ref() {
            "ggen_query_preview" => Ok(dispatch(arguments, tools::query_preview::query_preview)),
            "ggen_config_classify" => {
                Ok(dispatch(arguments, tools::config_classify::config_classify))
            }
            "ggen_frontmatter_schema" => Ok(dispatch(
                arguments,
                tools::frontmatter_schema::frontmatter_schema,
            )),
            "ggen_frontmatter_lint" => Ok(dispatch(
                arguments,
                tools::frontmatter_lint::frontmatter_lint,
            )),
            "ggen_sync_dry_run" => Ok(dispatch(arguments, tools::sync_dry_run::sync_dry_run)),
            "ggen_check_project" => Ok(dispatch(arguments, tools::check_project::check_project)),
            "ggen_rule_graph" => Ok(dispatch(arguments, tools::rule_graph::rule_graph)),
            "ggen_capability_status" => Ok(dispatch(
                arguments,
                tools::capability_status::capability_status,
            )),
            "ggen_write_apply" => Ok(dispatch(arguments, tools::write_apply::write_apply)),
            other => Err(McpError::invalid_params(
                format!("unknown tool: {other}"),
                None,
            )),
        })
    }
}
