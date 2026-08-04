//! `ggen-mcp` — an MCP server exposing ggen's SPARQL/frontmatter/diagnostic
//! introspection surface as tool calls.
//!
//! ggen's entire CLI is ten whole-pipeline verbs, none of which can run a
//! SPARQL query, enumerate the legal frontmatter keys, or report why a
//! template was skipped. This crate is that missing introspection surface:
//! ten tools, read-only except one, over stdio.
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

pub mod bridge;
pub mod error;
pub mod limits;
pub mod project_root;
pub mod selfplay;
pub mod tools;
pub mod watcher;

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
    "ggen_pack_capabilities" => (
        tools::pack_capabilities::PackCapabilitiesParams,
        read_only("Report a pack's classes, gates, and contract predicates"),
        "Introspect ONE pack directory (ontology.ttl + optional gates/*.rq / shapes.ttl): \
         which RDF classes/individuals it declares, what each gates/*.rq admission check \
         does (its own # MESSAGE: header, when present), and whether it uses the \
         expectsBinding/producesShape-style 'contract' predicate convention (matched \
         generically by predicate local name, never by hardcoding one pack's namespace -- \
         most packs will report none found, which is expected, not an error)."
    ),
    "ggen_write_apply" => (
        tools::write_apply::WriteApplyParams,
        destructive("Apply sync (WRITES FILES)"),
        "Run a real sync and WRITE its outputs. Requires confirm:true AND \
         expected_graph_hash (the graph_hash field from a real, immediately-prior \
         ggen_sync_dry_run call against this same root) -- refuses if the hash \
         does not match the project's current graph state. Returns the \
         BLAKE3 of each file read back after writing, as evidence of what actually \
         landed. Run ggen_sync_dry_run first."
    ),
}

/// The ggen-mcp server.
#[derive(Clone)]
pub struct GgenMcpServer {
    tools: Arc<Vec<Tool>>,
    /// Backing store for `GGEN-*` diagnostic push notifications (CP12's
    /// `crate::bridge`). Populated by `bridge::push_diagnostics_for_root`,
    /// read by `list_resources`/`read_resource` below.
    diagnostics: crate::bridge::DiagnosticStore,
    /// Every currently-retained `Peer<RoleServer>` this server can push
    /// diagnostic notifications to (CP16). One `GgenMcpServer` clone may
    /// back more than one concurrent `serve()` call (multiple simultaneous
    /// clients, or a reconnect after a prior client disconnected) -- see
    /// `crate::bridge::PeerRegistry`'s doc comment for the full lifecycle
    /// contract.
    peers: crate::bridge::PeerRegistry,
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
            diagnostics: crate::bridge::DiagnosticStore::new(),
            peers: crate::bridge::PeerRegistry::new(),
        }
    }

    /// The diagnostic-resource store backing this server's `resources/*`
    /// surface -- exposed so a caller (production `start_stdio`'s retained
    /// peer, or a test) can push into the exact store this server's
    /// `read_resource` reads from.
    #[must_use]
    pub fn diagnostic_store(&self) -> &crate::bridge::DiagnosticStore {
        &self.diagnostics
    }

    /// The peer registry backing this server's diagnostic push surface
    /// (CP16) -- exposed so a caller can retain a `serve()` call's `Peer`
    /// into the exact registry `crate::bridge::push_diagnostics_for_root`
    /// should broadcast to.
    #[must_use]
    pub fn peer_registry(&self) -> &crate::bridge::PeerRegistry {
        &self.peers
    }

    /// Serve over stdio. Bare invocation of the `ggen-mcp` binary lands
    /// here -- clients spawn it with no arguments and speak JSON-RPC on its
    /// stdio immediately, so nothing may write to stdout before this point
    /// (see `bin/ggen-mcp.rs`'s stderr-only tracing setup).
    ///
    /// Retains the `RunningService`'s `Peer<RoleServer>` in this server's
    /// `crate::bridge::PeerRegistry` past the initial `serve()` call (CP12,
    /// extended CP16) so an independently-spawned tokio task could call
    /// `crate::bridge::push_diagnostics_for_root` later and reach this
    /// connection. `start_stdio` is a single stdio session started once per
    /// process, so the registry it feeds is never observed with more than
    /// one live peer in production today -- the multi-peer path
    /// (`PeerRegistry::add` called more than once) is real, tested code in
    /// `crate::bridge`'s own test suite, but it is exercised over an
    /// in-process duplex transport there, not from this fn, since a single
    /// `ggen-mcp` process is spawned per stdio client by design (one binary
    /// invocation, one transport).
    ///
    /// CP15: also spawns a real filesystem watcher (`crate::watcher`) on the
    /// process's current working directory, so a real file change while
    /// this process is running fires a real `push_diagnostics_for_root`
    /// call against the exact `PeerRegistry`/`DiagnosticStore` this stdio
    /// session just retained -- not merely a function callable from a test.
    /// See `crate::watcher`'s module doc for why the watcher lives here (in
    /// `ggen-mcp`) rather than as a call from `ggen-lsp`'s
    /// `did_save`/`did_change` handlers: the two are separate processes
    /// with no existing IPC, and `crate::bridge` was already built as a
    /// stateless, file-reading adapter rather than a live-server bridge.
    ///
    /// The watcher is best-effort: if it fails to start (e.g. `getcwd`
    /// fails, or the cwd is not watchable), that is logged and does not
    /// prevent the server from serving its request/response tool surface --
    /// the watcher is an auxiliary push capability, not a prerequisite for
    /// `ggen-mcp`'s primary pull-based contract.
    ///
    /// # Errors
    /// Returns an error if the stdio transport fails to initialize or serve.
    pub async fn start_stdio() -> anyhow::Result<()> {
        use rmcp::ServiceExt;
        let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
        let server = Self::new();
        let peers = server.peers.clone();
        let diagnostics = server.diagnostics.clone();
        let running = server.serve((stdin, stdout)).await?;
        peers.add(running.peer().clone()).await;

        match std::env::current_dir() {
            Ok(root) => {
                if let Err(e) =
                    crate::watcher::spawn_root_watcher(root, peers.clone(), diagnostics)
                {
                    tracing::warn!(error = %e, "ggen-mcp: failed to start file watcher, serving without live diagnostic pushes");
                }
            }
            Err(e) => {
                tracing::warn!(error = %e, "ggen-mcp: could not resolve cwd, serving without live diagnostic pushes");
            }
        }

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
        ServerInfo::new(
            ServerCapabilities::builder()
                .enable_tools()
                // CP12: advertise resources (+ subscribe) so a spec-compliant
                // client expects `notifications/resources/updated` pushes for
                // GGEN-* diagnostics -- see `crate::bridge`.
                .enable_resources()
                .enable_resources_subscribe()
                .build(),
        )
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

    /// List currently-pushed `GGEN-*` diagnostic resources (CP12). Empty
    /// until `crate::bridge::push_diagnostics_for_root` has run at least
    /// once against this server's `diagnostic_store()`.
    fn list_resources(
        &self, _request: Option<PaginatedRequestParams>, _ctx: RequestContext<RoleServer>,
    ) -> impl std::future::Future<Output = Result<ListResourcesResult, McpError>> + Send + '_
    {
        let store = self.diagnostics.clone();
        async move {
            Ok(ListResourcesResult {
                resources: crate::bridge::list_resources(&store).await,
                next_cursor: None,
                meta: None,
            })
        }
    }

    /// Read one pushed diagnostic's real content back by URI (CP12). The
    /// counterpart to `notify_resource_updated`'s bare URI: a client that
    /// received the push follows up here for the actual diagnostic.
    fn read_resource(
        &self, request: ReadResourceRequestParams, _ctx: RequestContext<RoleServer>,
    ) -> impl std::future::Future<Output = Result<ReadResourceResult, McpError>> + Send + '_ {
        let store = self.diagnostics.clone();
        let read_span = tracing::info_span!(
            "mcp.resource.read",
            "operation.name" = "mcp.resource.read",
            "operation.type" = "mcp",
            "mcp.resource.uri" = %request.uri,
            "mcp.resource.result" = tracing::field::Empty,
        );
        async move {
            use tracing::Instrument as _;
            async move {
                let Some(diag) = store.get(&request.uri).await else {
                    tracing::Span::current().record("mcp.resource.result", "not_found");
                    tracing::warn!(uri = %request.uri, "mcp.resource.read: no such diagnostic resource");
                    return Err(McpError::resource_not_found(
                        format!("no such diagnostic resource: {}", request.uri),
                        None,
                    ));
                };
                let text = serde_json::to_string_pretty(&diag).map_err(|e| {
                    tracing::Span::current().record("mcp.resource.result", "serialize_error");
                    McpError::internal_error(format!("diagnostic serialize failed: {e}"), None)
                })?;
                tracing::Span::current().record("mcp.resource.result", "success");
                tracing::info!(uri = %request.uri, "mcp.resource.read: diagnostic content returned to client");
                Ok(ReadResourceResult::new(vec![ResourceContents::text(
                    text,
                    request.uri,
                )]))
            }
            .instrument(read_span)
            .await
        }
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
            "ggen_pack_capabilities" => Ok(dispatch(
                arguments,
                tools::pack_capabilities::pack_capabilities,
            )),
            "ggen_write_apply" => Ok(dispatch(arguments, tools::write_apply::write_apply)),
            other => Err(McpError::invalid_params(
                format!("unknown tool: {other}"),
                None,
            )),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// CP20: a real `resources/read` round trip against the *real*
    /// `GgenMcpServer::read_resource` (not `bridge.rs`'s in-module
    /// `BridgeTestServer` mock, which has its own minimal `read_resource`
    /// and never exercises this file's `mcp.resource.read` span). Proves
    /// the span added to `read_resource` above fires on both the
    /// found-and-served path and the not-found path.
    #[tokio::test]
    async fn read_resource_round_trip_against_real_server_emits_otel_span() -> anyhow::Result<()>
    {
        let _ = tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_test_writer()
            .try_init();

        use rmcp::ServiceExt;

        let server = GgenMcpServer::new();
        server
            .diagnostic_store()
            .insert(
                "ggen-diagnostic://demo.tera#GGEN-TPL-001-0".to_string(),
                crate::bridge::PushedDiagnostic {
                    file: "demo.tera".to_string(),
                    code: "GGEN-TPL-001".to_string(),
                    message: "unbound var `title`".to_string(),
                    range: lsp_max::lsp_types::Range::default(),
                },
            )
            .await;

        let (server_transport, client_transport) = tokio::io::duplex(8192);
        let server_task = tokio::spawn(async move {
            let running = server.serve(server_transport).await?;
            running.waiting().await?;
            anyhow::Ok(())
        });

        let client = ().serve(client_transport).await?;

        // Real found path -- exercises the `success` branch of the span.
        let read = client
            .read_resource(ReadResourceRequestParams::new(
                "ggen-diagnostic://demo.tera#GGEN-TPL-001-0".to_string(),
            ))
            .await?;
        let ResourceContents::TextResourceContents { text, .. } = &read.contents[0] else {
            panic!("expected text resource contents");
        };
        assert!(text.contains("GGEN-TPL-001"));

        // Real not-found path -- exercises the `not_found` branch of the
        // same span with a different `mcp.resource.result` value.
        let missing = client
            .read_resource(ReadResourceRequestParams::new(
                "ggen-diagnostic://does-not-exist#X-0".to_string(),
            ))
            .await;
        assert!(missing.is_err(), "unknown uri must be a real error, not Ok");

        drop(client);
        server_task.await??;
        Ok(())
    }
}
