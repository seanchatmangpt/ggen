//! MCP server — exposes receipt operations as LLM tool-call targets.
//!
//! Compile and run with the `mcp` feature:
//!
//! ```bash
//! cargo run --bin mcp_server --features mcp
//! ```
//!
//! The server speaks the Model Context Protocol over stdio and is immediately
//! consumable by any MCP-capable LLM agent without extra glue code.

#[cfg(feature = "mcp")]
mod server {
    use rmcp::{
        handler::server::{router::tool::ToolRouter, wrapper::Parameters},
        model::CallToolResult,
        tool, tool_handler, tool_router, ServerHandler,
    };
    use schemars::JsonSchema;
    use serde::Deserialize;

    /// Shared server state (clone-cheap; extend with connection pools, config, etc.)
    #[derive(Clone)]
    pub struct ServerState {
        tool_router: ToolRouter<Self>,
    }

    impl Default for ServerState {
        fn default() -> Self {
            Self {
                tool_router: Self::tool_router(),
            }
        }
    }

    #[tool_handler(router = self.tool_router)]
    impl ServerHandler for ServerState {}

    // -------------------------------------------------------------------------
    // Parameter types
    // -------------------------------------------------------------------------

    #[derive(Deserialize, JsonSchema)]
    pub struct EmitParams {
        /// Event type label (e.g. "build", "test", "audit-log").
        pub event_type: String,
        /// Object reference in `id:type[:qualifier]` format.
        pub object: String,
        /// Optional JSON payload string.
        #[serde(default)]
        pub payload: Option<String>,
    }

    #[derive(Deserialize, JsonSchema)]
    pub struct VerifyParams {
        /// Filesystem path to the assembled receipt JSON file.
        pub receipt_path: String,
    }

    #[derive(Deserialize, JsonSchema)]
    pub struct ShowParams {
        /// Filesystem path to the assembled receipt JSON file.
        pub receipt_path: String,
    }

    // -------------------------------------------------------------------------
    // Tool handlers
    // -------------------------------------------------------------------------

    #[tool_router]
    impl ServerState {
        #[tool(description = "Emit an operation-event and append it to the working receipt chain.")]
        async fn emit_event(
            &self, params: Parameters<EmitParams>,
        ) -> Result<CallToolResult, rmcp::ErrorData> {
            let p = &params.0;
            let mut cmd = std::process::Command::new("affi");
            cmd.args(["emit", "--type", &p.event_type, "--object", &p.object]);
            if let Some(payload) = &p.payload {
                cmd.args(["--payload", payload.as_str()]);
            }
            match cmd.output() {
                Ok(out) => {
                    let text = String::from_utf8_lossy(&out.stdout).into_owned();
                    if out.status.success() {
                        Ok(CallToolResult::success(vec![rmcp::model::Content::text(
                            text,
                        )]))
                    } else {
                        let err = String::from_utf8_lossy(&out.stderr).into_owned();
                        Ok(CallToolResult::error(vec![rmcp::model::Content::text(
                            format!("emit failed: {err}"),
                        )]))
                    }
                }
                Err(e) => Ok(CallToolResult::error(vec![rmcp::model::Content::text(
                    format!("could not launch affi: {e}"),
                )])),
            }
        }

        #[tool(description = "Verify a receipt and return ACCEPT or REJECT verdict.")]
        async fn verify_receipt(
            &self, params: Parameters<VerifyParams>,
        ) -> Result<CallToolResult, rmcp::ErrorData> {
            let path = &params.0.receipt_path;
            match std::process::Command::new("affi")
                .args(["verify", path])
                .output()
            {
                Ok(out) => {
                    let verdict = if out.status.success() {
                        "ACCEPT"
                    } else {
                        "REJECT"
                    };
                    let detail = String::from_utf8_lossy(&out.stdout).into_owned();
                    Ok(CallToolResult::success(vec![rmcp::model::Content::text(
                        format!("{verdict}\n{detail}"),
                    )]))
                }
                Err(e) => Ok(CallToolResult::error(vec![rmcp::model::Content::text(
                    format!("could not launch affi: {e}"),
                )])),
            }
        }

        #[tool(description = "Show a human-readable dump of a receipt chain.")]
        async fn show_receipt(
            &self, params: Parameters<ShowParams>,
        ) -> Result<CallToolResult, rmcp::ErrorData> {
            let path = &params.0.receipt_path;
            match std::process::Command::new("affi")
                .args(["show", path])
                .output()
            {
                Ok(out) => {
                    let text = String::from_utf8_lossy(&out.stdout).into_owned();
                    if out.status.success() {
                        Ok(CallToolResult::success(vec![rmcp::model::Content::text(
                            text,
                        )]))
                    } else {
                        let err = String::from_utf8_lossy(&out.stderr).into_owned();
                        Ok(CallToolResult::error(vec![rmcp::model::Content::text(
                            format!("show failed: {err}"),
                        )]))
                    }
                }
                Err(e) => Ok(CallToolResult::error(vec![rmcp::model::Content::text(
                    format!("could not launch affi: {e}"),
                )])),
            }
        }
    }
}

#[cfg(feature = "mcp")]
#[tokio::main]
async fn main() -> anyhow::Result<()> {
    use rmcp::{transport::stdio, ServiceExt as _};
    server::ServerState::default().serve(stdio()).await?;
    Ok(())
}

#[cfg(not(feature = "mcp"))]
fn main() -> anyhow::Result<()> {
    eprintln!("mcp_server requires the `mcp` Cargo feature. Rebuild with --features mcp");
    std::process::exit(1);
}
