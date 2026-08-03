//! `ggen-mcp` binary entry point.
//!
//! Bare invocation defaults to serving over stdio -- MCP clients spawn this
//! binary with no arguments and speak JSON-RPC on its stdio immediately, so
//! stdout must never carry anything but protocol frames. Tracing is
//! installed on stderr only, before anything else can log, mirroring
//! `claude-code-config-lsp`'s "stdout is sacred" convention (that project's
//! own regression test: `tests/stdout_is_lsp_frames_only.rs`).

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt().with_writer(std::io::stderr).with_target(false).init();
    ggen_mcp::GgenMcpServer::start_stdio().await
}
