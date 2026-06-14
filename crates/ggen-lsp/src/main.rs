use ggen_lsp::run_stdio;
use std::env;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // STDOUT carries the LSP JSON-RPC protocol (see run_stdio: Server::new uses
    // tokio stdout). Logs MUST go to STDERR or they corrupt message framing and
    // break Content-Length header parsing on the client.
    tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .init();

    let args: Vec<String> = env::args().collect();

    // stdio is the only transport editors and Claude Code use, so it is the
    // DEFAULT: a bare `ggen-lsp` invocation runs the language server over stdio,
    // matching the rust-analyzer convention (a dedicated LSP binary launched by
    // the editor/agent with no arguments). `ggen-lsp stdio` is also accepted.
    match args.get(1).map(String::as_str) {
        None | Some("stdio") => {
            run_stdio().await?;
        }
        Some(other) => {
            eprintln!(
                "ggen-lsp: unknown transport {other:?}. Usage: ggen-lsp [stdio]  (stdio is the default)"
            );
            std::process::exit(2);
        }
    }

    Ok(())
}
