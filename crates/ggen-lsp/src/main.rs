use ggen_lsp::run_stdio;
use std::env;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt().init();

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
