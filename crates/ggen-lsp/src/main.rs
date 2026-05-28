use ggen_lsp::{run_stdio, run_http};
use std::env;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt().init();

    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: ggen-lsp [stdio|http] [--port PORT]");
        return Ok(());
    }

    match args[1].as_str() {
        "stdio" => {
            run_stdio().await?;
        }
        "http" => {
            let port = args
                .iter()
                .position(|arg| arg == "--port")
                .and_then(|i| args.get(i + 1))
                .and_then(|s| s.parse::<u16>().ok())
                .unwrap_or(9999);

            run_http(port).await?;
        }
        _ => eprintln!("Unknown transport: {}", args[1]),
    }

    Ok(())
}
