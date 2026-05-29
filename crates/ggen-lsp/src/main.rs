use ggen_lsp::run_stdio;
use std::env;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt().init();

    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: ggen-lsp [stdio]");
        return Ok(());
    }

    match args[1].as_str() {
        "stdio" => {
            run_stdio().await?;
        }
        _ => eprintln!("Unknown transport: {}", args[1]),
    }

    Ok(())
}
