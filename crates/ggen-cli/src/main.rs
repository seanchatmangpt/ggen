use ggen_cli_lib;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    ggen_cli_lib::cli_match().await
        .map_err(|e| anyhow::anyhow!("CLI error: {}", e))
}
