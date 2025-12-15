use rig_mcp_integration::{prelude::*, example::run_example};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    println!("🚀 Rig MCP Integration Example");
    println!("==============================");

    run_example().await?;

    println!("✅ Example completed successfully!");

    Ok(())
}
