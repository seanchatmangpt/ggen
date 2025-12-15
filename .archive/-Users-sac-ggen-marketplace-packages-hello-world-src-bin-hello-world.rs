use hello_world_utils::{examples, HelloConfig, HelloWorld};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    println!("🌟 Hello World Utilities Demo");
    println!("==============================");
    println!();

    // Run basic example
    println!("📝 Running basic example:");
    examples::run_basic_example()?;

    println!();
    println!("✨ Running custom example:");
    examples::run_custom_example()?;

    println!();
    println!("🎉 Demo completed successfully!");

    Ok(())
}
