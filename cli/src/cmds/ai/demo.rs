//! Run the AI template demo

use ggen_utils::error::Result;

pub async fn run() -> Result<()> {
    println!("Running AI template generation demo...");

    // Run the demo example
    let demo_result = std::process::Command::new("cargo")
        .args(&["run", "--example", "iterative_template_improvement"])
        .current_dir("/Users/sac/ggen")
        .output();

    match demo_result {
        Ok(output) => {
            if output.status.success() {
                println!("Demo completed successfully!");
                println!("{}", String::from_utf8_lossy(&output.stdout));
            } else {
                println!("Demo failed:");
                println!("{}", String::from_utf8_lossy(&output.stderr));
            }
        }
        Err(e) => {
            println!("Failed to run demo: {}", e);
        }
    }

    Ok(())
}

