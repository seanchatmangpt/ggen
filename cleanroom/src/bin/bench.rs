//! Cleanroom performance benchmark

use cleanroom::{CleanroomConfig, run};
use std::time::{Duration, Instant};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🧪 Cleanroom Performance Benchmark");
    println!("===================================");

    // Benchmark 1: Simple command execution
    println!("\n📊 Benchmark 1: Simple Command Execution");
    let start = Instant::now();
    let result = run(["echo", "hello world"])?;
    let execution_time = start.elapsed();
    println!("✅ Command executed in {:?}", execution_time);
    println!("✅ Exit code: {}", result.exit_code);
    println!("✅ Output: {}", result.stdout.trim());

    // Benchmark 2: Multiple command execution
    println!("\n📊 Benchmark 2: Multiple Command Execution");
    let start = Instant::now();

    let commands = vec![["echo", "test1"], ["echo", "test2"], ["echo", "test3"]];

    for cmd in commands {
        let result = run(cmd)?;
        println!(
            "  Command '{}' completed with exit code {}",
            result.stdout.trim(),
            result.exit_code
        );
    }

    let multiple_time = start.elapsed();
    println!("✅ Multiple commands executed in {:?}", multiple_time);

    // Benchmark 3: Configuration loading
    println!("\n📊 Benchmark 3: Configuration Loading");
    let start = Instant::now();
    let _config = CleanroomConfig::default();
    let config_time = start.elapsed();
    println!("✅ Configuration loaded in {:?}", config_time);

    // Summary
    println!("\n📈 Performance Summary:");
    println!("  Simple command execution: {:?}", execution_time);
    println!("  Multiple command execution: {:?}", multiple_time);
    println!("  Configuration loading: {:?}", config_time);

    let total_time = execution_time + multiple_time + config_time;
    println!("\n🏆 Total benchmark time: {:?}", total_time);

    // SLO validation
    if total_time < Duration::from_secs(5) {
        println!("✅ SLO met: Total time < 5s");
    } else {
        println!("⚠️  SLO warning: Total time >= 5s");
    }

    Ok(())
}
