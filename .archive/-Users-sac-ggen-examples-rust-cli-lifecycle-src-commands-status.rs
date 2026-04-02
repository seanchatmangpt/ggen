//! Status and reporting commands

use anyhow::Result;
use colored::*;

pub async fn show(detailed: bool) -> Result<()> {
    println!("{} System Status", "📊".to_string().bold());
    println!();

    // Show summary
    println!(
        "  Tasks: {} total ({} done, {} active)",
        "15".bright_white().bold(),
        "8".green(),
        "7".yellow()
    );
    println!("  Projects: {} active", "3".bright_cyan());

    if detailed {
        println!();
        println!("  {}", "Detailed Statistics:".bright_white().underline());
        println!("    Tasks by priority:");
        println!("      High: {}", "2".bright_red());
        println!("      Medium: {}", "5".yellow());
        println!("      Low: {}", "1".green());
        println!();
        println!("    Completion rate: {}%", "53".bright_green());
        println!("    Average time per task: {} hours", "4.2".bright_white());
    }

    // Show lifecycle info
    println!();
    println!("  {}", "ggen Lifecycle Info:".bright_black().italic());
    println!("    Built with: make.toml universal lifecycle");
    println!("    Phase tracking: .ggen/state.json");
    println!("    Last build: 2025-01-10 12:34:56");

    Ok(())
}
