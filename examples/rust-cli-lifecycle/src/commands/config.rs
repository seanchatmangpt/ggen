//! Configuration commands

use crate::ConfigAction;
use anyhow::Result;
use colored::*;

pub async fn handle(action: ConfigAction, verbose: bool) -> Result<()> {
    if verbose {
        println!("{}", "→ Config command handler".bright_black());
    }

    match action {
        ConfigAction::Show => show().await,
        ConfigAction::Set { key, value } => set(key, value).await,
    }
}

async fn show() -> Result<()> {
    println!("{} Configuration", "⚙".to_string().bold());
    println!();
    println!(
        "  {} = {}",
        "data_dir".bright_white(),
        "~/.taskmgr".bright_cyan()
    );
    println!(
        "  {} = {}",
        "default_priority".bright_white(),
        "medium".yellow()
    );
    println!("  {} = {}", "color_output".bright_white(), "true".green());
    println!();
    println!(
        "  {}: {}",
        "Config file".bright_black(),
        "~/.taskmgr/config.json".bright_black()
    );

    Ok(())
}

async fn set(key: String, value: String) -> Result<()> {
    println!("{} Setting configuration...", "✓".green().bold());
    println!("  {} = {}", key.bright_white(), value.bright_cyan());

    // In real implementation: save to config file
    println!();
    println!("{} Configuration updated", "✓".green().bold());

    Ok(())
}
