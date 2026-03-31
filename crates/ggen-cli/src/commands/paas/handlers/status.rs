//! Show status handler
//! Verb: status | Noun: (default/implicit)

use crate::commands::paas::errors::Result;
use std::path::Path;

/// Show deployment or system status
pub async fn show_status(environment: Option<&str>, detailed: bool) -> Result<()> {
    println!("🔍 PaaS System Status");
    println!("==================");

    // Show environment filter
    if let Some(env) = environment {
        println!("Environment: {}", env);
    } else {
        println!("Environment: All environments");
    }

    println!();

    // Check for paas.toml configuration
    let config_path = Path::new("paas.toml");
    if config_path.exists() {
        println!("✅ Configuration: paas.toml found");
        if detailed {
            if let Ok(content) = std::fs::read_to_string(config_path) {
                // Count key sections
                let sections = content.lines().filter(|l| l.starts_with('[')).count();
                println!("   Sections: {}", sections);
            }
        }
    } else {
        println!("⚠️  Configuration: paas.toml not found (run 'ggen paas init' first)");
    }

    // Check for .specify directory
    let spec_path = Path::new(".specify");
    if spec_path.exists() {
        let spec_count = std::fs::read_dir(spec_path)
            .map(|entries| entries.filter_map(Result::ok).count())
            .unwrap_or(0);

        println!("✅ Specifications: .specify/ ({} files)", spec_count);

        if detailed {
            if let Ok(entries) = std::fs::read_dir(spec_path) {
                for entry in entries.filter_map(Result::ok).take(5) {
                    println!("   - {}", entry.file_name().to_string_lossy());
                }
                if spec_count > 5 {
                    println!("   ... and {} more", spec_count - 5);
                }
            }
        }
    } else {
        println!("⚠️  Specifications: .specify/ not found");
    }

    // Check git submodules status
    if let Ok(output) = std::process::Command::new("git")
        .args(&["submodule", "status"])
        .output()
    {
        let status_str = String::from_utf8_lossy(&output.stdout);
        if !status_str.trim().is_empty() {
            let submodules: Vec<&str> = status_str.lines().collect();
            println!("✅ Submodules: {} configured", submodules.len());

            if detailed {
                for sub in &submodules {
                    let clean = sub.starts_with(' ');
                    let status = if clean { "✓" } else { "⚠️" };
                    println!("   {} {}", status, sub.trim_start());
                }
            }
        } else {
            println!("⚠️  Submodules: None configured");
        }
    }

    println!();
    println!("Status check complete.");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_status_no_environment() {
        let result = show_status(None, false).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_status_with_environment() {
        let result = show_status(Some("staging"), true).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_status_with_missing_config() {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        std::env::set_current_dir(temp_dir.path()).expect("Failed to change dir");

        let result = show_status(None, false).await;
        assert!(result.is_ok());

        std::env::set_current_dir("..").expect("Failed to restore dir");
    }
}
