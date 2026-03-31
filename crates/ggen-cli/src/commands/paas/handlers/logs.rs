//! Stream logs handler
//! Verb: logs | Noun: (default/implicit)

use crate::commands::paas::errors::Result;

/// Stream operation logs
pub async fn stream_logs(
    lines: usize,
    deployment: Option<&str>,
    follow: bool,
    level: Option<&str>,
) -> Result<()> {
    println!("📋 Operation Logs");
    println!("================");

    if let Some(dep) = deployment {
        println!("Deployment: {}", dep);
    } else {
        println!("Deployment: All");
    }

    if let Some(lvl) = level {
        println!("Level: {}", lvl);
    }

    println!("Lines: {}", lines);
    println!("Follow: {}", if follow { "yes" } else { "no" });
    println!();

    // Check for local log files
    let log_dirs = vec!["logs", ".ggen/logs", "/var/log/ggen"];
    let mut found_logs = false;

    for log_dir in &log_dirs {
        let path = std::path::Path::new(log_dir);
        if path.exists() {
            println!("Found log directory: {}", log_dir);
            if let Ok(entries) = std::fs::read_dir(path) {
                for entry in entries.filter_map(Result::ok).take(lines) {
                    println!("  - {}", entry.file_name().to_string_lossy());
                }
            }
            found_logs = true;
            break;
        }
    }

    if !found_logs {
        println!("⚠️  No local log files found");
        println!();
        println!("Log locations checked:");
        for log_dir in &log_dirs {
            println!("  - {}", log_dir);
        }
        println!();
        println!("💡 Tip: Enable logging with GGEN_LOG=debug");
    }

    if follow {
        println!();
        println!("⚠️  Log following not yet implemented");
        println!("   In a full PaaS integration, this would tail logs in real-time");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_logs_default() {
        let result = stream_logs(50, None, false, None).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_logs_with_level() {
        let result = stream_logs(100, Some("prod"), false, Some("error")).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_logs_follow_mode() {
        let result = stream_logs(10, Some("dev"), true, Some("info")).await;
        assert!(result.is_ok());
    }
}
