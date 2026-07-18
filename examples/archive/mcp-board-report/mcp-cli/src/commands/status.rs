//! Status command
//!
//! Show current MCP+ system status.

use super::Context;
use anyhow::Result;
use colored::Colorize;
use std::fs;

/// Show system status
pub fn show(ctx: &Context) -> Result<()> {
    // Global status
    let global_disabled = ctx.state_dir.join("global_disabled").exists();

    println!("  Kill Switch Status");
    println!("  {}", "─".repeat(40));
    println!();

    if global_disabled {
        println!("  Global:       {}", "DISABLED".red().bold());
    } else {
        println!("  Global:       {}", "ENABLED".green());
    }

    // Families
    println!();
    println!("  Disabled Families:");
    let families_dir = ctx.state_dir.join("families");
    let mut found_families = false;

    if families_dir.exists() {
        for entry in fs::read_dir(&families_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.to_string_lossy().ends_with("_disabled") {
                let family = path
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("unknown")
                    .trim_end_matches("_disabled");
                println!("    - {}", family.red());
                found_families = true;
            }
        }
    }

    if !found_families {
        println!("    {}", "(none)".green());
    }

    // Capabilities
    println!();
    println!("  Disabled Capabilities:");
    let caps_dir = ctx.state_dir.join("capabilities");
    let mut found_caps = false;

    if caps_dir.exists() {
        for entry in fs::read_dir(&caps_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.to_string_lossy().ends_with("_disabled") {
                let cap = path
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("unknown")
                    .trim_end_matches("_disabled");
                println!("    - {}", cap.red());
                found_caps = true;
            }
        }
    }

    if !found_caps {
        println!("    {}", "(none)".green());
    }

    // Epochs
    println!();
    println!("  Revoked Epochs:");
    let epochs_dir = ctx.state_dir.join("epochs");
    let mut found_epochs = false;

    if epochs_dir.exists() {
        for entry in fs::read_dir(&epochs_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.to_string_lossy().ends_with("_status") {
                if let Ok(content) = fs::read_to_string(&path) {
                    if content.trim() == "revoked" {
                        let epoch = path
                            .file_stem()
                            .and_then(|s| s.to_str())
                            .unwrap_or("unknown")
                            .trim_end_matches("_status");
                        println!("    - {}", epoch.red());
                        found_epochs = true;
                    }
                }
            }
        }
    }

    if !found_epochs {
        println!("    {}", "(none)".green());
    }

    // Evidence summary
    println!();
    println!("  Evidence Summary");
    println!("  {}", "─".repeat(40));

    let mut bundle_count = 0;
    let mut receipt_count = 0;
    let mut verification_count = 0;

    if ctx.evidence_dir.exists() {
        for entry in fs::read_dir(&ctx.evidence_dir)? {
            let entry = entry?;
            let name = entry.file_name().to_string_lossy().to_string();

            if name.starts_with("bundle-") || name.starts_with("test-bundle-") {
                bundle_count += 1;
            } else if name.starts_with("rcpt-") {
                receipt_count += 1;
            } else if name.starts_with("verification-") {
                verification_count += 1;
            }
        }
    }

    println!();
    println!("  Bundles:       {}", bundle_count);
    println!("  Receipts:      {}", receipt_count);
    println!("  Verifications: {}", verification_count);

    // Active epoch
    println!();
    println!("  Active Epoch");
    println!("  {}", "─".repeat(40));

    let active_file = epochs_dir.join("active");
    if active_file.exists() {
        let active_epoch = fs::read_to_string(&active_file)?;
        println!();
        println!("  ID: {}", active_epoch.trim().cyan());
    } else {
        println!();
        println!("  {}", "(no active epoch)".yellow());
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_context() -> (Context, TempDir) {
        let tmp = TempDir::new().unwrap();
        let base = tmp.path().to_path_buf();
        let ctx = Context {
            base_dir: base.clone(),
            state_dir: base.join("state"),
            evidence_dir: base.join("evidence"),
            logs_dir: base.join("logs"),
        };
        fs::create_dir_all(&ctx.state_dir).unwrap();
        fs::create_dir_all(&ctx.evidence_dir).unwrap();
        fs::create_dir_all(&ctx.logs_dir).unwrap();
        (ctx, tmp)
    }

    #[test]
    fn test_status_empty() {
        let (ctx, _tmp) = create_test_context();
        show(&ctx).unwrap();
    }

    #[test]
    fn test_status_with_disabled() {
        let (ctx, _tmp) = create_test_context();

        // Create some state
        fs::write(ctx.state_dir.join("global_disabled"), "true").unwrap();

        let families_dir = ctx.state_dir.join("families");
        fs::create_dir_all(&families_dir).unwrap();
        fs::write(families_dir.join("test-family_disabled"), "true").unwrap();

        show(&ctx).unwrap();
    }
}
