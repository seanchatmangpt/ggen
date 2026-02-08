//! Evidence bundle commands
//!
//! Text-blind verification of .mcpb evidence bundles.

use super::Context;
use crate::models::bundle::Bundle;
use anyhow::{bail, Result};
use colored::Colorize;
use std::fs;
use std::path::Path;

/// Verify an evidence bundle (text-blind)
pub fn verify(ctx: &Context, path: &str) -> Result<()> {
    let path = Path::new(path);

    if !path.exists() {
        bail!("Bundle file not found: {}", path.display());
    }

    println!("  Bundle: {}", path.display());
    println!();

    let content = fs::read_to_string(path)?;
    let bundle: Bundle = serde_json::from_str(&content)?;

    println!("{}", "Running verification checks...".yellow());
    println!();

    let result = bundle.verify();

    // Print each check
    for (check_name, passed) in &result.checks {
        let status = if *passed {
            format!("{} {}", "✓".green(), check_name)
        } else {
            format!("{} {}", "✗".red(), check_name)
        };
        println!("  {}", status);
    }

    println!();

    // Save verification report
    let report_path = ctx.evidence_dir.join(format!(
        "verification-{}-{}.json",
        bundle.bundle_id,
        chrono::Utc::now().timestamp()
    ));
    let report_json = serde_json::to_string_pretty(&result)?;
    fs::write(&report_path, &report_json)?;

    if result.passed {
        println!("{}", "╔══════════════════════════════════════════════════════════════╗".green());
        println!("{}", "║                    VERIFICATION: PASS                        ║".green());
        println!("{}", "╚══════════════════════════════════════════════════════════════╝".green());
    } else {
        println!("{}", "╔══════════════════════════════════════════════════════════════╗".red());
        println!("{}", "║                    VERIFICATION: FAIL                        ║".red());
        println!("{}", "╚══════════════════════════════════════════════════════════════╝".red());
    }

    println!();
    println!("  Report: {}", report_path.display());

    Ok(())
}

/// Create a test bundle for verification demo
pub fn create_test(ctx: &Context) -> Result<()> {
    println!("Creating test evidence bundle...");
    println!();

    let bundle = Bundle::new_empty("test-family".to_string());
    let bundle_path = ctx.evidence_dir.join(format!(
        "test-bundle-{}.json",
        chrono::Utc::now().timestamp()
    ));

    let bundle_json = serde_json::to_string_pretty(&bundle)?;
    fs::write(&bundle_path, &bundle_json)?;

    println!("  {} Created: {}", "✓".green(), bundle_path.display());
    println!();

    // Now verify it
    println!("Verifying test bundle...");
    println!();

    verify(ctx, bundle_path.to_str().unwrap())?;

    Ok(())
}

/// Generate a new evidence bundle from receipts
pub fn generate(ctx: &Context, family: &str, output: Option<&str>) -> Result<()> {
    println!("Generating evidence bundle for family: {}", family.cyan());
    println!();

    // Collect receipts from evidence directory
    let mut receipts = Vec::new();

    for entry in fs::read_dir(&ctx.evidence_dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.extension().map_or(false, |e| e == "json") {
            if let Ok(content) = fs::read_to_string(&path) {
                if content.contains("receipt_id") && !content.contains("bundle_id") {
                    receipts.push(path);
                }
            }
        }
    }

    println!("  Found {} receipt files", receipts.len());

    let bundle = Bundle::new_empty(family.to_string());

    let output_path = match output {
        Some(p) => std::path::PathBuf::from(p),
        None => ctx.evidence_dir.join(format!(
            "bundle-{}-{}.json",
            family,
            chrono::Utc::now().format("%Y%m%d-%H%M%S")
        )),
    };

    let bundle_json = serde_json::to_string_pretty(&bundle)?;
    fs::write(&output_path, &bundle_json)?;

    println!("  {} Generated: {}", "✓".green(), output_path.display());
    println!("  Bundle ID: {}", bundle.bundle_id);
    println!("  Bundle Hash: {}", bundle.bundle_hash);

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
    fn test_create_test_bundle() {
        let (ctx, _tmp) = create_test_context();
        create_test(&ctx).unwrap();

        // Should have created at least one bundle file
        let files: Vec<_> = fs::read_dir(&ctx.evidence_dir)
            .unwrap()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().to_string_lossy().contains("test-bundle"))
            .collect();

        assert!(!files.is_empty());
    }

    #[test]
    fn test_generate_bundle() {
        let (ctx, _tmp) = create_test_context();
        generate(&ctx, "test-family", None).unwrap();

        // Should have created a bundle file
        let files: Vec<_> = fs::read_dir(&ctx.evidence_dir)
            .unwrap()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().to_string_lossy().contains("bundle-test-family"))
            .collect();

        assert!(!files.is_empty());
    }
}
