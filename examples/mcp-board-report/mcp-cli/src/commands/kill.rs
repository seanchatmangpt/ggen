//! Kill switch commands
//!
//! Board-authorized emergency control for sealed operating contracts.

use super::Context;
use crate::models::receipt::Receipt;
use anyhow::{bail, Result};
use colored::Colorize;
use std::fs;
use std::io::{self, Write};

/// Disable all MCP+ operations globally
pub fn disable_global(ctx: &Context) -> Result<()> {
    println!("{}", "CRITICAL: GLOBAL KILL SWITCH ACTIVATED".red().bold());
    println!();

    let global_file = ctx.state_dir.join("global_disabled");
    fs::write(&global_file, "true")?;

    let receipt = Receipt::new("disable_global", "all");
    let receipt_path = receipt.save(&ctx.evidence_dir)?;

    println!("{}", "╔══════════════════════════════════════════════════════════════╗".red());
    println!("{}", "║              GLOBAL KILL SWITCH ACTIVATED                    ║".red());
    println!("{}", "║                                                              ║".red());
    println!("{}", "║  All MCP+ operations are now DISABLED                        ║".red());
    println!("{}", "╚══════════════════════════════════════════════════════════════╝".red());
    println!();
    println!("  Receipt: {}", receipt_path.display());
    println!("  Hash:    {}", receipt.receipt_hash);

    Ok(())
}

/// Disable a specific contract family
pub fn disable_family(ctx: &Context, family_id: &str) -> Result<()> {
    println!("Disabling family: {}", family_id.yellow());
    println!();

    let families_dir = ctx.state_dir.join("families");
    fs::create_dir_all(&families_dir)?;

    let family_file = families_dir.join(format!("{}_disabled", family_id));
    fs::write(&family_file, "true")?;

    let receipt = Receipt::new("disable_family", family_id);
    let receipt_path = receipt.save(&ctx.evidence_dir)?;

    println!("  {} Family '{}' has been {}", "✓".green(), family_id, "DISABLED".red());
    println!();
    println!("  Receipt: {}", receipt_path.display());

    Ok(())
}

/// Disable a specific capability
pub fn disable_capability(ctx: &Context, capability_id: &str) -> Result<()> {
    println!("Disabling capability: {}", capability_id.yellow());
    println!();

    let caps_dir = ctx.state_dir.join("capabilities");
    fs::create_dir_all(&caps_dir)?;

    let cap_file = caps_dir.join(format!("{}_disabled", capability_id));
    fs::write(&cap_file, "true")?;

    let receipt = Receipt::new("disable_capability", capability_id);
    let receipt_path = receipt.save(&ctx.evidence_dir)?;

    println!("  {} Capability '{}' has been {}", "✓".green(), capability_id, "DISABLED".red());
    println!();
    println!("  Receipt: {}", receipt_path.display());

    Ok(())
}

/// Revoke a signing key epoch
pub fn revoke_epoch(ctx: &Context, epoch_id: &str) -> Result<()> {
    println!("Revoking epoch: {}", epoch_id.yellow());
    println!();

    let epochs_dir = ctx.state_dir.join("epochs");
    fs::create_dir_all(&epochs_dir)?;

    let epoch_file = epochs_dir.join(format!("{}_status", epoch_id));
    fs::write(&epoch_file, "revoked")?;

    let receipt = Receipt::new("revoke_epoch", epoch_id);
    let receipt_path = receipt.save(&ctx.evidence_dir)?;

    println!("  {} Epoch '{}' has been {}", "✓".green(), epoch_id, "REVOKED".red());
    println!();
    println!("  Receipt: {}", receipt_path.display());

    Ok(())
}

/// Reset all kill switches
pub fn reset(ctx: &Context, force: bool) -> Result<()> {
    if !force {
        print!("{}", "WARNING: This will reset ALL kill switches. Type 'CONFIRM' to proceed: ".yellow());
        io::stdout().flush()?;

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        if input.trim() != "CONFIRM" {
            println!("{}", "Reset cancelled".red());
            bail!("User cancelled reset");
        }
    }

    println!("Resetting all kill switches...");
    println!();

    // Remove global disable
    let global_file = ctx.state_dir.join("global_disabled");
    if global_file.exists() {
        fs::remove_file(&global_file)?;
        println!("  {} Removed global disable", "✓".green());
    }

    // Remove family disables
    let families_dir = ctx.state_dir.join("families");
    if families_dir.exists() {
        fs::remove_dir_all(&families_dir)?;
        println!("  {} Removed family disables", "✓".green());
    }

    // Remove capability disables
    let caps_dir = ctx.state_dir.join("capabilities");
    if caps_dir.exists() {
        fs::remove_dir_all(&caps_dir)?;
        println!("  {} Removed capability disables", "✓".green());
    }

    // Remove epoch revocations
    let epochs_dir = ctx.state_dir.join("epochs");
    if epochs_dir.exists() {
        fs::remove_dir_all(&epochs_dir)?;
        println!("  {} Removed epoch revocations", "✓".green());
    }

    let receipt = Receipt::new("reset_all", "all");
    let receipt_path = receipt.save(&ctx.evidence_dir)?;

    println!();
    println!("{}", "All kill switches have been RESET".green());
    println!("  Receipt: {}", receipt_path.display());

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
    fn test_disable_global() {
        let (ctx, _tmp) = create_test_context();
        disable_global(&ctx).unwrap();
        assert!(ctx.state_dir.join("global_disabled").exists());
    }

    #[test]
    fn test_disable_family() {
        let (ctx, _tmp) = create_test_context();
        disable_family(&ctx, "test-family").unwrap();
        assert!(ctx.state_dir.join("families/test-family_disabled").exists());
    }

    #[test]
    fn test_disable_capability() {
        let (ctx, _tmp) = create_test_context();
        disable_capability(&ctx, "test-cap").unwrap();
        assert!(ctx.state_dir.join("capabilities/test-cap_disabled").exists());
    }

    #[test]
    fn test_revoke_epoch() {
        let (ctx, _tmp) = create_test_context();
        revoke_epoch(&ctx, "epoch-001").unwrap();
        let content = fs::read_to_string(ctx.state_dir.join("epochs/epoch-001_status")).unwrap();
        assert_eq!(content, "revoked");
    }

    #[test]
    fn test_reset_with_force() {
        let (ctx, _tmp) = create_test_context();

        // Set up some state
        disable_global(&ctx).unwrap();
        disable_family(&ctx, "test-family").unwrap();

        // Reset with force
        reset(&ctx, true).unwrap();

        // Verify state is cleared
        assert!(!ctx.state_dir.join("global_disabled").exists());
        assert!(!ctx.state_dir.join("families").exists());
    }
}
