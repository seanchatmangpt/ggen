//! Key epoch commands
//!
//! Signing key epoch rotation and management.

use super::Context;
use crate::models::epoch::KeyEpoch;
use crate::models::receipt::Receipt;
use anyhow::Result;
use colored::Colorize;
use sha2::{Digest, Sha256};
use std::fs;

/// Rotate to a new signing key epoch
pub fn rotate(ctx: &Context, duration_days: i64, dry_run: bool) -> Result<()> {
    println!("Rotating to new epoch...");
    println!("  Duration: {} days", duration_days);
    println!("  Dry run:  {}", if dry_run { "yes" } else { "no" });
    println!();

    // Generate new epoch
    let epoch_id = format!("epoch-{}", chrono::Utc::now().format("%Y%m%d-%H%M%S"));

    // Generate a mock public key hash (in production, would use actual key generation)
    let mock_key_material = format!("{}-{}", epoch_id, chrono::Utc::now().timestamp());
    let mut hasher = Sha256::new();
    hasher.update(mock_key_material.as_bytes());
    let public_key_hash = hex::encode(hasher.finalize());

    let epoch = KeyEpoch::new(epoch_id.clone(), public_key_hash.clone(), duration_days);

    if dry_run {
        println!("{}", "[DRY RUN] Would create epoch:".yellow());
        println!();
        println!("  Epoch ID:        {}", epoch_id.cyan());
        println!("  Public Key Hash: {}", &public_key_hash[..16]);
        println!("  Start:           {}", epoch.epoch_start);
        println!("  End:             {}", epoch.epoch_end);
        println!();
        println!("{}", "No changes made (dry run)".yellow());
        return Ok(());
    }

    // Save epoch
    let epochs_dir = ctx.state_dir.join("epochs");
    fs::create_dir_all(&epochs_dir)?;

    let epoch_file = epochs_dir.join(format!("{}.json", epoch_id));
    let epoch_json = serde_json::to_string_pretty(&epoch)?;
    fs::write(&epoch_file, &epoch_json)?;

    // Mark as active
    let active_file = epochs_dir.join("active");
    fs::write(&active_file, &epoch_id)?;

    // Generate receipt
    let receipt = Receipt::new("rotate_epoch", &epoch_id);
    let receipt_path = receipt.save(&ctx.evidence_dir)?;

    println!("  {} Created new epoch", "✓".green());
    println!();
    println!("  Epoch ID:        {}", epoch_id.cyan());
    println!("  Public Key Hash: {}...", &public_key_hash[..32]);
    println!("  Valid Until:     {}", epoch.epoch_end);
    println!();
    println!("  Epoch File: {}", epoch_file.display());
    println!("  Receipt:    {}", receipt_path.display());

    Ok(())
}

/// List all epochs
pub fn list(ctx: &Context) -> Result<()> {
    let epochs_dir = ctx.state_dir.join("epochs");

    if !epochs_dir.exists() {
        println!("  No epochs found");
        return Ok(());
    }

    // Read active epoch
    let active_file = epochs_dir.join("active");
    let active_epoch = if active_file.exists() {
        fs::read_to_string(&active_file).ok()
    } else {
        None
    };

    println!("  {:<30} {:<10} {:<10}", "EPOCH ID", "STATUS", "VALID");
    println!("  {}", "─".repeat(55));

    for entry in fs::read_dir(&epochs_dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.extension().map_or(false, |e| e == "json") {
            if let Ok(content) = fs::read_to_string(&path) {
                if let Ok(epoch) = serde_json::from_str::<KeyEpoch>(&content) {
                    let is_active = active_epoch.as_ref().map_or(false, |a| a == &epoch.epoch_id);
                    let status = if epoch.is_revoked {
                        "REVOKED".red().to_string()
                    } else if is_active {
                        "ACTIVE".green().to_string()
                    } else {
                        "INACTIVE".yellow().to_string()
                    };

                    let valid = if epoch.is_valid() {
                        "YES".green().to_string()
                    } else {
                        "NO".red().to_string()
                    };

                    println!("  {:<30} {:<10} {:<10}", epoch.epoch_id, status, valid);
                }
            }
        }
    }

    Ok(())
}

/// Show current epoch status
pub fn status(ctx: &Context) -> Result<()> {
    let epochs_dir = ctx.state_dir.join("epochs");

    // Read active epoch
    let active_file = epochs_dir.join("active");
    if !active_file.exists() {
        println!("  No active epoch");
        println!();
        println!("  Run {} to create one", "mcp epoch rotate".cyan());
        return Ok(());
    }

    let active_epoch_id = fs::read_to_string(&active_file)?;
    let epoch_file = epochs_dir.join(format!("{}.json", active_epoch_id.trim()));

    if !epoch_file.exists() {
        println!("  Active epoch file not found: {}", active_epoch_id);
        return Ok(());
    }

    let content = fs::read_to_string(&epoch_file)?;
    let epoch: KeyEpoch = serde_json::from_str(&content)?;

    println!("  Current Active Epoch");
    println!("  {}", "─".repeat(40));
    println!();
    println!("  Epoch ID:        {}", epoch.epoch_id.cyan());
    println!("  Public Key Hash: {}...", &epoch.public_key_hash[..32]);
    println!("  Start:           {}", epoch.epoch_start);
    println!("  End:             {}", epoch.epoch_end);
    println!("  Revoked:         {}", if epoch.is_revoked { "YES".red() } else { "NO".green() });
    println!("  Valid:           {}", if epoch.is_valid() { "YES".green() } else { "NO".red() });

    // Calculate days remaining
    let now = chrono::Utc::now();
    if epoch.epoch_end > now {
        let remaining = epoch.epoch_end - now;
        println!("  Days Remaining:  {}", remaining.num_days());
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
    fn test_rotate_dry_run() {
        let (ctx, _tmp) = create_test_context();
        rotate(&ctx, 90, true).unwrap();

        // Dry run should not create any files
        let epochs_dir = ctx.state_dir.join("epochs");
        assert!(!epochs_dir.exists() || fs::read_dir(&epochs_dir).unwrap().count() == 0);
    }

    #[test]
    fn test_rotate_actual() {
        let (ctx, _tmp) = create_test_context();
        rotate(&ctx, 90, false).unwrap();

        // Should create epoch files
        let epochs_dir = ctx.state_dir.join("epochs");
        assert!(epochs_dir.exists());
        assert!(epochs_dir.join("active").exists());
    }

    #[test]
    fn test_status_no_epoch() {
        let (ctx, _tmp) = create_test_context();
        status(&ctx).unwrap(); // Should not panic
    }

    #[test]
    fn test_status_with_epoch() {
        let (ctx, _tmp) = create_test_context();
        rotate(&ctx, 90, false).unwrap();
        status(&ctx).unwrap(); // Should not panic
    }
}
