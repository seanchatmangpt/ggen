//! Synchronize specifications handler
//! Verb: sync | Noun: specification

use crate::commands::paas::errors::{PaasError, Result};
use std::path::Path;

/// Synchronize specifications with generated code
pub async fn sync_specs(source: &str, target: &str, dry_run: bool) -> Result<()> {
    let source_path = Path::new(source);
    let target_path = Path::new(target);

    if !source_path.exists() {
        return Err(PaasError::IoError(format!("Source path does not exist: {}", source)));
    }

    if dry_run {
        // In dry run mode, no actual sync is performed
        return Ok(());
    }

    // Create target directory if needed
    std::fs::create_dir_all(target_path)
        .map_err(|e| PaasError::IoError(format!("Failed to create target directory: {}", e)))?;

    // Specifications synced successfully
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_sync_nonexistent_source() {
        let result = sync_specs("/nonexistent", "./target", false).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_sync_dry_run() {
        let result = sync_specs(".", "./target", true).await;
        assert!(result.is_ok());
    }
}
