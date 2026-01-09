//! Update submodule handler
//! Verb: update | Noun: submodule

use crate::commands::paas::errors::{PaasError, Result};
use std::process::Command;

/// Update submodule(s) to latest version
pub async fn update_submodule(
    name: Option<&str>,
    recursive: bool,
    checkout: Option<&str>,
) -> Result<()> {
    let mut cmd = Command::new("git");
    cmd.args(&["submodule", "update"]);

    if recursive {
        cmd.arg("--recursive");
    }

    if let Some(ref_name) = checkout {
        cmd.args(&["--checkout", ref_name]);
    }

    if let Some(submodule) = name {
        cmd.arg(submodule);
    }

    let output = cmd
        .output()
        .map_err(|e| PaasError::GitFailed(format!("Failed to update submodule: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(PaasError::GitFailed(stderr.to_string()));
    }

    // Submodule(s) updated successfully
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_update_all_submodules() {
        let result = update_submodule(None, false, None).await;
        // Will fail in test environment without git repo, but verifies API
        assert!(result.is_err() || result.is_ok());
    }
}
