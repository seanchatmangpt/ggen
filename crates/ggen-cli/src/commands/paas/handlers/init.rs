//! Initialize submodule handler
//! Verb: init | Noun: submodule

use crate::commands::paas::errors::{PaasError, Result};
use std::process::Command;

/// Initialize a submodule (git submodule add or git submodule update --init)
pub async fn init_submodule(name: &str, recursive: bool, shallow: bool) -> Result<()> {
    // Validate submodule name
    if name.is_empty() {
        return Err(PaasError::MissingOption("submodule name".to_string()));
    }

    // Map friendly names to repo URLs
    let (url, path) = match name {
        "ggen-spec-kit" | "spec-kit" => (
            "https://github.com/seanchatmangpt/ggen-spec-kit.git",
            "ggen-spec-kit",
        ),
        "clap-noun-verb" | "clap" => (
            "https://github.com/seanchatmangpt/clap-noun-verb.git",
            "clap-noun-verb",
        ),
        other => {
            return Err(PaasError::InvalidCommand {
                noun: "submodule".to_string(),
                verb: "init".to_string(),
                available_verbs: vec!["ggen-spec-kit".to_string(), "clap-noun-verb".to_string()],
            })
        }
    };

    // Check if already initialized
    let status_output = Command::new("git")
        .args(&["submodule", "status", path])
        .output()
        .map_err(|e| PaasError::GitFailed(format!("Failed to check submodule status: {}", e)))?;

    let status_str = String::from_utf8_lossy(&status_output.stdout);
    if !status_str.contains("-") && !status_str.is_empty() {
        return Err(PaasError::SubmoduleExists(name.to_string()));
    }

    // Add submodule if not present
    let mut cmd = Command::new("git");
    cmd.args(&["submodule", "add"]);

    if shallow {
        cmd.args(&["--depth", "1"]);
    }

    cmd.arg(url).arg(path);

    let output = cmd
        .output()
        .map_err(|e| PaasError::GitFailed(format!("Failed to add submodule: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(PaasError::GitFailed(stderr.to_string()));
    }

    // Update submodule
    let mut update_cmd = Command::new("git");
    update_cmd.args(&["submodule", "update", "--init"]);

    if recursive {
        update_cmd.arg("--recursive");
    }

    update_cmd.arg(path);

    let update_output = update_cmd
        .output()
        .map_err(|e| PaasError::GitFailed(format!("Failed to update submodule: {}", e)))?;

    if !update_output.status.success() {
        let stderr = String::from_utf8_lossy(&update_output.stderr);
        return Err(PaasError::GitFailed(stderr.to_string()));
    }

    if cfg!(feature = "verbose") {
        eprintln!("✓ Initialized submodule: {} at {}", name, path);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_init_invalid_submodule() {
        let result = init_submodule("invalid-module", false, false).await;
        assert!(result.is_err());
        match result {
            Err(PaasError::InvalidCommand { .. }) => (),
            _ => panic!("Expected InvalidCommand error"),
        }
    }

    #[tokio::test]
    async fn test_init_requires_name() {
        let result = init_submodule("", false, false).await;
        assert!(result.is_err());
        match result {
            Err(PaasError::MissingOption(opt)) => assert_eq!(opt, "submodule name"),
            _ => panic!("Expected MissingOption error"),
        }
    }
}
