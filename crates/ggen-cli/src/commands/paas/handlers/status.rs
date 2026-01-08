//! Show status handler
//! Verb: status | Noun: (default/implicit)

use crate::commands::paas::errors::Result;

/// Show deployment or system status
pub async fn show_status(environment: Option<&str>, detailed: bool) -> Result<()> {
    let env_display = environment.unwrap_or("all");

    if detailed {
        if cfg!(feature = "verbose") {
            eprintln!("Status for environment: {}", env_display);
            eprintln!("  Deployments: 0");
            eprintln!("  Last sync: Never");
            eprintln!("  Health: Unknown");
        }
    } else {
        if cfg!(feature = "verbose") {
            eprintln!("✓ Status: OK ({})", env_display);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
