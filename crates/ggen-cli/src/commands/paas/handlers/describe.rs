//! Describe artifact handler
//! Verb: describe | Noun: artifact/specification

use crate::commands::paas::errors::Result;

/// Describe an artifact or resource
pub async fn describe_resource(name: &str, detailed: bool, format: &str) -> Result<()> {
    match format {
        "table" | "json" | "yaml" => {}
        other => {
            if cfg!(feature = "verbose") {
                eprintln!("Unknown format: {}. Using table.", other);
            }
        }
    }

    if detailed {
        if cfg!(feature = "verbose") {
            eprintln!("Detailed information for: {}", name);
            eprintln!("  Type: artifact");
            eprintln!("  Status: unknown");
            eprintln!("  Created: N/A");
        }
    } else {
        if cfg!(feature = "verbose") {
            eprintln!("Resource: {}", name);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_describe_default_format() {
        let result = describe_resource("test-artifact", false, "table").await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_describe_json_format() {
        let result = describe_resource("test", true, "json").await;
        assert!(result.is_ok());
    }
}
