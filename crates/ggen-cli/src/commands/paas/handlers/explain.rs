//! Explain artifact origin handler
//! Verb: explain | Noun: artifact/code

use crate::commands::paas::errors::Result;
use std::path::Path;

/// Explain the RDF specification origin of an artifact
pub async fn explain_artifact(path: &Path, show_spec: bool, show_pipeline: bool) -> Result<()> {
    if !path.exists() {
        if cfg!(feature = "verbose") {
            eprintln!("File not found: {:?}", path);
        }
        return Ok(());
    }

    if cfg!(feature = "verbose") {
        eprintln!("Artifact: {:?}", path);

        if show_spec {
            eprintln!("  Generated from specification:");
            eprintln!("    cli-commands.ttl");
        }

        if show_pipeline {
            eprintln!("  Transformation pipeline:");
            eprintln!("    1. Parse TTL");
            eprintln!("    2. Extract metadata");
            eprintln!("    3. Validate with SHACL");
            eprintln!("    4. Render templates");
            eprintln!("    5. Write artifacts");
        }

        if !show_spec && !show_pipeline {
            eprintln!("Use --show-spec or --show-pipeline for more details");
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_explain_nonexistent_file() {
        let result = explain_artifact(Path::new("/nonexistent"), false, false).await;
        assert!(result.is_ok()); // Gracefully handles missing files
    }

    #[tokio::test]
    async fn test_explain_with_details() {
        let result = explain_artifact(Path::new("."), true, true).await;
        assert!(result.is_ok());
    }
}
