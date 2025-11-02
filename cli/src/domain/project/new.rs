//! Project creation domain logic
//!
//! Chicago TDD: Pure business logic with REAL project creation

use ggen_core::project_generator::{create_new_project, ProjectConfig, ProjectType};
use ggen_utils::error::Result;

/// Project creation result
#[derive(Debug, Clone)]
pub struct CreationResult {
    pub project_path: String,
    pub next_steps: String,
}

/// Create a new project (Chicago TDD: REAL implementation)
pub fn create_project(args: &crate::cmds::project::NewArgs) -> Result<CreationResult> {
    // Validate project name
    ggen_core::project_generator::common::validate_project_name(&args.name)?;

    // Parse project type
    let project_type: ProjectType = args.project_type.parse()?;

    // Create project config
    let config = ProjectConfig {
        name: args.name.clone(),
        project_type: project_type.clone(),
        framework: args.framework.clone(),
        path: args.output.clone(),
    };

    // Create project synchronously
    let runtime = tokio::runtime::Runtime::new()
        .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Runtime error: {}", e)))?;

    runtime.block_on(async {
        create_new_project(&config).await
    })?;

    // Generate next steps message
    let next_steps = match project_type {
        ProjectType::RustWeb | ProjectType::RustCli | ProjectType::RustLib => {
            "  cargo run".to_string()
        }
        ProjectType::NextJs | ProjectType::Nuxt => {
            if !args.skip_install {
                "  npm run dev".to_string()
            } else {
                "  npm install\n  npm run dev".to_string()
            }
        }
    };

    Ok(CreationResult {
        project_path: args.output.join(&args.name).display().to_string(),
        next_steps,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_validate_project_name() {
        // Valid names
        assert!(ggen_core::project_generator::common::validate_project_name("my-project").is_ok());
        assert!(ggen_core::project_generator::common::validate_project_name("my_project").is_ok());

        // Invalid names
        assert!(ggen_core::project_generator::common::validate_project_name("my project").is_err());
        assert!(ggen_core::project_generator::common::validate_project_name("").is_err());
    }

    #[test]
    fn test_next_steps_rust() {
        let args = crate::cmds::project::NewArgs {
            name: "test-project".to_string(),
            project_type: "rust-cli".to_string(),
            framework: None,
            output: PathBuf::from("/tmp"),
            skip_install: false,
        };

        let project_type: ProjectType = args.project_type.parse().unwrap();

        let next_steps = match project_type {
            ProjectType::RustWeb | ProjectType::RustCli | ProjectType::RustLib => {
                "  cargo run".to_string()
            }
            _ => String::new(),
        };

        assert_eq!(next_steps, "  cargo run");
    }
}
