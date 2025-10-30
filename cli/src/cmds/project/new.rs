// CLI command for `ggen project new`
// Creates new projects from templates using London TDD approach

use ggen_utils::error::Result;
use clap::Args;
use ggen_core::project_generator::{create_new_project, ProjectConfig, ProjectType};
use std::path::PathBuf;

#[derive(Args, Debug, Clone)]
pub struct NewArgs {
    /// Project name
    #[arg(help = "Name of the project to create")]
    pub name: String,

    /// Project type: rust-web, rust-cli, rust-lib, nextjs, nuxt
    #[arg(
        short = 't',
        long = "type",
        help = "Project type to generate"
    )]
    pub project_type: String,

    /// Framework (optional)
    #[arg(
        short = 'f',
        long = "framework",
        help = "Framework to use (e.g., axum, warp for rust-web)"
    )]
    pub framework: Option<String>,

    /// Output directory (defaults to current directory)
    #[arg(
        short = 'o',
        long = "output",
        default_value = ".",
        help = "Output directory for the project"
    )]
    pub output: PathBuf,

    /// Skip dependency installation
    #[arg(
        long = "skip-install",
        help = "Skip installing dependencies after generation",
        default_value = "false"
    )]
    pub skip_install: bool,
}

pub async fn run(args: &NewArgs) -> Result<()> {
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

    // Print banner
    println!("🚀 Creating new project: {}", args.name);
    println!("   Type: {}", args.project_type);
    if let Some(framework) = &args.framework {
        println!("   Framework: {}", framework);
    }
    println!();

    // Create project
    create_new_project(&config).await?;

    // Print success message
    println!();
    println!("✅ Project created successfully!");
    println!();
    println!("Next steps:");
    println!("  cd {}", args.name);

    match project_type {
        ProjectType::RustWeb | ProjectType::RustCli | ProjectType::RustLib => {
            println!("  cargo run");
        }
        ProjectType::NextJs | ProjectType::Nuxt => {
            if !args.skip_install {
                println!("  npm run dev");
            } else {
                println!("  npm install");
                println!("  npm run dev");
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_args_parsing() {
        let args = NewArgs {
            name: "my-project".to_string(),
            project_type: "rust-web".to_string(),
            framework: Some("axum".to_string()),
            output: PathBuf::from("."),
            skip_install: false,
        };

        assert_eq!(args.name, "my-project");
        assert_eq!(args.project_type, "rust-web");
        assert_eq!(args.framework, Some("axum".to_string()));
    }

    #[tokio::test]
    async fn test_run_with_invalid_name() {
        let args = NewArgs {
            name: "my project".to_string(), // Invalid: contains space
            project_type: "rust-web".to_string(),
            framework: None,
            output: PathBuf::from("."),
            skip_install: false,
        };

        let result = run(&args).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("whitespace"));
    }

    #[tokio::test]
    async fn test_run_with_invalid_type() {
        let args = NewArgs {
            name: "my-project".to_string(),
            project_type: "invalid-type".to_string(),
            framework: None,
            output: PathBuf::from("."),
            skip_install: false,
        };

        let result = run(&args).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Unsupported"));
    }
}
