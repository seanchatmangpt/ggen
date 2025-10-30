// CLI command for `ggen new`
// Creates new projects from templates

use anyhow::Result;
use clap::Parser;
use ggen_core::generator::{create_new_project, ProjectConfig, ProjectType};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(name = "new")]
#[command(about = "Create a new project from a template", long_about = None)]
pub struct NewCommand {
    /// Project name
    #[arg(help = "Name of the project to create")]
    pub name: String,

    /// Project type
    #[arg(
        short = 't',
        long = "type",
        help = "Project type: rust-web, rust-cli, rust-lib, nextjs, nuxt"
    )]
    pub project_type: String,

    /// Framework (optional)
    #[arg(
        short = 'f',
        long = "framework",
        help = "Framework to use (e.g., axum, warp for rust-web)"
    )]
    pub framework: Option<String>,

    /// Output directory
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
        help = "Skip installing dependencies",
        default_value = "false"
    )]
    pub skip_install: bool,
}

impl NewCommand {
    pub async fn execute(&self) -> Result<()> {
        // Validate project name
        ggen_core::generator::common::validate_project_name(&self.name)?;

        // Parse project type
        let project_type: ProjectType = self.project_type.parse()?;

        // Create project config
        let config = ProjectConfig {
            name: self.name.clone(),
            project_type,
            framework: self.framework.clone(),
            path: self.output.clone(),
        };

        // Print banner
        println!("🚀 Creating new project: {}", self.name);
        println!("   Type: {}", self.project_type);
        if let Some(framework) = &self.framework {
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
        println!("  cd {}", self.name);

        match config.project_type {
            ProjectType::RustWeb | ProjectType::RustCli | ProjectType::RustLib => {
                println!("  cargo run");
            }
            ProjectType::NextJs | ProjectType::Nuxt => {
                if !self.skip_install {
                    println!("  npm run dev");
                } else {
                    println!("  npm install");
                    println!("  npm run dev");
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_new_command() {
        let cmd = NewCommand::parse_from(&["new", "my-project", "--type", "rust-web"]);
        assert_eq!(cmd.name, "my-project");
        assert_eq!(cmd.project_type, "rust-web");
    }

    #[test]
    fn test_parse_with_framework() {
        let cmd = NewCommand::parse_from(&[
            "new",
            "my-project",
            "--type",
            "rust-web",
            "--framework",
            "axum",
        ]);
        assert_eq!(cmd.framework, Some("axum".to_string()));
    }
}
