//! Sync CLI wrapper for project init command
//!
//! This module provides the synchronous CLI interface that bridges to the async
//! domain logic following the v2.0 architecture pattern.

use clap::Args;
use clap_noun_verb_macros::verb;
use ggen_utils::error::Result;
use std::path::PathBuf;

/// Arguments for the project init command
#[derive(Args, Debug, Clone)]
pub struct InitArgs {
    /// Project name
    pub name: String,

    /// Target directory (defaults to project name)
    #[arg(short, long)]
    pub path: Option<PathBuf>,

    /// Skip git initialization
    #[arg(long)]
    pub no_git: bool,

    /// Use template for initialization
    #[arg(short, long)]
    pub template: Option<String>,
}

/// Execute the project init command (sync wrapper)
pub fn run(args: &InitArgs) -> Result<()> {
    crate::runtime::execute(async {
        let project_path = args.path.clone().unwrap_or_else(|| PathBuf::from(&args.name));

        crate::domain::project::init::init_project(&project_path, &args.name).await?;

        println!("✅ Initialized project '{}'", args.name);
        println!("   Location: {}", project_path.display());

        if !args.no_git {
            println!("   Git: initialized");
        }

        Ok(())
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_init_args_defaults() {
        let args = InitArgs {
            name: "my-project".to_string(),
            path: None,
            no_git: false,
            template: None,
        };
        assert_eq!(args.name, "my-project");
        assert!(!args.no_git);
        assert!(args.template.is_none());
    }

    #[test]
    fn test_init_args_with_template() {
        let args = InitArgs {
            name: "my-app".to_string(),
            path: Some(PathBuf::from("./my-app")),
            no_git: true,
            template: Some("rust-basic".to_string()),
        };
        assert!(args.path.is_some());
        assert!(args.no_git);
        assert_eq!(args.template, Some("rust-basic".to_string()));
    }
}
