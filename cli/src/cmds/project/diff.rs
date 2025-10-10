use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct DiffArgs {
    /// Template reference (e.g., "gpack-id:path/to.tmpl" or "local/path.tmpl")
    pub template_ref: String,

    /// Variables to pass to the template (key=value format)
    #[arg(short = 'v', long = "var")]
    pub vars: Vec<String>,

    /// Show unified diff with N lines of context
    #[arg(short = 'U', long = "unified", default_value = "3")]
    pub context: usize,

    /// Colorize the diff output
    #[arg(long, default_value = "true")]
    pub color: bool,
}

/// Main entry point for `ggen project diff`
pub async fn run(args: &DiffArgs) -> Result<()> {
    println!("🚧 Placeholder: project diff");
    println!("  Template: {}", args.template_ref);
    println!("  Vars: {:?}", args.vars);
    println!("  Context lines: {}", args.context);
    println!("  Color: {}", args.color);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_diff_args_parsing() {
        let args = DiffArgs {
            template_ref: "test.tmpl".to_string(),
            vars: vec!["name=Test".to_string()],
            context: 5,
            color: true,
        };

        assert_eq!(args.template_ref, "test.tmpl");
        assert_eq!(args.context, 5);
    }
}
