use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct PlanArgs {
    /// Template reference (e.g., "gpack-id:path/to.tmpl" or "local/path.tmpl")
    pub template_ref: String,

    /// Variables to pass to the template (key=value format)
    #[arg(short = 'v', long = "var")]
    pub vars: Vec<String>,

    /// Output file for the generated plan
    #[arg(short = 'o', long = "output")]
    pub output: Option<String>,

    /// Output format (json, yaml, toml)
    #[arg(long, default_value = "json")]
    pub format: String,
}

/// Main entry point for `ggen project plan`
pub async fn run(args: &PlanArgs) -> Result<()> {
    println!("🚧 Placeholder: project plan");
    println!("  Template: {}", args.template_ref);
    println!("  Vars: {:?}", args.vars);
    println!("  Output: {:?}", args.output);
    println!("  Format: {}", args.format);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_plan_args_parsing() {
        let args = PlanArgs {
            template_ref: "test.tmpl".to_string(),
            vars: vec!["key=value".to_string()],
            output: Some("plan.json".to_string()),
            format: "json".to_string(),
        };

        assert_eq!(args.template_ref, "test.tmpl");
        assert_eq!(args.format, "json");
    }
}
