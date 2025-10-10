use clap::Args;
use ggen_utils::error::Result;
use std::process::Command;

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
    println!("Generating unified diff");

    // Validate template reference
    if args.template_ref.is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Template reference cannot be empty",
        ));
    }

    // Parse variables
    let mut variables = std::collections::HashMap::new();
    for var in &args.vars {
        if let Some((key, value)) = var.split_once('=') {
            variables.insert(key.to_string(), value.to_string());
        } else {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Invalid variable format: {}. Expected key=value",
                var
            )));
        }
    }

    // Generate diff using cargo make
    let mut cmd = Command::new("cargo");
    cmd.args(["make", "project-diff"]);
    cmd.arg("--template").arg(&args.template_ref);
    cmd.arg("--context").arg(args.context.to_string());

    if args.color {
        cmd.arg("--color");
    }

    for (key, value) in &variables {
        cmd.arg("--var").arg(format!("{}={}", key, value));
    }

    let output = cmd.output().map_err(ggen_utils::error::Error::from)?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Diff generation failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);

    if stdout.trim().is_empty() {
        println!("✅ No differences found");
        println!("No differences found between template output and current project state.");
    } else {
        println!("✅ Diff generated successfully");
        println!("{}", stdout);
    }

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
