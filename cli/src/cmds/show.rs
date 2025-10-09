use clap::Args;
use core::template::Template;
use std::path::Path;
use utils::error::Result;

#[derive(Args, Debug)]
pub struct ShowArgs {
    #[arg(
        value_name = "TEMPLATE",
        help = "Template file path or pack_id:template_path"
    )]
    pub template: String,

    #[arg(short = 'v', long = "vars", help = "Variables (key=value pairs)")]
    pub vars: Vec<String>,
}

pub fn run(args: &ShowArgs) -> Result<()> {
    // Parse template path
    let template_path = if args.template.contains(':') {
        // Pack-based template reference
        return Err(
            anyhow::anyhow!("Pack-based templates not yet supported in show command").into(),
        );
    } else {
        // Direct file path
        Path::new(&args.template)
    };

    if !template_path.exists() {
        return Err(anyhow::anyhow!("Template file not found: {}", template_path.display()).into());
    }

    // Read and parse template
    let content = std::fs::read_to_string(template_path)?;
    let mut template = Template::parse(&content)?;

    // Parse variables
    let mut vars = std::collections::BTreeMap::new();
    for var in &args.vars {
        if let Some((key, value)) = var.split_once('=') {
            vars.insert(key.to_string(), value.to_string());
        }
    }

    // Create context and render frontmatter
    let mut ctx = tera::Context::from_serialize(&vars)?;
    let mut pipeline = core::pipeline::PipelineBuilder::new().build()?;

    // Access tera through a helper method since it's pub(crate)
    render_template_frontmatter(&mut template, &mut pipeline, &ctx)?;

    // Display template metadata
    println!("📄 Template: {}", template_path.display());
    println!("{}", "=".repeat(50));

    // Show frontmatter
    if let Some(to) = &template.front.to {
        println!("Output: {}", to);
    }

    if !template.front.vars.is_empty() {
        println!("\nVariables:");
        for (key, value) in &template.front.vars {
            println!("  {}: {}", key, value);
        }
    }

    if template.front.inject {
        println!("\nInjection Mode: Enabled");
        if template.front.prepend {
            println!("  Mode: prepend");
        } else if template.front.append {
            println!("  Mode: append");
        } else if let Some(before) = &template.front.before {
            println!("  Mode: before '{}'", before);
        } else if let Some(after) = &template.front.after {
            println!("  Mode: after '{}'", after);
        } else if let Some(line) = template.front.at_line {
            println!("  Mode: at line {}", line);
        }

        if let Some(skip_if) = &template.front.skip_if {
            println!("  Skip if: {}", skip_if);
        }

        if template.front.idempotent {
            println!("  Idempotent: true");
        }

        if template.front.backup.unwrap_or(false) {
            println!("  Backup: true");
        }
    }

    if !template.front.rdf_inline.is_empty() {
        println!("\nInline RDF blocks: {}", template.front.rdf_inline.len());
    }

    if !template.front.rdf.is_empty() {
        println!("\nRDF files: {}", template.front.rdf.len());
        for rdf_file in &template.front.rdf {
            println!("  - {}", rdf_file);
        }
    }

    if !template.front.sparql.is_empty() {
        println!("\nSPARQL queries: {}", template.front.sparql.len());
        for (name, query) in &template.front.sparql {
            println!("  {}: {}", name, query);
        }
    }

    if let Some(sh_before) = &template.front.sh_before {
        println!("\nShell hook (before): {}", sh_before);
    }

    if let Some(sh_after) = &template.front.sh_after {
        println!("\nShell hook (after): {}", sh_after);
    }

    // Show template body preview
    println!("\nTemplate Body Preview:");
    println!("{}", "-".repeat(30));
    let body_lines: Vec<&str> = template.body.lines().take(10).collect();
    for line in body_lines {
        println!("{}", line);
    }
    if template.body.lines().count() > 10 {
        println!("... ({} more lines)", template.body.lines().count() - 10);
    }

    Ok(())
}

// Helper function to render template frontmatter
fn render_template_frontmatter(
    template: &mut Template, pipeline: &mut core::pipeline::Pipeline, ctx: &tera::Context,
) -> Result<()> {
    // Since tera is pub(crate), we can access it from within the crate
    template
        .render_frontmatter(pipeline.tera_mut(), ctx)
        .map_err(|e| anyhow::anyhow!("Template rendering error: {}", e).into())
}
