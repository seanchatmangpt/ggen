use clap::Args;
use std::collections::BTreeMap;
use std::path::PathBuf;
use tera::Context;
use utils::error::Result;

use core::template::Template;

#[derive(Args, Debug)]
pub struct ValidateArgs {
    /// Path to the template file to validate
    #[arg(value_name = "TEMPLATE")]
    pub template: PathBuf,
    
    /// Variables (key=value pairs) for frontmatter rendering
    #[arg(short = 'v', long = "var", value_parser = parse_key_val::<String, String>)]
    pub vars: Vec<(String, String)>,
    
    /// Show detailed validation output
    #[arg(long)]
    pub verbose: bool,
}

fn parse_key_val<K, V>(s: &str) -> std::result::Result<(K, V), String>
where
    K: std::str::FromStr,
    K::Err: ToString,
    V: std::str::FromStr,
    V::Err: ToString,
{
    let pos = s.find('=').ok_or_else(|| format!("invalid KEY=value: no `=` found in `{s}`"))?;
    let key = s[..pos].parse().map_err(|e: K::Err| e.to_string())?;
    let val = s[pos + 1..].parse().map_err(|e: V::Err| e.to_string())?;
    Ok((key, val))
}

pub fn run(args: &ValidateArgs) -> Result<()> {
    // Read template file
    let template_content = std::fs::read_to_string(&args.template)?;
    
    // Parse template
    let mut template = Template::parse(&template_content)?;
    
    if args.verbose {
        println!("✓ Template parsed successfully");
        println!("  File: {}", args.template.display());
        println!("  Body length: {} characters", template.body.len());
    }
    
    // Create Tera context from vars
    let mut vars = BTreeMap::new();
    for (k, v) in &args.vars {
        vars.insert(k.clone(), v.clone());
    }
    let mut ctx = Context::from_serialize(&vars)?;
    
    // Insert environment variables
    insert_env(&mut ctx);
    
    // Create Tera instance
    let mut tera = tera::Tera::default();
    tera.autoescape_on(vec![]);
    
    // Register text transformation filters
    core::register::register_all(&mut tera);
    
    // Render frontmatter
    template.render_frontmatter(&mut tera, &ctx)?;
    
    if args.verbose {
        println!("✓ Frontmatter rendered successfully");
        
        // Show rendered frontmatter details
        if let Some(to) = &template.front.to {
            println!("  Output path: {}", to);
        }
        if let Some(from) = &template.front.from {
            println!("  Source path: {}", from);
        }
        if template.front.inject {
            println!("  Injection mode: enabled");
        }
        if !template.front.prefixes.is_empty() {
            println!("  Prefixes: {} defined", template.front.prefixes.len());
        }
        if !template.front.rdf.is_empty() {
            println!("  RDF files: {} defined", template.front.rdf.len());
        }
        if !template.front.rdf_inline.is_empty() {
            println!("  Inline RDF: {} blocks", template.front.rdf_inline.len());
        }
        if !template.front.sparql.is_empty() {
            println!("  SPARQL queries: {} defined", template.front.sparql.len());
        }
        if !template.front.vars.is_empty() {
            println!("  Template vars: {} defined", template.front.vars.len());
        }
    }
    
    // Validate that required fields are present for injection
    if template.front.inject {
        if template.front.to.is_none() {
            return Err(utils::error::Error::new("Injection mode requires 'to:' field to specify target file"));
        }
    }
    
    // Validate that from: file exists if specified
    if let Some(from_path) = &template.front.from {
        let rendered_from = tera.render_str(from_path, &ctx)?;
        if !std::path::Path::new(&rendered_from).exists() {
            return Err(utils::error::Error::new(&format!("Source file '{}' does not exist", rendered_from)));
        }
    }
    
    println!("✓ Template validation passed");
    Ok(())
}

fn insert_env(ctx: &mut Context) {
    let mut env_map: BTreeMap<String, String> = BTreeMap::new();
    for (k, v) in std::env::vars() {
        env_map.insert(k, v);
    }
    ctx.insert("env", &env_map);

    if let Ok(cwd) = std::env::current_dir() {
        ctx.insert("cwd", &cwd.display().to_string());
    }
}
