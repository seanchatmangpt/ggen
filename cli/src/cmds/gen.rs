use clap::Args;
use std::collections::BTreeMap;
use std::path::PathBuf;
use utils::error::Result;

use core::pipeline::PipelineBuilder;
use core::resolver::TemplateResolver;
// sha2 imports removed - not currently used in this implementation

#[derive(Args, Debug)]
pub struct GenArgs {
    /// Generator name (e.g., "cli")
    pub generator: String,
    
    /// Action name (e.g., "subcommand")
    pub action: String,
    
    /// Template name (optional, defaults to all templates for the action)
    #[arg(short, long)]
    pub name: Option<String>,
    
    /// Path to the template file (alternative to generator/action, for backward compatibility)
    #[arg(short, long)]
    pub template: Option<PathBuf>,

    /// Output directory root
    #[arg(short, long, default_value = ".")]
    pub out: PathBuf,

    /// Variables (key=value pairs)
    #[arg(short = 'v', long = "var", value_parser = parse_key_val::<String, String>)]
    pub vars: Vec<(String, String)>,

    /// Dry run (no write)
    #[arg(long)]
    pub dry: bool,
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

pub fn run(args: &GenArgs) -> Result<()> {
    let mut pipeline = PipelineBuilder::new()
        .with_config_discovery()
        .map_err(utils::error::Error::from)?
        .build()
        .map_err(utils::error::Error::from)?;
    
    let mut vars = BTreeMap::new();
    for (k, v) in &args.vars {
        vars.insert(k.clone(), v.clone());
    }

    // Determine template paths to process
    let template_paths = if let Some(template_path) = &args.template {
        // Backward compatibility: use explicit template path
        vec![template_path.clone()]
    } else {
        // New behavior: resolve by generator/action
        let resolver = TemplateResolver::new(pipeline.templates_dir());
        resolver.resolve(&args.generator, &args.action, args.name.as_deref())
            .map_err(utils::error::Error::from)?
    };

    let mut generated_paths = Vec::new();
    
    for template_path in template_paths {
        let rendered_path = pipeline.run_from_path(&template_path, &args.out, &vars, args.dry)
            .map_err(utils::error::Error::from)?;
        generated_paths.push(rendered_path);
    }
    
    if args.dry {
        println!("DRY RUN - Would generate {} file(s):", generated_paths.len());
        for (i, path) in generated_paths.iter().enumerate() {
            println!("  {}: {}", i + 1, path.display());
        }
        println!("  Vars: {} variables", vars.len());
    } else {
        for path in &generated_paths {
            println!("{}", path.display());
        }
    }
    
    Ok(())
}