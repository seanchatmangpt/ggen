use clap::Args;
use std::collections::BTreeMap;
use std::path::PathBuf;
use utils::error::Result;

use core::pipeline::PipelineBuilder;
use core::resolver::TemplateResolver;
use core::{CacheManager, LockfileManager};

#[derive(Args, Debug)]
pub struct GenArgs {
    /// Template reference in format "pack_id:template_path"
    pub template: String,

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
    let pos = s
        .find('=')
        .ok_or_else(|| format!("invalid KEY=value: no `=` found in `{s}`"))?;
    let key = s[..pos].parse().map_err(|e: K::Err| e.to_string())?;
    let val = s[pos + 1..].parse().map_err(|e: V::Err| e.to_string())?;
    Ok((key, val))
}

pub fn run(args: &GenArgs) -> Result<()> {
    // Initialize cache and lockfile managers
    let cache_manager = CacheManager::new()?;
    let project_dir = std::env::current_dir()?;
    let lockfile_manager = LockfileManager::new(&project_dir);

    // Create resolver
    let resolver = TemplateResolver::new(cache_manager, lockfile_manager);

    // Resolve template
    let template_source = resolver.resolve(&args.template)?;

    // Build pipeline (use default prefixes for now)
    let mut pipeline = PipelineBuilder::new().build()?;

    // Convert vars to BTreeMap
    let vars: BTreeMap<String, String> = args.vars.iter().cloned().collect();

    // Render template
    let plan = pipeline.render_file(&template_source.template_path, &vars, args.dry)?;

    if args.dry {
        println!("DRY RUN - Would generate:");
        println!("  Template: {}", template_source.template_path.display());
        println!("  Vars:     {} variables", vars.len());
    } else {
        plan.apply()?;
        println!("Generated successfully");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_key_val_valid() {
        let result = parse_key_val::<String, String>("name=value");
        assert!(result.is_ok());
        let (key, val) = result.unwrap();
        assert_eq!(key, "name");
        assert_eq!(val, "value");
    }

    #[test]
    fn test_parse_key_val_with_spaces() {
        let result = parse_key_val::<String, String>("name=hello world");
        assert!(result.is_ok());
        let (key, val) = result.unwrap();
        assert_eq!(key, "name");
        assert_eq!(val, "hello world");
    }

    #[test]
    fn test_parse_key_val_integer() {
        let result = parse_key_val::<String, i32>("count=42");
        assert!(result.is_ok());
        let (key, val) = result.unwrap();
        assert_eq!(key, "count");
        assert_eq!(val, 42);
    }

    #[test]
    fn test_parse_key_val_no_equals() {
        let result = parse_key_val::<String, String>("invalid");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("no `=` found"));
    }

    #[test]
    fn test_parse_key_val_empty_key() {
        let result = parse_key_val::<String, String>("=value");
        assert!(result.is_ok());
        let (key, val) = result.unwrap();
        assert_eq!(key, "");
        assert_eq!(val, "value");
    }

    #[test]
    fn test_parse_key_val_empty_value() {
        let result = parse_key_val::<String, String>("key=");
        assert!(result.is_ok());
        let (key, val) = result.unwrap();
        assert_eq!(key, "key");
        assert_eq!(val, "");
    }

    #[test]
    fn test_parse_key_val_multiple_equals() {
        let result = parse_key_val::<String, String>("key=value=extra");
        assert!(result.is_ok());
        let (key, val) = result.unwrap();
        assert_eq!(key, "key");
        assert_eq!(val, "value=extra");
    }
}
