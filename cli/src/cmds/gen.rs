use clap::Args;
use ggen_utils::error::Result;
use std::collections::BTreeMap;
use std::path::PathBuf;

use ggen_core::pipeline::PipelineBuilder;
use ggen_core::resolver::TemplateResolver;
use ggen_core::{CacheManager, LockfileManager};

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

/// Validate and sanitize generation input
fn validate_gen_input(args: &GenArgs) -> Result<()> {
    // Validate template reference is not empty
    if args.template.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Template reference cannot be empty",
        ));
    }
    // Validate template reference length
    if args.template.len() > 500 {
        return Err(ggen_utils::error::Error::new(
            "Template reference too long (max 500 characters)",
        ));
    }
    // Basic path traversal protection
    if args.template.contains("..") {
        return Err(ggen_utils::error::Error::new(
            "Path traversal detected: template reference cannot contain '..'",
        ));
    }
    // Validate template reference format (basic pattern check)
    if !args
        .template
        .chars()
        .all(|c| c.is_alphanumeric() || c == '.' || c == '/' || c == ':' || c == '-' || c == '_')
    {
        return Err(ggen_utils::error::Error::new(
            "Invalid template reference format: only alphanumeric characters, dots, slashes, colons, dashes, and underscores allowed",
        ));
    }
    // Validate output path
    if args
        .out
        .components()
        .any(|c| matches!(c, std::path::Component::ParentDir))
    {
        return Err(ggen_utils::error::Error::new(
            "Path traversal detected: output path cannot contain '..'",
        ));
    }
    // Validate variables format
    for (key, value) in &args.vars {
        if key.trim().is_empty() {
            return Err(ggen_utils::error::Error::new(
                "Variable key cannot be empty",
            ));
        }
        if key.len() > 100 {
            return Err(ggen_utils::error::Error::new(
                "Variable key too long (max 100 characters)",
            ));
        }
        if value.len() > 1000 {
            return Err(ggen_utils::error::Error::new(
                "Variable value too long (max 1000 characters)",
            ));
        }
    }
    Ok(())
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
    validate_gen_input(args)?;
    println!("🔍 Resolving template...");
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
    println!("⚙️  Processing variables...");
    let vars: BTreeMap<String, String> = args.vars.iter().cloned().collect();

    // Render template
    println!("📋 Generating plan...");
    let plan = pipeline.render_file(&template_source.template_path, &vars, args.dry)?;

    if args.dry {
        println!("DRY RUN - Would generate:");
        println!("  Template: {}", template_source.template_path.display());
        println!("  Vars:     {} variables", vars.len());
    } else {
        println!("🚀 Applying changes...");
        plan.apply()?;
        println!("✅ Generation completed successfully!");
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
