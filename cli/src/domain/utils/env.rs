//! Environment management - domain layer
//!
//! Pure business logic for environment variable and directory management.

use clap::Parser;
use ggen_utils::error::Result;
use std::path::PathBuf;
use std::collections::HashMap;

// Domain types
#[derive(Debug, Clone)]
pub struct GgenEnvironment {
    pub home_dir: PathBuf,
    pub templates_dir: PathBuf,
    pub cache_dir: PathBuf,
    pub config_dir: PathBuf,
    pub config: GgenConfig,
}

#[derive(Debug, Clone)]
pub struct GgenConfig {
    pub editor: Option<String>,
    pub default_template: Option<String>,
    pub custom_vars: HashMap<String, String>,
}

pub trait EnvironmentManager {
    fn get_environment(&self) -> Result<GgenEnvironment>;
    fn set_var(&self, key: &str, value: &str) -> Result<()>;
    fn get_var(&self, key: &str) -> Option<String>;
    fn list_vars(&self) -> Vec<(String, String)>;
    fn ensure_directories(&self) -> Result<()>;
    fn clear_cache(&self) -> Result<usize>;
}

#[derive(Debug, Clone)]
pub struct DefaultEnvironmentManager;

impl EnvironmentManager for DefaultEnvironmentManager {
    fn get_environment(&self) -> Result<GgenEnvironment> {
        let home_dir = std::env::var("HOME")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("."));

        Ok(GgenEnvironment {
            home_dir: home_dir.clone(),
            templates_dir: home_dir.join(".ggen/templates"),
            cache_dir: home_dir.join(".ggen/cache"),
            config_dir: home_dir.join(".ggen/config"),
            config: GgenConfig {
                editor: None,
                default_template: None,
                custom_vars: HashMap::new(),
            },
        })
    }

    fn set_var(&self, key: &str, value: &str) -> Result<()> {
        std::env::set_var(key, value);
        Ok(())
    }

    fn get_var(&self, key: &str) -> Option<String> {
        std::env::var(key).ok()
    }

    fn list_vars(&self) -> Vec<(String, String)> {
        std::env::vars()
            .filter(|(k, _)| k.starts_with("GGEN_"))
            .collect()
    }

    fn ensure_directories(&self) -> Result<()> {
        let env = self.get_environment()?;
        std::fs::create_dir_all(&env.templates_dir)?;
        std::fs::create_dir_all(&env.cache_dir)?;
        std::fs::create_dir_all(&env.config_dir)?;
        Ok(())
    }

    fn clear_cache(&self) -> Result<usize> {
        let env = self.get_environment()?;
        let mut count = 0;
        if env.cache_dir.exists() {
            for entry in std::fs::read_dir(&env.cache_dir)? {
                let entry = entry?;
                std::fs::remove_file(entry.path())?;
                count += 1;
            }
        }
        Ok(count)
    }
}

// CLI wrapper
#[derive(Debug, Parser)]
pub struct EnvArgs {
    /// List all GGEN environment variables
    #[arg(short, long)]
    list: bool,

    /// Set environment variable
    #[arg(short, long, value_name = "KEY=VALUE", num_args = 0..)]
    set: Vec<String>,

    /// Get environment variable value
    #[arg(short, long)]
    get: Option<String>,

    /// Show environment directories
    #[arg(long)]
    show_dirs: bool,

    /// Ensure all directories exist
    #[arg(long)]
    ensure_dirs: bool,

    /// Clear cache directory
    #[arg(long)]
    clear_cache: bool,
}

pub fn run(args: &EnvArgs) -> Result<()> {
    let manager = DefaultEnvironmentManager;

    if args.list {
        let vars = manager.list_vars();
        if vars.is_empty() {
            println!("No GGEN environment variables found");
        } else {
            println!("GGEN Environment Variables:");
            for (key, value) in vars {
                println!("  {}={}", key, value);
            }
        }
    }

    if args.show_dirs {
        let env = manager.get_environment()?;
        println!("GGEN Directories:");
        println!("  Home: {}", env.home_dir.display());
        println!("  Templates: {}", env.templates_dir.display());
        println!("  Cache: {}", env.cache_dir.display());
        println!("  Config: {}", env.config_dir.display());
    }

    if args.ensure_dirs {
        manager.ensure_directories()?;
        println!("✓ All directories created");
    }

    if args.clear_cache {
        let count = manager.clear_cache()?;
        println!("✓ Cleared {} cache files", count);
    }

    if let Some(key) = &args.get {
        match manager.get_var(key) {
            Some(value) => println!("{}={}", key, value),
            None => println!("Variable '{}' not found", key),
        }
    }

    for set_arg in &args.set {
        if let Some((key, value)) = set_arg.split_once('=') {
            manager.set_var(key, value)?;
            println!("✓ Set {}={}", key, value);
        } else {
            eprintln!("Invalid format: {}. Use KEY=VALUE", set_arg);
        }
    }

    // Default: show environment info
    if !args.list && !args.show_dirs && !args.ensure_dirs && !args.clear_cache
        && args.get.is_none() && args.set.is_empty() {
        let env = manager.get_environment()?;
        println!("GGEN Environment:");
        println!("  Home: {}", env.home_dir.display());
        println!("  Templates: {}", env.templates_dir.display());
        println!("  Cache: {}", env.cache_dir.display());
        println!("  Config: {}", env.config_dir.display());
    }

    Ok(())
}
