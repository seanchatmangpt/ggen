//! Environment management - domain layer
//!
//! Pure business logic for environment variable and directory management.

// NOTE: clap::Parser removed - domain layer must be CLI-agnostic
use ggen_utils::error::Result;
use std::collections::HashMap;
use std::path::PathBuf;

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

// CLI wrapper - MOVED TO ggen-cli crate
// Domain layer must not depend on clap
// See ggen-cli/src/cmds/utils_v3.rs for CLI Args definitions
/*
#[derive(Debug, Parser)]
pub struct EnvInput {
    /// List all GGEN environment variables
    list: bool,

    /// Set environment variable
    set: Vec<String>,

    /// Get environment variable value
    get: Option<String>,

    /// Show environment directories
    show_dirs: bool,

    /// Ensure all directories exist
    ensure_dirs: bool,

    /// Clear cache directory
    clear_cache: bool,
}
*/

// ============================================================================
// CLI Bridge Function (Old Pattern - Commented Out)
// ============================================================================
// This function is being replaced by the v3.4.0 #[verb] pattern in utils_v3.rs
/*
pub fn run_env_old(args: &EnvInput) -> Result<()> {
    let manager = DefaultEnvironmentManager;

    if args.list {
        let vars = manager.list_vars();
        if vars.is_empty() {
            ggen_utils::alert_info!("No GGEN environment variables found");
        } else {
            ggen_utils::alert_info!("GGEN Environment Variables:");
            for (key, value) in vars {
                ggen_utils::alert_info!("  {}={}", key, value);
            }
        }
    }

    if args.show_dirs {
        let env = manager.get_environment()?;
        ggen_utils::alert_info!("GGEN Directories:");
        ggen_utils::alert_info!("  Home: {}", env.home_dir.display());
        ggen_utils::alert_info!("  Templates: {}", env.templates_dir.display());
        ggen_utils::alert_info!("  Cache: {}", env.cache_dir.display());
        ggen_utils::alert_info!("  Config: {}", env.config_dir.display());
    }

    if args.ensure_dirs {
        manager.ensure_directories()?;
        ggen_utils::alert_success!("All directories created");
    }

    if args.clear_cache {
        let count = manager.clear_cache()?;
        ggen_utils::alert_success!("Cleared {} cache files", count);
    }

    if let Some(key) = &args.get {
        match manager.get_var(key) {
            Some(value) => ggen_utils::alert_info!("{}={}", key, value),
            None => ggen_utils::alert_info!("Variable '{}' not found", key),
        }
    }

    for set_arg in &args.set {
        if let Some((key, value)) = set_arg.split_once('=') {
            manager.set_var(key, value)?;
            ggen_utils::alert_success!("Set {}={}", key, value);
        } else {
            ggen_utils::alert_warning!("Invalid format: {}. Use KEY=VALUE", set_arg);
        }
    }

    // Default: show environment info
    if !args.list && !args.show_dirs && !args.ensure_dirs && !args.clear_cache
        && args.get.is_none() && args.set.is_empty() {
        let env = manager.get_environment()?;
        ggen_utils::alert_info!("GGEN Environment:");
        ggen_utils::alert_info!("  Home: {}", env.home_dir.display());
        ggen_utils::alert_info!("  Templates: {}", env.templates_dir.display());
        ggen_utils::alert_info!("  Cache: {}", env.cache_dir.display());
        ggen_utils::alert_info!("  Config: {}", env.config_dir.display());
    }

    Ok(())
}
*/

// ============================================================================
// Domain Functions for v3.4.0 Pattern
// ============================================================================

/// Execute environment listing (pure domain function)
pub async fn execute_env_list() -> Result<HashMap<String, String>> {
    let manager = DefaultEnvironmentManager;
    Ok(manager.list_vars().into_iter().collect())
}

/// Execute environment show directories (pure domain function)
pub async fn execute_env_show_dirs() -> Result<GgenEnvironment> {
    let manager = DefaultEnvironmentManager;
    manager.get_environment()
}

/// Execute environment ensure directories (pure domain function)
pub async fn execute_env_ensure_dirs() -> Result<()> {
    let manager = DefaultEnvironmentManager;
    manager.ensure_directories()
}

/// Execute environment clear cache (pure domain function)
pub async fn execute_env_clear_cache() -> Result<usize> {
    let manager = DefaultEnvironmentManager;
    manager.clear_cache()
}

/// Execute environment get variable (pure domain function)
pub async fn execute_env_get(key: String) -> Result<Option<String>> {
    let manager = DefaultEnvironmentManager;
    Ok(manager.get_var(&key))
}

/// Execute environment set variable (pure domain function)
pub async fn execute_env_set(key: String, value: String) -> Result<()> {
    let manager = DefaultEnvironmentManager;
    manager.set_var(&key, &value)
}
