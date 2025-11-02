//! Environment management - Domain layer
//!
//! Pure business logic for managing ggen environment variables, directories,
//! and configuration.

use ggen_utils::error::Result;
use std::collections::HashMap;
use std::path::PathBuf;

/// Ggen environment configuration
#[derive(Debug, Clone)]
pub struct GgenEnvironment {
    pub home_dir: PathBuf,
    pub templates_dir: PathBuf,
    pub cache_dir: PathBuf,
    pub config_dir: PathBuf,
    pub config: EnvironmentConfig,
}

/// Environment configuration
#[derive(Debug, Clone)]
pub struct EnvironmentConfig {
    pub editor: Option<String>,
    pub default_template: Option<String>,
    pub custom_vars: HashMap<String, String>,
}

impl Default for EnvironmentConfig {
    fn default() -> Self {
        Self {
            editor: std::env::var("EDITOR").ok(),
            default_template: None,
            custom_vars: HashMap::new(),
        }
    }
}

/// Trait for managing ggen environment
pub trait EnvironmentManager {
    fn get_home_dir(&self) -> Result<PathBuf>;
    fn ensure_directories(&self) -> Result<()>;
    fn get_environment(&self) -> Result<GgenEnvironment>;
    fn set_var(&self, key: &str, value: &str) -> Result<()>;
    fn get_var(&self, key: &str) -> Option<String>;
    fn list_vars(&self) -> HashMap<String, String>;
    fn clear_cache(&self) -> Result<usize>;
}

/// Default implementation for environment management
pub struct DefaultEnvironmentManager;

impl EnvironmentManager for DefaultEnvironmentManager {
    fn get_home_dir(&self) -> Result<PathBuf> {
        if let Ok(home) = std::env::var("GGEN_HOME") {
            return Ok(PathBuf::from(home));
        }

        let home = dirs::home_dir()
            .ok_or_else(|| ggen_utils::error::Error::new("Cannot find home directory"))?;

        Ok(home.join(".ggen"))
    }

    fn ensure_directories(&self) -> Result<()> {
        let home = self.get_home_dir()?;

        let dirs = vec![
            home.join("templates"),
            home.join("cache"),
            home.join("config"),
        ];

        for dir in dirs {
            std::fs::create_dir_all(&dir)?;
        }

        Ok(())
    }

    fn get_environment(&self) -> Result<GgenEnvironment> {
        let home_dir = self.get_home_dir()?;
        let templates_dir = home_dir.join("templates");
        let cache_dir = home_dir.join("cache");
        let config_dir = home_dir.join("config");

        let config = self.load_config(&config_dir)?;

        Ok(GgenEnvironment {
            home_dir,
            templates_dir,
            cache_dir,
            config_dir,
            config,
        })
    }

    fn set_var(&self, key: &str, value: &str) -> Result<()> {
        if !key.starts_with("GGEN_") {
            return Err(ggen_utils::error::Error::new(
                "Environment variable must start with GGEN_",
            ));
        }

        std::env::set_var(key, value);
        self.persist_var(key, value)?;

        Ok(())
    }

    fn get_var(&self, key: &str) -> Option<String> {
        std::env::var(key).ok()
    }

    fn list_vars(&self) -> HashMap<String, String> {
        std::env::vars()
            .filter(|(k, _)| k.starts_with("GGEN_"))
            .collect()
    }

    fn clear_cache(&self) -> Result<usize> {
        let cache_dir = self.get_home_dir()?.join("cache");

        if !cache_dir.exists() {
            return Ok(0);
        }

        let mut count = 0;
        for entry in std::fs::read_dir(&cache_dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_file() {
                std::fs::remove_file(&path)?;
                count += 1;
            } else if path.is_dir() {
                std::fs::remove_dir_all(&path)?;
                count += 1;
            }
        }

        Ok(count)
    }
}

impl DefaultEnvironmentManager {
    fn load_config(&self, config_dir: &PathBuf) -> Result<EnvironmentConfig> {
        let config_file = config_dir.join("config.toml");

        if !config_file.exists() {
            return Ok(EnvironmentConfig::default());
        }

        let content = std::fs::read_to_string(&config_file)?;
        let mut config = EnvironmentConfig::default();

        for line in content.lines() {
            if let Some((key, value)) = line.split_once('=') {
                let key = key.trim();
                let value = value.trim().trim_matches('"');

                match key {
                    "editor" => config.editor = Some(value.to_string()),
                    "default_template" => config.default_template = Some(value.to_string()),
                    _ => {
                        config.custom_vars.insert(key.to_string(), value.to_string());
                    }
                }
            }
        }

        Ok(config)
    }

    fn persist_var(&self, key: &str, value: &str) -> Result<()> {
        let config_dir = self.get_home_dir()?.join("config");
        std::fs::create_dir_all(&config_dir)?;

        let env_file = config_dir.join("env");

        let mut lines = if env_file.exists() {
            std::fs::read_to_string(&env_file)?
                .lines()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        } else {
            Vec::new()
        };

        let entry = format!("{}={}", key, value);
        if let Some(pos) = lines.iter().position(|l| l.starts_with(&format!("{}=", key))) {
            lines[pos] = entry;
        } else {
            lines.push(entry);
        }

        std::fs::write(&env_file, lines.join("\n"))?;

        Ok(())
    }
}

pub fn init_environment() -> Result<GgenEnvironment> {
    let manager = DefaultEnvironmentManager;
    manager.ensure_directories()?;
    manager.get_environment()
}

pub fn get_ggen_home() -> Result<PathBuf> {
    let manager = DefaultEnvironmentManager;
    manager.get_home_dir()
}

pub fn ensure_ggen_dirs() -> Result<()> {
    let manager = DefaultEnvironmentManager;
    manager.ensure_directories()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_environment_config_default() {
        let config = EnvironmentConfig::default();
        assert_eq!(config.default_template, None);
        assert!(config.custom_vars.is_empty());
    }

    #[test]
    fn test_environment_manager_get_home_dir() {
        let manager = DefaultEnvironmentManager;
        let result = manager.get_home_dir();
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_ggen_home() {
        let result = get_ggen_home();
        assert!(result.is_ok());
    }
}
