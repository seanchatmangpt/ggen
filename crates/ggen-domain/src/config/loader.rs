//! Configuration loader for ggen.toml

use super::validation::{ConfigValidator, ValidationResult};
use ggen_utils::error::Error as DomainError;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Complete ggen.toml configuration structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GgenConfig {
    pub project: ProjectMetadata,
    #[serde(default)]
    pub ontology: OntologyConfig,
    #[serde(default)]
    pub ai: AiConfig,
    #[serde(default)]
    pub templates: TemplatesConfig,
    #[serde(default)]
    pub rdf: RdfConfig,
    #[serde(default)]
    pub sparql: SparqlConfig,
    #[serde(default)]
    pub lifecycle: LifecycleConfig,
    #[serde(default)]
    pub security: SecurityConfig,
    #[serde(default)]
    pub performance: PerformanceConfig,
    #[serde(default)]
    pub logging: LoggingConfig,
    #[serde(default)]
    pub dependencies: DependenciesConfig,
    #[serde(default)]
    pub generation: GenerationConfig,
    #[serde(default)]
    pub zai: ZaiConfig,
    #[serde(default)]
    pub mcp: McpConfig,
    #[serde(default)]
    pub a2a: A2aConfig,
    #[serde(default)]
    pub marketplace: MarketplaceConfig,
    #[serde(default)]
    pub codeowners: CodeownersConfig,
    #[serde(default)]
    pub env: HashMap<String, HashMap<String, toml::Value>>,
}

/// Project metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectMetadata {
    pub name: String,
    pub version: String,
    pub description: String,
    pub authors: Vec<String>,
    pub license: String,
    pub repository: String,
}

/// Ontology configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologyConfig {
    pub source: String,
    #[serde(default = "default_ontology_base_uri")]
    pub base_uri: String,
    #[serde(default = "default_ontology_format")]
    pub format: String,
}

fn default_ontology_base_uri() -> String {
    "https://ggen.dev/".to_string()
}

fn default_ontology_format() -> String {
    "turtle".to_string()
}

impl Default for OntologyConfig {
    fn default() -> Self {
        Self {
            source: "ontology.ttl".to_string(),
            base_uri: default_ontology_base_uri(),
            format: default_ontology_format(),
        }
    }
}

/// AI configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AiConfig {
    #[serde(default = "default_ai_provider")]
    pub provider: String,
    pub model: String,
    #[serde(default = "default_ai_temperature")]
    pub temperature: f64,
    #[serde(default = "default_ai_max_tokens")]
    pub max_tokens: u32,
    #[serde(default = "default_ai_timeout")]
    pub timeout: u64,
    #[serde(default)]
    pub base_url: Option<String>,
}

fn default_ai_provider() -> String {
    "anthropic".to_string()
}

fn default_ai_temperature() -> f64 {
    0.5
}

fn default_ai_max_tokens() -> u32 {
    8000
}

fn default_ai_timeout() -> u64 {
    120
}

impl Default for AiConfig {
    fn default() -> Self {
        Self {
            provider: default_ai_provider(),
            model: "claude-3-opus-20240229".to_string(),
            temperature: default_ai_temperature(),
            max_tokens: default_ai_max_tokens(),
            timeout: default_ai_timeout(),
            base_url: None,
        }
    }
}

/// Templates configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplatesConfig {
    #[serde(default = "default_templates_directory")]
    pub directory: String,
    #[serde(default = "default_true")]
    pub backup_enabled: bool,
    #[serde(default = "default_true")]
    pub idempotent: bool,
}

fn default_templates_directory() -> String {
    "templates".to_string()
}

impl Default for TemplatesConfig {
    fn default() -> Self {
        Self {
            directory: default_templates_directory(),
            backup_enabled: true,
            idempotent: true,
        }
    }
}

/// RDF configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RdfConfig {
    #[serde(default = "default_rdf_base_uri")]
    pub base_uri: String,
    #[serde(default = "default_rdf_format")]
    pub default_format: String,
    #[serde(default = "default_true")]
    pub cache_queries: bool,
    #[serde(default = "default_rdf_store_path")]
    pub store_path: String,
    #[serde(default)]
    pub prefixes: HashMap<String, String>,
}

fn default_rdf_base_uri() -> String {
    "https://ggen.dev/".to_string()
}

fn default_rdf_format() -> String {
    "turtle".to_string()
}

fn default_rdf_store_path() -> String {
    ".ggen/rdf-store".to_string()
}

impl Default for RdfConfig {
    fn default() -> Self {
        Self {
            base_uri: default_rdf_base_uri(),
            default_format: default_rdf_format(),
            cache_queries: true,
            store_path: default_rdf_store_path(),
            prefixes: HashMap::new(),
        }
    }
}

/// SPARQL configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SparqlConfig {
    #[serde(default = "default_sparql_timeout")]
    pub timeout: u64,
    #[serde(default = "default_sparql_max_results")]
    pub max_results: u32,
    #[serde(default = "default_true")]
    pub cache_enabled: bool,
    #[serde(default = "default_sparql_cache_ttl")]
    pub cache_ttl: u64,
}

fn default_sparql_timeout() -> u64 {
    60
}

fn default_sparql_max_results() -> u32 {
    5000
}

fn default_sparql_cache_ttl() -> u64 {
    7200
}

impl Default for SparqlConfig {
    fn default() -> Self {
        Self {
            timeout: default_sparql_timeout(),
            max_results: default_sparql_max_results(),
            cache_enabled: true,
            cache_ttl: default_sparql_cache_ttl(),
        }
    }
}

/// Lifecycle configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LifecycleConfig {
    #[serde(default = "default_true")]
    pub enabled: bool,
    #[serde(default = "default_lifecycle_config_file")]
    pub config_file: String,
    #[serde(default = "default_lifecycle_cache_dir")]
    pub cache_directory: String,
    #[serde(default = "default_lifecycle_state_file")]
    pub state_file: String,
    #[serde(default)]
    pub phases: LifecyclePhases,
}

fn default_lifecycle_config_file() -> String {
    ".ggen/lifecycle.toml".to_string()
}

fn default_lifecycle_cache_dir() -> String {
    ".ggen/cache".to_string()
}

fn default_lifecycle_state_file() -> String {
    ".ggen/state.json".to_string()
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct LifecyclePhases {
    #[serde(default)]
    pub pre_generate: PhaseConfig,
    #[serde(default)]
    pub post_generate: PhaseConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct PhaseConfig {
    #[serde(default)]
    pub scripts: Vec<String>,
}

impl Default for LifecycleConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            config_file: default_lifecycle_config_file(),
            cache_directory: default_lifecycle_cache_dir(),
            state_file: default_lifecycle_state_file(),
            phases: Default::default(),
        }
    }
}

/// Security configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityConfig {
    #[serde(default)]
    pub allowed_domains: Vec<String>,
    #[serde(default = "default_max_file_size")]
    pub max_file_size: u64,
    #[serde(default = "default_true")]
    pub validate_ssl: bool,
}

fn default_max_file_size() -> u64 {
    104_857_600 // 100MB
}

impl Default for SecurityConfig {
    fn default() -> Self {
        Self {
            allowed_domains: vec![
                "schema.org".to_string(),
                "w3.org".to_string(),
                "ggen.dev".to_string(),
                "github.com".to_string(),
            ],
            max_file_size: default_max_file_size(),
            validate_ssl: true,
        }
    }
}

/// Performance configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceConfig {
    #[serde(default = "default_true")]
    pub parallel_generation: bool,
    #[serde(default = "default_max_workers")]
    pub max_workers: usize,
    #[serde(default = "default_true")]
    pub cache_templates: bool,
    #[serde(default = "default_true")]
    pub incremental_build: bool,
}

fn default_max_workers() -> usize {
    8
}

impl Default for PerformanceConfig {
    fn default() -> Self {
        Self {
            parallel_generation: true,
            max_workers: default_max_workers(),
            cache_templates: true,
            incremental_build: true,
        }
    }
}

/// Logging configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LoggingConfig {
    #[serde(default = "default_log_level")]
    pub level: String,
    #[serde(default = "default_log_format")]
    pub format: String,
    #[serde(default = "default_log_output")]
    pub output: String,
}

fn default_log_level() -> String {
    "info".to_string()
}

fn default_log_format() -> String {
    "pretty".to_string()
}

fn default_log_output() -> String {
    "stderr".to_string()
}

impl Default for LoggingConfig {
    fn default() -> Self {
        Self {
            level: default_log_level(),
            format: default_log_format(),
            output: default_log_output(),
        }
    }
}

/// Dependencies configuration
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct DependenciesConfig {
    #[serde(default)]
    pub crates: Vec<CrateDependency>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CrateDependency {
    pub name: String,
    pub version: String,
    #[serde(default)]
    pub optional: bool,
}

/// Generation configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationConfig {
    #[serde(default = "default_true")]
    pub enabled: bool,
    #[serde(default)]
    pub rules: Vec<GenerationRule>,
    #[serde(default)]
    pub protected_paths: Vec<String>,
    #[serde(default)]
    pub regenerate_paths: Vec<String>,
    #[serde(default = "default_generated_header")]
    pub generated_header: String,
    #[serde(default = "default_true")]
    pub require_confirmation: bool,
    #[serde(default = "default_true")]
    pub backup_before_write: bool,
    #[serde(default)]
    pub poka_yoke: PokaYokeConfig,
}

fn default_generated_header() -> String {
    r#"
// ============================================================
// DO NOT EDIT THIS FILE
//
// This file is auto-generated by ggen from RDF ontology.
// Any manual changes will be OVERWRITTEN on regeneration.
//
// Regenerate with: ggen generate
// ============================================================
"#
    .to_string()
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct PokaYokeConfig {
    #[serde(default = "default_true")]
    pub warning_headers: bool,
    #[serde(default)]
    pub gitignore_generated: bool,
    #[serde(default = "default_true")]
    pub gitattributes_generated: bool,
    #[serde(default)]
    pub validate_imports: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationRule {
    pub name: String,
    pub source: String,
    pub output: String,
    #[serde(default)]
    pub templates: Option<String>,
}

impl Default for GenerationConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            rules: Vec::new(),
            protected_paths: Vec::new(),
            regenerate_paths: Vec::new(),
            generated_header: default_generated_header(),
            require_confirmation: true,
            backup_before_write: true,
            poka_yoke: Default::default(),
        }
    }
}

/// ZAI configuration (local GenAI integration)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ZaiConfig {
    #[serde(default = "default_zai_provider")]
    pub provider: String,
    #[serde(default = "default_zai_model")]
    pub model: String,
    #[serde(default = "default_zai_base_url")]
    pub base_url: String,
    #[serde(default)]
    pub api_key: Option<String>,
    #[serde(default = "default_temperature")]
    pub temperature: f64,
    #[serde(default = "default_max_tokens")]
    pub max_tokens: u32,
    #[serde(default = "default_timeout")]
    pub timeout: u64,
    #[serde(default = "default_true")]
    pub cache_enabled: bool,
    #[serde(default = "default_cache_ttl")]
    pub cache_ttl: u64,
}

fn default_zai_provider() -> String {
    "zai".to_string()
}

fn default_zai_model() -> String {
    "zai-chat".to_string()
}

fn default_zai_base_url() -> String {
    "http://localhost:8080".to_string()
}

fn default_temperature() -> f64 {
    0.7
}

fn default_max_tokens() -> u32 {
    8000
}

fn default_timeout() -> u64 {
    120
}

fn default_cache_ttl() -> u64 {
    3600
}

impl Default for ZaiConfig {
    fn default() -> Self {
        Self {
            provider: default_zai_provider(),
            model: default_zai_model(),
            base_url: default_zai_base_url(),
            api_key: None,
            temperature: default_temperature(),
            max_tokens: default_max_tokens(),
            timeout: default_timeout(),
            cache_enabled: true,
            cache_ttl: default_cache_ttl(),
        }
    }
}

/// MCP configuration
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct McpConfig {
    #[serde(default = "default_true")]
    pub enabled: bool,
    #[serde(default)]
    pub servers: Vec<McpServerDef>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpServerDef {
    pub name: String,
    pub command: String,
    #[serde(default)]
    pub args: Vec<String>,
}

/// A2A configuration
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct A2aConfig {
    #[serde(default)]
    pub server: A2aServerDef,
    #[serde(default)]
    pub agents: Vec<A2aAgentDef>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A2aServerDef {
    #[serde(default = "default_a2a_host")]
    pub host: String,
    #[serde(default = "default_a2a_port")]
    pub port: u16,
}

fn default_a2a_host() -> String {
    "127.0.0.1".to_string()
}

fn default_a2a_port() -> u16 {
    8080
}

impl Default for A2aServerDef {
    fn default() -> Self {
        Self {
            host: default_a2a_host(),
            port: default_a2a_port(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A2aAgentDef {
    pub name: String,
    pub agent_type: String,
    #[serde(default)]
    pub description: Option<String>,
}

/// Marketplace configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MarketplaceConfig {
    #[serde(default = "default_marketplace_url")]
    pub registry_url: String,
    #[serde(default = "default_true")]
    pub cache_packages: bool,
    #[serde(default = "default_true")]
    pub verify_signatures: bool,
}

fn default_marketplace_url() -> String {
    "https://marketplace.ggen.dev".to_string()
}

impl Default for MarketplaceConfig {
    fn default() -> Self {
        Self {
            registry_url: default_marketplace_url(),
            cache_packages: true,
            verify_signatures: true,
        }
    }
}

/// CODEOWNERS configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeownersConfig {
    #[serde(default = "default_true")]
    pub enabled: bool,
    #[serde(default)]
    pub source_dirs: Vec<String>,
    #[serde(default)]
    pub base_dirs: Vec<String>,
    #[serde(default = "default_codeowners_output")]
    pub output_path: String,
    #[serde(default)]
    pub auto_regenerate: bool,
}

fn default_codeowners_output() -> String {
    ".github/CODEOWNERS".to_string()
}

impl Default for CodeownersConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            source_dirs: vec!["ontology".to_string(), "ontologies".to_string()],
            base_dirs: vec![
                "ontology".to_string(),
                "ontologies".to_string(),
                "src/generated".to_string(),
                "src/domain".to_string(),
                "crates/*/src/generated".to_string(),
                "crates/*/src/domain".to_string(),
            ],
            output_path: default_codeowners_output(),
            auto_regenerate: false,
        }
    }
}

fn default_true() -> bool {
    true
}

/// Configuration loader
pub struct ConfigLoader {
    validator: ConfigValidator,
}

impl ConfigLoader {
    pub fn new() -> Self {
        Self {
            validator: ConfigValidator::new(),
        }
    }

    /// Load ggen.toml with environment override
    pub fn load_with_env(
        &self, config_path: &Path, environment: &str,
    ) -> Result<(GgenConfig, ValidationResult), ConfigError> {
        let mut config = self.load_base_config(config_path)?;
        self.apply_environment_overrides(&mut config, environment)?;
        let validation = self.validator.validate_ggen_config(&config);
        Ok((config, validation))
    }

    /// Load ggen.toml without environment override
    pub fn load(&self, config_path: &Path) -> Result<(GgenConfig, ValidationResult), ConfigError> {
        let config = self.load_base_config(config_path)?;
        let validation = self.validator.validate_ggen_config(&config);
        Ok((config, validation))
    }

    fn load_base_config(&self, path: &Path) -> Result<GgenConfig, ConfigError> {
        let content = std::fs::read_to_string(path).map_err(|e| ConfigError::IoError {
            path: path.to_path_buf(),
            source: e,
        })?;

        toml::from_str(&content).map_err(|e| ConfigError::ParseError {
            path: path.to_path_buf(),
            source: e,
        })
    }

    fn apply_environment_overrides(
        &self, config: &mut GgenConfig, environment: &str,
    ) -> Result<(), ConfigError> {
        let overrides = config.env.get(environment).cloned().unwrap_or_default();

        for (key_path, value) in overrides {
            self.apply_override(config, &key_path, value)?;
        }

        Ok(())
    }

    fn apply_override(
        &self, config: &mut GgenConfig, key_path: &str, value: toml::Value,
    ) -> Result<(), ConfigError> {
        let parts: Vec<&str> = key_path.split('.').collect();

        match parts.as_slice() {
            ["ai", "provider"] => {
                if let Some(s) = value.as_str() {
                    config.ai.provider = s.to_string();
                }
            }
            ["ai", "model"] => {
                if let Some(s) = value.as_str() {
                    config.ai.model = s.to_string();
                }
            }
            ["ai", "temperature"] => {
                if let Some(f) = value.as_float() {
                    config.ai.temperature = f;
                }
            }
            ["ai", "base_url"] => {
                if let Some(s) = value.as_str() {
                    config.ai.base_url = Some(s.to_string());
                }
            }
            ["zai", "provider"] => {
                if let Some(s) = value.as_str() {
                    config.zai.provider = s.to_string();
                }
            }
            ["zai", "model"] => {
                if let Some(s) = value.as_str() {
                    config.zai.model = s.to_string();
                }
            }
            ["zai", "base_url"] => {
                if let Some(s) = value.as_str() {
                    config.zai.base_url = s.to_string();
                }
            }
            ["logging", "level"] => {
                if let Some(s) = value.as_str() {
                    config.logging.level = s.to_string();
                }
            }
            ["logging", "format"] => {
                if let Some(s) = value.as_str() {
                    config.logging.format = s.to_string();
                }
            }
            ["performance", "max_workers"] => {
                if let Some(n) = value.as_integer() {
                    config.performance.max_workers = n as usize;
                }
            }
            ["sparql", "cache_ttl"] => {
                if let Some(n) = value.as_integer() {
                    config.sparql.cache_ttl = n as u64;
                }
            }
            _ => {
                // Unknown override - ignore
            }
        }

        Ok(())
    }
}

impl Default for ConfigLoader {
    fn default() -> Self {
        Self::new()
    }
}

/// Configuration error type
#[derive(Debug, thiserror::Error)]
pub enum ConfigError {
    #[error("IO error reading config at {path:?}: {source}")]
    IoError {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Parse error in config at {path:?}: {source}")]
    ParseError {
        path: PathBuf,
        #[source]
        source: toml::de::Error,
    },

    #[error("Validation error: {0}")]
    ValidationError(String),

    #[error("Invalid environment override: {0}")]
    InvalidOverride(String),
}

impl From<ConfigError> for DomainError {
    fn from(err: ConfigError) -> Self {
        DomainError::invalid_input(err.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_loader_default() {
        use crate::config::validation::ConfigValidator;
        let validator = ConfigValidator::new();
        assert!(validator.allowed_licenses().len() > 0);
    }

    #[test]
    fn test_ontology_config_default() {
        let config = OntologyConfig::default();
        assert_eq!(config.base_uri, "https://ggen.dev/");
        assert_eq!(config.format, "turtle");
    }

    #[test]
    fn test_ai_config_default() {
        let config = AiConfig::default();
        assert_eq!(config.provider, "anthropic");
        assert_eq!(config.temperature, 0.5);
        assert_eq!(config.max_tokens, 8000);
    }

    #[test]
    fn test_zai_config_default() {
        let config = ZaiConfig::default();
        assert_eq!(config.provider, "zai");
        assert_eq!(config.model, "zai-chat");
        assert_eq!(config.base_url, "http://localhost:8080");
        assert_eq!(config.temperature, 0.7);
    }

    #[test]
    fn test_generation_config_default() {
        let config = GenerationConfig::default();
        assert!(config.enabled);
        assert!(config.require_confirmation);
        assert!(config.backup_before_write);
    }

    #[test]
    fn test_a2a_server_def_default() {
        let config = A2aServerDef::default();
        assert_eq!(config.host, "127.0.0.1");
        assert_eq!(config.port, 8080);
    }

    #[test]
    fn test_marketplace_config_default() {
        let config = MarketplaceConfig::default();
        assert_eq!(config.registry_url, "https://marketplace.ggen.dev");
        assert!(config.cache_packages);
        assert!(config.verify_signatures);
    }

    #[test]
    fn test_security_config_default() {
        let config = SecurityConfig::default();
        assert_eq!(config.max_file_size, 104_857_600);
        assert!(config.validate_ssl);
        assert!(!config.allowed_domains.is_empty());
    }

    #[test]
    fn test_performance_config_default() {
        let config = PerformanceConfig::default();
        assert!(config.parallel_generation);
        assert_eq!(config.max_workers, 8);
        assert!(config.incremental_build);
    }

    #[test]
    fn test_logging_config_default() {
        let config = LoggingConfig::default();
        assert_eq!(config.level, "info");
        assert_eq!(config.format, "pretty");
        assert_eq!(config.output, "stderr");
    }
}
