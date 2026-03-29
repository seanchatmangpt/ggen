//! Configuration validation

use super::loader::GgenConfig;
use std::fmt;

/// Configuration validator
pub struct ConfigValidator {
    allowed_licenses: Vec<String>,
    allowed_log_levels: Vec<String>,
    allowed_ai_providers: Vec<String>,
    allowed_ontology_formats: Vec<String>,
}

impl ConfigValidator {
    pub fn new() -> Self {
        Self {
            allowed_licenses: vec![
                "MIT".to_string(),
                "Apache-2.0".to_string(),
                "BSD-3-Clause".to_string(),
                "BSD-2-Clause".to_string(),
                "ISC".to_string(),
                "0BSD".to_string(),
            ],
            allowed_log_levels: vec![
                "trace".to_string(),
                "debug".to_string(),
                "info".to_string(),
                "warn".to_string(),
                "error".to_string(),
            ],
            allowed_ai_providers: vec![
                "anthropic".to_string(),
                "openai".to_string(),
                "ollama".to_string(),
                "zai".to_string(),
                "claude".to_string(),
            ],
            allowed_ontology_formats: vec![
                "turtle".to_string(),
                "rdf/xml".to_string(),
                "jsonld".to_string(),
                "ntriples".to_string(),
                "n3".to_string(),
            ],
        }
    }

    pub fn validate_ggen_config(&self, config: &GgenConfig) -> ValidationResult {
        let mut errors = Vec::new();
        let mut warnings = Vec::new();

        self.validate_project_metadata(config, &mut errors, &mut warnings);
        self.validate_ai_config(config, &mut errors, &mut warnings);
        self.validate_zai_config(config, &mut errors, &mut warnings);
        self.validate_logging_config(config, &mut errors, &mut warnings);
        self.validate_ontology_config(config, &mut errors, &mut warnings);
        self.validate_sparql_config(config, &mut errors, &mut warnings);
        self.validate_performance_config(config, &mut errors, &mut warnings);
        self.validate_a2a_config(config, &mut errors, &mut warnings);

        ValidationResult {
            is_valid: errors.is_empty(),
            errors,
            warnings,
        }
    }

    fn validate_project_metadata(
        &self, config: &GgenConfig, errors: &mut Vec<ValidationError>,
        warnings: &mut Vec<ValidationWarning>,
    ) {
        if config.project.name.is_empty() {
            errors.push(ValidationError::missing_required_field("project.name"));
        }

        if config.project.version.is_empty() {
            errors.push(ValidationError::missing_required_field("project.version"));
        } else {
            // Validate semver format
            if !self.is_valid_semver(&config.project.version) {
                warnings.push(ValidationWarning {
                    field: Some("project.version".to_string()),
                    message: "Version does not follow semver format (e.g., 1.0.0)".to_string(),
                });
            }
        }

        if config.project.license.is_empty() {
            errors.push(ValidationError::missing_required_field("project.license"));
        } else if !self.allowed_licenses.contains(&config.project.license) {
            warnings.push(ValidationWarning {
                field: Some("project.license".to_string()),
                message: format!(
                    "License '{}' is not in the recommended list: {}",
                    config.project.license,
                    self.allowed_licenses.join(", ")
                ),
            });
        }

        if config.project.repository.is_empty() {
            warnings.push(ValidationWarning {
                field: Some("project.repository".to_string()),
                message: "Repository URL is empty".to_string(),
            });
        } else if !config.project.repository.starts_with("http://")
            && !config.project.repository.starts_with("https://")
        {
            warnings.push(ValidationWarning {
                field: Some("project.repository".to_string()),
                message: "Repository URL should start with http:// or https://".to_string(),
            });
        }
    }

    fn validate_ai_config(
        &self, config: &GgenConfig, errors: &mut Vec<ValidationError>,
        warnings: &mut Vec<ValidationWarning>,
    ) {
        if config.ai.model.is_empty() {
            errors.push(ValidationError::missing_required_field("ai.model"));
        }

        if !self.allowed_ai_providers.contains(&config.ai.provider) {
            warnings.push(ValidationWarning {
                field: Some("ai.provider".to_string()),
                message: format!(
                    "AI provider '{}' is not in the known list: {}",
                    config.ai.provider,
                    self.allowed_ai_providers.join(", ")
                ),
            });
        }

        if config.ai.temperature < 0.0 || config.ai.temperature > 2.0 {
            errors.push(ValidationError::invalid_value(
                "ai.temperature",
                format!(
                    "Temperature must be between 0.0 and 2.0, got {}",
                    config.ai.temperature
                ),
            ));
        }

        if config.ai.max_tokens == 0 {
            warnings.push(ValidationWarning {
                field: Some("ai.max_tokens".to_string()),
                message: "max_tokens is set to 0, which may limit model output".to_string(),
            });
        }

        if let Some(ref base_url) = config.ai.base_url {
            if !base_url.starts_with("http://") && !base_url.starts_with("https://") {
                errors.push(ValidationError::invalid_value(
                    "ai.base_url",
                    "Must start with http:// or https://",
                ));
            }
        }
    }

    fn validate_zai_config(
        &self, config: &GgenConfig, errors: &mut Vec<ValidationError>,
        warnings: &mut Vec<ValidationWarning>,
    ) {
        if !config.zai.base_url.starts_with("http://")
            && !config.zai.base_url.starts_with("https://")
        {
            errors.push(ValidationError::invalid_value(
                "zai.base_url",
                "Must start with http:// or https://",
            ));
        }

        if config.zai.temperature < 0.0 || config.zai.temperature > 2.0 {
            errors.push(ValidationError::invalid_value(
                "zai.temperature",
                format!(
                    "Temperature must be between 0.0 and 2.0, got {}",
                    config.zai.temperature
                ),
            ));
        }

        if config.zai.timeout == 0 {
            warnings.push(ValidationWarning {
                field: Some("zai.timeout".to_string()),
                message: "timeout is set to 0, requests may hang indefinitely".to_string(),
            });
        }

        if config.zai.api_key.is_some() {
            warnings.push(ValidationWarning {
                field: Some("zai.api_key".to_string()),
                message:
                    "API key is hardcoded in config file. Consider using environment variables."
                        .to_string(),
            });
        }
    }

    fn validate_logging_config(
        &self, config: &GgenConfig, errors: &mut Vec<ValidationError>,
        warnings: &mut Vec<ValidationWarning>,
    ) {
        if !self.allowed_log_levels.contains(&config.logging.level) {
            errors.push(ValidationError::invalid_value(
                "logging.level",
                format!(
                    "Invalid log level '{}'. Must be one of: {}",
                    config.logging.level,
                    self.allowed_log_levels.join(", ")
                ),
            ));
        }

        let allowed_formats = ["pretty", "json", "compact"];
        if !allowed_formats.contains(&config.logging.format.as_str()) {
            warnings.push(ValidationWarning {
                field: Some("logging.format".to_string()),
                message: format!(
                    "Log format '{}' is not standard. Allowed: {}",
                    config.logging.format,
                    allowed_formats.join(", ")
                ),
            });
        }

        let allowed_outputs = ["stdout", "stderr", "file"];
        if !allowed_outputs.contains(&config.logging.output.as_str()) {
            warnings.push(ValidationWarning {
                field: Some("logging.output".to_string()),
                message: format!(
                    "Log output '{}' is not standard. Allowed: {}",
                    config.logging.output,
                    allowed_outputs.join(", ")
                ),
            });
        }
    }

    fn validate_ontology_config(
        &self, config: &GgenConfig, errors: &mut Vec<ValidationError>,
        warnings: &mut Vec<ValidationWarning>,
    ) {
        if config.ontology.source.is_empty() {
            errors.push(ValidationError::missing_required_field("ontology.source"));
        }

        if !self
            .allowed_ontology_formats
            .contains(&config.ontology.format)
        {
            warnings.push(ValidationWarning {
                field: Some("ontology.format".to_string()),
                message: format!(
                    "Ontology format '{}' is not standard. Allowed: {}",
                    config.ontology.format,
                    self.allowed_ontology_formats.join(", ")
                ),
            });
        }

        if !config.ontology.base_uri.ends_with('/') && !config.ontology.base_uri.ends_with('#') {
            warnings.push(ValidationWarning {
                field: Some("ontology.base_uri".to_string()),
                message: "base_uri should end with '/' or '#'".to_string(),
            });
        }
    }

    fn validate_sparql_config(
        &self, config: &GgenConfig, errors: &mut Vec<ValidationError>,
        _warnings: &mut Vec<ValidationWarning>,
    ) {
        if config.sparql.timeout == 0 {
            errors.push(ValidationError::invalid_value(
                "sparql.timeout",
                "SPARQL timeout must be greater than 0",
            ));
        }

        if config.sparql.max_results == 0 {
            errors.push(ValidationError::invalid_value(
                "sparql.max_results",
                "SPARQL max_results must be greater than 0",
            ));
        }
    }

    fn validate_performance_config(
        &self, config: &GgenConfig, errors: &mut Vec<ValidationError>,
        warnings: &mut Vec<ValidationWarning>,
    ) {
        if config.performance.max_workers == 0 {
            errors.push(ValidationError::invalid_value(
                "performance.max_workers",
                "max_workers must be greater than 0",
            ));
        }

        if config.performance.max_workers > 256 {
            warnings.push(ValidationWarning {
                field: Some("performance.max_workers".to_string()),
                message: format!(
                    "max_workers ({}) is very high and may cause system strain",
                    config.performance.max_workers
                ),
            });
        }
    }

    fn validate_a2a_config(
        &self, config: &GgenConfig, errors: &mut Vec<ValidationError>,
        warnings: &mut Vec<ValidationWarning>,
    ) {
        if config.a2a.server.port == 0 {
            errors.push(ValidationError::invalid_value(
                "a2a.server.port",
                "A2A port cannot be 0",
            ));
        }

        if config.a2a.server.port < 1024 {
            warnings.push(ValidationWarning {
                field: Some("a2a.server.port".to_string()),
                message: "A2A port is in the privileged range (< 1024)".to_string(),
            });
        }
    }

    fn is_valid_semver(&self, version: &str) -> bool {
        let parts: Vec<&str> = version.split('.').collect();
        if parts.len() < 3 {
            return false;
        }

        // Check major and minor are numeric
        if parts[0].parse::<u32>().is_err() || parts[1].parse::<u32>().is_err() {
            return false;
        }

        // Check patch (strip pre-release suffix like "-alpha")
        let patch = parts[2].split('-').next().unwrap_or("");
        if patch.parse::<u32>().is_err() {
            return false;
        }

        true
    }

    /// Get the allowed licenses (for testing)
    pub fn allowed_licenses(&self) -> &[String] {
        &self.allowed_licenses
    }
}

impl Default for ConfigValidator {
    fn default() -> Self {
        Self::new()
    }
}

/// Validation result
#[derive(Debug, Clone)]
pub struct ValidationResult {
    pub is_valid: bool,
    pub errors: Vec<ValidationError>,
    pub warnings: Vec<ValidationWarning>,
}

impl ValidationResult {
    /// Check if validation passed without errors
    pub fn is_success(&self) -> bool {
        self.is_valid
    }

    /// Get error count
    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    /// Get warning count
    pub fn warning_count(&self) -> usize {
        self.warnings.len()
    }

    /// Format errors as a single string
    pub fn format_errors(&self) -> String {
        self.errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Format warnings as a single string
    pub fn format_warnings(&self) -> String {
        self.warnings
            .iter()
            .map(|w| w.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

/// Validation error
#[derive(Debug, Clone)]
pub struct ValidationError {
    pub field: String,
    pub message: String,
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.field, self.message)
    }
}

impl ValidationError {
    pub fn missing_required_field(field: impl Into<String>) -> Self {
        Self {
            field: field.into(),
            message: "Missing required field".to_string(),
        }
    }

    pub fn invalid_value(field: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            field: field.into(),
            message: message.into(),
        }
    }
}

/// Validation warning
#[derive(Debug, Clone)]
pub struct ValidationWarning {
    pub field: Option<String>,
    pub message: String,
}

impl fmt::Display for ValidationWarning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.field {
            Some(field) => write!(f, "{}: {}", field, self.message),
            None => write!(f, "{}", self.message),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::loader::{
        A2aConfig, AiConfig, CodeownersConfig, DependenciesConfig, GenerationConfig,
        LifecycleConfig, LoggingConfig, MarketplaceConfig, McpConfig, OntologyConfig,
        PerformanceConfig, ProjectMetadata, RdfConfig, SecurityConfig, SparqlConfig,
        TemplatesConfig, ZaiConfig,
    };
    use super::*;

    fn create_test_config() -> GgenConfig {
        GgenConfig {
            project: ProjectMetadata {
                name: "test-project".to_string(),
                version: "1.0.0".to_string(),
                description: "Test project".to_string(),
                authors: vec!["Test Author".to_string()],
                license: "MIT".to_string(),
                repository: "https://github.com/test/test".to_string(),
            },
            ontology: OntologyConfig::default(),
            ai: AiConfig::default(),
            templates: TemplatesConfig::default(),
            rdf: RdfConfig::default(),
            sparql: SparqlConfig::default(),
            lifecycle: LifecycleConfig::default(),
            security: SecurityConfig::default(),
            performance: PerformanceConfig::default(),
            logging: LoggingConfig::default(),
            dependencies: DependenciesConfig::default(),
            generation: GenerationConfig::default(),
            zai: ZaiConfig::default(),
            mcp: McpConfig::default(),
            a2a: A2aConfig::default(),
            marketplace: MarketplaceConfig::default(),
            codeowners: CodeownersConfig::default(),
            env: Default::default(),
        }
    }

    #[test]
    fn test_validator_new() {
        let validator = ConfigValidator::new();
        assert_eq!(validator.allowed_licenses().len(), 6);
    }

    #[test]
    fn test_valid_config() {
        let validator = ConfigValidator::new();
        let config = create_test_config();
        let result = validator.validate_ggen_config(&config);
        assert!(result.is_valid);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_missing_project_name() {
        let validator = ConfigValidator::new();
        let mut config = create_test_config();
        config.project.name = String::new();

        let result = validator.validate_ggen_config(&config);
        assert!(!result.is_valid);
        assert!(result.errors.iter().any(|e| e.field == "project.name"));
    }

    #[test]
    fn test_invalid_temperature() {
        let validator = ConfigValidator::new();
        let mut config = create_test_config();
        config.ai.temperature = 3.0;

        let result = validator.validate_ggen_config(&config);
        assert!(!result.is_valid);
        assert!(result.errors.iter().any(|e| e.field == "ai.temperature"));
    }

    #[test]
    fn test_invalid_log_level() {
        let validator = ConfigValidator::new();
        let mut config = create_test_config();
        config.logging.level = "invalid".to_string();

        let result = validator.validate_ggen_config(&config);
        assert!(!result.is_valid);
        assert!(result.errors.iter().any(|e| e.field == "logging.level"));
    }

    #[test]
    fn test_warning_unknown_ai_provider() {
        let validator = ConfigValidator::new();
        let mut config = create_test_config();
        config.ai.provider = "unknown-provider".to_string();

        let result = validator.validate_ggen_config(&config);
        assert!(result.is_valid); // Warnings don't make it invalid
        assert!(result.warnings.iter().any(|w| w
            .field
            .as_ref()
            .map(|f| f == "ai.provider")
            .unwrap_or(false)));
    }

    #[test]
    fn test_warning_non_semver_version() {
        let validator = ConfigValidator::new();
        let mut config = create_test_config();
        config.project.version = "1.0".to_string(); // Missing patch

        let result = validator.validate_ggen_config(&config);
        assert!(result.is_valid);
        assert!(result.warnings.iter().any(|w| w
            .field
            .as_ref()
            .map(|f| f == "project.version")
            .unwrap_or(false)));
    }

    #[test]
    fn test_zero_sparql_timeout() {
        let validator = ConfigValidator::new();
        let mut config = create_test_config();
        config.sparql.timeout = 0;

        let result = validator.validate_ggen_config(&config);
        assert!(!result.is_valid);
        assert!(result.errors.iter().any(|e| e.field == "sparql.timeout"));
    }

    #[test]
    fn test_validation_result_format() {
        let result = ValidationResult {
            is_valid: false,
            errors: vec![ValidationError {
                field: "test.field".to_string(),
                message: "Test error".to_string(),
            }],
            warnings: vec![],
        };

        let formatted = result.format_errors();
        assert!(formatted.contains("test.field"));
        assert!(formatted.contains("Test error"));
    }

    #[test]
    fn test_is_valid_semver() {
        let validator = ConfigValidator::new();
        assert!(validator.is_valid_semver("1.0.0"));
        assert!(validator.is_valid_semver("2.1.3"));
        assert!(!validator.is_valid_semver("1.0"));
        assert!(!validator.is_valid_semver("abc"));
        assert!(validator.is_valid_semver("1.0.0-alpha")); // Has extra parts but starts valid
    }

    #[test]
    fn test_validation_error_constructors() {
        let error1 = ValidationError::missing_required_field("test.field");
        assert_eq!(error1.field, "test.field");
        assert!(error1.message.contains("Missing required"));

        let error2 = ValidationError::invalid_value("test.field", "invalid value");
        assert_eq!(error2.field, "test.field");
        assert_eq!(error2.message, "invalid value");
    }
}
