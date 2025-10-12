//! Production Readiness Tracking System (80/20 Rule Implementation)
//!
//! This module implements a comprehensive production readiness tracking system
//! that follows the 80/20 rule - focusing on the 20% of features that provide
//! 80% of production value.
//!
//! # Core Philosophy
//!
//! Production readiness is not about implementing 100% of features. It's about:
//! - **Security**: Core authentication, authorization, input validation
//! - **Reliability**: Error handling, logging, monitoring
//! - **Observability**: Metrics, tracing, health checks
//! - **Performance**: Basic optimization, resource management
//! - **Deployability**: Containerization, configuration management
//!
//! # 80/20 Rule Categories
//!
//! ## Critical (20% effort, 80% value)
//! - Authentication & authorization
//! - Error handling & logging
//! - Health checks & monitoring
//! - Basic security (input validation, CSRF)
//! - Database migrations & backups
//!
//! ## Important (30% effort, 15% value)
//! - API documentation
//! - Testing (unit, integration)
//! - Performance monitoring
//! - Configuration management
//! - Deployment automation
//!
//! ## Nice-to-have (50% effort, 5% value)
//! - Advanced caching
//! - Rate limiting
//! - Circuit breakers
//! - Advanced security features
//! - Complex monitoring dashboards

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::Path;
use thiserror::Error;

/// Production readiness status for a component
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ReadinessStatus {
    /// Component is complete and production-ready
    Complete,
    /// Component has placeholder implementation
    Placeholder,
    /// Component is missing entirely
    Missing,
    /// Component exists but needs review
    NeedsReview,
}

/// Production readiness category following 80/20 rule
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum ReadinessCategory {
    /// Critical features (20% effort, 80% value)
    Critical,
    /// Important features (30% effort, 15% value)
    Important,
    /// Nice-to-have features (50% effort, 5% value)
    NiceToHave,
}

impl std::fmt::Display for ReadinessCategory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReadinessCategory::Critical => write!(f, "Critical"),
            ReadinessCategory::Important => write!(f, "Important"),
            ReadinessCategory::NiceToHave => write!(f, "Nice-to-Have"),
        }
    }
}

/// Production readiness requirement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReadinessRequirement {
    /// Unique identifier for this requirement
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Detailed description
    pub description: String,
    /// Category following 80/20 rule
    pub category: ReadinessCategory,
    /// Current implementation status
    pub status: ReadinessStatus,
    /// Files or components this requirement affects
    pub components: Vec<String>,
    /// Dependencies on other requirements
    pub dependencies: Vec<String>,
    /// Estimated effort in hours
    pub effort_hours: Option<u32>,
    /// Priority score (1-10, higher = more critical)
    pub priority: u8,
    /// Date when this requirement was last assessed
    pub last_assessed: DateTime<Utc>,
    /// Notes about implementation status
    pub notes: Option<String>,
}

/// Production readiness report for a project
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReadinessReport {
    /// Project name
    pub project_name: String,
    /// Generation timestamp
    pub generated_at: DateTime<Utc>,
    /// Overall readiness score (0-100)
    pub overall_score: f64,
    /// Requirements by category
    pub by_category: BTreeMap<ReadinessCategory, CategoryReport>,
    /// All requirements
    pub requirements: Vec<ReadinessRequirement>,
    /// Critical path requirements that block production
    pub blocking_requirements: Vec<String>,
    /// Next steps for production readiness
    pub next_steps: Vec<String>,
}

/// Report for a specific readiness category
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CategoryReport {
    /// Category type
    pub category: ReadinessCategory,
    /// Number of requirements in this category
    pub total_requirements: usize,
    /// Number of completed requirements
    pub completed: usize,
    /// Number with placeholder implementations
    pub placeholders: usize,
    /// Number missing entirely
    pub missing: usize,
    /// Category score (0-100)
    pub score: f64,
    /// Requirements in this category
    pub requirements: Vec<String>,
}

/// Error types for production readiness operations
#[derive(Error, Debug)]
pub enum ProductionError {
    #[error("Failed to load readiness configuration: {0}")]
    ConfigLoad(#[from] std::io::Error),

    #[error("Failed to parse readiness configuration: {0}")]
    ConfigParse(#[from] toml::de::Error),

    #[error("Failed to serialize readiness configuration: {0}")]
    ConfigSerialize(#[from] toml::ser::Error),

    #[error("Requirement not found: {0}")]
    RequirementNotFound(String),

    #[error("Circular dependency detected in requirements")]
    CircularDependency,

    #[error("Invalid requirement status transition")]
    InvalidTransition,

    #[error("Project analysis failed: {0}")]
    AnalysisFailed(String),
}

/// Result type for production readiness operations
pub type Result<T> = std::result::Result<T, ProductionError>;

/// Production readiness tracker
pub struct ReadinessTracker {
    /// Project root directory
    project_root: std::path::PathBuf,
    /// Current requirements
    requirements: Vec<ReadinessRequirement>,
    /// Configuration file path
    config_path: std::path::PathBuf,
}

impl ReadinessTracker {
    /// Create a new readiness tracker for a project
    pub fn new<P: AsRef<Path>>(project_root: P) -> Self {
        let project_root = project_root.as_ref().to_path_buf();
        let config_path = project_root.join(".ggen").join("production.toml");

        Self {
            project_root,
            requirements: Vec::new(),
            config_path,
        }
    }

    /// Load production readiness configuration
    pub fn load(&mut self) -> Result<()> {
        if self.config_path.exists() {
            let content = std::fs::read_to_string(&self.config_path)?;
            let config: ProductionConfig = toml::from_str(&content)?;
            self.requirements = config.requirements;
        } else {
            // Initialize with default requirements
            self.requirements = Self::default_requirements();
        }
        Ok(())
    }

    /// Save current requirements to configuration
    pub fn save(&self) -> Result<()> {
        let config = ProductionConfig {
            requirements: self.requirements.clone(),
        };

        // Ensure .ggen directory exists
        std::fs::create_dir_all(self.config_path.parent().unwrap())?;

        let content = toml::to_string_pretty(&config)?;
        std::fs::write(&self.config_path, content)?;
        Ok(())
    }

    /// Generate a comprehensive readiness report
    pub fn generate_report(&self) -> ReadinessReport {
        let mut by_category = BTreeMap::new();
        let mut blocking_requirements = Vec::new();
        let mut next_steps = Vec::new();

        // Group requirements by category
        for req in &self.requirements {
            let category = req.category.clone();
            let entry = by_category
                .entry(category)
                .or_insert_with(|| CategoryReport {
                    category: req.category.clone(),
                    total_requirements: 0,
                    completed: 0,
                    placeholders: 0,
                    missing: 0,
                    score: 0.0,
                    requirements: Vec::new(),
                });

            entry.total_requirements += 1;
            entry.requirements.push(req.id.clone());

            match req.status {
                ReadinessStatus::Complete => entry.completed += 1,
                ReadinessStatus::Placeholder => entry.placeholders += 1,
                ReadinessStatus::Missing => entry.missing += 1,
                ReadinessStatus::NeedsReview => {
                    // Treat as incomplete for scoring
                    if req.category == ReadinessCategory::Critical {
                        blocking_requirements.push(req.id.clone());
                    }
                }
            }
        }

        // Calculate scores for each category
        for (_, report) in &mut by_category {
            let total = report.total_requirements as f64;
            if total > 0.0 {
                let completed_ratio = report.completed as f64 / total;
                let placeholder_ratio = report.placeholders as f64 / total;

                // Weight: Complete = 1.0, Placeholder = 0.5, Missing/Review = 0.0
                report.score = (completed_ratio * 1.0 + placeholder_ratio * 0.5) * 100.0;
            }
        }

        // Calculate overall score (weighted by category importance)
        let critical_score = by_category
            .get(&ReadinessCategory::Critical)
            .map(|r| r.score)
            .unwrap_or(0.0)
            * 0.8; // 80% weight
        let important_score = by_category
            .get(&ReadinessCategory::Important)
            .map(|r| r.score)
            .unwrap_or(0.0)
            * 0.15; // 15% weight
        let nice_score = by_category
            .get(&ReadinessCategory::NiceToHave)
            .map(|r| r.score)
            .unwrap_or(0.0)
            * 0.05; // 5% weight

        let overall_score = critical_score + important_score + nice_score;

        // Generate next steps based on critical missing items
        for req in &self.requirements {
            if req.category == ReadinessCategory::Critical
                && (req.status == ReadinessStatus::Missing
                    || req.status == ReadinessStatus::NeedsReview)
            {
                next_steps.push(format!("Implement {}: {}", req.name, req.description));
            }
        }

        ReadinessReport {
            project_name: "Current Project".to_string(),
            generated_at: Utc::now(),
            overall_score,
            by_category,
            requirements: self.requirements.clone(),
            blocking_requirements,
            next_steps,
        }
    }

    /// Update the status of a requirement
    pub fn update_requirement(
        &mut self, requirement_id: &str, status: ReadinessStatus,
    ) -> Result<()> {
        // Find the requirement first
        let req_index = self
            .requirements
            .iter()
            .position(|r| r.id == requirement_id)
            .ok_or_else(|| ProductionError::RequirementNotFound(requirement_id.to_string()))?;

        // Validate status transition before updating
        let current_status = self.requirements[req_index].status.clone();
        self.validate_transition(&current_status, &status)?;

        // Update the requirement
        self.requirements[req_index].status = status;
        self.requirements[req_index].last_assessed = Utc::now();
        Ok(())
    }

    /// Add a new requirement
    pub fn add_requirement(&mut self, requirement: ReadinessRequirement) -> Result<()> {
        // Check for circular dependencies
        self.validate_dependencies(&requirement)?;

        self.requirements.push(requirement);
        Ok(())
    }

    /// Get requirements by status
    pub fn get_by_status(&self, status: &ReadinessStatus) -> Vec<&ReadinessRequirement> {
        self.requirements
            .iter()
            .filter(|r| &r.status == status)
            .collect()
    }

    /// Get requirements by category
    pub fn get_by_category(&self, category: &ReadinessCategory) -> Vec<&ReadinessRequirement> {
        self.requirements
            .iter()
            .filter(|r| r.category == *category)
            .collect()
    }

    /// Validate status transition
    fn validate_transition(&self, from: &ReadinessStatus, to: &ReadinessStatus) -> Result<()> {
        match (from, to) {
            (ReadinessStatus::Missing, ReadinessStatus::Placeholder) => Ok(()),
            (ReadinessStatus::Missing, ReadinessStatus::Complete) => Ok(()),
            (ReadinessStatus::Placeholder, ReadinessStatus::Complete) => Ok(()),
            (ReadinessStatus::Placeholder, ReadinessStatus::NeedsReview) => Ok(()),
            (ReadinessStatus::Complete, ReadinessStatus::NeedsReview) => Ok(()),
            _ => Err(ProductionError::InvalidTransition),
        }
    }

    /// Validate dependency graph for cycles
    fn validate_dependencies(&self, requirement: &ReadinessRequirement) -> Result<()> {
        let mut visited = std::collections::HashSet::new();
        let mut path = Vec::new();

        for dep_id in &requirement.dependencies {
            if self.has_cycle(dep_id, &mut visited, &mut path) {
                return Err(ProductionError::CircularDependency);
            }
        }
        Ok(())
    }

    /// Check for cycles in dependency graph
    fn has_cycle(
        &self, req_id: &str, visited: &mut std::collections::HashSet<String>,
        path: &mut Vec<String>,
    ) -> bool {
        if path.contains(&req_id.to_string()) {
            return true; // Cycle detected
        }

        if visited.contains(req_id) {
            return false; // Already processed
        }

        let req = match self.requirements.iter().find(|r| r.id == req_id) {
            Some(r) => r,
            None => return false,
        };

        visited.insert(req_id.to_string());
        path.push(req_id.to_string());

        for dep_id in &req.dependencies {
            if self.has_cycle(dep_id, visited, path) {
                return true;
            }
        }

        path.pop();
        false
    }

    /// Get default production requirements following 80/20 rule
    fn default_requirements() -> Vec<ReadinessRequirement> {
        vec![
            // Critical (20% effort, 80% value)
            ReadinessRequirement {
                id: "auth-basic".to_string(),
                name: "Basic Authentication".to_string(),
                description: "User authentication system with login/logout".to_string(),
                category: ReadinessCategory::Critical,
                status: ReadinessStatus::Missing,
                components: vec!["src/auth.rs".to_string(), "templates/auth.tmpl".to_string()],
                dependencies: vec![],
                effort_hours: Some(8),
                priority: 10,
                last_assessed: Utc::now(),
                notes: Some("Core authentication is essential for production".to_string()),
            },
            ReadinessRequirement {
                id: "error-handling".to_string(),
                name: "Comprehensive Error Handling".to_string(),
                description:
                    "Proper error handling with thiserror, no unwrap/expect in production code"
                        .to_string(),
                category: ReadinessCategory::Critical,
                status: ReadinessStatus::Missing,
                components: vec![
                    "src/error.rs".to_string(),
                    "templates/error-handling.tmpl".to_string(),
                ],
                dependencies: vec![],
                effort_hours: Some(12),
                priority: 10,
                last_assessed: Utc::now(),
                notes: Some("Production code must handle errors gracefully".to_string()),
            },
            ReadinessRequirement {
                id: "logging-tracing".to_string(),
                name: "Structured Logging & Tracing".to_string(),
                description: "Comprehensive logging with tracing crate and structured output"
                    .to_string(),
                category: ReadinessCategory::Critical,
                status: ReadinessStatus::Missing,
                components: vec![
                    "src/logging.rs".to_string(),
                    "config/tracing.toml".to_string(),
                ],
                dependencies: vec![],
                effort_hours: Some(6),
                priority: 9,
                last_assessed: Utc::now(),
                notes: Some("Essential for debugging and monitoring in production".to_string()),
            },
            ReadinessRequirement {
                id: "health-checks".to_string(),
                name: "Health Check Endpoints".to_string(),
                description: "HTTP health check endpoints for load balancer and monitoring"
                    .to_string(),
                category: ReadinessCategory::Critical,
                status: ReadinessStatus::Missing,
                components: vec![
                    "src/health.rs".to_string(),
                    "templates/health.tmpl".to_string(),
                ],
                dependencies: vec![],
                effort_hours: Some(4),
                priority: 8,
                last_assessed: Utc::now(),
                notes: Some("Required for container orchestration and monitoring".to_string()),
            },
            ReadinessRequirement {
                id: "input-validation".to_string(),
                name: "Input Validation & Sanitization".to_string(),
                description:
                    "Comprehensive input validation and sanitization to prevent injection attacks"
                        .to_string(),
                category: ReadinessCategory::Critical,
                status: ReadinessStatus::Missing,
                components: vec![
                    "src/validation.rs".to_string(),
                    "templates/validation.tmpl".to_string(),
                ],
                dependencies: vec!["auth-basic".to_string()],
                effort_hours: Some(10),
                priority: 9,
                last_assessed: Utc::now(),
                notes: Some("Critical for security and data integrity".to_string()),
            },
            ReadinessRequirement {
                id: "database-migrations".to_string(),
                name: "Database Schema Migrations".to_string(),
                description: "Automated database schema migrations with rollback capability"
                    .to_string(),
                category: ReadinessCategory::Critical,
                status: ReadinessStatus::Missing,
                components: vec!["migrations/".to_string(), "src/database.rs".to_string()],
                dependencies: vec![],
                effort_hours: Some(8),
                priority: 8,
                last_assessed: Utc::now(),
                notes: Some("Essential for zero-downtime deployments".to_string()),
            },
            // Important (30% effort, 15% value)
            ReadinessRequirement {
                id: "api-documentation".to_string(),
                name: "OpenAPI Documentation".to_string(),
                description: "Complete OpenAPI/Swagger documentation for all endpoints".to_string(),
                category: ReadinessCategory::Important,
                status: ReadinessStatus::Missing,
                components: vec!["docs/api.md".to_string(), "openapi.yaml".to_string()],
                dependencies: vec![],
                effort_hours: Some(12),
                priority: 7,
                last_assessed: Utc::now(),
                notes: Some("Important for API consumers and automated testing".to_string()),
            },
            ReadinessRequirement {
                id: "unit-tests".to_string(),
                name: "Comprehensive Unit Tests".to_string(),
                description: "Unit tests for all public functions with >80% coverage".to_string(),
                category: ReadinessCategory::Important,
                status: ReadinessStatus::Missing,
                components: vec!["tests/unit/".to_string()],
                dependencies: vec![],
                effort_hours: Some(20),
                priority: 7,
                last_assessed: Utc::now(),
                notes: Some("Essential for code quality and refactoring confidence".to_string()),
            },
            ReadinessRequirement {
                id: "integration-tests".to_string(),
                name: "Integration Tests".to_string(),
                description: "Integration tests for component interactions".to_string(),
                category: ReadinessCategory::Important,
                status: ReadinessStatus::Missing,
                components: vec!["tests/integration/".to_string()],
                dependencies: vec!["unit-tests".to_string()],
                effort_hours: Some(16),
                priority: 6,
                last_assessed: Utc::now(),
                notes: Some("Validates component interactions work correctly".to_string()),
            },
            ReadinessRequirement {
                id: "performance-monitoring".to_string(),
                name: "Performance Monitoring".to_string(),
                description: "Basic performance metrics collection and alerting".to_string(),
                category: ReadinessCategory::Important,
                status: ReadinessStatus::Missing,
                components: vec![
                    "src/metrics.rs".to_string(),
                    "config/metrics.toml".to_string(),
                ],
                dependencies: vec!["logging-tracing".to_string()],
                effort_hours: Some(8),
                priority: 6,
                last_assessed: Utc::now(),
                notes: Some("Essential for production performance management".to_string()),
            },
            ReadinessRequirement {
                id: "docker-containerization".to_string(),
                name: "Docker Containerization".to_string(),
                description: "Production-ready Docker containers with multi-stage builds"
                    .to_string(),
                category: ReadinessCategory::Important,
                status: ReadinessStatus::Missing,
                components: vec!["Dockerfile".to_string(), "docker-compose.yml".to_string()],
                dependencies: vec!["health-checks".to_string()],
                effort_hours: Some(6),
                priority: 7,
                last_assessed: Utc::now(),
                notes: Some("Required for consistent deployment across environments".to_string()),
            },
            ReadinessRequirement {
                id: "configuration-management".to_string(),
                name: "Configuration Management".to_string(),
                description: "Environment-based configuration with validation".to_string(),
                category: ReadinessCategory::Important,
                status: ReadinessStatus::Missing,
                components: vec!["config/".to_string(), "src/config.rs".to_string()],
                dependencies: vec![],
                effort_hours: Some(8),
                priority: 7,
                last_assessed: Utc::now(),
                notes: Some("Essential for multi-environment deployments".to_string()),
            },
            // Nice-to-have (50% effort, 5% value)
            ReadinessRequirement {
                id: "rate-limiting".to_string(),
                name: "Rate Limiting".to_string(),
                description: "API rate limiting to prevent abuse".to_string(),
                category: ReadinessCategory::NiceToHave,
                status: ReadinessStatus::Missing,
                components: vec!["src/rate_limit.rs".to_string()],
                dependencies: vec!["auth-basic".to_string()],
                effort_hours: Some(12),
                priority: 4,
                last_assessed: Utc::now(),
                notes: Some("Nice to have for high-traffic applications".to_string()),
            },
            ReadinessRequirement {
                id: "caching-layer".to_string(),
                name: "Advanced Caching".to_string(),
                description: "Redis-based caching with cache invalidation strategies".to_string(),
                category: ReadinessCategory::NiceToHave,
                status: ReadinessStatus::Missing,
                components: vec!["src/cache.rs".to_string(), "config/redis.toml".to_string()],
                dependencies: vec!["performance-monitoring".to_string()],
                effort_hours: Some(16),
                priority: 3,
                last_assessed: Utc::now(),
                notes: Some("Improves performance but adds complexity".to_string()),
            },
            ReadinessRequirement {
                id: "circuit-breaker".to_string(),
                name: "Circuit Breaker Pattern".to_string(),
                description: "Circuit breaker for external service calls".to_string(),
                category: ReadinessCategory::NiceToHave,
                status: ReadinessStatus::Missing,
                components: vec!["src/circuit_breaker.rs".to_string()],
                dependencies: vec!["error-handling".to_string()],
                effort_hours: Some(10),
                priority: 3,
                last_assessed: Utc::now(),
                notes: Some("Improves resilience but adds complexity".to_string()),
            },
            ReadinessRequirement {
                id: "advanced-security".to_string(),
                name: "Advanced Security Features".to_string(),
                description:
                    "Advanced security features like CSRF protection, CORS, security headers"
                        .to_string(),
                category: ReadinessCategory::NiceToHave,
                status: ReadinessStatus::Missing,
                components: vec![
                    "src/security.rs".to_string(),
                    "config/security.toml".to_string(),
                ],
                dependencies: vec!["input-validation".to_string()],
                effort_hours: Some(14),
                priority: 4,
                last_assessed: Utc::now(),
                notes: Some("Additional security hardening for sensitive applications".to_string()),
            },
            ReadinessRequirement {
                id: "monitoring-dashboard".to_string(),
                name: "Monitoring Dashboard".to_string(),
                description: "Grafana/Kibana dashboard for comprehensive monitoring".to_string(),
                category: ReadinessCategory::NiceToHave,
                status: ReadinessStatus::Missing,
                components: vec!["docker/grafana/".to_string(), "docker/kibana/".to_string()],
                dependencies: vec!["performance-monitoring".to_string()],
                effort_hours: Some(20),
                priority: 2,
                last_assessed: Utc::now(),
                notes: Some("Nice for observability but not essential for MVP".to_string()),
            },
        ]
    }
}

/// Production readiness configuration file format
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProductionConfig {
    /// List of all production requirements
    pub requirements: Vec<ReadinessRequirement>,
}

/// Placeholder marker for incomplete implementations
#[derive(Debug, Clone)]
pub struct Placeholder {
    /// Unique identifier for this placeholder
    pub id: String,
    /// What this placeholder represents
    pub description: String,
    /// Category of placeholder
    pub category: ReadinessCategory,
    /// Files/components this affects
    pub affects: Vec<String>,
    /// Implementation guidance
    pub guidance: String,
    /// Priority for implementation
    pub priority: u8,
}

impl Placeholder {
    /// Create a new placeholder marker
    pub fn new(
        id: String, description: String, category: ReadinessCategory, affects: Vec<String>,
        guidance: String, priority: u8,
    ) -> Self {
        Self {
            id,
            description,
            category,
            affects,
            guidance,
            priority,
        }
    }

    /// Generate a placeholder comment for code
    pub fn to_comment(&self) -> String {
        format!(
            r#"// 🚧 PLACEHOLDER: {description}
// Category: {category:?}
// Priority: {priority}
// Guidance: {guidance}
// TODO: Implement this placeholder for production readiness"#,
            description = self.description,
            category = self.category,
            priority = self.priority,
            guidance = self.guidance
        )
    }

    /// Generate a placeholder template section
    pub fn to_template_section(&self) -> String {
        format!(
            r#"{{{{!-- 🚧 PLACEHOLDER: {description} --}}
{{{{!-- Category: {category:?} --}}
{{{{!-- Priority: {priority} --}}
{{{{!-- Guidance: {guidance} --}}
{{{{!-- TODO: Implement this placeholder for production readiness --}}}}"#,
            description = self.description,
            category = self.category,
            priority = self.priority,
            guidance = self.guidance
        )
    }
}

impl ReadinessTracker {
    /// Analyze project for existing production features
    pub fn analyze_project(&mut self) -> Result<()> {
        // Scan for existing implementations
        self.scan_for_authentication()?;
        self.scan_for_error_handling()?;
        self.scan_for_logging()?;
        self.scan_for_health_checks()?;
        self.scan_for_input_validation()?;
        self.scan_for_database_setup()?;
        self.scan_for_tests()?;
        self.scan_for_documentation()?;
        self.scan_for_docker_setup()?;
        self.scan_for_configuration()?;

        Ok(())
    }

    /// Scan for authentication implementation
    fn scan_for_authentication(&mut self) -> Result<()> {
        let auth_patterns = ["auth", "login", "authentication", "jwt", "session"];
        let has_auth = self.scan_for_patterns(&auth_patterns);

        if has_auth {
            self.update_requirement("auth-basic", ReadinessStatus::Complete)?;
        }
        Ok(())
    }

    /// Scan for error handling implementation
    fn scan_for_error_handling(&mut self) -> Result<()> {
        let error_patterns = ["thiserror", "Result<", "map_err", "anyhow"];
        let has_error_handling = self.scan_for_patterns(&error_patterns);

        if has_error_handling {
            self.update_requirement("error-handling", ReadinessStatus::Complete)?;
        }
        Ok(())
    }

    /// Scan for logging implementation
    fn scan_for_logging(&mut self) -> Result<()> {
        let logging_patterns = ["tracing", "log::", "slog", "println!"];
        let has_logging = self.scan_for_patterns(&logging_patterns);

        if has_logging {
            self.update_requirement("logging-tracing", ReadinessStatus::Complete)?;
        }
        Ok(())
    }

    /// Scan for health check implementation
    fn scan_for_health_checks(&mut self) -> Result<()> {
        let health_patterns = ["health", "/health", "health_check"];
        let has_health = self.scan_for_patterns(&health_patterns);

        if has_health {
            self.update_requirement("health-checks", ReadinessStatus::Complete)?;
        }
        Ok(())
    }

    /// Scan for input validation
    fn scan_for_input_validation(&mut self) -> Result<()> {
        let validation_patterns = ["validate", "validator", "serde", "Deserialize"];
        let has_validation = self.scan_for_patterns(&validation_patterns);

        if has_validation {
            self.update_requirement("input-validation", ReadinessStatus::Complete)?;
        }
        Ok(())
    }

    /// Scan for database setup
    fn scan_for_database_setup(&mut self) -> Result<()> {
        let db_patterns = ["sqlx", "diesel", "sea-orm", "migration", "schema"];
        let has_db = self.scan_for_patterns(&db_patterns);

        if has_db {
            self.update_requirement("database-migrations", ReadinessStatus::Complete)?;
        }
        Ok(())
    }

    /// Scan for test implementation
    fn scan_for_tests(&mut self) -> Result<()> {
        let test_patterns = ["#[test]", "#[cfg(test)]", "tests/"];
        let has_tests = self.scan_for_patterns(&test_patterns);

        if has_tests {
            self.update_requirement("unit-tests", ReadinessStatus::Complete)?;
            self.update_requirement("integration-tests", ReadinessStatus::Complete)?;
        }
        Ok(())
    }

    /// Scan for documentation
    fn scan_for_documentation(&mut self) -> Result<()> {
        let doc_patterns = ["README.md", "docs/", "openapi", "swagger"];
        let has_docs = self.scan_for_patterns(&doc_patterns);

        if has_docs {
            self.update_requirement("api-documentation", ReadinessStatus::Complete)?;
        }
        Ok(())
    }

    /// Scan for Docker setup
    fn scan_for_docker_setup(&mut self) -> Result<()> {
        let docker_patterns = ["Dockerfile", "docker-compose", ".dockerignore"];
        let has_docker = self.scan_for_patterns(&docker_patterns);

        if has_docker {
            self.update_requirement("docker-containerization", ReadinessStatus::Complete)?;
        }
        Ok(())
    }

    /// Scan for configuration management
    fn scan_for_configuration(&mut self) -> Result<()> {
        let config_patterns = ["config.toml", "settings.toml", "app.toml"];
        let has_config = self.scan_for_patterns(&config_patterns);

        if has_config {
            self.update_requirement("configuration-management", ReadinessStatus::Complete)?;
        }
        Ok(())
    }

    /// Scan source files for pattern matches
    fn scan_for_patterns(&self, patterns: &[&str]) -> bool {
        let src_dir = self.project_root.join("src");
        let templates_dir = self.project_root.join("templates");
        let config_dir = self.project_root.join("config");

        let search_dirs = [src_dir, templates_dir, config_dir];

        for dir in &search_dirs {
            if !dir.exists() {
                continue;
            }

            for pattern in patterns {
                if self.directory_contains_pattern(dir, pattern) {
                    return true;
                }
            }
        }

        false
    }

    /// Check if directory contains files matching pattern
    fn directory_contains_pattern(&self, dir: &std::path::Path, pattern: &str) -> bool {
        if let Ok(entries) = std::fs::read_dir(dir) {
            for entry in entries {
                if let Ok(entry) = entry {
                    let path = entry.path();
                    if path.is_file() {
                        if let Ok(content) = std::fs::read_to_string(&path) {
                            if content.contains(pattern) {
                                return true;
                            }
                        }
                    } else if path.is_dir() {
                        // Recursively check subdirectories
                        if self.directory_contains_pattern(&path, pattern) {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_requirements() {
        let requirements = ReadinessTracker::default_requirements();

        // Should have requirements in all categories
        let critical = requirements
            .iter()
            .filter(|r| r.category == ReadinessCategory::Critical)
            .count();
        let important = requirements
            .iter()
            .filter(|r| r.category == ReadinessCategory::Important)
            .count();
        let nice = requirements
            .iter()
            .filter(|r| r.category == ReadinessCategory::NiceToHave)
            .count();

        assert!(critical > 0, "Should have critical requirements");
        assert!(important > 0, "Should have important requirements");
        assert!(nice > 0, "Should have nice-to-have requirements");

        // Critical should be highest priority
        for req in &requirements {
            if req.category == ReadinessCategory::Critical {
                assert!(
                    req.priority >= 8,
                    "Critical requirements should have high priority"
                );
            }
        }
    }

    #[test]
    fn test_readiness_report_generation() {
        let tracker = ReadinessTracker::new("/tmp/test");
        let report = tracker.generate_report();

        assert_eq!(report.project_name, "Current Project");
        assert!(report.overall_score >= 0.0 && report.overall_score <= 100.0);
        assert!(!report.by_category.is_empty());
        assert!(!report.requirements.is_empty());
    }

    #[test]
    fn test_status_transitions() {
        let mut tracker = ReadinessTracker::new("/tmp/test");
        tracker.load().unwrap();

        // Valid transitions
        assert!(tracker
            .update_requirement("auth-basic", ReadinessStatus::Placeholder)
            .is_ok());
        assert!(tracker
            .update_requirement("auth-basic", ReadinessStatus::Complete)
            .is_ok());

        // Invalid transition (Complete -> Missing not allowed)
        assert!(tracker
            .update_requirement("auth-basic", ReadinessStatus::Missing)
            .is_err());
    }
}

/// Placeholder registry for managing placeholder implementations
#[derive(Debug, Clone, Default)]
pub struct PlaceholderRegistry {
    placeholders: std::collections::HashMap<String, Placeholder>,
}

impl PlaceholderRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(&mut self, id: String, placeholder: Placeholder) {
        self.placeholders.insert(id, placeholder);
    }

    pub fn get(&self, id: &str) -> Option<&Placeholder> {
        self.placeholders.get(id)
    }

    pub fn list(&self) -> Vec<&Placeholder> {
        self.placeholders.values().collect()
    }

    pub fn get_by_category(&self, category: &ReadinessCategory) -> Vec<&Placeholder> {
        self.placeholders
            .values()
            .filter(|p| &p.category == category)
            .collect()
    }

    pub fn generate_summary(&self) -> String {
        let mut summary = String::new();
        summary.push_str("Placeholder Summary:\n");

        for (id, placeholder) in &self.placeholders {
            summary.push_str(&format!(
                "  {}: {} ({})\n",
                id,
                placeholder.description,
                format!("{:?}", placeholder.category)
            ));
        }

        summary
    }
}

/// Placeholder processor for handling placeholder operations
#[derive(Debug, Clone)]
pub struct PlaceholderProcessor {
    registry: PlaceholderRegistry,
}

impl PlaceholderProcessor {
    pub fn new() -> Self {
        Self {
            registry: PlaceholderRegistry::new(),
        }
    }

    pub fn process(&self, placeholder_id: &str) -> Result<()> {
        if let Some(_placeholder) = self.registry.get(placeholder_id) {
            tracing::info!("Processing placeholder: {}", placeholder_id);
            Ok(())
        } else {
            Err(ProductionError::RequirementNotFound(
                placeholder_id.to_string(),
            ))
        }
    }

    pub fn registry(&self) -> &PlaceholderRegistry {
        &self.registry
    }

    pub fn registry_mut(&mut self) -> &mut PlaceholderRegistry {
        &mut self.registry
    }
}
