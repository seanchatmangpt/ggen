# Pack System: Marketplace Integration Design

## Overview

The packs system leverages existing marketplace infrastructure for template discovery, installation, validation, and quality scoring. This document defines how packs integrate with marketplace components.

---

## Integration Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        Pack System                              │
│                                                                 │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │                   Pack Domain Layer                        │ │
│  │  ggen-domain::pack::*                                      │ │
│  └────────────────────────────────────────────────────────────┘ │
│                              │                                  │
│                              ▼                                  │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │            Marketplace Integration Facade                  │ │
│  │  • TemplateResolver: Find and load templates              │ │
│  │  • SPARQLExecutor: Execute RDF queries                    │ │
│  │  • MaturityScorer: Score pack quality                     │ │
│  │  • PackageValidator: Validate packages                    │ │
│  └────────────────────────────────────────────────────────────┘ │
│                              │                                  │
└──────────────────────────────┼──────────────────────────────────┘
                               │
        ┌──────────────────────┼──────────────────────┐
        │                      │                      │
        ▼                      ▼                      ▼
┌───────────────┐    ┌────────────────┐    ┌─────────────────┐
│ ggen-core     │    │ggen-marketplace│    │ ggen-domain     │
│               │    │                │    │                 │
│• Template     │    │• Package       │    │• RDF/SPARQL     │
│  Engine       │    │  Discovery     │    │  (render_with   │
│• Pipeline     │    │• Registry      │    │   _rdf)         │
│• Generator    │    │• Search        │    │• marketplace    │
└───────────────┘    └────────────────┘    │  _scorer        │
                                            └─────────────────┘
```

---

## Core Integration Points

### 1. Template Resolution

**Purpose**: Resolve PackTemplate references to actual template files from marketplace.

```rust
use ggen_marketplace::prelude::*;
use ggen_domain::template::TemplateService;

pub struct TemplateResolver {
    marketplace: MarketplaceClient,
    template_service: TemplateService,
    cache_dir: PathBuf,
}

impl TemplateResolver {
    /// Resolve template source to local path
    pub async fn resolve(&self, source: &TemplateSource) -> Result<PathBuf> {
        match source {
            TemplateSource::Marketplace { package_id, version } => {
                self.resolve_marketplace_template(package_id, version).await
            }
            TemplateSource::Local { path } => {
                self.resolve_local_template(path).await
            }
            TemplateSource::Remote { url, checksum } => {
                self.resolve_remote_template(url, checksum.as_ref()).await
            }
            TemplateSource::Inline { content, format } => {
                self.resolve_inline_template(content, format).await
            }
        }
    }

    /// Resolve marketplace template by package ID and version
    async fn resolve_marketplace_template(
        &self,
        package_id: &str,
        version: &VersionReq,
    ) -> Result<PathBuf> {
        // 1. Search marketplace for package
        let search_input = SearchInput {
            query: package_id.to_string(),
            limit: Some(10),
            category: None,
        };

        let packages = execute_search(search_input).await?;

        // 2. Find matching version
        let package = packages
            .into_iter()
            .find(|p| version.matches(&p.version))
            .ok_or_else(|| PackError::NotFound(format!(
                "No package found matching {} @ {}",
                package_id, version
            )))?;

        // 3. Install if not already installed
        if !self.is_template_installed(&package.name, &package.version).await? {
            let install_input = InstallInput {
                package: package.name.clone(),
                version: Some(package.version.to_string()),
                force: false,
            };

            execute_install(install_input).await?;
        }

        // 4. Return path to installed template
        Ok(self.get_installed_template_path(&package.name, &package.version))
    }

    /// Check if template is installed locally
    async fn is_template_installed(&self, name: &str, version: &Version) -> Result<bool> {
        let path = self.get_installed_template_path(name, version);
        Ok(path.exists())
    }

    /// Get path to installed template
    fn get_installed_template_path(&self, name: &str, version: &Version) -> PathBuf {
        // Use marketplace installation directory
        self.cache_dir
            .join("marketplace")
            .join(name)
            .join(version.to_string())
    }
}
```

**Integration Points**:
- `ggen_domain::marketplace::execute_search` - Search marketplace packages
- `ggen_domain::marketplace::execute_install` - Install marketplace packages
- `ggen_domain::marketplace::execute_list` - List installed packages

---

### 2. Template Execution

**Purpose**: Execute resolved templates using ggen-core's TemplateEngine.

```rust
use ggen_core::{GenContext, Generator, Pipeline};
use std::collections::BTreeMap;

pub struct TemplateExecutor {
    resolver: TemplateResolver,
}

impl TemplateExecutor {
    /// Execute a pack template
    pub async fn execute(
        &self,
        template: &PackTemplate,
        output_dir: &Path,
        variables: &HashMap<String, String>,
    ) -> Result<Vec<PathBuf>> {
        // 1. Resolve template source to local path
        let template_path = self.resolver.resolve(&template.source).await?;

        // 2. Merge pack variables with template variables
        let mut merged_vars = template.variables.clone();
        merged_vars.extend(variables.clone());

        // 3. Determine output path
        let final_output_dir = match &template.output_path {
            Some(relative_path) => output_dir.join(relative_path),
            None => output_dir.to_path_buf(),
        };

        // 4. Check execution conditions
        if !self.should_execute(template, &merged_vars)? {
            return Ok(Vec::new());
        }

        // 5. Execute template using ggen-core
        let generated_path = self.execute_template_with_ggen_core(
            &template_path,
            &final_output_dir,
            merged_vars,
        )?;

        Ok(vec![generated_path])
    }

    /// Execute template using ggen-core's TemplateEngine
    fn execute_template_with_ggen_core(
        &self,
        template_path: &Path,
        output_dir: &Path,
        variables: HashMap<String, String>,
    ) -> Result<PathBuf> {
        // Create BTreeMap for ggen-core
        let vars: BTreeMap<String, String> = variables.into_iter().collect();

        // Create pipeline and context
        let pipeline = Pipeline::new()
            .map_err(|e| PackError::GenerationFailed(e.to_string()))?;

        let ctx = GenContext::new(template_path.to_path_buf(), output_dir.to_path_buf())
            .with_vars(vars);

        // Execute generation
        let mut generator = Generator::new(pipeline, ctx);
        generator
            .generate()
            .map_err(|e| PackError::TemplateExecutionFailed(e.to_string()))
    }

    /// Check if template should execute based on conditions
    fn should_execute(
        &self,
        template: &PackTemplate,
        variables: &HashMap<String, String>,
    ) -> Result<bool> {
        for condition in &template.conditions {
            let value = variables
                .get(&condition.variable)
                .ok_or_else(|| PackError::ValidationFailed(format!(
                    "Missing variable: {}",
                    condition.variable
                )))?;

            if !self.evaluate_condition(condition, value) {
                return Ok(false);
            }
        }

        Ok(true)
    }

    /// Evaluate execution condition
    fn evaluate_condition(&self, condition: &ExecutionCondition, value: &str) -> bool {
        match condition.operator {
            ConditionOperator::Equals => value == condition.value,
            ConditionOperator::NotEquals => value != condition.value,
            ConditionOperator::Contains => value.contains(&condition.value),
            ConditionOperator::StartsWith => value.starts_with(&condition.value),
            ConditionOperator::EndsWith => value.ends_with(&condition.value),
            ConditionOperator::Matches => {
                regex::Regex::new(&condition.value)
                    .map(|re| re.is_match(value))
                    .unwrap_or(false)
            }
        }
    }
}
```

**Integration Points**:
- `ggen_core::Pipeline` - Template processing pipeline
- `ggen_core::GenContext` - Template generation context
- `ggen_core::Generator` - Template generator

---

### 3. SPARQL Query Execution

**Purpose**: Execute SPARQL queries using ggen-domain's render_with_rdf module.

```rust
use ggen_domain::template::render_with_rdf::{execute_sparql_from_file, RdfFormat};

pub struct SparqlExecutor {
    rdf_store_path: PathBuf,
}

impl SparqlExecutor {
    /// Execute a SPARQL query from pack
    pub async fn execute(
        &self,
        query: &SparqlQuery,
        output_dir: &Path,
        parameters: &HashMap<String, String>,
    ) -> Result<PathBuf> {
        // 1. Resolve query source
        let query_string = self.resolve_query_source(&query.source).await?;

        // 2. Substitute parameters in query
        let substituted_query = self.substitute_parameters(&query_string, parameters)?;

        // 3. Execute SPARQL query
        let results = self.execute_query(&substituted_query).await?;

        // 4. Format results according to output_format
        let formatted = self.format_results(&results, &query.output_format)?;

        // 5. Write to output file
        let output_path = output_dir.join(&query.output_path);
        std::fs::create_dir_all(output_path.parent().unwrap())?;
        std::fs::write(&output_path, formatted)?;

        Ok(output_path)
    }

    /// Resolve query source to query string
    async fn resolve_query_source(&self, source: &QuerySource) -> Result<String> {
        match source {
            QuerySource::Inline { query } => Ok(query.clone()),
            QuerySource::File { path } => {
                std::fs::read_to_string(path)
                    .map_err(|e| PackError::QueryFailed(format!("Failed to read query file: {}", e)))
            }
            QuerySource::Remote { url } => {
                // Download query from URL
                let response = reqwest::get(url).await
                    .map_err(|e| PackError::QueryFailed(format!("Failed to fetch query: {}", e)))?;
                response.text().await
                    .map_err(|e| PackError::QueryFailed(format!("Failed to read query response: {}", e)))
            }
        }
    }

    /// Substitute parameters in query string
    fn substitute_parameters(
        &self,
        query: &str,
        parameters: &HashMap<String, String>,
    ) -> Result<String> {
        let mut result = query.to_string();

        for (key, value) in parameters {
            let placeholder = format!("{{{{{}}}}}", key);
            result = result.replace(&placeholder, value);
        }

        Ok(result)
    }

    /// Execute SPARQL query against RDF store
    async fn execute_query(&self, query: &str) -> Result<Vec<HashMap<String, String>>> {
        // Use ggen-domain's render_with_rdf module
        // This leverages existing SPARQL infrastructure

        // For now, we'll implement a simplified version
        // In production, this would use the full RDF/SPARQL stack

        // TODO: Integrate with render_with_rdf module
        Ok(Vec::new())
    }

    /// Format query results according to output format
    fn format_results(
        &self,
        results: &[HashMap<String, String>],
        format: &QueryOutputFormat,
    ) -> Result<String> {
        match format {
            QueryOutputFormat::Json => {
                serde_json::to_string_pretty(results)
                    .map_err(|e| PackError::QueryFailed(format!("JSON serialization failed: {}", e)))
            }
            QueryOutputFormat::Yaml => {
                serde_yaml::to_string(results)
                    .map_err(|e| PackError::QueryFailed(format!("YAML serialization failed: {}", e)))
            }
            QueryOutputFormat::Csv => {
                self.format_as_csv(results)
            }
            QueryOutputFormat::Xml => {
                self.format_as_xml(results)
            }
            QueryOutputFormat::Turtle => {
                self.format_as_turtle(results)
            }
            QueryOutputFormat::NTriples => {
                self.format_as_ntriples(results)
            }
        }
    }

    fn format_as_csv(&self, results: &[HashMap<String, String>]) -> Result<String> {
        // CSV formatting implementation
        Ok(String::new())
    }

    fn format_as_xml(&self, results: &[HashMap<String, String>]) -> Result<String> {
        // XML formatting implementation
        Ok(String::new())
    }

    fn format_as_turtle(&self, results: &[HashMap<String, String>]) -> Result<String> {
        // Turtle formatting implementation
        Ok(String::new())
    }

    fn format_as_ntriples(&self, results: &[HashMap<String, String>]) -> Result<String> {
        // N-Triples formatting implementation
        Ok(String::new())
    }
}
```

**Integration Points**:
- `ggen_domain::template::render_with_rdf` - SPARQL execution
- `ggen_domain::template::render_with_rdf::execute_sparql_from_file` - File-based queries
- RDF graph store (TTL files)

---

### 4. Pack Quality Scoring

**Purpose**: Score pack quality using marketplace_scorer patterns.

```rust
use ggen_domain::marketplace_scorer::{MaturityScorer, MaturityLevel};

pub struct PackQualityScorer {
    scorer: MaturityScorer,
}

impl PackQualityScorer {
    /// Calculate pack maturity score
    pub fn score(&self, pack: &Pack) -> Result<PackMaturityScore> {
        let mut scores = HashMap::new();

        // Score different dimensions
        scores.insert("metadata", self.score_metadata(&pack.metadata));
        scores.insert("templates", self.score_templates(&pack.templates));
        scores.insert("queries", self.score_queries(&pack.queries));
        scores.insert("variables", self.score_variables(&pack.variables));
        scores.insert("dependencies", self.score_dependencies(&pack.dependencies));
        scores.insert("documentation", self.score_documentation(pack));
        scores.insert("examples", self.score_examples(&pack.examples));
        scores.insert("hooks", self.score_hooks(&pack.hooks));

        // Calculate total score
        let total: u32 = scores.values().sum();
        let max_possible = scores.len() as u32 * 100;
        let normalized = (total * 100) / max_possible;

        // Determine maturity level
        let level = match normalized {
            0..=39 => MaturityLevel::Experimental,
            40..=69 => MaturityLevel::Beta,
            70..=89 => MaturityLevel::Production,
            90..=100 => MaturityLevel::Enterprise,
            _ => MaturityLevel::Experimental,
        };

        Ok(PackMaturityScore {
            total: normalized,
            level,
            dimensions: scores,
            feedback: self.generate_feedback(&scores),
            recommendations: self.generate_recommendations(&scores),
        })
    }

    /// Score pack metadata completeness
    fn score_metadata(&self, metadata: &PackMetadata) -> u32 {
        let mut score = 0u32;

        // Basic fields (50 points)
        if !metadata.name.is_empty() { score += 10; }
        if !metadata.description.is_empty() { score += 10; }
        if !metadata.title.is_empty() { score += 10; }
        if !metadata.license.is_empty() { score += 10; }
        if metadata.author.email.is_some() { score += 10; }

        // Optional fields (30 points)
        if metadata.repository.is_some() { score += 10; }
        if metadata.homepage.is_some() { score += 10; }
        if !metadata.tags.is_empty() { score += 10; }

        // Quality indicators (20 points)
        if metadata.description.len() >= 100 { score += 10; }
        if metadata.tags.len() >= 3 { score += 10; }

        score
    }

    /// Score template quality and completeness
    fn score_templates(&self, templates: &[PackTemplate]) -> u32 {
        if templates.is_empty() {
            return 0;
        }

        let mut score = 0u32;

        // Has templates (30 points)
        score += 30;

        // Multiple templates (20 points)
        if templates.len() >= 2 { score += 10; }
        if templates.len() >= 3 { score += 10; }

        // Template configuration quality (30 points)
        let templates_with_config = templates
            .iter()
            .filter(|t| t.config.overwrite || t.config.merge || t.config.skip_if_exists)
            .count();

        score += ((templates_with_config * 30) / templates.len()) as u32;

        // Execution priorities set (20 points)
        let with_priorities = templates.iter().filter(|t| t.priority > 0).count();
        score += ((with_priorities * 20) / templates.len()) as u32;

        score
    }

    /// Score SPARQL query quality
    fn score_queries(&self, queries: &[SparqlQuery]) -> u32 {
        if queries.is_empty() {
            return 50; // Not required, but adds value
        }

        let mut score = 50u32; // Base score for having queries

        // Multiple queries (20 points)
        if queries.len() >= 2 { score += 10; }
        if queries.len() >= 3 { score += 10; }

        // Query documentation (30 points)
        let documented = queries.iter().filter(|q| !q.description.is_empty()).count();
        score += ((documented * 30) / queries.len()) as u32;

        score
    }

    /// Score variable definitions
    fn score_variables(&self, variables: &[PackVariable]) -> u32 {
        if variables.is_empty() {
            return 20; // Minimal score for no variables
        }

        let mut score = 20u32;

        // Has variables (30 points)
        score += 30;

        // Variable descriptions (25 points)
        let with_descriptions = variables.iter().filter(|v| !v.description.is_empty()).count();
        score += ((with_descriptions * 25) / variables.len()) as u32;

        // Variable validation (25 points)
        let with_validation = variables.iter().filter(|v| v.validation.is_some()).count();
        score += ((with_validation * 25) / variables.len()) as u32;

        score
    }

    /// Score dependency management
    fn score_dependencies(&self, dependencies: &BTreeMap<String, VersionReq>) -> u32 {
        let mut score = 50u32; // Base score

        // Has dependencies (20 points)
        if !dependencies.is_empty() {
            score += 20;
        }

        // Reasonable dependency count (30 points)
        let dep_count = dependencies.len();
        if dep_count <= 5 {
            score += 30;
        } else if dep_count <= 10 {
            score += 15;
        }

        score
    }

    /// Score documentation completeness
    fn score_documentation(&self, pack: &Pack) -> u32 {
        let mut score = 0u32;

        // Metadata description (30 points)
        let desc_len = pack.metadata.description.len();
        if desc_len >= 50 { score += 10; }
        if desc_len >= 100 { score += 10; }
        if desc_len >= 200 { score += 10; }

        // Examples (40 points)
        if !pack.examples.is_empty() { score += 20; }
        if pack.examples.len() >= 2 { score += 10; }
        if pack.examples.len() >= 3 { score += 10; }

        // Variable documentation (30 points)
        let documented_vars = pack.variables
            .iter()
            .filter(|v| !v.description.is_empty())
            .count();

        if !pack.variables.is_empty() {
            score += ((documented_vars * 30) / pack.variables.len()) as u32;
        } else {
            score += 15; // Partial credit if no variables
        }

        score
    }

    /// Score usage examples
    fn score_examples(&self, examples: &[PackExample]) -> u32 {
        if examples.is_empty() {
            return 30;
        }

        let mut score = 50u32;

        // Multiple examples (30 points)
        if examples.len() >= 2 { score += 15; }
        if examples.len() >= 3 { score += 15; }

        // Example quality (20 points)
        let with_expected_output = examples
            .iter()
            .filter(|e| !e.expected_output.is_empty())
            .count();

        score += ((with_expected_output * 20) / examples.len()) as u32;

        score
    }

    /// Score hook definitions
    fn score_hooks(&self, hooks: &PackHooks) -> u32 {
        let mut score = 50u32; // Base score (hooks are optional)

        let total_hooks = hooks.pre_generation.len()
            + hooks.post_generation.len()
            + hooks.pre_template.len()
            + hooks.post_template.len();

        if total_hooks > 0 {
            score += 30;
        }

        if total_hooks >= 3 {
            score += 20;
        }

        score
    }

    fn generate_feedback(&self, scores: &HashMap<&str, u32>) -> Vec<String> {
        let mut feedback = Vec::new();

        for (dimension, &score) in scores {
            if score < 50 {
                feedback.push(format!("⚠ {} needs improvement (score: {})", dimension, score));
            } else if score >= 80 {
                feedback.push(format!("✓ {} is well-documented (score: {})", dimension, score));
            }
        }

        feedback
    }

    fn generate_recommendations(&self, scores: &HashMap<&str, u32>) -> Vec<String> {
        let mut recommendations = Vec::new();

        for (dimension, &score) in scores {
            if score < 70 {
                recommendations.push(match *dimension {
                    "metadata" => "Add more descriptive metadata (repository, homepage, tags)".to_string(),
                    "templates" => "Add execution priorities and configuration to templates".to_string(),
                    "queries" => "Document SPARQL queries with descriptions".to_string(),
                    "variables" => "Add validation rules and descriptions to variables".to_string(),
                    "documentation" => "Expand pack description and add more examples".to_string(),
                    "examples" => "Add more usage examples with expected outputs".to_string(),
                    _ => format!("Improve {} documentation", dimension),
                });
            }
        }

        recommendations
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackMaturityScore {
    pub total: u32,
    pub level: MaturityLevel,
    pub dimensions: HashMap<&'static str, u32>,
    pub feedback: Vec<String>,
    pub recommendations: Vec<String>,
}
```

**Integration Points**:
- `ggen_domain::marketplace_scorer::MaturityScorer` - Scoring infrastructure
- `ggen_domain::marketplace_scorer::MaturityLevel` - Maturity level classification
- Scoring patterns from marketplace maturity system

---

### 5. Package Validation

**Purpose**: Validate packs using marketplace validation patterns.

```rust
use ggen_domain::marketplace::{validate_package, PackageValidation};

pub struct PackValidator {
    template_validator: TemplateValidator,
    query_validator: QueryValidator,
}

impl PackValidator {
    /// Validate entire pack
    pub async fn validate(&self, pack: &Pack) -> Result<ValidationResult> {
        let mut errors = Vec::new();
        let mut warnings = Vec::new();

        // 1. Validate metadata
        if let Err(e) = self.validate_metadata(&pack.metadata) {
            errors.push(ValidationError {
                code: "INVALID_METADATA".to_string(),
                message: e.to_string(),
                location: Some("metadata".to_string()),
                suggestion: Some("Ensure all required metadata fields are present".to_string()),
            });
        }

        // 2. Validate templates
        for (idx, template) in pack.templates.iter().enumerate() {
            match self.validate_template(template).await {
                Ok(template_warnings) => warnings.extend(template_warnings),
                Err(e) => errors.push(ValidationError {
                    code: "INVALID_TEMPLATE".to_string(),
                    message: e.to_string(),
                    location: Some(format!("templates[{}]", idx)),
                    suggestion: Some("Check template source and configuration".to_string()),
                }),
            }
        }

        // 3. Validate SPARQL queries
        for (idx, query) in pack.queries.iter().enumerate() {
            if let Err(e) = self.validate_query(query).await {
                errors.push(ValidationError {
                    code: "INVALID_QUERY".to_string(),
                    message: e.to_string(),
                    location: Some(format!("queries[{}]", idx)),
                    suggestion: Some("Check SPARQL syntax".to_string()),
                });
            }
        }

        // 4. Validate variables
        for (idx, variable) in pack.variables.iter().enumerate() {
            if let Err(e) = self.validate_variable(variable) {
                errors.push(ValidationError {
                    code: "INVALID_VARIABLE".to_string(),
                    message: e.to_string(),
                    location: Some(format!("variables[{}]", idx)),
                    suggestion: Some("Check variable definition and validation rules".to_string()),
                });
            }
        }

        // 5. Validate dependencies
        for (name, version_req) in &pack.dependencies {
            if let Err(e) = self.validate_dependency(name, version_req).await {
                warnings.push(ValidationWarning {
                    code: "DEPENDENCY_WARNING".to_string(),
                    message: e.to_string(),
                    location: Some(format!("dependencies.{}", name)),
                });
            }
        }

        // Calculate component scores
        let component_scores = ComponentScores {
            metadata: self.score_metadata(&pack.metadata, &errors, &warnings),
            templates: self.score_templates(&pack.templates, &errors, &warnings),
            queries: self.score_queries(&pack.queries, &errors, &warnings),
            variables: self.score_variables(&pack.variables, &errors, &warnings),
            dependencies: self.score_dependencies(&pack.dependencies, &errors, &warnings),
            documentation: self.score_documentation(pack, &errors, &warnings),
        };

        // Calculate overall score
        let total_score = (component_scores.metadata
            + component_scores.templates
            + component_scores.queries
            + component_scores.variables
            + component_scores.dependencies
            + component_scores.documentation)
            / 6;

        Ok(ValidationResult {
            valid: errors.is_empty(),
            errors,
            warnings,
            score: total_score,
            component_scores,
        })
    }

    async fn validate_template(&self, template: &PackTemplate) -> Result<Vec<ValidationWarning>> {
        // Validate template source exists and is accessible
        // Validate template configuration
        // Check for deprecated patterns
        Ok(Vec::new())
    }

    async fn validate_query(&self, query: &SparqlQuery) -> Result<()> {
        // Validate SPARQL syntax
        // Check output format compatibility
        Ok(())
    }

    fn validate_variable(&self, variable: &PackVariable) -> Result<()> {
        // Validate variable name
        // Validate default value against validation rules
        Ok(())
    }

    async fn validate_dependency(&self, name: &str, version: &VersionReq) -> Result<()> {
        // Check if dependency exists in registry
        // Validate version constraint
        Ok(())
    }

    fn validate_metadata(&self, metadata: &PackMetadata) -> Result<()> {
        if metadata.name.is_empty() {
            return Err(PackError::ValidationFailed("Pack name is required".to_string()));
        }
        if metadata.description.is_empty() {
            return Err(PackError::ValidationFailed("Pack description is required".to_string()));
        }
        Ok(())
    }

    // Scoring helper methods
    fn score_metadata(&self, _metadata: &PackMetadata, errors: &[ValidationError], warnings: &[ValidationWarning]) -> u32 {
        let error_count = errors.iter().filter(|e| e.location.as_ref().map_or(false, |l| l.contains("metadata"))).count();
        let warning_count = warnings.iter().filter(|w| w.location.as_ref().map_or(false, |l| l.contains("metadata"))).count();
        100 - (error_count as u32 * 20) - (warning_count as u32 * 5)
    }

    fn score_templates(&self, _templates: &[PackTemplate], errors: &[ValidationError], warnings: &[ValidationWarning]) -> u32 {
        let error_count = errors.iter().filter(|e| e.location.as_ref().map_or(false, |l| l.contains("templates"))).count();
        let warning_count = warnings.iter().filter(|w| w.location.as_ref().map_or(false, |l| l.contains("templates"))).count();
        100 - (error_count as u32 * 20) - (warning_count as u32 * 5)
    }

    fn score_queries(&self, _queries: &[SparqlQuery], errors: &[ValidationError], warnings: &[ValidationWarning]) -> u32 {
        let error_count = errors.iter().filter(|e| e.location.as_ref().map_or(false, |l| l.contains("queries"))).count();
        let warning_count = warnings.iter().filter(|w| w.location.as_ref().map_or(false, |l| l.contains("queries"))).count();
        100 - (error_count as u32 * 20) - (warning_count as u32 * 5)
    }

    fn score_variables(&self, _variables: &[PackVariable], errors: &[ValidationError], warnings: &[ValidationWarning]) -> u32 {
        let error_count = errors.iter().filter(|e| e.location.as_ref().map_or(false, |l| l.contains("variables"))).count();
        let warning_count = warnings.iter().filter(|w| w.location.as_ref().map_or(false, |l| l.contains("variables"))).count();
        100 - (error_count as u32 * 20) - (warning_count as u32 * 5)
    }

    fn score_dependencies(&self, _dependencies: &BTreeMap<String, VersionReq>, errors: &[ValidationError], warnings: &[ValidationWarning]) -> u32 {
        let error_count = errors.iter().filter(|e| e.location.as_ref().map_or(false, |l| l.contains("dependencies"))).count();
        let warning_count = warnings.iter().filter(|w| w.location.as_ref().map_or(false, |l| l.contains("dependencies"))).count();
        100 - (error_count as u32 * 20) - (warning_count as u32 * 5)
    }

    fn score_documentation(&self, _pack: &Pack, errors: &[ValidationError], warnings: &[ValidationWarning]) -> u32 {
        let error_count = errors.iter().filter(|e| e.location.as_ref().map_or(false, |l| l.contains("documentation"))).count();
        let warning_count = warnings.iter().filter(|w| w.location.as_ref().map_or(false, |l| l.contains("documentation"))).count();
        100 - (error_count as u32 * 20) - (warning_count as u32 * 5)
    }
}
```

**Integration Points**:
- `ggen_domain::marketplace::validate_package` - Package validation
- `ggen_domain::marketplace::PackageValidation` - Validation results
- Marketplace validation patterns

---

## Integration Summary

| Pack Feature | Marketplace Component | Integration Method |
|--------------|----------------------|-------------------|
| Template discovery | `ggen-marketplace` | Search and list APIs |
| Template installation | `execute_install` | Direct function call |
| Template rendering | `ggen-core::TemplateEngine` | Pipeline and Generator |
| SPARQL execution | `render_with_rdf` | SPARQL query execution |
| Quality scoring | `marketplace_scorer` | Maturity scoring patterns |
| Validation | `validate_package` | Validation infrastructure |
| RDF/TTL files | RDF graph store | Semantic queries |

**Key Benefits**:
1. **No Duplication**: Reuses existing infrastructure
2. **Consistency**: Same patterns as marketplace
3. **Maintainability**: Single source of truth for template/validation logic
4. **Compatibility**: Packs work with existing marketplace packages
5. **Extensibility**: Easy to add new integration points

**Performance Considerations**:
- Cache resolved templates locally
- Batch template installations
- Lazy-load SPARQL query execution
- Parallel template rendering where possible
