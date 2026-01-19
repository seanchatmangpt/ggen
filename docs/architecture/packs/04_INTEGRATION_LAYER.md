# Packs Integration Layer

**Version:** 3.2.0
**Purpose:** Bridge between packs domain and infrastructure (marketplace, templates, graph)

## Architecture Overview

```
┌─────────────────────────────────────────────────────────┐
│             Packs Domain Services                       │
│  (Business logic, orchestration, validation)            │
└────────┬───────────┬───────────────┬──────────┬─────────┘
         │           │               │          │
    ┌────▼────┐ ┌────▼────┐   ┌─────▼─────┐ ┌──▼─────┐
    │Marketplace│ │Template │   │   Graph   │ │  File  │
    │ Adapter  │ │ Adapter │   │  Adapter  │ │Adapter │
    └────┬────┘ └────┬────┘   └─────┬─────┘ └──┬─────┘
         │           │               │          │
    ┌────▼────┐ ┌────▼────┐   ┌─────▼─────┐ ┌──▼─────┐
    │ggen-    │ │ggen-    │   │ggen-      │ │std::fs │
    │marketplace│ │core    │   │core::graph│ │tokio::fs│
    └─────────┘ └─────────┘   └───────────┘ └────────┘
```

## Integration Points

### 1. Marketplace Integration

#### Trait Definition
```rust
/// Client for marketplace operations
#[async_trait]
pub trait MarketplaceClient: Send + Sync {
    /// Install package from marketplace
    async fn install_package(
        &self,
        name: &str,
        version: &str,
        options: &InstallOptions,
    ) -> Result<InstallResult>;

    /// Search packages in marketplace
    async fn search_packages(&self, query: &str) -> Result<Vec<PackageMetadata>>;

    /// Validate package exists
    async fn package_exists(&self, name: &str, version: &str) -> Result<bool>;

    /// Resolve version constraint to actual version
    async fn resolve_version(
        &self,
        name: &str,
        constraint: &VersionConstraint,
    ) -> Result<ResolvedPackage>;

    /// Get package metadata
    async fn get_package_metadata(&self, name: &str, version: &str) -> Result<PackageMetadata>;
}
```

#### Implementation
```rust
use ggen_domain::marketplace;

/// Production marketplace client using ggen-domain
pub struct ProductionMarketplaceClient {
    registry_url: String,
    cache: Arc<CacheManager>,
}

impl ProductionMarketplaceClient {
    pub fn new() -> Self {
        Self {
            registry_url: std::env::var("GGEN_REGISTRY_URL")
                .unwrap_or_else(|_| DEFAULT_REGISTRY_URL.to_string()),
            cache: Arc::new(CacheManager::new()),
        }
    }
}

#[async_trait]
impl MarketplaceClient for ProductionMarketplaceClient {
    async fn install_package(
        &self,
        name: &str,
        version: &str,
        options: &InstallOptions,
    ) -> Result<InstallResult> {
        // Call ggen-domain marketplace::install_package
        let install_opts = marketplace::InstallOptions::new(name)
            .with_version(version)
            .with_target(options.target_path.clone().unwrap_or_default());

        let result = marketplace::install_package(&install_opts).await?;

        Ok(InstallResult {
            package_name: result.package_name,
            version: result.version,
            install_path: result.install_path,
        })
    }

    async fn search_packages(&self, query: &str) -> Result<Vec<PackageMetadata>> {
        // Call ggen-domain marketplace::search
        let search_input = marketplace::SearchInput {
            query: query.to_string(),
            filters: marketplace::SearchFilters::default(),
            limit: Some(50),
            offset: 0,
        };

        let results = marketplace::execute_search(search_input).await?;

        Ok(results
            .packages
            .into_iter()
            .map(|p| PackageMetadata {
                name: p.name,
                version: p.version,
                description: p.description,
                category: p.category,
            })
            .collect())
    }

    async fn package_exists(&self, name: &str, version: &str) -> Result<bool> {
        // Try to get metadata; if successful, package exists
        match self.get_package_metadata(name, version).await {
            Ok(_) => Ok(true),
            Err(e) if e.to_string().contains("not found") => Ok(false),
            Err(e) => Err(e),
        }
    }

    async fn resolve_version(
        &self,
        name: &str,
        constraint: &VersionConstraint,
    ) -> Result<ResolvedPackage> {
        // Load registry index
        let index = self.load_registry_index().await?;

        // Find package versions
        let versions = index
            .packages
            .get(name)
            .ok_or_else(|| Error::PackageNotFound(name.to_string()))?;

        // Filter versions matching constraint
        let matching: Vec<_> = versions
            .iter()
            .filter(|v| constraint.satisfies(&v.version))
            .collect();

        // Select highest matching version
        let selected = matching
            .iter()
            .max_by(|a, b| a.version.cmp(&b.version))
            .ok_or_else(|| Error::NoMatchingVersion)?;

        Ok(ResolvedPackage {
            name: name.to_string(),
            version: selected.version.clone(),
            download_url: selected.download_url.clone(),
            checksum: selected.checksum.clone(),
        })
    }

    async fn get_package_metadata(&self, name: &str, version: &str) -> Result<PackageMetadata> {
        // Check cache first
        let cache_key = format!("{}@{}", name, version);
        if let Some(cached) = self.cache.get::<PackageMetadata>(&cache_key) {
            return Ok(cached);
        }

        // Load from registry
        let index = self.load_registry_index().await?;
        let package = index
            .packages
            .get(name)
            .and_then(|versions| versions.iter().find(|v| v.version.to_string() == version))
            .ok_or_else(|| Error::PackageNotFound(format!("{}@{}", name, version)))?;

        let metadata = PackageMetadata {
            name: package.name.clone(),
            version: package.version.to_string(),
            description: package.description.clone(),
            category: package.category.clone(),
        };

        // Cache result
        self.cache.set(&cache_key, &metadata, Duration::from_secs(300));

        Ok(metadata)
    }
}

impl ProductionMarketplaceClient {
    async fn load_registry_index(&self) -> Result<RegistryIndex> {
        // Use ggen-core registry client
        let registry = ggen_core::RegistryClient::new(&self.registry_url)?;
        registry.load_index().await
    }
}
```

#### Error Handling Strategy
```rust
/// Marketplace adapter errors
#[derive(Debug, thiserror::Error)]
pub enum MarketplaceError {
    #[error("Package not found: {0}")]
    PackageNotFound(String),

    #[error("No version matching constraint: {name} {constraint}")]
    NoMatchingVersion { name: String, constraint: String },

    #[error("Installation failed: {0}")]
    InstallationFailed(String),

    #[error("Network error: {0}")]
    NetworkError(String),

    #[error("Registry error: {0}")]
    RegistryError(String),

    // FM2: Download failures with retry exhaustion
    #[error("Download failed after {attempts} attempts: {reason}")]
    DownloadFailed { attempts: u32, reason: String },

    // FM12: Lockfile corruption
    #[error("Lockfile corrupted: {path}. Reason: {reason}")]
    LockfileCorrupted { path: PathBuf, reason: String },

    // FM14: Partial installation rollback
    #[error("Installation incomplete, rolled back: {reason}")]
    InstallationRolledBack { reason: String },
}

/// Retry policy for marketplace operations
pub struct RetryPolicy {
    max_attempts: u32,
    base_delay: Duration,
    max_delay: Duration,
}

impl RetryPolicy {
    pub fn new() -> Self {
        Self {
            max_attempts: 3,
            base_delay: Duration::from_secs(1),
            max_delay: Duration::from_secs(30),
        }
    }

    /// Execute with exponential backoff retry
    pub async fn execute<F, T, E>(&self, mut f: F) -> Result<T, E>
    where
        F: FnMut() -> std::pin::Pin<Box<dyn Future<Output = Result<T, E>> + Send>>,
        E: std::fmt::Display,
    {
        let mut last_error = None;

        for attempt in 1..=self.max_attempts {
            match f().await {
                Ok(result) => return Ok(result),
                Err(e) => {
                    tracing::warn!("Attempt {}/{} failed: {}", attempt, self.max_attempts, e);
                    last_error = Some(e);

                    if attempt < self.max_attempts {
                        let delay = self.calculate_backoff(attempt);
                        tokio::time::sleep(delay).await;
                    }
                }
            }
        }

        Err(last_error.unwrap())
    }

    fn calculate_backoff(&self, attempt: u32) -> Duration {
        let delay = self.base_delay * 2u32.pow(attempt - 1);
        delay.min(self.max_delay)
    }
}
```

---

### 2. Template Engine Integration

#### Trait Definition
```rust
/// Template rendering engine
#[async_trait]
pub trait TemplateEngine: Send + Sync {
    /// Render template file with variables
    async fn render_file(
        &self,
        template_path: &Path,
        variables: &HashMap<String, String>,
    ) -> Result<String>;

    /// Render template string with variables
    fn render_string(&self, template: &str, variables: &HashMap<String, String>)
        -> Result<String>;

    /// Generate file tree from templates
    async fn generate_file_tree(
        &self,
        templates: &[PackTemplate],
        variables: &HashMap<String, String>,
        output_dir: &Path,
    ) -> Result<GenerationResult>;

    /// Validate template syntax
    fn validate_template(&self, template_path: &Path) -> Result<()>;

    /// List available filters
    fn available_filters(&self) -> Vec<String>;

    /// List available functions
    fn available_functions(&self) -> Vec<String>;
}
```

#### Implementation
```rust
use ggen_core::{Generator, GenContext, Pipeline, template};

/// Production template engine using ggen-core
pub struct ProductionTemplateEngine {
    pipeline: Arc<Pipeline>,
}

impl ProductionTemplateEngine {
    pub fn new() -> Result<Self> {
        let pipeline = Pipeline::new()?;

        Ok(Self {
            pipeline: Arc::new(pipeline),
        })
    }
}

#[async_trait]
impl TemplateEngine for ProductionTemplateEngine {
    async fn render_file(
        &self,
        template_path: &Path,
        variables: &HashMap<String, String>,
    ) -> Result<String> {
        // Convert HashMap to BTreeMap (required by ggen-core)
        let vars: BTreeMap<String, String> = variables
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

        // Create temporary output directory
        let temp_dir = tempfile::tempdir()?;
        let output_path = temp_dir.path().join("output");

        // Create generation context
        let ctx = GenContext::new(template_path.to_path_buf(), temp_dir.path().to_path_buf())
            .with_vars(vars);

        // Generate file
        let mut generator = Generator::new(self.pipeline.as_ref().clone(), ctx);
        let result_path = generator.generate()?;

        // Read generated content
        let content = tokio::fs::read_to_string(&result_path).await?;

        Ok(content)
    }

    fn render_string(
        &self,
        template: &str,
        variables: &HashMap<String, String>,
    ) -> Result<String> {
        // Create temporary file for template
        let temp_dir = tempfile::tempdir()?;
        let template_path = temp_dir.path().join("template.tmpl");

        std::fs::write(&template_path, template)?;

        // Render using render_file
        let content = tokio::runtime::Handle::current()
            .block_on(self.render_file(&template_path, variables))?;

        Ok(content)
    }

    async fn generate_file_tree(
        &self,
        templates: &[PackTemplate],
        variables: &HashMap<String, String>,
        output_dir: &Path,
    ) -> Result<GenerationResult> {
        // Create output directory
        tokio::fs::create_dir_all(output_dir).await?;

        let mut files_created = 0;
        let mut total_bytes = 0;
        let mut errors = Vec::new();

        // Render each template
        for template in templates {
            match self.render_template(template, variables, output_dir).await {
                Ok(result) => {
                    files_created += 1;
                    total_bytes += result.bytes_written;
                }
                Err(e) => {
                    tracing::error!("Failed to render template {}: {}", template.name, e);
                    errors.push((template.name.clone(), e.to_string()));
                }
            }
        }

        Ok(GenerationResult {
            files_created,
            total_bytes,
            output_dir: output_dir.to_path_buf(),
            errors,
        })
    }

    fn validate_template(&self, template_path: &Path) -> Result<()> {
        // Use ggen-core template validation
        let content = std::fs::read_to_string(template_path)?;

        // Parse template to check syntax
        let template = ggen_core::Template::from_str(&content)?;

        // Validate frontmatter
        template.validate()?;

        Ok(())
    }

    fn available_filters(&self) -> Vec<String> {
        vec![
            "upper".to_string(),
            "lower".to_string(),
            "capitalize".to_string(),
            "snake_case".to_string(),
            "camel_case".to_string(),
            "pascal_case".to_string(),
            "kebab_case".to_string(),
            "trim".to_string(),
            "replace".to_string(),
        ]
    }

    fn available_functions(&self) -> Vec<String> {
        vec![
            "now".to_string(),
            "uuid".to_string(),
            "env".to_string(),
            "read_file".to_string(),
            "exec".to_string(),
        ]
    }
}

impl ProductionTemplateEngine {
    async fn render_template(
        &self,
        template: &PackTemplate,
        variables: &HashMap<String, String>,
        output_dir: &Path,
    ) -> Result<RenderResult> {
        // Render output path (may contain variables)
        let output_path_str = self.render_string(&template.output_path, variables)?;
        let output_path = output_dir.join(&output_path_str);

        // Ensure parent directory exists
        if let Some(parent) = output_path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        // Check if file exists and overwrite not allowed
        if output_path.exists() && !template.overwrite {
            return Err(Error::FileExists(output_path));
        }

        // Render template content
        let content = self.render_file(&template.template_path, variables).await?;

        // Write to output file
        tokio::fs::write(&output_path, &content).await?;

        // Set permissions if specified
        if let Some(perms) = template.permissions {
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let permissions = std::fs::Permissions::from_mode(perms);
                std::fs::set_permissions(&output_path, permissions)?;
            }
        }

        Ok(RenderResult {
            output_path,
            bytes_written: content.len(),
        })
    }
}

/// Result of rendering a single template
#[derive(Debug)]
pub struct RenderResult {
    pub output_path: PathBuf,
    pub bytes_written: usize,
}

/// Result of generating file tree
#[derive(Debug)]
pub struct GenerationResult {
    pub files_created: usize,
    pub total_bytes: usize,
    pub output_dir: PathBuf,
    pub errors: Vec<(String, String)>, // (template_name, error_message)
}
```

#### Error Handling
```rust
/// Template adapter errors
#[derive(Debug, thiserror::Error)]
pub enum TemplateError {
    #[error("Template not found: {0}")]
    TemplateNotFound(PathBuf),

    #[error("Template syntax error: {0}")]
    SyntaxError(String),

    #[error("Missing required variable: {0}")]
    MissingVariable(String),

    #[error("File already exists: {0}")]
    FileExists(PathBuf),

    #[error("Invalid template path: {0}")]
    InvalidPath(String),

    // FM3: Template rendering failures
    #[error("Template rendering failed: {template} - {reason}")]
    RenderingFailed { template: String, reason: String },

    // FM7: Missing template variables
    #[error("Required variable missing: {variable} in template {template}")]
    RequiredVariableMissing { variable: String, template: String },
}

/// Template validation with detailed error reporting
pub struct TemplateValidator {
    engine: Arc<dyn TemplateEngine>,
}

impl TemplateValidator {
    pub fn new(engine: Arc<dyn TemplateEngine>) -> Self {
        Self { engine }
    }

    /// Validate all templates in pack
    pub async fn validate_pack_templates(&self, pack: &Pack) -> ValidationReport {
        let mut report = ValidationReport::new(&pack.id);

        for template in &pack.templates {
            match self.validate_template(template).await {
                Ok(()) => {
                    report.add_success(&template.name, "Template valid");
                }
                Err(e) => {
                    report.add_error(&template.name, &e.to_string());
                }
            }
        }

        report
    }

    async fn validate_template(&self, template: &PackTemplate) -> Result<()> {
        // Check template file exists
        if !template.template_path.exists() {
            return Err(TemplateError::TemplateNotFound(template.template_path.clone()).into());
        }

        // Validate syntax
        self.engine.validate_template(&template.template_path)?;

        // Validate required variables are defined
        for var in &template.variables {
            if var.required && var.default.is_none() {
                // This is a required variable without default
                // Actual validation happens at render time
            }
        }

        Ok(())
    }
}
```

---

### 3. Graph Integration (SPARQL)

#### Trait Definition
```rust
/// RDF graph operations
#[async_trait]
pub trait GraphClient: Send + Sync {
    /// Execute SPARQL query
    async fn query(&self, sparql: &str) -> Result<QueryResults>;

    /// Insert RDF triples
    async fn insert_turtle(&self, turtle: &str) -> Result<()>;

    /// Load pack metadata into graph
    async fn load_pack_metadata(&self, pack: &Pack) -> Result<()>;

    /// Query related packages
    async fn find_related_packages(&self, pack_id: &str) -> Result<Vec<String>>;

    /// Validate compatibility between packs
    async fn check_compatibility(&self, pack_ids: &[String]) -> Result<CompatibilityReport>;
}
```

#### Implementation
```rust
use ggen_core::Graph;

/// Production graph client using ggen-core
pub struct ProductionGraphClient {
    graph: Arc<Graph>,
}

impl ProductionGraphClient {
    pub fn new() -> Result<Self> {
        let graph = Graph::new()?;

        Ok(Self {
            graph: Arc::new(graph),
        })
    }
}

#[async_trait]
impl GraphClient for ProductionGraphClient {
    async fn query(&self, sparql: &str) -> Result<QueryResults> {
        // Execute query using ggen-core Graph
        let results = self.graph.query(sparql)?;

        // Convert to common format
        Ok(QueryResults {
            bindings: results
                .into_iter()
                .map(|row| {
                    row.into_iter()
                        .map(|(k, v)| (k, v.to_string()))
                        .collect()
                })
                .collect(),
        })
    }

    async fn insert_turtle(&self, turtle: &str) -> Result<()> {
        self.graph.insert_turtle(turtle)?;
        Ok(())
    }

    async fn load_pack_metadata(&self, pack: &Pack) -> Result<()> {
        // Convert pack to RDF triples
        let turtle = self.pack_to_turtle(pack)?;

        // Insert into graph
        self.insert_turtle(&turtle).await?;

        Ok(())
    }

    async fn find_related_packages(&self, pack_id: &str) -> Result<Vec<String>> {
        // Execute predefined SPARQL query
        let query = format!(
            r#"
            PREFIX ggen: <http://ggen.io/ontology#>

            SELECT ?package WHERE {{
                ?pack ggen:id "{}" .
                ?pack ggen:hasPackage ?package .
            }}
            "#,
            pack_id
        );

        let results = self.query(&query).await?;

        Ok(results
            .bindings
            .into_iter()
            .filter_map(|binding| binding.get("package").cloned())
            .collect())
    }

    async fn check_compatibility(&self, pack_ids: &[String]) -> Result<CompatibilityReport> {
        let mut report = CompatibilityReport::default();

        // Load all packs into graph
        for pack_id in pack_ids {
            // Query pack dependencies
            let query = format!(
                r#"
                PREFIX ggen: <http://ggen.io/ontology#>

                SELECT ?dependency ?version WHERE {{
                    ?pack ggen:id "{}" .
                    ?pack ggen:dependsOn ?dep .
                    ?dep ggen:id ?dependency .
                    ?dep ggen:version ?version .
                }}
                "#,
                pack_id
            );

            let results = self.query(&query).await?;

            for binding in results.bindings {
                if let (Some(dep), Some(ver)) = (binding.get("dependency"), binding.get("version"))
                {
                    report.add_dependency(pack_id, dep, ver);
                }
            }
        }

        // Check for conflicts
        report.detect_conflicts()?;

        Ok(report)
    }
}

impl ProductionGraphClient {
    fn pack_to_turtle(&self, pack: &Pack) -> Result<String> {
        let mut turtle = String::new();

        // Add prefixes
        turtle.push_str("@prefix ggen: <http://ggen.io/ontology#> .\n");
        turtle.push_str("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\n");

        // Add pack node
        turtle.push_str(&format!("ggen:{} a ggen:Pack ;\n", pack.id.0));
        turtle.push_str(&format!("  ggen:name \"{}\" ;\n", pack.name));
        turtle.push_str(&format!("  ggen:version \"{}\" ;\n", pack.version));
        turtle.push_str(&format!("  ggen:description \"{}\" ;\n", pack.description));
        turtle.push_str(&format!("  ggen:category ggen:{} ;\n", pack.category.as_str()));

        // Add packages
        for package in &pack.packages {
            turtle.push_str(&format!("  ggen:hasPackage \"{}\" ;\n", package.name));
        }

        // Add dependencies
        for dep in &pack.dependencies {
            turtle.push_str(&format!("  ggen:dependsOn ggen:{} ;\n", dep.pack_id.0));
        }

        turtle.push_str(".\n");

        Ok(turtle)
    }
}

/// SPARQL query results
#[derive(Debug)]
pub struct QueryResults {
    pub bindings: Vec<HashMap<String, String>>,
}

/// Pack compatibility report
#[derive(Debug, Default)]
pub struct CompatibilityReport {
    dependencies: HashMap<String, Vec<(String, String)>>, // pack_id -> [(dep_name, version)]
    conflicts: Vec<Conflict>,
}

impl CompatibilityReport {
    fn add_dependency(&mut self, pack_id: &str, dep_name: &str, version: &str) {
        self.dependencies
            .entry(pack_id.to_string())
            .or_default()
            .push((dep_name.to_string(), version.to_string()));
    }

    fn detect_conflicts(&mut self) -> Result<()> {
        // Group dependencies by name
        let mut dep_versions: HashMap<String, Vec<(String, String)>> = HashMap::new();

        for (pack_id, deps) in &self.dependencies {
            for (dep_name, version) in deps {
                dep_versions
                    .entry(dep_name.clone())
                    .or_default()
                    .push((pack_id.clone(), version.clone()));
            }
        }

        // Check for version conflicts
        for (dep_name, packs) in dep_versions {
            if packs.len() > 1 {
                let versions: HashSet<_> = packs.iter().map(|(_, v)| v.clone()).collect();

                if versions.len() > 1 {
                    // Multiple different versions required
                    self.conflicts.push(Conflict {
                        dependency: dep_name,
                        required_by: packs,
                    });
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct Conflict {
    pub dependency: String,
    pub required_by: Vec<(String, String)>, // (pack_id, version)
}
```

---

### 4. Dependency Resolution Service

```rust
/// Dependency resolver with circular detection
pub struct DependencyResolver {
    marketplace: Arc<dyn MarketplaceClient>,
    graph: Arc<dyn GraphClient>,
}

impl DependencyResolver {
    pub fn new(
        marketplace: Arc<dyn MarketplaceClient>,
        graph: Arc<dyn GraphClient>,
    ) -> Self {
        Self { marketplace, graph }
    }

    /// Resolve full dependency graph for pack
    pub async fn resolve(&self, pack: &Pack) -> Result<DependencyGraph> {
        let mut graph = DependencyGraph {
            nodes: HashMap::new(),
            edges: Vec::new(),
        };

        self.resolve_recursive(pack, &mut graph, &mut HashSet::new())
            .await?;

        // Validate acyclic
        graph.validate_acyclic()?;

        Ok(graph)
    }

    async fn resolve_recursive(
        &self,
        pack: &Pack,
        graph: &mut DependencyGraph,
        visited: &mut HashSet<PackId>,
    ) -> Result<()> {
        let pack_id = pack.id.clone();

        // Prevent infinite recursion
        if visited.contains(&pack_id) {
            return Ok(());
        }

        visited.insert(pack_id.clone());

        // Add node
        graph.nodes.insert(
            pack_id.clone(),
            DependencyNode {
                pack: pack.clone(),
                resolved_version: pack.version.clone(),
                dependencies: pack.dependencies.iter().map(|d| d.pack_id.clone()).collect(),
            },
        );

        // Resolve dependencies
        for dep in &pack.dependencies {
            // Add edge
            graph.edges.push((pack_id.clone(), dep.pack_id.clone()));

            // Load dependent pack (from marketplace or cache)
            let dep_pack = self.load_pack(&dep.pack_id).await?;

            // Recurse
            self.resolve_recursive(&dep_pack, graph, visited).await?;
        }

        Ok(())
    }

    async fn load_pack(&self, pack_id: &PackId) -> Result<Pack> {
        // Implementation would load from pack registry
        todo!("Load pack from registry")
    }
}
```

---

## Complete Integration Example

### Install Pack Workflow
```rust
/// Service orchestrating pack installation with full integration
pub struct PackInstallationService {
    marketplace: Arc<dyn MarketplaceClient>,
    template_engine: Arc<dyn TemplateEngine>,
    graph_client: Arc<dyn GraphClient>,
    dependency_resolver: Arc<DependencyResolver>,
}

impl PackInstallationService {
    pub async fn install_pack(&self, pack_id: &str) -> Result<InstallResult> {
        // 1. Load pack manifest
        let pack = self.load_pack_manifest(pack_id)?;

        // 2. Resolve dependencies
        let dep_graph = self.dependency_resolver.resolve(&pack).await?;

        // 3. Topological sort for install order
        let install_order = dep_graph.topological_sort()?;

        // 4. Install packages via marketplace
        let mut installed = Vec::new();
        for node_pack_id in install_order {
            let node = &dep_graph.nodes[&node_pack_id];

            for package_ref in &node.pack.packages {
                let result = self
                    .marketplace
                    .install_package(
                        &package_ref.name,
                        &package_ref.version.to_string(),
                        &InstallOptions::default(),
                    )
                    .await?;

                installed.push(result);
            }
        }

        // 5. Load pack metadata into graph
        self.graph_client.load_pack_metadata(&pack).await?;

        Ok(InstallResult {
            pack_id: pack_id.to_string(),
            packages_installed: installed,
            dependencies_resolved: dep_graph.nodes.len(),
        })
    }
}
```

---

**Related Documents:**
- [Architecture Overview](01_ARCHITECTURE_OVERVIEW.md)
- [Command Specification](02_COMMAND_SPECIFICATION.md)
- [Data Model](03_DATA_MODEL.md)
- [User Workflows](05_USER_WORKFLOWS.md)
