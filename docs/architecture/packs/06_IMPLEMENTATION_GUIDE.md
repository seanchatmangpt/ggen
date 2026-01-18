# Packs Implementation Guide

**Version:** 3.2.0
**Purpose:** Step-by-step implementation roadmap to achieve 90%+ health score

## Implementation Strategy

### Phase-Based Approach
1. **Phase 1: Foundation** (Week 1-2) - Core types, marketplace integration
2. **Phase 2: Generation** (Week 3) - Template rendering, variable resolution
3. **Phase 3: Dependencies** (Week 4) - Dependency resolution, composition
4. **Phase 4: Polish** (Week 5) - Error handling, performance, telemetry

### 80/20 Rule Application
Focus on the 20% of features that enable 80% of user workflows:
- Install + Generate = 80% of use cases
- Everything else = 20% enhancement

---

## Phase 1: Foundation (Week 1-2)

### Goal
Enable basic pack installation with real marketplace integration.

### Deliverables
1. ✅ Complete data model (types.rs)
2. ✅ Marketplace adapter implementation
3. ✅ Pack manifest loading
4. ✅ Basic installation workflow

### Implementation Steps

#### Step 1.1: Update Data Model
```rust
// File: crates/ggen-domain/src/packs/types.rs

// Add missing types from architecture
pub struct PackageRef {
    pub name: String,
    pub version: VersionConstraint,
    pub optional: bool,
    pub features: Vec<String>,
    pub install_path: Option<PathBuf>,
}

pub enum VersionConstraint {
    Exact(String),
    Compatible(String),   // ^ prefix
    Tilde(String),        // ~ prefix
    Range { min: String, max: String },
    Latest,
}

impl VersionConstraint {
    pub fn satisfies(&self, version: &SemanticVersion) -> bool {
        // Implement version matching logic
    }
}

pub struct PackTemplate {
    pub name: String,
    pub template_path: PathBuf,
    pub output_path: String,
    pub description: String,
    pub variables: Vec<TemplateVariable>,
    pub optional_variables: HashMap<String, String>,
    pub engine: TemplateEngine,
    pub overwrite: bool,
    pub permissions: Option<u32>,
}

pub struct TemplateVariable {
    pub name: String,
    pub description: String,
    pub var_type: VariableType,
    pub required: bool,
    pub default: Option<String>,
    pub pattern: Option<String>,
    pub example: Option<String>,
}

pub enum VariableType {
    String,
    Integer,
    Boolean,
    Path,
}
```

**Test Coverage:**
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_constraint_exact() {
        let constraint = VersionConstraint::Exact("1.2.3".to_string());
        let version = SemanticVersion::parse("1.2.3").unwrap();
        assert!(constraint.satisfies(&version));
    }

    #[test]
    fn test_version_constraint_compatible() {
        let constraint = VersionConstraint::Compatible("1.2.0".to_string());
        let v1 = SemanticVersion::parse("1.2.3").unwrap();
        let v2 = SemanticVersion::parse("2.0.0").unwrap();

        assert!(constraint.satisfies(&v1));
        assert!(!constraint.satisfies(&v2));
    }

    #[test]
    fn test_template_variable_validation() {
        let var = TemplateVariable {
            name: "port".to_string(),
            var_type: VariableType::Integer,
            pattern: Some(r"^[0-9]{4,5}$".to_string()),
            required: true,
            ..Default::default()
        };

        assert!(var.validate_value("3000").is_ok());
        assert!(var.validate_value("invalid").is_err());
    }
}
```

#### Step 1.2: Create Marketplace Adapter
```rust
// File: crates/ggen-domain/src/packs/adapters/marketplace.rs

use crate::marketplace;
use async_trait::async_trait;

#[async_trait]
pub trait MarketplaceClient: Send + Sync {
    async fn install_package(
        &self,
        name: &str,
        version: &str,
        options: &InstallOptions,
    ) -> Result<InstallResult>;

    async fn package_exists(&self, name: &str, version: &str) -> Result<bool>;

    async fn resolve_version(
        &self,
        name: &str,
        constraint: &VersionConstraint,
    ) -> Result<ResolvedPackage>;
}

pub struct ProductionMarketplaceClient {
    registry_url: String,
    cache: Arc<CacheManager>,
    retry_policy: RetryPolicy,
}

#[async_trait]
impl MarketplaceClient for ProductionMarketplaceClient {
    async fn install_package(
        &self,
        name: &str,
        version: &str,
        options: &InstallOptions,
    ) -> Result<InstallResult> {
        // Retry with exponential backoff
        self.retry_policy.execute(|| async {
            let install_opts = marketplace::InstallOptions::new(name)
                .with_version(version)
                .with_target(options.target_path.clone().unwrap_or_default())
                .force(options.force);

            marketplace::install_package(&install_opts).await
        }).await
    }

    async fn package_exists(&self, name: &str, version: &str) -> Result<bool> {
        // Check cache first
        let cache_key = format!("exists:{}@{}", name, version);

        if let Some(cached) = self.cache.get::<bool>(&cache_key) {
            return Ok(cached);
        }

        // Query marketplace
        let exists = marketplace::package_exists(name, version).await?;

        // Cache result (5 min TTL)
        self.cache.set(&cache_key, &exists, Duration::from_secs(300));

        Ok(exists)
    }

    async fn resolve_version(
        &self,
        name: &str,
        constraint: &VersionConstraint,
    ) -> Result<ResolvedPackage> {
        // Load registry index
        let index = self.load_registry_index().await?;

        // Find matching versions
        let versions = index.get_package_versions(name)?;

        // Filter by constraint
        let matching: Vec<_> = versions
            .iter()
            .filter(|v| constraint.satisfies(&v.version))
            .collect();

        // Select highest version
        let selected = matching
            .iter()
            .max_by(|a, b| a.version.cmp(&b.version))
            .ok_or(Error::NoMatchingVersion)?;

        Ok(ResolvedPackage {
            name: name.to_string(),
            version: selected.version.clone(),
            download_url: selected.download_url.clone(),
            checksum: selected.checksum.clone(),
        })
    }
}
```

**Test Coverage:**
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_install_package_real_integration() {
        let client = ProductionMarketplaceClient::new();

        let result = client
            .install_package(
                "hello-world",
                "1.0.0",
                &InstallOptions::default(),
            )
            .await;

        assert!(result.is_ok());
        let install_result = result.unwrap();
        assert_eq!(install_result.package_name, "hello-world");
    }

    #[tokio::test]
    async fn test_package_exists() {
        let client = ProductionMarketplaceClient::new();

        let exists = client.package_exists("hello-world", "1.0.0").await.unwrap();
        assert!(exists);

        let not_exists = client.package_exists("nonexistent-pkg", "1.0.0").await.unwrap();
        assert!(!not_exists);
    }

    #[tokio::test]
    async fn test_resolve_version_compatible() {
        let client = ProductionMarketplaceClient::new();

        let constraint = VersionConstraint::Compatible("1.0.0".to_string());
        let resolved = client
            .resolve_version("hello-world", &constraint)
            .await
            .unwrap();

        assert!(resolved.version >= SemanticVersion::parse("1.0.0").unwrap());
        assert!(resolved.version < SemanticVersion::parse("2.0.0").unwrap());
    }
}
```

#### Step 1.3: Implement Pack Installation
```rust
// File: crates/ggen-domain/src/packs/install.rs

pub async fn install_pack(input: &InstallPackInput) -> Result<InstallPackOutput> {
    let marketplace = ProductionMarketplaceClient::new();

    // 1. Load pack manifest
    let pack = load_pack_metadata(&input.pack_id)?;

    tracing::info!("Installing pack: {} v{}", pack.name, pack.version);

    // 2. Resolve dependencies if requested
    let packages_to_install = if input.with_dependencies {
        let resolver = DependencyResolver::new(Arc::new(marketplace.clone()));
        let dep_graph = resolver.resolve(&pack).await?;

        // Get install order (topological sort)
        let install_order = dep_graph.topological_sort()?;

        // Flatten to package list
        install_order
            .into_iter()
            .flat_map(|pack_id| {
                dep_graph.nodes[&pack_id].pack.packages.clone()
            })
            .collect()
    } else {
        pack.packages.clone()
    };

    // 3. Install packages
    let mut installed = Vec::new();
    let total = packages_to_install.len();

    for (index, package_ref) in packages_to_install.iter().enumerate() {
        tracing::info!("[{}/{}] Installing {}...", index + 1, total, package_ref.name);

        // Resolve version
        let resolved = marketplace
            .resolve_version(&package_ref.name, &package_ref.version)
            .await?;

        // Install package
        let result = marketplace
            .install_package(
                &resolved.name,
                &resolved.version.to_string(),
                &InstallOptions {
                    target_path: input.target_dir.clone(),
                    force: input.force,
                    ..Default::default()
                },
            )
            .await?;

        installed.push(result);

        tracing::info!("✓ {} installed", package_ref.name);
    }

    // 4. Update lockfile
    if !input.dry_run {
        update_lockfile(&pack, &installed)?;
    }

    Ok(InstallPackOutput {
        pack_id: input.pack_id.clone(),
        pack_name: pack.name,
        packages_installed: installed.iter().map(|r| r.package_name.clone()).collect(),
        total_packages: installed.len(),
    })
}
```

**Integration Test:**
```rust
#[tokio::test]
async fn test_install_pack_end_to_end() {
    // Create test pack manifest
    let pack = Pack::new("test-pack", "Test Pack", "1.0.0")
        .with_package(PackageRef::exact("hello-world", "1.0.0"));

    // Save manifest
    save_test_pack(&pack).await.unwrap();

    // Install pack
    let input = InstallPackInput {
        pack_id: "test-pack".to_string(),
        target_dir: Some(temp_dir()),
        force: false,
        with_dependencies: true,
        dry_run: false,
    };

    let result = install_pack(&input).await.unwrap();

    assert_eq!(result.packages_installed.len(), 1);
    assert_eq!(result.packages_installed[0], "hello-world");

    // Verify package installed
    let install_path = temp_dir().join("hello-world");
    assert!(install_path.exists());
}
```

---

## Phase 2: Generation (Week 3)

### Goal
Enable project generation from pack templates with real template engine.

### Deliverables
1. ✅ Template engine adapter
2. ✅ Variable resolution system
3. ✅ File tree generation
4. ✅ Post-generation hooks

### Implementation Steps

#### Step 2.1: Create Template Engine Adapter
```rust
// File: crates/ggen-domain/src/packs/adapters/template.rs

use ggen_core::{Generator, GenContext, Pipeline};

#[async_trait]
pub trait TemplateEngine: Send + Sync {
    async fn render_file(
        &self,
        template_path: &Path,
        variables: &HashMap<String, String>,
    ) -> Result<String>;

    fn render_string(
        &self,
        template: &str,
        variables: &HashMap<String, String>,
    ) -> Result<String>;

    async fn generate_file_tree(
        &self,
        templates: &[PackTemplate],
        variables: &HashMap<String, String>,
        output_dir: &Path,
    ) -> Result<GenerationResult>;
}

pub struct ProductionTemplateEngine {
    pipeline: Arc<Pipeline>,
}

impl ProductionTemplateEngine {
    pub fn new() -> Result<Self> {
        Ok(Self {
            pipeline: Arc::new(Pipeline::new()?),
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
        // Convert to BTreeMap for ggen-core
        let vars: BTreeMap<String, String> = variables
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

        // Create temp output dir
        let temp_dir = tempfile::tempdir()?;

        // Create generation context
        let ctx = GenContext::new(
            template_path.to_path_buf(),
            temp_dir.path().to_path_buf(),
        )
        .with_vars(vars);

        // Generate
        let mut generator = Generator::new(self.pipeline.as_ref().clone(), ctx);
        let output_path = generator.generate()?;

        // Read content
        let content = tokio::fs::read_to_string(&output_path).await?;

        Ok(content)
    }

    fn render_string(
        &self,
        template: &str,
        variables: &HashMap<String, String>,
    ) -> Result<String> {
        // Create temp template file
        let temp_dir = tempfile::tempdir()?;
        let template_path = temp_dir.path().join("template.tmpl");

        std::fs::write(&template_path, template)?;

        // Render using render_file
        tokio::runtime::Handle::current()
            .block_on(self.render_file(&template_path, variables))
    }

    async fn generate_file_tree(
        &self,
        templates: &[PackTemplate],
        variables: &HashMap<String, String>,
        output_dir: &Path,
    ) -> Result<GenerationResult> {
        tokio::fs::create_dir_all(output_dir).await?;

        let mut files_created = 0;
        let mut total_bytes = 0;
        let mut errors = Vec::new();

        for template in templates {
            match self.render_template(template, variables, output_dir).await {
                Ok(result) => {
                    files_created += 1;
                    total_bytes += result.bytes_written;
                }
                Err(e) => {
                    tracing::error!("Failed to render {}: {}", template.name, e);
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

        // Ensure parent dir exists
        if let Some(parent) = output_path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        // Check overwrite
        if output_path.exists() && !template.overwrite {
            return Err(Error::FileExists(output_path));
        }

        // Render content
        let content = self.render_file(&template.template_path, variables).await?;

        // Write file
        tokio::fs::write(&output_path, &content).await?;

        // Set permissions if specified
        #[cfg(unix)]
        if let Some(perms) = template.permissions {
            use std::os::unix::fs::PermissionsExt;
            let permissions = std::fs::Permissions::from_mode(perms);
            std::fs::set_permissions(&output_path, permissions)?;
        }

        Ok(RenderResult {
            output_path,
            bytes_written: content.len(),
        })
    }
}
```

#### Step 2.2: Implement Generation Service
```rust
// File: crates/ggen-domain/src/packs/generator.rs

pub async fn generate_from_pack(input: &GenerateInput) -> Result<GenerateOutput> {
    let template_engine = ProductionTemplateEngine::new()?;

    // 1. Load pack
    let pack = load_pack_metadata(&input.pack_id)?;

    tracing::info!("Generating project from pack: {}", pack.name);

    // 2. Resolve variables
    let variables = resolve_variables(&pack, input)?;

    // 3. Validate variables
    validate_variables(&pack.templates, &variables)?;

    // 4. Filter templates if specific template requested
    let templates = if let Some(ref tmpl_name) = input.template_name {
        pack.templates
            .iter()
            .filter(|t| t.name == *tmpl_name)
            .cloned()
            .collect()
    } else {
        pack.templates.clone()
    };

    // 5. Generate file tree
    let result = template_engine
        .generate_file_tree(&templates, &variables, &input.output_dir)
        .await?;

    // 6. Run post-generation hooks
    if !input.skip_hooks {
        run_post_generation_hooks(&pack, &input.output_dir)?;
    }

    Ok(GenerateOutput {
        pack_id: input.pack_id.clone(),
        project_name: input.project_name.clone(),
        templates_generated: result.files_created,
        files_created: result.files_created,
        output_path: result.output_dir,
    })
}

fn resolve_variables(pack: &Pack, input: &GenerateInput) -> Result<HashMap<String, String>> {
    let mut variables = input.variables.clone();

    // Add auto-detected variables
    variables.entry("project_name".to_string())
        .or_insert_with(|| input.project_name.clone());

    variables.entry("timestamp".to_string())
        .or_insert_with(|| Utc::now().to_rfc3339());

    // Add defaults from templates
    for template in &pack.templates {
        for (key, value) in &template.optional_variables {
            variables.entry(key.clone()).or_insert(value.clone());
        }
    }

    Ok(variables)
}

fn validate_variables(
    templates: &[PackTemplate],
    variables: &HashMap<String, String>,
) -> Result<()> {
    for template in templates {
        for var in &template.variables {
            if var.required {
                let value = variables
                    .get(&var.name)
                    .ok_or_else(|| Error::MissingRequiredVariable {
                        template: template.name.clone(),
                        variable: var.name.clone(),
                    })?;

                var.validate_value(value)?;
            }
        }
    }

    Ok(())
}
```

**Integration Test:**
```rust
#[tokio::test]
async fn test_generate_from_pack_full_workflow() {
    // Create test pack with template
    let pack = Pack::new("test-pack", "Test", "1.0.0")
        .with_template(PackTemplate {
            name: "main".to_string(),
            template_path: create_test_template("Hello {{ name }}!"),
            output_path: "output.txt".to_string(),
            variables: vec![TemplateVariable {
                name: "name".to_string(),
                required: true,
                var_type: VariableType::String,
                ..Default::default()
            }],
            ..Default::default()
        });

    save_test_pack(&pack).await.unwrap();

    // Generate
    let input = GenerateInput {
        pack_id: "test-pack".to_string(),
        project_name: "my-project".to_string(),
        output_dir: temp_dir(),
        variables: hashmap! {
            "name".to_string() => "World".to_string(),
        },
        ..Default::default()
    };

    let result = generate_from_pack(&input).await.unwrap();

    assert_eq!(result.files_created, 1);

    // Verify output
    let content = std::fs::read_to_string(temp_dir().join("output.txt")).unwrap();
    assert_eq!(content, "Hello World!");
}
```

---

## Phase 3: Dependencies (Week 4)

### Goal
Implement dependency resolution with circular detection and composition.

### Deliverables
1. ✅ Dependency graph builder
2. ✅ Circular dependency detection
3. ✅ Topological sort for install order
4. ✅ Pack composition

### Implementation Steps

#### Step 3.1: Dependency Resolver
```rust
// File: crates/ggen-domain/src/packs/dependency_resolver.rs

pub struct DependencyResolver {
    marketplace: Arc<dyn MarketplaceClient>,
    cache: Arc<Mutex<HashMap<PackId, Pack>>>,
}

impl DependencyResolver {
    pub async fn resolve(&self, pack: &Pack) -> Result<DependencyGraph> {
        let mut graph = DependencyGraph {
            nodes: HashMap::new(),
            edges: Vec::new(),
        };

        let mut visited = HashSet::new();

        self.resolve_recursive(pack, &mut graph, &mut visited).await?;

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
            graph.edges.push((pack_id.clone(), dep.pack_id.clone()));

            // Load dependent pack
            let dep_pack = self.load_pack(&dep.pack_id, &dep.version).await?;

            // Recurse
            self.resolve_recursive(&dep_pack, graph, visited).await?;
        }

        Ok(())
    }

    async fn load_pack(&self, pack_id: &PackId, version: &VersionConstraint) -> Result<Pack> {
        // Check cache
        let cache_key = pack_id.clone();

        {
            let cache = self.cache.lock().unwrap();
            if let Some(pack) = cache.get(&cache_key) {
                return Ok(pack.clone());
            }
        }

        // Load from registry
        let pack = load_pack_metadata(&pack_id.0)?;

        // Verify version matches constraint
        if !version.satisfies(&pack.version) {
            return Err(Error::VersionMismatch {
                pack: pack_id.0.clone(),
                required: version.clone(),
                found: pack.version.clone(),
            });
        }

        // Cache
        {
            let mut cache = self.cache.lock().unwrap();
            cache.insert(cache_key, pack.clone());
        }

        Ok(pack)
    }
}

impl DependencyGraph {
    pub fn validate_acyclic(&self) -> Result<()> {
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();

        for pack_id in self.nodes.keys() {
            if !visited.contains(pack_id) {
                self.dfs_detect_cycle(pack_id, &mut visited, &mut rec_stack)?;
            }
        }

        Ok(())
    }

    fn dfs_detect_cycle(
        &self,
        pack_id: &PackId,
        visited: &mut HashSet<PackId>,
        rec_stack: &mut HashSet<PackId>,
    ) -> Result<()> {
        visited.insert(pack_id.clone());
        rec_stack.insert(pack_id.clone());

        if let Some(node) = self.nodes.get(pack_id) {
            for dep_id in &node.dependencies {
                if !visited.contains(dep_id) {
                    self.dfs_detect_cycle(dep_id, visited, rec_stack)?;
                } else if rec_stack.contains(dep_id) {
                    return Err(Error::CircularDependency {
                        from: pack_id.clone(),
                        to: dep_id.clone(),
                    });
                }
            }
        }

        rec_stack.remove(pack_id);
        Ok(())
    }

    pub fn topological_sort(&self) -> Result<Vec<PackId>> {
        let mut in_degree: HashMap<PackId, usize> = HashMap::new();
        let mut adj_list: HashMap<PackId, Vec<PackId>> = HashMap::new();

        // Initialize
        for pack_id in self.nodes.keys() {
            in_degree.insert(pack_id.clone(), 0);
            adj_list.insert(pack_id.clone(), Vec::new());
        }

        // Build adjacency list
        for (from, to) in &self.edges {
            adj_list.get_mut(from).unwrap().push(to.clone());
            *in_degree.get_mut(to).unwrap() += 1;
        }

        // Kahn's algorithm
        let mut queue: VecDeque<PackId> = in_degree
            .iter()
            .filter(|(_, &degree)| degree == 0)
            .map(|(id, _)| id.clone())
            .collect();

        let mut result = Vec::new();

        while let Some(pack_id) = queue.pop_front() {
            result.push(pack_id.clone());

            for neighbor in adj_list.get(&pack_id).unwrap() {
                let degree = in_degree.get_mut(neighbor).unwrap();
                *degree -= 1;
                if *degree == 0 {
                    queue.push_back(neighbor.clone());
                }
            }
        }

        if result.len() != self.nodes.len() {
            return Err(Error::CycleInTopologicalSort);
        }

        Ok(result)
    }
}
```

**Test Coverage:**
```rust
#[tokio::test]
async fn test_dependency_resolution_linear() {
    // A -> B -> C
    let pack_a = Pack::new("a", "A", "1.0.0")
        .with_dependency(PackDependency::new("b", "1.0.0"));

    let pack_b = Pack::new("b", "B", "1.0.0")
        .with_dependency(PackDependency::new("c", "1.0.0"));

    let pack_c = Pack::new("c", "C", "1.0.0");

    // Setup registry
    setup_test_registry(vec![pack_a.clone(), pack_b, pack_c]);

    // Resolve
    let resolver = DependencyResolver::new(test_marketplace());
    let graph = resolver.resolve(&pack_a).await.unwrap();

    assert_eq!(graph.nodes.len(), 3);

    // Verify install order
    let order = graph.topological_sort().unwrap();
    assert_eq!(order, vec![
        PackId("c".to_string()),
        PackId("b".to_string()),
        PackId("a".to_string()),
    ]);
}

#[tokio::test]
async fn test_circular_dependency_detection() {
    // A -> B -> C -> A (cycle)
    let pack_a = Pack::new("a", "A", "1.0.0")
        .with_dependency(PackDependency::new("b", "1.0.0"));

    let pack_b = Pack::new("b", "B", "1.0.0")
        .with_dependency(PackDependency::new("c", "1.0.0"));

    let pack_c = Pack::new("c", "C", "1.0.0")
        .with_dependency(PackDependency::new("a", "1.0.0"));

    setup_test_registry(vec![pack_a.clone(), pack_b, pack_c]);

    let resolver = DependencyResolver::new(test_marketplace());
    let result = resolver.resolve(&pack_a).await;

    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Circular dependency"));
}
```

---

## Phase 4: Polish (Week 5)

### Goal
Production-ready error handling, performance, and telemetry.

### Deliverables
1. ✅ Comprehensive error handling for all failure modes
2. ✅ Performance optimization (caching, parallelization)
3. ✅ Telemetry and metrics
4. ✅ Complete test coverage (>90%)

### Implementation Steps

#### Step 4.1: Error Handling
```rust
// File: crates/ggen-domain/src/packs/error.rs

#[derive(Debug, thiserror::Error)]
pub enum PacksError {
    // FM1: Pack manifest not found
    #[error("Pack manifest not found: {0}. Run 'ggen packs list' to see available packs.")]
    ManifestNotFound(String),

    // FM2: Marketplace package install fails
    #[error("Failed to install package {package}: {reason}. Attempted {attempts} times.")]
    InstallationFailed {
        package: String,
        reason: String,
        attempts: u32,
    },

    // FM3: Template rendering fails
    #[error("Template rendering failed: {template} - {reason}")]
    TemplateRenderingFailed { template: String, reason: String },

    // FM4: Circular dependency
    #[error("Circular dependency detected: {from} -> {to}")]
    CircularDependency { from: PackId, to: PackId },

    // FM5: Version conflict
    #[error("Version conflict: {package} required as {required} and {conflicting}")]
    VersionConflict {
        package: String,
        required: String,
        conflicting: String,
    },

    // FM6: Partial installation state
    #[error("Installation incomplete. Run 'ggen packs install {pack} --resume' to continue.")]
    PartialInstallation { pack: String },

    // FM7: Missing template variable
    #[error("Required variable missing: {variable} in template {template}")]
    MissingVariable { variable: String, template: String },

    // Additional errors...
}

impl PacksError {
    /// Get user-friendly suggestion for error
    pub fn suggestion(&self) -> Option<String> {
        match self {
            Self::ManifestNotFound(_) => {
                Some("Run 'ggen packs list' to see available packs".to_string())
            }
            Self::InstallationFailed { .. } => {
                Some("Check internet connection and retry".to_string())
            }
            Self::MissingVariable { variable, .. } => {
                Some(format!("Provide variable with: --var {}=<value>", variable))
            }
            _ => None,
        }
    }

    /// Get exit code for CLI
    pub fn exit_code(&self) -> i32 {
        match self {
            Self::ManifestNotFound(_) => 1,
            Self::InstallationFailed { .. } => 2,
            Self::TemplateRenderingFailed { .. } => 3,
            Self::CircularDependency { .. } => 4,
            Self::VersionConflict { .. } => 5,
            Self::PartialInstallation { .. } => 6,
            Self::MissingVariable { .. } => 7,
        }
    }
}
```

#### Step 4.2: Performance Optimization
```rust
// File: crates/ggen-domain/src/packs/cache.rs

pub struct PacksCache {
    manifest_cache: Arc<Mutex<HashMap<String, (Pack, Instant)>>>,
    package_existence_cache: Arc<Mutex<HashMap<String, (bool, Instant)>>>,
    ttl: Duration,
}

impl PacksCache {
    pub fn new(ttl: Duration) -> Self {
        Self {
            manifest_cache: Arc::new(Mutex::new(HashMap::new())),
            package_existence_cache: Arc::new(Mutex::new(HashMap::new())),
            ttl,
        }
    }

    pub fn get_manifest(&self, pack_id: &str) -> Option<Pack> {
        let cache = self.manifest_cache.lock().unwrap();

        if let Some((pack, cached_at)) = cache.get(pack_id) {
            if cached_at.elapsed() < self.ttl {
                return Some(pack.clone());
            }
        }

        None
    }

    pub fn set_manifest(&self, pack_id: &str, pack: &Pack) {
        let mut cache = self.manifest_cache.lock().unwrap();
        cache.insert(pack_id.to_string(), (pack.clone(), Instant::now()));
    }

    pub fn invalidate(&self, pack_id: &str) {
        let mut manifest_cache = self.manifest_cache.lock().unwrap();
        manifest_cache.remove(pack_id);
    }

    pub fn clear(&self) {
        let mut manifest_cache = self.manifest_cache.lock().unwrap();
        let mut existence_cache = self.package_existence_cache.lock().unwrap();

        manifest_cache.clear();
        existence_cache.clear();
    }
}

// Parallel package installation
pub async fn install_packages_parallel(
    packages: Vec<PackageRef>,
    marketplace: Arc<dyn MarketplaceClient>,
    max_concurrency: usize,
) -> Result<Vec<InstallResult>> {
    use futures::stream::{self, StreamExt};

    let results = stream::iter(packages)
        .map(|pkg| {
            let marketplace = marketplace.clone();
            async move {
                marketplace
                    .install_package(&pkg.name, &pkg.version.to_string(), &InstallOptions::default())
                    .await
            }
        })
        .buffer_unordered(max_concurrency)
        .collect::<Vec<_>>()
        .await;

    results.into_iter().collect()
}
```

#### Step 4.3: Telemetry
```rust
// File: crates/ggen-domain/src/packs/telemetry.rs

use tracing::{info, warn, error};

pub struct PacksTelemetry {
    metrics: Arc<Mutex<PacksMetrics>>,
}

#[derive(Debug, Default)]
pub struct PacksMetrics {
    pub installs_total: u64,
    pub installs_failed: u64,
    pub generations_total: u64,
    pub generations_failed: u64,
    pub cache_hits: u64,
    pub cache_misses: u64,
    pub avg_install_time_ms: f64,
    pub avg_generation_time_ms: f64,
}

impl PacksTelemetry {
    pub fn record_install(&self, duration: Duration, success: bool) {
        let mut metrics = self.metrics.lock().unwrap();

        metrics.installs_total += 1;
        if !success {
            metrics.installs_failed += 1;
        }

        // Update rolling average
        let n = metrics.installs_total as f64;
        let new_time = duration.as_millis() as f64;
        metrics.avg_install_time_ms =
            (metrics.avg_install_time_ms * (n - 1.0) + new_time) / n;

        info!(
            install_time_ms = duration.as_millis(),
            success = success,
            "Pack installation complete"
        );
    }

    pub fn record_generation(&self, duration: Duration, files_created: usize, success: bool) {
        let mut metrics = self.metrics.lock().unwrap();

        metrics.generations_total += 1;
        if !success {
            metrics.generations_failed += 1;
        }

        let n = metrics.generations_total as f64;
        let new_time = duration.as_millis() as f64;
        metrics.avg_generation_time_ms =
            (metrics.avg_generation_time_ms * (n - 1.0) + new_time) / n;

        info!(
            generation_time_ms = duration.as_millis(),
            files_created = files_created,
            success = success,
            "Pack generation complete"
        );
    }

    pub fn get_metrics(&self) -> PacksMetrics {
        self.metrics.lock().unwrap().clone()
    }
}
```

---

## Testing Strategy

### Unit Tests (per module)
Target: 90%+ coverage per module

```bash
# Run unit tests
cargo test --package ggen-domain --lib packs

# Check coverage
cargo tarpaulin --package ggen-domain --lib packs --out Html
```

### Integration Tests
Test cross-module interactions:

```bash
# Run integration tests
cargo test --package ggen-domain --test packs_integration
```

### E2E Tests
Test complete workflows:

```bash
# Run E2E tests (requires real marketplace)
cargo test --package ggen-domain --test packs_e2e -- --ignored
```

### Performance Tests
```bash
# Run benchmarks
cargo bench --package ggen-domain --bench packs_benchmarks
```

---

## Deployment Checklist

### Before Release
- [ ] All unit tests pass (90%+ coverage)
- [ ] All integration tests pass
- [ ] All 6 user workflows work end-to-end
- [ ] Performance benchmarks meet targets
- [ ] Documentation complete (API docs + user guide)
- [ ] FMEA coverage for all failure modes
- [ ] Security audit passed
- [ ] Breaking changes documented

### Health Score Validation
```bash
# Run health score calculation
cargo run --bin ggen -- packs validate --all --deep
```

Target: 90%+ for all packs

### Release Notes Template
```markdown
## ggen v3.3.0 - Production-Ready Packs System

### New Features
- ✅ Complete pack installation with real marketplace integration
- ✅ Project generation from pack templates
- ✅ Dependency resolution with circular detection
- ✅ Pack composition support
- ✅ SPARQL metadata queries
- ✅ Full deployment workflows

### Performance
- Install time: 17s for typical pack (8 packages)
- Generation time: 2.3s for 12 templates
- Dependency resolution: <1s for 18 packages
- 90%+ test coverage

### Breaking Changes
- None (backward compatible)

### Migration Guide
- Existing packs continue to work
- New features opt-in via flags
```

---

**Related Documents:**
- [Architecture Overview](01_ARCHITECTURE_OVERVIEW.md)
- [Command Specification](02_COMMAND_SPECIFICATION.md)
- [Data Model](03_DATA_MODEL.md)
- [Integration Layer](04_INTEGRATION_LAYER.md)
- [User Workflows](05_USER_WORKFLOWS.md)
