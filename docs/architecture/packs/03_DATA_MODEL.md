<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Packs Data Model - Rust Trait Definitions](#packs-data-model---rust-trait-definitions)
  - [Core Types Hierarchy](#core-types-hierarchy)
  - [1. Pack - Root Entity](#1-pack---root-entity)
  - [2. PackMetadata - Quality Metrics](#2-packmetadata---quality-metrics)
  - [3. PackageRef - Marketplace Package Reference](#3-packageref---marketplace-package-reference)
  - [4. PackTemplate - Code Generation Template](#4-packtemplate---code-generation-template)
  - [5. PackDependency - Inter-Pack Dependencies](#5-packdependency---inter-pack-dependencies)
  - [6. Supporting Types](#6-supporting-types)
  - [7. Composition Types](#7-composition-types)
  - [Error Types](#error-types)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Packs Data Model - Rust Trait Definitions

**Version:** 3.2.0
**Target:** Production-ready type system with 90%+ health score

## Core Types Hierarchy

```
Pack (root entity)
├── PackMetadata (identity & discovery)
├── PackageRef[] (what to install)
│   └── PackageConstraints
├── PackTemplate[] (what to generate)
│   ├── TemplateVariable[]
│   └── TemplateOutput
├── PackDependency[] (other packs needed)
│   └── DependencyConstraints
└── PackComposition (how to merge)
```

---

## 1. Pack - Root Entity

```rust
/// Main pack definition representing a curated collection of packages and templates
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Pack {
    /// Unique identifier (e.g., "web-api-starter")
    pub id: PackId,

    /// Human-readable name (e.g., "Web API Starter Pack")
    pub name: String,

    /// Semantic version (e.g., "1.2.0")
    pub version: SemanticVersion,

    /// Short description (max 200 chars)
    pub description: String,

    /// Long description with usage guide (optional)
    pub readme: Option<String>,

    /// Pack category for organization
    pub category: PackCategory,

    /// Author information
    pub author: Option<Author>,

    /// Source repository
    pub repository: Option<RepositoryInfo>,

    /// License identifier (SPDX format)
    pub license: Option<String>,

    /// Packages to install from marketplace
    pub packages: Vec<PackageRef>,

    /// Templates for code generation
    pub templates: Vec<PackTemplate>,

    /// Predefined SPARQL queries for metadata operations
    #[serde(default)]
    pub sparql_queries: HashMap<String, String>,

    /// Dependencies on other packs
    #[serde(default)]
    pub dependencies: Vec<PackDependency>,

    /// Post-generation hooks (scripts to run after generation)
    #[serde(default)]
    pub hooks: PackHooks,

    /// Search and discovery metadata
    #[serde(default)]
    pub tags: Vec<String>,

    #[serde(default)]
    pub keywords: Vec<String>,

    /// Production readiness indicators
    #[serde(default)]
    pub production_ready: bool,

    /// Telemetry and quality metadata
    #[serde(default)]
    pub metadata: PackMetadata,

    /// Creation and modification timestamps
    pub created_at: Option<DateTime<Utc>>,
    pub updated_at: Option<DateTime<Utc>>,
}

impl Pack {
    /// Create new pack with required fields
    pub fn new(id: impl Into<String>, name: impl Into<String>, version: impl Into<String>) -> Self {
        Self {
            id: PackId(id.into()),
            name: name.into(),
            version: SemanticVersion::parse(&version.into()).expect("Invalid version"),
            description: String::new(),
            readme: None,
            category: PackCategory::Other,
            author: None,
            repository: None,
            license: None,
            packages: Vec::new(),
            templates: Vec::new(),
            sparql_queries: HashMap::new(),
            dependencies: Vec::new(),
            hooks: PackHooks::default(),
            tags: Vec::new(),
            keywords: Vec::new(),
            production_ready: false,
            metadata: PackMetadata::default(),
            created_at: Some(Utc::now()),
            updated_at: Some(Utc::now()),
        }
    }

    /// Add package reference
    pub fn with_package(mut self, package: PackageRef) -> Self {
        self.packages.push(package);
        self
    }

    /// Add template
    pub fn with_template(mut self, template: PackTemplate) -> Self {
        self.templates.push(template);
        self
    }

    /// Add dependency
    pub fn with_dependency(mut self, dep: PackDependency) -> Self {
        self.dependencies.push(dep);
        self
    }

    /// Validate pack integrity
    pub fn validate(&self) -> Result<(), ValidationError> {
        // Validate ID format
        if self.id.0.is_empty() || self.id.0.len() > 100 {
            return Err(ValidationError::InvalidPackId);
        }

        // Validate packages exist
        if self.packages.is_empty() && self.templates.is_empty() {
            return Err(ValidationError::EmptyPack);
        }

        // Validate templates have valid paths
        for template in &self.templates {
            template.validate()?;
        }

        // Validate no circular dependencies (requires dependency graph)
        self.check_circular_dependencies()?;

        Ok(())
    }

    /// Check for circular dependencies
    fn check_circular_dependencies(&self) -> Result<(), ValidationError> {
        // Build dependency graph and detect cycles
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();

        fn dfs(
            pack_id: &PackId,
            all_packs: &HashMap<PackId, Pack>,
            visited: &mut HashSet<PackId>,
            rec_stack: &mut HashSet<PackId>,
        ) -> Result<(), ValidationError> {
            visited.insert(pack_id.clone());
            rec_stack.insert(pack_id.clone());

            if let Some(pack) = all_packs.get(pack_id) {
                for dep in &pack.dependencies {
                    let dep_id = &dep.pack_id;

                    if !visited.contains(dep_id) {
                        dfs(dep_id, all_packs, visited, rec_stack)?;
                    } else if rec_stack.contains(dep_id) {
                        return Err(ValidationError::CircularDependency {
                            from: pack_id.clone(),
                            to: dep_id.clone(),
                        });
                    }
                }
            }

            rec_stack.remove(pack_id);
            Ok(())
        }

        // Note: Full validation requires loading all packs
        // This is simplified; production version would load from registry
        Ok(())
    }
}
```

---

## 2. PackMetadata - Quality Metrics

```rust
/// Metadata for pack quality and telemetry
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq)]
pub struct PackMetadata {
    /// Test coverage percentage
    #[serde(skip_serializing_if = "Option::is_none")]
    pub test_coverage: Option<f64>,

    /// RDF ontology size in triples
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rdf_ontology_size: Option<usize>,

    /// Number of SPARQL query templates
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sparql_templates: Option<usize>,

    /// Number of code examples
    #[serde(skip_serializing_if = "Option::is_none")]
    pub code_examples: Option<usize>,

    /// Number of documentation files
    #[serde(skip_serializing_if = "Option::is_none")]
    pub documentation_files: Option<usize>,

    /// Health score (0-100)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub health_score: Option<u8>,

    /// Download count
    #[serde(skip_serializing_if = "Option::is_none")]
    pub download_count: Option<u64>,

    /// Star count (GitHub)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub star_count: Option<u32>,

    /// Last validation timestamp
    #[serde(skip_serializing_if = "Option::is_none")]
    pub last_validated: Option<DateTime<Utc>>,
}

impl PackMetadata {
    /// Calculate health score from metadata
    pub fn calculate_health_score(&self) -> u8 {
        let mut score = 0u8;

        // Test coverage (max 30 points)
        if let Some(coverage) = self.test_coverage {
            score += (coverage * 0.3).min(30.0) as u8;
        }

        // Documentation (max 20 points)
        if let Some(docs) = self.documentation_files {
            score += docs.min(10) as u8 * 2;
        }

        // SPARQL queries (max 15 points)
        if let Some(queries) = self.sparql_templates {
            score += queries.min(5) as u8 * 3;
        }

        // Code examples (max 15 points)
        if let Some(examples) = self.code_examples {
            score += examples.min(5) as u8 * 3;
        }

        // RDF ontology (max 20 points)
        if let Some(size) = self.rdf_ontology_size {
            score += (size / 10).min(20) as u8;
        }

        score.min(100)
    }
}
```

---

## 3. PackageRef - Marketplace Package Reference

```rust
/// Reference to a marketplace package to install
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct PackageRef {
    /// Package name in marketplace
    pub name: String,

    /// Version constraint (semver range or exact)
    pub version: VersionConstraint,

    /// Whether package is optional
    #[serde(default)]
    pub optional: bool,

    /// Features to enable (for Cargo-style packages)
    #[serde(default)]
    pub features: Vec<String>,

    /// Custom install path (relative to pack root)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub install_path: Option<PathBuf>,
}

impl PackageRef {
    /// Create exact version reference
    pub fn exact(name: impl Into<String>, version: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            version: VersionConstraint::Exact(version.into()),
            optional: false,
            features: Vec::new(),
            install_path: None,
        }
    }

    /// Create compatible version reference (^)
    pub fn compatible(name: impl Into<String>, version: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            version: VersionConstraint::Compatible(version.into()),
            optional: false,
            features: Vec::new(),
            install_path: None,
        }
    }

    /// Create range version reference
    pub fn range(name: impl Into<String>, min: impl Into<String>, max: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            version: VersionConstraint::Range {
                min: min.into(),
                max: max.into(),
            },
            optional: false,
            features: Vec::new(),
            install_path: None,
        }
    }

    /// Mark as optional
    pub fn optional(mut self) -> Self {
        self.optional = true;
        self
    }

    /// Add features
    pub fn with_features(mut self, features: Vec<String>) -> Self {
        self.features = features;
        self
    }

    /// Resolve to actual version from marketplace
    pub async fn resolve(&self, marketplace: &dyn MarketplaceClient) -> Result<ResolvedPackage> {
        marketplace.resolve_version(&self.name, &self.version).await
    }
}

/// Version constraint for package resolution
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(untagged)]
pub enum VersionConstraint {
    /// Exact version (e.g., "1.2.3")
    Exact(String),

    /// Compatible version (e.g., "^1.2.3" means >=1.2.3 <2.0.0)
    Compatible(String),

    /// Tilde version (e.g., "~1.2.3" means >=1.2.3 <1.3.0)
    Tilde(String),

    /// Version range (e.g., ">=1.0.0 <2.0.0")
    Range { min: String, max: String },

    /// Latest version
    Latest,
}

impl VersionConstraint {
    /// Check if given version satisfies constraint
    pub fn satisfies(&self, version: &SemanticVersion) -> bool {
        match self {
            VersionConstraint::Exact(v) => {
                SemanticVersion::parse(v).map(|exact| exact == *version).unwrap_or(false)
            }
            VersionConstraint::Compatible(v) => {
                SemanticVersion::parse(v)
                    .map(|base| version.is_compatible_with(&base))
                    .unwrap_or(false)
            }
            VersionConstraint::Tilde(v) => {
                SemanticVersion::parse(v)
                    .map(|base| version.is_tilde_compatible_with(&base))
                    .unwrap_or(false)
            }
            VersionConstraint::Range { min, max } => {
                let min_ver = SemanticVersion::parse(min).ok();
                let max_ver = SemanticVersion::parse(max).ok();

                match (min_ver, max_ver) {
                    (Some(min), Some(max)) => version >= &min && version < &max,
                    _ => false,
                }
            }
            VersionConstraint::Latest => true,
        }
    }
}
```

---

## 4. PackTemplate - Code Generation Template

```rust
/// Template for code generation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PackTemplate {
    /// Template identifier
    pub name: String,

    /// Path to template file (relative to pack root)
    pub template_path: PathBuf,

    /// Output path (relative to project root, supports Tera templates)
    /// Example: "src/{{ module_name }}.rs"
    pub output_path: String,

    /// Template description
    pub description: String,

    /// Required variables for rendering
    #[serde(default)]
    pub variables: Vec<TemplateVariable>,

    /// Optional variables with defaults
    #[serde(default)]
    pub optional_variables: HashMap<String, String>,

    /// Template engine (default: Tera)
    #[serde(default)]
    pub engine: TemplateEngine,

    /// Whether to overwrite existing files
    #[serde(default)]
    pub overwrite: bool,

    /// File permissions (Unix-style, e.g., 0o755 for executable)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub permissions: Option<u32>,
}

impl PackTemplate {
    /// Create new template
    pub fn new(
        name: impl Into<String>,
        template_path: impl Into<PathBuf>,
        output_path: impl Into<String>,
    ) -> Self {
        Self {
            name: name.into(),
            template_path: template_path.into(),
            output_path: output_path.into(),
            description: String::new(),
            variables: Vec::new(),
            optional_variables: HashMap::new(),
            engine: TemplateEngine::Tera,
            overwrite: false,
            permissions: None,
        }
    }

    /// Add required variable
    pub fn with_variable(mut self, var: TemplateVariable) -> Self {
        self.variables.push(var);
        self
    }

    /// Add optional variable with default
    pub fn with_optional(mut self, name: impl Into<String>, default: impl Into<String>) -> Self {
        self.optional_variables.insert(name.into(), default.into());
        self
    }

    /// Validate template configuration
    pub fn validate(&self) -> Result<(), ValidationError> {
        // Check template path exists (relative to pack)
        if self.template_path.as_os_str().is_empty() {
            return Err(ValidationError::EmptyTemplatePath);
        }

        // Check output path is valid
        if self.output_path.is_empty() {
            return Err(ValidationError::EmptyOutputPath);
        }

        // Validate variable names are valid identifiers
        for var in &self.variables {
            var.validate()?;
        }

        Ok(())
    }

    /// Render template with variables
    pub async fn render(
        &self,
        variables: &HashMap<String, String>,
        engine: &dyn TemplateEngineBackend,
    ) -> Result<RenderedTemplate> {
        // Merge required, optional, and provided variables
        let mut all_vars = self.optional_variables.clone();
        all_vars.extend(variables.clone());

        // Validate all required variables present
        for var in &self.variables {
            if var.required && !all_vars.contains_key(&var.name) {
                return Err(TemplateError::MissingRequiredVariable {
                    template: self.name.clone(),
                    variable: var.name.clone(),
                }
                .into());
            }
        }

        // Render output path (may contain variables)
        let output_path = engine.render_string(&self.output_path, &all_vars)?;

        // Render template content
        let content = engine.render_file(&self.template_path, &all_vars)?;

        Ok(RenderedTemplate {
            template_name: self.name.clone(),
            output_path: PathBuf::from(output_path),
            content,
            permissions: self.permissions,
        })
    }
}

/// Template variable definition
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TemplateVariable {
    /// Variable name (must be valid identifier)
    pub name: String,

    /// Variable description
    pub description: String,

    /// Data type hint
    pub var_type: VariableType,

    /// Whether variable is required
    #[serde(default = "default_true")]
    pub required: bool,

    /// Default value (if not required)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default: Option<String>,

    /// Validation pattern (regex)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pattern: Option<String>,

    /// Example value
    #[serde(skip_serializing_if = "Option::is_none")]
    pub example: Option<String>,
}

fn default_true() -> bool {
    true
}

impl TemplateVariable {
    /// Validate variable definition
    pub fn validate(&self) -> Result<(), ValidationError> {
        // Name must be valid identifier
        if !self.name.chars().all(|c| c.is_alphanumeric() || c == '_') {
            return Err(ValidationError::InvalidVariableName(self.name.clone()));
        }

        // If pattern specified, validate it compiles
        if let Some(ref pattern) = self.pattern {
            regex::Regex::new(pattern)
                .map_err(|e| ValidationError::InvalidPattern(e.to_string()))?;
        }

        Ok(())
    }

    /// Validate provided value against variable constraints
    pub fn validate_value(&self, value: &str) -> Result<(), ValidationError> {
        // Check pattern if specified
        if let Some(ref pattern) = self.pattern {
            let re = regex::Regex::new(pattern)?;
            if !re.is_match(value) {
                return Err(ValidationError::ValueDoesNotMatchPattern {
                    variable: self.name.clone(),
                    value: value.to_string(),
                    pattern: pattern.clone(),
                });
            }
        }

        // Type validation
        match self.var_type {
            VariableType::String => Ok(()),
            VariableType::Integer => value
                .parse::<i64>()
                .map(|_| ())
                .map_err(|_| ValidationError::InvalidType {
                    variable: self.name.clone(),
                    expected: "integer",
                }),
            VariableType::Boolean => value
                .parse::<bool>()
                .map(|_| ())
                .map_err(|_| ValidationError::InvalidType {
                    variable: self.name.clone(),
                    expected: "boolean",
                }),
            VariableType::Path => {
                PathBuf::from(value);
                Ok(())
            }
        }
    }
}

/// Variable data types
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum VariableType {
    String,
    Integer,
    Boolean,
    Path,
}

/// Template engine backend
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum TemplateEngine {
    Tera,
    Handlebars,
    Liquid,
}

/// Rendered template output
#[derive(Debug, Clone)]
pub struct RenderedTemplate {
    pub template_name: String,
    pub output_path: PathBuf,
    pub content: String,
    pub permissions: Option<u32>,
}
```

---

## 5. PackDependency - Inter-Pack Dependencies

```rust
/// Dependency on another pack
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct PackDependency {
    /// Pack identifier
    pub pack_id: PackId,

    /// Version constraint
    pub version: VersionConstraint,

    /// Whether dependency is optional
    #[serde(default)]
    pub optional: bool,

    /// Reason for dependency (documentation)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,
}

impl PackDependency {
    /// Create new dependency
    pub fn new(pack_id: impl Into<String>, version: impl Into<String>) -> Self {
        Self {
            pack_id: PackId(pack_id.into()),
            version: VersionConstraint::Compatible(version.into()),
            optional: false,
            reason: None,
        }
    }

    /// Mark as optional
    pub fn optional(mut self) -> Self {
        self.optional = true;
        self
    }

    /// Add reason
    pub fn with_reason(mut self, reason: impl Into<String>) -> Self {
        self.reason = Some(reason.into());
        self
    }
}
```

---

## 6. Supporting Types

```rust
/// Pack identifier (newtype for type safety)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PackId(pub String);

/// Pack category for organization
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum PackCategory {
    Web,
    Cli,
    ML,
    Data,
    DevOps,
    Mobile,
    Desktop,
    Game,
    Embedded,
    Library,
    Other,
}

/// Author information
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Author {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub email: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
}

/// Repository information
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RepositoryInfo {
    pub url: String,
    #[serde(rename = "type")]
    pub repo_type: String, // "git", "github", etc.
}

/// Post-generation hooks
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq)]
pub struct PackHooks {
    /// Scripts to run after generation (e.g., "npm install")
    #[serde(default)]
    pub post_generate: Vec<String>,

    /// Scripts to run after installation
    #[serde(default)]
    pub post_install: Vec<String>,
}

/// Resolved package from marketplace
#[derive(Debug, Clone)]
pub struct ResolvedPackage {
    pub name: String,
    pub version: SemanticVersion,
    pub download_url: String,
    pub checksum: String,
}

/// Semantic version parsing and comparison
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct SemanticVersion {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
    pub pre_release: Option<String>,
    pub build: Option<String>,
}

impl SemanticVersion {
    /// Parse semver string
    pub fn parse(s: &str) -> Result<Self, ParseError> {
        // Simplified parser (production would use semver crate)
        let parts: Vec<&str> = s.split('.').collect();
        if parts.len() < 3 {
            return Err(ParseError::InvalidSemanticVersion);
        }

        Ok(Self {
            major: parts[0].parse()?,
            minor: parts[1].parse()?,
            patch: parts[2].parse()?,
            pre_release: None,
            build: None,
        })
    }

    /// Check compatible version (^)
    pub fn is_compatible_with(&self, base: &Self) -> bool {
        self.major == base.major && self >= base
    }

    /// Check tilde compatible version (~)
    pub fn is_tilde_compatible_with(&self, base: &Self) -> bool {
        self.major == base.major && self.minor == base.minor && self >= base
    }
}
```

---

## 7. Composition Types

```rust
/// Pack composition configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackComposition {
    /// Packs to compose
    pub packs: Vec<PackId>,

    /// Composition strategy
    pub strategy: CompositionStrategy,

    /// Conflict resolution rules
    #[serde(default)]
    pub resolution_rules: Vec<ConflictResolution>,
}

/// How to compose multiple packs
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum CompositionStrategy {
    /// Merge all packs (default) - conflicts resolved interactively
    Merge,

    /// Layer packs (apply in order, later overrides earlier)
    Layer,

    /// Custom composition with explicit rules
    Custom(HashMap<String, serde_json::Value>),
}

/// Conflict resolution rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConflictResolution {
    /// Conflict type (e.g., "version", "template", "config")
    pub conflict_type: String,

    /// Resolution strategy (e.g., "highest", "first", "last", "merge")
    pub strategy: String,

    /// Additional parameters
    #[serde(default)]
    pub params: HashMap<String, String>,
}

/// Dependency graph for installation ordering
#[derive(Debug, Clone)]
pub struct DependencyGraph {
    pub nodes: HashMap<PackId, DependencyNode>,
    pub edges: Vec<(PackId, PackId)>, // (from, to)
}

#[derive(Debug, Clone)]
pub struct DependencyNode {
    pub pack: Pack,
    pub resolved_version: SemanticVersion,
    pub dependencies: Vec<PackId>,
}

impl DependencyGraph {
    /// Build graph from pack
    pub async fn build(
        pack: &Pack,
        registry: &dyn PackRegistry,
    ) -> Result<Self> {
        let mut graph = Self {
            nodes: HashMap::new(),
            edges: Vec::new(),
        };

        graph.build_recursive(pack, registry).await?;
        Ok(graph)
    }

    /// Validate no circular dependencies
    pub fn validate_acyclic(&self) -> Result<(), ValidationError> {
        // DFS with recursion stack
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
    ) -> Result<(), ValidationError> {
        visited.insert(pack_id.clone());
        rec_stack.insert(pack_id.clone());

        if let Some(node) = self.nodes.get(pack_id) {
            for dep_id in &node.dependencies {
                if !visited.contains(dep_id) {
                    self.dfs_detect_cycle(dep_id, visited, rec_stack)?;
                } else if rec_stack.contains(dep_id) {
                    return Err(ValidationError::CircularDependency {
                        from: pack_id.clone(),
                        to: dep_id.clone(),
                    });
                }
            }
        }

        rec_stack.remove(pack_id);
        Ok(())
    }

    /// Topological sort for installation order
    pub fn topological_sort(&self) -> Result<Vec<PackId>> {
        // Kahn's algorithm
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

        // Queue nodes with no incoming edges
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
            return Err(ValidationError::CircularDependency {
                from: PackId("unknown".to_string()),
                to: PackId("unknown".to_string()),
            }
            .into());
        }

        Ok(result)
    }
}
```

---

## Error Types

```rust
/// Validation errors
#[derive(Debug, thiserror::Error)]
pub enum ValidationError {
    #[error("Invalid pack ID format")]
    InvalidPackId,

    #[error("Pack must contain at least one package or template")]
    EmptyPack,

    #[error("Empty template path")]
    EmptyTemplatePath,

    #[error("Empty output path")]
    EmptyOutputPath,

    #[error("Invalid variable name: {0}")]
    InvalidVariableName(String),

    #[error("Invalid pattern: {0}")]
    InvalidPattern(String),

    #[error("Circular dependency detected: {from:?} -> {to:?}")]
    CircularDependency { from: PackId, to: PackId },

    #[error("Value does not match pattern: variable={variable}, value={value}, pattern={pattern}")]
    ValueDoesNotMatchPattern {
        variable: String,
        value: String,
        pattern: String,
    },

    #[error("Invalid type for variable {variable}: expected {expected}")]
    InvalidType {
        variable: String,
        expected: &'static str,
    },
}

/// Template rendering errors
#[derive(Debug, thiserror::Error)]
pub enum TemplateError {
    #[error("Missing required variable: template={template}, variable={variable}")]
    MissingRequiredVariable { template: String, variable: String },

    #[error("Template rendering failed: {0}")]
    RenderingFailed(String),

    #[error("Invalid template syntax: {0}")]
    InvalidSyntax(String),
}
```

---

**Related Documents:**
- [Architecture Overview](01_ARCHITECTURE_OVERVIEW.md)
- [Command Specification](02_COMMAND_SPECIFICATION.md)
- [Integration Layer](04_INTEGRATION_LAYER.md)
