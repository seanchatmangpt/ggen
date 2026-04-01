//! μ₀: Pack Resolution Stage
//!
//! This module implements the μ₀ stage of the generation pipeline, which resolves
//! packs from the lockfile, expands bundles to atomic packs, checks compatibility,
//! merges pack ontologies, and builds ownership maps for conflict detection.
//!
//! ## μ₀ Stage Responsibilities
//!
//! 1. **Read lockfile** - Load requested packs/bundles from `.ggen/packs.lock`
//! 2. **Expand bundles** - Convert bundle aliases to atomic pack IDs
//! 3. **Resolve dependencies** - Transitively resolve all pack dependencies
//! 4. **Check compatibility** - Multi-dimensional conflict detection
//! 5. **Merge ontologies** - Combine RDF graphs from all packs
//! 6. **Build ownership map** - Track artifact ownership for conflict detection
//!
//! ## Output
//!
//! The μ₀ stage produces `ResolvedPacks`, which contains:
//! - `atomic_packs`: The complete set of atomic pack IDs
//! - `merged_ontology`: Combined RDF graph from all packs
//! - `ownership_map`: Ownership declarations for conflict detection
//! - `queries` / `templates`: Contributed SPARQL (CONSTRUCT) and Tera sources for μ₂ / staging
//!
//! ## Integration
//!
//! This stage runs **before** μ₁ (normalization) in the pipeline:
//!
//! ```text
//! μ₀ (pack resolution) → μ₁ (normalization) → μ₂ (extraction) → μ₃ (emission) → μ₄ (canonicalization) → μ₅ (receipt)
//! ```

use crate::graph::Graph;
use crate::packs::lockfile::PackLockfile;
use ggen_marketplace::atomic::{foundation_packs, AtomicPackId};
use ggen_marketplace::bundle::Bundle;
use ggen_marketplace::metadata::load_pack_metadata;
use ggen_marketplace::ownership::{
    MergeStrategy, OwnershipClass, OwnershipDeclaration, OwnershipMap, OwnershipTarget,
};
use ggen_marketplace::policy::{PackContext, PolicyEnforcer};
use ggen_marketplace::profile::get_profile;
use ggen_utils::{
    bail,
    error::{Error, Result},
};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tracing;

/// SPARQL query contributed by a pack.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SparqlQuery {
    /// Query name (for binding lookup)
    pub name: String,
    /// SPARQL query string
    pub sparql: String,
}

/// Tera template contributed by a pack.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateDef {
    /// Relative path to template file
    pub path: PathBuf,
    /// Template content
    pub content: String,
}

/// Resolution result from μ₀ stage.
///
/// Contains the complete set of resolved atomic packs, their merged ontology,
/// and ownership information for conflict detection.
#[derive(Clone)]
pub struct ResolvedPacks {
    /// Complete set of atomic pack IDs (after bundle expansion and dependency resolution)
    pub atomic_packs: Vec<AtomicPackId>,

    /// Merged RDF ontology from all packs
    pub merged_ontology: Graph,

    /// Ownership declarations for conflict detection
    pub ownership_map: OwnershipMap,

    /// Bundle expansions performed (for provenance)
    pub bundle_expansions: Vec<BundleExpansion>,

    /// Pack versions resolved (for lockfile updates)
    pub pack_versions: HashMap<String, String>,

    /// Policy profile used for enforcement
    pub profile: String,

    /// SPARQL queries loaded from packs (μ₂; CONSTRUCT-only — see `PACK_QUERY_CONTRACT.md`)
    pub queries: Vec<SparqlQuery>,

    /// Tera templates loaded from packs (staged under `.ggen/pack-stage/` during sync)
    pub templates: Vec<TemplateDef>,

    /// Ed25519 signatures per pack (hex strings), loaded from pack metadata.
    pub pack_signatures: HashMap<String, String>,

    /// SHA-256 checksums per pack, loaded from pack metadata.
    pub pack_checksums: HashMap<String, String>,
}

/// Record of a bundle expansion (for provenance in receipts).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BundleExpansion {
    /// Bundle ID that was expanded
    pub bundle_id: String,

    /// Atomic packs it expanded to
    pub expanded_to: Vec<String>,
}

impl ResolvedPacks {
    /// Deterministic digest of pack queries + templates for one atomic pack (receipt provenance).
    pub fn digest_for_pack(&self, pack: &AtomicPackId) -> String {
        use sha2::{Digest, Sha256};
        let pid = pack.to_string();
        let prefix_q = format!("{}::", pid);
        let mut h = Sha256::new();
        for q in &self.queries {
            if q.name.starts_with(&prefix_q) {
                h.update(q.name.as_bytes());
                h.update([0_u8]);
                h.update(q.sparql.as_bytes());
            }
        }
        let path_prefix = format!("{}/", pid);
        for t in &self.templates {
            let ps = t.path.display().to_string();
            if ps.starts_with(&path_prefix) {
                h.update(ps.as_bytes());
                h.update([0_u8]);
                h.update(t.content.as_bytes());
            }
        }
        format!("sha256:{:x}", h.finalize())
    }

    /// Query names contributed by `pack` (`<pack>::<stem>`).
    pub fn query_names_for_pack(&self, pack: &AtomicPackId) -> Vec<String> {
        let p = format!("{}::", pack);
        self.queries
            .iter()
            .filter(|q| q.name.starts_with(&p))
            .map(|q| q.name.clone())
            .collect()
    }

    /// Template relative paths for `pack` under the pack cache layout.
    pub fn template_paths_for_pack(&self, pack: &AtomicPackId) -> Vec<String> {
        let prefix = format!("{}/", pack);
        self.templates
            .iter()
            .filter(|t| t.path.display().to_string().starts_with(&prefix))
            .map(|t| t.path.display().to_string())
            .collect()
    }
}

/// μ₀ Pack Resolver
///
/// Resolves packs from lockfile, expands bundles, checks compatibility,
/// merges ontologies, and builds ownership maps.
pub struct PackResolver {
    /// Lockfile path (.ggen/packs.lock)
    #[allow(dead_code)]
    lockfile_path: PathBuf,

    /// Registry for loading pack metadata
    registry: Arc<PackRegistry>,
}

impl PackResolver {
    /// Create a new pack resolver for the given project.
    ///
    /// # Arguments
    ///
    /// * `project_dir` - Path to project directory (contains `.ggen/`)
    ///
    /// # Errors
    ///
    /// Returns error if project directory doesn't exist.
    pub fn new(project_dir: &Path) -> Result<Self> {
        let project_dir = project_dir
            .canonicalize()
            .map_err(|e| Error::new(&format!("Invalid project directory: {}", e)))?;

        let lockfile_path = project_dir.join(".ggen").join("packs.lock");
        let cache_dir = std::env::var_os("GGEN_PACK_CACHE_DIR")
            .map(PathBuf::from)
            .or_else(|| dirs::home_dir().map(|h| h.join(".ggen").join("packs")))
            .ok_or_else(|| {
                Error::new("Cannot resolve pack cache directory: set HOME or GGEN_PACK_CACHE_DIR")
            })?;

        Ok(Self {
            lockfile_path,
            registry: Arc::new(PackRegistry::new(cache_dir)?),
        })
    }

    /// Resolve packs from lockfile (μ₀ stage).
    ///
    /// This is the main entry point for μ₀. It performs the complete
    /// pack resolution pipeline:
    ///
    /// 1. Read lockfile
    /// 2. Expand bundles to atomic packs
    /// 3. Resolve dependencies transitively
    /// 4. Check multi-dimensional compatibility
    /// 5. Enforce policy profiles
    /// 6. Merge pack ontologies
    /// 7. Build ownership map
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Lockfile not found or invalid
    /// - Bundle expansion fails
    /// - Dependency resolution fails
    /// - Compatibility check fails
    /// - Policy enforcement fails (error-level violations)
    /// - Ontology merge fails
    /// - Ownership conflict detected
    pub fn resolve(&self) -> Result<ResolvedPacks> {
        // Step 1: Read lockfile
        let lockfile = self.read_lockfile()?;

        // Step 2: Expand bundles to atomic packs
        let (atomic_packs, bundle_expansions) = self.expand_bundles(&lockfile)?;

        // Step 3: Resolve dependencies transitively
        let resolved_packs = self.resolve_dependencies(&atomic_packs)?;

        // Step 4: Check compatibility (multi-dimensional)
        self.check_compatibility(&resolved_packs)?;

        // Step 5: Enforce policy profiles
        let profile = self.enforce_policies(&resolved_packs, &lockfile)?;

        // Step 6: Merge pack ontologies
        let merged_ontology = self.merge_ontologies(&resolved_packs)?;

        // Step 7: Build ownership map
        let ownership_map = self.build_ownership_map(&resolved_packs)?;

        // Step 8: Load pack queries (μ₂) and templates (μ₃ staging)
        let mut all_queries = Vec::new();
        let mut all_templates = Vec::new();
        for pack in &resolved_packs {
            let pack_queries = self.registry.get_pack_queries(pack)?;
            all_queries.extend(pack_queries);
            let pack_templates = self.registry.get_pack_templates(pack)?;
            all_templates.extend(pack_templates);
        }

        // Extract pack versions for lockfile updates
        let pack_versions = self.extract_pack_versions(&lockfile);

        Ok(ResolvedPacks {
            atomic_packs: resolved_packs,
            merged_ontology,
            ownership_map,
            bundle_expansions,
            pack_versions,
            profile,
            queries: all_queries,
            templates: all_templates,
        })
    }

    /// Read the lockfile from `.ggen/packs.lock`.
    fn read_lockfile(&self) -> Result<PackLockfile> {
        if !self.lockfile_path.exists() {
            bail!(
                "Lockfile not found at {}. Run 'ggen packs install' first.",
                self.lockfile_path.display()
            );
        }

        PackLockfile::from_file(&self.lockfile_path)
    }

    /// Expand bundles to atomic packs.
    ///
    /// For each entry in the lockfile:
    /// - If it's a bundle, expand it to atomic packs
    /// - If it's an atomic pack, use it directly
    ///
    /// Returns the expanded atomic pack IDs and bundle expansion records.
    fn expand_bundles(
        &self, lockfile: &PackLockfile,
    ) -> Result<(Vec<AtomicPackId>, Vec<BundleExpansion>)> {
        let mut atomic_packs = Vec::new();
        let mut bundle_expansions = Vec::new();
        let mut seen = HashSet::new();

        for pack_id in lockfile.packs.keys() {
            // Check if this is a bundle or atomic pack
            if let Some(bundle) = self.registry.get_bundle(pack_id)? {
                // Expand bundle to atomic packs
                let expanded = bundle.expand();

                // Record expansion for provenance
                bundle_expansions.push(BundleExpansion {
                    bundle_id: pack_id.clone(),
                    expanded_to: expanded.iter().map(|p| p.to_string()).collect(),
                });

                // Add atomic packs (avoid duplicates)
                for pack in expanded {
                    if seen.insert(pack.clone()) {
                        atomic_packs.push(pack);
                    }
                }
            } else if let Some(atomic_pack) = self.parse_atomic_pack_id(pack_id)? {
                // Direct atomic pack
                if seen.insert(atomic_pack.clone()) {
                    atomic_packs.push(atomic_pack);
                }
            } else {
                bail!("Unknown pack or bundle: {}", pack_id);
            }
        }

        Ok((atomic_packs, bundle_expansions))
    }

    /// Resolve dependencies transitively.
    ///
    /// Loads dependency metadata for each pack and recursively resolves
    /// all transitive dependencies. Returns the complete set of atomic packs.
    fn resolve_dependencies(&self, atomic_packs: &[AtomicPackId]) -> Result<Vec<AtomicPackId>> {
        let mut resolved = HashSet::new();
        let mut to_resolve = atomic_packs.to_vec();

        while let Some(pack) = to_resolve.pop() {
            if resolved.contains(&pack) {
                continue;
            }

            // Add pack to resolved set
            resolved.insert(pack.clone());

            // Load pack dependencies
            let dependencies = self.registry.get_pack_dependencies(&pack)?;

            // Add dependencies to resolution queue
            for dep in dependencies {
                if !resolved.contains(&dep) {
                    to_resolve.push(dep);
                }
            }
        }

        // Convert to deterministic order (sort for reproducibility)
        let mut sorted: Vec<_> = resolved.into_iter().collect();
        sorted.sort_by_key(|p| p.to_string());

        Ok(sorted)
    }

    /// Check multi-dimensional compatibility.
    ///
    /// Performs compatibility checks across multiple dimensions:
    /// - Ontology namespace conflicts
    /// - Protocol field conflicts
    /// - Emitted file path conflicts
    /// - Runtime compatibility
    /// - Validator contradictions
    /// - Policy contradictions
    ///
    /// Returns error if any conflicts are detected.
    fn check_compatibility(&self, packs: &[AtomicPackId]) -> Result<()> {
        // For now, implement basic conflict detection
        // Full multi-dimensional checking will be added in Phase 5

        // Check for duplicate packs (shouldn't happen after dependency resolution)
        let mut seen = HashSet::new();
        for pack in packs {
            if !seen.insert(pack) {
                bail!("Duplicate pack detected: {}", pack);
            }
        }

        // Load and check ownership declarations
        let mut ownership_map = OwnershipMap::new();
        for pack in packs {
            let declarations = self.registry.get_ownership_declarations(pack)?;
            for decl in declarations {
                ownership_map.add(decl)?;
            }
        }

        // Check for conflicts
        let conflicts = ownership_map.check_conflicts();
        if !conflicts.is_empty() {
            let mut error_msg = "Pack compatibility check failed:\n".to_string();
            for conflict in conflicts {
                error_msg.push_str(&format!("  - {}\n", conflict));
            }
            bail!("{}", error_msg);
        }

        Ok(())
    }

    /// Enforce policy profiles against resolved packs.
    ///
    /// Loads the profile from the lockfile (or uses "development" as default),
    /// converts packs to policy context, and enforces all profile policies.
    ///
    /// Returns the profile ID that was enforced.
    ///
    /// # Errors
    ///
    /// Returns error if profile not found or policy enforcement fails.
    fn enforce_policies(&self, packs: &[AtomicPackId], lockfile: &PackLockfile) -> Result<String> {
        // Determine profile from lockfile or use default
        let profile_id = lockfile.profile.as_ref().cloned().unwrap_or_else(|| {
            // Default to "development" profile if not specified
            "development".to_string()
        });

        // Load profile
        let profile = get_profile(&profile_id)
            .map_err(|e| Error::new(&format!("Failed to load profile '{}': {}", profile_id, e)))?;

        // Convert atomic packs to policy context, loading real metadata from cache
        let pack_contexts: Vec<PackContext> = packs
            .iter()
            .map(|pack_id| {
                let pack_dir = self.registry.pack_dir(pack_id);
                let metadata = load_pack_metadata(&pack_dir).unwrap_or_default();

                let has_signature = metadata.signature.is_some();
                PackContext::new(pack_id.to_string())
                    .with_trust_tier(metadata.trust_tier)
                    .with_signed_receipts(has_signature)
                    .with_signature_verification(has_signature)
                    .with_allowlisted(has_signature)
                    .with_semver(true) // installed packs use semver versions
            })
            .collect();

        // Enforce policies
        let report = PolicyEnforcer::enforce(&profile.policy_overlays, &pack_contexts)
            .map_err(|e| Error::new(&format!("Policy enforcement failed: {}", e)))?;

        // Handle violations
        if !report.passed {
            // Check for error-level violations
            // For now, all violations are treated as errors
            let mut error_msg =
                format!("Policy enforcement failed for profile '{}':\n", profile_id);
            for violation in &report.violations {
                error_msg.push_str(&format!("  - {}\n", violation.description));
            }
            bail!("{}", error_msg);
        } else {
            // Policy passed - log success
            if report.violations.is_empty() {
                tracing::info!(
                    profile = %profile_id,
                    packs = packs.len(),
                    "All policies passed for profile '{}'",
                    profile_id
                );
            } else {
                tracing::debug!(
                    profile = %profile_id,
                    warnings = report.violations.len(),
                    "Policy check passed with warnings"
                );
            }
        }

        Ok(profile_id)
    }

    /// Merge RDF ontologies from all packs.
    ///
    /// Loads ontology files from each pack and merges them into a single graph.
    /// Foundation packs are loaded first to establish base ontology.
    fn merge_ontologies(&self, packs: &[AtomicPackId]) -> Result<Graph> {
        let merged = Graph::new()?;

        // Load foundation ontology first (CISO requirement)
        for foundation in foundation_packs() {
            if packs.contains(&foundation) {
                let ontology_path = self.registry.get_pack_ontology_path(&foundation)?;
                if ontology_path.exists() {
                    let content = std::fs::read_to_string(&ontology_path).map_err(|e| {
                        Error::new(&format!(
                            "Failed to read ontology for {}: {}",
                            foundation, e
                        ))
                    })?;
                    merged.insert_turtle(&content)?;
                }
            }
        }

        // Load pack ontologies
        for pack in packs {
            let ontology_path = self.registry.get_pack_ontology_path(pack)?;
            if ontology_path.exists() {
                let content = std::fs::read_to_string(&ontology_path).map_err(|e| {
                    Error::new(&format!("Failed to read ontology for {}: {}", pack, e))
                })?;
                merged.insert_turtle(&content)?;
            }
        }

        Ok(merged)
    }

    /// Build ownership map from all packs.
    ///
    /// Loads ownership declarations from each pack and builds a consolidated
    /// ownership map for conflict detection.
    fn build_ownership_map(&self, packs: &[AtomicPackId]) -> Result<OwnershipMap> {
        let mut ownership_map = OwnershipMap::new();

        for pack in packs {
            let declarations = self.registry.get_ownership_declarations(pack)?;
            for decl in declarations {
                ownership_map.add(decl)?;
            }
        }

        Ok(ownership_map)
    }

    /// Extract pack versions from lockfile.
    ///
    /// Returns a map of pack ID to version string.
    fn extract_pack_versions(&self, lockfile: &PackLockfile) -> HashMap<String, String> {
        lockfile
            .packs
            .iter()
            .map(|(id, pack)| (id.clone(), pack.version.clone()))
            .collect()
    }

    /// Parse a pack ID string to an AtomicPackId.
    ///
    /// Returns None if the string is not a valid atomic pack ID.
    fn parse_atomic_pack_id(&self, pack_id: &str) -> Result<Option<AtomicPackId>> {
        // Try to parse as atomic pack ID (e.g., "surface-mcp", "projection-rust")
        Ok(AtomicPackId::from_str(pack_id))
    }
}

/// Pack Registry
///
/// Provides access to pack metadata, ontologies, templates, and ownership
/// declarations from the local cache.
struct PackRegistry {
    /// Cache directory where packs are stored
    cache_dir: PathBuf,

    /// In-memory bundle index
    bundles: HashMap<String, Bundle>,
}

impl PackRegistry {
    /// Create a new pack registry.
    fn new(cache_dir: PathBuf) -> Result<Self> {
        let mut registry = Self {
            cache_dir,
            bundles: HashMap::new(),
        };

        // Initialize common bundles
        registry.initialize_bundles();

        Ok(registry)
    }

    /// Initialize common bundle definitions.
    fn initialize_bundles(&mut self) {
        use ggen_marketplace::bundle::Bundles;

        // Register common bundles
        self.bundles
            .insert("mcp-rust".to_string(), Bundles::mcp_rust());
        self.bundles
            .insert("mcp-rust-stdio".to_string(), Bundles::mcp_rust_stdio());
        self.bundles
            .insert("mcp-rust-axum".to_string(), Bundles::mcp_rust_axum());
        self.bundles
            .insert("a2a-rust".to_string(), Bundles::a2a_rust());
        self.bundles
            .insert("openapi-rust".to_string(), Bundles::openapi_rust());
        self.bundles.insert(
            "graphql-typescript".to_string(),
            Bundles::graphql_typescript(),
        );
    }

    /// Get a bundle by ID.
    fn get_bundle(&self, bundle_id: &str) -> Result<Option<&Bundle>> {
        Ok(self.bundles.get(bundle_id))
    }

    /// Get the cache directory for a specific pack.
    fn pack_dir(&self, pack: &AtomicPackId) -> PathBuf {
        self.cache_dir.join(pack.to_string())
    }

    /// Get the path to a pack's ontology file.
    fn get_pack_ontology_path(&self, pack: &AtomicPackId) -> Result<PathBuf> {
        let pack_dir = self.cache_dir.join(pack.to_string());
        Ok(pack_dir.join("ontology").join("pack.ttl"))
    }

    /// Get dependencies for a pack.
    ///
    /// Loads dependencies from the pack's `package.toml` file under a
    /// `[dependencies]` section, or from a `dependencies.json` file.
    /// Returns an empty vector if no dependency metadata exists.
    fn get_pack_dependencies(&self, pack: &AtomicPackId) -> Result<Vec<AtomicPackId>> {
        let pack_dir = self.pack_dir(pack);
        let pack_id_str = pack.to_string();

        // Try package.toml [dependencies] section first
        let toml_path = pack_dir.join("package.toml");
        if toml_path.exists() {
            if let Ok(deps) = Self::load_deps_from_toml(&toml_path) {
                if !deps.is_empty() {
                    tracing::debug!(
                        pack = %pack_id_str,
                        deps = deps.len(),
                        "Loaded {} dependencies from package.toml",
                        deps.len()
                    );
                    return Ok(deps);
                }
            }
        }

        // Fallback to dependencies.json
        let json_path = pack_dir.join("dependencies.json");
        if json_path.exists() {
            if let Ok(deps) = Self::load_deps_from_json(&json_path) {
                if !deps.is_empty() {
                    tracing::debug!(
                        pack = %pack_id_str,
                        deps = deps.len(),
                        "Loaded {} dependencies from dependencies.json",
                        deps.len()
                    );
                    return Ok(deps);
                }
            }
        }

        Ok(Vec::new())
    }

    /// Load dependency pack IDs from package.toml [dependencies] section.
    fn load_deps_from_toml(toml_path: &Path) -> Result<Vec<AtomicPackId>> {
        let content = std::fs::read_to_string(toml_path)
            .map_err(|e| Error::new(&format!("Failed to read {}: {}", toml_path.display(), e)))?;
        let value: toml::Value = toml::from_str(&content)
            .map_err(|e| Error::new(&format!("Failed to parse {}: {}", toml_path.display(), e)))?;

        let deps_array = value.get("dependencies").and_then(|v| v.as_array());

        let mut result = Vec::new();
        if let Some(array) = deps_array {
            for item in array {
                if let Some(dep_str) = item.as_str() {
                    if let Some(pack_id) = AtomicPackId::from_str(dep_str) {
                        result.push(pack_id);
                    }
                }
            }
        }

        Ok(result)
    }

    /// Load dependency pack IDs from dependencies.json file.
    fn load_deps_from_json(json_path: &Path) -> Result<Vec<AtomicPackId>> {
        let content = std::fs::read_to_string(json_path)
            .map_err(|e| Error::new(&format!("Failed to read {}: {}", json_path.display(), e)))?;
        let deps: Vec<String> = serde_json::from_str(&content)
            .map_err(|e| Error::new(&format!("Failed to parse {}: {}", json_path.display(), e)))?;

        let mut result = Vec::new();
        for dep_str in deps {
            if let Some(pack_id) = AtomicPackId::from_str(&dep_str) {
                result.push(pack_id);
            }
        }

        Ok(result)
    }

    /// Get ownership declarations for a pack.
    ///
    /// Loads ownership declarations from the pack's `ownership.json` file
    /// or from the `[ownership.declarations]` section in `package.toml`.
    /// Returns an empty vector if no ownership metadata exists.
    fn get_ownership_declarations(&self, pack: &AtomicPackId) -> Result<Vec<OwnershipDeclaration>> {
        let pack_dir = self.pack_dir(pack);
        let pack_id_str = pack.to_string();

        // Try ownership.json first (primary format)
        let ownership_json_path = pack_dir.join("ownership.json");
        if ownership_json_path.exists() {
            let declarations = Self::load_ownership_json(&ownership_json_path)?;
            if !declarations.is_empty() {
                tracing::debug!(
                    pack = %pack_id_str,
                    declarations = declarations.len(),
                    "Loaded {} ownership declarations from ownership.json",
                    declarations.len()
                );
                return Ok(declarations);
            }
        }

        // Fallback to package.toml [ownership.declarations] section
        let package_toml_path = pack_dir.join("package.toml");
        if package_toml_path.exists() {
            let declarations = Self::load_ownership_from_toml(&package_toml_path, &pack_id_str)?;
            if !declarations.is_empty() {
                tracing::debug!(
                    pack = %pack_id_str,
                    declarations = declarations.len(),
                    "Loaded {} ownership declarations from package.toml",
                    declarations.len()
                );
                return Ok(declarations);
            }
        }

        Ok(Vec::new())
    }

    /// Load ownership declarations from ownership.json file.
    fn load_ownership_json(path: &Path) -> Result<Vec<OwnershipDeclaration>> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| Error::new(&format!("Failed to read {}: {}", path.display(), e)))?;
        let declarations: Vec<OwnershipDeclaration> = serde_json::from_str(&content)
            .map_err(|e| Error::new(&format!("Failed to parse {}: {}", path.display(), e)))?;
        Ok(declarations)
    }

    /// Load ownership declarations from package.toml [ownership.declarations] section.
    fn load_ownership_from_toml(
        path: &Path, pack_id_str: &str,
    ) -> Result<Vec<OwnershipDeclaration>> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| Error::new(&format!("Failed to read {}: {}", path.display(), e)))?;
        let value: toml::Value = toml::from_str(&content)
            .map_err(|e| Error::new(&format!("Failed to parse {}: {}", path.display(), e)))?;

        let ownership_section = value.get("ownership").and_then(|v| v.as_table());
        let declarations_array = ownership_section
            .and_then(|t| t.get("declarations"))
            .and_then(|v| v.as_array());

        let mut result = Vec::new();
        if let Some(array) = declarations_array {
            for (idx, decl_value) in array.iter().enumerate() {
                let table = decl_value.as_table().ok_or_else(|| {
                    Error::new(&format!(
                        "ownership.declarations[{}] is not a table in {:?}",
                        idx, path
                    ))
                })?;

                let target = Self::parse_ownership_target(table, path, idx)?;
                let class = Self::parse_ownership_class(table, path, idx)?;
                let merge_strategy = Self::parse_merge_strategy(table);

                result.push(OwnershipDeclaration {
                    target,
                    class,
                    owner_pack: pack_id_str.to_string(),
                    merge_strategy,
                    metadata: None,
                });
            }
        }

        Ok(result)
    }

    /// Parse an ownership target from a TOML table.
    fn parse_ownership_target(
        table: &toml::Table, path: &Path, idx: usize,
    ) -> Result<OwnershipTarget> {
        let kind = table.get("kind").and_then(|v| v.as_str()).ok_or_else(|| {
            Error::new(&format!(
                "ownership.declarations[{}].kind is missing in {:?}",
                idx, path
            ))
        })?;

        let value = table.get("value").and_then(|v| v.as_str()).ok_or_else(|| {
            Error::new(&format!(
                "ownership.declarations[{}].value is missing in {:?}",
                idx, path
            ))
        })?;

        let target = match kind {
            "file_path" => OwnershipTarget::FilePath(PathBuf::from(value)),
            "rdf_namespace" => OwnershipTarget::RdfNamespace(value.to_string()),
            "protocol_field" => OwnershipTarget::ProtocolField(value.to_string()),
            "template_variable" => OwnershipTarget::TemplateVariable(value.to_string()),
            "dependency_package" => OwnershipTarget::DependencyPackage(value.to_string()),
            "feature_flag" => OwnershipTarget::FeatureFlag(value.to_string()),
            other => {
                bail!(
                    "Unknown ownership target kind '{}' at index {} in {:?}",
                    other,
                    idx,
                    path
                );
            }
        };

        Ok(target)
    }

    /// Parse an ownership class from a TOML table.
    fn parse_ownership_class(
        table: &toml::Table, path: &Path, idx: usize,
    ) -> Result<OwnershipClass> {
        let class_str = table.get("class").and_then(|v| v.as_str()).ok_or_else(|| {
            Error::new(&format!(
                "ownership.declarations[{}].class is missing in {:?}",
                idx, path
            ))
        })?;

        match class_str {
            "exclusive" => Ok(OwnershipClass::Exclusive),
            "mergeable" => Ok(OwnershipClass::Mergeable),
            "overlay" => Ok(OwnershipClass::Overlay),
            "forbidden_overlap" => Ok(OwnershipClass::ForbiddenOverlap),
            other => {
                bail!(
                    "Unknown ownership class '{}' at index {} in {:?}",
                    other,
                    idx,
                    path
                );
            }
        }
    }

    /// Parse an optional merge strategy from a TOML table.
    fn parse_merge_strategy(table: &toml::Table) -> Option<MergeStrategy> {
        let strategy_str = table.get("merge_strategy").and_then(|v| v.as_str())?;
        match strategy_str {
            "concat" => Some(MergeStrategy::Concat),
            "last_writer_wins" => Some(MergeStrategy::LastWriterWins),
            "first_writer_wins" => Some(MergeStrategy::FirstWriterWins),
            "recursive" => Some(MergeStrategy::Recursive),
            "fail_on_conflict" => Some(MergeStrategy::FailOnConflict),
            _ => None,
        }
    }

    /// Get SPARQL queries for a pack.
    ///
    /// Scans the pack's queries directory for .rq files and returns a list of SparqlQuery objects.
    /// This enables packs to contribute custom extraction logic to the μ₂ stage.
    fn get_pack_queries(&self, pack: &AtomicPackId) -> Result<Vec<SparqlQuery>> {
        let queries_dir = self.cache_dir.join(pack.to_string()).join("queries");

        if !queries_dir.exists() {
            // No queries directory is valid - return empty list
            return Ok(Vec::new());
        }

        let mut queries = Vec::new();

        let entries = std::fs::read_dir(&queries_dir).map_err(|e| {
            ggen_utils::error::Error::new(&format!(
                "Failed to read queries directory {}: {}",
                queries_dir.display(),
                e
            ))
        })?;

        for entry in entries {
            let entry = entry.map_err(|e| {
                ggen_utils::error::Error::new(&format!("Failed to read directory entry: {}", e))
            })?;
            let path = entry.path();

            // Only process .rq files
            if path.extension().and_then(|s| s.to_str()) != Some("rq") {
                continue;
            }

            let content = std::fs::read_to_string(&path).map_err(|e| {
                ggen_utils::error::Error::new(&format!(
                    "Failed to read query file {}: {}",
                    path.display(),
                    e
                ))
            })?;

            let stem = path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("unknown")
                .to_string();
            let name = format!("{}::{}", pack, stem);

            queries.push(SparqlQuery {
                name,
                sparql: content,
            });
        }

        Ok(queries)
    }

    /// Get Tera templates for a pack.
    ///
    /// Scans the pack's templates directory for .tera files and returns a list of TemplateDef objects.
    /// This enables packs to contribute code generation templates to the μ₃ stage.
    fn get_pack_templates(&self, pack: &AtomicPackId) -> Result<Vec<TemplateDef>> {
        let templates_dir = self.cache_dir.join(pack.to_string()).join("templates");

        if !templates_dir.exists() {
            // No templates directory is valid - return empty list
            return Ok(Vec::new());
        }

        let mut templates = Vec::new();

        let entries = std::fs::read_dir(&templates_dir).map_err(|e| {
            ggen_utils::error::Error::new(&format!(
                "Failed to read templates directory {}: {}",
                templates_dir.display(),
                e
            ))
        })?;

        for entry in entries {
            let entry = entry.map_err(|e| {
                ggen_utils::error::Error::new(&format!("Failed to read directory entry: {}", e))
            })?;
            let path = entry.path();

            // Only process .tera files
            if path.extension().and_then(|s| s.to_str()) != Some("tera") {
                continue;
            }

            let content = std::fs::read_to_string(&path).map_err(|e| {
                ggen_utils::error::Error::new(&format!(
                    "Failed to read template file {}: {}",
                    path.display(),
                    e
                ))
            })?;

            // Get relative path from templates_dir
            let relative_path = path.strip_prefix(&self.cache_dir).map_err(|e| {
                ggen_utils::error::Error::new(&format!("Failed to create relative path: {}", e))
            })?;

            templates.push(TemplateDef {
                path: relative_path.to_path_buf(),
                content,
            });
        }

        Ok(templates)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_pack_resolver_new() {
        let temp_dir = TempDir::new().unwrap();
        let resolver = PackResolver::new(temp_dir.path());
        assert!(resolver.is_ok());
    }

    #[test]
    fn test_bundle_expansion() {
        let temp_dir = TempDir::new().unwrap();
        let _resolver = PackResolver::new(temp_dir.path()).unwrap();

        // Create a mock lockfile with mcp-rust bundle
        let _lockfile = PackLockfile::new("6.0.0");
        // TODO: Add test when lockfile structure is finalized
    }

    #[test]
    fn test_atomic_pack_parsing() {
        let temp_dir = TempDir::new().unwrap();
        let resolver = PackResolver::new(temp_dir.path()).unwrap();

        // Valid atomic pack IDs
        assert!(resolver
            .parse_atomic_pack_id("surface-mcp")
            .unwrap()
            .is_some());
        assert!(resolver
            .parse_atomic_pack_id("projection-rust")
            .unwrap()
            .is_some());
        assert!(resolver
            .parse_atomic_pack_id("runtime-axum")
            .unwrap()
            .is_some());

        // Bundle IDs should return None (they're expanded separately)
        assert!(resolver.parse_atomic_pack_id("mcp-rust").unwrap().is_none());

        // Invalid pack ID
        assert!(resolver
            .parse_atomic_pack_id("invalid-pack")
            .unwrap()
            .is_none());
    }

    #[test]
    fn test_foundation_packs() {
        let foundations = foundation_packs();
        assert!(!foundations.is_empty());
        assert!(foundations.iter().all(|p| p.class.is_foundation()));
    }
}
