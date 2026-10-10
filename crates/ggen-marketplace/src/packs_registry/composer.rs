//! Multi-pack composition for complex projects
//!
//! This module provides functionality to compose multiple packs into a single
//! cohesive project, with conflict resolution and template merging.
//!
//! It carries two layers:
//!
//! 1. The legacy async `PackComposer` (repository-backed merge/layer flow).
//! 2. The deterministic capability composition kernel (`compose`,
//!    `PackCompositionPlan`, `CompositionRefusal`): a pure function over a
//!    pack set with typed refusals for unsound compositions.

use crate::marketplace::error::{Error, Result};
use crate::packs_registry::dependency_graph::DependencyGraph;
use crate::packs_registry::repository::{FileSystemRepository, PackRepository};
use crate::packs_registry::types::{CompositionStrategy, Pack, PackFile};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::path::PathBuf;
use std::time::Instant;
use tracing::{info, warn};

/// Capability surface of one pack, derived from the declared data the
/// marketplace `PackFile` model carries:
///
/// - provides universe = `packages` entries ∪ `capabilities.provides` URNs
///   (a package name or capability URN is the unit two packs can ambiguously
///   both claim; `DuplicateCapability` covers both channels).
/// - requires universe = non-optional `dependencies` pack ids ∪
///   `capabilities.requires` URNs. A requirement is bound when some pack in
///   the set carries the id OR provides the capability.
/// - `templates[].path` entries are the declared artifact output paths.
/// - `dependencies` edges are the only inter-pack ordering data; the cycle
///   check runs over them.
///
/// Packs with `capabilities: None` produce exactly the pre-annotation surface
/// (zero drift). Cross-corpus composition (the same logical pack mirrored in
/// two corpus directories) merges silently: same-named mirror packs dedupe by
/// pack id before the provider set is built, so duplicate self-URNs never
/// reach `DuplicateCapability` (composition collapses 403 -> 332 providers —
/// observed in `cross_corpus_tripwire_test.rs`). `DuplicateCapability` fires only
/// for differing-id packs claiming the same URN. This silent merge-collapse is
/// the intended mirror handling per the falsified-dedup decision
/// (`docs/pack_urn_namespace_proposal.md`, FALSIFIED section).
struct PackCapabilitySurface<'a> {
    pack_id: &'a str,
    provides: Vec<&'a str>,
    requires: Vec<&'a str>,
    artifact_paths: Vec<(&'a str, &'a str)>,
}

impl<'a> PackCapabilitySurface<'a> {
    fn from_pack(pack: &'a PackFile) -> Self {
        let p = &pack.pack;
        // Provides universe = declared `packages` ∪ capabilities.provides URNs.
        // Requires universe = non-optional dependency pack ids ∪
        // capabilities.requires URNs. Packs with `capabilities: None` yield
        // exactly the pre-annotation surface (zero drift).
        let mut provides: Vec<&str> = p.packages.iter().map(String::as_str).collect();
        let mut requires: Vec<&str> = p
            .dependencies
            .iter()
            .filter(|d| !d.optional)
            .map(|d| d.pack_id.as_str())
            .collect();
        if let Some(caps) = &pack.capabilities {
            if let Some(urns) = &caps.provides {
                provides.extend(urns.iter().map(String::as_str));
            }
            if let Some(urns) = &caps.requires {
                requires.extend(urns.iter().map(String::as_str));
            }
        }
        Self {
            pack_id: &p.id,
            provides,
            requires,
            artifact_paths: p
                .templates
                .iter()
                .map(|t| (p.id.as_str(), t.path.as_str()))
                .collect(),
        }
    }
}

/// Deterministic composition refusal taxonomy.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum CompositionRefusal {
    /// Two packs claim the same provided capability (package name).
    #[error(
        "duplicate capability '{capability}': packs [{}] both provide it; \
         composition is ambiguous without an explicit precedence rule",
        providers.join(", ")
    )]
    DuplicateCapability {
        capability: String,
        providers: Vec<String>,
    },
    /// A required provider pack is absent from the composed set.
    #[error(
        "unbound requirement: pack '{requiring_pack}' requires provider pack \
         '{required_pack}', which is not in the composed set"
    )]
    UnboundRequirement {
        requiring_pack: String,
        required_pack: String,
    },
    /// Two packs write the same artifact output path.
    #[error(
        "refused: duplicate artifact path '{path}': packs [{}] both declare it as a \
         template output target; later writes would clobber earlier ones",
        packs.join(", ")
    )]
    DuplicateArtifactPath { path: String, packs: Vec<String> },
    /// The inter-pack dependency graph is cyclic.
    #[error(
        "cyclic inter-pack dependency detected: {cycle}. Composition has no valid \
         install order; remove at least one dependency edge"
    )]
    CyclicDependencies { cycle: String },
}

/// Deterministic multi-pack composition plan.
///
/// Content is a pure function of the input pack set: BTreeMap/BTreeSet
/// throughout, topological order with `BTreeSet` tie-breaking.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackCompositionPlan {
    /// All pack IDs in the composed set.
    pub pack_ids: BTreeSet<String>,
    /// Provided capability (package name) -> set of provider pack IDs.
    pub provides: BTreeMap<String, BTreeSet<String>>,
    /// Topological order, dependencies before dependents.
    pub order: Vec<String>,
    /// Declared artifact output paths per pack ID.
    pub artifact_paths: BTreeMap<String, BTreeSet<String>>,
    /// Pack IDs whose `requires` were satisfied by their own `provides`
    /// (self-satisfaction through the provides union). Legitimate per union
    /// semantics, surfaced so consumers can audit it. Sorted.
    pub self_satisfied: Vec<String>,
}

///
/// # Errors
///
/// Returns an error if the operation cannot be completed.
/// Compose a set of packs into a deterministic plan, refusing unsound
/// compositions with typed refusals.
///
/// Refusal checks run in fixed order: `DuplicateCapability`, `UnboundRequirement`,
/// `DuplicateArtifactPath`, `CyclicDependencies`.
pub fn compose(packs: &[PackFile]) -> std::result::Result<PackCompositionPlan, CompositionRefusal> {
    let surfaces: Vec<PackCapabilitySurface> =
        packs.iter().map(PackCapabilitySurface::from_pack).collect();

    // 1. DuplicateCapability: >1 provider for the same package name.
    let mut provides: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    for s in &surfaces {
        for cap in &s.provides {
            provides
                .entry((*cap).to_string())
                .or_default()
                .insert(s.pack_id.to_string());
        }
    }
    for (cap, providers) in &provides {
        if providers.len() > 1 {
            return Err(CompositionRefusal::DuplicateCapability {
                capability: cap.clone(),
                providers: providers.iter().cloned().collect(),
            });
        }
    }

    // 2. UnboundRequirement: a requirement (non-optional dependency pack id or
    //    capabilities.requires URN) is bound only if some pack in the set
    //    carries that id OR provides that capability (package name or
    //    capabilities.provides URN).
    let known: BTreeSet<&str> = surfaces.iter().map(|s| s.pack_id).collect();
    let provided: BTreeSet<&str> = provides.keys().map(String::as_str).collect();
    let mut self_satisfied: BTreeSet<&str> = BTreeSet::new();
    for s in &surfaces {
        for req in &s.requires {
            if !known.contains(req) && !provided.contains(req) {
                return Err(CompositionRefusal::UnboundRequirement {
                    requiring_pack: s.pack_id.to_string(),
                    required_pack: (*req).to_string(),
                });
            }
            // Self-satisfaction: the requirement binds through this same
            // pack's own provides (union semantics — legitimate, audited).
            if s.provides.contains(req) {
                self_satisfied.insert(s.pack_id);
            }
        }
    }

    // 3. DuplicateArtifactPath: same template output path from two packs.
    let mut path_owners: BTreeMap<&str, BTreeSet<&str>> = BTreeMap::new();
    for s in &surfaces {
        for (owner, path) in &s.artifact_paths {
            path_owners.entry(path).or_default().insert(owner);
        }
    }
    for (path, owners) in &path_owners {
        if owners.len() > 1 {
            return Err(CompositionRefusal::DuplicateArtifactPath {
                path: (*path).to_string(),
                packs: owners.iter().map(|s| (*s).to_string()).collect(),
            });
        }
    }

    // 4. CyclicDependencies over the declared dependency edges (the only
    //    inter-pack dependency data the model carries).
    let pack_views: Vec<Pack> = packs.iter().map(|pf| pf.pack.clone()).collect();
    let graph = DependencyGraph::from_packs(&pack_views).map_err(|e| {
        CompositionRefusal::CyclicDependencies {
            cycle: match e {
                Error::Other(msg) => msg,
                other => other.to_string(),
            },
        }
    })?;
    // Cycle REFUSAL stays dependencies-only.
    graph
        .detect_cycles()
        .map_err(|e| CompositionRefusal::CyclicDependencies {
            cycle: match e {
                Error::Other(msg) => msg,
                other => other.to_string(),
            },
        })?;

    // 5. Capability-derived ordering edges (experiment, lane composer-cap-order):
    //    a capabilities.requires URN that is provided by another pack in the set
    //    creates an ordering edge provider -> requirer (requirer installs after
    //    provider). Satisfaction/refusal semantics stay union-based — these
    //    edges NEVER refuse; an edge that would close a cycle is dropped
    //    deterministically (cycle refusal remains dependencies-only).
    let cap_edges = capability_order_edges(&surfaces, &provides);
    let order = order_with_capability_edges(&graph, &pack_views, &cap_edges);

    Ok(PackCompositionPlan {
        pack_ids: pack_views.iter().map(|p| p.id.clone()).collect(),
        provides,
        order,
        artifact_paths: path_owners
            .into_iter()
            .map(|(path, owners)| {
                (
                    path.to_string(),
                    owners.into_iter().map(String::from).collect(),
                )
            })
            .collect(),
        self_satisfied: self_satisfied.into_iter().map(String::from).collect(),
    })
}

/// Capability-derived ordering edges: (`provider_pack_id`, `requirer_pack_id`)
/// pairs. A capabilities.requires URN bound by exactly one other pack's
/// provides (union channel — packages or capabilities.provides) yields
/// requirer-after-provider. Self-edges (a pack bound by its own provides)
/// are skipped: union self-satisfaction is audited in `self_satisfied`, not
/// ordered.
fn capability_order_edges(
    surfaces: &[PackCapabilitySurface<'_>], provides: &BTreeMap<String, BTreeSet<String>>,
) -> Vec<(String, String)> {
    let mut edges = BTreeSet::new();
    for s in surfaces {
        for req in &s.requires {
            if let Some(providers) = provides.get(*req) {
                for provider in providers {
                    if provider != s.pack_id {
                        edges.insert((provider.clone(), s.pack_id.to_string()));
                    }
                }
            }
        }
    }
    edges.into_iter().collect()
}

/// Re-order the plan over dependencies ∪ kept capability edges with
/// deterministic Kahn (`BTreeSet` tie-break by pack id). Capability edges that
/// would close a cycle are dropped BEFORE the sort: an edge (provider ->
/// requirer) is dropped iff the requirer already precedes the provider in the
/// before-graph built from dependency edges plus the OTHER candidate
/// capability edges. Cycle REFUSAL stays dependencies-only (checked above via
/// `DependencyGraph::detect_cycles`); capability edges never refuse, they only
/// order or get dropped.
///
/// Defensive fallback: the kept set is acyclic by construction, so Kahn
/// cannot stall; if it ever did, the dependencies-only order is returned
/// unchanged (documented degraded mode, never a partial order).
fn order_with_capability_edges(
    graph: &DependencyGraph, packs: &[Pack], cap_edges: &[(String, String)],
) -> Vec<String> {
    fn add_edge(
        succ: &mut BTreeMap<String, BTreeSet<String>>, indeg: &mut BTreeMap<String, usize>,
        from: &str, to: &str,
    ) {
        if succ
            .entry(from.to_string())
            .or_default()
            .insert(to.to_string())
        {
            *indeg.entry(to.to_string()).or_insert(0) += 1;
        }
    }

    // All pack ids in the composed set.
    let ids: BTreeSet<String> = packs.iter().map(|p| p.id.clone()).collect();

    // Build the before-graph (x -> y means "x comes before y") over dependency
    // edges plus ALL candidate capability edges, as a successor map. Each
    // candidate capability edge is then re-checked against the graph with
    // itself removed: it is dropped iff its requirer already precedes its
    // provider (the edge would close a cycle). Per-edge, order-independent,
    // deterministic; a 2-cycle drops BOTH edges. The kept set is acyclic: any
    // cycle among kept edges would let each of its edges reach around via the
    // others and be dropped.
    fn build_succ(
        graph: &DependencyGraph, ids: &BTreeSet<String>, edges: &[(String, String)],
    ) -> BTreeMap<String, BTreeSet<String>> {
        let mut succ: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
        for id in ids {
            succ.entry(id.clone()).or_default();
        }
        for id in ids {
            for dep in graph.dependencies(id) {
                succ.entry(dep.clone()).or_default().insert(id.clone());
            }
        }
        for (p, r) in edges {
            succ.entry(p.clone()).or_default().insert(r.clone());
        }
        succ
    }

    fn reaches(from: &str, to: &str, succ: &BTreeMap<String, BTreeSet<String>>) -> bool {
        let mut seen = BTreeSet::new();
        let mut stack: Vec<String> = vec![from.to_string()];
        while let Some(next) = stack.pop() {
            if next == to {
                return true;
            }
            if seen.insert(next.clone()) {
                if let Some(nexts) = succ.get(&next) {
                    stack.extend(nexts.iter().cloned());
                }
            }
        }
        false
    }

    let kept: Vec<(String, String)> = cap_edges
        .iter()
        .filter(|(provider, requirer)| {
            let without: Vec<(String, String)> = cap_edges
                .iter()
                .filter(|(p, r)| !(p == provider && r == requirer))
                .cloned()
                .collect();
            let succ = build_succ(graph, &ids, &without);
            !reaches(requirer, provider, &succ)
        })
        .cloned()
        .collect();

    // Successor map: dependency edge "pack depends on dep" means dep first,
    // i.e. dep -> pack; capability edge is already provider -> requirer.
    let mut succ: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut indeg: BTreeMap<String, usize> = BTreeMap::new();
    for id in &ids {
        succ.entry(id.clone()).or_default();
        indeg.entry(id.clone()).or_insert(0);
    }
    for id in &ids {
        for dep in graph.dependencies(id) {
            add_edge(&mut succ, &mut indeg, &dep, id);
        }
    }
    for (provider, requirer) in &kept {
        add_edge(&mut succ, &mut indeg, provider, requirer);
    }

    // Kahn with deterministic BTreeSet tie-break by pack id.
    let mut ready: BTreeSet<String> = indeg
        .iter()
        .filter(|(_, &d)| d == 0)
        .map(|(id, _)| id.clone())
        .collect();
    let mut order = Vec::with_capacity(ids.len());
    while let Some(id) = ready.pop_first() {
        order.push(id.clone());
        if let Some(neighbors) = succ.get(&id) {
            for n in neighbors.clone() {
                if let Some(d) = indeg.get_mut(&n) {
                    *d -= 1;
                    if *d == 0 {
                        ready.insert(n);
                    }
                }
            }
        }
    }

    if order.len() == ids.len() {
        order
    } else {
        // Unreachable given the acyclic kept set; degrade to the
        // dependencies-only order rather than emit a partial plan.
        DependencyGraph::from_packs(packs)
            .and_then(|g| g.topological_sort())
            .unwrap_or(order)
    }
}

/// Pack composer for multi-pack projects
pub struct PackComposer {
    repository: Box<dyn PackRepository>,
}

impl PackComposer {
    /// Create new composer with custom repository
    pub fn new(repository: Box<dyn PackRepository>) -> Self {
        Self { repository }
    }

    ///
    /// # Errors
    ///
    /// Returns an error if the operation cannot be completed.
    /// Create composer with default filesystem repository
    pub fn with_default_repo() -> Result<Self> {
        let repo = FileSystemRepository::discover()?;
        Ok(Self::new(Box::new(repo)))
    }

    ///
    /// # Errors
    ///
    /// Returns an error if the operation cannot be completed.
    /// Compose multiple packs into a single project
    pub async fn compose(
        &self, pack_ids: &[String], project_name: &str, options: &CompositionOptions,
    ) -> Result<CompositionResult> {
        let start = Instant::now();

        if pack_ids.is_empty() {
            return Err(Error::Other(
                "At least one pack ID must be specified for composition".to_string(),
            ));
        }

        info!("Starting composition of {} packs", pack_ids.len());

        // Load all packs
        let mut packs = Vec::new();
        for pack_id in pack_ids {
            let pack = self.repository.load(pack_id).await?;
            packs.push(pack);
        }

        // Deterministic capability admission before any merge work.
        let pack_files: Vec<PackFile> = packs
            .iter()
            .map(|p| PackFile {
                pack: p.clone(),
                capabilities: None,
            })
            .collect();
        compose(&pack_files).map_err(|e| Error::Other(e.to_string()))?;

        // Build dependency graph
        let graph = DependencyGraph::from_packs(&packs)?;
        let composition_order = graph.topological_sort()?;

        info!("Composition order: {:?}", composition_order);

        // Detect conflicts (advisory, post-admission)
        let conflicts = self.detect_composition_conflicts(&packs);
        if !conflicts.is_empty() {
            warn!(
                "Detected {} conflict(s) during composition",
                conflicts.len()
            );
            for conflict in &conflicts {
                warn!("  - {}", conflict);
            }

            if !options.force_composition {
                return Err(Error::Other(format!(
                    "Composition conflicts detected:\n{}",
                    conflicts.join("\n")
                )));
            }
        }

        // Compose packs according to strategy
        let composed_pack = match options.strategy {
            CompositionStrategy::Merge => self.merge_packs(&packs, &composition_order)?,
            CompositionStrategy::Layer => self.layer_packs(&packs, &composition_order)?,
            CompositionStrategy::Custom(ref rules) => {
                self.custom_composition(&packs, &composition_order, rules)?
            }
        };

        // Generate composition plan
        let plan = self.generate_composition_plan(&packs, &composed_pack, &composition_order);

        // Determine output path
        let output_path = options
            .output_dir
            .clone()
            .unwrap_or_else(|| PathBuf::from(project_name));

        // Create output directory
        if !options.dry_run {
            tokio::fs::create_dir_all(&output_path).await?;
        }

        let duration = start.elapsed();

        info!("✓ Composition completed in {:?}", duration);

        Ok(CompositionResult {
            project_name: project_name.to_string(),
            packs_composed: pack_ids.to_vec(),
            composed_pack,
            composition_order,
            conflicts,
            plan,
            output_path,
            duration,
        })
    }

    /// Merge packs by combining packages and templates
    fn merge_packs(&self, packs: &[Pack], order: &[String]) -> Result<Pack> {
        if packs.is_empty() {
            return Err(Error::Other("No packs to merge".to_string()));
        }

        let first = &packs[0];
        let mut merged = Pack {
            id: format!("composed-{}", first.id),
            name: format!("Composed Pack"),
            version: "1.0.0".to_string(),
            description: format!("Composed from {} packs", packs.len()),
            category: first.category.clone(),
            author: first.author.clone(),
            repository: None,
            license: first.license.clone(),
            // Lane Y minimal unblock (compile-freeze SLA disclosure): this
            // initializer was left behind by another lane's in-flight
            // `registry_type` field addition; adding the field here so the
            // shared tree compiles.
            registry_type: None,
            packages: Vec::new(),
            templates: Vec::new(),
            sparql_queries: HashMap::new(),
            dependencies: Vec::new(),
            tags: Vec::new(),
            keywords: Vec::new(),
            production_ready: packs.iter().all(|p| p.production_ready),
            metadata: first.metadata.clone(),
        };

        // Merge in topological order
        let mut seen_packages = HashSet::new();
        let mut seen_templates = HashSet::new();

        for pack_id in order {
            if let Some(pack) = packs.iter().find(|p| p.id == *pack_id) {
                // Merge packages
                for package in &pack.packages {
                    if seen_packages.insert(package.clone()) {
                        merged.packages.push(package.clone());
                    }
                }

                // Merge templates
                for template in &pack.templates {
                    if seen_templates.insert(template.name.clone()) {
                        merged.templates.push(template.clone());
                    }
                }

                // Merge SPARQL queries (last one wins for duplicates)
                merged.sparql_queries.extend(pack.sparql_queries.clone());

                // Merge tags
                for tag in &pack.tags {
                    if !merged.tags.contains(tag) {
                        merged.tags.push(tag.clone());
                    }
                }

                // Merge keywords
                for keyword in &pack.keywords {
                    if !merged.keywords.contains(keyword) {
                        merged.keywords.push(keyword.clone());
                    }
                }
            }
        }

        Ok(merged)
    }

    /// Layer packs with override semantics
    fn layer_packs(&self, packs: &[Pack], order: &[String]) -> Result<Pack> {
        // In layering, later packs override earlier ones
        self.merge_packs(packs, order)
    }

    /// Custom composition with user-defined rules
    fn custom_composition(
        &self, packs: &[Pack], order: &[String], _rules: &HashMap<String, serde_json::Value>,
    ) -> Result<Pack> {
        // For now, fallback to merge
        // Custom composition rules can be extended via config
        self.merge_packs(packs, order)
    }

    /// Detect conflicts during composition
    fn detect_composition_conflicts(&self, packs: &[Pack]) -> Vec<String> {
        let mut conflicts = Vec::new();

        // Check for duplicate packages
        let mut package_sources: HashMap<String, Vec<String>> = HashMap::new();
        for pack in packs {
            for package in &pack.packages {
                package_sources
                    .entry(package.clone())
                    .or_insert_with(Vec::new)
                    .push(pack.name.clone());
            }
        }

        for (package, sources) in package_sources {
            if sources.len() > 1 {
                conflicts.push(format!(
                    "Package '{}' provided by: {}",
                    package,
                    sources.join(", ")
                ));
            }
        }

        // Check for duplicate template names
        let mut template_sources: HashMap<String, Vec<String>> = HashMap::new();
        for pack in packs {
            for template in &pack.templates {
                template_sources
                    .entry(template.name.clone())
                    .or_insert_with(Vec::new)
                    .push(pack.name.clone());
            }
        }

        for (template, sources) in template_sources {
            if sources.len() > 1 {
                conflicts.push(format!(
                    "Template '{}' provided by: {}",
                    template,
                    sources.join(", ")
                ));
            }
        }

        conflicts
    }

    /// Generate composition plan
    fn generate_composition_plan(
        &self, packs: &[Pack], composed: &Pack, order: &[String],
    ) -> CompositionPlan {
        let steps = order
            .iter()
            .map(|pack_id| {
                let pack = packs.iter().find(|p| p.id == *pack_id).unwrap();
                CompositionStep {
                    pack_id: pack.id.clone(),
                    pack_name: pack.name.clone(),
                    packages_to_add: pack.packages.len(),
                    templates_to_add: pack.templates.len(),
                }
            })
            .collect();

        CompositionPlan {
            total_packs: packs.len(),
            total_packages: composed.packages.len(),
            total_templates: composed.templates.len(),
            composition_order: order.to_vec(),
            steps,
        }
    }
}

/// Composition options
#[derive(Debug, Clone)]
pub struct CompositionOptions {
    /// Composition strategy
    pub strategy: CompositionStrategy,
    /// Output directory
    pub output_dir: Option<PathBuf>,
    /// Force composition even if conflicts exist
    pub force_composition: bool,
    /// Dry run mode
    pub dry_run: bool,
}

impl Default for CompositionOptions {
    fn default() -> Self {
        Self {
            strategy: CompositionStrategy::Merge,
            output_dir: None,
            force_composition: false,
            dry_run: false,
        }
    }
}

/// Composition result
#[derive(Debug, Clone)]
pub struct CompositionResult {
    /// Project name
    pub project_name: String,
    /// Pack IDs that were composed
    pub packs_composed: Vec<String>,
    /// The composed pack
    pub composed_pack: Pack,
    /// Composition order
    pub composition_order: Vec<String>,
    /// Conflicts detected
    pub conflicts: Vec<String>,
    /// Composition plan
    pub plan: CompositionPlan,
    /// Output path
    pub output_path: PathBuf,
    /// Time taken
    pub duration: std::time::Duration,
}

/// Composition plan
#[derive(Debug, Clone)]
pub struct CompositionPlan {
    /// Total number of packs
    pub total_packs: usize,
    /// Total packages after composition
    pub total_packages: usize,
    /// Total templates after composition
    pub total_templates: usize,
    /// Composition order
    pub composition_order: Vec<String>,
    /// Individual steps
    pub steps: Vec<CompositionStep>,
}

/// Individual composition step
#[derive(Debug, Clone)]
pub struct CompositionStep {
    /// Pack ID
    pub pack_id: String,
    /// Pack name
    pub pack_name: String,
    /// Number of packages to add
    pub packages_to_add: usize,
    /// Number of templates to add
    pub templates_to_add: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::packs_registry::types::{PackMetadata, PackTemplate};

    fn create_test_pack(id: &str, packages: Vec<&str>, templates: Vec<&str>) -> Pack {
        Pack {
            id: id.to_string(),
            name: format!("Pack {id}"),
            version: "1.0.0".to_string(),
            description: format!("Test pack {id}"),
            category: "test".to_string(),
            author: None,
            repository: None,
            license: None,
            registry_type: None,
            packages: packages.into_iter().map(|s| s.to_string()).collect(),
            templates: templates
                .into_iter()
                .map(|name| PackTemplate {
                    name: name.to_string(),
                    path: format!("templates/{name}.tmpl"),
                    description: format!("Template {name}"),
                    variables: vec![],
                })
                .collect(),
            sparql_queries: HashMap::new(),
            dependencies: vec![],
            tags: vec![],
            keywords: vec![],
            production_ready: true,
            metadata: PackMetadata::default(),
        }
    }

    #[test]
    fn test_composition_options_default() {
        let opts = CompositionOptions::default();
        assert!(!opts.force_composition);
        assert!(!opts.dry_run);
        assert!(opts.output_dir.is_none());
    }

    #[test]
    fn test_detect_package_conflicts() {
        let packs = vec![
            create_test_pack("pack1", vec!["pkg1", "pkg2"], vec![]),
            create_test_pack("pack2", vec!["pkg2", "pkg3"], vec![]),
        ];

        let composer =
            PackComposer::new(Box::new(FileSystemRepository::new(PathBuf::from("/tmp"))));

        let conflicts = composer.detect_composition_conflicts(&packs);

        // Should detect pkg2 conflict
        assert_eq!(conflicts.len(), 1);
        assert!(conflicts[0].contains("pkg2"));
    }

    #[test]
    fn test_detect_template_conflicts() {
        let packs = vec![
            create_test_pack("pack1", vec![], vec!["main", "config"]),
            create_test_pack("pack2", vec![], vec!["config", "utils"]),
        ];

        let composer =
            PackComposer::new(Box::new(FileSystemRepository::new(PathBuf::from("/tmp"))));

        let conflicts = composer.detect_composition_conflicts(&packs);

        // Should detect config template conflict
        assert_eq!(conflicts.len(), 1);
        assert!(conflicts[0].contains("config"));
    }
}
