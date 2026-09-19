//! DfCM candidate scoping over admitted pack topology.
//!
//! This module is deliberately SELECT-only. It may reduce the candidate set
//! examined by a caller, but it cannot admit a pack, manufacture an artifact,
//! mint a receipt, or authorize DO. Formal GraphLaw/SHACL/gates and BRCE remain
//! downstream boundaries.
//!
//! Default escalation preserves lawful options:
//!
//! LOCAL -> DIRECT -> TWO_LEVEL -> GLOBAL -> UNKNOWN
//!
//! A failed narrow scope is topology information, not graph failure.

use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    sync::Mutex,
    time::Instant,
};

use serde::Serialize;
use sha2::Digest as _;

use crate::{
    error::{AppError, Result},
    pack::{dependency_scope, pack_digest_sha256, Pack, ScopeDepth},
};

/// The only authority a scope result carries.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum CandidateAuthority {
    /// Candidate ordering/filtering only. Admission and actuation are absent.
    SelectOnly,
}

/// Outcome of the bounded DfCM escalation ladder.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ScopeDisposition {
    /// One or more candidates matched the requested semantic boundary.
    Candidates,
    /// Every lawful configured scope was exhausted without a match.
    Unknown,
}

/// Semantic requirement used to filter a structural candidate scope.
///
/// Capability and semantic type are conjunctive when both are supplied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct ScopeRequirement {
    /// Required provided capability, if any.
    pub capability: Option<String>,
    /// Required semantic type, if any.
    pub semantic_type: Option<String>,
}

impl ScopeRequirement {
    /// Construct a capability-only requirement.
    #[must_use]
    pub fn capability(value: impl Into<String>) -> Self {
        Self {
            capability: Some(value.into()),
            semantic_type: None,
        }
    }

    /// Construct a semantic-type-only requirement.
    #[must_use]
    pub fn semantic_type(value: impl Into<String>) -> Self {
        Self {
            capability: None,
            semantic_type: Some(value.into()),
        }
    }

    /// Construct a conjunctive semantic-type + capability requirement.
    #[must_use]
    pub fn typed_capability(
        semantic_type: impl Into<String>, capability: impl Into<String>,
    ) -> Self {
        Self {
            capability: Some(capability.into()),
            semantic_type: Some(semantic_type.into()),
        }
    }

    fn validate(&self) -> Result<()> {
        let capability_valid = self
            .capability
            .as_deref()
            .is_some_and(|value| !value.trim().is_empty());
        let type_valid = self
            .semantic_type
            .as_deref()
            .is_some_and(|value| !value.trim().is_empty());
        if capability_valid || type_valid {
            return Ok(());
        }
        Err(AppError::fm_pack(
            19,
            "scope requirement must contain a non-empty capability and/or semantic type. \
             Remediation: provide the semantic boundary being resolved.",
        ))
    }

    fn matches(&self, pack: &Pack) -> bool {
        let capability_matches = self
            .capability
            .as_ref()
            .is_none_or(|capability| pack.provides.contains(capability));
        let type_matches = self
            .semantic_type
            .as_ref()
            .is_none_or(|semantic_type| pack.semantic_types.contains(semantic_type));
        capability_matches && type_matches
    }
}

/// Replayable result of one bounded candidate resolution.
///
/// The result intentionally carries no admitted object, artifact, authority
/// token, or receipt handle.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ScopeResolution {
    /// Whether a candidate set was found or the bounded search ended UNKNOWN.
    pub disposition: ScopeDisposition,
    /// Scope where the first lawful match was found; absent for UNKNOWN.
    pub depth: Option<ScopeDepth>,
    /// Resolution names of matching candidates, in deterministic scope order.
    pub candidate_names: Vec<String>,
    /// Scope depths actually attempted, in order.
    pub searched_depths: Vec<ScopeDepth>,
    /// Content/edge/registry identity that keys the resolver cache.
    pub cache_fingerprint: String,
    /// Whether this result was served from the exact-fingerprint cache.
    pub cache_hit: bool,
    /// Always SELECT-only.
    pub authority: CandidateAuthority,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct ScopeCacheKey {
    fingerprint: String,
    subject: String,
    requirement: ScopeRequirement,
}

/// Dependency-aware candidate resolver with exact-input cache identity.
///
/// The fingerprint binds every resolved pack portable digest, parsed
/// dependency/type/requires/provides edges, and the caller-supplied registry
/// revision. Re-evaluating after source mutation computes a different key.
pub struct DependencyScopeResolver<'a> {
    packs: &'a [Pack],
    registry_revision: String,
    cache: Mutex<BTreeMap<ScopeCacheKey, ScopeResolution>>,
}

impl<'a> DependencyScopeResolver<'a> {
    /// Create a resolver bound to one resolved pack set and registry snapshot.
    #[must_use]
    pub fn new(packs: &'a [Pack], registry_revision: impl Into<String>) -> Self {
        Self {
            packs,
            registry_revision: registry_revision.into(),
            cache: Mutex::new(BTreeMap::new()),
        }
    }

    /// Change registry identity and invalidate all cached candidate results.
    pub fn set_registry_revision(&mut self, registry_revision: impl Into<String>) {
        self.registry_revision = registry_revision.into();
        if let Ok(mut cache) = self.cache.lock() {
            cache.clear();
        }
    }

    /// Compute exact cache identity for source bytes, topology, and registry.
    ///
    /// # Errors
    /// Propagates pack digest failures.
    pub fn fingerprint(&self) -> Result<String> {
        let mut packs: Vec<&Pack> = self.packs.iter().collect();
        packs.sort_by(|a, b| a.name.cmp(&b.name));

        let mut hasher = sha2::Sha256::new();
        hasher.update(b"ggen-dfcm-scope-v1\0");
        hash_field(&mut hasher, self.registry_revision.as_bytes());

        for pack in packs {
            hash_field(&mut hasher, pack.name.as_bytes());
            hash_field(&mut hasher, pack.version.as_bytes());
            hash_field(&mut hasher, &pack_digest_sha256(pack)?);
            for (dependency, requirement) in &pack.dependencies {
                hash_field(&mut hasher, b"dependency");
                hash_field(&mut hasher, dependency.as_bytes());
                hash_field(&mut hasher, requirement.as_bytes());
            }
            for semantic_type in &pack.semantic_types {
                hash_field(&mut hasher, b"type");
                hash_field(&mut hasher, semantic_type.as_bytes());
            }
            for capability in &pack.provides {
                hash_field(&mut hasher, b"provides");
                hash_field(&mut hasher, capability.as_bytes());
            }
            for capability in &pack.requires {
                hash_field(&mut hasher, b"requires");
                hash_field(&mut hasher, capability.as_bytes());
            }
        }
        Ok(format!("sha256:{}", hex::encode(hasher.finalize())))
    }

    /// Resolve first matching scope in LOCAL -> DIRECT -> TWO_LEVEL -> GLOBAL.
    ///
    /// Exhausting all four scopes returns UNKNOWN, not a refusal and not an
    /// admitted empty result.
    ///
    /// # Errors
    /// Propagates typed pack topology errors and FM-PACK-019 for an empty
    /// requirement.
    pub fn resolve(&self, subject: &str, requirement: &ScopeRequirement) -> Result<ScopeResolution> {
        requirement.validate()?;
        let fingerprint = self.fingerprint()?;
        let key = ScopeCacheKey {
            fingerprint: fingerprint.clone(),
            subject: subject.to_string(),
            requirement: requirement.clone(),
        };

        if let Ok(cache) = self.cache.lock() {
            if let Some(hit) = cache.get(&key) {
                let mut replay = hit.clone();
                replay.cache_hit = true;
                return Ok(replay);
            }
        }

        let mut searched_depths = Vec::new();
        for depth in [
            ScopeDepth::Local,
            ScopeDepth::Direct,
            ScopeDepth::TwoLevel,
            ScopeDepth::Global,
        ] {
            searched_depths.push(depth);
            let candidates = dependency_scope(self.packs, subject, depth)?;
            let candidate_names: Vec<String> = candidates
                .into_iter()
                .filter(|pack| requirement.matches(pack))
                .map(|pack| pack.name.clone())
                .collect();
            if !candidate_names.is_empty() {
                let result = ScopeResolution {
                    disposition: ScopeDisposition::Candidates,
                    depth: Some(depth),
                    candidate_names,
                    searched_depths,
                    cache_fingerprint: fingerprint,
                    cache_hit: false,
                    authority: CandidateAuthority::SelectOnly,
                };
                if let Ok(mut cache) = self.cache.lock() {
                    cache.insert(key, result.clone());
                }
                return Ok(result);
            }
        }

        let result = ScopeResolution {
            disposition: ScopeDisposition::Unknown,
            depth: None,
            candidate_names: Vec::new(),
            searched_depths,
            cache_fingerprint: fingerprint,
            cache_hit: false,
            authority: CandidateAuthority::SelectOnly,
        };
        if let Ok(mut cache) = self.cache.lock() {
            cache.insert(key, result.clone());
        }
        Ok(result)
    }
}

fn hash_field(hasher: &mut sha2::Sha256, bytes: &[u8]) {
    hasher.update((bytes.len() as u64).to_be_bytes());
    hasher.update(bytes);
}

/// Deterministic reverse dependency closure for incremental planning.
///
/// The result contains the changed pack plus every transitive dependent.
/// It is an optimization candidate set only; it never authorizes pruning.
///
/// # Errors
/// FM-PACK-020 when changed is not in the resolved set.
pub fn reverse_dependency_closure(packs: &[Pack], changed: &str) -> Result<Vec<String>> {
    if !packs.iter().any(|pack| pack.name == changed) {
        return Err(AppError::fm_pack(
            20,
            format!(
                "incremental invalidation subject '{changed}' is not among the resolved packs. \
                 Remediation: re-resolve the project before computing reverse closure."
            ),
        ));
    }

    let mut reverse: BTreeMap<&str, BTreeSet<&str>> = BTreeMap::new();
    for pack in packs {
        reverse.entry(pack.name.as_str()).or_default();
        for dependency in pack.dependencies.keys() {
            reverse
                .entry(dependency.as_str())
                .or_default()
                .insert(pack.name.as_str());
        }
    }

    let mut seen = BTreeSet::new();
    let mut queue = VecDeque::from([changed]);
    let mut result = Vec::new();
    while let Some(name) = queue.pop_front() {
        if !seen.insert(name) {
            continue;
        }
        result.push(name.to_string());
        if let Some(dependents) = reverse.get(name) {
            for dependent in dependents {
                queue.push_back(dependent);
            }
        }
    }
    Ok(result)
}

/// Compile dependency/type/capability facts into canonical RDF with public
/// ontology predicates only.
///
/// Pack/capability/type IRIs are stable instance identities. Predicate
/// semantics come from Schema.org and Dublin Core Terms; no project-local
/// predicate vocabulary is introduced.
///
/// Returns an empty string when no pack declares topology/capability facts,
/// preserving legacy graph behavior.
#[must_use]
pub fn topology_turtle(packs: &[Pack]) -> String {
    if !packs.iter().any(|pack| {
        !pack.dependencies.is_empty()
            || !pack.semantic_types.is_empty()
            || !pack.provides.is_empty()
            || !pack.requires.is_empty()
    }) {
        return String::new();
    }

    let mut triples = BTreeSet::new();
    for pack in packs {
        let pack_iri = resource_iri("pack", &pack.name);
        triples.insert(format!(
            "<{pack_iri}> a <https://schema.org/SoftwareSourceCode> ."
        ));
        triples.insert(format!(
            "<{pack_iri}> <https://schema.org/name> {} .",
            ttl_literal(&pack.name)
        ));
        triples.insert(format!(
            "<{pack_iri}> <https://schema.org/softwareVersion> {} .",
            ttl_literal(&pack.version)
        ));

        for dependency in pack.dependencies.keys() {
            let dependency_iri = resource_iri("pack", dependency);
            triples.insert(format!(
                "<{pack_iri}> <http://purl.org/dc/terms/requires> <{dependency_iri}> ."
            ));
        }
        for semantic_type in &pack.semantic_types {
            let type_iri = resource_iri("semantic-type", semantic_type);
            triples.insert(format!(
                "<{type_iri}> a <https://schema.org/DefinedTerm> ."
            ));
            triples.insert(format!(
                "<{type_iri}> <https://schema.org/name> {} .",
                ttl_literal(semantic_type)
            ));
            triples.insert(format!(
                "<{pack_iri}> <https://schema.org/additionalType> <{type_iri}> ."
            ));
        }
        for capability in &pack.provides {
            let capability_iri = resource_iri("capability", capability);
            triples.insert(format!("<{capability_iri}> a <https://schema.org/Action> ."));
            triples.insert(format!(
                "<{capability_iri}> <https://schema.org/name> {} .",
                ttl_literal(capability)
            ));
            triples.insert(format!(
                "<{pack_iri}> <https://schema.org/potentialAction> <{capability_iri}> ."
            ));
        }
        for capability in &pack.requires {
            let capability_iri = resource_iri("capability", capability);
            triples.insert(format!("<{capability_iri}> a <https://schema.org/Action> ."));
            triples.insert(format!(
                "<{capability_iri}> <https://schema.org/name> {} .",
                ttl_literal(capability)
            ));
            triples.insert(format!(
                "<{pack_iri}> <http://purl.org/dc/terms/requires> <{capability_iri}> ."
            ));
        }
    }
    triples.into_iter().collect::<Vec<_>>().join("\n") + "\n"
}

fn resource_iri(kind: &str, value: &str) -> String {
    if oxigraph::model::NamedNode::new(value).is_ok() {
        return value.to_string();
    }
    let digest = sha2::Sha256::digest(value.as_bytes());
    format!("urn:ggen:{kind}:sha256:{}", hex::encode(digest))
}

fn ttl_literal(value: &str) -> String {
    let mut escaped = String::with_capacity(value.len() + 2);
    escaped.push('"');
    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            other => escaped.push(other),
        }
    }
    escaped.push('"');
    escaped
}

/// One DfCM scope benchmark observation.
///
/// LLM tokens, admission events, artifact count, and receipt count are
/// intentionally zero: scoping is deterministic structural machinery.
#[derive(Debug, Clone, Serialize)]
pub struct ScopeBenchmarkRecord {
    /// Scope under evaluation.
    pub depth: ScopeDepth,
    /// Wall-clock observation for this one scope call.
    pub latency_ns: u128,
    /// Number of structural candidates admitted to inspection.
    pub candidate_count: usize,
    /// Reciprocal rank of the first relevant pack for this query.
    pub reciprocal_rank: f64,
    /// Whether a relevant pack appears within the requested top-k.
    pub top_k_hit: bool,
    /// True when the scope contains no relevant candidate.
    pub unknown: bool,
    /// Distinct provided capabilities reachable in this scope.
    pub semantic_capacity: usize,
    /// Declared dependency + capability-requirement edges in this scope.
    pub constraint_coupling_width: usize,
    /// LLM tokens consumed by this deterministic resolver.
    pub llm_tokens: u64,
    /// Admission events performed by this resolver.
    pub admission_events: u64,
    /// Artifacts manufactured by this resolver.
    pub artifact_count: u64,
    /// Receipts minted by this resolver.
    pub receipt_count: u64,
}

/// Measure LOCAL, DIRECT, TWO_LEVEL, and GLOBAL for one relevance judgment.
///
/// This is an observation utility, not a verifier and not release standing.
///
/// # Errors
/// Propagates dependency scope errors.
pub fn benchmark_dfcm_scopes(
    packs: &[Pack], subject: &str, relevant: &BTreeSet<String>, top_k: usize,
) -> Result<Vec<ScopeBenchmarkRecord>> {
    [
        ScopeDepth::Local,
        ScopeDepth::Direct,
        ScopeDepth::TwoLevel,
        ScopeDepth::Global,
    ]
    .into_iter()
    .map(|depth| benchmark_one_scope(packs, subject, depth, relevant, top_k))
    .collect()
}

fn benchmark_one_scope(
    packs: &[Pack], subject: &str, depth: ScopeDepth, relevant: &BTreeSet<String>, top_k: usize,
) -> Result<ScopeBenchmarkRecord> {
    let started = Instant::now();
    let candidates = dependency_scope(packs, subject, depth)?;
    let latency_ns = started.elapsed().as_nanos();

    let first_rank = candidates
        .iter()
        .position(|pack| relevant.contains(&pack.name))
        .map(|index| index + 1);
    let reciprocal_rank = first_rank.map_or(0.0, |rank| 1.0 / rank as f64);
    let top_k_hit = first_rank.is_some_and(|rank| rank <= top_k);

    let mut capabilities = BTreeSet::new();
    let scope_names: BTreeSet<&str> = candidates.iter().map(|pack| pack.name.as_str()).collect();
    let mut coupling_width = 0usize;
    for pack in &candidates {
        capabilities.extend(pack.provides.iter().cloned());
        coupling_width += pack
            .dependencies
            .keys()
            .filter(|dependency| scope_names.contains(dependency.as_str()))
            .count();
        coupling_width += pack.requires.len();
    }

    Ok(ScopeBenchmarkRecord {
        depth,
        latency_ns,
        candidate_count: candidates.len(),
        reciprocal_rank,
        top_k_hit,
        unknown: first_rank.is_none(),
        semantic_capacity: capabilities.len(),
        constraint_coupling_width: coupling_width,
        llm_tokens: 0,
        admission_events: 0,
        artifact_count: 0,
        receipt_count: 0,
    })
}

/// Mean reciprocal rank across benchmark observations.
#[must_use]
pub fn mean_reciprocal_rank(records: &[ScopeBenchmarkRecord]) -> f64 {
    if records.is_empty() {
        return 0.0;
    }
    records
        .iter()
        .map(|record| record.reciprocal_rank)
        .sum::<f64>()
        / records.len() as f64
}
