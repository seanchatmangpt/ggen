#![forbid(unsafe_code)]
//! ABB/SBB manufacture admission kernel (RFC `docs/rfc/v26.9.26/abb-sbb-implementation.md`).
//!
//! ggen is the manufacturing function `A = mu(O*)`. This crate is the pure, IO-free
//! kernel for that function over an admitted enterprise-architecture graph:
//!
//! * [`admit`] refuses manufacture when the selected SBB is UNKNOWN, mutable,
//!   unqualified, stale-qualified, bound to a changed contract, or exceeds the
//!   contract/authority ceilings.
//! * [`plan`] decides `SELECT existing SBB || MANUFACTURE missing realization`. The
//!   decision is a value; nothing here actuates (no DO).
//! * [`manufacture`] compiles one admitted SBB into deterministic artifacts, each
//!   carrying exact architecture provenance, plus a machine-readable receipt.
//! * [`replay`] re-derives a receipt and refuses on any mismatch.
//!
//! Pack, ABB and SBB are distinct identity spaces: a graph that names a pack with an
//! ABB or SBB identity is refused ([`Refusal::PackConflation`]).

use serde::{Deserialize, Serialize};
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use thiserror::Error;

pub const GRAPH_SCHEMA: &str = "ggen.ea.graph.v1";
pub const RECEIPT_SCHEMA: &str = "ggen.abb-sbb.manufacture-receipt.v1";

/// Authority lattice. Manufacture never exceeds `Construct`; `Do` is always refused.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum Authority {
    None,
    Select,
    Construct,
    Do,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Strategy {
    pub id: String,
    pub title: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Capability {
    pub id: String,
    pub strategy: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Abb {
    pub id: String,
    pub capability: String,
    pub contract: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArchitectureContract {
    pub id: String,
    pub abb: String,
    pub version: String,
    pub max_artifacts: usize,
    pub authority_ceiling: Authority,
    pub required_ports: BTreeSet<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArtifactSpec {
    pub path: String,
    pub template: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CandidateSbb {
    pub id: String,
    pub abb: String,
    pub pack: String,
    /// Content digest of this SBB's realization. `None` = UNKNOWN identity.
    pub digest: Option<String>,
    pub mutable: bool,
    pub authority: Authority,
    pub provides_ports: BTreeSet<String>,
    pub artifacts: Vec<ArtifactSpec>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum Verdict {
    Qualified,
    Refuted,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Qualification {
    pub id: String,
    pub sbb: String,
    pub sbb_digest: String,
    pub contract: String,
    pub contract_digest: String,
    pub verdict: Verdict,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Pack {
    pub id: String,
}

/// An admitted EA graph (JSON projection).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EaGraph {
    pub schema: String,
    pub strategy: Strategy,
    pub capabilities: Vec<Capability>,
    pub abbs: Vec<Abb>,
    pub contracts: Vec<ArchitectureContract>,
    pub candidate_sbbs: Vec<CandidateSbb>,
    pub qualifications: Vec<Qualification>,
    #[serde(default)]
    pub packs: Vec<Pack>,
}

/// Generator identity bound into every receipt.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Generator {
    pub id: String,
    pub version: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Request {
    pub abb: String,
    pub sbb: String,
    pub requested_authority: Authority,
    /// Digest of the graph the caller admitted; `Some` binds the request to that subject.
    pub expected_graph_digest: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Error, Serialize, Deserialize)]
#[serde(tag = "refusal", rename_all = "SCREAMING_SNAKE_CASE")]
pub enum Refusal {
    #[error("MALFORMED_GRAPH: {reason}")]
    MalformedGraph { reason: String },
    #[error("DUPLICATE_ELEMENT: {id}")]
    DuplicateElement { id: String },
    #[error("DANGLING_REFERENCE: {from} -> {to}")]
    DanglingReference { from: String, to: String },
    #[error("GRAPH_DIGEST_MISMATCH: expected {expected}, actual {actual}")]
    GraphDigestMismatch { expected: String, actual: String },
    #[error("AUTHORITY_EXCEEDED: requested {requested:?}, ceiling CONSTRUCT")]
    AuthorityExceeded { requested: Authority },
    #[error("INSUFFICIENT_AUTHORITY: requested {admitted:?}, admission for manufacture requires CONSTRUCT")]
    InsufficientAuthority { admitted: Authority },
    #[error("PACK_CONFLATION: {id}")]
    PackConflation { id: String },
    #[error("UNKNOWN_ABB: {id}")]
    UnknownAbb { id: String },
    #[error("UNKNOWN_SBB: {id}")]
    UnknownSbb { id: String },
    #[error("SBB_ABB_MISMATCH: {sbb} realizes {actual}, not {expected}")]
    SbbAbbMismatch {
        sbb: String,
        expected: String,
        actual: String,
    },
    #[error("SBB_IDENTITY_UNKNOWN: {sbb}")]
    SbbIdentityUnknown { sbb: String },
    #[error("SBB_MUTABLE: {sbb}")]
    SbbMutable { sbb: String },
    #[error("SBB_DIGEST_MISMATCH: {sbb} declared {declared}, actual {actual}")]
    SbbDigestMismatch {
        sbb: String,
        declared: String,
        actual: String,
    },
    #[error("SBB_UNQUALIFIED: {sbb}")]
    SbbUnqualified { sbb: String },
    #[error("STALE_QUALIFICATION: {qualification} bound {bound}, SBB now {current}")]
    StaleQualification {
        qualification: String,
        bound: String,
        current: String,
    },
    #[error("CONTRACT_CHANGED: {qualification} bound {bound}, contract now {current}")]
    ContractChanged {
        qualification: String,
        bound: String,
        current: String,
    },
    #[error("EXCEEDS_CONTRACT: {reason}")]
    ExceedsContract { reason: String },
    #[error("EXCEEDS_AUTHORITY_CEILING: {sbb} {authority:?} > {ceiling:?}")]
    ExceedsAuthorityCeiling {
        sbb: String,
        authority: Authority,
        ceiling: Authority,
    },
    #[error("UNSAFE_ARTIFACT_PATH: {path}")]
    UnsafeArtifactPath { path: String },
    #[error("DUPLICATE_ARTIFACT_PATH: {path}")]
    DuplicateArtifactPath { path: String },
    #[error("UNBOUND_PLACEHOLDER: {placeholder} in {path}")]
    UnboundPlaceholder { placeholder: String, path: String },
    #[error("RECEIPT_TAMPERED: declared {declared}, actual {actual}")]
    ReceiptTampered { declared: String, actual: String },
    #[error("REPLAY_MISMATCH: {field}")]
    ReplayMismatch { field: String },
}

/// `SELECT existing SBB || MANUFACTURE missing realization`. A value, never an action.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "decision", rename_all = "SCREAMING_SNAKE_CASE")]
pub enum Decision {
    Select {
        abb: String,
        sbb: String,
        authority: Authority,
    },
    Manufacture {
        abb: String,
        /// Every port the contract requires.
        required_ports: BTreeSet<String>,
        /// Required ports that no candidate SBB of this ABB provides.
        missing_ports: BTreeSet<String>,
        refusals: Vec<Refusal>,
        authority: Authority,
    },
}

/// Output of [`admit`]. Sealed: fields are private and the only constructor is the
/// admission gate, so `manufacture` (mu) is type-restricted to admitted input (O*).
/// A hand-built or edited value cannot exist outside this crate:
///
/// ```compile_fail
/// // Falsifier for the sealed constructor: forging an Admitted outside the crate fails
/// // to compile (private fields), so mu cannot be applied to unadmitted input.
/// let g = ggen_abb_sbb::synthetic_graph(1, 1);
/// let forged = ggen_abb_sbb::Admitted {
///     graph_digest: g.digest(),
///     qualification: "qual:does-not-exist".into(),
/// };
/// ```
///
/// ```compile_fail
/// // Falsifier: an admitted value cannot be edited in place either.
/// let g = ggen_abb_sbb::synthetic_graph(1, 1);
/// let req = ggen_abb_sbb::Request {
///     abb: "abb:event-ingest".into(),
///     sbb: "sbb:ingest-0000".into(),
///     requested_authority: ggen_abb_sbb::Authority::Construct,
///     expected_graph_digest: None,
/// };
/// let mut ad = ggen_abb_sbb::admit(&g, &req).unwrap();
/// ad.qualification = "qual:does-not-exist".into();
/// ```
///
/// ```
/// // Positive control for the two compile_fail blocks above: the same setup compiles
/// // and admits through the gate, so they fail only on the forgery itself.
/// let g = ggen_abb_sbb::synthetic_graph(1, 1);
/// let req = ggen_abb_sbb::Request {
///     abb: "abb:event-ingest".into(),
///     sbb: "sbb:ingest-0000".into(),
///     requested_authority: ggen_abb_sbb::Authority::Construct,
///     expected_graph_digest: None,
/// };
/// let ad = ggen_abb_sbb::admit(&g, &req).unwrap();
/// assert_eq!(ad.qualification(), "qual:ingest-0000");
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Admitted {
    graph_digest: String,
    strategy: String,
    capability: String,
    abb: Abb,
    contract: ArchitectureContract,
    contract_digest: String,
    sbb: CandidateSbb,
    sbb_digest: String,
    qualification: String,
}

impl Admitted {
    pub fn graph_digest(&self) -> &str {
        &self.graph_digest
    }
    pub fn strategy(&self) -> &str {
        &self.strategy
    }
    pub fn capability(&self) -> &str {
        &self.capability
    }
    pub fn abb(&self) -> &Abb {
        &self.abb
    }
    pub fn contract(&self) -> &ArchitectureContract {
        &self.contract
    }
    pub fn contract_digest(&self) -> &str {
        &self.contract_digest
    }
    pub fn sbb(&self) -> &CandidateSbb {
        &self.sbb
    }
    pub fn sbb_digest(&self) -> &str {
        &self.sbb_digest
    }
    pub fn qualification(&self) -> &str {
        &self.qualification
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Artifact {
    pub path: String,
    pub bytes: String,
    pub digest: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OutputDigest {
    pub path: String,
    pub digest: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ManufactureReceipt {
    pub schema: String,
    pub graph_digest: String,
    pub strategy: String,
    pub capability: String,
    pub abb: String,
    pub contract: String,
    pub contract_digest: String,
    pub sbb: String,
    pub sbb_digest: String,
    pub pack: String,
    pub qualification: String,
    pub generator: Generator,
    pub templates_digest: String,
    pub outputs: Vec<OutputDigest>,
    pub authority: Authority,
    pub ceiling: Authority,
    pub receipt_digest: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Manufactured {
    pub artifacts: Vec<Artifact>,
    pub receipt: ManufactureReceipt,
}

fn sha256_hex(bytes: &[u8]) -> String {
    let mut h = Sha256::new();
    h.update(bytes);
    let out = h.finalize();
    let mut s = String::with_capacity(7 + 64);
    s.push_str("sha256:");
    for b in out {
        s.push_str(&format!("{b:02x}"));
    }
    s
}

/// Canonical digest of any serializable value: serde_json maps are key-sorted
/// (no `preserve_order`), so digests are independent of field order.
pub fn canonical_digest<T: Serialize>(value: &T) -> String {
    let v: Value = serde_json::to_value(value).expect("kernel values serialize");
    sha256_hex(v.to_string().as_bytes())
}

/// Parse a graph from JSON bytes, refusing malformed input with a typed refusal.
pub fn parse_graph(json: &str) -> Result<EaGraph, Refusal> {
    let g: EaGraph = serde_json::from_str(json).map_err(|e| Refusal::MalformedGraph {
        reason: e.to_string(),
    })?;
    if g.schema != GRAPH_SCHEMA {
        return Err(Refusal::MalformedGraph {
            reason: format!("schema {} != {GRAPH_SCHEMA}", g.schema),
        });
    }
    Ok(g)
}

impl EaGraph {
    /// Order-canonical copy: every element list sorted by id, so delivery order of
    /// elements never changes the graph digest.
    pub fn canonical(&self) -> EaGraph {
        let mut g = self.clone();
        g.capabilities.sort_by(|a, b| a.id.cmp(&b.id));
        g.abbs.sort_by(|a, b| a.id.cmp(&b.id));
        g.contracts.sort_by(|a, b| a.id.cmp(&b.id));
        g.candidate_sbbs.sort_by(|a, b| a.id.cmp(&b.id));
        // Nested lists too: artifact delivery order inside an SBB must not move the
        // graph digest (falsifier: element_reordering_does_not_change_digest_or_receipt).
        for s in &mut g.candidate_sbbs {
            s.artifacts.sort_by(|a, b| a.path.cmp(&b.path));
        }
        g.qualifications.sort_by(|a, b| a.id.cmp(&b.id));
        g.packs.sort_by(|a, b| a.id.cmp(&b.id));
        g
    }

    pub fn digest(&self) -> String {
        canonical_digest(&self.canonical())
    }

    fn validate(&self) -> Result<(), Refusal> {
        if self.schema != GRAPH_SCHEMA {
            return Err(Refusal::MalformedGraph {
                reason: format!("schema {} != {GRAPH_SCHEMA}", self.schema),
            });
        }
        let mut seen = BTreeSet::new();
        let ids = std::iter::once(self.strategy.id.as_str())
            .chain(self.capabilities.iter().map(|x| x.id.as_str()))
            .chain(self.abbs.iter().map(|x| x.id.as_str()))
            .chain(self.contracts.iter().map(|x| x.id.as_str()))
            .chain(self.candidate_sbbs.iter().map(|x| x.id.as_str()))
            .chain(self.qualifications.iter().map(|x| x.id.as_str()));
        for id in ids {
            if id.trim().is_empty() {
                return Err(Refusal::MalformedGraph {
                    reason: "empty element id".into(),
                });
            }
            if !seen.insert(id) {
                return Err(Refusal::DuplicateElement { id: id.to_string() });
            }
        }
        let mut pack_ids = BTreeSet::new();
        for p in &self.packs {
            if !pack_ids.insert(p.id.as_str()) {
                return Err(Refusal::DuplicateElement { id: p.id.clone() });
            }
            // Pack is neither an ABB nor an SBB (RFC DoD 9).
            if seen.contains(p.id.as_str()) {
                return Err(Refusal::PackConflation { id: p.id.clone() });
            }
        }
        let dangling = |from: &str, to: &str| Refusal::DanglingReference {
            from: from.into(),
            to: to.into(),
        };
        for c in &self.capabilities {
            if c.strategy != self.strategy.id {
                return Err(dangling(&c.id, &c.strategy));
            }
        }
        for a in &self.abbs {
            if !self.capabilities.iter().any(|c| c.id == a.capability) {
                return Err(dangling(&a.id, &a.capability));
            }
            match self.contracts.iter().find(|k| k.id == a.contract) {
                Some(k) if k.abb == a.id => {}
                _ => return Err(dangling(&a.id, &a.contract)),
            }
        }
        for s in &self.candidate_sbbs {
            // Pack is neither an ABB nor an SBB, nor any other element (RFC DoD 9): the
            // pack id space is disjoint from every element id, whether or not the graph
            // carries a `packs` list.
            if s.pack.trim().is_empty() || seen.contains(s.pack.as_str()) {
                return Err(Refusal::PackConflation { id: s.pack.clone() });
            }
            if !self.packs.is_empty() && !pack_ids.contains(s.pack.as_str()) {
                return Err(dangling(&s.id, &s.pack));
            }
            if !self.abbs.iter().any(|a| a.id == s.abb) {
                return Err(dangling(&s.id, &s.abb));
            }
        }
        for q in &self.qualifications {
            if !self.candidate_sbbs.iter().any(|s| s.id == q.sbb) {
                return Err(dangling(&q.id, &q.sbb));
            }
            if !self.contracts.iter().any(|k| k.id == q.contract) {
                return Err(dangling(&q.id, &q.contract));
            }
        }
        Ok(())
    }
}

/// Content digest of an SBB realization (id, abb, pack, ports, authority, artifact specs).
/// Excludes the declared `digest` and `mutable` flag.
pub fn sbb_content_digest(s: &CandidateSbb) -> String {
    let mut arts = s.artifacts.clone();
    arts.sort_by(|a, b| a.path.cmp(&b.path));
    canonical_digest(&serde_json::json!({
        "id": s.id, "abb": s.abb, "pack": s.pack, "authority": s.authority,
        "provides_ports": s.provides_ports, "artifacts": arts,
    }))
}

pub fn contract_digest(k: &ArchitectureContract) -> String {
    canonical_digest(k)
}

fn admit_sbb(
    g: &EaGraph, graph_digest: &str, abb: &Abb, sbb_id: &str,
) -> Result<Admitted, Refusal> {
    let sbb = g
        .candidate_sbbs
        .iter()
        .find(|s| s.id == sbb_id)
        .ok_or_else(|| Refusal::UnknownSbb { id: sbb_id.into() })?;
    if sbb.abb != abb.id {
        return Err(Refusal::SbbAbbMismatch {
            sbb: sbb.id.clone(),
            expected: abb.id.clone(),
            actual: sbb.abb.clone(),
        });
    }
    let declared = sbb
        .digest
        .clone()
        .ok_or_else(|| Refusal::SbbIdentityUnknown {
            sbb: sbb.id.clone(),
        })?;
    if sbb.mutable {
        return Err(Refusal::SbbMutable {
            sbb: sbb.id.clone(),
        });
    }
    let actual = sbb_content_digest(sbb);
    if declared != actual {
        return Err(Refusal::SbbDigestMismatch {
            sbb: sbb.id.clone(),
            declared,
            actual,
        });
    }
    let contract = g
        .contracts
        .iter()
        .find(|k| k.id == abb.contract)
        .expect("validated reference");
    let k_digest = contract_digest(contract);
    let mut quals: Vec<&Qualification> = g
        .qualifications
        .iter()
        .filter(|q| q.sbb == sbb.id && q.verdict == Verdict::Qualified)
        .collect();
    quals.sort_by(|a, b| a.id.cmp(&b.id));
    if quals.is_empty()
        || g.qualifications
            .iter()
            .any(|q| q.sbb == sbb.id && q.verdict == Verdict::Refuted)
    {
        return Err(Refusal::SbbUnqualified {
            sbb: sbb.id.clone(),
        });
    }
    // Every positive qualification must still bind the current subject and contract.
    for q in &quals {
        if q.sbb_digest != actual {
            return Err(Refusal::StaleQualification {
                qualification: q.id.clone(),
                bound: q.sbb_digest.clone(),
                current: actual.clone(),
            });
        }
        if q.contract != contract.id || q.contract_digest != k_digest {
            return Err(Refusal::ContractChanged {
                qualification: q.id.clone(),
                bound: q.contract_digest.clone(),
                current: k_digest.clone(),
            });
        }
    }
    if sbb.artifacts.is_empty() {
        return Err(Refusal::ExceedsContract {
            reason: format!("{} realizes no artifacts", sbb.id),
        });
    }
    if sbb.artifacts.len() > contract.max_artifacts {
        return Err(Refusal::ExceedsContract {
            reason: format!(
                "{} artifacts > max {}",
                sbb.artifacts.len(),
                contract.max_artifacts
            ),
        });
    }
    let missing: Vec<&String> = contract
        .required_ports
        .difference(&sbb.provides_ports)
        .collect();
    if !missing.is_empty() {
        return Err(Refusal::ExceedsContract {
            reason: format!("missing ports {missing:?}"),
        });
    }
    let ceiling = contract.authority_ceiling.min(Authority::Construct);
    if sbb.authority > ceiling {
        return Err(Refusal::ExceedsAuthorityCeiling {
            sbb: sbb.id.clone(),
            authority: sbb.authority,
            ceiling,
        });
    }
    let cap = g
        .capabilities
        .iter()
        .find(|c| c.id == abb.capability)
        .expect("validated reference");
    // Admission implies manufacturability: every artifact path is safe and unique and
    // every template placeholder binds, so SELECT never names an SBB mu would refuse.
    let mut paths = BTreeSet::new();
    for spec in &sbb.artifacts {
        check_path(&spec.path)?;
        if !paths.insert(spec.path.as_str()) {
            return Err(Refusal::DuplicateArtifactPath {
                path: spec.path.clone(),
            });
        }
        check_template(&spec.template, &spec.path)?;
    }
    Ok(Admitted {
        graph_digest: graph_digest.to_string(),
        strategy: g.strategy.id.clone(),
        capability: cap.id.clone(),
        abb: abb.clone(),
        contract: contract.clone(),
        contract_digest: k_digest,
        sbb: sbb.clone(),
        sbb_digest: actual,
        qualification: quals[0].id.clone(),
    })
}

/// Validates the graph and computes its digest exactly once per call (the digest is the
/// dominant cost; see `bench/receipt.json`).
fn admit_common<'g>(
    g: &'g EaGraph, abb: &str, authority: Authority, expected: Option<&str>,
) -> Result<(&'g Abb, String), Refusal> {
    g.validate()?;
    let actual = g.digest();
    if let Some(expected) = expected {
        if expected != actual {
            return Err(Refusal::GraphDigestMismatch {
                expected: expected.into(),
                actual,
            });
        }
    }
    if authority > Authority::Construct {
        return Err(Refusal::AuthorityExceeded {
            requested: authority,
        });
    }
    let a = g
        .abbs
        .iter()
        .find(|a| a.id == abb)
        .ok_or_else(|| Refusal::UnknownAbb { id: abb.into() })?;
    Ok((a, actual))
}

/// Admit one selected SBB for manufacture, or refuse with a typed reason.
pub fn admit(g: &EaGraph, req: &Request) -> Result<Admitted, Refusal> {
    let (abb, digest) = admit_common(
        g,
        &req.abb,
        req.requested_authority,
        req.expected_graph_digest.as_deref(),
    )?;
    // An Admitted value licenses manufacture, which runs at CONSTRUCT; admission at a
    // lower requested authority is refused so the value is bound to CONSTRUCT (court A5).
    // Selection below CONSTRUCT goes through `plan`, which returns a value, not an Admitted.
    if req.requested_authority < Authority::Construct {
        return Err(Refusal::InsufficientAuthority {
            admitted: req.requested_authority,
        });
    }
    admit_sbb(g, &digest, abb, &req.sbb)
}

/// `SELECT existing SBB || MANUFACTURE missing realization`, separate from DO.
/// Selection is deterministic: the lowest-id admissible candidate wins.
pub fn plan(g: &EaGraph, abb: &str, authority: Authority) -> Result<Decision, Refusal> {
    let (a, digest) = admit_common(g, abb, authority, None)?;
    let mut candidates: Vec<&CandidateSbb> =
        g.candidate_sbbs.iter().filter(|s| s.abb == a.id).collect();
    candidates.sort_by(|x, y| x.id.cmp(&y.id));
    let mut refusals = Vec::new();
    // Ports no candidate realization of this ABB provides at all. A candidate refused
    // for another reason (stale, mutable, ...) still counts as providing its ports.
    let mut provided = BTreeSet::new();
    for s in &candidates {
        provided.extend(s.provides_ports.iter().cloned());
    }
    for s in candidates {
        match admit_sbb(g, &digest, a, &s.id) {
            Ok(ad) => {
                return Ok(Decision::Select {
                    abb: a.id.clone(),
                    sbb: ad.sbb.id,
                    authority: Authority::Select,
                })
            }
            Err(r) => refusals.push(r),
        }
    }
    let contract = g
        .contracts
        .iter()
        .find(|k| k.id == a.contract)
        .expect("validated reference");
    Ok(Decision::Manufacture {
        abb: a.id.clone(),
        required_ports: contract.required_ports.clone(),
        missing_ports: contract
            .required_ports
            .difference(&provided)
            .cloned()
            .collect(),
        refusals,
        authority: Authority::Construct,
    })
}

fn check_path(path: &str) -> Result<(), Refusal> {
    let bad = path.is_empty()
        || path.starts_with('/')
        || path.contains('\\')
        || path.contains('\0')
        || path
            .split('/')
            .any(|seg| seg.is_empty() || seg == "." || seg == "..");
    if bad {
        return Err(Refusal::UnsafeArtifactPath { path: path.into() });
    }
    Ok(())
}

/// Placeholder keys `manufacture` binds; `check_template` and `render` share this list.
const TEMPLATE_KEYS: [&str; 7] = [
    "strategy",
    "capability",
    "abb",
    "sbb",
    "contract",
    "contract_version",
    "graph_digest",
];

/// Allocation-free admission-time check that every `{{key}}` is terminated and bound.
fn check_template(template: &str, path: &str) -> Result<(), Refusal> {
    let mut rest = template;
    while let Some(start) = rest.find("{{") {
        let after = &rest[start + 2..];
        let end = after
            .find("}}")
            .ok_or_else(|| Refusal::UnboundPlaceholder {
                placeholder: after.chars().take(32).collect(),
                path: path.into(),
            })?;
        let key = after[..end].trim();
        if !TEMPLATE_KEYS.contains(&key) {
            return Err(Refusal::UnboundPlaceholder {
                placeholder: key.into(),
                path: path.into(),
            });
        }
        rest = &after[end + 2..];
    }
    Ok(())
}

fn render(template: &str, vars: &BTreeMap<&str, &str>, path: &str) -> Result<String, Refusal> {
    let mut out = String::with_capacity(template.len() + 64);
    let mut rest = template;
    while let Some(start) = rest.find("{{") {
        out.push_str(&rest[..start]);
        let after = &rest[start + 2..];
        let end = after
            .find("}}")
            .ok_or_else(|| Refusal::UnboundPlaceholder {
                placeholder: after.chars().take(32).collect(),
                path: path.into(),
            })?;
        let key = after[..end].trim();
        let val = vars.get(key).ok_or_else(|| Refusal::UnboundPlaceholder {
            placeholder: key.into(),
            path: path.into(),
        })?;
        out.push_str(val);
        rest = &after[end + 2..];
    }
    out.push_str(rest);
    Ok(out)
}

fn receipt_digest(r: &ManufactureReceipt) -> String {
    let mut c = r.clone();
    c.receipt_digest = String::new();
    canonical_digest(&c)
}

/// Compile an admitted SBB into deterministic artifacts and a receipt. Pure: returns
/// bytes, writes nothing.
pub fn manufacture(ad: &Admitted, generator: &Generator) -> Result<Manufactured, Refusal> {
    let values = [
        ad.strategy.as_str(),
        ad.capability.as_str(),
        ad.abb.id.as_str(),
        ad.sbb.id.as_str(),
        ad.contract.id.as_str(),
        ad.contract.version.as_str(),
        ad.graph_digest.as_str(),
    ];
    let vars: BTreeMap<&str, &str> = TEMPLATE_KEYS.into_iter().zip(values).collect();
    let mut specs = ad.sbb.artifacts.clone();
    specs.sort_by(|a, b| a.path.cmp(&b.path));
    let mut artifacts = Vec::with_capacity(specs.len());
    let mut paths = BTreeSet::new();
    for spec in &specs {
        check_path(&spec.path)?;
        if !paths.insert(spec.path.as_str()) {
            return Err(Refusal::DuplicateArtifactPath {
                path: spec.path.clone(),
            });
        }
        let body = render(&spec.template, &vars, &spec.path)?;
        let bytes = format!(
            "ggen-provenance: graph={} strategy={} capability={} abb={} contract={}@{} sbb={} sbb_digest={} generator={}@{}\n{}",
            ad.graph_digest,
            ad.strategy,
            ad.capability,
            ad.abb.id,
            ad.contract.id,
            ad.contract_digest,
            ad.sbb.id,
            ad.sbb_digest,
            generator.id,
            generator.version,
            body
        );
        let digest = sha256_hex(bytes.as_bytes());
        artifacts.push(Artifact {
            path: spec.path.clone(),
            bytes,
            digest,
        });
    }
    let mut receipt = ManufactureReceipt {
        schema: RECEIPT_SCHEMA.into(),
        graph_digest: ad.graph_digest.clone(),
        strategy: ad.strategy.clone(),
        capability: ad.capability.clone(),
        abb: ad.abb.id.clone(),
        contract: ad.contract.id.clone(),
        contract_digest: ad.contract_digest.clone(),
        sbb: ad.sbb.id.clone(),
        sbb_digest: ad.sbb_digest.clone(),
        pack: ad.sbb.pack.clone(),
        qualification: ad.qualification.clone(),
        generator: generator.clone(),
        templates_digest: canonical_digest(&specs),
        outputs: artifacts
            .iter()
            .map(|a| OutputDigest {
                path: a.path.clone(),
                digest: a.digest.clone(),
            })
            .collect(),
        authority: Authority::None,
        ceiling: Authority::Construct,
        receipt_digest: String::new(),
    };
    receipt.receipt_digest = receipt_digest(&receipt);
    Ok(Manufactured { artifacts, receipt })
}

/// Verify a receipt's self-digest (tamper evidence) without re-manufacturing.
pub fn verify_receipt(r: &ManufactureReceipt) -> Result<(), Refusal> {
    let actual = receipt_digest(r);
    if actual != r.receipt_digest {
        return Err(Refusal::ReceiptTampered {
            declared: r.receipt_digest.clone(),
            actual,
        });
    }
    if r.schema != RECEIPT_SCHEMA {
        return Err(Refusal::ReplayMismatch {
            field: "schema".into(),
        });
    }
    if r.authority != Authority::None || r.ceiling > Authority::Construct {
        return Err(Refusal::AuthorityExceeded {
            requested: r.ceiling.max(r.authority),
        });
    }
    Ok(())
}

/// Re-admit and re-manufacture from the graph; refuse on the first differing field.
pub fn replay(
    r: &ManufactureReceipt, g: &EaGraph, generator: &Generator,
) -> Result<Manufactured, Refusal> {
    verify_receipt(r)?;
    let req = Request {
        abb: r.abb.clone(),
        sbb: r.sbb.clone(),
        requested_authority: Authority::Construct,
        expected_graph_digest: Some(r.graph_digest.clone()),
    };
    let ad = admit(g, &req).map_err(|e| match e {
        Refusal::GraphDigestMismatch { .. } => Refusal::ReplayMismatch {
            field: "graph_digest".into(),
        },
        other => other,
    })?;
    let m = manufacture(&ad, generator)?;
    let fields: [(&str, bool); 8] = [
        (
            "contract_digest",
            m.receipt.contract_digest == r.contract_digest,
        ),
        ("sbb_digest", m.receipt.sbb_digest == r.sbb_digest),
        ("qualification", m.receipt.qualification == r.qualification),
        ("generator", m.receipt.generator == r.generator),
        (
            "templates_digest",
            m.receipt.templates_digest == r.templates_digest,
        ),
        ("outputs", m.receipt.outputs == r.outputs),
        ("pack", m.receipt.pack == r.pack),
        (
            "receipt_digest",
            m.receipt.receipt_digest == r.receipt_digest,
        ),
    ];
    if let Some((field, _)) = fields.iter().find(|(_, ok)| !ok) {
        return Err(Refusal::ReplayMismatch {
            field: (*field).into(),
        });
    }
    Ok(m)
}

/// Deterministic scaled fixture: one strategy/capability/ABB, `n_sbbs` candidates each
/// with `n_artifacts` artifacts, every candidate qualified against the live contract.
/// Used by tests and the benchmark so both measure the same subject.
pub fn synthetic_graph(n_sbbs: usize, n_artifacts: usize) -> EaGraph {
    let contract = ArchitectureContract {
        id: "contract:ingest".into(),
        abb: "abb:event-ingest".into(),
        version: "1.0.0".into(),
        max_artifacts: n_artifacts.max(1),
        authority_ceiling: Authority::Construct,
        required_ports: BTreeSet::from([
            "port:events-in".to_string(),
            "port:receipts-out".to_string(),
        ]),
    };
    let k_digest = contract_digest(&contract);
    let mut sbbs = Vec::new();
    let mut quals = Vec::new();
    let mut packs = Vec::new();
    for i in 0..n_sbbs {
        let pack = format!("pack:ingest-{i:04}");
        let mut s = CandidateSbb {
            id: format!("sbb:ingest-{i:04}"),
            abb: "abb:event-ingest".into(),
            pack: pack.clone(),
            digest: None,
            mutable: false,
            authority: Authority::Construct,
            provides_ports: BTreeSet::from(["port:events-in".to_string(), "port:receipts-out".to_string()]),
            artifacts: (0..n_artifacts)
                .map(|j| ArtifactSpec {
                    path: format!("gen/{i:04}/module_{j:04}.rs"),
                    template: "// abb {{abb}} realized by {{sbb}} for {{capability}}\npub const CONTRACT: &str = \"{{contract}}@{{contract_version}}\";\n".into(),
                })
                .collect(),
        };
        let d = sbb_content_digest(&s);
        s.digest = Some(d.clone());
        quals.push(Qualification {
            id: format!("qual:ingest-{i:04}"),
            sbb: s.id.clone(),
            sbb_digest: d,
            contract: contract.id.clone(),
            contract_digest: k_digest.clone(),
            verdict: Verdict::Qualified,
        });
        packs.push(Pack { id: pack });
        sbbs.push(s);
    }
    EaGraph {
        schema: GRAPH_SCHEMA.into(),
        strategy: Strategy {
            id: "strategy:real-time-ops".into(),
            title: "Real-time operations".into(),
        },
        capabilities: vec![Capability {
            id: "cap:event-ingest".into(),
            strategy: "strategy:real-time-ops".into(),
        }],
        abbs: vec![Abb {
            id: "abb:event-ingest".into(),
            capability: "cap:event-ingest".into(),
            contract: "contract:ingest".into(),
        }],
        contracts: vec![contract],
        candidate_sbbs: sbbs,
        qualifications: quals,
        packs,
    }
}
