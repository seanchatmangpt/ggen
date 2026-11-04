//! Origin-authority admission and verification — Rust port of
//! `GgenIgniter.SemanticJira.Authority` (authority.ex) and the pack shape
//! `sj:AuthorityTrustRootShape`
//! (ggen_igniter/priv/ggen/semantic-jira-pack/shapes/work-order.shacl.ttl).
//!
//! The law: every work order carries `origin_authority` — an IRI that must
//! resolve to an ADMITTED `sj:CodeWorkAuthority` node. Typed is not admitted:
//! a node is admitted only when it carries an `sj:admissionDigest`
//! (`sha256:<64 hex>`) that RECOMPUTES byte-equal over its canonical
//! description with all `admissionDigest` predicates excluded (the
//! self-referential-exclusion construction — self-declaration is defeated
//! because stamping is idempotent: a node cannot change its own digest by
//! stating it). Admission further requires the `(iri, digest)` pair to be
//! pinned by an `sj:AuthorityTrustRoot` under a DURABLE locator; an admitted
//! node whose pair is not exactly pinned is refused
//! [`OriginRefusal::AuthorityNotPinned`].
//!
//! Surfaces (mirroring the igniter module):
//! - [`admission_digest`] — the self-exclusion digest law;
//! - [`AuthorityGraph::from_json`] / [`AuthorityGraph::from_json_str`] — the
//!   authority graph source (JSON variant; TTL parsing deferred — disclosed
//!   at [`AuthorityGraph::from_json`]);
//! - [`AuthorityIndex::build`] + [`AuthorityIndex::resolve`] — the ONE origin
//!   law every selection/promotion surface resolves through;
//! - [`AuthorityTrustRoot`] / [`pins`] — the pin law (G1 hardening);
//! - [`AuthorityIndex::apply_pins`] — index projection through the pins.
//!
//! Nothing here grants authority or performs DO: the module computes
//! digests, stamps them onto typed authority nodes, and refuses when an
//! origin binding does not resolve. All refusals fail closed.
//!
//! `#![forbid(unsafe_code)]` is inherited from the crate root. IO-free except
//! the path loader. NO SHACL validation runs inside admission — callers run
//! their court first.

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::BTreeMap;
use std::fmt;
use std::path::Path;

pub const SJ_NAMESPACE: &str = "https://ggen-igniter.dev/ontology/semantic-jira#";

/// Digest context (`@admission_digest_context`, authority.ex line 47).
pub const ADMISSION_DIGEST_CONTEXT: &str = "code-work-authority:v1";

/// Authority classes (`@authority_classes`, lines 49-52).
pub const AUTHORITY_CLASSES: [&str; 2] = ["StrategicObjective", "GoalCheckpoint"];

/// Prose classes — never code-work authority, whatever else they are typed as
/// (ADR-012; `@prose_classes`, lines 54-59).
pub const PROSE_CLASSES: [&str; 2] = ["Proposition", "ProseObservation"];

/// The digest pattern (`@digest_pattern`, line 64): `sha256:<64 hex>`.
pub fn valid_digest(s: &str) -> bool {
    match s.strip_prefix("sha256:") {
        Some(rest) => {
            rest.len() == 64
                && rest.bytes().all(|b| matches!(b, b'0'..=b'9' | b'a'..=b'f'))
        }
        None => false,
    }
}

fn sha256_hex(input: &str) -> String {
    use sha2::Digest as _;
    let mut hasher = sha2::Sha256::new();
    hasher.update(input.as_bytes());
    let out = hasher.finalize();
    out.iter().map(|b| format!("{b:02x}")).collect()
}

/// The type of an admitted code-work authority node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AuthorityType {
    StrategicObjective,
    GoalCheckpoint,
}

impl fmt::Display for AuthorityType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AuthorityType::StrategicObjective => write!(f, "StrategicObjective"),
            AuthorityType::GoalCheckpoint => write!(f, "GoalCheckpoint"),
        }
    }
}

/// One authority node description of the JSON authority graph.
///
/// JSON source shape (the TTL variant is deferred — this module ports the
/// admission LAW, not the Turtle parser; disclosure per contract):
///
/// ```json
/// {
///   "iri": "https://graph.test/auth/x1",
///   "types": ["StrategicObjective"],
///   "statements": { "sj:title": ["Ship the kernel"] }
/// }
/// ```
///
/// Types participate in the digest as `rdf:type` statements, so an edited
/// type set under a kept digest is refused `AuthorityDigestMismatch`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct AuthorityNode {
    pub iri: String,
    #[serde(default)]
    pub types: Vec<String>,
    /// Predicate -> objects. Deterministic iteration via BTreeMap.
    #[serde(default)]
    pub statements: BTreeMap<String, Vec<String>>,
}

/// One `sj:AuthorityTrustRoot` candidate of the JSON authority graph.
///
/// JSON source shape (names follow the SHACL shape's properties):
///
/// ```json
/// {
///   "node": "https://graph.test/trust/r1",
///   "authority_iri": "https://graph.test/auth/x1",
///   "admission_digest": "sha256:...",
///   "source_locator": "git:owner/repo@0123...def:priv/ontology.ttl"
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TrustRootCandidate {
    /// The trust-root node's own IRI (diagnostic only; pinning keys on the
    /// `authority_iri`).
    #[serde(default)]
    pub node: String,
    pub authority_iri: String,
    #[serde(rename = "admission_digest", alias = "admissionDigest")]
    pub admission_digest: String,
    #[serde(rename = "source_locator", alias = "sourceLocator")]
    pub source_locator: String,
}

/// The full JSON authority graph: authority nodes plus trust-root candidates.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(deny_unknown_fields)]
pub struct AuthorityGraph {
    #[serde(default)]
    pub authorities: Vec<AuthorityNode>,
    #[serde(default)]
    pub trust_roots: Vec<TrustRootCandidate>,
}

impl AuthorityGraph {
    /// Parses the JSON authority graph. Parse failure is
    /// [`OriginRefusal::AuthorityIndexUnavailable`] — fail closed.
    pub fn from_json(value: &Value) -> Result<AuthorityGraph, OriginRefusal> {
        serde_json::from_value(value.clone())
            .map_err(|e| OriginRefusal::AuthorityIndexUnavailable {
                path: None,
                detail: format!("unparseable authority graph: {e}"),
            })
    }

    /// Parses the JSON authority graph from a JSON string.
    pub fn from_json_str(s: &str) -> Result<AuthorityGraph, OriginRefusal> {
        serde_json::from_str(s).map_err(|e| OriginRefusal::AuthorityIndexUnavailable {
            path: None,
            detail: format!("unparseable authority graph: {e}"),
        })
    }

    /// Reads and parses the JSON authority graph from `path`.
    /// Unreadable or unparseable — [`OriginRefusal::AuthorityIndexUnavailable`],
    /// fail closed.
    pub fn from_path(path: impl AsRef<Path>) -> Result<AuthorityGraph, OriginRefusal> {
        let shown = path.as_ref().display().to_string();
        let bytes = std::fs::read_to_string(path).map_err(|e| {
            OriginRefusal::AuthorityIndexUnavailable {
                path: Some(shown.clone()),
                detail: format!("unreadable: {e}"),
            }
        })?;
        AuthorityGraph::from_json_str(&bytes)
    }
}

/// A parsed, validated durable source locator (CE23-9 grammar from the SHACL
/// shape): `git:owner/repo@<40-hex-sha>:<path>`, `git-notes:refs/notes/<ref>@
/// <40-hex-sha>`, or `https://...`. Scratch/tmp/`~`-rooted locators are
/// REFUSED — a pin under an ephemeral locator is not durable admission.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceLocator {
    Git { repo: String, sha: String, path: String },
    GitNotes { note_ref: String, sha: String },
    Https(String),
}

/// Typed locator refusal.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum LocatorRefusal {
    #[error("LocatorNotDurable: {0} — remediation: source_locator must be a durable \
             locator (git:owner/repo@<40-hex>:<path>, git-notes:refs/notes/<ref>@<40-hex>, \
             or https://...); scratch, tmp, home-relative, and bare filesystem paths \
             are refused")]
    NotDurable(String),

    #[error("LocatorMalformed: {0} — remediation: source_locator must match the \
             CE23-9 grammar (git:owner/repo@<40-hex>:<path> | \
             git-notes:refs/notes/<ref>@<40-hex> | https://...)")]
    Malformed(String),
}

fn is_forbidden_locator_path(path: &str) -> bool {
    path.is_empty()
        || path.starts_with('/')
        || path.starts_with('~')
        || path == "tmp"
        || path == "scratch"
        || path.starts_with("tmp/")
        || path.starts_with("scratch/")
        || path.starts_with("./")
        || path.starts_with("../")
}

fn valid_locator_sha(s: &str) -> bool {
    s.len() == 40 && s.bytes().all(|b| matches!(b, b'0'..=b'9' | b'a'..=b'f'))
}

impl SourceLocator {
    /// Parses and validates a source locator string per the CE23-9 grammar.
    pub fn parse(s: &str) -> Result<SourceLocator, LocatorRefusal> {
        if s.starts_with('/') || s.starts_with('~') {
            return Err(LocatorRefusal::NotDurable(s.to_string()));
        }
        if let Some(rest) = s.strip_prefix("git:") {
            return Self::parse_git(rest);
        }
        if let Some(rest) = s.strip_prefix("git-notes:") {
            return Self::parse_git_notes(rest);
        }
        if let Some(rest) = s.strip_prefix("https://") {
            if rest.is_empty() || rest.chars().any(|c| c.is_whitespace()) {
                return Err(LocatorRefusal::Malformed(s.to_string()));
            }
            return Ok(SourceLocator::Https(s.to_string()));
        }
        // `file:` and every other scheme: not a durable pack locator.
        Err(LocatorRefusal::NotDurable(s.to_string()))
    }

    fn parse_git(rest: &str) -> Result<SourceLocator, LocatorRefusal> {
        // git:<owner/repo>@<sha>:<path>
        let bad = || LocatorRefusal::Malformed(format!("git:{rest}"));
        let at = rest.rfind('@').ok_or_else(bad)?;
        let repo = &rest[..at];
        let after = &rest[at + 1..];
        let colon = after.find(':').ok_or_else(bad)?;
        let sha = &after[..colon];
        let path = &after[colon + 1..];
        if repo.is_empty()
            || !repo.contains('/')
            || repo.starts_with('/')
            || !valid_locator_sha(sha)
            || is_forbidden_locator_path(path)
        {
            return Err(bad());
        }
        Ok(SourceLocator::Git {
            repo: repo.to_string(),
            sha: sha.to_string(),
            path: path.to_string(),
        })
    }

    fn parse_git_notes(rest: &str) -> Result<SourceLocator, LocatorRefusal> {
        // git-notes:refs/notes/<ref>@<sha>
        let bad = || LocatorRefusal::Malformed(format!("git-notes:{rest}"));
        let at = rest.rfind('@').ok_or_else(bad)?;
        let note_ref = &rest[..at];
        let sha = &rest[at + 1..];
        if !note_ref.starts_with("refs/notes/")
            || note_ref.len() <= "refs/notes/".len()
            || !valid_locator_sha(sha)
        {
            return Err(bad());
        }
        Ok(SourceLocator::GitNotes {
            note_ref: note_ref.to_string(),
            sha: sha.to_string(),
        })
    }
}

/// One `sj:AuthorityTrustRoot` pin: `(authority_iri, sha256)` pinned under a
/// durable locator. Well-formed only when the digest matches the sha256
/// pattern AND the locator parses under the CE23-9 grammar; a malformed
/// trust root pins nothing (fail closed), mirroring the igniter `pin_of/1`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AuthorityTrustRoot {
    pub node: String,
    pub authority_iri: String,
    pub admission_digest: String,
    pub locator: SourceLocator,
}

impl AuthorityTrustRoot {
    /// Validates a candidate into a well-formed trust root, or a typed
    /// refusal ([`LocatorRefusal`] for the locator,
    /// [`OriginRefusal::AuthorityDigestInvalid`] for the digest).
    pub fn new(
        node: impl Into<String>,
        authority_iri: impl Into<String>,
        admission_digest: impl Into<String>,
        source_locator: impl Into<String>,
    ) -> Result<AuthorityTrustRoot, OriginRefusal> {
        let node = node.into();
        let authority_iri = authority_iri.into();
        let admission_digest = admission_digest.into();
        let locator_str = source_locator.into();
        if !valid_digest(&admission_digest) {
            return Err(OriginRefusal::AuthorityDigestInvalid {
                iri: authority_iri,
            });
        }
        let locator = SourceLocator::parse(&locator_str).map_err(|e| {
            OriginRefusal::AuthorityIndexUnavailable {
                path: None,
                detail: format!("trust root {node}: {e}"),
            }
        })?;
        Ok(AuthorityTrustRoot {
            node,
            authority_iri,
            admission_digest,
            locator,
        })
    }

    /// Validates a candidate from the parsed graph. `Err` carries the typed
    /// refusal; a refused candidate pins nothing.
    pub fn from_candidate(c: &TrustRootCandidate) -> Result<AuthorityTrustRoot, OriginRefusal> {
        AuthorityTrustRoot::new(
            c.node.clone(),
            c.authority_iri.clone(),
            c.admission_digest.clone(),
            c.source_locator.clone(),
        )
    }
}

/// The pin map of a graph's well-formed trust roots:
/// `authority_iri -> digest`. An IRI pinned to two different digests is
/// ambiguous and pins nothing (fail closed). Malformed trust roots (bad
/// digest or refused locator) pin nothing.
pub fn pins(graph: &AuthorityGraph) -> BTreeMap<String, String> {
    let mut by_iri: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for c in &graph.trust_roots {
        if let Ok(root) = AuthorityTrustRoot::from_candidate(c) {
            by_iri
                .entry(root.authority_iri)
                .or_default()
                .push(root.admission_digest);
        }
    }
    by_iri
        .into_iter()
        .filter_map(|(iri, digests)| {
            let mut uniq = digests;
            uniq.sort();
            uniq.dedup();
            match uniq.as_slice() {
                [digest] => Some((iri, digest.to_string())),
                _ => None, // ambiguous or (impossibly) empty
            }
        })
        .collect()
}

/// The typed origin refusal vocabulary (authority.ex `origin_refusal`).
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum OriginRefusal {
    #[error("AuthorityNotAdmitted: {iri} — remediation: typed is not admitted; the node \
             must carry exactly one sj:admissionDigest (sha256:<64 hex>) in the canonical \
             authority graph")]
    AuthorityNotAdmitted { iri: String },

    #[error("AuthorityTypeMismatch: {iri} — remediation: prose classes \
             (sj:Proposition, sj:ProseObservation) are never code-work authority \
             (ADR-012)")]
    AuthorityTypeMismatch { iri: String },

    #[error("AuthorityDigestMismatch: {iri} — remediation: the stated sj:admissionDigest \
             does not recompute byte-equal over the node description with all \
             admissionDigest predicates excluded (edited node under a kept digest, \
             multiple digests, or a placeholder)")]
    AuthorityDigestMismatch { iri: String },

    #[error("AuthorityNotPinned: {iri} — remediation: a recomputing digest is an UNKEYED \
             content hash; the (iri, digest) pair must be pinned by an \
             sj:AuthorityTrustRoot under a durable locator")]
    AuthorityNotPinned { iri: String },

    #[error("AuthorityDigestInvalid: {iri} — remediation: digests must be \
             `sha256:<64 lowercase hex>`")]
    AuthorityDigestInvalid { iri: String },

    #[error("AuthorityIndexConflict: {iri} — remediation: the index carries both an \
             admitted and a refused entry for this IRI; rebuild the index")]
    AuthorityIndexConflict { iri: String },

    #[error("AuthorityIndexUnavailable: path {path:?}: {detail} — remediation: the \
             authority source must be readable and parse; resolution fails closed")]
    AuthorityIndexUnavailable { path: Option<String>, detail: String },
}

/// The type-bearing admitted entry of an authority index.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedAuthority {
    pub iri: String,
    pub admission_digest: String,
    pub authority_type: AuthorityType,
}

impl fmt::Display for ResolvedAuthority {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({}, {})", self.iri, self.authority_type, self.admission_digest)
    }
}

/// Judgment of one typed authority node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Judged {
    Admitted(ResolvedAuthority),
    Refused(OriginRefusal),
}

/// The admission index of an authority graph (authority.ex `index/1`): every
/// typed authority node is either admitted or refused with its typed refusal.
/// An IRI in neither map is not an authority node at all.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct AuthorityIndex {
    pub admitted: BTreeMap<String, ResolvedAuthority>,
    pub refused: BTreeMap<String, OriginRefusal>,
}

/// The admission digest of authority node `node` — the self-exclusion law.
///
/// sha256 over `code-work-authority:v1\n<iri>\n` plus the sorted
/// `predicate object` lines of the node's description with ALL
/// `admissionDigest` entries excluded, each line newline-terminated. Stamping
/// is idempotent under this construction: recomputing over a stamped graph
/// returns the same digest, so a node cannot change its own digest by stating
/// it (self-declaration defeated). The returned digest carries the
/// `sha256:` prefix — the wire form the SHACL pattern and every stated
/// `sj:admissionDigest` use.
pub fn admission_digest(node: &AuthorityNode) -> String {
    let mut lines: Vec<String> = Vec::new();
    for t in &node.types {
        lines.push(format!("rdf:type sj:{t}"));
    }
    for (predicate, objects) in &node.statements {
        for object in objects {
            lines.push(format!("{predicate} {object}"));
        }
    }
    lines.retain(|l| !l.starts_with("admissionDigest "));
    lines.sort();
    let body: String = lines.iter().map(|l| format!("{l}\n")).collect();
    format!(
        "sha256:{}",
        sha256_hex(&format!(
            "{ADMISSION_DIGEST_CONTEXT}\n{}\n{body}",
            node.iri
        ))
    )
}

impl AuthorityIndex {
    /// The admission index of `graph` — every typed authority node judged:
    ///
    /// - also typed a prose class → refused `AuthorityTypeMismatch`;
    /// - no `admissionDigest` → refused `AuthorityNotAdmitted` (typed is not
    ///   admitted);
    /// - more than one digest, an ill-formed digest, or a digest that does
    ///   not RECOMPUTE byte-equal → refused `AuthorityDigestMismatch`;
    /// - otherwise admitted with its recomputed digest and authority type.
    pub fn build(graph: &AuthorityGraph) -> AuthorityIndex {
        let mut index = AuthorityIndex::default();
        for node in &graph.authorities {
            if !node.types.iter().any(|t| AUTHORITY_CLASSES.contains(&t.as_str())) {
                continue; // not an authority node at all
            }
            let judged = judge(node);
            match judged {
                Judged::Admitted(a) => {
                    index.admitted.insert(node.iri.clone(), a);
                }
                Judged::Refused(r) => {
                    index.refused.insert(node.iri.clone(), r);
                }
            }
        }
        index
    }

    /// Builds the index and applies the graph's own well-formed trust-root
    /// pins — the default full admission path.
    pub fn build_pinned(graph: &AuthorityGraph) -> AuthorityIndex {
        let index = AuthorityIndex::build(graph);
        index.apply_pins(&pins(graph))
    }

    /// Applies the trust-root pin law: an admitted entry stays admitted only
    /// when its `(iri, digest)` pair is exactly pinned; otherwise it moves to
    /// `refused` as `AuthorityNotPinned`. Already-refused entries keep their
    /// refusal.
    pub fn apply_pins(mut self, pins: &BTreeMap<String, String>) -> AuthorityIndex {
        let unpinned: Vec<String> = self
            .admitted
            .iter()
            .filter(|(iri, a)| pins.get(*iri).map(|d| d.as_str()) != Some(a.admission_digest.as_str()))
            .map(|(iri, _)| iri.clone())
            .collect();
        for iri in unpinned {
            if self.admitted.remove(&iri).is_some() {
                self.refused.insert(
                    iri.clone(),
                    OriginRefusal::AuthorityNotPinned { iri },
                );
            }
        }
        self
    }

    /// Resolves `origin` against the index: `Ok` when admitted, the node's
    /// typed refusal when it is a refused authority node, and
    /// `AuthorityNotAdmitted` when the IRI is not an authority node at all
    /// (a proposition, a prose observation, a fresh IRI). An IRI in both maps
    /// is an index conflict (fail closed).
    pub fn resolve(&self, origin: &str) -> Result<&ResolvedAuthority, OriginRefusal> {
        let admitted = self.admitted.get(origin);
        let refused = self.refused.get(origin);
        match (admitted, refused) {
            (Some(_), Some(_)) => Err(OriginRefusal::AuthorityIndexConflict {
                iri: origin.to_string(),
            }),
            (Some(a), None) => Ok(a),
            (None, Some(r)) => Err(r.clone()),
            (None, None) => Err(OriginRefusal::AuthorityNotAdmitted {
                iri: origin.to_string(),
            }),
        }
    }
}

fn judge(node: &AuthorityNode) -> Judged {
    if node.types.iter().any(|t| PROSE_CLASSES.contains(&t.as_str())) {
        return Judged::Refused(OriginRefusal::AuthorityTypeMismatch { iri: node.iri.clone() });
    }
    let stated: Vec<&String> = node
        .statements
        .get("admissionDigest")
        .map(|v| v.iter().collect())
        .unwrap_or_default();
    match stated.len() {
        0 => Judged::Refused(OriginRefusal::AuthorityNotAdmitted { iri: node.iri.clone() }),
        1 => {
            let s = stated[0].as_str();
            if !valid_digest(s) || *s != admission_digest(node) {
                Judged::Refused(OriginRefusal::AuthorityDigestMismatch { iri: node.iri.clone() })
            } else {
                let authority_type = if node.types.iter().any(|t| t == "StrategicObjective") {
                    AuthorityType::StrategicObjective
                } else {
                    AuthorityType::GoalCheckpoint
                };
                Judged::Admitted(ResolvedAuthority {
                    iri: node.iri.clone(),
                    admission_digest: s.to_string(),
                    authority_type,
                })
            }
        }
        _ => Judged::Refused(OriginRefusal::AuthorityDigestMismatch { iri: node.iri.clone() }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn node(iri: &str, types: &[&str], statements: &[(&str, &[&str])]) -> AuthorityNode {
        let mut statements_map: BTreeMap<String, Vec<String>> = BTreeMap::new();
        for (p, objs) in statements {
            statements_map.insert(
                p.to_string(),
                objs.iter().map(|s| s.to_string()).collect(),
            );
        }
        AuthorityNode {
            iri: iri.to_string(),
            types: types.iter().map(|s| s.to_string()).collect(),
            statements: statements_map,
        }
    }

    #[test]
    fn stamping_is_idempotent() {
        let n = node("https://graph.test/auth/x1", &["StrategicObjective"], &[
            ("sj:title", &["Ship the kernel"]),
        ]);
        let d1 = admission_digest(&n);
        let mut stamped = n.clone();
        stamped
            .statements
            .insert("admissionDigest".to_string(), vec![d1.clone()]);
        assert_eq!(admission_digest(&stamped), d1);
    }

    #[test]
    fn locator_grammar() {
        assert!(matches!(
            SourceLocator::parse("git:owner/repo@0123456789abcdef0123456789abcdef01234567:priv/ontology.ttl"),
            Ok(SourceLocator::Git { .. })
        ));
        assert!(matches!(
            SourceLocator::parse("git-notes:refs/notes/authors@0123456789abcdef0123456789abcdef01234567"),
            Ok(SourceLocator::GitNotes { .. })
        ));
        assert!(matches!(
            SourceLocator::parse("https://graph.test/ontology.ttl"),
            Ok(SourceLocator::Https(_))
        ));
        // scratch/tmp/~/absolute refusals
        for bad in [
            "/tmp/ontology.ttl",
            "~/ontology.ttl",
            "git:owner/repo@0123456789abcdef0123456789abcdef01234567:/tmp/x.ttl",
            "git:owner/repo@0123456789abcdef0123456789abcdef01234567:~/x.ttl",
            "git:owner/repo@0123456789abcdef0123456789abcdef01234567:scratch/x.ttl",
            "file:///etc/ontology.ttl",
            "git:owner/repo@nothex:priv/x.ttl",
        ] {
            assert!(
                matches!(
                    SourceLocator::parse(bad),
                    Err(LocatorRefusal::NotDurable(_)) | Err(LocatorRefusal::Malformed(_))
                ),
                "expected refusal for {bad}"
            );
        }
    }
}
