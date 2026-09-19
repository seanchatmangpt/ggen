//! Portable receipt envelope — RFC-GPACK-001 §54/§55 (v26.9.17).
//!
//! Every non-dry-run frontmatter-schema sync writes one envelope at
//! `<root>/.ggen-v2/receipt-portable.json` alongside the existing BLAKE3
//! chain receipt (`.ggen-v2/receipt.json`, untouched — RFC §56: the Rust
//! BLAKE3 chains continue; the portable envelope is a separate file and the
//! two formats are NOT byte-equivalent).
//!
//! The envelope REPORTS standing, it does not confer it (RFC §57): a
//! completed sync whose consequences were re-hashed off disk reports
//! `ALIVE`; a sync that landed only some of its writes reports
//! `PARTIAL_ALIVE`; a gate-refused sync reports the typed `REFUSED:<CODE>`
//! (RFC §82 vocabulary only, states are never collapsed — §82).
//!
//! Fields this run cannot know are present with `"UNKNOWN"` rather than
//! omitted (RFC §54 "binds at least"): replay is never executed by this
//! engine, so `replay.status` is always `"UNKNOWN"`, and a sync with no
//! resolved pack reports `UNKNOWN` subject identity instead of inventing
//! one. Declared pack dependencies are resolved fail-closed and the portable
//! envelope binds the subject's exact transitive dependency closure by name,
 //! version, digest, and the currently indivisible SEMANTICS/LAW/PROJECTION
//! pack surface.

use std::path::Path;

use serde::Serialize;

use crate::{
    error::{AppError, Result},
    pack::{dependency_scope, pack_digest_sha256, Pack, ScopeDepth},
};

/// Where the portable receipt envelope is written, relative to the project
/// root. Deliberately a sibling of [`crate::sync::RECEIPT_REL_PATH`] under
/// `.ggen-v2/` (already watcher-ignored, like the legacy receipt).
pub const PORTABLE_RECEIPT_REL_PATH: &str = ".ggen-v2/receipt-portable.json";

/// The envelope schema URI (RFC §55).
pub const PORTABLE_RECEIPT_SCHEMA: &str = "https://ggen.dev/receipt/pack/v1";

/// The spec revision this envelope reports against (RFC §55).
pub const PORTABLE_RECEIPT_SPEC: &str = "RFC-GPACK-001-v26.9.17";

/// Optional canonical semantic work-order input. When present it is parsed as
/// Turtle and bound by deterministic graph state hash; when absent the receipt
/// reports UNKNOWN rather than inventing a ticket identity.
pub const WORK_ORDER_REL_PATH: &str = "work-order.ttl";

/// §82 standing value for a fully-landed manufacture.
const STANDING_ALIVE: &str = "ALIVE";

/// §82 standing value for a run that landed only part of its writes.
const STANDING_PARTIAL_ALIVE: &str = "PARTIAL_ALIVE";

/// The standing the envelope reports for this run (RFC §82 vocabulary only;
/// `REFUSED(code)` renders as `REFUSED:<code>`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PortableStanding {
    /// Every admitted write landed.
    Alive,
    /// The run stopped early: some writes landed, some did not.
    PartialAlive,
    /// Admission refused the sync; the payload is the typed refusal
    /// identity (e.g. `GATE_VIOLATION`).
    Refused(String),
}

impl std::fmt::Display for PortableStanding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Alive => f.write_str(STANDING_ALIVE),
            Self::PartialAlive => f.write_str(STANDING_PARTIAL_ALIVE),
            Self::Refused(code) => write!(f, "REFUSED:{code}"),
        }
    }
}

/// Engine identity (RFC §54: engine identity + version).
#[derive(Debug, Clone, Serialize)]
pub struct PortableEngine {
    /// Engine name.
    pub name: &'static str,
    /// Engine version (`CARGO_PKG_VERSION` at compile time).
    pub version: &'static str,
}

/// The manufactured subject (RFC §55 `subject`).
#[derive(Debug, Clone, Serialize)]
pub struct PortableSubject {
    /// Canonical pack name. `"UNKNOWN"` when the run resolved no pack.
    pub pack: String,
    /// Pack version. `"UNKNOWN"` when the run resolved no pack.
    pub version: String,
    /// RFC §38 portable SHA-256 pack digest, `sha256:<hex>`-prefixed.
    /// `"UNKNOWN"` when the run resolved no pack.
    pub pack_digest: String,
}

/// One declared dependency (RFC §26 scope algebra).
///
/// The current Rust pack format composes ontology semantics, law inputs, and
/// projection templates as one unit, so a declared dependency carries all
/// three surfaces. A future narrower manifest may reduce that set, but this
/// engine never invents a narrower claim than it actually consumes.
#[derive(Debug, Clone, Serialize)]
pub struct PortableDependency {
    /// Dependency pack name.
    pub name: String,
    /// Exact resolved version (RFC §32: constraints are not replay identity).
    pub version: String,
    /// Exact resolved content digest (`sha256:<hex>`).
    pub digest: String,
    /// Required surfaces (RFC §26: subset of SEMANTICS/LAW/PROJECTION).
    pub scope: Vec<&'static str>,
}

/// Admitted graph identity (RFC §55 `graph`).
#[derive(Debug, Clone, Serialize)]
pub struct PortableGraph {
    /// Hash of the post-Enrich canonical graph state this run rendered from
    /// (the same value the legacy receipt chains over). `"UNKNOWN"` when the
    /// state hash could not be computed (refusal-envelope best effort).
    pub canonical_digest: String,
}

/// Admission evidence (RFC §14/§15: `GatePass = AttemptObserved ∧
/// ViolationAbsent`; a gate that never executed is never reported passed).
#[derive(Debug, Clone, Serialize)]
pub struct PortableAdmission {
    /// Gate identities actually evaluated this run, in evaluation order.
    pub gates_attempted: Vec<String>,
    /// Typed refusal identities (RFC Appendix C vocabulary), empty on a
    /// passing run.
    pub refusals: Vec<String>,
}

/// One manufactured consequence (RFC §55 `consequences`).
#[derive(Debug, Clone, Serialize)]
pub struct PortableConsequence {
    /// Root-relative target path.
    pub target: String,
    /// Write operation performed (`MANAGED_WRITE` / `MANAGED_INJECT`).
    pub operation: &'static str,
    /// SHA-256 hex of the target's bytes as observed on disk after the write.
    pub sha256: String,
}

/// Replay relation (RFC §53/§55). This engine does not execute replay yet.
#[derive(Debug, Clone, Serialize)]
pub struct PortableReplay {
    /// UNKNOWN on the first observed run; MATCH when the previous portable
    /// envelope has the same replay-binding projection; MISMATCH otherwise.
    pub status: String,
}

/// Optional admitted semantic work-order identity. Markdown/WBPR/Vision are
/// projections; this graph digest is the machine identity when the TTL exists.
#[derive(Debug, Clone, Serialize)]
pub struct PortableWorkOrder {
    /// Root-relative source path, or UNKNOWN when no semantic work order exists.
    pub source: String,
    /// Deterministic graph state hash of the Turtle work order, or UNKNOWN.
    pub canonical_digest: String,
}

/// The complete portable receipt envelope (RFC §54 field set, §55 shape).
#[derive(Debug, Clone, Serialize)]
pub struct PortableReceiptEnvelope {
    /// Envelope schema URI.
    pub schema: &'static str,
    /// Spec revision.
    pub spec: &'static str,
    /// Engine identity.
    pub engine: PortableEngine,
    /// Manufactured subject.
    pub subject: PortableSubject,
    /// Exact declared transitive dependency closure for the receipt subject.
    pub dependencies: Vec<PortableDependency>,
    /// Admitted graph identity.
    pub graph: PortableGraph,
    /// Admission evidence.
    pub admission: PortableAdmission,
    /// Manufactured consequences, target-sorted.
    pub consequences: Vec<PortableConsequence>,
    /// Optional semantic work-order identity driving this manufacture.
    pub work_order: PortableWorkOrder,
    /// Replay relation.
    pub replay: PortableReplay,
    /// Reported standing (§82 vocabulary).
    pub standing: String,
}

/// Multi-decision mapping: which legacy decision prefix maps to which
/// portable operation string. Only actual writes are consequences; skips are
/// not manufactured output.
fn operation_for_decision(decision: &str) -> Option<&'static str> {
    if decision == "written" || decision.starts_with("written;") {
        Some("MANAGED_WRITE")
    } else if decision == "injected" || decision.starts_with("injected;") {
        Some("MANAGED_INJECT")
    } else {
        None
    }
}

/// Build the consequence list for `decisions`, hashing every landed target
/// straight off disk (the same re-hash discipline
/// `crate::sync::write_receipt` applies — the envelope binds what is on
/// disk, not what the renderer claimed).
///
/// # Errors
/// Fail closed (typed `[FM-CHAIN-015]`) when a decision recorded as landed
/// can no longer be read: silently omitting it would unbind the consequence
/// from the envelope.
fn build_consequences(
    root: &Path, decisions: &std::collections::BTreeMap<String, String>,
) -> Result<Vec<PortableConsequence>> {
    use sha2::Digest as _;

    let mut consequences = Vec::new();
    for (target, decision) in decisions {
        let Some(operation) = operation_for_decision(decision) else {
            continue;
        };
        let path = root.join(target);
        let bytes = std::fs::read(&path).map_err(|e| {
            AppError::fm_chain(
                15,
                format!(
                    "portable envelope: consequence `{target}` (decision \
                     `{decision}`) cannot be read for digest binding: {e}. \
                     Remediation: fix the file's permissions."
                ),
            )
        })?;
        let digest = sha2::Sha256::digest(&bytes);
        consequences.push(PortableConsequence {
            target: target.clone(),
            operation,
            sha256: hex::encode(digest),
        });
    }
    Ok(consequences)
}

/// Build the subject block: the single resolved pack when exactly one was
/// resolved; the lexicographically-first pack for multi-pack runs (the
/// envelope is one-per-sync until declared dependency resolution exists);
/// all-`UNKNOWN` for a packless run (present, never invented).
///
/// # Errors
/// Propagates [`pack_digest_sha256`] failures.
fn build_subject(packs: &[Pack]) -> Result<PortableSubject> {
    match packs.first() {
        Some(pack) => {
            let digest = pack_digest_sha256(pack)?;
            Ok(PortableSubject {
                pack: pack.name.clone(),
                version: pack.version.clone(),
                pack_digest: format!("sha256:{}", crate::sync::hex32(&digest)),
            })
        }
        None => Ok(PortableSubject {
            pack: "UNKNOWN".to_string(),
            version: "UNKNOWN".to_string(),
            pack_digest: "UNKNOWN".to_string(),
        }),
    }
}

/// Build the exact declared dependency closure for the receipt subject.
///
/// The receipt subject remains the first resolved pack for compatibility with
/// the existing one-envelope-per-sync shape. Only packs reachable from that
/// subject by declared dependency edges are reported here; unrelated
/// top-level packs are not mislabeled as dependencies.
fn build_dependencies(packs: &[Pack]) -> Result<Vec<PortableDependency>> {
    let Some(subject) = packs.first() else {
        return Ok(Vec::new());
    };
    let scoped = dependency_scope(packs, &subject.name, ScopeDepth::Transitive)?;
    let mut dependencies = Vec::with_capacity(scoped.len().saturating_sub(1));
    for pack in scoped.into_iter().skip(1) {
        let digest = pack_digest_sha256(pack)?;
        dependencies.push(PortableDependency {
            name: pack.name.clone(),
            version: pack.version.clone(),
            digest: format!("sha256:{}", crate::sync::hex32(&digest)),
            scope: vec!["SEMANTICS", "LAW", "PROJECTION"],
        });
    }
    Ok(dependencies)
}

/// Resolve the optional semantic work-order identity. The graph is parsed by
/// the same deterministic RDF engine used elsewhere in ggen, so whitespace or
/// triple ordering cannot become a second identity.
fn build_work_order(root: &Path) -> Result<PortableWorkOrder> {
    let path = root.join(WORK_ORDER_REL_PATH);
    if !path.is_file() {
        return Ok(PortableWorkOrder {
            source: "UNKNOWN".to_string(),
            canonical_digest: "UNKNOWN".to_string(),
        });
    }

    let ttl = std::fs::read_to_string(&path)?;
    let graph = crate::graph::DeterministicGraph::new()?;
    graph.insert_turtle(&ttl)?;
    let hash = graph.state_hash()?;
    Ok(PortableWorkOrder {
        source: WORK_ORDER_REL_PATH.to_string(),
        canonical_digest: crate::sync::hex32(&hash),
    })
}

/// Projection that defines replay identity. Standing and replay status are
/// deliberately excluded: replay is evidence about whether the same admitted
/// subject/input/effect relation was reconstructed, not a recursive comparison
/// of the verdict with itself.
fn replay_projection(value: &serde_json::Value) -> serde_json::Value {
    serde_json::json!({
        "engine": value.get("engine"),
        "subject": value.get("subject"),
        "dependencies": value.get("dependencies"),
        "graph": value.get("graph"),
        "work_order": value.get("work_order"),
        "admission": value.get("admission"),
        "consequences": value.get("consequences"),
    })
}

fn previous_replay_status(path: &Path, current: &PortableReceiptEnvelope) -> String {
    let previous = std::fs::read_to_string(path)
        .ok()
        .and_then(|raw| serde_json::from_str::<serde_json::Value>(&raw).ok());
    let Some(previous) = previous else {
        return "UNKNOWN".to_string();
    };
    let Ok(current) = serde_json::to_value(current) else {
        return "UNKNOWN".to_string();
    };
    if replay_projection(&previous) == replay_projection(&current) {
        "MATCH".to_string()
    } else {
        "MISMATCH".to_string()
    }
}

/// Assemble and write the portable receipt envelope for one sync run.
///
/// `gates_attempted` must list every gate identity actually evaluated (in
/// order); `refusals` the typed refusal identities (empty on success);
/// `graph_hash_hex` the post-Enrich graph state hash (`"UNKNOWN"` when it
/// could not be computed); `decisions` the run's write decisions (empty for
/// a refused run — gates precede all writes).
///
/// # Errors
/// `[FM-CHAIN-015]` when a landed consequence cannot be read, or I/O /
/// serialization failures while persisting the envelope.
pub fn write_portable_envelope(
    root: &Path, packs: &[Pack], graph_hash_hex: &str, gates_attempted: &[String],
    refusals: &[String], standing: &PortableStanding,
    decisions: &std::collections::BTreeMap<String, String>,
) -> Result<()> {
    let envelope = PortableReceiptEnvelope {
        schema: PORTABLE_RECEIPT_SCHEMA,
        spec: PORTABLE_RECEIPT_SPEC,
        engine: PortableEngine {
            name: "ggen",
            version: env!("CARGO_PKG_VERSION"),
        },
        subject: build_subject(packs)?,
        dependencies: build_dependencies(packs)?,
        graph: PortableGraph {
            canonical_digest: graph_hash_hex.to_string(),
        },
        admission: PortableAdmission {
            gates_attempted: gates_attempted.to_vec(),
            refusals: refusals.to_vec(),
        },
        consequences: build_consequences(root, decisions)?,
        work_order: build_work_order(root)?,
        replay: PortableReplay {
            status: "UNKNOWN".to_string(),
        },
        standing: standing.to_string(),
    };

    let path = root.join(PORTABLE_RECEIPT_REL_PATH);
    let mut envelope = envelope;
    envelope.replay.status = previous_replay_status(&path, &envelope);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(&path, serde_json::to_vec(&envelope)?)?;
    Ok(())
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;

    /// A decision that is not an actual write is not a consequence: skips,
    /// plans, and unknown decision strings map to no portable operation.
    #[test]
    fn only_landed_writes_are_consequences() {
        assert_eq!(operation_for_decision("written"), Some("MANAGED_WRITE"));
        assert_eq!(
            operation_for_decision("written; match=x"),
            Some("MANAGED_WRITE")
        );
        assert_eq!(operation_for_decision("injected"), Some("MANAGED_INJECT"));
        assert_eq!(operation_for_decision("skipped: when guard false"), None);
        assert_eq!(operation_for_decision("planned: write (dry-run)"), None);
        assert_eq!(operation_for_decision(""), None);
    }

    /// §82: the standing vocabulary must not be collapsed — each variant
    /// renders to its exact §82 string, REFUSED carrying its typed code.
    #[test]
    fn standing_renders_exact_rfc82_vocabulary() {
        assert_eq!(PortableStanding::Alive.to_string(), "ALIVE");
        assert_eq!(PortableStanding::PartialAlive.to_string(), "PARTIAL_ALIVE");
        assert_eq!(
            PortableStanding::Refused("GATE_VIOLATION".to_string()).to_string(),
            "REFUSED:GATE_VIOLATION"
        );
    }

    /// A packless run reports an all-UNKNOWN subject — every §54 field is
    /// present, nothing is invented.
    #[test]
    fn packless_subject_is_unknown_not_invented() {
        let subject = build_subject(&[]).expect("subject");
        assert_eq!(subject.pack, "UNKNOWN");
        assert_eq!(subject.version, "UNKNOWN");
        assert_eq!(subject.pack_digest, "UNKNOWN");
    }
}
