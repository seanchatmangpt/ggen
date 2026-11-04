//! The admission controller's composition function (reverse-80/20 §5
//! synthesis, R80-14): the ordered, fail-closed gate pipeline that turns a
//! batch of proposed edits plus a WorkOrder payload into one
//! [`ControllerDecision`], journaling every outcome.
//!
//! Gate order (each gate halts early-return before the next; every outcome
//! is journaled):
//!
//! 1. **Authority pinning** — the proposal's `origin_authority` IRI must be
//!    pinned in the injected authority index. Not pinned ⇒
//!    [`Stage::Authority`] refusal; the blast radius is never computed and
//!    the sjira admission kernel is never invoked.
//! 2. **Blast radius** (R80-6 shape) — the transitive callers*/subtypes*
//!    radius of the edits' symbols is computed through the injected
//!    [`SymbolGraph`]; if any radius symbol's definition site lands on a
//!    protected critical surface, the proposal refuses at
//!    [`Stage::BlastRadius`] and the sjira admission kernel is never
//!    invoked. A depth-cap-truncated radius is a lower bound, so a
//!    truncated clean radius refuses fail-closed (PARTIAL cannot certify
//!    ALIVE).
//! 3. **sjira admission** — `crate::admit::admit_work_order` (typed
//!    refusals, standing-at-UNKNOWN law, authority stamped `NONE`).
//! 4. **Transition journaling** — every outcome (admission OR any gate
//!    refusal) is appended to the injected
//!    [`crate::receipts::TransitionJournal`] as a blake3-chained
//!    transition, so a refusal record is as tamper-evident as an admission
//!    record.
//!
//! Pure and IO-free: every collaborator is injected — [`SymbolGraph`] and
//! [`AuthorityPin`] as traits, the journal as `&mut TransitionJournal`.
//! The [`SymbolGraph`] shape mirrors the landed R80-6 gate in
//! `crates/ggen-engine/src/blast_radius.rs` (`SymbolGraph`,
//! `DEFAULT_PROTECTED_PATHS`) so the R80-3 SCIP index can implement this
//! seam unchanged. The trait is re-declared locally because ggen-sjira
//! must not depend on ggen-engine; seam shapes were verified against the
//! engine file this session (disclosed per the compile-freeze SLA).

use serde_json::Value;
use std::collections::{BTreeMap, HashSet};

use crate::admit::admit_work_order;
use crate::receipts::TransitionJournal;

/// Protected critical-system surfaces (mirrors
/// `crates/ggen-engine/src/blast_radius.rs::DEFAULT_PROTECTED_PATHS`).
pub const DEFAULT_PROTECTED_PATHS: &[&str] =
    &["ggen-engine/src/sync.rs", "ggen-abb-sbb/src/lib.rs"];

/// Default callers*/subtypes* closure depth cap (mirrors ggen-engine's
/// R80-6 gate).
pub const DEFAULT_MAX_DEPTH: usize = 16;

/// The symbol-graph seam (R80-6 / R80-3). Mirrors
/// `crates/ggen-engine/src/blast_radius.rs::SymbolGraph` so a SCIP index
/// (R80-3) implements this trait unchanged.
pub trait SymbolGraph {
    /// Symbols that reference `symbol` (callers* direction, one hop).
    fn callers_of(&self, symbol: &str) -> Vec<String>;
    /// Direct subtypes of `symbol`.
    fn subtypes_of(&self, symbol: &str) -> Vec<String>;
    /// Definition site of a symbol (for protected-path intersection).
    fn def_site(&self, symbol: &str) -> Option<SymbolLoc>;
}

/// A symbol's definition site.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolLoc {
    pub symbol: String,
    pub path: String,
}

/// The authority-index seam: which origin-authority IRIs are pinned, and
/// to what digest. Implemented for `BTreeMap<String, String>` — the exact
/// shape `crate::authority::pins` returns — so the landed authority index
/// plugs in directly.
pub trait AuthorityPin {
    fn pin_for(&self, iri: &str) -> Option<String>;
}

impl AuthorityPin for BTreeMap<String, String> {
    fn pin_for(&self, iri: &str) -> Option<String> {
        self.get(iri).cloned()
    }
}

/// One proposed edit in the controller's batch.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ControllerEdit {
    /// Target path of the edit (relative to the canonical checkout root).
    pub path: String,
    /// Symbols the edit touches, resolved by the planner.
    pub symbols: Vec<String>,
}

impl ControllerEdit {
    /// Convenience constructor.
    pub fn new(path: impl Into<String>, symbols: Vec<String>) -> Self {
        Self {
            path: path.into(),
            symbols,
        }
    }
}

/// Which ordered gate refused the proposal.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Stage {
    Authority,
    BlastRadius,
    Admission,
}

impl std::fmt::Display for Stage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stage::Authority => write!(f, "authority"),
            Stage::BlastRadius => write!(f, "blast-radius"),
            Stage::Admission => write!(f, "admission"),
        }
    }
}

/// The controller's verdict: one admitted proposal, or a typed refusal
/// naming the ordered gate that halted the pipeline. Every variant carries
/// the journal hash of its recorded transition plus the journal head after
/// recording — the receipt half (`R=receipt(A)`) of the composition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ControllerDecision {
    /// All gates passed; the order is admitted with authority stamped
    /// `NONE` (admission never creates authority).
    Admitted {
        order_digest: String,
        definition_digest: String,
        authority: String,
        radius_digest: String,
        journal_hash: String,
        journal_head: String,
    },
    /// A gate refused. `radius_digest` is `None` exactly when the radius
    /// was never computed (authority-stage refusals); the journal
    /// transition for the outcome carries the same `None`, so "radius
    /// never ran" is tamper-evident in the chain, not merely unstated.
    Refused {
        stage: Stage,
        detail: String,
        radius_digest: Option<String>,
        journal_hash: String,
        journal_head: String,
    },
}

/// Deterministic digest binding a refusal to the payload it refused:
/// blake3 over a stage tag plus the origin authority, so repeated refusals
/// of the same payload collide predictably. A refused order has no
/// admission snapshot digest to bind (admission never ran), so the
/// controller binds the refusal by this derived digest instead — the
/// refusal class is distinguishable from the `sha256:` admission-digest
/// class by its `blake3:` prefix.
fn refusal_digest(origin_authority: &str) -> String {
    let mut h = blake3::Hasher::new();
    h.update(b"controller-refusal");
    h.update(origin_authority.as_bytes());
    let hex = h.finalize().to_hex().to_string();
    format!("blake3:{}", hex)
}

/// The composition function (reverse-80/20 §5): run the ordered,
/// fail-closed gate pipeline over one proposal and journal the outcome.
///
/// `order` is the WorkOrder-shaped JSON payload admitted (or refused) at
/// [`Stage::Admission`]. Its `origin_authority` field is read raw by the
/// authority gate before any validation, fail-closed: a missing or
/// non-string field is treated as unpinned and refuses at
/// [`Stage::Authority`].
pub fn propose(
    edits: &[ControllerEdit],
    order: &Value,
    graph: &dyn SymbolGraph,
    authority: &dyn AuthorityPin,
    journal: &mut TransitionJournal,
    protected_paths: &[&str],
) -> ControllerDecision {
    // ---- Gate 1: authority pinning (before radius, before admission) ----
    let origin_authority = order
        .get("origin_authority")
        .and_then(|v| v.as_str())
        .unwrap_or("");
    let pin: Option<String> = authority.pin_for(origin_authority);
    if origin_authority.is_empty() || pin.is_none() {
        let detail = format!(
            "AuthorityNotPinned: origin_authority {:?} is not pinned in the authority \
             index — remediation: pin the origin-authority IRI in the authority index \
             before proposing edits bound to it; authority never flows without a pinned \
             index entry",
            origin_authority
        );
        let journal_hash = journal.append(
            refusal_digest(origin_authority),
            crate::receipts::Decision::Refused { detail: detail.clone() },
            None,
        );
        let journal_head = journal.head();
        return ControllerDecision::Refused {
            stage: Stage::Authority,
            detail,
            radius_digest: None,
            journal_hash,
            journal_head,
        };
    }

    // ---- Gate 2: blast radius (before sjira admission) ----
    let mut radius: BTreeMap<String, SymbolLoc> = BTreeMap::new();
    let mut truncated = false;
    let mut seen: HashSet<String> = HashSet::new();
    let mut frontier: Vec<(String, usize)> = Vec::new();
    for e in edits {
        for s in &e.symbols {
            if seen.insert(s.clone()) {
                frontier.push((s.clone(), 0));
            }
        }
    }
    while let Some((sym, depth)) = frontier.pop() {
        if depth >= DEFAULT_MAX_DEPTH {
            truncated = true;
            continue;
        }
        if let Some(loc) = graph.def_site(&sym) {
            radius.insert(sym.clone(), loc);
        }
        for caller in graph.callers_of(&sym) {
            if seen.insert(caller.clone()) {
                frontier.push((caller, depth + 1));
            }
        }
        for sub in graph.subtypes_of(&sym) {
            if seen.insert(sub.clone()) {
                frontier.push((sub, depth + 1));
            }
        }
    }

    // Deterministic radius digest: blake3 over the sorted symbol@path pairs
    // (BTreeMap iteration order is sorted, so the digest is stable).
    let radius_digest: String = {
        let mut hasher = blake3::Hasher::new();
        for (sym, loc) in &radius {
            let pair = format!("{}@{}\n", sym, loc.path);
            hasher.update(pair.as_bytes());
        }
        let hex = hasher.finalize().to_hex().to_string();
        format!("blake3:{}", hex)
    };

    let mut violations: Vec<String> = Vec::new();
    for loc in radius.values() {
        if protected_paths.contains(&loc.path.as_str()) {
            violations.push(loc.path.clone());
        }
    }
    if !violations.is_empty() {
        violations.sort();
        violations.dedup();
        let detail = format!(
            "CriticalPathRefusal: protected surface(s) [{}] inside the blast radius — \
             remediation: scope the edits off the critical path; the sjira admission \
             kernel is never invoked on a protected-surface proposal",
            violations.join(", ")
        );
        let journal_hash = journal.append(
            refusal_digest(origin_authority),
            crate::receipts::Decision::Refused { detail: detail.clone() },
            Some(radius_digest.clone()),
        );
        let journal_head = journal.head();
        return ControllerDecision::Refused {
            stage: Stage::BlastRadius,
            detail,
            radius_digest: Some(radius_digest),
            journal_hash,
            journal_head,
        };
    }

    if truncated {
        let detail = String::from(
            "RadiusTruncated: the callers*/subtypes* closure hit the depth cap before \
             covering the full radius — remediation: raise the depth cap \
             (DEFAULT_MAX_DEPTH) or narrow the edits' symbol set; a truncated radius is \
             a lower bound and cannot certify a clean verdict",
        );
        let journal_hash = journal.append(
            refusal_digest(origin_authority),
            crate::receipts::Decision::Refused { detail: detail.clone() },
            Some(radius_digest.clone()),
        );
        let journal_head = journal.head();
        return ControllerDecision::Refused {
            stage: Stage::BlastRadius,
            detail,
            radius_digest: Some(radius_digest),
            journal_hash,
            journal_head,
        };
    }

    // ---- Gate 3: sjira admission ----
    let result = admit_work_order(order);
    let order_digest: String = match &result {
        Ok(a) => a.work_order_digest.clone(),
        Err(_) => refusal_digest(origin_authority),
    };
    let outcome = match &result {
        Ok(a) => crate::receipts::Decision::Admitted {
            authority: a.authority.clone(),
        },
        Err(r) => crate::receipts::Decision::Refused {
            detail: r.to_string(),
        },
    };
    let journal_hash = journal.append(order_digest, outcome, Some(radius_digest.clone()));
    let journal_head = journal.head();

    match result {
        Ok(a) => ControllerDecision::Admitted {
            order_digest: a.work_order_digest.clone(),
            definition_digest: a.definition_digest.clone(),
            authority: a.authority.clone(),
            radius_digest,
            journal_hash,
            journal_head,
        },
        Err(r) => ControllerDecision::Refused {
            stage: Stage::Admission,
            detail: r.to_string(),
            radius_digest: Some(radius_digest),
            journal_hash,
            journal_head,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::receipts::{ChainError, GENESIS_PREV_HASH};
    use serde_json::json;

    fn pinned() -> BTreeMap<String, String> {
        let mut m = BTreeMap::new();
        m.insert(
            "https://graph.test/auth/T".to_string(),
            "sha256:aa".to_string(),
        );
        m
    }

    fn order() -> Value {
        json!({
            "identity": "T1",
            "title": "t",
            "description": "d",
            "subject": "src/t.rs",
            "repository": "owner/repo",
            "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            "standing": "UNKNOWN",
            "evidence_ceiling": "UNVERIFIED",
            "promotion_rule": "courts",
            "replay_identity": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "origin_authority": "https://graph.test/auth/T",
            "projections": ["receipt"],
            "required_courts": ["c"],
            "required_evidence": ["e"],
            "acceptance": ["a"],
            "falsifiers": ["f"]
        })
    }

    #[test]
    fn smoke() {
        let edits = vec![ControllerEdit::new("src/a.rs", vec!["sym_a".to_string()])];
        assert_eq!(edits.len(), 1);
        assert_eq!(DEFAULT_MAX_DEPTH, 16);
        assert_eq!(DEFAULT_PROTECTED_PATHS.len(), 2);
    }

    /// Tamper-evidence at depth, through the composed pipeline: the journal
    /// mutations are only possible in-crate (private field), so this lives
    /// here rather than in the integration test.
    #[test]
    fn composed_journal_tamper_detected_at_depth() {
        struct Empty;
        impl SymbolGraph for Empty {
            fn callers_of(&self, _s: &str) -> Vec<String> {
                Vec::new()
            }
            fn subtypes_of(&self, _s: &str) -> Vec<String> {
                Vec::new()
            }
            fn def_site(&self, _s: &str) -> Option<SymbolLoc> {
                None
            }
        }
        let mut journal = TransitionJournal::new();
        let pins = pinned();
        let e = [ControllerEdit::new("src/a.rs", vec!["s".to_string()])];
        let d1 = propose(&e, &order(), &Empty, &pins, &mut journal, DEFAULT_PROTECTED_PATHS);
        assert!(matches!(d1, ControllerDecision::Admitted { .. }));
        let d2 = propose(&e, &json!({}), &Empty, &pins, &mut journal, DEFAULT_PROTECTED_PATHS);
        assert!(matches!(
            d2,
            ControllerDecision::Refused {
                stage: Stage::Authority,
                ..
            }
        ));
        assert_eq!(journal.len(), 2);
        assert_eq!(journal.verify_chain(), Ok(()));

        // Tamper the middle transition's body.
        let mut tampered = journal.clone();
        tampered.transitions[1].work_order_digest = "blake3:dead".to_string();
        match tampered.verify_chain() {
            Err(ChainError::Tampered { index: 1, .. }) => {}
            other => panic!("expected Tampered at 1, got {other:?}"),
        }

        // Break linkage only.
        let mut broken = journal.clone();
        broken.transitions[1].prev_hash = GENESIS_PREV_HASH.to_string();
        match broken.verify_chain() {
            Err(ChainError::BrokenLink { index: 1, .. }) => {}
            other => panic!("expected BrokenLink at 1, got {other:?}"),
        }
    }
}
