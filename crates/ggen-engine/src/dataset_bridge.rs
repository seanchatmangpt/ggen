//! Receipt→dataset bridge (v26.10.20 dialect slice 2, lane G15).
//!
//! OPT-IN projection of sync receipt identity facts into V17's
//! [`ggen_graph::DatasetStore`] intent-segregated store. No existing sync or
//! receipt path calls into this module; a consumer that never constructs a
//! `DatasetStore` observes zero behavior change (the module owns no state and
//! mutates nothing outside the store it is handed).
//!
//! # In-module vocabulary (small, closed)
//!
//! Subject: `urn:ggen:receipt:<chain_hash_hex>` — one node per receipt,
//! minted from the chain hash so the RDF identity and the receipt-chain
//! identity cannot drift apart.
//!
//! Predicate namespace: `urn:ggen:proof#` — exactly three predicates:
//!
//! | Predicate              | Object                          |
//! |------------------------|---------------------------------|
//! | `urn:ggen:proof#chainHash`  | the BLAKE3 chain hash hex (plain literal) |
//! | `urn:ggen:proof#generation` | the monotonic generation epoch, when the receipt carries one (plain literal of the integer) |
//! | `urn:ggen:proof#standing`   | the RFC §82 standing string (plain literal) |
//!
//! All triples land in the [`ggen_graph::IntentGraph::ReceiptProof`] intent
//! graph via [`DatasetStore::insert_quad`], which rewrites the graph name to
//! the intent's IRI — the bridge can never scatter facts across intents by
//! construction.
//!
//! Standing is NOT synthesized here: a [`SyncReceipt`] carries no standing
//! field (standing lives on the envelope, RFC §82), so the caller supplies
//! it explicitly and the bridge refuses to invent one.

use crate::sync::SyncReceipt;
use ggen_graph::{CrossGraphLeak, DatasetError, DatasetStore, IntentGraph};

/// Predicate namespace for the receipt-proof vocabulary (see module docs).
pub const PROOF_NS: &str = "urn:ggen:proof#";

/// Receipt identity facts projected into the `ReceiptProof` intent graph.
///
/// This is the "ReceiptPayload-like" seam of the bridge: any producer that can
/// name a subject, a chain hash, a generation and a standing can project —
/// the bridge does not depend on the full wire shape of `SyncReceipt`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReceiptProofFacts {
    /// Subject IRI/URN of the receipt node (e.g. `urn:ggen:receipt:<hash>`).
    pub subject: String,
    /// BLAKE3 chain hash hex of the receipt record.
    pub chain_hash: String,
    /// Monotonic generation epoch, when the receipt carries one.
    pub generation: Option<u64>,
    /// RFC §82 standing string (caller-supplied; never synthesized).
    pub standing: String,
}

impl ReceiptProofFacts {
    /// Derive facts from a real sync receipt; `standing` is supplied by the
    /// caller because the receipt itself reports none (envelope concern).
    #[must_use]
    pub fn from_sync_receipt(receipt: &SyncReceipt, standing: impl Into<String>) -> Self {
        Self {
            subject: format!("urn:ggen:receipt:{}", receipt.record.chain_hash_hex),
            chain_hash: receipt.record.chain_hash_hex.clone(),
            generation: receipt.payload.generation,
            standing: standing.into(),
        }
    }
}

/// Project one receipt's identity triples into the `ReceiptProof` intent graph.
///
/// Idempotent per (subject, predicate): re-projecting the same facts inserts
/// nothing new (Oxigraph quads are a set).
///
/// # Errors
///
/// [`DatasetError`] on store insertion failure (the intent check cannot fail
/// here — the bridge always targets its own intent).
pub fn project_receipt_proof(
    store: &mut DatasetStore, facts: &ReceiptProofFacts,
) -> Result<(), DatasetError> {
    let subject = oxigraph::model::NamedNode::new_unchecked(facts.subject.clone());
    let insert = |pred: &str, obj: String| {
        let quad = oxigraph::model::Quad::new(
            subject.clone(),
            oxigraph::model::NamedNode::new_unchecked(format!("{PROOF_NS}{pred}")),
            oxigraph::model::Literal::new_simple_literal(obj),
            oxigraph::model::GraphName::DefaultGraph,
        );
        store.insert_quad(IntentGraph::ReceiptProof, &quad)
    };
    insert("chainHash", facts.chain_hash.clone())?;
    if let Some(gen) = facts.generation {
        insert("generation", gen.to_string())?;
    }
    insert("standing", facts.standing.clone())?;
    Ok(())
}

/// Result of a passing [`audit_isolation`] sweep.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IsolationReport {
    /// Distinct subjects observed in the `ReceiptProof` intent graph.
    pub proof_subjects: usize,
    /// Distinct subjects observed in the `SystemState` intent graph.
    pub state_subjects: usize,
}

/// Typed isolation refusal: a `ReceiptProof` subject also appears as a
/// `SystemState` subject.
///
/// Carries V17's [`CrossGraphLeak`] (authority `SystemState` witnessing intent,
/// target `ReceiptProof`) plus the offending subject — the one datum
/// [`CrossGraphLeak`] itself cannot name, since V17's leak check is a pure
/// intent-pair predicate.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[error(
    "isolation leak refused: subject {subject} appears in both {authority} and {target} intent graphs",
    authority = leak.authority.tag(),
    target = leak.target.tag()
)]
pub struct IsolationLeak {
    /// V17's intent-pair refusal (`SystemState` → `ReceiptProof`).
    pub leak: CrossGraphLeak,
    /// The subject shared across both intent graphs.
    pub subject: String,
}

/// Assert no triple in `ReceiptProof` shares a subject with any `SystemState`
/// quad.
///
/// Reuses V17's leak semantics: the two intents are disjoint authority
/// surfaces, so a shared subject is a cross-graph leak even though each
/// individual write was intent-authorized.
///
/// # Errors
///
/// [`IsolationLeak`] naming the shared subject and both intents;
/// [`DatasetError::Store`] (wrapped) on query failure.
pub fn audit_isolation(store: &DatasetStore) -> Result<IsolationReport, BridgeError> {
    let state_subjects = subject_set(store.quads_for(IntentGraph::SystemState)?);
    let proof_subjects = subject_set(store.quads_for(IntentGraph::ReceiptProof)?);
    if let Some(shared) = proof_subjects.iter().find(|s| state_subjects.contains(*s)) {
        return Err(BridgeError::Leak(IsolationLeak {
            leak: CrossGraphLeak {
                authority: IntentGraph::SystemState,
                target: IntentGraph::ReceiptProof,
            },
            subject: shared.clone(),
        }));
    }
    Ok(IsolationReport {
        proof_subjects: proof_subjects.len(),
        state_subjects: state_subjects.len(),
    })
}

/// Errors surfaced by the bridge.
#[derive(Debug, thiserror::Error)]
pub enum BridgeError {
    /// A `ReceiptProof` subject leaked into `SystemState` (or vice versa).
    #[error(transparent)]
    Leak(#[from] IsolationLeak),
    /// Underlying store failure.
    #[error(transparent)]
    Store(#[from] DatasetError),
}

#[must_use]
fn subject_set(quads: Vec<oxigraph::model::Quad>) -> std::collections::BTreeSet<String> {
    let mut set = std::collections::BTreeSet::new();
    for q in quads {
        match q.subject {
            oxigraph::model::NamedOrBlankNode::NamedNode(n) => {
                set.insert(n.as_str().to_string());
            }
            oxigraph::model::NamedOrBlankNode::BlankNode(b) => {
                set.insert(format!("_:{b}"));
            }
        }
    }
    set
}
