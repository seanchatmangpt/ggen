//! Generated discriminating falsifiers from ontology-declared mutations.

use mfw_pcp_generated::receipts::{canonical_digest, CloseReceipt, OpenReceipt};
use mfw_pcp_generated::replay::{close_standing, verify_pair, ReplayError};

fn pair() -> (OpenReceipt, String, CloseReceipt) {
    let open = OpenReceipt { sequence: 1, action_digest: "action".into(), bundle_digest: "bundle".into(), previous_close_digest: None };
    let digest = canonical_digest(&open).expect("canonical open receipt");
    let close = CloseReceipt { sequence: 1, open_digest: digest.clone(), action_digest: "action".into(), bundle_digest: "bundle".into(), effect_succeeded: true, postcondition_satisfied: true, final_state_digest: "state-1".into() };
    (open, digest, close)
}

#[test]
fn mutated_open_reference_is_refused() { let (open, _, mut close) = pair(); close.open_digest = "mutated".into(); assert_eq!(verify_pair(&open, "real", &close), Err(ReplayError::OpenReferenceDrift)); }
#[test]
fn sequence_drift_is_refused() { let (open, digest, mut close) = pair(); close.sequence = 2; assert_eq!(verify_pair(&open, &digest, &close), Err(ReplayError::SequenceDrift)); }
#[test]
fn failed_effect_is_refused() { let (open, digest, mut close) = pair(); close.effect_succeeded = false; assert_eq!(close_standing(&open, &digest, &close, true), Err(ReplayError::EffectFailed)); }
#[test]
fn false_goal_is_refused() { let (open, digest, close) = pair(); assert_eq!(close_standing(&open, &digest, &close, false), Err(ReplayError::GoalNotSatisfied)); }

pub const ONTOLOGY_FALSIFIERS: &[(&str, &str)] = &[
    ("certificate_inventory_is_complete", "deleting one kind is refused"),
    ("receipt_pair_requires_exact_open_reference", "mutated open digest is refused"),
    ("receipt_pair_requires_equal_sequence", "different sequence is refused"),
    ("standing_requires_goal", "goal false is refused"),
    ("standing_requires_all_effects", "failed effect is refused"),
    ("bounded_is_not_exhausted", "bounded promotion to exhausted is refused"),
];
