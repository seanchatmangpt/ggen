//! Generated positive proofs from ontology-declared invariants.

use mfw_pcp_generated::certificates;
use mfw_pcp_generated::receipts::{canonical_digest, CloseReceipt, OpenReceipt};
use mfw_pcp_generated::replay::{close_standing, verify_pair};

#[test]
fn certificate_inventory_is_complete() { assert_eq!(certificates::ALL.len(), 10); }

#[test]
fn exact_receipt_pair_and_goal_close_standing() {
    let open = OpenReceipt { sequence: 1, action_digest: "action".into(), bundle_digest: "bundle".into(), previous_close_digest: None };
    let open_digest = canonical_digest(&open).expect("canonical open receipt");
    let close = CloseReceipt { sequence: 1, open_digest: open_digest.clone(), action_digest: "action".into(), bundle_digest: "bundle".into(), effect_succeeded: true, postcondition_satisfied: true, final_state_digest: "state-1".into() };
    assert_eq!(verify_pair(&open, &open_digest, &close), Ok(()));
    assert_eq!(close_standing(&open, &open_digest, &close, true), Ok(()));
}

pub const ONTOLOGY_INVARIANTS: &[(&str, &str)] = &[
    ("certificate_inventory_is_complete", "all ten certificate kinds are generated"),
    ("receipt_pair_requires_exact_open_reference", "close receipt references the exact open digest"),
    ("receipt_pair_requires_equal_sequence", "open and close sequence numbers agree"),
    ("standing_requires_goal", "goal and replay closure grant standing"),
    ("standing_requires_all_effects", "all effects succeeded"),
    ("bounded_is_not_exhausted", "bounded remains bounded"),
];
