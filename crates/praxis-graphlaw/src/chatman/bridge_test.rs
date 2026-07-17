#![cfg(test)]

use super::causal_adapter::{frame_payload_bytes, CausalFrameEntry};
use super::*;
use bcinr_powl_receipt::causal_receipt::{OcelCausalFrame, PackedObjRef};
use bcinr_powl_receipt::denial::DenialPolarity;
use chicago_tdd_tools::observability::receipt::{Blake3ChainValidator, ChainError};
use wasm4pm_compat::pddl::Pddl8GroundAction;

/// Fixed-fixture ground action (no wall clock, no randomness).
fn ground_action(label: &str) -> Pddl8GroundAction {
    Pddl8GroundAction {
        schema_name: label.to_owned(),
        label: format!("{label}()"),
        preconditions: Vec::new(),
        add_effects: Vec::new(),
        del_effects: Vec::new(),
    }
}

/// v2 op with the given kind and masks (all other fields zeroed).
fn v2_op(kind: v2::OpKind, pred: u64, succ: u64, fan_out: u8) -> v2::Powl64Op {
    let mut op = v2::Powl64Op::silent();
    op.op_kind = kind;
    op.pred_mask = pred;
    op.succ_mask = succ;
    op.fan_out = fan_out;
    op
}

/// Three-op lawful v2 tape: Activity -> XorChoice -> Activity.
fn lawful_v2_tape() -> Result<v2::PowlTape, Refusal> {
    let mut tape = v2::PowlTape::new();
    let ops = [
        v2_op(v2::OpKind::Activity, 0, 0b010, 1),
        v2_op(v2::OpKind::XorChoice, 0b001, 0b100, 1),
        v2_op(v2::OpKind::Activity, 0b010, 0, 0),
    ];
    // O(3): fixed fixture size.
    for op in ops {
        if tape.push(op).is_none() {
            return Err(Refusal::ValidationFailed(
                "test fixture tape overflow (unreachable: 3 ops)".to_owned(),
            ));
        }
    }
    tape.entry_op = 0;
    tape.exit_op = 2;
    Ok(tape)
}

/// Deterministic frame fixture with ordinal (non-wall-clock) timestamps.
fn frame(instruction_id: u64, prior_hash: [u8; 32]) -> OcelCausalFrame {
    OcelCausalFrame {
        instruction_id,
        fired_mask: 1u64 << (instruction_id & 63),
        denial: DenialPolarity::ADMITTED,
        obj_refs: [PackedObjRef::new(1, instruction_id as u32); 8],
        ts_ns: instruction_id, // ordinal, not wall clock
        activity_idx: instruction_id as u16,
        node_kind: 0,
        pad: [0u8; 5],
        prior_hash,
    }
}

/// Chain `n` fixture frames under the adapter rule
/// `stored = BLAKE3(prev ‖ payload)`.
///
/// # Complexity
/// O(n) frames, one BLAKE3 each.
fn chained_entries(n: u64) -> Vec<CausalFrameEntry> {
    let mut prev = [0u8; 32];
    let mut entries = Vec::with_capacity(n as usize);
    // O(n): forward chaining, one hash per frame.
    for i in 0..n {
        let f = frame(i, prev);
        let mut h = blake3::Hasher::new();
        h.update(&prev);
        h.update(&frame_payload_bytes(&f));
        let chain_hash: [u8; 32] = *h.finalize().as_bytes();
        prev = chain_hash;
        entries.push(CausalFrameEntry {
            frame: f,
            chain_hash,
        });
    }
    entries
}

#[test]
fn legacy_projection_maps_kinds_and_masks() -> Result<(), Refusal> {
    // Arrange
    let pddl = Pddl8Tape::from_plan(vec![ground_action("a"), ground_action("b")]);
    let v2_tape = lawful_v2_tape()?;

    // Act
    let bridge = TapeBridge::new(&pddl, &v2_tape, Vec::new())?;

    // Assert
    assert_eq!(bridge.legacy.len, 3);
    assert_eq!(bridge.legacy.entry_mask, 0b001);
    assert_eq!(bridge.legacy.ops[0].kind, legacy_tape::OpKind::Atom);
    assert_eq!(bridge.legacy.ops[1].kind, legacy_tape::OpKind::XorDispatch);
    assert_eq!(bridge.legacy.ops[1].branch_mask, 0b100);
    assert_eq!(bridge.legacy.ops[1].branch_count, 1);
    assert_eq!(bridge.legacy.ops[2].kind, legacy_tape::OpKind::Atom);
    assert_eq!(bridge.legacy.ops[2].pred_mask, 0b010);
    Ok(())
}

#[test]
fn unmappable_v2_kind_refuses_trace_unlawful() -> Result<(), Refusal> {
    // Arrange: Concur has no legacy counterpart.
    let pddl = Pddl8Tape::from_plan(vec![ground_action("a")]);
    let mut v2_tape = v2::PowlTape::new();
    if v2_tape.push(v2_op(v2::OpKind::Concur, 0, 0, 0)).is_none() {
        return Err(Refusal::ValidationFailed(
            "test fixture tape overflow (unreachable: 1 op)".to_owned(),
        ));
    }

    // Act
    let result = TapeBridge::new(&pddl, &v2_tape, Vec::new());

    // Assert
    assert!(matches!(result, Err(Refusal::TraceUnlawful(ref msg)) if msg.contains("slot 0")));
    Ok(())
}

#[test]
fn tape_hash_is_deterministic_and_input_sensitive() -> Result<(), Refusal> {
    // Arrange
    let pddl_a = Pddl8Tape::from_plan(vec![ground_action("a"), ground_action("b")]);
    let pddl_b = Pddl8Tape::from_plan(vec![ground_action("a"), ground_action("c")]);
    let v2_tape = lawful_v2_tape()?;

    // Act
    let h1 = TapeBridge::new(&pddl_a, &v2_tape, Vec::new())?.tape_hash()?;
    let h2 = TapeBridge::new(&pddl_a, &v2_tape, Vec::new())?.tape_hash()?;
    let h_other_pddl = TapeBridge::new(&pddl_b, &v2_tape, Vec::new())?.tape_hash()?;
    let mut v2_mut = lawful_v2_tape()?;
    v2_mut.ops[0].succ_mask = 0b110; // perturb one canonical field
    let h_other_powl = TapeBridge::new(&pddl_a, &v2_mut, Vec::new())?.tape_hash()?;

    // Assert
    assert_eq!(h1, h2, "same inputs must hash byte-identically");
    assert_ne!(h1, h_other_pddl, "PDDL side must enter the hash");
    assert_ne!(h1, h_other_powl, "POWL v2 side must enter the hash");
    Ok(())
}

#[test]
fn map_to_workflow_emits_slot_ordered_plan_sealed_by_tape_hash() -> Result<(), Refusal> {
    // Arrange
    let pddl = Pddl8Tape::from_plan(vec![ground_action("a")]);
    let v2_tape = lawful_v2_tape()?;
    let bridge = TapeBridge::new(&pddl, &v2_tape, Vec::new())?;

    // Act
    let plan = bridge.map_to_workflow()?;

    // Assert
    assert_eq!(plan.tape_hash, bridge.tape_hash()?);
    let kinds: Vec<&str> = plan.steps.iter().map(|s| s.kind.as_str()).collect();
    assert_eq!(kinds, ["Atom", "XorDispatch", "Atom"]);
    let indices: Vec<u8> = plan.steps.iter().map(|s| s.index).collect();
    assert_eq!(indices, [0, 1, 2], "steps must be in canonical slot order");
    Ok(())
}

#[test]
fn causal_adapter_chain_validates_three_frames() {
    // Arrange
    let entries = chained_entries(3);

    // Act
    let verdict = Blake3ChainValidator::validate_chain(&entries);

    // Assert
    assert_eq!(verdict, Ok(()));
}

#[test]
fn causal_adapter_detects_single_bit_tamper() {
    // Arrange: flip one bit in the middle frame's payload.
    let mut entries = chained_entries(3);
    entries[1].frame.fired_mask ^= 1;

    // Act
    let verdict = Blake3ChainValidator::validate_chain(&entries);

    // Assert: the recomputed hash at index 1 no longer matches.
    assert!(matches!(
        verdict,
        Err(ChainError::HashMismatch { index: 1, .. })
    ));
}

#[test]
fn causal_adapter_detects_parent_hash_tamper() {
    // Arrange: flip one bit in the last frame's parent-hash link.
    let mut entries = chained_entries(3);
    entries[2].frame.prior_hash[0] ^= 0x01;

    // Act
    let verdict = Blake3ChainValidator::validate_chain(&entries);

    // Assert: prev-hash threading breaks at index 2.
    assert!(matches!(
        verdict,
        Err(ChainError::PrevHashMismatch { index: 2, .. })
    ));
}

#[test]
fn hot_law_witnesses_compile_and_carry_bounds() {
    // Arrange / Act
    let chain = HotCausalChain::new();
    let _conditions = HotConditions;

    // Assert
    assert_eq!(chain.length(), 64);
}
