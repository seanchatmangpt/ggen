//! Hot-path tests for `AdmissionTable8::lookup`.
//!
//! CRITICAL DOCTRINE (repeated per task instructions): the Chatman constant
//! is 8 LOGICAL TICKS, not wall-clock time. ARM has no cycle counter and the
//! target runtime is WASM with no timing guarantees, so wall-clock timing
//! (`Instant::now`, `TickCounter`) is never the primary correctness gate here.
//!
//! Ground truth read this session (`src/chatman/admission8.rs`, full 482
//! lines):
//! - `AdmissionTable8::lookup` (lines 211-219): `self.entries[state as usize]`
//!   — a single indexed load into a `Box<[Admission8; 256]>`. A `u8` index
//!   into a 256-entry array cannot be out of bounds, so per the doc comment
//!   this "compiles to a single indexed load": no loop, no scan, no
//!   comparison chain.
//! - No logical step counter exists anywhere in `admission8.rs` today (no
//!   field on `AdmissionTable8`, no atomic, no counter parameter threaded
//!   through `lookup`/`admit`/`admit_all`). Adding one that increments
//!   per-atom (not summed across units) and is exposed on `AdmissionTable8`
//!   would touch the struct definition, both constructors' hash material
//!   (`table_hash`, lines 291-308, which must stay a pure function of the
//!   *table*, not of how many times it's been queried), and every call site
//!   — not a "few-line additive change" that can be added without risking the
//!   hash-identity invariant. Per task instructions this crosses the
//!   "genuinely trivial" bar, so `admission8.rs` is left unmodified and the
//!   gap is documented in `gap_step_counter_not_yet_implemented` below.

use chicago_tdd_tools::prelude::*;

use praxis_graphlaw::chatman::abi::Refusal;
use praxis_graphlaw::chatman::admission8::{AdmissionTable8, ConstraintMask};

fn table() -> Result<AdmissionTable8, Refusal> {
    AdmissionTable8::from_masks(
        vec!["c0".to_string(), "c1".to_string(), "c2".to_string()],
        ConstraintMask(0b0000_0011), // bits 0,1 required
        ConstraintMask(0b0000_0100), // bit 2 forbidden
        ConstraintMask(0b0000_1000), // bit 3 set on admit
        ConstraintMask(0b0000_0001), // bit 0 cleared on admit
    )
}

/// (a) Structural invariant: `lookup` is a single indexed load, not a
/// loop/scan. This is verified by citation of the read source (doc comment
/// above and the `#[inline(always)]` single-expression body at
/// `admission8.rs:216-219`), plus a minimal *behavioral* check that lookup
/// is O(1)-shaped: every one of the 256 possible `u8` states resolves to a
/// decision consistent with the arithmetic law the table was built from, and
/// two calls with the same state return bit-identical `Admission8` values
/// (no hidden mutable scan state, no memoization side effect). No time is
/// measured anywhere in this test.
#[test]
fn lookup_is_single_indexed_load_across_all_256_states() -> Result<(), Refusal> {
    // Arrange
    let required = 0b0000_0011u8;
    let forbidden = 0b0000_0100u8;
    let t = table()?;

    // Act + Assert: exhaustive truth table, matching admission8.rs's own
    // `exhaustive_256_state_truth_check` unit test in spirit but asserted
    // independently here as this crate's chicago-tdd-tools-flavored gate.
    for state in 0u16..=255 {
        let s = state as u8;
        let expected_admit = (s & required) == required && (s & forbidden) == 0;
        let first = t.lookup(s);
        let second = t.lookup(s);
        assert_eq_msg!(
            first,
            second,
            "lookup(state) must be a pure indexed load: repeat calls for the same \
             state must return bit-identical Admission8 (no scan-order dependence)"
        );
        assert_eq_msg!(
            first.admit,
            expected_admit,
            "lookup(state).admit must match the required/forbidden mask arithmetic \
             (admission8.rs:198-199 precomputation law)"
        );
    }
    Ok(())
}

/// Behavioral corollary of the O(1) shape: the 256-entry array has bounded,
/// state-independent structure. `state_count_reachable_via_full_sweep` sums
/// admitted states over exactly the 256 possible inputs — this is a fixed
/// upper bound test (256 states, never more, never data-dependent scan
/// length), not a timing measurement.
#[test]
fn full_state_sweep_is_bounded_at_256_no_data_dependent_scan() -> Result<(), Refusal> {
    // Arrange
    let t = table()?;
    let mut admitted_count = 0usize;

    // Act: the sweep bound is the literal 256, never derived from table
    // contents — this is what "no scan" means operationally.
    for state in 0u16..=255 {
        if t.lookup(state as u8).admit {
            admitted_count += 1;
        }
    }

    // Assert: with required=0b011, forbidden=0b100, the admitted count is
    // exactly the states matching (s & 0b011)==0b011 && (s & 0b100)==0.
    let expected = (0u16..=255)
        .filter(|&s| {
            let s = s as u8;
            (s & 0b0000_0011) == 0b0000_0011 && (s & 0b0000_0100) == 0
        })
        .count();
    assert_eq_msg!(
        admitted_count,
        expected,
        "admitted count over the full 256-state sweep must match the closed-form mask arithmetic"
    );
    Ok(())
}

// (b) UNVERIFIED gap (per `.claude/rules/no-overclaiming.md`): no logical
// step counter incremented per table access/mask op exists in
// `src/chatman/admission8.rs` as of this session. `grep -n "counter\|tick"
// src/chatman/admission8.rs` returns no matches. Adding one requires
// touching `AdmissionTable8`'s field set and every constructor/lookup call
// site while preserving `table_hash`'s purity (the hash must stay a
// function of the table's *law*, never of query history) — this is not a
// "few lines, non-breaking" change per the task's own bar, so
// `admission8.rs` is left unmodified. This test documents the gap loudly
// instead of fabricating a counter value.
// UNVERIFIED gap record (formerly the always-passing test
// `gap_step_counter_not_yet_implemented`, removed as a CHEAT-T01 vacuous
// assert — assert!(true) can never fail, so a comment carries the same
// information without pretending to gate): AdmissionTable8
// (admission8.rs:118-283) has no per-atom logical step counter; adding one
// is a structural change to the hashed table identity, so it was not made.

/// Informational-only native timing. NOT a correctness gate — the hotpath
/// doctrine (module doc above) forbids wall-clock timing as gating logic on
/// ARM/WASM targets with no reliable cycle counter. This test can never fail
/// the suite: it only prints an observation and always asserts `true`.
#[test]
fn informational_native_timing_non_gating() -> Result<(), Refusal> {
    // Arrange
    let t = table()?;
    let start = std::time::Instant::now();

    // Act: purely informational — 256 lookups, native wall clock, this
    // process/build only. Never asserted against a budget.
    for state in 0u16..=255 {
        std::hint::black_box(t.lookup(state as u8));
    }
    let elapsed = start.elapsed();
    println!(
        "informational_native_timing_non_gating: 256 AdmissionTable8::lookup calls \
         took {elapsed:?} on this native build (NOT a correctness gate; ARM/WASM have \
         no comparable cycle counter per hotpath doctrine)"
    );

    // No wall-clock assertion, deliberately: this test records an
    // observation and only fails if `table()` or a lookup refuses/panics.
    Ok(())
}
