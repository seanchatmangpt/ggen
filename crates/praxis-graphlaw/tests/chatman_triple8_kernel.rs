//! Boundary-fence tests over `ProfileSymbolTable`'s 256-term Triple8
//! universe, using `chicago_tdd_tools::param_test!` (an `rstest`-backed
//! wrapper per the framework's `parameterized-testing` feature).

use chicago_tdd_tools::assert_matches;
use chicago_tdd_tools::param_test;
use chicago_tdd_tools::prelude::*;

use praxis_graphlaw::chatman::abi::{ProfileId, Refusal};
use praxis_graphlaw::chatman::triple8::{ProfileSymbolTable, Term8};

fn terms(n: usize) -> Vec<String> {
    (0..n).map(|i| format!("<urn:t8:{i:05}>")).collect()
}

param_test! {
    #[case(0)]
    #[case(1)]
    #[case(255)]
    #[case(256)]
    #[case(257)]
    fn build_boundary_fence(size: usize) {
        // Arrange
        let profile_id = ProfileId::new("triple8-boundary-fence");
        let input = terms(size);

        // Act
        let result = ProfileSymbolTable::build(profile_id, input);

        // Assert: the fence is exactly at 256 — 0..=256 admits, 257 refuses.
        if size <= 256 {
            let table = result.expect("sizes up to 256 must build");
            assert_eq!(table.len(), size);
            assert!(table.is_empty() == (size == 0));
            if size > 0 {
                // Id 0 must resolve back to its own name (round trip).
                let name = table.name(Term8(0)).expect("id 0 must resolve within a non-empty table");
                assert_eq!(name, "<urn:t8:00000>");
            }
        } else {
            match result {
                Err(Refusal::Triple8UniverseOverflow(msg)) => {
                    assert!(
                        msg.contains("<urn:t8:00256>"),
                        "overflow message must name the first overflowing term (257th, sorted): {msg}"
                    );
                }
                other => panic!("expected Triple8UniverseOverflow at size {size}, got {other:?}"),
            }
        }
    }
}

// ---- TermNotInTriple8Universe: resolve/name misses on a frozen table -----

test!(
    resolve_miss_on_nonempty_table_refuses_term_not_in_universe,
    {
        // Arrange
        let profile_id = ProfileId::new("triple8-resolve-miss");
        let table = ProfileSymbolTable::build(profile_id, terms(4))?;

        // Act
        let miss = table.resolve("<urn:t8:absent>");

        // Assert
        assert_matches!(
            miss,
            Err(Refusal::TermNotInTriple8Universe(ref msg)) if msg.contains("absent")
        );
        Ok::<(), Refusal>(())
    }
);

test!(
    name_miss_beyond_table_length_refuses_term_not_in_universe,
    {
        // Arrange: a table with only 4 terms; id 4 is out of range.
        let profile_id = ProfileId::new("triple8-name-miss");
        let table = ProfileSymbolTable::build(profile_id, terms(4))?;

        // Act
        let miss = table.name(Term8(4));

        // Assert
        assert_matches!(
            miss,
            Err(Refusal::TermNotInTriple8Universe(ref msg)) if msg.contains("Term8(4)")
        );
        Ok::<(), Refusal>(())
    }
);

test!(
    name_miss_at_the_255_boundary_is_in_range_for_a_full_table,
    {
        // Arrange: exactly 256 terms — the boundary case where id 255 is the
        // last valid id and id 256 (not representable in a u8) cannot even be
        // constructed, which is itself part of the boundary proof.
        let profile_id = ProfileId::new("triple8-full-table");
        let table = ProfileSymbolTable::build(profile_id, terms(256))?;

        // Act + Assert
        assert!(table.name(Term8(255)).is_ok());
        // Term8 is a single byte (u8), so 255 is the structurally maximum id —
        // there is no Term8(256) to construct, proving the universe is bounded
        // at the type level, not merely by a runtime check.
        Ok::<(), Refusal>(())
    }
);
