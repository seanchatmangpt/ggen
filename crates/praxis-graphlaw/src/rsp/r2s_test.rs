#![cfg(test)]

use crate::rsp::r2s::Relation2StreamOperator;
use crate::rsp::r2s::StreamOperator::{DSTREAM, ISTREAM, RSTREAM};
use crate::sparql::Binding;

#[test]
fn test_rstream() {
    let new_result = vec![
        vec![
            Binding {
                var: "?1".to_string(),
                val: "1".to_string(),
            },
            Binding {
                var: "?2".to_string(),
                val: "2".to_string(),
            },
        ],
        vec![
            Binding {
                var: "?1".to_string(),
                val: "1.2".to_string(),
            },
            Binding {
                var: "?2".to_string(),
                val: "2.2".to_string(),
            },
        ],
    ];
    let mut s2r: Relation2StreamOperator<Vec<Binding>> = Relation2StreamOperator::new(RSTREAM, 0);
    let expected_result = new_result.clone();

    assert_eq!(expected_result, s2r.eval(new_result, 1));
}
#[test]
fn test_dstream() {
    let old_result = vec![
        vec![
            Binding {
                var: "?1".to_string(),
                val: "1".to_string(),
            },
            Binding {
                var: "?2".to_string(),
                val: "2".to_string(),
            },
        ],
        vec![
            Binding {
                var: "?1".to_string(),
                val: "1.2".to_string(),
            },
            Binding {
                var: "?2".to_string(),
                val: "2.2".to_string(),
            },
        ],
    ];
    let new_result = vec![
        vec![
            Binding {
                var: "?1".to_string(),
                val: "1".to_string(),
            },
            Binding {
                var: "?2".to_string(),
                val: "2".to_string(),
            },
        ],
        vec![
            Binding {
                var: "?1".to_string(),
                val: "1.3".to_string(),
            },
            Binding {
                var: "?2".to_string(),
                val: "2.3".to_string(),
            },
        ],
    ];
    let expected_deletion = vec![vec![
        Binding {
            var: "?1".to_string(),
            val: "1.2".to_string(),
        },
        Binding {
            var: "?2".to_string(),
            val: "2.2".to_string(),
        },
    ]];
    let mut s2r: Relation2StreamOperator<Vec<Binding>> = Relation2StreamOperator::new(DSTREAM, 0);
    s2r.eval(old_result, 1);

    assert_eq!(expected_deletion, s2r.eval(new_result, 2));
}
#[test]
fn test_istream() {
    let old_result = vec![
        vec![
            Binding {
                var: "?1".to_string(),
                val: "1".to_string(),
            },
            Binding {
                var: "?2".to_string(),
                val: "2".to_string(),
            },
        ],
        vec![
            Binding {
                var: "?1".to_string(),
                val: "1.2".to_string(),
            },
            Binding {
                var: "?2".to_string(),
                val: "2.2".to_string(),
            },
        ],
    ];
    let new_result = vec![
        vec![
            Binding {
                var: "?1".to_string(),
                val: "1".to_string(),
            },
            Binding {
                var: "?2".to_string(),
                val: "2".to_string(),
            },
        ],
        vec![
            Binding {
                var: "?1".to_string(),
                val: "1.3".to_string(),
            },
            Binding {
                var: "?2".to_string(),
                val: "2.3".to_string(),
            },
        ],
    ];
    let expected_deletion = vec![vec![
        Binding {
            var: "?1".to_string(),
            val: "1.3".to_string(),
        },
        Binding {
            var: "?2".to_string(),
            val: "2.3".to_string(),
        },
    ]];
    let mut s2r: Relation2StreamOperator<Vec<Binding>> = Relation2StreamOperator::new(ISTREAM, 0);
    s2r.eval(old_result, 1);

    assert_eq!(expected_deletion, s2r.eval(new_result, 2));
}

// Regression for a real, second-half-of-task-#52 determinism bug (the sibling to swarm findings
// #22/#23/#24, found by an adversarial sweep, not fixed at the time): `Relation2StreamOperator::
// eval`'s DSTREAM branch iterated `self.old_result` -- a `std::collections::HashSet<O>`, the
// same `RandomState`-hashed, per-process-reseeded map type as finding #22 -- directly into its
// returned `Vec<O>`, so with >= 2 deleted rows, the returned deletion order differed across
// separate process runs of byte-identical input. `test_dstream` above only ever asserts a single
// deleted row (order is trivially unaffected by iteration order for one element), so it never
// exercised this bug -- this test uses 10 distinct deleted rows and asserts the returned order is
// the canonical ascending `Binding` order (unambiguous now that `Binding` derives `Ord`; ties
// can't arise since these 10 rows are pairwise distinct by construction), which is only
// guaranteed by the fix's `sort_unstable()`, not by chance HashSet iteration order for 10 items.
#[test]
fn test_dstream_deletion_order_is_deterministic_with_multiple_rows() {
    let row = |n: u32| {
        vec![
            Binding {
                var: "?1".to_string(),
                val: format!("{n}"),
            },
            Binding {
                var: "?2".to_string(),
                val: format!("v{n}"),
            },
        ]
    };
    let old_result: Vec<Vec<Binding>> = (0..10).map(row).collect();
    // new_result is disjoint from old_result (all rows deleted) except one shared row (?9),
    // matching the shape of the existing test_dstream (some overlap, some deletion) rather than
    // a fully-disjoint edge case.
    let new_result: Vec<Vec<Binding>> = vec![row(9), row(100)];

    let mut expected_deletion: Vec<Vec<Binding>> = (0..9).map(row).collect();
    expected_deletion.sort_unstable();

    let mut s2r: Relation2StreamOperator<Vec<Binding>> = Relation2StreamOperator::new(DSTREAM, 0);
    s2r.eval(old_result, 1);
    let actual_deletion = s2r.eval(new_result, 2);

    assert_eq!(
        actual_deletion.len(),
        9,
        "exactly 9 of the 10 old rows must be reported deleted, got: {actual_deletion:?}"
    );
    assert_eq!(
        actual_deletion, expected_deletion,
        "DSTREAM deletion rows must be returned in canonical ascending order, not raw HashSet \
         iteration order, got: {actual_deletion:?}"
    );
}
