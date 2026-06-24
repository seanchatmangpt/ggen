//! Chicago TDD integration tests for `ExpansionPlan` and `ExpansionSummary`.
//!
//! These tests use real `RepoCatalogEntry` values and real `ExpansionPlan`
//! state transitions.  No mocks or test doubles are used.

use ggen_daemon::catalog::RepoCatalogEntry;
use ggen_daemon::expansion::{ExpansionPlan, ExpansionSummary};

// ---------------------------------------------------------------------------
// Helper: build a minimal RepoCatalogEntry for use as test data.
// ---------------------------------------------------------------------------
fn make_entry(name: &str) -> RepoCatalogEntry {
    RepoCatalogEntry {
        name: name.to_owned(),
        github_url: format!("https://github.com/example/{}", name),
        short_desc: format!("{} project", name),
        primary_language: None,
    }
}

// ---------------------------------------------------------------------------
// Construction
// ---------------------------------------------------------------------------

#[test]
fn new_plan_starts_all_bundles_in_pending() {
    let bundles = vec!["b1".to_owned(), "b2".to_owned(), "b3".to_owned()];
    let repos = vec![make_entry("repo-a"), make_entry("repo-b")];
    let plan = ExpansionPlan::new(bundles.clone(), repos);

    assert_eq!(
        plan.spec_bundles.len(),
        3,
        "spec_bundles must record all 3 bundles"
    );
    assert_eq!(
        plan.pending.len(),
        3,
        "all bundles must start as pending"
    );
    assert!(
        plan.completed.is_empty(),
        "no bundle must start as completed"
    );
    assert_eq!(
        plan.remaining(),
        3,
        "remaining() must equal the bundle count on construction"
    );
}

#[test]
fn new_plan_records_repo_list() {
    let repos = vec![
        make_entry("alpha"),
        make_entry("beta"),
        make_entry("gamma"),
    ];
    let plan = ExpansionPlan::new(vec!["b".to_owned()], repos);
    assert_eq!(plan.repos.len(), 3);
    assert_eq!(plan.repos[0].name, "alpha");
    assert_eq!(plan.repos[2].name, "gamma");
}

#[test]
fn empty_bundles_plan_has_zero_remaining_and_zero_completion_ratio() {
    let plan = ExpansionPlan::new(vec![], vec![make_entry("r")]);
    assert_eq!(plan.remaining(), 0);
    assert!(
        (plan.completion_ratio() - 0.0).abs() < f64::EPSILON,
        "completion_ratio with no bundles must be 0.0"
    );
}

// ---------------------------------------------------------------------------
// next_bundle — sequential dispatch ordering
// ---------------------------------------------------------------------------

#[test]
fn next_bundle_returns_bundles_in_insertion_order() {
    let bundles = vec!["first".to_owned(), "second".to_owned(), "third".to_owned()];
    let mut plan = ExpansionPlan::new(bundles, vec![make_entry("r")]);

    assert_eq!(plan.next_bundle().as_deref(), Some("first"));
    assert_eq!(plan.next_bundle().as_deref(), Some("second"));
    assert_eq!(plan.next_bundle().as_deref(), Some("third"));
    assert_eq!(plan.next_bundle(), None, "None after all bundles consumed");
}

#[test]
fn next_bundle_decrements_remaining() {
    let mut plan = ExpansionPlan::new(
        vec!["a".to_owned(), "b".to_owned()],
        vec![make_entry("r")],
    );

    assert_eq!(plan.remaining(), 2);
    plan.next_bundle();
    assert_eq!(plan.remaining(), 1);
    plan.next_bundle();
    assert_eq!(plan.remaining(), 0);
}

#[test]
fn next_bundle_on_empty_plan_returns_none() {
    let mut plan = ExpansionPlan::new(vec![], vec![]);
    assert_eq!(plan.next_bundle(), None);
}

// ---------------------------------------------------------------------------
// mark_complete — moves bundle from pending to completed
// ---------------------------------------------------------------------------

#[test]
fn mark_complete_moves_bundle_to_completed() {
    let mut plan = ExpansionPlan::new(
        vec!["b1".to_owned(), "b2".to_owned()],
        vec![make_entry("r")],
    );

    plan.mark_complete("b1");

    assert!(
        plan.completed.contains(&"b1".to_owned()),
        "b1 must be in completed"
    );
    assert!(
        !plan.pending.contains(&"b1".to_owned()),
        "b1 must not remain in pending"
    );
    assert_eq!(plan.pending.len(), 1, "b2 must still be pending");
}

#[test]
fn mark_complete_is_idempotent() {
    let mut plan = ExpansionPlan::new(
        vec!["b1".to_owned()],
        vec![make_entry("r")],
    );

    plan.mark_complete("b1");
    plan.mark_complete("b1"); // second call must not add a duplicate

    assert_eq!(
        plan.completed.len(),
        1,
        "completed must contain b1 exactly once"
    );
}

#[test]
fn mark_complete_on_unknown_bundle_does_not_corrupt_state() {
    let mut plan = ExpansionPlan::new(
        vec!["b1".to_owned()],
        vec![make_entry("r")],
    );

    // Marking a bundle that was never in the plan must not panic or modify state.
    plan.mark_complete("ghost");

    assert_eq!(plan.pending.len(), 1, "b1 must still be pending");
    assert!(
        plan.completed.is_empty(),
        "completed must remain empty when marking an unknown bundle"
    );
}

// ---------------------------------------------------------------------------
// completion_ratio
// ---------------------------------------------------------------------------

#[test]
fn completion_ratio_is_zero_before_any_completes() {
    let plan = ExpansionPlan::new(
        vec!["a".to_owned(), "b".to_owned()],
        vec![make_entry("r")],
    );
    assert!(
        (plan.completion_ratio() - 0.0).abs() < f64::EPSILON,
        "ratio must be 0.0 with no completed bundles"
    );
}

#[test]
fn completion_ratio_is_one_when_all_complete() {
    let bundles = vec!["a".to_owned(), "b".to_owned(), "c".to_owned()];
    let mut plan = ExpansionPlan::new(bundles, vec![make_entry("r")]);

    plan.mark_complete("a");
    plan.mark_complete("b");
    plan.mark_complete("c");

    assert!(
        (plan.completion_ratio() - 1.0).abs() < f64::EPSILON,
        "ratio must be 1.0 when all bundles are complete"
    );
}

#[test]
fn completion_ratio_is_partial_when_some_complete() {
    let mut plan = ExpansionPlan::new(
        vec!["a".to_owned(), "b".to_owned(), "c".to_owned(), "d".to_owned()],
        vec![make_entry("r")],
    );

    plan.mark_complete("a");
    plan.mark_complete("b");

    let ratio = plan.completion_ratio();
    assert!(
        (ratio - 0.5).abs() < f64::EPSILON,
        "2 of 4 complete = 0.5, got {}",
        ratio
    );
}

// ---------------------------------------------------------------------------
// Integration: next_bundle + mark_complete workflow
// ---------------------------------------------------------------------------

#[test]
fn typical_dispatch_loop_exhausts_all_bundles() {
    let bundles = vec![
        "spec-a".to_owned(),
        "spec-b".to_owned(),
        "spec-c".to_owned(),
    ];
    let repos = vec![make_entry("r1"), make_entry("r2")];
    let mut plan = ExpansionPlan::new(bundles, repos);

    let mut dispatched = Vec::new();
    while let Some(bundle) = plan.next_bundle() {
        dispatched.push(bundle.clone());
        plan.mark_complete(&bundle);
    }

    assert_eq!(dispatched, vec!["spec-a", "spec-b", "spec-c"]);
    assert_eq!(plan.remaining(), 0);
    assert_eq!(plan.completed.len(), 3);
    assert!(
        (plan.completion_ratio() - 1.0).abs() < f64::EPSILON,
        "ratio must be 1.0 after full dispatch loop"
    );
}

#[test]
fn plan_with_100_bundles_and_50_repos_tracks_state_correctly() {
    let bundles: Vec<String> = (0..100).map(|i| format!("bundle-{}", i)).collect();
    let repos: Vec<RepoCatalogEntry> = (0..50).map(|i| make_entry(&format!("repo-{}", i))).collect();

    let mut plan = ExpansionPlan::new(bundles, repos);

    assert_eq!(plan.remaining(), 100);
    assert_eq!(plan.repos.len(), 50);

    // Consume and complete the first half.
    for _ in 0..50 {
        let b = plan.next_bundle().expect("must have bundle");
        plan.mark_complete(&b);
    }

    assert_eq!(plan.completed.len(), 50);
    assert_eq!(plan.remaining(), 50);
    assert!(
        (plan.completion_ratio() - 0.5).abs() < f64::EPSILON,
        "50 of 100 bundles complete = 0.5, got {}",
        plan.completion_ratio()
    );
}

// ---------------------------------------------------------------------------
// ExpansionSummary — serialization (struct has Serialize/Deserialize)
// ---------------------------------------------------------------------------

#[test]
fn expansion_summary_serializes_to_json_with_expected_fields() {
    let summary = ExpansionSummary {
        total_bundles: 7,
        total_repos: 120,
        total_commits_generated: 840,
        total_failures: 3,
    };

    let json = serde_json::to_string(&summary).expect("serialization must not fail");
    assert!(json.contains("total_bundles"), "JSON must contain total_bundles");
    assert!(json.contains("total_repos"), "JSON must contain total_repos");
    assert!(
        json.contains("total_commits_generated"),
        "JSON must contain total_commits_generated"
    );
    assert!(json.contains("total_failures"), "JSON must contain total_failures");
}

#[test]
fn expansion_summary_round_trips_through_json() {
    let original = ExpansionSummary {
        total_bundles: 14,
        total_repos: 176,
        total_commits_generated: 2464,
        total_failures: 0,
    };

    let json = serde_json::to_string(&original).expect("serialization must not fail");
    let decoded: ExpansionSummary =
        serde_json::from_str(&json).expect("deserialization must not fail");

    assert_eq!(decoded.total_bundles, original.total_bundles);
    assert_eq!(decoded.total_repos, original.total_repos);
    assert_eq!(decoded.total_commits_generated, original.total_commits_generated);
    assert_eq!(decoded.total_failures, original.total_failures);
}
