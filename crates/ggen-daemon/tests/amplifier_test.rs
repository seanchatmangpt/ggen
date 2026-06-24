//! Chicago TDD integration tests for `CommitMultiplier` and `MultiplierSummary`.
//!
//! `amplifier.rs` already includes `#[cfg(test)]` unit tests that are compiled
//! into the crate itself.  These integration tests exercise the public API
//! through the crate boundary (`ggen_daemon::`) and cover additional behavioural
//! scenarios not present in the inline tests.  No mocks or test doubles are used.

use ggen_daemon::{CommitMultiplier, MultiplierSummary};

// ---------------------------------------------------------------------------
// Empty-state invariants
// ---------------------------------------------------------------------------

#[test]
fn new_multiplier_is_empty() {
    let m = CommitMultiplier::new();
    assert_eq!(m.total_commits(), 0, "fresh multiplier must have zero commits");
    assert_eq!(m.repos_covered(), 0, "fresh multiplier must cover zero repos");
    assert!(
        (m.bundles_per_repo() - 0.0).abs() < f64::EPSILON,
        "bundles_per_repo on empty multiplier must be 0.0"
    );
}

#[test]
fn default_is_equivalent_to_new() {
    let m_new = CommitMultiplier::new();
    let m_def = CommitMultiplier::default();
    assert_eq!(m_new.total_commits(), m_def.total_commits());
    assert_eq!(m_new.repos_covered(), m_def.repos_covered());
}

// ---------------------------------------------------------------------------
// Deduplication — recording the same (repo, bundle) pair is idempotent
// ---------------------------------------------------------------------------

#[test]
fn recording_same_pair_twice_does_not_double_count() {
    let mut m = CommitMultiplier::new();
    m.record("repo-a", "bundle-x");
    m.record("repo-a", "bundle-x"); // duplicate

    assert_eq!(
        m.total_commits(),
        1,
        "second record of same pair must not increment total_commits"
    );
    assert_eq!(
        m.repos_covered(),
        1,
        "second record of same pair must not add a new repo"
    );
}

#[test]
fn recording_same_pair_many_times_stays_at_one() {
    let mut m = CommitMultiplier::new();
    for _ in 0..50 {
        m.record("myrepo", "mybundle");
    }
    assert_eq!(m.total_commits(), 1);
}

#[test]
fn is_applied_distinguishes_repo_and_bundle_independently() {
    let mut m = CommitMultiplier::new();
    m.record("repo-a", "bundle-x");

    // Same repo, different bundle — not applied.
    assert!(!m.is_applied("repo-a", "bundle-y"));
    // Different repo, same bundle — not applied.
    assert!(!m.is_applied("repo-b", "bundle-x"));
    // Applied pair returns true.
    assert!(m.is_applied("repo-a", "bundle-x"));
}

// ---------------------------------------------------------------------------
// Summary totals match the recorded state
// ---------------------------------------------------------------------------

#[test]
fn summary_reports_correct_totals_after_multi_repo_multi_bundle() {
    let mut m = CommitMultiplier::new();
    // repo-a gets 2 bundles
    m.record("repo-a", "bundle-1");
    m.record("repo-a", "bundle-2");
    // repo-b gets 1 bundle
    m.record("repo-b", "bundle-1");

    let s: MultiplierSummary = m.summary();

    assert_eq!(s.total_commits, 3, "3 unique (repo, bundle) pairs");
    assert_eq!(s.repos_covered, 2, "2 distinct repos");
    assert_eq!(s.bundles_applied, 3, "sum of bundle lists == 3");
    assert!(
        (s.average_bundles_per_repo - 1.5).abs() < f64::EPSILON,
        "3 commits / 2 repos = 1.5, got {}",
        s.average_bundles_per_repo
    );
}

#[test]
fn summary_on_empty_multiplier_has_zero_average() {
    let m = CommitMultiplier::new();
    let s = m.summary();
    assert_eq!(s.total_commits, 0);
    assert_eq!(s.repos_covered, 0);
    assert_eq!(s.bundles_applied, 0);
    assert!(
        (s.average_bundles_per_repo - 0.0).abs() < f64::EPSILON,
        "average must be 0.0 when no repos are covered"
    );
}

#[test]
fn summary_after_duplicates_reflects_de_duplicated_counts() {
    let mut m = CommitMultiplier::new();
    // Record the same pair 3 times; it should count as 1.
    m.record("solo-repo", "solo-bundle");
    m.record("solo-repo", "solo-bundle");
    m.record("solo-repo", "solo-bundle");

    let s = m.summary();
    assert_eq!(s.total_commits, 1);
    assert_eq!(s.repos_covered, 1);
    assert_eq!(s.bundles_applied, 1);
    assert!(
        (s.average_bundles_per_repo - 1.0).abs() < f64::EPSILON,
        "1 bundle / 1 repo = 1.0"
    );
}

// ---------------------------------------------------------------------------
// Large-scale consistency check (100 repos × 5 bundles)
// ---------------------------------------------------------------------------

#[test]
fn large_scale_recording_produces_consistent_summary() {
    let mut m = CommitMultiplier::new();
    let repo_count = 100usize;
    let bundle_count = 5usize;

    for r in 0..repo_count {
        for b in 0..bundle_count {
            m.record(&format!("repo-{}", r), &format!("bundle-{}", b));
        }
    }

    let s = m.summary();
    assert_eq!(s.total_commits, repo_count * bundle_count);
    assert_eq!(s.repos_covered, repo_count);
    assert_eq!(s.bundles_applied, repo_count * bundle_count);
    assert!(
        (s.average_bundles_per_repo - bundle_count as f64).abs() < f64::EPSILON,
        "each repo received exactly {} bundles",
        bundle_count
    );
}

// ---------------------------------------------------------------------------
// Serialization round-trip (MultiplierSummary must survive JSON)
// ---------------------------------------------------------------------------

#[test]
fn multiplier_summary_round_trips_through_json() {
    let mut m = CommitMultiplier::new();
    m.record("repo-json", "bundle-json");
    let original = m.summary();

    let json = serde_json::to_string(&original).expect("serialization must not fail");

    // Verify required fields are present in the serialized form.
    assert!(json.contains("total_commits"), "JSON must contain total_commits");
    assert!(json.contains("repos_covered"), "JSON must contain repos_covered");
    assert!(json.contains("bundles_applied"), "JSON must contain bundles_applied");
    assert!(
        json.contains("average_bundles_per_repo"),
        "JSON must contain average_bundles_per_repo"
    );

    let deserialized: MultiplierSummary =
        serde_json::from_str(&json).expect("deserialization must not fail");
    assert_eq!(deserialized.total_commits, original.total_commits);
    assert_eq!(deserialized.repos_covered, original.repos_covered);
    assert_eq!(deserialized.bundles_applied, original.bundles_applied);
    assert!(
        (deserialized.average_bundles_per_repo - original.average_bundles_per_repo).abs()
            < f64::EPSILON
    );
}

// ---------------------------------------------------------------------------
// Clone preserves all state
// ---------------------------------------------------------------------------

#[test]
fn clone_preserves_committed_pairs() {
    let mut m = CommitMultiplier::new();
    m.record("repo-x", "b1");
    m.record("repo-x", "b2");
    m.record("repo-y", "b1");

    let m2 = m.clone();
    assert_eq!(m2.total_commits(), m.total_commits());
    assert_eq!(m2.repos_covered(), m.repos_covered());
    assert!(m2.is_applied("repo-x", "b1"));
    assert!(m2.is_applied("repo-x", "b2"));
    assert!(m2.is_applied("repo-y", "b1"));
    assert!(!m2.is_applied("repo-y", "b2"));
}

#[test]
fn mutations_after_clone_do_not_affect_original() {
    let mut m1 = CommitMultiplier::new();
    m1.record("repo-a", "b1");

    let mut m2 = m1.clone();
    m2.record("repo-a", "b2"); // mutate clone

    // Original must be unchanged.
    assert_eq!(m1.total_commits(), 1, "original must not see clone mutations");
    assert!(!m1.is_applied("repo-a", "b2"));
}
