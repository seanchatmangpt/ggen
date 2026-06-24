use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Summary of commit-multiplication statistics across all repos and bundles.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MultiplierSummary {
    pub total_commits: usize,
    pub repos_covered: usize,
    pub bundles_applied: usize,
    pub average_bundles_per_repo: f64,
}

/// Tracks which (repo, bundle) pairs have been applied and computes aggregate
/// commit-multiplication statistics.
///
/// Each successful application of a spec bundle to a repository constitutes
/// one commit. `CommitMultiplier` prevents duplicate applications within a
/// session and surfaces opportunities to re-apply bundles when the ontology
/// is updated.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CommitMultiplier {
    /// repo_name → list of bundle names that have been applied to it.
    applied: HashMap<String, Vec<String>>,
    total_commits: usize,
}

impl CommitMultiplier {
    /// Create a new, empty multiplier.
    pub fn new() -> Self {
        Self {
            applied: HashMap::new(),
            total_commits: 0,
        }
    }

    /// Record that `bundle` was successfully applied to `repo`.
    ///
    /// If the pair was already recorded this call is a no-op (idempotent).
    pub fn record(&mut self, repo: &str, bundle: &str) {
        if self.is_applied(repo, bundle) {
            return;
        }
        self.applied
            .entry(repo.to_owned())
            .or_default()
            .push(bundle.to_owned());
        self.total_commits += 1;
    }

    /// Return `true` if `bundle` has already been applied to `repo`.
    pub fn is_applied(&self, repo: &str, bundle: &str) -> bool {
        self.applied
            .get(repo)
            .map(|bundles| bundles.iter().any(|b| b == bundle))
            .unwrap_or(false)
    }

    /// Total number of commits recorded (one per unique (repo, bundle) pair).
    pub fn total_commits(&self) -> usize {
        self.total_commits
    }

    /// Number of distinct repositories that have had at least one bundle applied.
    pub fn repos_covered(&self) -> usize {
        self.applied.len()
    }

    /// Average number of bundles applied per repository.
    ///
    /// Returns `0.0` when no repositories have been touched yet.
    pub fn bundles_per_repo(&self) -> f64 {
        let repos = self.repos_covered();
        if repos == 0 {
            return 0.0;
        }
        self.total_commits as f64 / repos as f64
    }

    /// Return a snapshot summary suitable for logging or API responses.
    pub fn summary(&self) -> MultiplierSummary {
        let bundles_applied: usize = self.applied.values().map(|v| v.len()).sum();
        MultiplierSummary {
            total_commits: self.total_commits,
            repos_covered: self.repos_covered(),
            bundles_applied,
            average_bundles_per_repo: self.bundles_per_repo(),
        }
    }
}

impl Default for CommitMultiplier {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_multiplier_is_empty() {
        let m = CommitMultiplier::new();
        assert_eq!(m.total_commits(), 0);
        assert_eq!(m.repos_covered(), 0);
        assert!((m.bundles_per_repo() - 0.0).abs() < f64::EPSILON);
    }

    #[test]
    fn record_increments_total_commits() {
        let mut m = CommitMultiplier::new();
        m.record("repo-a", "bundle-x");
        assert_eq!(m.total_commits(), 1);
        assert_eq!(m.repos_covered(), 1);
    }

    #[test]
    fn duplicate_record_is_idempotent() {
        let mut m = CommitMultiplier::new();
        m.record("repo-a", "bundle-x");
        m.record("repo-a", "bundle-x");
        assert_eq!(m.total_commits(), 1);
    }

    #[test]
    fn is_applied_tracks_pairs_independently() {
        let mut m = CommitMultiplier::new();
        m.record("repo-a", "bundle-x");
        assert!(m.is_applied("repo-a", "bundle-x"));
        assert!(!m.is_applied("repo-a", "bundle-y"));
        assert!(!m.is_applied("repo-b", "bundle-x"));
    }

    #[test]
    fn multiple_repos_and_bundles() {
        let mut m = CommitMultiplier::new();
        m.record("repo-a", "bundle-x");
        m.record("repo-a", "bundle-y");
        m.record("repo-b", "bundle-x");
        assert_eq!(m.total_commits(), 3);
        assert_eq!(m.repos_covered(), 2);
        let avg = m.bundles_per_repo();
        // 3 commits / 2 repos = 1.5
        assert!((avg - 1.5).abs() < f64::EPSILON);
    }

    #[test]
    fn summary_fields_are_consistent() {
        let mut m = CommitMultiplier::new();
        m.record("repo-a", "bundle-x");
        m.record("repo-a", "bundle-y");
        m.record("repo-b", "bundle-z");
        let s = m.summary();
        assert_eq!(s.total_commits, 3);
        assert_eq!(s.repos_covered, 2);
        assert_eq!(s.bundles_applied, 3);
        assert!((s.average_bundles_per_repo - 1.5).abs() < f64::EPSILON);
    }

    #[test]
    fn summary_serializes_to_json() {
        let mut m = CommitMultiplier::new();
        m.record("repo-a", "bundle-x");
        let s = m.summary();
        let json = serde_json::to_string(&s).expect("serialization must not fail");
        assert!(json.contains("total_commits"));
        assert!(json.contains("repos_covered"));
        assert!(json.contains("bundles_applied"));
        assert!(json.contains("average_bundles_per_repo"));
    }
}
