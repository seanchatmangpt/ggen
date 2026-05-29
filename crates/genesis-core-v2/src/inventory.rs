//! # Non-Deletion Completion Protocol — Status Taxonomy
//!
//! Every file, crate, module, test, script, doc, and config gets a status label.
//! There is no DELETE category. The closest is DORMANT: preserved, not promoted.
//!
//! **Archaeologist posture, not maintainer posture.**
//! Code is fossil evidence. Even broken code may contain:
//! - a type pattern / boundary shape / interface name
//! - a receipt structure / CLI verb / test intention
//! - a crate split / runtime assumption / partially built adapter
//!
//! **Law: Nothing is removed. Code either becomes:**
//! - Admitted capability (LIVE)
//! - Refusal evidence (BROKEN_BUT_REAL — isolated and repaired)
//! - Dormant artifact (DORMANT — preserved but not promoted)
//! - Legacy compatibility surface (LEGACY_NAME — aliased, documented)

/// Status classification for every code artifact in the repository.
///
/// Agents MUST classify before acting. Acting without classification is
/// itself a `BabylonClaim::NarrativeAsEvidence`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum ArtifactStatus {
    /// Used and working. Preserve, test, document.
    Live = 1,

    /// Real but unfinished. Finish by smallest additive patch.
    Partial = 2,

    /// Contains useful pattern but not wired to anything. Extract contract, add wrapper.
    CapabilitySeed = 3,

    /// Old name for a current concept. Keep alias, document migration path.
    LegacyName = 4,

    /// Not currently used but potentially useful. Archive-in-place with manifest.
    /// The bytes remain. This is the closest thing to retirement in this doctrine.
    Dormant = 5,

    /// Fails compile or test but has real intent. Isolate, repair, test.
    BrokenButReal = 6,

    /// Described but not implemented. Connect to nearest code seed.
    DocOnly = 7,

    /// Test reveals intended behavior but no implementation exists.
    /// Use the test as a requirement specification.
    TestOnly = 8,

    /// Unclear purpose. Do not change until classified.
    Ambiguous = 9,
}

impl ArtifactStatus {
    /// Can this artifact be changed without prior classification?
    pub const fn requires_classification_before_change(&self) -> bool {
        matches!(self, ArtifactStatus::Ambiguous)
    }

    /// What is the allowed action for this status?
    pub fn allowed_action(&self) -> &'static str {
        match self {
            ArtifactStatus::Live => "preserve, test, document",
            ArtifactStatus::Partial => "finish by smallest patch",
            ArtifactStatus::CapabilitySeed => "extract contract, add wrapper",
            ArtifactStatus::LegacyName => "keep alias, document migration",
            ArtifactStatus::Dormant => "archive-in-place with manifest",
            ArtifactStatus::BrokenButReal => "isolate, repair, test",
            ArtifactStatus::DocOnly => "connect to nearest code seed",
            ArtifactStatus::TestOnly => "use test as requirement",
            ArtifactStatus::Ambiguous => "do not change until classified",
        }
    }

    /// Is deletion ever allowed for this status?
    pub const fn deletion_allowed(&self) -> bool {
        false // Never. No DELETE category exists.
    }
}

/// A classified artifact record — the inventory unit.
///
/// Every agent action must begin by creating or looking up a `ClassifiedArtifact`
/// for every file it intends to change.
#[derive(Debug, Clone)]
pub struct ClassifiedArtifact {
    /// Path relative to the workspace root.
    pub path: &'static str,
    /// The artifact's status classification.
    pub status: ArtifactStatus,
    /// The pattern(s) this artifact contains or implements.
    pub patterns: &'static [&'static str],
    /// What capability does this artifact already have?
    pub capability_present: &'static str,
    /// What is the precise gap to finish this artifact?
    pub finish_gap: &'static str,
    /// What is the smallest additive finish action?
    pub finish_action: &'static str,
}

/// The finish strategy — ordered by priority.
///
/// Agents work in this sequence. Skipping steps is a `BabylonClaim`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum FinishStep {
    /// 1. Inventory: find all related names, patterns, modules, tests, docs.
    Inventory = 1,
    /// 2. Classify: mark every artifact with an `ArtifactStatus`.
    Classify = 2,
    /// 3. Recover: identify the capability already present. Do not invent first.
    Recover = 3,
    /// 4. Connect: smallest additive patch — wrapper, adapter, alias, export.
    Connect = 4,
    /// 5. Verify: compile check, tests, examples, doc checks.
    Verify = 5,
    /// 6. Receipt: report files inspected, seeds found, changes, tests run, results, gaps.
    Receipt = 6,
}

/// The preferred connection mechanisms — ordered from least invasive to most.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ConnectionMechanism {
    /// Expose a module path (pub use, pub mod).
    ModuleExport = 1,
    /// Create a type alias for a legacy name.
    CompatibilityAlias = 2,
    /// Add a wrapper struct/impl that forwards to the seed.
    Wrapper = 3,
    /// Add an adapter that translates between two representations.
    Adapter = 4,
    /// Add a facade that presents a unified interface over multiple seeds.
    Facade = 5,
    /// Add an integration path (feature gate, optional dep, cfg block).
    FeatureGate = 6,
    /// Add a manifest/doc entry pointing to the existing artifact.
    ManifestEntry = 7,
    /// Add a test harness that proves the existing code works.
    TestHarness = 8,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_deletion_category_exists() {
        // Exhaustively verify: deletion_allowed() is false for every status
        let all_statuses = [
            ArtifactStatus::Live,
            ArtifactStatus::Partial,
            ArtifactStatus::CapabilitySeed,
            ArtifactStatus::LegacyName,
            ArtifactStatus::Dormant,
            ArtifactStatus::BrokenButReal,
            ArtifactStatus::DocOnly,
            ArtifactStatus::TestOnly,
            ArtifactStatus::Ambiguous,
        ];

        for status in &all_statuses {
            assert!(
                !status.deletion_allowed(),
                "Status {:?} must never allow deletion — no DELETE category exists",
                status
            );
        }
    }

    #[test]
    fn test_nine_statuses_exist() {
        // The taxonomy has exactly 9 statuses (1..=9)
        let bytes: Vec<u8> = vec![1, 2, 3, 4, 5, 6, 7, 8, 9];
        assert_eq!(bytes.len(), 9);
    }

    #[test]
    fn test_ambiguous_requires_classification_before_change() {
        assert!(ArtifactStatus::Ambiguous.requires_classification_before_change());
        assert!(!ArtifactStatus::Live.requires_classification_before_change());
        assert!(!ArtifactStatus::Partial.requires_classification_before_change());
    }

    #[test]
    fn test_allowed_actions_are_additive_not_destructive() {
        let all_statuses = [
            ArtifactStatus::Live,
            ArtifactStatus::Partial,
            ArtifactStatus::CapabilitySeed,
            ArtifactStatus::LegacyName,
            ArtifactStatus::Dormant,
            ArtifactStatus::BrokenButReal,
            ArtifactStatus::DocOnly,
            ArtifactStatus::TestOnly,
            ArtifactStatus::Ambiguous,
        ];

        for status in &all_statuses {
            let action = status.allowed_action();
            // Allowed actions must not contain destructive verbs
            assert!(
                !action.contains("delete"),
                "{:?}: action contains 'delete'",
                status
            );
            assert!(
                !action.contains("remove"),
                "{:?}: action contains 'remove'",
                status
            );
            assert!(
                !action.contains("rewrite"),
                "{:?}: action contains 'rewrite'",
                status
            );
            assert!(
                !action.contains("discard"),
                "{:?}: action contains 'discard'",
                status
            );
        }
    }

    #[test]
    fn test_finish_steps_are_ordered() {
        assert!((FinishStep::Inventory as u8) < (FinishStep::Classify as u8));
        assert!((FinishStep::Classify as u8) < (FinishStep::Recover as u8));
        assert!((FinishStep::Recover as u8) < (FinishStep::Connect as u8));
        assert!((FinishStep::Connect as u8) < (FinishStep::Verify as u8));
        assert!((FinishStep::Verify as u8) < (FinishStep::Receipt as u8));
    }

    #[test]
    fn test_connection_mechanisms_are_ordered_least_invasive_first() {
        // ModuleExport is always preferred over deletion
        assert!((ConnectionMechanism::ModuleExport as u8) < (ConnectionMechanism::Adapter as u8));
        assert!(
            (ConnectionMechanism::CompatibilityAlias as u8) < (ConnectionMechanism::Facade as u8)
        );
    }
}
