//! SPARQL CONSTRUCT transformation module.
//!
//! Provides functionality for executing SPARQL CONSTRUCT queries to transform
//! industry ontologies into YAWL workflow specifications.

pub mod executor;

pub use executor::{ConstructExecutor, Query};

/// Pattern mappings for FIBO to YAWL transformation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FiboPattern {
    /// Entity lifecycle (Created→Active→Closed) maps to Sequence (WP1)
    Lifecycle,
    /// Multiple obligations (all must complete) maps to Parallel Split (WP2)
    MultiObligation,
    /// Conditional requirement maps to Exclusive Choice (WP4)
    Conditional,
    /// Alternative paths maps to Multi-Choice (WP6)
    Alternative,
    /// State machine maps to State-based Choice (WP15)
    StateBased,
}

impl FiboPattern {
    /// Get the YAWL workflow pattern ID for this FIBO pattern.
    pub fn yawl_pattern_id(&self) -> &'static str {
        match self {
            Self::Lifecycle => "WP1",
            Self::MultiObligation => "WP2",
            Self::Conditional => "WP4",
            Self::Alternative => "WP6",
            Self::StateBased => "WP15",
        }
    }

    /// Get the split behavior for this pattern.
    pub fn split_behavior(&self) -> SplitBehavior {
        match self {
            Self::Lifecycle => SplitBehavior::Xor,
            Self::MultiObligation => SplitBehavior::And,
            Self::Conditional => SplitBehavior::Xor,
            Self::Alternative => SplitBehavior::Or,
            Self::StateBased => SplitBehavior::Xor,
        }
    }
}

/// YAWL split/join behaviors.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SplitBehavior {
    /// XOR split/join (exclusive choice)
    Xor,
    /// AND split/join (parallel)
    And,
    /// OR split/join (multi-choice)
    Or,
}

impl SplitBehavior {
    /// Get the YAWL IRI for this split behavior.
    pub fn iri(&self) -> &'static str {
        match self {
            Self::Xor => "http://unrdf.org/yawl#XOR_Split",
            Self::And => "http://unrdf.org/yawl#AND_Split",
            Self::Or => "http://unrdf.org/yawl#OR_Split",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fibo_pattern_ids() {
        assert_eq!(FiboPattern::Lifecycle.yawl_pattern_id(), "WP1");
        assert_eq!(FiboPattern::MultiObligation.yawl_pattern_id(), "WP2");
    }

    #[test]
    fn test_split_behavior_iri() {
        assert_eq!(SplitBehavior::Xor.iri(), "http://unrdf.org/yawl#XOR_Split");
        assert_eq!(SplitBehavior::And.iri(), "http://unrdf.org/yawl#AND_Split");
    }
}
