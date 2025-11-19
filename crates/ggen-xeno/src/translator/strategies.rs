//! Translation strategies for different types of semantic mapping.

/// Translation strategies for semantic mapping
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TranslationStrategy {
    /// Direct mapping (1:1 correspondence)
    Direct,

    /// Analogical mapping (using analogies)
    Analogical,

    /// Compositional (breaking down into components)
    Compositional,

    /// Statistical (using frequency and patterns)
    Statistical,

    /// Neural network based
    Neural,

    /// Hybrid approach
    Hybrid,
}

impl TranslationStrategy {
    /// Get the expected quality for this strategy
    #[must_use]
    pub const fn expected_quality(self) -> f64 {
        match self {
            Self::Direct => 0.95,
            Self::Analogical => 0.75,
            Self::Compositional => 0.85,
            Self::Statistical => 0.70,
            Self::Neural => 0.80,
            Self::Hybrid => 0.90,
        }
    }

    /// Get the information loss for this strategy
    #[must_use]
    pub const fn information_loss(self) -> f64 {
        match self {
            Self::Direct => 0.05,
            Self::Analogical => 0.25,
            Self::Compositional => 0.15,
            Self::Statistical => 0.30,
            Self::Neural => 0.20,
            Self::Hybrid => 0.10,
        }
    }
}
