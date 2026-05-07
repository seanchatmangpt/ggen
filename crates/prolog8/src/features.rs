// GENERATED — DO NOT EDIT — source: schema/domain.ttl

/// Prolog8 feature flags, encoded as a u8 bitmask in Rule8.feature_mask.
pub mod feature {

    /// Facts
    pub const FACTS: u8 = 1 << 0;

    /// Horn rules
    pub const HORNRULES: u8 = 1 << 1;

    /// Equality
    pub const EQUALITY: u8 = 1 << 2;

    /// Typed comparisons
    pub const TYPEDCOMPARISONS: u8 = 1 << 3;

    /// Stratified negation
    pub const STRATIFIEDNEGATION: u8 = 1 << 4;

    /// Bounded recursion
    pub const BOUNDEDRECURSION: u8 = 1 << 5;

    /// Controlled aggregates
    pub const CONTROLLEDAGGREGATES: u8 = 1 << 6;

    /// Contracted foreign predicates
    pub const CONTRACTEDFOREIGNPREDICATES: u8 = 1 << 7;

}
