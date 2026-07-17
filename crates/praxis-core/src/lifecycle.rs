//! Sealed typestate lifecycle: Raw → Validated → Admitted → Receipted

/// Sealed trait that marks valid lifecycle stages.
pub mod sealed {
    /// Marks a valid stage in the object lifecycle.
    pub trait Stage {}
}

/// Initial, unevaluated state: obligations and Andon status unknown.
pub struct Raw;

/// Post-judgment state: all obligations evaluated, Andon determined.
pub struct Validated;

/// Post-admission state: passed verdict, ready for receipt.
pub struct Admitted;

/// Post-receipt state: chain hash computed, signature (if signed feature) applied.
pub struct Receipted;

impl sealed::Stage for Raw {}
impl sealed::Stage for Validated {}
impl sealed::Stage for Admitted {}
impl sealed::Stage for Receipted {}
