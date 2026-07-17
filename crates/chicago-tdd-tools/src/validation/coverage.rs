//! Coverage Analysis
//!
//! Provides test coverage analysis and reporting.
//!
//! # Poka-Yoke: Type-Level Validation
//!
//! This module uses newtypes to prevent count and percentage errors at compile time.
//! Use `TotalCount`, `CoveredCount`, and `CoveragePercentage` instead of raw `usize`/`f64`.

use std::collections::HashMap;
use std::fmt::Write;

// ============================================================================
// Poka-Yoke: Type-Level Validation
// ============================================================================

/// Total count newtype
///
/// **Poka-Yoke**: Use this newtype instead of `usize` to prevent count errors.
/// Ensures total count is always >= 0 and >= covered count.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::coverage::{TotalCount, CoveredCount};
///
/// let total = TotalCount::new(100);
/// let covered = CoveredCount::new(80);
///
/// // Validate: covered <= total
/// assert!(covered.get() <= total.get());
/// assert_eq!(total.get(), 100);
/// assert_eq!(covered.get(), 80);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TotalCount(usize);

impl TotalCount {
    /// Zero total count constant
    ///
    /// **Poka-Yoke**: Infallible constructor - no Option wrapping needed.
    /// Use this instead of `TotalCount::new(0)`.
    pub const ZERO: Self = Self(0);

    /// Create a new total count
    ///
    /// Any `usize` value is valid for total count. For clarity, `from_usize()` is an alias.
    #[must_use]
    pub const fn new(value: usize) -> Self {
        Self(value)
    }

    /// Create a new total count (infallible)
    ///
    /// **Poka-Yoke**: Infallible constructor - use this instead of `.new().unwrap()`.
    /// Any `usize` value is valid for total count.
    #[must_use]
    pub const fn from_usize(value: usize) -> Self {
        Self(value)
    }

    /// Get the count value
    #[must_use]
    #[allow(clippy::trivially_copy_pass_by_ref)] // Const fn - cannot change signature to pass by value
    pub const fn get(&self) -> usize {
        self.0
    }

    /// Convert to usize
    #[must_use]
    pub const fn into_usize(self) -> usize {
        self.0
    }
}

impl From<TotalCount> for usize {
    fn from(count: TotalCount) -> Self {
        count.0
    }
}

/// Covered count newtype
///
/// **Poka-Yoke**: Use this newtype instead of `usize` to prevent count errors.
/// Ensures covered count is always <= total count.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::coverage::{TotalCount, CoveredCount};
///
/// let total = TotalCount::new(100);
/// let covered = CoveredCount::new_for_total(80, total).unwrap();
///
/// // Validated: covered <= total
/// assert_eq!(covered.get(), 80);
/// assert_eq!(total.get(), 100);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CoveredCount(usize);

impl CoveredCount {
    /// Zero covered count constant
    ///
    /// **Poka-Yoke**: Infallible constructor - no Option wrapping needed.
    /// Use this instead of `CoveredCount::new(0)`.
    pub const ZERO: Self = Self(0);

    /// Create a new covered count
    ///
    /// Any `usize` value is valid for covered count (validation against total happens separately
    /// via `new_for_total`). For clarity, `from_usize()` is an alias.
    #[must_use]
    pub const fn new(value: usize) -> Self {
        Self(value)
    }

    /// Create a new covered count (infallible)
    ///
    /// **Poka-Yoke**: Infallible constructor - use this instead of `.new().unwrap()`.
    /// Any `usize` value is valid for covered count (validation against total happens separately).
    #[must_use]
    pub const fn from_usize(value: usize) -> Self {
        Self(value)
    }

    /// Create a new covered count validated against total count
    ///
    /// Returns `None` if covered > total.
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::coverage::{TotalCount, CoveredCount};
    ///
    /// let total = TotalCount::new(100);
    /// let covered = CoveredCount::new_for_total(80, total).unwrap(); // Valid
    /// assert_eq!(covered.get(), 80);
    /// let invalid = CoveredCount::new_for_total(150, total); // None - 150 > 100
    /// assert!(invalid.is_none());
    /// ```
    #[must_use]
    pub const fn new_for_total(covered: usize, total: TotalCount) -> Option<Self> {
        if covered <= total.get() {
            Some(Self(covered))
        } else {
            None
        }
    }

    /// Get the count value
    #[must_use]
    #[allow(clippy::trivially_copy_pass_by_ref)] // Const fn - cannot change signature to pass by value
    pub const fn get(&self) -> usize {
        self.0
    }

    /// Convert to usize
    #[must_use]
    pub const fn into_usize(self) -> usize {
        self.0
    }
}

impl From<CoveredCount> for usize {
    fn from(count: CoveredCount) -> Self {
        count.0
    }
}

/// Coverage percentage newtype
///
/// **Poka-Yoke**: Use this newtype instead of `f64` to prevent invalid percentage values.
/// Ensures percentage is always in range [0.0, 100.0].
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::coverage::{CoveragePercentage, TotalCount, CoveredCount};
///
/// let total = TotalCount::new(100);
/// let covered = CoveredCount::new_for_total(80, total).unwrap();
/// let percentage = CoveragePercentage::from_counts(covered, total).unwrap();
///
/// assert_eq!(percentage.get(), 80.0);
/// assert!(percentage.get() >= 0.0);
/// assert!(percentage.get() <= 100.0);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct CoveragePercentage(f64);

impl CoveragePercentage {
    /// Minimum valid percentage value
    pub const MIN: f64 = 0.0;

    /// Maximum valid percentage value
    pub const MAX: f64 = 100.0;

    /// Zero percentage constant (0.0%)
    ///
    /// **Poka-Yoke**: Infallible constructor - no Option wrapping needed.
    /// Use this instead of `CoveragePercentage::new(0.0).unwrap()`.
    pub const ZERO: Self = Self(0.0);

    /// Create a new coverage percentage from a value
    ///
    /// Returns `None` if value is outside [0.0, 100.0].
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::coverage::CoveragePercentage;
    ///
    /// let valid = CoveragePercentage::new(80.0).unwrap();
    /// assert_eq!(valid.get(), 80.0);
    ///
    /// let invalid = CoveragePercentage::new(150.0); // None - > 100%
    /// assert!(invalid.is_none());
    ///
    /// let invalid_negative = CoveragePercentage::new(-10.0); // None - < 0%
    /// assert!(invalid_negative.is_none());
    /// ```
    #[must_use]
    pub fn new(value: f64) -> Option<Self> {
        if (Self::MIN..=Self::MAX).contains(&value) {
            Some(Self(value))
        } else {
            None
        }
    }

    /// Create a coverage percentage from covered and total counts
    ///
    /// Returns `None` if total is 0 (division by zero) or if calculated percentage is invalid.
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::coverage::{CoveragePercentage, TotalCount, CoveredCount};
    ///
    /// let total = TotalCount::new(100);
    /// let covered = CoveredCount::new_for_total(80, total).unwrap();
    /// let percentage = CoveragePercentage::from_counts(covered, total).unwrap();
    ///
    /// assert_eq!(percentage.get(), 80.0);
    ///
    /// // Division by zero case
    /// let zero_total = TotalCount::new(0);
    /// let zero_covered = CoveredCount::new(0);
    /// let result = CoveragePercentage::from_counts(zero_covered, zero_total);
    /// assert!(result.is_none()); // Cannot calculate percentage for zero total
    /// ```
    #[must_use]
    pub fn from_counts(covered: CoveredCount, total: TotalCount) -> Option<Self> {
        if total.get() == 0 {
            return None; // Division by zero
        }

        #[allow(clippy::cast_precision_loss)]
        // Percentage calculation - precision loss acceptable for coverage percentages
        let percentage = (covered.get() as f64 / total.get() as f64) * 100.0;
        Self::new(percentage)
    }

    /// Get the percentage value
    #[must_use]
    #[allow(clippy::trivially_copy_pass_by_ref)] // Const fn - cannot change signature to pass by value
    pub const fn get(&self) -> f64 {
        self.0
    }

    /// Convert to f64
    #[must_use]
    pub const fn into_f64(self) -> f64 {
        self.0
    }
}

impl From<CoveragePercentage> for f64 {
    fn from(percentage: CoveragePercentage) -> Self {
        percentage.0
    }
}

/// Coverage report
#[derive(Debug, Clone)]
pub struct CoverageReport {
    /// Total items
    /// **Poka-Yoke**: Uses `TotalCount` newtype to prevent count errors
    pub total: TotalCount,
    /// Covered items
    /// **Poka-Yoke**: Uses `CoveredCount` newtype to prevent count errors
    pub covered: CoveredCount,
    /// Coverage percentage
    /// **Poka-Yoke**: Uses `CoveragePercentage` newtype to prevent invalid percentage values
    pub percentage: CoveragePercentage,
    /// Coverage details
    pub details: HashMap<String, bool>,
}

impl CoverageReport {
    /// Create new coverage report
    ///
    /// **Poka-Yoke**: Uses const ZERO constructors - no panic possible.
    #[must_use]
    pub fn new() -> Self {
        Self {
            // Poka-Yoke: Infallible constructors - no unwrap/expect needed
            total: TotalCount::ZERO,
            covered: CoveredCount::ZERO,
            percentage: CoveragePercentage::ZERO,
            details: HashMap::new(),
        }
    }

    /// Add coverage item
    /// Add an item to the coverage report
    ///
    /// **Poka-Yoke**: Uses infallible constructors - no panic possible.
    pub fn add_item(&mut self, name: String, covered: bool) {
        self.details.insert(name, covered);
        let new_total = self.total.get() + 1;
        // Poka-Yoke: Infallible constructor - any usize is valid
        self.total = TotalCount::from_usize(new_total);
        if covered {
            let new_covered = self.covered.get() + 1;
            // Validate: covered <= total
            if let Some(new_covered_count) = CoveredCount::new_for_total(new_covered, self.total) {
                self.covered = new_covered_count;
            }
        }
        // Update percentage using Poka-Yoke validated type
        if self.total.get() > 0 {
            // Poka-Yoke: from_counts validates percentage range
            if let Some(percentage) = CoveragePercentage::from_counts(self.covered, self.total) {
                self.percentage = percentage;
            }
        } else {
            // Total is 0, percentage is 0.0
            self.percentage = CoveragePercentage::ZERO;
        }
    }

    /// Generate markdown report
    #[must_use]
    pub fn generate_markdown(&self) -> String {
        let mut markdown = format!(
            "# Coverage Report\n\n**Coverage**: {:.2}% ({} / {})\n\n## Details\n\n",
            self.percentage.get(),
            self.covered.get(),
            self.total.get()
        );
        let mut keys: Vec<&String> = self.details.keys().collect();
        keys.sort();
        for name in keys {
            let covered = self.details.get(name).unwrap_or(&false);
            let status = if *covered {
                "[x] covered"
            } else {
                "[ ] uncovered"
            };
            let _ = writeln!(markdown, "- {name}: {status}");
        }
        markdown
    }
}

impl Default for CoverageReport {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
#[allow(clippy::unwrap_used)] // Test code - unwrap is acceptable for test setup
#[allow(clippy::float_cmp)] // Test code - exact float comparison is intentional
mod tests {
    use super::*;

    #[test]
    fn test_total_count() {
        let total = TotalCount::new(100);
        assert_eq!(total.get(), 100);

        let usize_value: usize = total.into();
        assert_eq!(usize_value, 100);
    }

    #[test]
    fn test_covered_count() {
        let covered = CoveredCount::new(80);
        assert_eq!(covered.get(), 80);

        let usize_value: usize = covered.into();
        assert_eq!(usize_value, 80);
    }

    #[test]
    fn test_covered_count_validation() {
        let total = TotalCount::new(100);

        // Valid: covered <= total
        let covered = CoveredCount::new_for_total(80, total).unwrap();
        assert_eq!(covered.get(), 80);

        // Valid: covered == total
        let covered = CoveredCount::new_for_total(100, total).unwrap();
        assert_eq!(covered.get(), 100);

        // Invalid: covered > total
        let invalid = CoveredCount::new_for_total(150, total);
        assert!(invalid.is_none());
    }

    #[test]
    fn test_coverage_report_with_newtypes() {
        let mut report = CoverageReport::new();
        assert_eq!(report.total.get(), 0);
        assert_eq!(report.covered.get(), 0);

        // Add covered item
        report.add_item("test1".to_string(), true);
        assert_eq!(report.total.get(), 1);
        assert_eq!(report.covered.get(), 1);

        // Add uncovered item
        report.add_item("test2".to_string(), false);
        assert_eq!(report.total.get(), 2);
        assert_eq!(report.covered.get(), 1); // Still 1 covered

        // Add another covered item
        report.add_item("test3".to_string(), true);
        assert_eq!(report.total.get(), 3);
        assert_eq!(report.covered.get(), 2);

        // Verify percentage using Poka-Yoke validated type
        let expected_percentage =
            CoveragePercentage::from_counts(CoveredCount::new(2), TotalCount::new(3)).unwrap();
        assert_eq!(report.percentage.get(), expected_percentage.get());
    }

    #[test]
    fn test_coverage_percentage_new() {
        // Valid percentages
        let p50 = CoveragePercentage::new(50.0).unwrap();
        assert_eq!(p50.get(), 50.0);

        let p0 = CoveragePercentage::new(0.0).unwrap();
        assert_eq!(p0.get(), 0.0);

        let p100 = CoveragePercentage::new(100.0).unwrap();
        assert_eq!(p100.get(), 100.0);

        // Invalid percentages
        let invalid_high = CoveragePercentage::new(150.0);
        assert!(invalid_high.is_none());

        let invalid_low = CoveragePercentage::new(-10.0);
        assert!(invalid_low.is_none());
    }

    #[test]
    fn test_coverage_percentage_from_counts() {
        let total = TotalCount::new(100);
        let covered = CoveredCount::new_for_total(80, total).unwrap();

        let percentage = CoveragePercentage::from_counts(covered, total).unwrap();
        assert_eq!(percentage.get(), 80.0);

        // Edge case: 100% coverage
        let covered_all = CoveredCount::new_for_total(100, total).unwrap();
        let percentage_all = CoveragePercentage::from_counts(covered_all, total).unwrap();
        assert_eq!(percentage_all.get(), 100.0);

        // Edge case: 0% coverage
        let covered_none = CoveredCount::ZERO;
        let percentage_none = CoveragePercentage::from_counts(covered_none, total).unwrap();
        assert_eq!(percentage_none.get(), 0.0);

        // Division by zero case
        let zero_total = TotalCount::ZERO;
        let zero_covered = CoveredCount::ZERO;
        let result = CoveragePercentage::from_counts(zero_covered, zero_total);
        assert!(result.is_none());
    }

    #[test]
    fn test_coverage_percentage_into_f64() {
        let percentage = CoveragePercentage::new(75.5).unwrap();
        let f64_value: f64 = percentage.into();
        assert_eq!(f64_value, 75.5);
    }
}
