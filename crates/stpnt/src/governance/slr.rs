/// Silent Loss Rate (SLR) Adjudicator.
pub struct SlrAdjudicator;

impl SlrAdjudicator {
    /// Calculates the Silent Loss Rate.
    /// SLR = (orphaned obligations) / (total welcomed persons)
    pub fn calculate_slr(orphaned: usize, total: usize) -> f64 {
        if total == 0 { return 0.0; }
        (orphaned as f64) / (total as f64)
    }
}
