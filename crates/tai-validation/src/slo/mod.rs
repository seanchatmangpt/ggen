//! SLO/SLA tracking and validation

pub mod metrics;
pub mod tracker;
pub mod validator;

pub use metrics::SloMetrics;
pub use tracker::SloTracker;
pub use validator::SloValidator;

/// SLO definitions for ggen project

/// First build SLO threshold in seconds
pub const SLO_BUILD_FIRST: f64 = 15.0;
/// Incremental build SLO threshold in seconds
pub const SLO_BUILD_INCREMENTAL: f64 = 2.0;
/// RDF processing SLO threshold in seconds (for 1000+ triples)
pub const SLO_RDF_PROCESSING: f64 = 5.0;
/// Memory usage SLO threshold in megabytes
pub const SLO_MEMORY_MAX: f64 = 100.0;
/// CLI end-to-end execution SLO threshold in seconds
pub const SLO_CLI_END_TO_END: f64 = 3.0;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_slo_constants() {
        assert!(SLO_BUILD_FIRST > 0.0);
        assert!(SLO_BUILD_INCREMENTAL > 0.0);
        assert!(SLO_RDF_PROCESSING > 0.0);
        assert!(SLO_MEMORY_MAX > 0.0);
        assert!(SLO_CLI_END_TO_END > 0.0);
    }
}
