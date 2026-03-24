use crate::{QualityMetrics, Result, Supplier, SupplierError};
use crate::scorer::QualityScorer;
use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RateLimitConfig {
    pub defect_threshold: f64,
    pub cooldown_duration: Duration,
    pub max_violations: u32,
}

impl Default for RateLimitConfig {
    fn default() -> Self {
        Self {
            defect_threshold: 20.0,
            cooldown_duration: Duration::hours(24),
            max_violations: 3,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RateLimitStatus {
    pub is_limited: bool,
    pub violations: u32,
    pub limited_until: Option<DateTime<Utc>>,
    pub current_defect_rate: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct SupplierLimitState {
    violations: u32,
    last_violation: Option<DateTime<Utc>>,
    limited_until: Option<DateTime<Utc>>,
}

impl Default for SupplierLimitState {
    fn default() -> Self {
        Self {
            violations: 0,
            last_violation: None,
            limited_until: None,
        }
    }
}

pub struct RateLimiter {
    config: RateLimitConfig,
    states: HashMap<String, SupplierLimitState>,
}

impl RateLimiter {
    pub fn new(config: RateLimitConfig) -> Self {
        Self {
            config,
            states: HashMap::new(),
        }
    }

    pub fn with_default_config() -> Self {
        Self::new(RateLimitConfig::default())
    }

    pub fn check_supplier(&mut self, supplier: &Supplier, metrics: &QualityMetrics) -> Result<RateLimitStatus> {
        supplier.validate()?;

        let score = QualityScorer::score(metrics)?;
        let now = Utc::now();

        let state = self.states.entry(supplier.id.clone()).or_default();

        // Check if currently limited
        if let Some(limited_until) = state.limited_until {
            if now < limited_until {
                return Ok(RateLimitStatus {
                    is_limited: true,
                    violations: state.violations,
                    limited_until: Some(limited_until),
                    current_defect_rate: score.defect_rate,
                });
            } else {
                // Cooldown expired, reset
                state.limited_until = None;
            }
        }

        // Check if defect rate exceeds threshold
        if score.defect_rate > self.config.defect_threshold {
            state.violations += 1;
            state.last_violation = Some(now);

            if state.violations >= self.config.max_violations {
                let limited_until = now + self.config.cooldown_duration;
                state.limited_until = Some(limited_until);

                return Ok(RateLimitStatus {
                    is_limited: true,
                    violations: state.violations,
                    limited_until: Some(limited_until),
                    current_defect_rate: score.defect_rate,
                });
            }
        }

        Ok(RateLimitStatus {
            is_limited: false,
            violations: state.violations,
            limited_until: None,
            current_defect_rate: score.defect_rate,
        })
    }

    pub fn enforce(&mut self, supplier: &Supplier, metrics: &QualityMetrics) -> Result<()> {
        let status = self.check_supplier(supplier, metrics)?;

        if status.is_limited {
            return Err(SupplierError::RateLimited(status.current_defect_rate));
        }

        Ok(())
    }

    pub fn reset_supplier(&mut self, supplier_id: &str) {
        self.states.remove(supplier_id);
    }

    pub fn get_status(&self, supplier_id: &str) -> Option<RateLimitStatus> {
        self.states.get(supplier_id).map(|state| {
            let now = Utc::now();
            let is_limited = state.limited_until.map_or(false, |until| now < until);

            RateLimitStatus {
                is_limited,
                violations: state.violations,
                limited_until: state.limited_until,
                current_defect_rate: 0.0,
            }
        })
    }

    pub fn is_limited(&self, supplier_id: &str) -> bool {
        self.states.get(supplier_id).map_or(false, |state| {
            state.limited_until.map_or(false, |until| Utc::now() < until)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_supplier() -> Supplier {
        Supplier::new("s1", "Test Supplier")
    }

    #[test]
    fn test_rate_limiter_new() {
        let config = RateLimitConfig::default();
        let limiter = RateLimiter::new(config);
        assert_eq!(limiter.states.len(), 0);
    }

    #[test]
    fn test_rate_limiter_with_default_config() {
        let limiter = RateLimiter::with_default_config();
        assert_eq!(limiter.config.defect_threshold, 20.0);
    }

    #[test]
    fn test_check_supplier_low_defects() {
        let mut limiter = RateLimiter::with_default_config();
        let supplier = create_test_supplier();
        let metrics = QualityMetrics::new(5, 3, 2, 0, 100).unwrap();

        let status = limiter.check_supplier(&supplier, &metrics).unwrap();

        assert!(!status.is_limited);
        assert_eq!(status.violations, 0);
        assert!(status.limited_until.is_none());
        assert_eq!(status.current_defect_rate, 10.0);
    }

    #[test]
    fn test_check_supplier_high_defects_first_violation() {
        let mut limiter = RateLimiter::with_default_config();
        let supplier = create_test_supplier();
        let metrics = QualityMetrics::new(25, 10, 5, 5, 100).unwrap();

        let status = limiter.check_supplier(&supplier, &metrics).unwrap();

        assert!(!status.is_limited);
        assert_eq!(status.violations, 1);
        assert!(status.limited_until.is_none());
    }

    #[test]
    fn test_check_supplier_multiple_violations() {
        let mut limiter = RateLimiter::with_default_config();
        let supplier = create_test_supplier();
        let bad_metrics = QualityMetrics::new(25, 10, 5, 5, 100).unwrap();

        // First violation
        let status1 = limiter.check_supplier(&supplier, &bad_metrics).unwrap();
        assert!(!status1.is_limited);
        assert_eq!(status1.violations, 1);

        // Second violation
        let status2 = limiter.check_supplier(&supplier, &bad_metrics).unwrap();
        assert!(!status2.is_limited);
        assert_eq!(status2.violations, 2);

        // Third violation - should trigger rate limit
        let status3 = limiter.check_supplier(&supplier, &bad_metrics).unwrap();
        assert!(status3.is_limited);
        assert_eq!(status3.violations, 3);
        assert!(status3.limited_until.is_some());
    }

    #[test]
    fn test_enforce_success() {
        let mut limiter = RateLimiter::with_default_config();
        let supplier = create_test_supplier();
        let metrics = QualityMetrics::new(5, 3, 2, 0, 100).unwrap();

        assert!(limiter.enforce(&supplier, &metrics).is_ok());
    }

    #[test]
    fn test_enforce_failure() {
        let mut limiter = RateLimiter::with_default_config();
        let supplier = create_test_supplier();
        let bad_metrics = QualityMetrics::new(25, 10, 5, 5, 100).unwrap();

        // Trigger three violations
        let _ = limiter.check_supplier(&supplier, &bad_metrics);
        let _ = limiter.check_supplier(&supplier, &bad_metrics);
        let _ = limiter.check_supplier(&supplier, &bad_metrics);

        let result = limiter.enforce(&supplier, &bad_metrics);
        assert!(matches!(result, Err(SupplierError::RateLimited(_))));
    }

    #[test]
    fn test_reset_supplier() {
        let mut limiter = RateLimiter::with_default_config();
        let supplier = create_test_supplier();
        let bad_metrics = QualityMetrics::new(25, 10, 5, 5, 100).unwrap();

        let _ = limiter.check_supplier(&supplier, &bad_metrics);
        assert_eq!(limiter.states.len(), 1);

        limiter.reset_supplier(&supplier.id);
        assert_eq!(limiter.states.len(), 0);
    }

    #[test]
    fn test_get_status_exists() {
        let mut limiter = RateLimiter::with_default_config();
        let supplier = create_test_supplier();
        let bad_metrics = QualityMetrics::new(25, 10, 5, 5, 100).unwrap();

        let _ = limiter.check_supplier(&supplier, &bad_metrics);

        let status = limiter.get_status(&supplier.id);
        assert!(status.is_some());
        assert_eq!(status.unwrap().violations, 1);
    }

    #[test]
    fn test_get_status_not_exists() {
        let limiter = RateLimiter::with_default_config();
        let status = limiter.get_status("nonexistent");
        assert!(status.is_none());
    }

    #[test]
    fn test_is_limited_true() {
        let mut limiter = RateLimiter::with_default_config();
        let supplier = create_test_supplier();
        let bad_metrics = QualityMetrics::new(25, 10, 5, 5, 100).unwrap();

        // Trigger three violations to get rate limited
        let _ = limiter.check_supplier(&supplier, &bad_metrics);
        let _ = limiter.check_supplier(&supplier, &bad_metrics);
        let _ = limiter.check_supplier(&supplier, &bad_metrics);

        assert!(limiter.is_limited(&supplier.id));
    }

    #[test]
    fn test_is_limited_false() {
        let mut limiter = RateLimiter::with_default_config();
        let supplier = create_test_supplier();
        let metrics = QualityMetrics::new(5, 3, 2, 0, 100).unwrap();

        let _ = limiter.check_supplier(&supplier, &metrics);

        assert!(!limiter.is_limited(&supplier.id));
    }

    #[test]
    fn test_is_limited_nonexistent() {
        let limiter = RateLimiter::with_default_config();
        assert!(!limiter.is_limited("nonexistent"));
    }

    #[test]
    fn test_rate_limit_config_default() {
        let config = RateLimitConfig::default();
        assert_eq!(config.defect_threshold, 20.0);
        assert_eq!(config.cooldown_duration, Duration::hours(24));
        assert_eq!(config.max_violations, 3);
    }

    #[test]
    fn test_rate_limit_config_custom() {
        let config = RateLimitConfig {
            defect_threshold: 15.0,
            cooldown_duration: Duration::hours(48),
            max_violations: 2,
        };

        let mut limiter = RateLimiter::new(config);
        let supplier = create_test_supplier();
        let metrics = QualityMetrics::new(20, 5, 5, 5, 100).unwrap();

        // First violation
        let status1 = limiter.check_supplier(&supplier, &metrics).unwrap();
        assert!(!status1.is_limited);

        // Second violation - should trigger with max_violations=2
        let status2 = limiter.check_supplier(&supplier, &metrics).unwrap();
        assert!(status2.is_limited);
    }
}
