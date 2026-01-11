//! Usage tracking for token counts and cost estimation

use crate::DspyError;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Usage statistics for a model
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct UsageStats {
    /// Prompt tokens consumed
    pub prompt_tokens: u64,

    /// Completion tokens generated
    pub completion_tokens: u64,

    /// Total tokens (prompt + completion)
    pub total_tokens: u64,

    /// Number of requests
    pub requests: u64,

    /// Number of cached requests
    pub cached_requests: u64,

    /// Estimated cost in USD
    pub cost_usd: f64,
}

impl UsageStats {
    /// Calculate cache hit rate (0.0-1.0)
    pub fn cache_hit_rate(&self) -> f64 {
        if self.requests == 0 {
            0.0
        } else {
            self.cached_requests as f64 / self.requests as f64
        }
    }

    /// Merge statistics from another UsageStats
    pub fn merge(&mut self, other: &UsageStats) {
        self.prompt_tokens += other.prompt_tokens;
        self.completion_tokens += other.completion_tokens;
        self.total_tokens += other.total_tokens;
        self.requests += other.requests;
        self.cached_requests += other.cached_requests;
        self.cost_usd += other.cost_usd;
    }

    /// Add statistics from another UsageStats (alias for merge)
    pub fn add(&mut self, other: &UsageStats) {
        self.merge(other);
    }
}

/// Usage tracker for monitoring token consumption and costs
pub struct UsageTracker {
    /// Statistics per model
    stats: Arc<Mutex<HashMap<String, UsageStats>>>,

    /// Whether tracking is enabled
    enabled: bool,
}

impl UsageTracker {
    /// Create new usage tracker
    pub fn new(enabled: bool) -> Self {
        Self {
            stats: Arc::new(Mutex::new(HashMap::new())),
            enabled,
        }
    }

    /// Track a model call
    pub fn track(
        &self,
        model: &str,
        prompt_tokens: u64,
        completion_tokens: u64,
        cached: bool,
    ) {
        if !self.enabled {
            return;
        }

        let mut stats = self.stats.lock().unwrap();
        let entry = stats.entry(model.to_string()).or_default();

        entry.prompt_tokens += prompt_tokens;
        entry.completion_tokens += completion_tokens;
        entry.total_tokens += prompt_tokens + completion_tokens;
        entry.requests += 1;

        if cached {
            entry.cached_requests += 1;
        }

        // Estimate cost
        entry.cost_usd += self.estimate_cost(model, prompt_tokens, completion_tokens);
    }

    /// Estimate cost based on model pricing
    fn estimate_cost(&self, model: &str, prompt_tokens: u64, completion_tokens: u64) -> f64 {
        // Model-specific pricing (per 1M tokens)
        let (prompt_price, completion_price) = match model {
            "openai/gpt-4o" => (5.0, 15.0),
            "openai/gpt-4o-mini" => (0.15, 0.60),
            "openai/gpt-4-turbo" => (10.0, 30.0),
            "openai/gpt-3.5-turbo" => (0.50, 1.50),
            "anthropic/claude-3-5-sonnet-20241022" => (3.0, 15.0),
            "anthropic/claude-3-opus-20240229" => (15.0, 75.0),
            "anthropic/claude-3-haiku-20240307" => (0.25, 1.25),
            _ => (0.0, 0.0), // Unknown model
        };

        let prompt_cost = (prompt_tokens as f64 / 1_000_000.0) * prompt_price;
        let completion_cost = (completion_tokens as f64 / 1_000_000.0) * completion_price;

        prompt_cost + completion_cost
    }

    /// Get statistics for a specific model
    pub fn get_model_stats(&self, model: &str) -> Option<UsageStats> {
        let stats = self.stats.lock().unwrap();
        stats.get(model).cloned()
    }

    /// Get all statistics
    pub fn get_stats(&self) -> HashMap<String, UsageStats> {
        self.stats.lock().unwrap().clone()
    }

    /// Get total statistics across all models
    pub fn get_total_stats(&self) -> UsageStats {
        let stats = self.stats.lock().unwrap();
        let mut total = UsageStats::default();

        for model_stats in stats.values() {
            total.merge(model_stats);
        }

        total
    }

    /// Reset all statistics
    pub fn reset(&self) {
        self.stats.lock().unwrap().clear();
    }

    /// Reset statistics for a specific model
    pub fn reset_model(&self, model: &str) {
        self.stats.lock().unwrap().remove(model);
    }

    /// Check if tracking is enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Export statistics as JSON
    pub fn export_json(&self) -> Result<String, DspyError> {
        let stats = self.stats.lock().unwrap();
        serde_json::to_string_pretty(&*stats)
            .map_err(|e| DspyError::config_error(format!("Failed to export JSON: {}", e)))
    }

    /// Import statistics from JSON
    pub fn import_json(&self, json: &str) -> Result<(), DspyError> {
        let imported: HashMap<String, UsageStats> = serde_json::from_str(json)
            .map_err(|e| DspyError::config_error(format!("Failed to import JSON: {}", e)))?;

        let mut stats = self.stats.lock().unwrap();
        *stats = imported;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_usage_tracking() {
        let tracker = UsageTracker::new(true);

        tracker.track("openai/gpt-4o-mini", 100, 50, false);
        tracker.track("openai/gpt-4o-mini", 200, 100, true);

        let stats = tracker.get_model_stats("openai/gpt-4o-mini").unwrap();

        assert_eq!(stats.prompt_tokens, 300);
        assert_eq!(stats.completion_tokens, 150);
        assert_eq!(stats.total_tokens, 450);
        assert_eq!(stats.requests, 2);
        assert_eq!(stats.cached_requests, 1);
        assert!(stats.cost_usd > 0.0);
    }

    #[test]
    fn test_cost_estimation() {
        let tracker = UsageTracker::new(true);

        tracker.track("openai/gpt-4o-mini", 1_000_000, 1_000_000, false);

        let stats = tracker.get_model_stats("openai/gpt-4o-mini").unwrap();

        // Should be approximately $0.15 + $0.60 = $0.75
        assert!((stats.cost_usd - 0.75).abs() < 0.01);
    }

    #[test]
    fn test_cache_hit_rate() {
        let mut stats = UsageStats::default();
        stats.requests = 10;
        stats.cached_requests = 3;

        assert_eq!(stats.cache_hit_rate(), 0.3);
    }

    #[test]
    fn test_json_export_import() {
        let tracker = UsageTracker::new(true);

        tracker.track("openai/gpt-4o-mini", 100, 50, false);
        tracker.track("anthropic/claude-3-haiku-20240307", 200, 100, true);

        let json = tracker.export_json().unwrap();

        let new_tracker = UsageTracker::new(true);
        new_tracker.import_json(&json).unwrap();

        let total1 = tracker.get_total_stats();
        let total2 = new_tracker.get_total_stats();

        assert_eq!(total1.total_tokens, total2.total_tokens);
        assert_eq!(total1.requests, total2.requests);
    }
}
