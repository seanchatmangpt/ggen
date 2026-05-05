use serde::{Deserialize, Serialize};

/// Result of token-based replay conformance checking
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TokenReplayResult {
    pub fitness: f64,
    pub produced_tokens: usize,
    pub consumed_tokens: usize,
    pub missing_tokens: usize,
    pub remaining_tokens: usize,
}

impl TokenReplayResult {
    pub fn new(
        fitness: f64, produced_tokens: usize, consumed_tokens: usize, missing_tokens: usize,
        remaining_tokens: usize,
    ) -> Self {
        TokenReplayResult {
            fitness,
            produced_tokens,
            consumed_tokens,
            missing_tokens,
            remaining_tokens,
        }
    }

    pub fn calculate_fitness(
        produced: usize, consumed: usize, missing: usize, remaining: usize,
    ) -> f64 {
        let denom = (produced + remaining).max(1) as f64;
        let num = consumed.saturating_sub(missing) as f64;
        (num / denom).clamp(0.0, 1.0)
    }
}

/// Detailed conformance checking result
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ConformanceResult {
    pub fitness: f64,
    pub precision: Option<f64>,
    pub generalization: Option<f64>,
    pub simplicity: Option<f64>,
    pub total_traces: usize,
    pub fitting_traces: usize,
    pub deviating_traces: usize,
}

impl ConformanceResult {
    pub fn new(
        fitness: f64, total_traces: usize, fitting_traces: usize, deviating_traces: usize,
    ) -> Self {
        ConformanceResult {
            fitness,
            precision: None,
            generalization: None,
            simplicity: None,
            total_traces,
            fitting_traces,
            deviating_traces,
        }
    }

    pub fn with_precision(mut self, precision: f64) -> Self {
        self.precision = Some(precision.clamp(0.0, 1.0));
        self
    }

    pub fn with_generalization(mut self, generalization: f64) -> Self {
        self.generalization = Some(generalization.clamp(0.0, 1.0));
        self
    }

    pub fn with_simplicity(mut self, simplicity: f64) -> Self {
        self.simplicity = Some(simplicity.clamp(0.0, 1.0));
        self
    }

    pub fn conformance_rate(&self) -> f64 {
        if self.total_traces == 0 {
            0.0
        } else {
            self.fitting_traces as f64 / self.total_traces as f64
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_replay_fitness() {
        let fitness = TokenReplayResult::calculate_fitness(100, 95, 5, 10);
        assert!((fitness - 0.8181818).abs() < 0.001); // (95 - 5) / (100 + 10) = 90/110
    }

    #[test]
    fn test_conformance_result() {
        let result = ConformanceResult::new(0.95, 100, 95, 5);
        assert_eq!(result.conformance_rate(), 0.95);
        assert_eq!(result.fitting_traces, 95);
    }
}
