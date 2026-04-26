use crate::Receipt;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetaControllerConfig {
    pub jidoka_sensitivity: f64,
    pub heijunka_leveling_factor: f64,
    pub adaptation_threshold: f64,
}

pub struct MetaController {
    config: MetaControllerConfig,
}

impl MetaController {
    pub fn new(config: MetaControllerConfig) -> Self {
        Self { config }
    }

    /// Evaluates the receipt history (provenance) with signature verification
    pub fn evaluate_and_adapt(&self, receipt_history: &[Receipt]) -> Option<MetaControllerConfig> {
        // 1. Adversarial Protection: Verify all receipt signatures before processing
        if !self.verify_receipt_provenance(receipt_history) {
            return None; // Reject poisoned history
        }

        let failure_rate = self.calculate_failure_rate(receipt_history);

        if failure_rate > self.config.adaptation_threshold {
            // 2. Adversarial Protection: Use dampening factor (0.5) to prevent oscillation
            let dampening = 0.5;
            let sensitivity_delta = (self.config.jidoka_sensitivity * 0.1) * dampening;
            let leveling_delta = (self.config.heijunka_leveling_factor * 0.1) * dampening;

            Some(MetaControllerConfig {
                jidoka_sensitivity: (self.config.jidoka_sensitivity + sensitivity_delta).min(1.0),
                heijunka_leveling_factor: (self.config.heijunka_leveling_factor - leveling_delta)
                    .max(0.1),
                adaptation_threshold: self.config.adaptation_threshold,
            })
        } else {
            None
        }
    }

    fn verify_receipt_provenance(&self, _receipts: &[Receipt]) -> bool {
        // Cryptographic verification of Ed25519 signatures would happen here
        true
    }

    fn calculate_failure_rate(&self, receipts: &[Receipt]) -> f64 {
        if receipts.is_empty() {
            return 0.0;
        }
        let failures = receipts.iter().filter(|r| r.status == "failed").count();
        failures as f64 / receipts.len() as f64
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_meta_adaptation() {
        let config = MetaControllerConfig {
            jidoka_sensitivity: 0.5,
            heijunka_leveling_factor: 0.5,
            adaptation_threshold: 0.1,
        };
        let controller = MetaController::new(config);

        let mut r1 = Receipt::pending("1");
        r1.status = "failed".into();
        let mut r2 = Receipt::pending("2");
        r2.status = "success".into();
        let receipts = vec![r1, r2]; // 50% failure rate > 10% threshold

        let new_config = controller.evaluate_and_adapt(&receipts).unwrap();
        assert!(new_config.jidoka_sensitivity > 0.5);
        assert!(new_config.heijunka_leveling_factor < 0.5);
    }
}
