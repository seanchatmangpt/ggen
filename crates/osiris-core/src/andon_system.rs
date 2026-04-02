//! Andon System - critical monitored component with Permanent restart strategy
//!
//! Andon is a critical system for quality control and anomaly detection.
//! Uses Permanent restart strategy: always restarts, even on normal termination,
//! because this is a critical system that must never stop.

use async_trait::async_trait;
use tokio::time::{interval, Duration};
use tracing::{error, info, warn};

use crate::error::Result;
use crate::supervisor::Restartable;

/// Critical Andon system for quality control
#[derive(Debug, Clone)]
pub struct AndonSystem {
    id: String,
    restart_count: u32,
    is_running: bool,
    alert_threshold: f64,
}

impl AndonSystem {
    /// Create a new Andon system
    pub fn new(id: String) -> Self {
        Self {
            id,
            restart_count: 0,
            is_running: false,
            alert_threshold: 80.0,
        }
    }

    /// Monitor for quality anomalies
    async fn check_quality(&self) -> Result<bool> {
        // Simulate quality check
        let quality_score = rand::random::<f64>() * 100.0;

        if quality_score < self.alert_threshold {
            warn!(
                "AndonSystem {} - QUALITY ALERT: score {:.2}",
                self.id, quality_score
            );
            Ok(false)
        } else {
            Ok(true)
        }
    }

    /// Run the Andon monitoring loop
    async fn run_loop(&mut self) -> Result<()> {
        self.is_running = true;
        info!("AndonSystem {} started (CRITICAL)", self.id);

        let mut interval = interval(Duration::from_secs(2));

        loop {
            interval.tick().await;

            match self.check_quality().await {
                Ok(_quality_ok) => {
                    // Continue monitoring
                }
                Err(e) => {
                    error!("AndonSystem {} quality check failed: {}", self.id, e);
                    return Err(e);
                }
            }
        }
    }
}

#[async_trait]
impl Restartable for AndonSystem {
    fn id(&self) -> &str {
        &self.id
    }

    async fn start(&mut self) -> Result<()> {
        self.restart_count += 1;
        info!(
            "AndonSystem {} starting (attempt #{}) - CRITICAL SYSTEM",
            self.id, self.restart_count
        );
        self.run_loop().await
    }

    async fn stop(&mut self) -> Result<()> {
        self.is_running = false;
        warn!("AndonSystem {} stopped - CRITICAL SYSTEM DOWN", self.id);
        Ok(())
    }

    async fn is_healthy(&self) -> bool {
        self.is_running
    }

    fn restart_count(&self) -> u32 {
        self.restart_count
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_andon_system_creation() {
        let andon = AndonSystem::new("andon_1".to_string());
        assert_eq!(andon.id(), "andon_1");
        assert_eq!(andon.restart_count(), 0);
        assert!(!andon.is_running);
        assert_eq!(andon.alert_threshold, 80.0);
    }

    #[tokio::test]
    async fn test_andon_system_not_healthy_initially() {
        let andon = AndonSystem::new("andon_1".to_string());
        assert!(!andon.is_healthy().await);
    }
}
