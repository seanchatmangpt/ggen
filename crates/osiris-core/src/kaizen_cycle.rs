//! Kaizen Cycle - continuous improvement loop with Temporary restart strategy
//!
//! The Kaizen cycle implements continuous improvement processes.
//! Uses Temporary restart strategy: try up to N times then give up,
//! as it's not critical if this particular cycle fails.

use async_trait::async_trait;
use tokio::time::{interval, Duration};
use tracing::{debug, info};

use crate::error::Result;
use crate::supervisor::Restartable;

/// Kaizen cycle for continuous improvement
#[derive(Debug, Clone)]
pub struct KaizenCycle {
    id: String,
    restart_count: u32,
    is_running: bool,
    improvements_found: u32,
}

impl KaizenCycle {
    /// Create a new Kaizen cycle
    pub fn new(id: String) -> Self {
        Self {
            id,
            restart_count: 0,
            is_running: false,
            improvements_found: 0,
        }
    }

    /// Analyze for improvement opportunities
    async fn analyze_opportunities(&mut self) -> Result<Option<String>> {
        // Simulate opportunity analysis
        if rand::random::<f64>() > 0.7 {
            let improvement = format!("Improvement opportunity #{}", self.improvements_found + 1);
            self.improvements_found += 1;
            Ok(Some(improvement))
        } else {
            Ok(None)
        }
    }

    /// Run the Kaizen improvement cycle
    async fn run_loop(&mut self) -> Result<()> {
        self.is_running = true;
        info!("KaizenCycle {} started", self.id);

        let mut interval = interval(Duration::from_secs(3));

        loop {
            interval.tick().await;

            match self.analyze_opportunities().await {
                Ok(Some(improvement)) => {
                    debug!("KaizenCycle {} found improvement: {}", self.id, improvement);
                }
                Ok(None) => {
                    debug!("KaizenCycle {} - no new opportunities", self.id);
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
    }
}

#[async_trait]
impl Restartable for KaizenCycle {
    fn id(&self) -> &str {
        &self.id
    }

    async fn start(&mut self) -> Result<()> {
        self.restart_count += 1;
        info!(
            "KaizenCycle {} starting (attempt #{})",
            self.id, self.restart_count
        );
        self.run_loop().await
    }

    async fn stop(&mut self) -> Result<()> {
        self.is_running = false;
        info!(
            "KaizenCycle {} stopped (found {} improvements)",
            self.id, self.improvements_found
        );
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
    fn test_kaizen_cycle_creation() {
        let cycle = KaizenCycle::new("kaizen_1".to_string());
        assert_eq!(cycle.id(), "kaizen_1");
        assert_eq!(cycle.restart_count(), 0);
        assert!(!cycle.is_running);
        assert_eq!(cycle.improvements_found, 0);
    }

    #[tokio::test]
    async fn test_kaizen_cycle_not_healthy_initially() {
        let cycle = KaizenCycle::new("kaizen_1".to_string());
        assert!(!cycle.is_healthy().await);
    }
}
