//! Sensor Manager - monitored component with Transient restart strategy
//!
//! SensorManager periodically collects data from sensors and emits signals.
//! Uses Transient restart strategy: only restarts on abnormal failure.

use async_trait::async_trait;
use tokio::time::{interval, Duration};
use tracing::{debug, error, info};

use crate::error::Result;
use crate::supervisor::Restartable;

/// Manages sensor data collection and processing
#[derive(Debug, Clone)]
pub struct SensorManager {
    id: String,
    restart_count: u32,
    is_running: bool,
}

impl SensorManager {
    /// Create a new sensor manager
    pub fn new(id: String) -> Self {
        Self {
            id,
            restart_count: 0,
            is_running: false,
        }
    }

    /// Simulate sensor reading
    async fn read_sensor(&self) -> Result<f64> {
        // Simulate sensor reading with occasional failures
        let value = rand::random::<f64>() * 100.0;
        debug!("Sensor {} reading: {:.2}", self.id, value);
        Ok(value)
    }

    /// Run the sensor collection loop
    async fn run_loop(&mut self) -> Result<()> {
        self.is_running = true;
        info!("SensorManager {} started", self.id);

        let mut interval = interval(Duration::from_secs(1));

        loop {
            interval.tick().await;

            match self.read_sensor().await {
                Ok(value) => {
                    debug!("SensorManager {} collected value: {:.2}", self.id, value);
                }
                Err(e) => {
                    error!("SensorManager {} sensor read failed: {}", self.id, e);
                    return Err(e);
                }
            }
        }
    }
}

#[async_trait]
impl Restartable for SensorManager {
    fn id(&self) -> &str {
        &self.id
    }

    async fn start(&mut self) -> Result<()> {
        self.restart_count += 1;
        info!(
            "SensorManager {} starting (attempt #{})",
            self.id, self.restart_count
        );
        self.run_loop().await
    }

    async fn stop(&mut self) -> Result<()> {
        self.is_running = false;
        info!("SensorManager {} stopped", self.id);
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
    fn test_sensor_manager_creation() {
        let manager = SensorManager::new("sensor_1".to_string());
        assert_eq!(manager.id(), "sensor_1");
        assert_eq!(manager.restart_count(), 0);
        assert!(!manager.is_running);
    }

    #[tokio::test]
    async fn test_sensor_manager_not_healthy_initially() {
        let manager = SensorManager::new("sensor_1".to_string());
        assert!(!manager.is_healthy().await);
    }
}
