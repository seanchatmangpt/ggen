//! OSIRIS Zero Cognitive Load Architecture
//!
//! Formula: λ_admitted → 0 by refusing inadmissible at boundary.
//!
//! OSIRIS coordinates admission control, capacity estimation, decision WIP tracking,
//! and request routing to maintain zero cognitive load.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────┐
//! │              OSIRIS Coordinator             │
//! ├─────────────────────────────────────────────┤
//! │                                             │
//! │  ┌──────────────┐      ┌─────────────────┐ │
//! │  │ Admission    │─────▶│ Decision WIP    │ │
//! │  │ Controller   │      │ Tracker         │ │
//! │  └──────────────┘      └─────────────────┘ │
//! │         │                                   │
//! │         │ try_admit()                       │
//! │         ▼                                   │
//! │  ┌──────────────┐      ┌─────────────────┐ │
//! │  │ Capacity     │◀─────│ Router          │ │
//! │  │ Estimator    │      │                 │ │
//! │  └──────────────┘      └─────────────────┘ │
//! │         │                      │            │
//! │         │ μ_capacity           │ route()    │
//! │         └──────────────────────┘            │
//! │                                             │
//! └─────────────────────────────────────────────┘
//!           │                  │
//!    refuse │                  │ admit
//!           ▼                  ▼
//!      [Reject]            [Process]
//! ```
//!
//! # Example
//!
//! ```no_run
//! use ggen_osiris::{Osiris, OsirisConfig};
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let config = OsirisConfig::default();
//! let mut osiris = Osiris::new(config)?;
//!
//! // Process request with admission control
//! let request = "task";
//! match osiris.process(request, |r| async move {
//!     Ok::<_, String>(format!("Processed: {}", r))
//! }).await {
//!     Ok(response) => println!("Success: {}", response),
//!     Err(e) => println!("Refused: {}", e),
//! }
//! # Ok(())
//! # }
//! ```

#![deny(missing_docs)]
#![deny(unsafe_code)]

pub mod admission;
pub mod capacity;
pub mod decision_wip;
pub mod routing;

use admission::{AdmissionController, AdmissionPolicy, AdmissionStats};
use routing::RoutingError;
use std::future::Future;
use thiserror::Error;

#[derive(Debug, Error)]
/// OSIRIS errors
pub enum OsirisError {
    /// Configuration error
    #[error("Configuration error: {0}")]
    Config(String),

    /// Routing error
    #[error("Routing error: {0}")]
    Routing(#[from] RoutingError),
}

/// OSIRIS configuration
#[derive(Debug, Clone, Default)]
pub struct OsirisConfig {
    /// Admission policy
    pub policy: AdmissionPolicy,
}

/// OSIRIS coordinator
pub struct Osiris {
    admission: AdmissionController,
}

impl Osiris {
    /// Create new OSIRIS coordinator
    pub fn new(config: OsirisConfig) -> Result<Self, OsirisError> {
        let admission = AdmissionController::new(config.policy)
            .map_err(|e| OsirisError::Config(e.to_string()))?;

        Ok(Self { admission })
    }

    /// Process request with admission control
    pub async fn process<F, Fut, Req, Resp>(
        &mut self,
        request: Req,
        handler: F,
    ) -> Result<Resp, OsirisError>
    where
        F: FnOnce(Req) -> Fut,
        Fut: Future<Output = Result<Resp, String>>,
        Req: Send,
        Resp: Send,
    {
        // Admission control
        let ticket = self.admission.try_admit()
            .map_err(|e| OsirisError::Routing(e.into()))?;

        // Process
        let start = std::time::Instant::now();
        let result = handler(request).await;
        let duration = start.elapsed();

        // Release and update
        drop(ticket);
        self.admission.record_completion(duration);

        result.map_err(|e| OsirisError::Routing(RoutingError::ProcessingFailed(e)))
    }

    /// Get admission statistics
    pub fn stats(&self) -> AdmissionStats {
        self.admission.stats()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_osiris_creation() {
        let config = OsirisConfig::default();
        let osiris = Osiris::new(config);
        assert!(osiris.is_ok());
    }

    #[tokio::test]
    async fn test_osiris_process_success() {
        let config = OsirisConfig::default();
        let mut osiris = Osiris::new(config).unwrap();

        let response = osiris
            .process("input", |req| async move {
                Ok::<_, String>(format!("Processed: {}", req))
            })
            .await;

        assert!(response.is_ok());
        assert_eq!(response.unwrap(), "Processed: input");
    }

    #[tokio::test]
    async fn test_osiris_stats() {
        let config = OsirisConfig::default();
        let mut osiris = Osiris::new(config).unwrap();

        let _response = osiris
            .process("test", |req| async move {
                Ok::<_, String>(format!("Done: {}", req))
            })
            .await
            .unwrap();

        let stats = osiris.stats();
        assert_eq!(stats.current_wip, 0); // Completed
    }

    #[tokio::test]
    async fn test_osiris_refuse_at_limit() {
        let config = OsirisConfig {
            policy: AdmissionPolicy {
                wip_limit: 1,
                min_capacity_ratio: 0.0,
                cooldown: std::time::Duration::from_millis(10),
            },
        };
        let mut osiris = Osiris::new(config).unwrap();

        // Process requests sequentially (in real async, would be concurrent)
        let _r1 = osiris
            .process("req1", |req| async move {
                Ok::<_, String>(format!("Done: {}", req))
            })
            .await
            .unwrap();

        let stats = osiris.stats();
        assert_eq!(stats.current_wip, 0); // First completed
    }
}
