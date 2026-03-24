//! Pipeline connecting all TPS systems
//!
//! Flow: firewall → packet → backpressure → a2a → jidoka → receipt

use ggen_a2a::Task;
use ggen_backpressure::{KanbanBoard, KanbanConfig, Stage};
use ggen_firewall::{AdmissionResponse, Firewall, IngressRequest};
use ggen_jidoka::{AndonSignal, ProductionLine};
use ggen_packet::WorkOrder;
use ggen_receipt::{generate_keypair, Receipt, ReceiptChain};
use serde::{Deserialize, Serialize};
use thiserror::Error;
use uuid::Uuid;

/// Pipeline configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PipelineConfig {
    /// Maximum work-in-progress limit
    pub wip_limit: usize,
    /// Enable receipt generation
    pub enable_receipts: bool,
    /// Enable jidoka quality gates
    pub enable_jidoka: bool,
}

impl Default for PipelineConfig {
    fn default() -> Self {
        Self {
            wip_limit: 10,
            enable_receipts: true,
            enable_jidoka: true,
        }
    }
}

/// Pipeline orchestrating all TPS systems
pub struct Pipeline {
    config: PipelineConfig,
    firewall: Firewall,
    kanban: KanbanBoard,
    production_line: ProductionLine,
    receipt_chain: Option<ReceiptChain>,
    signing_key: ed25519_dalek::SigningKey,
    verifying_key: ed25519_dalek::VerifyingKey,
}

impl Pipeline {
    /// Create a new pipeline
    pub async fn new(config: PipelineConfig) -> Result<Self> {
        let firewall = Firewall::with_defaults();

        let kanban_config = KanbanConfig {
            ready_limit: config.wip_limit,
            in_progress_limit: config.wip_limit,
            review_limit: config.wip_limit,
        };
        let kanban = KanbanBoard::new(kanban_config);

        let production_line = ProductionLine::new();

        let (signing_key, verifying_key) = generate_keypair();

        let receipt_chain = if config.enable_receipts {
            // Create genesis receipt
            let genesis = Receipt::new(
                "pipeline-genesis".to_string(),
                vec![],
                vec![],
                None,
            )
            .sign(&signing_key)
            .map_err(|e| PipelineError::Receipt(e.to_string()))?;

            Some(
                ReceiptChain::from_genesis(genesis)
                    .map_err(|e| PipelineError::Receipt(e.to_string()))?,
            )
        } else {
            None
        };

        Ok(Self {
            config,
            firewall,
            kanban,
            production_line,
            receipt_chain,
            signing_key,
            verifying_key,
        })
    }

    /// Start the pipeline
    pub async fn start(&mut self) -> Result<()> {
        tracing::info!("Starting pipeline");
        Ok(())
    }

    /// Shutdown the pipeline
    pub async fn shutdown(&mut self) -> Result<()> {
        tracing::info!("Shutting down pipeline");

        // Wait for in-flight work to complete
        while self.kanban.count(Stage::InProgress).await > 0 {
            tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
        }

        Ok(())
    }

    /// Process work through the full pipeline
    pub async fn process_work(&mut self, request: IngressRequest) -> Result<PipelineResult> {
        // Stage 1: Firewall admission control
        let admission = self.firewall.process(request.clone()).await;

        let request_id = match &admission {
            AdmissionResponse::Admitted { request_id, .. } => *request_id,
            AdmissionResponse::Refused { request_id, reason, .. } => {
                return Ok(PipelineResult::Refused {
                    request_id: *request_id,
                    reason: reason.clone(),
                });
            }
        };

        // Stage 2: Parse work order packet
        let work_order = self.parse_work_order(&request.payload)?;

        // Stage 3: Add to kanban backlog (enforces WIP limits via pull)
        let item_id = format!("wo-{}", work_order.id);
        self.kanban
            .add_to_backlog(item_id.clone())
            .await
            .map_err(|e| PipelineError::Backpressure(e.to_string()))?;

        // Pull to Ready stage
        let _token = self
            .kanban
            .pull(&item_id)
            .await
            .map_err(|e| PipelineError::Backpressure(e.to_string()))?;

        // Stage 4: Create A2A task
        let task = self.create_task(&work_order)?;

        // Stage 5: Run through jidoka quality gates
        let signal = if self.config.enable_jidoka {
            let results = self
                .production_line
                .run()
                .await
                .map_err(|e| PipelineError::Jidoka(e.to_string()))?;

            // Get highest signal (worst case)
            results
                .iter()
                .map(|r| r.signal)
                .max()
                .unwrap_or(AndonSignal::Green)
        } else {
            AndonSignal::Green
        };

        // Stage 6: Generate cryptographic receipt
        let receipt = if self.config.enable_receipts {
            self.generate_receipt(&work_order, &task, signal)?
        } else {
            None
        };

        Ok(PipelineResult::Processed {
            request_id,
            work_order_id: work_order.id.to_string(),
            task_id: task.id,
            signal,
            receipt,
        })
    }

    /// Parse work order from payload
    fn parse_work_order(&self, payload: &[u8]) -> Result<WorkOrder> {
        let work_order: WorkOrder = serde_json::from_slice(payload)
            .map_err(|e| PipelineError::InvalidPacket(e.to_string()))?;
        Ok(work_order)
    }

    /// Create A2A task from work order
    fn create_task(&self, work_order: &WorkOrder) -> Result<Task> {
        let task = Task::new(work_order.objective.clone(), work_order.owner.clone())
            .with_metadata("work_order_id".to_string(), work_order.id.to_string())
            .with_metadata("priority".to_string(), format!("{:?}", work_order.priority));

        Ok(task)
    }

    /// Generate cryptographic receipt
    fn generate_receipt(
        &mut self,
        work_order: &WorkOrder,
        task: &Task,
        _signal: AndonSignal,
    ) -> Result<Option<Receipt>> {
        let operation = format!("process-work-order-{}", work_order.id);

        let input_hashes = vec![
            ggen_receipt::hash_data(work_order.id.to_string().as_bytes()),
        ];

        let output_hashes = vec![
            ggen_receipt::hash_data(task.id.to_string().as_bytes()),
        ];

        // Get previous receipt hash from chain
        let previous_hash = if let Some(chain) = &self.receipt_chain {
            chain
                .last()
                .and_then(|r| r.hash().ok())
        } else {
            None
        };

        let receipt = Receipt::new(operation, input_hashes, output_hashes, previous_hash)
            .sign(&self.signing_key)
            .map_err(|e| PipelineError::Receipt(e.to_string()))?;

        // Add to chain if present
        if let Some(chain) = &mut self.receipt_chain {
            chain
                .append(receipt.clone())
                .map_err(|e| PipelineError::Receipt(e.to_string()))?;

            Ok(Some(receipt))
        } else {
            Ok(Some(receipt))
        }
    }

    /// Verify receipt chain integrity
    pub fn verify_receipt_chain(&self) -> Result<bool> {
        if let Some(chain) = &self.receipt_chain {
            chain
                .verify(&self.verifying_key)
                .map_err(|e| PipelineError::Receipt(e.to_string()))?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Get current WIP count in InProgress stage
    pub async fn wip_count(&self) -> usize {
        self.kanban.count(Stage::InProgress).await
    }

    /// Get WIP utilization for InProgress stage (0.0 to 1.0)
    pub async fn wip_utilization(&self) -> f64 {
        let count = self.kanban.count(Stage::InProgress).await as f64;
        let limit = self.kanban.wip_limit(Stage::InProgress).await as f64;
        if limit > 0.0 {
            count / limit
        } else {
            0.0
        }
    }
}

/// Result of pipeline processing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PipelineResult {
    /// Request was processed successfully
    Processed {
        /// Original request ID
        request_id: Uuid,
        /// Work order ID
        work_order_id: String,
        /// A2A task ID
        task_id: Uuid,
        /// Jidoka signal
        signal: AndonSignal,
        /// Cryptographic receipt
        receipt: Option<Receipt>,
    },
    /// Request was refused by firewall
    Refused {
        /// Request ID
        request_id: Uuid,
        /// Refusal reason
        reason: String,
    },
}

/// Pipeline errors
#[derive(Error, Debug)]
pub enum PipelineError {
    /// Firewall error
    #[error("Firewall error: {0}")]
    Firewall(String),

    /// Invalid packet
    #[error("Invalid packet: {0}")]
    InvalidPacket(String),

    /// Backpressure error
    #[error("Backpressure error: {0}")]
    Backpressure(String),

    /// A2A error
    #[error("A2A error: {0}")]
    A2A(String),

    /// Jidoka error
    #[error("Jidoka error: {0}")]
    Jidoka(String),

    /// Receipt error
    #[error("Receipt error: {0}")]
    Receipt(String),

    /// Internal error
    #[error("Internal error: {0}")]
    Internal(String),
}

/// Result type for pipeline operations
pub type Result<T> = std::result::Result<T, PipelineError>;

#[cfg(test)]
mod tests {
    use super::*;
    use ggen_firewall::IngressChannel;
    use ggen_packet::Priority;

    #[tokio::test]
    async fn test_pipeline_creation() {
        // Arrange
        let config = PipelineConfig::default();

        // Act
        let pipeline = Pipeline::new(config).await;

        // Assert
        assert!(pipeline.is_ok());
        let pipeline = pipeline.ok().unwrap();
        assert_eq!(pipeline.wip_count().await, 0);
    }

    #[tokio::test]
    async fn test_pipeline_process_work() {
        // Arrange
        let config = PipelineConfig::default();
        let mut pipeline = Pipeline::new(config).await.ok().unwrap();

        let work_order = WorkOrder::new(
            "Test objective".to_string(),
            "test@example.com".to_string(),
        )
        .ok()
        .unwrap()
        .with_priority(Priority::Normal)
        .ok()
        .unwrap();

        let payload = serde_json::to_vec(&work_order).ok().unwrap();
        let request = IngressRequest::new(IngressChannel::Batch, payload);

        // Act
        let result = pipeline.process_work(request).await;

        // Assert
        assert!(result.is_ok());
        match result.ok().unwrap() {
            PipelineResult::Processed { signal, .. } => {
                assert_eq!(signal, AndonSignal::Green);
            }
            PipelineResult::Refused { .. } => panic!("Expected Processed, got Refused"),
        }
    }

    #[tokio::test]
    async fn test_pipeline_receipt_chain_verification() {
        // Arrange
        let config = PipelineConfig::default();
        let pipeline = Pipeline::new(config).await.ok().unwrap();

        // Act
        let result = pipeline.verify_receipt_chain();

        // Assert
        assert!(result.is_ok());
        assert!(result.ok().unwrap());
    }

    #[tokio::test]
    async fn test_pipeline_wip_metrics() {
        // Arrange
        let config = PipelineConfig {
            wip_limit: 5,
            ..Default::default()
        };
        let pipeline = Pipeline::new(config).await.ok().unwrap();

        // Act & Assert
        assert_eq!(pipeline.wip_count().await, 0);
        assert_eq!(pipeline.wip_utilization().await, 0.0);
    }

    #[tokio::test]
    async fn test_pipeline_start_shutdown() {
        // Arrange
        let config = PipelineConfig::default();
        let mut pipeline = Pipeline::new(config).await.ok().unwrap();

        // Act
        let start_result = pipeline.start().await;
        let shutdown_result = pipeline.shutdown().await;

        // Assert
        assert!(start_result.is_ok());
        assert!(shutdown_result.is_ok());
    }
}
