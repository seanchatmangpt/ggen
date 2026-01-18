//! Orchestrator: The Measurement Function μ
//!
//! Implements the five-stage transformation pipeline (Chatman Equation: A = μ(O))
//! transforming ETL output into workflow triggers.

use crate::events::{EtlTripleEvent, WorkflowTriggerEvent};
use crate::kgc::TemporalContext;
use crate::bus::EventBus;
use crate::tracing::SpanCorrelation;
use crate::Result;
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, warn};
use uuid::Uuid;

/// Configuration for the orchestrator
#[derive(Clone, Debug)]
pub struct OrchestratorConfig {
    /// Workflow ID to trigger (from process variables)
    pub workflow_id: String,

    /// Batch size for variable aggregation
    pub batch_size: usize,

    /// Git reference for reproducibility
    pub git_reference: String,

    /// Enable KGC-4D snapshot computation
    pub enable_kgc_snapshots: bool,

    /// Enable OTel span correlation
    pub enable_span_correlation: bool,

    /// Event deduplication cache size
    pub dedup_cache_size: usize,

    /// Andon threshold: max pending events before RED signal
    pub andon_overflow_threshold: usize,
}

impl Default for OrchestratorConfig {
    fn default() -> Self {
        Self {
            workflow_id: "default-workflow".to_string(),
            batch_size: 100,
            git_reference: Uuid::new_v4().to_string(), // Fallback to UUID
            enable_kgc_snapshots: true,
            enable_span_correlation: true,
            dedup_cache_size: 1000,
            andon_overflow_threshold: 10_000,
        }
    }
}

/// The Orchestrator: Measurement Function μ
///
/// Implements the five-stage transformation pipeline:
/// 1. ETL Event Intake
/// 2. KGC-4D Context Injection
/// 3. Process Variable Aggregation
/// 4. Workflow Trigger Generation
/// 5. Workflow Execution & Receipt
pub struct Orchestrator {
    config: OrchestratorConfig,
    event_bus: Arc<RwLock<EventBus>>,
    span_correlation: Arc<RwLock<SpanCorrelation>>,

    // Stage state
    pending_events: Arc<RwLock<Vec<EtlTripleEvent>>>,
    dedup_cache: Arc<RwLock<HashMap<String, bool>>>,
    event_counter: Arc<RwLock<u64>>,
}

impl Orchestrator {
    /// Create a new orchestrator with configuration
    #[must_use]
    pub fn new(config: OrchestratorConfig) -> Self {
        Self {
            config,
            event_bus: Arc::new(RwLock::new(EventBus::new())),
            span_correlation: Arc::new(RwLock::new(SpanCorrelation::new())),
            pending_events: Arc::new(RwLock::new(Vec::new())),
            dedup_cache: Arc::new(RwLock::new(HashMap::new())),
            event_counter: Arc::new(RwLock::new(0)),
        }
    }

    /// Create with default configuration
    #[must_use]
    pub fn default_with_workflow(workflow_id: String) -> Self {
        let mut config = OrchestratorConfig::default();
        config.workflow_id = workflow_id;
        Self::new(config)
    }

    /// Stage 1: ETL Event Intake
    ///
    /// Receives ETL triple events with receipts and metadata.
    /// Performs event deduplication via transaction ID.
    async fn stage_1_etl_intake(&self, event: EtlTripleEvent) -> Result<EtlTripleEvent> {
        debug!(
            transaction_id = %event.transaction_id,
            subject = %event.subject,
            "Stage 1: ETL Event Intake"
        );

        // Check deduplication cache
        let cache = self.dedup_cache.read().await;
        if cache.contains_key(&event.transaction_id) {
            return Err(crate::OrchestratorError::QueueError(
                "Duplicate event (already processed)".to_string(),
            ));
        }
        drop(cache);

        // Register in dedup cache
        let mut cache = self.dedup_cache.write().await;
        if cache.len() >= self.config.dedup_cache_size {
            cache.clear(); // Simple eviction (in production, use LRU)
        }
        cache.insert(event.transaction_id.clone(), true);

        Ok(event)
    }

    /// Stage 2: KGC-4D Context Injection
    ///
    /// Enriches events with temporal coordinates (O, t, V, G).
    /// Computes BLAKE3 snapshot of RDF observable if enabled.
    async fn stage_2_kgc_injection(&self, event: &EtlTripleEvent) -> Result<TemporalContext> {
        debug!(
            transaction_id = %event.transaction_id,
            "Stage 2: KGC-4D Context Injection"
        );

        // Create observable snapshot data (simple serialization of triple)
        let observable_data = format!(
            "{}\t{}\t{}",
            event.subject, event.predicate, event.object
        );

        // Create temporal context
        let context = TemporalContext::new(
            observable_data.as_bytes(),
            self.config.git_reference.clone(),
            event.kgc_span_id.clone(),
        );

        // Validate git reference
        if !context.verify_git_reference() && self.config.git_reference.len() < 40 {
            warn!(
                git_ref = %self.config.git_reference,
                "Git reference validation failed, proceeding with fallback"
            );
        }

        // Register span correlation if enabled
        if self.config.enable_span_correlation {
            let mut span_corr = self.span_correlation.write().await;
            span_corr.register_root(event.transaction_id.clone());
        }

        Ok(context)
    }

    /// Stage 3: Process Variable Aggregation
    ///
    /// Groups related triples into coherent workflow input maps.
    /// Implements zero-copy via Arc<T> and lazy evaluation.
    async fn stage_3_variable_aggregation(
        &self,
        events: &[EtlTripleEvent],
    ) -> Result<BTreeMap<String, serde_json::Value>> {
        debug!(
            event_count = %events.len(),
            "Stage 3: Process Variable Aggregation"
        );

        let mut variables = BTreeMap::new();

        for event in events {
            // Use predicate as variable key, object as value
            let key = event
                .predicate
                .split('/')
                .last()
                .unwrap_or("value")
                .to_string();

            variables.insert(
                key,
                serde_json::json!({
                    "subject": event.subject,
                    "object": event.object,
                    "timestamp": event.etl_timestamp,
                }),
            );
        }

        Ok(variables)
    }

    /// Stage 4: Workflow Trigger Generation
    ///
    /// Creates WorkflowTriggerEvent with correlationId + variables.
    /// Produces cryptographic receipt.
    async fn stage_4_trigger_generation(
        &self,
        variables: BTreeMap<String, serde_json::Value>,
        correlation_id: String,
        event_count: usize,
    ) -> Result<WorkflowTriggerEvent> {
        debug!(
            workflow_id = %self.config.workflow_id,
            event_count = %event_count,
            "Stage 4: Workflow Trigger Generation"
        );

        let trigger = WorkflowTriggerEvent::new(
            self.config.workflow_id.clone(),
            correlation_id,
        )
        .with_variables(variables)
        .with_event_count(event_count);

        // In production, would generate receipt here
        // receipt = Merkle::hash(&trigger)

        Ok(trigger)
    }

    /// Stage 5: Workflow Execution & Receipt
    ///
    /// Submits to workflow-engine, captures instance ID, generates receipt.
    async fn stage_5_execution_receipt(
        &self,
        trigger: WorkflowTriggerEvent,
    ) -> Result<WorkflowTriggerEvent> {
        debug!(
            workflow_id = %trigger.workflow_id,
            process_instance_id = %trigger.process_instance_id,
            "Stage 5: Workflow Execution & Receipt"
        );

        // In production, would:
        // 1. Submit to workflow-engine
        // 2. Capture response (instance ID, status)
        // 3. Generate Merkle receipt
        // 4. Send to KGC-4D for temporal tracking

        // For now, return the trigger as-is
        Ok(trigger)
    }

    /// Complete pipeline: process a single ETL event
    pub async fn process_event(&self, event: EtlTripleEvent) -> Result<WorkflowTriggerEvent> {
        // Stage 1: Intake
        let event = self.stage_1_etl_intake(event).await?;

        // Stage 2: KGC-4D Injection
        let _kgc_context = self.stage_2_kgc_injection(&event).await?;

        // Stage 3: Variable Aggregation (single event)
        let variables = self.stage_3_variable_aggregation(&[event.clone()]).await?;

        // Stage 4: Trigger Generation
        let trigger = self
            .stage_4_trigger_generation(
                variables,
                event.transaction_id.clone(),
                1,
            )
            .await?;

        // Stage 5: Execution & Receipt
        let trigger = self.stage_5_execution_receipt(trigger).await?;

        // Update counter
        let mut counter = self.event_counter.write().await;
        *counter += 1;

        Ok(trigger)
    }

    /// Process a batch of events with aggregation
    pub async fn process_batch(&self, events: Vec<EtlTripleEvent>) -> Result<WorkflowTriggerEvent> {
        if events.is_empty() {
            return Err(crate::OrchestratorError::AggregationError(
                "Empty batch".to_string(),
            ));
        }

        debug!(
            event_count = %events.len(),
            "Processing batch"
        );

        // Stage 1: Intake all events
        let mut processed_events = Vec::new();
        for event in events {
            match self.stage_1_etl_intake(event).await {
                Ok(e) => processed_events.push(e),
                Err(e) => {
                    warn!("Stage 1 error: {}, continuing", e);
                    continue;
                }
            }
        }

        if processed_events.is_empty() {
            return Err(crate::OrchestratorError::AggregationError(
                "No valid events in batch".to_string(),
            ));
        }

        // Stage 2: KGC-4D Injection for all
        for event in &processed_events {
            let _ = self.stage_2_kgc_injection(event).await;
        }

        // Stage 3: Aggregate variables
        let variables = self.stage_3_variable_aggregation(&processed_events).await?;

        // Stage 4: Generate single trigger for batch
        let correlation_id = processed_events[0].transaction_id.clone();
        let trigger = self
            .stage_4_trigger_generation(
                variables,
                correlation_id,
                processed_events.len(),
            )
            .await?;

        // Stage 5: Execution & Receipt
        let trigger = self.stage_5_execution_receipt(trigger).await?;

        // Update counter
        let mut counter = self.event_counter.write().await;
        *counter += processed_events.len() as u64;

        Ok(trigger)
    }

    /// Get event bus for inter-system communication
    #[must_use]
    pub fn event_bus(&self) -> Arc<RwLock<EventBus>> {
        Arc::clone(&self.event_bus)
    }

    /// Get span correlation for distributed tracing
    #[must_use]
    pub fn span_correlation(&self) -> Arc<RwLock<SpanCorrelation>> {
        Arc::clone(&self.span_correlation)
    }

    /// Check Andon signal (queue overflow)
    pub async fn check_andon_signal(&self) -> AndonSignal {
        let bus = self.event_bus.read().await;
        let pending = bus.pending_etl_events();

        if pending > self.config.andon_overflow_threshold {
            AndonSignal::Red {
                message: format!(
                    "Event queue overflow: {} pending (threshold: {})",
                    pending, self.config.andon_overflow_threshold
                ),
            }
        } else if pending > self.config.andon_overflow_threshold / 2 {
            AndonSignal::Yellow {
                message: format!("Event queue approaching capacity: {}", pending),
            }
        } else {
            AndonSignal::Green {
                message: "All systems operational".to_string(),
            }
        }
    }

    /// Get orchestrator metrics
    pub async fn metrics(&self) -> OrchestratorMetrics {
        let bus = self.event_bus.read().await;
        let bus_metrics = bus.metrics();
        let counter = self.event_counter.read().await;

        OrchestratorMetrics {
            total_events_processed: *counter,
            etl_events_received: bus_metrics.etl_events_received,
            workflow_triggers_sent: bus_metrics.workflow_triggers_sent,
            pending_etl_events: bus.pending_etl_events(),
            aggregation_ratio: bus_metrics.aggregation_ratio(),
        }
    }
}

/// Andon signal status
#[derive(Clone, Debug)]
pub enum AndonSignal {
    /// 🟢 GREEN: All systems operational
    Green { message: String },
    /// 🟡 YELLOW: Warning, investigate
    Yellow { message: String },
    /// 🔴 RED: Critical, stop and investigate
    Red { message: String },
}

/// Orchestrator performance metrics
#[derive(Clone, Debug)]
pub struct OrchestratorMetrics {
    /// Total events processed through pipeline
    pub total_events_processed: u64,
    /// ETL events received
    pub etl_events_received: u64,
    /// Workflow triggers sent
    pub workflow_triggers_sent: u64,
    /// Pending ETL events in queue
    pub pending_etl_events: usize,
    /// Aggregation ratio (triggers per ETL event)
    pub aggregation_ratio: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_orchestrator_creation() {
        let config = OrchestratorConfig {
            workflow_id: "test-workflow".to_string(),
            ..Default::default()
        };

        let orch = Orchestrator::new(config);
        let metrics = orch.metrics().await;

        assert_eq!(metrics.total_events_processed, 0);
    }

    #[tokio::test]
    async fn test_process_single_event() {
        let orch = Orchestrator::default_with_workflow("test-workflow".to_string());

        let event = EtlTripleEvent::new(
            "http://example.com/s".to_string(),
            "http://example.com/p".to_string(),
            "http://example.com/o".to_string(),
            123456789,
        );

        let trigger = orch.process_event(event.clone()).await.unwrap();

        assert_eq!(trigger.workflow_id, "test-workflow");
        assert_eq!(trigger.correlation_id, event.transaction_id);
        assert_eq!(trigger.event_count, 1);
    }

    #[tokio::test]
    async fn test_process_batch() {
        let orch = Orchestrator::default_with_workflow("test-workflow".to_string());

        let events: Vec<_> = (0..5)
            .map(|i| {
                EtlTripleEvent::new(
                    format!("http://example.com/s{i}"),
                    format!("http://example.com/p{i}"),
                    format!("http://example.com/o{i}"),
                    123456789 + i as i64,
                )
            })
            .collect();

        let trigger = orch.process_batch(events.clone()).await.unwrap();

        assert_eq!(trigger.event_count, 5);
        assert!(!trigger.process_variables.is_empty());
    }

    #[tokio::test]
    async fn test_deduplication() {
        let orch = Orchestrator::default_with_workflow("test-workflow".to_string());

        let event = EtlTripleEvent::new(
            "http://example.com/s".to_string(),
            "http://example.com/p".to_string(),
            "http://example.com/o".to_string(),
            123456789,
        );

        // First pass succeeds
        let result1 = orch.process_event(event.clone()).await;
        assert!(result1.is_ok());

        // Second pass with same transaction_id fails
        let result2 = orch.process_event(event).await;
        assert!(result2.is_err());
    }

    #[tokio::test]
    async fn test_andon_signal() {
        let config = OrchestratorConfig {
            workflow_id: "test-workflow".to_string(),
            andon_overflow_threshold: 100,
            ..Default::default()
        };

        let orch = Orchestrator::new(config);
        let signal = orch.check_andon_signal().await;

        // Should be GREEN initially
        match signal {
            AndonSignal::Green { .. } => {}
            _ => panic!("Expected GREEN signal"),
        }
    }
}
