// rust/knhk-etl/src/integration.rs
// Integration layer connecting ETL pipeline with connectors, lockchain, and OTEL

#![no_std]
extern crate alloc;

use super::*;
use alloc::string::String;
use alloc::string::ToString;
use alloc::vec::Vec;
use alloc::format;
use alloc::collections::BTreeMap;

/// Integrated pipeline with all components wired together
pub struct IntegratedPipeline {
    connectors: Vec<String>,
    schema_iri: String,
    lockchain_enabled: bool,
    downstream_endpoints: Vec<String>,
}

impl IntegratedPipeline {
    pub fn new(
        connectors: Vec<String>,
        schema_iri: String,
        lockchain_enabled: bool,
        downstream_endpoints: Vec<String>,
    ) -> Self {
        Self {
            connectors,
            schema_iri,
            lockchain_enabled,
            downstream_endpoints,
        }
    }

    /// Execute pipeline with full integration
    pub fn execute(&mut self) -> Result<IntegratedResult, PipelineError> {
        // Use the base Pipeline for execution
        let pipeline = Pipeline::new(
            self.connectors.clone(),
            self.schema_iri.clone(),
            self.lockchain_enabled,
            self.downstream_endpoints.clone(),
        );
        
        let result = pipeline.execute()?;
        
        // Record OTEL metrics using proper API
        #[cfg(all(feature = "std", feature = "knhk-otel"))]
        let metrics_recorded = {
            use knhk_otel::{Tracer, Metric, MetricValue, MetricsHelper};
            use std::time::{SystemTime, UNIX_EPOCH};
            
            let mut tracer = Tracer::new();
            
            // Record pipeline execution metrics using MetricsHelper
            MetricsHelper::record_connector_throughput(&mut tracer, "pipeline", result.actions_sent);
            
            // Record receipt generation
            if result.receipts_written > 0 {
                MetricsHelper::record_receipt(&mut tracer, &format!("pipeline_batch_{}", result.receipts_written));
            }
            
            // Record lockchain writes
            let timestamp_ms = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_millis() as u64)
                .unwrap_or(0);
            
            for hash in &result.lockchain_hashes {
                let metric = Metric {
                    name: "knhk.lockchain.entry".to_string(),
                    value: MetricValue::Counter(1),
                    timestamp_ms,
                    attributes: {
                        let mut attrs = alloc::collections::BTreeMap::new();
                        attrs.insert("hash".to_string(), hash.clone());
                        attrs
                    },
                };
                tracer.record_metric(metric);
            }
            
            tracer.metrics().len()
        };
        
        #[cfg(not(all(feature = "std", feature = "knhk-otel")))]
        let metrics_recorded = 0;
        
        Ok(IntegratedResult {
            receipts_written: result.receipts_written,
            actions_sent: result.actions_sent,
            lockchain_hashes: result.lockchain_hashes,
            metrics_recorded,
        })
    }
}

pub struct IntegratedResult {
    pub receipts_written: usize,
    pub actions_sent: usize,
    pub lockchain_hashes: Vec<String>,
    pub metrics_recorded: usize,
}
