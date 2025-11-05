// rust/knhks-etl/src/integration.rs
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
        
        Ok(IntegratedResult {
            receipts_written: result.receipts_written,
            actions_sent: result.actions_sent,
            lockchain_hashes: result.lockchain_hashes,
            metrics_recorded: 0, // TODO: integrate OTEL metrics when available
        })
    }
}

pub struct IntegratedResult {
    pub receipts_written: usize,
    pub actions_sent: usize,
    pub lockchain_hashes: Vec<String>,
    pub metrics_recorded: usize,
}
