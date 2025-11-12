//! Data Pipeline CLI - ETL data pipelines, transformation, and integration
//!
//! This library provides a comprehensive ETL (Extract, Transform, Load) framework
//! with support for multiple data sources, rich transformations, and flexible sinks.
//!
//! # Features
//!
//! - **Multiple Data Sources**: RDF stores, CSV files, JSON, SQL databases, REST APIs
//! - **Rich Transformations**: Map, filter, aggregate, join, and validate data
//! - **Flexible Sinks**: Write to RDF stores, databases, files, or APIs
//! - **Scheduling**: Cron expressions, intervals, or event-driven execution
//! - **Monitoring**: Real-time metrics, tracing, and performance tracking
//! - **Fault Tolerance**: Checkpointing and graceful error handling
//! - **Performance**: Parallel processing and batch optimization
//!
//! # Example
//!
//! ```rust,no_run
//! use data_pipeline_cli::{Pipeline, Source, Transform, Sink};
//!
//! #[tokio::main]
//! async fn main() -> anyhow::Result<()> {
//!     // Create pipeline
//!     let pipeline = Pipeline::builder()
//!         .name("csv-to-rdf")
//!         .source(Source::csv("users.csv")?)
//!         .transform(Transform::map(vec![
//!             ("firstName", "foaf:givenName"),
//!             ("lastName", "foaf:familyName"),
//!         ])?)
//!         .sink(Sink::rdf("oxigraph://store.db")?)
//!         .build()?;
//!
//!     // Execute pipeline
//!     pipeline.run().await?;
//!
//!     Ok(())
//! }
//! ```

pub mod pipeline;
pub mod source;
pub mod transform;
pub mod sink;
pub mod scheduler;
pub mod metrics;
pub mod checkpoint;

pub use pipeline::{Pipeline, PipelineBuilder, PipelineConfig};
pub use source::{Source, SourceType, SourceConfig};
pub use transform::{Transform, TransformType, TransformConfig};
pub use sink::{Sink, SinkType, SinkConfig};
pub use scheduler::{Scheduler, ScheduleType};
pub use metrics::{Metrics, MetricsCollector};
pub use checkpoint::{Checkpoint, CheckpointManager};

use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Record type for data flowing through pipeline
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Record {
    pub fields: HashMap<String, Value>,
}

/// Value types supported in records
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Value {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Null,
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
}

/// Batch of records for efficient processing
#[derive(Debug, Clone)]
pub struct Batch {
    pub records: Vec<Record>,
    pub metadata: BatchMetadata,
}

/// Metadata for tracking batch processing
#[derive(Debug, Clone)]
pub struct BatchMetadata {
    pub batch_id: String,
    pub source: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub record_count: usize,
}

/// Trait for data sources
#[async_trait]
pub trait DataSource: Send + Sync {
    /// Initialize the data source
    async fn initialize(&mut self) -> anyhow::Result<()>;

    /// Extract next batch of records
    async fn extract_batch(&mut self, batch_size: usize) -> anyhow::Result<Option<Batch>>;

    /// Get source schema
    async fn schema(&self) -> anyhow::Result<Schema>;

    /// Test connection
    async fn test_connection(&self) -> anyhow::Result<bool>;

    /// Close the data source
    async fn close(&mut self) -> anyhow::Result<()>;
}

/// Trait for data transformations
#[async_trait]
pub trait DataTransform: Send + Sync {
    /// Apply transformation to a batch
    async fn transform(&self, batch: Batch) -> anyhow::Result<Batch>;

    /// Validate transformation configuration
    fn validate(&self) -> anyhow::Result<()>;
}

/// Trait for data sinks
#[async_trait]
pub trait DataSink: Send + Sync {
    /// Initialize the data sink
    async fn initialize(&mut self) -> anyhow::Result<()>;

    /// Write batch to sink
    async fn write_batch(&mut self, batch: Batch) -> anyhow::Result<()>;

    /// Flush buffered data
    async fn flush(&mut self) -> anyhow::Result<()>;

    /// Test connection
    async fn test_connection(&self) -> anyhow::Result<bool>;

    /// Close the data sink
    async fn close(&mut self) -> anyhow::Result<()>;
}

/// Schema definition for data sources
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Schema {
    pub fields: Vec<Field>,
}

/// Field definition in schema
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Field {
    pub name: String,
    pub field_type: FieldType,
    pub nullable: bool,
}

/// Supported field types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FieldType {
    String,
    Integer,
    Float,
    Boolean,
    Date,
    DateTime,
    Array(Box<FieldType>),
    Object,
}

/// Error types for pipeline operations
#[derive(Debug, thiserror::Error)]
pub enum PipelineError {
    #[error("Source error: {0}")]
    Source(String),

    #[error("Transform error: {0}")]
    Transform(String),

    #[error("Sink error: {0}")]
    Sink(String),

    #[error("Configuration error: {0}")]
    Configuration(String),

    #[error("Execution error: {0}")]
    Execution(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_record_creation() {
        let mut fields = HashMap::new();
        fields.insert("name".to_string(), Value::String("Alice".to_string()));
        fields.insert("age".to_string(), Value::Integer(30));

        let record = Record { fields };

        assert_eq!(record.fields.len(), 2);
        assert!(matches!(record.fields.get("name"), Some(Value::String(_))));
    }

    #[test]
    fn test_batch_creation() {
        let records = vec![
            Record {
                fields: vec![
                    ("id".to_string(), Value::Integer(1)),
                    ("name".to_string(), Value::String("Alice".to_string())),
                ].into_iter().collect(),
            },
        ];

        let metadata = BatchMetadata {
            batch_id: uuid::Uuid::new_v4().to_string(),
            source: "test".to_string(),
            timestamp: chrono::Utc::now(),
            record_count: records.len(),
        };

        let batch = Batch { records, metadata };

        assert_eq!(batch.records.len(), 1);
        assert_eq!(batch.metadata.record_count, 1);
    }
}
