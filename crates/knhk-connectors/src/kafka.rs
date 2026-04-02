// rust/knhk-connectors/src/kafka.rs
// Kafka connector implementation

use alloc::string::String;
use alloc::vec::Vec;

use crate::{Connector, ConnectorError, ConnectorId, ConnectorSpec, DataFormat, Delta};

/// Kafka connector for streaming data
pub struct KafkaConnector {
    id: ConnectorId,
    topic: String,
    _format: DataFormat,
    bootstrap_servers: Vec<String>,
    spec: Option<ConnectorSpec>,
}

impl KafkaConnector {
    /// Create a new Kafka connector
    pub fn new(id: String, topic: String, format: DataFormat) -> Self {
        Self {
            id,
            topic,
            _format: format,
            bootstrap_servers: Vec::new(),
            spec: None,
        }
    }

    /// Add bootstrap servers
    pub fn with_bootstrap_servers(mut self, servers: Vec<String>) -> Self {
        self.bootstrap_servers = servers;
        self
    }

    /// Get topic
    pub fn topic(&self) -> &str {
        &self.topic
    }
}

impl Connector for KafkaConnector {
    fn initialize(&mut self, spec: ConnectorSpec) -> Result<(), ConnectorError> {
        self.spec = Some(spec);
        Ok(())
    }

    fn fetch_delta(&mut self) -> Result<Delta, ConnectorError> {
        Ok(Delta {
            additions: Vec::new(),
            removals: Vec::new(),
            actor: self.id.clone(),
            timestamp_ms: 0,
        })
    }

    fn transform_to_soa(&self, _delta: &Delta) -> Result<crate::SoAArrays, ConnectorError> {
        Ok(crate::SoAArrays::new())
    }

    fn id(&self) -> &ConnectorId {
        &self.id
    }

    fn schema(&self) -> &crate::SchemaIri {
        static DEFAULT_SCHEMA: String = String::new();
        &DEFAULT_SCHEMA
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_kafka_connector_new() {
        let connector = KafkaConnector::new(
            "test".to_string(),
            "test.topic".to_string(),
            DataFormat::JsonLd,
        );
        assert_eq!(connector.id(), "test");
        assert_eq!(connector.topic(), "test.topic");
    }

    #[test]
    fn test_kafka_connector_with_servers() {
        let connector = KafkaConnector::new(
            "test".to_string(),
            "test.topic".to_string(),
            DataFormat::JsonLd,
        )
        .with_bootstrap_servers(vec!["localhost:9092".to_string()]);
        assert_eq!(connector.id(), "test");
    }
}
