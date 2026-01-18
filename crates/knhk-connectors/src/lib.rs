// rust/knhk-connectors/src/lib.rs
// Dark Matter 80/20 Connector Framework
// Provides typed, validated connectors for enterprise data sources

#![no_std]
extern crate alloc;

use alloc::string::String;
use alloc::vec::Vec;
use alloc::collections::BTreeMap;

#[cfg(feature = "std")]
use hashbrown::HashMap;
#[cfg(not(feature = "std"))]
use alloc::collections::BTreeMap as HashMap;

/// Connector identifier
pub type ConnectorId = String;

/// Schema IRI for type validation
pub type SchemaIri = String;

/// Source specification
#[derive(Debug, Clone)]
pub enum SourceType {
    Kafka {
        topic: String,
        format: DataFormat,
        bootstrap_servers: Vec<String>,
    },
    Http {
        url: String,
        format: DataFormat,
        headers: BTreeMap<String, String>,
    },
    File {
        path: String,
        format: DataFormat,
    },
    Salesforce {
        instance_url: String,
        api_version: String,
        object_type: String,
    },
    Sap {
        endpoint: String,
        client: String,
        format: DataFormat,
    },
}

/// Data format for parsing
#[derive(Debug, Clone)]
pub enum DataFormat {
    RdfTurtle,
    JsonLd,
    Json,
    Csv,
    Xml,
}

/// S/P/O/G mapping configuration
#[derive(Debug, Clone)]
pub struct Mapping {
    pub subject: String,      // S field path/mapping
    pub predicate: String,    // P field path/mapping
    pub object: String,       // O field path/mapping
    pub graph: Option<String>, // G (optional graph context)
}

/// Admission guards (H constraints)
#[derive(Debug, Clone)]
pub struct Guards {
    pub max_batch_size: usize,      // Max Δ batch size
    pub max_lag_ms: u64,            // Max ingestion lag
    pub max_run_len: usize,         // Must be ≤ 8 for hot path
    pub schema_validation: bool,    // Enforce Σ typing
}

/// Connector specification
#[derive(Debug, Clone)]
pub struct ConnectorSpec {
    pub name: ConnectorId,
    pub schema: SchemaIri,
    pub source: SourceType,
    pub mapping: Mapping,
    pub guards: Guards,
}

/// Delta (Δ) representing additions/removals
#[derive(Debug, Clone)]
pub struct Delta {
    pub additions: Vec<Triple>,
    pub removals: Vec<Triple>,
    pub actor: String,
    pub timestamp_ms: u64,
}

/// RDF triple (S, P, O, G)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Triple {
    pub subject: u64,    // Hashed IRI
    pub predicate: u64,  // Hashed IRI
    pub object: u64,     // Hashed value
    pub graph: Option<u64>, // Optional graph context
}

/// SoA arrays for hot path (64-byte aligned)
#[repr(align(64))]
pub struct SoAArrays {
    pub s: [u64; 8],
    pub p: [u64; 8],
    pub o: [u64; 8],
}

impl SoAArrays {
    pub fn new() -> Self {
        Self {
            s: [0; 8],
            p: [0; 8],
            o: [0; 8],
        }
    }

    /// Convert triples to SoA layout (run.len ≤ 8)
    pub fn from_triples(triples: &[Triple], max_len: usize) -> Self {
        let mut arrays = Self::new();
        let len = core::cmp::min(triples.len(), max_len);
        for i in 0..len {
            arrays.s[i] = triples[i].subject;
            arrays.p[i] = triples[i].predicate;
            arrays.o[i] = triples[i].object;
        }
        arrays
    }
}

/// Connector health status
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConnectorHealth {
    Healthy,
    Degraded(String),
    Unhealthy(String),
}

/// Connector lifecycle state
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConnectorState {
    Stopped,
    Starting,
    Running,
    Stopping,
    Error(String),
}

/// Connector trait - all connectors implement this
pub trait Connector {
    /// Initialize connector with spec
    fn initialize(&mut self, spec: ConnectorSpec) -> Result<(), ConnectorError>;

    /// Fetch next delta batch (validated, typed)
    fn fetch_delta(&mut self) -> Result<Delta, ConnectorError>;

    /// Transform delta to SoA arrays (for hot path)
    fn transform_to_soa(&self, delta: &Delta) -> Result<SoAArrays, ConnectorError>;

    /// Get connector ID
    fn id(&self) -> &ConnectorId;

    /// Get schema IRI
    fn schema(&self) -> &SchemaIri;

    /// Check connector health
    fn health(&self) -> ConnectorHealth {
        ConnectorHealth::Healthy
    }

    /// Start connector (if applicable)
    fn start(&mut self) -> Result<(), ConnectorError> {
        Ok(())
    }

    /// Stop connector (if applicable)
    fn stop(&mut self) -> Result<(), ConnectorError> {
        Ok(())
    }
}

/// Connector errors
#[derive(Debug)]
pub enum ConnectorError {
    ValidationFailed(String),
    SchemaMismatch(String),
    GuardViolation(String),
    ParseError(String),
    IoError(String),
    NetworkError(String),
}

/// Circuit breaker state
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CircuitBreakerState {
    Closed,      // Normal operation
    Open,        // Failing, rejecting requests
    HalfOpen,   // Testing if recovered
}

/// Circuit breaker for connector failure handling
pub struct CircuitBreaker {
    state: CircuitBreakerState,
    failure_count: u32,
    failure_threshold: u32,
    success_count: u32,
    success_threshold: u32,
    last_failure_time_ms: u64,
    reset_timeout_ms: u64,
}

impl CircuitBreaker {
    pub fn new(failure_threshold: u32, reset_timeout_ms: u64) -> Self {
        Self {
            state: CircuitBreakerState::Closed,
            failure_count: 0,
            failure_threshold,
            success_count: 0,
            success_threshold: 1,
            last_failure_time_ms: 0,
            reset_timeout_ms,
        }
    }

    pub fn call<F, T>(&mut self, f: F) -> Result<T, ConnectorError>
    where
        F: FnOnce() -> Result<T, ConnectorError>,
    {
        match self.state {
            CircuitBreakerState::Open => {
                // Check if reset timeout has passed
                let current_time_ms = Self::get_current_time_ms();
                if current_time_ms - self.last_failure_time_ms >= self.reset_timeout_ms {
                    self.state = CircuitBreakerState::HalfOpen;
                    self.success_count = 0;
                } else {
                    return Err(ConnectorError::NetworkError(
                        "Circuit breaker is open".to_string()
                    ));
                }
            }
            CircuitBreakerState::HalfOpen => {
                // Already in half-open, proceed
            }
            CircuitBreakerState::Closed => {
                // Normal operation
            }
        }

        match f() {
            Ok(result) => {
                self.success_count += 1;
                if self.state == CircuitBreakerState::HalfOpen {
                    if self.success_count >= self.success_threshold {
                        self.state = CircuitBreakerState::Closed;
                        self.failure_count = 0;
                    }
                }
                Ok(result)
            }
            Err(e) => {
                self.failure_count += 1;
                self.last_failure_time_ms = Self::get_current_time_ms();
                
                if self.failure_count >= self.failure_threshold {
                    self.state = CircuitBreakerState::Open;
                }
                
                Err(e)
            }
        }
    }

    pub fn state(&self) -> &CircuitBreakerState {
        &self.state
    }

    fn get_current_time_ms() -> u64 {
        #[cfg(feature = "std")]
        {
            use std::time::{SystemTime, UNIX_EPOCH};
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_millis() as u64
        }
        #[cfg(not(feature = "std"))]
        {
            0
        }
    }
}

/// Connector metrics
#[derive(Debug, Clone, Default)]
pub struct ConnectorMetrics {
    pub total_requests: u64,
    pub successful_requests: u64,
    pub failed_requests: u64,
    pub total_deltas_fetched: u64,
    pub total_triples_processed: u64,
    pub last_request_time_ms: u64,
    pub last_error: Option<String>,
}

/// Connector registry
pub struct ConnectorRegistry {
    connectors: BTreeMap<ConnectorId, Box<dyn Connector>>,
    circuit_breakers: BTreeMap<ConnectorId, CircuitBreaker>,
    metrics: BTreeMap<ConnectorId, ConnectorMetrics>,
}

impl ConnectorRegistry {
    pub fn new() -> Self {
        Self {
            connectors: BTreeMap::new(),
            circuit_breakers: BTreeMap::new(),
            metrics: BTreeMap::new(),
        }
    }

    /// Register a connector
    pub fn register(&mut self, connector: Box<dyn Connector>) -> Result<(), ConnectorError> {
        let id = connector.id().clone();
        if self.connectors.contains_key(&id) {
            return Err(ConnectorError::ValidationFailed(
                format!("Connector {} already registered", id)
            ));
        }
        
        // Initialize circuit breaker for connector
        self.circuit_breakers.insert(id.clone(), CircuitBreaker::new(5, 60000));
        
        // Initialize metrics
        self.metrics.insert(id.clone(), ConnectorMetrics::default());
        
        self.connectors.insert(id, connector);
        Ok(())
    }

    /// Get connector by ID
    pub fn get(&self, id: &ConnectorId) -> Option<&dyn Connector> {
        self.connectors.get(id).map(|c| c.as_ref())
    }

    /// Get mutable connector by ID
    pub fn get_mut(&mut self, id: &ConnectorId) -> Option<&mut dyn Connector> {
        self.connectors.get_mut(id).map(|c| c.as_mut())
    }

    /// List all connector IDs
    pub fn list(&self) -> Vec<ConnectorId> {
        self.connectors.keys().cloned().collect()
    }

    /// Fetch delta from connector with circuit breaker protection
    pub fn fetch_delta(&mut self, id: &ConnectorId) -> Result<Delta, ConnectorError> {
        let connector = self.connectors.get_mut(id)
            .ok_or_else(|| ConnectorError::ValidationFailed(
                format!("Connector {} not found", id)
            ))?;

        let circuit_breaker = self.circuit_breakers.get_mut(id)
            .ok_or_else(|| ConnectorError::ValidationFailed(
                format!("Circuit breaker for connector {} not found", id)
            ))?;

        let metrics = self.metrics.get_mut(id)
            .ok_or_else(|| ConnectorError::ValidationFailed(
                format!("Metrics for connector {} not found", id)
            ))?;

        let current_time_ms = Self::get_current_time_ms();
        metrics.total_requests += 1;
        metrics.last_request_time_ms = current_time_ms;

        let result = circuit_breaker.call(|| connector.fetch_delta());

        match &result {
            Ok(delta) => {
                metrics.successful_requests += 1;
                metrics.total_deltas_fetched += 1;
                metrics.total_triples_processed += delta.additions.len() as u64;
                metrics.last_error = None;
            }
            Err(e) => {
                metrics.failed_requests += 1;
                metrics.last_error = Some(format!("{:?}", e));
            }
        }

        result
    }

    /// Check connector health
    pub fn health(&self, id: &ConnectorId) -> Option<ConnectorHealth> {
        self.connectors.get(id).map(|c| c.health())
    }

    /// Start connector
    pub fn start(&mut self, id: &ConnectorId) -> Result<(), ConnectorError> {
        let connector = self.connectors.get_mut(id)
            .ok_or_else(|| ConnectorError::ValidationFailed(
                format!("Connector {} not found", id)
            ))?;

        connector.start()
    }

    /// Stop connector
    pub fn stop(&mut self, id: &ConnectorId) -> Result<(), ConnectorError> {
        let connector = self.connectors.get_mut(id)
            .ok_or_else(|| ConnectorError::ValidationFailed(
                format!("Connector {} not found", id)
            ))?;

        connector.stop()
    }

    /// Get connector metrics
    pub fn metrics(&self, id: &ConnectorId) -> Option<&ConnectorMetrics> {
        self.metrics.get(id)
    }

    /// Get circuit breaker state
    pub fn circuit_breaker_state(&self, id: &ConnectorId) -> Option<&CircuitBreakerState> {
        self.circuit_breakers.get(id).map(|cb| cb.state())
    }

    fn get_current_time_ms() -> u64 {
        #[cfg(feature = "std")]
        {
            use std::time::{SystemTime, UNIX_EPOCH};
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_millis() as u64
        }
        #[cfg(not(feature = "std"))]
        {
            0
        }
    }
}

pub mod kafka;
pub mod salesforce;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_soa_from_triples() {
        let triples = vec![
            Triple {
                subject: 0xA11CE,
                predicate: 0xC0FFEE,
                object: 0xB0B,
                graph: None,
            },
            Triple {
                subject: 0xB22FF,
                predicate: 0xC0FFEE,
                object: 0xC0C,
                graph: None,
            },
        ];

        let soa = SoAArrays::from_triples(&triples, 8);
        assert_eq!(soa.s[0], 0xA11CE);
        assert_eq!(soa.p[0], 0xC0FFEE);
        assert_eq!(soa.o[0], 0xB0B);
        assert_eq!(soa.s[1], 0xB22FF);
    }

    #[test]
    fn test_connector_registry() {
        let mut registry = ConnectorRegistry::new();
        assert_eq!(registry.list().len(), 0);
    }

    #[test]
    fn test_circuit_breaker() {
        let mut cb = CircuitBreaker::new(3, 1000);
        
        // Successful calls
        let result1 = cb.call(|| Ok(42));
        assert!(result1.is_ok());
        assert_eq!(*cb.state(), CircuitBreakerState::Closed);
        
        // Failures
        let result2 = cb.call(|| Err(ConnectorError::NetworkError("test".to_string())));
        assert!(result2.is_err());
        
        let result3 = cb.call(|| Err(ConnectorError::NetworkError("test".to_string())));
        assert!(result3.is_err());
        
        let result4 = cb.call(|| Err(ConnectorError::NetworkError("test".to_string())));
        assert!(result4.is_err());
        
        // Should be open now
        assert_eq!(*cb.state(), CircuitBreakerState::Open);
        
        // Calls should fail immediately
        let result5 = cb.call(|| Ok(42));
        assert!(result5.is_err());
    }

    #[test]
    fn test_connector_registry_with_circuit_breaker() {
        use crate::kafka::KafkaConnector;
        
        let mut registry = ConnectorRegistry::new();
        let connector = Box::new(KafkaConnector::new(
            "test_kafka".to_string(),
            "test.topic".to_string(),
            DataFormat::JsonLd,
        ));
        
        assert!(registry.register(connector).is_ok());
        assert_eq!(registry.list().len(), 1);
        
        // Test circuit breaker state
        let state = registry.circuit_breaker_state(&"test_kafka".to_string());
        assert!(state.is_some());
        assert_eq!(*state.unwrap(), CircuitBreakerState::Closed);
    }

    #[test]
    fn test_connector_metrics() {
        use crate::kafka::KafkaConnector;
        
        let mut registry = ConnectorRegistry::new();
        let connector = Box::new(KafkaConnector::new(
            "test_kafka".to_string(),
            "test.topic".to_string(),
            DataFormat::JsonLd,
        ));
        
        registry.register(connector).unwrap();
        
        // Get metrics (should be zero initially)
        let metrics = registry.metrics(&"test_kafka".to_string());
        assert!(metrics.is_some());
        let m = metrics.unwrap();
        assert_eq!(m.total_requests, 0);
        assert_eq!(m.successful_requests, 0);
        assert_eq!(m.failed_requests, 0);
    }
}

