# knhk-connectors Architecture Design

## Overview

This document details the architecture design for the `knhk-connectors` crate, which provides enterprise-grade connectors for Kafka and Salesforce. The framework implements a dark matter 80/20 approach with typed validation, circuit breaker patterns, and high-performance data processing.

## Core Architecture

### System Components

```rust
/// Connector Framework Core - Central coordinator for all connector operations
pub struct ConnectorFramework {
    /// Registry of all active connectors
    registry: ConnectorRegistry,
    /// Circuit breaker manager for fault tolerance
    circuit_breaker_manager: CircuitBreakerManager,
    /// Metrics collection and monitoring
    metrics: MetricsCollector,
    /// Health monitoring system
    health_monitor: HealthMonitor,
    /// Configuration management
    config: ConfigManager,
    /// Security framework
    security: SecurityFramework,
}

/// Connector Interface - All connectors implement this trait
pub trait Connector: Send + Sync {
    /// Initialize connector with specification
    fn initialize(&mut self, spec: ConnectorSpec) -> Result<(), ConnectorError>;

    /// Fetch next delta batch (validated, typed)
    fn fetch_delta(&mut self) -> Result<Delta, ConnectorError>;

    /// Transform delta to SoA arrays (hot path optimization)
    fn transform_to_soa(&self, delta: &Delta) -> Result<SoAArrays, ConnectorError>;

    /// Get connector ID
    fn id(&self) -> &ConnectorId;

    /// Get schema IRI
    fn schema(&self) -> &SchemaIri;

    /// Check connector health
    fn health(&self) -> ConnectorHealth;

    /// Start connector (if applicable)
    fn start(&mut self) -> Result<(), ConnectorError> {
        Ok(())
    }

    /// Stop connector (if applicable)
    fn stop(&mut self) -> Result<(), ConnectorError> {
        Ok(())
    }
}

/// Connector Specification - Immutable configuration
#[derive(Debug, Clone)]
pub struct ConnectorSpec {
    /// Unique connector identifier
    pub name: ConnectorId,
    /// Schema IRI for type validation
    pub schema: SchemaIri,
    /// Source configuration
    pub source: SourceType,
    /// S/P/O/G mapping configuration
    pub mapping: Mapping,
    /// Admission guards (H constraints)
    pub guards: Guards,
}

/// Source Types - Supported data sources
#[derive(Debug, Clone)]
pub enum SourceType {
    /// Apache Kafka source
    Kafka {
        topic: String,
        format: DataFormat,
        bootstrap_servers: Vec<String>,
        consumer_config: Option<KafkaConsumerConfig>,
    },
    /// HTTP/HTTPS source
    Http {
        url: String,
        format: DataFormat,
        headers: BTreeMap<String, String>,
        method: HttpMethod,
        auth: Option<HttpAuth>,
    },
    /// File system source
    File {
        path: String,
        format: DataFormat,
        watch_mode: bool,
        pattern: Option<String>,
    },
    /// Salesforce source
    Salesforce {
        instance_url: String,
        api_version: String,
        object_type: String,
        soql_query: Option<String>,
        auth: SalesforceAuth,
    },
    /// SAP source
    Sap {
        endpoint: String,
        client: String,
        format: DataFormat,
        system_id: String,
    },
}

/// Data Formats for parsing and serialization
#[derive(Debug, Clone, PartialEq)]
pub enum DataFormat {
    /// RDF Turtle format
    RdfTurtle,
    /// JSON-LD format
    JsonLd,
    /// Regular JSON format
    Json,
    /// CSV format
    Csv,
    /// XML format
    Xml,
}

/// Mapping Configuration for S/P/O/G transformation
#[derive(Debug, Clone)]
pub struct Mapping {
    /// Subject field path/mapping
    pub subject: String,
    /// Predicate field path/mapping
    pub predicate: String,
    /// Object field path/mapping
    pub object: String,
    /// Graph (optional graph context)
    pub graph: Option<String>,
}

/// Admission Guards - Constraint validation
#[derive(Debug, Clone)]
pub struct Guards {
    /// Maximum batch size
    pub max_batch_size: usize,
    /// Maximum ingestion lag (milliseconds)
    pub max_lag_ms: u64,
    /// Maximum run length (hot path optimization)
    pub max_run_len: usize,
    /// Schema validation required
    pub schema_validation: bool,
}
```

### Delta Processing Architecture

```rust
/// Delta (Δ) - Represents additions/removals
#[derive(Debug, Clone)]
pub struct Delta {
    /// Added triples
    pub additions: Vec<Triple>,
    /// Removed triples
    pub removals: Vec<Triple>,
    /// Actor who made the change
    pub actor: String,
    /// Timestamp in milliseconds
    pub timestamp_ms: u64,
    /// Metadata about the change
    pub metadata: ChangeMetadata,
}

/// RDF Triple (S, P, O, G)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Triple {
    /// Hashed IRI (64-bit)
    pub subject: u64,
    /// Hashed IRI (64-bit)
    pub predicate: u64,
    /// Hashed value (64-bit)
    pub object: u64,
    /// Optional graph context (hashed)
    pub graph: Option<u64>,
}

/// SoA Arrays - Structure of Arrays for hot path (64-byte aligned)
#[repr(align(64))]
pub struct SoAArrays {
    /// Subject array
    pub s: [u64; 8],
    /// Predicate array
    pub p: [u64; 8],
    /// Object array
    pub o: [u64; 8],
    /// Optional graph array
    pub g: [Option<u64>; 8],
    /// Length of valid data
    pub len: usize,
}

impl SoAArrays {
    /// Create new SoA arrays
    pub fn new() -> Self {
        Self {
            s: [0; 8],
            p: [0; 8],
            o: [0; 8],
            g: [None; 8],
            len: 0,
        }
    }

    /// Convert triples to SoA layout (run.len ≤ 8 for hot path)
    pub fn from_triples(triples: &[Triple], max_len: usize) -> Self {
        let mut arrays = Self::new();
        let len = core::cmp::min(triples.len(), max_len).min(8);

        arrays.len = len;
        for i in 0..len {
            arrays.s[i] = triples[i].subject;
            arrays.p[i] = triples[i].predicate;
            arrays.o[i] = triples[i].object;
            arrays.g[i] = triples[i].graph;
        }

        arrays
    }

    /// Convert back to triples
    pub fn to_triples(&self) -> Vec<Triple> {
        (0..self.len).map(|i| Triple {
            subject: self.s[i],
            predicate: self.p[i],
            object: self.o[i],
            graph: self.g[i],
        }).collect()
    }
}

/// Change Metadata - Additional information about changes
#[derive(Debug, Clone)]
pub struct ChangeMetadata {
    /// Source system
    pub source: String,
    /// Change type
    pub change_type: ChangeType,
    /// Processing timestamp
    pub processed_at: u64,
    /// Additional metadata
    pub properties: BTreeMap<String, String>,
}

/// Change Types
#[derive(Debug, Clone, PartialEq)]
pub enum ChangeType {
    /// Insert operation
    Insert,
    /// Update operation
    Update,
    /// Delete operation
    Delete,
    /// Batch operation
    Batch,
}
```

### Kafka Connector Implementation

```rust
/// Kafka Connector - Apache Kafka data source connector
pub struct KafkaConnector {
    /// Client configuration
    client_config: KafkaClientConfig,
    /// Consumer instance
    consumer: Option<KafkaConsumer>,
    /// Configuration
    config: KafkaConnectorConfig,
    /// Circuit breaker for fault tolerance
    circuit_breaker: CircuitBreaker,
    /// Metrics collector
    metrics: KafkaMetrics,
    /// Cache for SPARQL queries
    query_cache: QueryCache,
    /// Rate limiter
    rate_limiter: RateLimiter,
}

/// Kafka Connector Configuration
#[derive(Debug, Clone)]
pub struct KafkaConnectorConfig {
    /// Topic name
    pub topic: String,
    /// Data format
    pub format: DataFormat,
    /// Bootstrap servers
    pub bootstrap_servers: Vec<String>,
    /// Consumer configuration
    pub consumer_config: KafkaConsumerConfig,
    /// Mapping configuration
    pub mapping: Mapping,
    /// Guards for validation
    pub guards: Guards,
}

impl Connector for KafkaConnector {
    fn initialize(&mut self, spec: ConnectorSpec) -> Result<(), ConnectorError> {
        // Validate specification
        if let SourceType::Kafka { topic, format, bootstrap_servers, .. } = spec.source {
            self.config = KafkaConnectorConfig {
                topic,
                format,
                bootstrap_servers,
                consumer_config: KafkaConsumerConfig::default(),
                mapping: spec.mapping,
                guards: spec.guards,
            };
        } else {
            return Err(ConnectorError::InvalidSourceType("Kafka".to_string()));
        }

        // Initialize consumer
        self.consumer = Some(self.create_consumer()?);

        // Initialize circuit breaker
        self.circuit_breaker = CircuitBreaker::new(5, 60000); // 5 failures, 60s reset

        // Initialize metrics
        self.metrics = KafkaMetrics::default();

        Ok(())
    }

    fn fetch_delta(&mut self) -> Result<Delta, ConnectorError> {
        // Rate-limited fetch
        self.rate_limiter.throttle(|| {
            // Circuit breaker protected fetch
            self.circuit_breaker.call(|| {
                self.fetch_delta_internal()
            })
        })
    }

    fn transform_to_soa(&self, delta: &Delta) -> Result<SoAArrays, ConnectorError> {
        // Hot-path optimization with SoA layout
        if delta.additions.len() > 8 {
            return Err(ConnectorError::BatchTooLarge(delta.additions.len()));
        }

        Ok(SoAArrays::from_triples(&delta.additions, 8))
    }

    fn health(&self) -> ConnectorHealth {
        match self.consumer.as_ref() {
            Some(consumer) => match consumer.check_health() {
                Ok(_) => ConnectorHealth::Healthy,
                Err(e) => ConnectorHealth::Degraded(format!("Consumer error: {}", e)),
            },
            None => ConnectorHealth::Unhealthy("Consumer not initialized".to_string()),
        }
    }
}

impl KafkaConnector {
    /// Create Kafka consumer
    fn create_consumer(&self) -> Result<KafkaConsumer, ConnectorError> {
        // Create consumer with configuration
        let consumer = KafkaConsumer::new(&self.config.consumer_config)
            .map_err(|e| ConnectorError::InitializationFailed(e.to_string()))?;

        // Subscribe to topic
        consumer.subscribe(&[&self.config.topic])
            .map_err(|e| ConnectorError::InitializationFailed(e.to_string()))?;

        Ok(consumer)
    }

    /// Internal delta fetch implementation
    fn fetch_delta_internal(&mut self) -> Result<Delta, ConnectorError> {
        let consumer = self.consumer.as_mut()
            .ok_or(ConnectorError::NotInitialized)?;

        // Fetch messages
        let messages = consumer.poll_batch(self.config.guards.max_batch_size)
            .map_err(|e| ConnectorError::FetchFailed(e.to_string()))?;

        if messages.is_empty() {
            return Ok(Delta::empty());
        }

        // Transform messages to triples
        let mut additions = Vec::new();
        let mut removals = Vec::new();

        for message in messages {
            let triple = self.transform_message_to_triple(&message)?;
            additions.push(triple);
        }

        // Validate batch size
        if additions.len() > self.config.guards.max_batch_size {
            return Err(ConnectorError::BatchTooLarge(additions.len()));
        }

        // Update metrics
        self.metrics.total_messages_processed += additions.len() as u64;
        self.metrics.last_fetch_time = current_timestamp();

        Ok(Delta {
            additions,
            removals: Vec::new(), // Kafka is append-only
            actor: "kafka-connector".to_string(),
            timestamp_ms: current_timestamp(),
            metadata: ChangeMetadata {
                source: "kafka".to_string(),
                change_type: ChangeType::Batch,
                processed_at: current_timestamp(),
                properties: BTreeMap::new(),
            },
        })
    }

    /// Transform Kafka message to triple
    fn transform_message_to_triple(&self, message: &KafkaMessage) -> Result<Triple, ConnectorError> {
        // Parse message based on format
        let data = match self.config.format {
            DataFormat::Json => self.parse_json_message(message)?,
            DataFormat::RdfTurtle => self.parse_turtle_message(message)?,
            DataFormat::JsonLd => self.parse_jsonld_message(message)?,
            DataFormat::Csv => self.parse_csv_message(message)?,
            DataFormat::Xml => self.parse_xml_message(message)?,
        };

        // Apply mapping to extract S/P/O
        let subject = self.extract_field(&data, &self.config.mapping.subject)?;
        let predicate = self.extract_field(&data, &self.config.mapping.predicate)?;
        let object = self.extract_field(&data, &self.config.mapping.object)?;
        let graph = self.config.mapping.graph.as_ref()
            .map(|g| self.extract_field(&data, g))
            .transpose()?;

        // Hash the values
        Ok(Triple {
            subject: hash_iri(&subject),
            predicate: hash_iri(&predicate),
            object: hash_value(&object),
            graph: graph.map(hash_iri),
        })
    }

    /// Parse JSON message
    fn parse_json_message(&self, message: &KafkaMessage) -> Result<Value, ConnectorError> {
        let json: Value = serde_json::from_slice(message.payload())
            .map_err(|e| ConnectorError::ParseError(e.to_string()))?;
        Ok(json)
    }

    /// Extract field from JSON using path
    fn extract_field(&self, data: &Value, path: &str) -> Result<String, ConnectorError> {
        // Implement JSON path extraction
        let parts: Vec<&str> = path.split('.').collect();
        let mut current = data;

        for part in parts {
            match current {
                Value::Object(map) => {
                    current = map.get(part)
                        .ok_or_else(|| ConnectorError::FieldNotFound(part.to_string()))?;
                }
                Value::Array(arr) => {
                    let index = part.parse::<usize>()
                        .map_err(|_| ConnectorError::FieldNotFound(part.to_string()))?;
                    current = arr.get(index)
                        .ok_or_else(|| ConnectorError::FieldNotFound(part.to_string()))?;
                }
                _ => return Err(ConnectorError::FieldNotFound(part.to_string())),
            }
        }

        Ok(current.to_string())
    }
}
```

### Salesforce Connector Implementation

```rust
/// Salesforce Connector - Salesforce data source connector
pub struct SalesforceConnector {
    /// Salesforce client
    client: SalesforceClient,
    /// Query builder for SOQL queries
    query_builder: SOQLQueryBuilder,
    /// Configuration
    config: SalesforceConnectorConfig,
    /// Rate limiter
    rate_limiter: RateLimiter,
    /// Query cache
    query_cache: QueryCache,
    /// Metrics
    metrics: SalesforceMetrics,
}

/// Salesforce Connector Configuration
#[derive(Debug, Clone)]
pub struct SalesforceConnectorConfig {
    /// Instance URL
    pub instance_url: String,
    /// API version
    pub api_version: String,
    /// Object type
    pub object_type: String,
    /// SOQL query (optional)
    pub soql_query: Option<String>,
    /// Authentication configuration
    pub auth: SalesforceAuth,
    /// Mapping configuration
    pub mapping: Mapping,
    /// Guards for validation
    pub guards: Guards,
}

/// Salesforce Authentication
#[derive(Debug, Clone)]
pub enum SalesforceAuth {
    /// OAuth2 authentication
    OAuth2 { access_token: String, instance_url: String },
    /// Username/password authentication
    UsernamePassword { username: String, password: String, security_token: String },
    /// JWT bearer authentication
    JWTBearer { client_id: String, client_secret: String, username: String, private_key: String },
}

impl Connector for SalesforceConnector {
    fn initialize(&mut self, spec: ConnectorSpec) -> Result<(), ConnectorError> {
        // Validate specification
        if let SourceType::Salesforce { instance_url, api_version, object_type, .. } = spec.source {
            self.config = SalesforceConnectorConfig {
                instance_url,
                api_version,
                object_type,
                soql_query: None,
                auth: self.parse_auth_from_spec(&spec)?,
                mapping: spec.mapping,
                guards: spec.guards,
            };
        } else {
            return Err(ConnectorError::InvalidSourceType("Salesforce".to_string()));
        }

        // Initialize client
        self.client = SalesforceClient::new(&self.config)
            .map_err(|e| ConnectorError::InitializationFailed(e.to_string()))?;

        // Initialize rate limiter
        self.rate_limiter = RateLimiter::new(100, 1000); // 100 requests per second

        // Initialize metrics
        self.metrics = SalesforceMetrics::default();

        Ok(())
    }

    fn fetch_delta(&mut self) -> Result<Delta, ConnectorError> {
        // Rate-limited fetch
        self.rate_limiter.throttle(|| {
            self.fetch_salesforce_data()
        })
    }

    fn transform_to_soa(&self, delta: &Delta) -> Result<SoAArrays, ConnectorError> {
        // Hot-path optimization with SoA layout
        if delta.additions.len() > 8 {
            return Err(ConnectorError::BatchTooLarge(delta.additions.len()));
        }

        Ok(SoAArrays::from_triples(&delta.additions, 8))
    }

    fn health(&self) -> ConnectorHealth {
        match self.client.test_connection() {
            Ok(_) => ConnectorHealth::Healthy,
            Err(e) => ConnectorHealth::Degraded(format!("Salesforce connection error: {}", e)),
        }
    }
}

impl SalesforceConnector {
    /// Parse authentication from specification
    fn parse_auth_from_spec(&self, spec: &ConnectorSpec) -> Result<SalesforceAuth, ConnectorError> {
        // Parse additional authentication details from spec
        match &spec.source {
            SourceType::Salesforce { auth, .. } => match auth {
                SalesforceAuthConfig::OAuth2 { access_token, instance_url } => {
                    Ok(SalesforceAuth::OAuth2 {
                        access_token: access_token.clone(),
                        instance_url: instance_url.clone(),
                    })
                }
                // ... other auth types
            },
            _ => Err(ConnectorError::InvalidSourceType("Salesforce".to_string())),
        }
    }

    /// Fetch data from Salesforce
    fn fetch_salesforce_data(&mut self) -> Result<Delta, ConnectorError> {
        // Build SOQL query
        let query = self.query_builder.build_soql_query(&self.config.object_type, &self.config.mapping)
            .map_err(|e| ConnectorError::QueryBuildFailed(e.to_string()))?;

        // Execute query with pagination
        let mut all_records = Vec::new();
        let mut query_locator = None;

        loop {
            let mut query = query.clone();
            if let Some(locator) = query_locator.take() {
                query = format!("{} {}", query, locator);
            }

            let records = self.client.query(&query)
                .map_err(|e| ConnectorError::QueryFailed(e.to_string()))?;

            all_records.extend(records.records.clone());

            // Check if there are more records
            if records.done {
                break;
            }
            query_locator = records.next_records_url.clone();
        }

        if all_records.is_empty() {
            return Ok(Delta::empty());
        }

        // Transform records to triples
        let mut additions = Vec::new();
        for record in all_records {
            let triple = self.transform_record_to_triple(&record)?;
            additions.push(triple);
        }

        // Validate batch size
        if additions.len() > self.config.guards.max_batch_size {
            return Err(ConnectorError::BatchTooLarge(additions.len()));
        }

        // Update metrics
        self.metrics.total_records_processed += additions.len() as u64;
        self.metrics.last_query_time = current_timestamp();

        Ok(Delta {
            additions,
            removals: Vec::new(), // Salesforce is append-only
            actor: "salesforce-connector".to_string(),
            timestamp_ms: current_timestamp(),
            metadata: ChangeMetadata {
                source: "salesforce".to_string(),
                change_type: ChangeType::Batch,
                processed_at: current_timestamp(),
                properties: BTreeMap::new(),
            },
        })
    }

    /// Transform Salesforce record to triple
    fn transform_record_to_triple(&self, record: &SalesforceRecord) -> Result<Triple, ConnectorError> {
        // Extract fields based on mapping
        let subject = self.extract_field_from_record(record, &self.config.mapping.subject)?;
        let predicate = self.extract_field_from_record(record, &self.config.mapping.predicate)?;
        let object = self.extract_field_from_record(record, &self.config.mapping.object)?;
        let graph = self.config.mapping.graph.as_ref()
            .map(|g| self.extract_field_from_record(record, g))
            .transpose()?;

        // Hash the values
        Ok(Triple {
            subject: hash_iri(&subject),
            predicate: hash_iri(&predicate),
            object: hash_value(&object),
            graph: graph.map(hash_iri),
        })
    }

    /// Extract field from Salesforce record
    fn extract_field_from_record(&self, record: &SalesforceRecord, field: &str) -> Result<String, ConnectorError> {
        let value = record.fields.get(field)
            .ok_or_else(|| ConnectorError::FieldNotFound(field.to_string()))?;
        Ok(value.to_string())
    }
}
```

### Circuit Breaker Implementation

```rust
/// Circuit Breaker - Fault tolerance mechanism
pub struct CircuitBreaker {
    /// Current state
    state: CircuitBreakerState,
    /// Failure count
    failure_count: u32,
    /// Failure threshold
    failure_threshold: u32,
    /// Success count
    success_count: u32,
    /// Success threshold for half-open state
    success_threshold: u32,
    /// Last failure time
    last_failure_time_ms: u64,
    /// Reset timeout
    reset_timeout_ms: u64,
}

/// Circuit Breaker States
#[derive(Debug, Clone, PartialEq)]
pub enum CircuitBreakerState {
    /// Normal operation
    Closed,
    /// Failing, rejecting requests
    Open,
    /// Testing if recovered
    HalfOpen,
}

impl CircuitBreaker {
    /// Create new circuit breaker
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

    /// Execute function with circuit breaker protection
    pub fn call<F, T>(&mut self, f: F) -> Result<T, ConnectorError>
    where
        F: FnOnce() -> Result<T, ConnectorError>,
    {
        match self.state {
            CircuitBreakerState::Open => {
                // Check if reset timeout has passed
                let current_time_ms = current_timestamp();
                if current_time_ms - self.last_failure_time_ms >= self.reset_timeout_ms {
                    self.state = CircuitBreakerState::HalfOpen;
                    self.success_count = 0;
                } else {
                    return Err(ConnectorError::CircuitBreakerOpen);
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
                self.last_failure_time_ms = current_timestamp();

                if self.failure_count >= self.failure_threshold {
                    self.state = CircuitBreakerState::Open;
                }

                Err(e)
            }
        }
    }

    /// Get current state
    pub fn state(&self) -> &CircuitBreakerState {
        &self.state
    }
}
```

### Integration with Five-Stage Pipeline

The `knhk-connectors` crate integrates with the existing five-stage transformation pipeline as follows:

#### μ₁ (Normalize) Stage Integration
- Connector specifications validated against RDF schema
- Mapping configurations normalized to standard format
- Authentication credentials validated and stored securely

#### μ₂ (Extract) Stage Integration
- SPARQL queries generated for specific data extraction
- OWL inference applied to extracted data
- Rule execution for data transformation

#### μ₃ (Emit) Stage Integration
- Tera templates generate connector configurations
- Code generation for connector initialization
- Event handlers for data processing workflows

#### μ₄ (Canonicalize) Stage Integration
- Connector state canonicalized to standard format
- Data batches standardized and optimized for storage
- Metrics and logs normalized for analysis

#### μ₅ (Receipt) Stage Integration
- Cryptographic receipts generated for data provenance
- Audit trail created for all connector operations
- Provenance information stored for lineage tracking

### Performance Optimizations

```rust
/// Hot-Path Buffer for Fast Processing
#[repr(align(64))]
pub struct HotPathBuffer {
    /// Cache for frequently accessed data
    connector_cache: LruCache<ConnectorId, Connector>,
    /// SPARQL query cache
    query_cache: LruCache<SparqlQuery, QueryResults>,
    /// Data format parsers
    parsers: DataFormatParsers,
    /// Hashing utilities
    hasher: HashUtilities,
}

/// Zero-Cost Data Processing
pub struct DataProcessor {
    /// Pre-allocated buffers
    buffers: FixedBufferPool,
    /// SIMD-optimized processing
    simd_processor: SIMDProcessor,
    /// Parallel processing engine
    parallel_engine: ParallelEngine,
}

impl DataProcessor {
    /// Process data with zero-cost abstractions
    pub fn process_delta(&mut self, delta: &Delta) -> ProcessedDelta {
        // Use SIMD-optimized processing
        let simd_result = self.simd_processor.process(delta);

        // Apply parallel processing if needed
        if delta.additions.len() > 1000 {
            self.parallel_engine.process(&simd_result)
        } else {
            simd_result
        }
    }
}

/// Batch Processing for High Throughput
pub struct BatchProcessor {
    /// Batch accumulator
    accumulator: Vec<Delta>,
    /// Batch size limit
    max_batch_size: usize,
    /// Batch timeout
    batch_timeout: Duration,
    /// Batch completion callback
    callback: Box<dyn BatchCallback>,
}

impl BatchProcessor {
    /// Add delta to batch
    pub fn add_delta(&mut self, delta: Delta) -> Option<Vec<Delta>> {
        self.accumulator.push(delta);

        if self.accumulator.len() >= self.max_batch_size {
            let batch = std::mem::take(&mut self.accumulator);
            Some(batch)
        } else {
            None
        }
    }

    /// Process pending batch
    pub fn process_pending(&mut self) -> Result<(), ConnectorError> {
        if !self.accumulator.is_empty() {
            let batch = std::mem::take(&mut self.accumulator);
            self.callback.process_batch(batch)?;
        }
        Ok(())
    }
}
```

### Error Handling and Recovery

```rust
/// Connector Error Types
#[derive(Debug)]
pub enum ConnectorError {
    /// Validation failed
    ValidationFailed(String),
    /// Schema mismatch
    SchemaMismatch(String),
    /// Guard violation
    GuardViolation(String),
    /// Parse error
    ParseError(String),
    /// I/O error
    IoError(String),
    /// Network error
    NetworkError(String),
    /// Initialization failed
    InitializationFailed(String),
    /// Batch too large
    BatchTooLarge(usize),
    /// Field not found
    FieldNotFound(String),
    /// Query failed
    QueryFailed(String),
    /// Circuit breaker open
    CircuitBreakerOpen,
    /// Not initialized
    NotInitialized,
    /// Invalid source type
    InvalidSourceType(String),
    /// Rate limit exceeded
    RateLimitExceeded,
}

/// Recovery Strategy
pub enum RecoveryStrategy {
    /// Retry with exponential backoff
    Retry {
        max_attempts: usize,
        base_delay_ms: u64,
        max_delay_ms: u64,
    },
    /// Fallback to alternative source
    Fallback {
        source_type: SourceType,
        mapping: Mapping,
    },
    /// Dead letter queue
    DeadLetterQueue {
        queue_path: String,
        retry_later: bool,
    },
    /// Manual intervention required
    ManualIntervention {
        error_message: String,
    },
}

/// Retry Manager
pub struct RetryManager {
    /// Configuration for retry strategy
    config: RetryConfig,
    /// Current retry count
    retry_count: usize,
    /// Last error
    last_error: Option<ConnectorError>,
}

impl RetryManager {
    /// Attempt recovery
    pub fn attempt_recovery(&mut self, error: &ConnectorError) -> Option<RecoveryStrategy> {
        if self.retry_count >= self.config.max_attempts {
            return Some(RecoveryStrategy::ManualIntervention {
                error_message: format!("Max retries exceeded: {}", error),
            });
        }

        self.retry_count += 1;
        self.last_error = Some(error.clone());

        // Exponential backoff
        let delay = std::cmp::min(
            self.config.base_delay_ms * (2u64.pow(self.retry_count as u32 - 1)),
            self.config.max_delay_ms,
        );

        // Schedule retry
        Some(RecoveryStrategy::Retry {
            max_attempts: self.config.max_attempts - self.retry_count,
            base_delay_ms: delay,
            max_delay_ms: self.config.max_delay_ms,
        })
    }
}
```

### Monitoring and Observability

```rust
/// Metrics Collection
#[derive(Debug, Clone, Default)]
pub struct ConnectorMetrics {
    /// Total requests
    pub total_requests: u64,
    /// Successful requests
    pub successful_requests: u64,
    /// Failed requests
    pub failed_requests: u64,
    /// Total deltas fetched
    pub total_deltas_fetched: u64,
    /// Total triples processed
    pub total_triples_processed: u64,
    /// Last request time
    pub last_request_time_ms: u64,
    /// Last error
    pub last_error: Option<String>,
    /// Performance metrics
    pub performance: PerformanceMetrics,
    /// Resource usage
    pub resource_usage: ResourceUsage,
}

/// Performance Metrics
#[derive(Debug, Clone, Default)]
pub struct PerformanceMetrics {
    /// Average response time
    pub avg_response_time_ms: f64,
    /// Requests per second
    pub requests_per_second: f64,
    /// Error rate
    pub error_rate: f64,
    /// Throughput (records per second)
    pub throughput: f64,
    /// Percentile metrics
    pub percentiles: PercentileMetrics,
}

/// Health Monitor
pub struct HealthMonitor {
    /// Health check configuration
    config: HealthCheckConfig,
    /// Current health status
    health_status: BTreeMap<ConnectorId, ConnectorHealth>,
    /// Alert manager
    alert_manager: AlertManager,
}

impl HealthMonitor {
    /// Check connector health
    pub fn check_health(&mut self, connector_id: &ConnectorId, connector: &dyn Connector) {
        let health = connector.health();
        self.health_status.insert(connector_id.clone(), health.clone());

        // Check for alerts
        if health == ConnectorHealth::Unhealthy("".to_string()) {
            self.alert_manager.trigger_alert(connector_id, "Connector unhealthy");
        }
    }
}

/// Alert Manager
pub struct AlertManager {
    /// Alert rules
    rules: Vec<AlertRule>,
    /// Active alerts
    active_alerts: BTreeMap<AlertId, Alert>,
    /// Notification channels
    channels: Vec<NotificationChannel>,
}

impl AlertManager {
    /// Trigger alert
    pub fn trigger_alert(&mut self, connector_id: &ConnectorId, message: &str) {
        let alert = Alert {
            id: AlertId::new(),
            connector_id: connector_id.clone(),
            message: message.to_string(),
            severity: AlertSeverity::High,
            created_at: current_timestamp(),
        };

        self.active_alerts.insert(alert.id, alert.clone());

        // Notify channels
        for channel in &self.channels {
            channel.notify(&alert);
        }
    }
}
```

## Usage Examples

```rust
/// Example: Creating and using a Kafka connector
#[tokio::main]
async fn example() -> Result<(), ConnectorError> {
    // Create connector specification
    let spec = ConnectorSpec {
        name: "kafka-orders".to_string(),
        schema: "http://example.org/schema/kafka".to_string(),
        source: SourceType::Kafka {
            topic: "orders".to_string(),
            format: DataFormat::Json,
            bootstrap_servers: vec!["kafka:9092".to_string()],
            consumer_config: KafkaConsumerConfig::default(),
        },
        mapping: Mapping {
            subject: "order_id".to_string(),
            predicate: "order_type".to_string(),
            object: "order_data".to_string(),
            graph: Some("orders".to_string()),
        },
        guards: Guards {
            max_batch_size: 1000,
            max_lag_ms: 5000,
            max_run_len: 8,
            schema_validation: true,
        },
    };

    // Create connector
    let mut kafka_connector = KafkaConnector::new();
    kafka_connector.initialize(spec)?;

    // Fetch data
    let delta = kafka_connector.fetch_delta()?;
    println!("Fetched {} records", delta.additions.len());

    // Transform to SoA for hot path processing
    let soa = kafka_connector.transform_to_soa(&delta)?;
    println!("Transformed to SoA: {} records", soa.len);

    Ok(())
}

/// Example: Creating and using a Salesforce connector
async fn salesforce_example() -> Result<(), ConnectorError> {
    // Create connector specification
    let spec = ConnectorSpec {
        name: "salesforce-accounts".to_string(),
        schema: "http://example.org/schema/salesforce".to_string(),
        source: SourceType::Salesforce {
            instance_url: "https://test.salesforce.com".to_string(),
            api_version: "56.0".to_string(),
            object_type: "Account".to_string(),
            soql_query: None,
            auth: SalesforceAuth::OAuth2 {
                access_token: "access-token".to_string(),
                instance_url: "https://test.salesforce.com".to_string(),
            },
        },
        mapping: Mapping {
            subject: "Id".to_string(),
            predicate: "Name".to_string(),
            object: "Type".to_string(),
            graph: Some("accounts".to_string()),
        },
        guards: Guards {
            max_batch_size: 2000,
            max_lag_ms: 10000,
            max_run_len: 8,
            schema_validation: true,
        },
    };

    // Create connector
    let mut salesforce_connector = SalesforceConnector::new();
    salesforce_connector.initialize(spec)?;

    // Fetch data with pagination
    let delta = salesforce_connector.fetch_delta()?;
    println!("Fetched {} accounts", delta.additions.len());

    Ok(())
}

/// Example: Using circuit breaker for fault tolerance
async fn circuit_breaker_example() -> Result<(), ConnectorError> {
    let mut connector = KafkaConnector::new();
    // ... initialize connector ...

    let mut circuit_breaker = CircuitBreaker::new(3, 60000); // 3 failures, 60s reset

    let result = circuit_breaker.call(|| {
        connector.fetch_delta()
    });

    match result {
        Ok(delta) => println!("Success: {} records", delta.additions.len()),
        Err(e) => println!("Failed with circuit breaker protection: {}", e),
    }

    Ok(())
}
```

## Conclusion

The `knhk-connectors` architecture provides a robust, type-safe, and performant framework for enterprise data integration. It supports Kafka and Salesforce connectors with advanced features like circuit breakers, rate limiting, and comprehensive monitoring. The architecture ensures data quality through validation and provides zero-cost abstractions for high-performance processing. The integration with the existing ggen five-stage pipeline ensures seamless operation within the broader system while maintaining enterprise-grade reliability and scalability.