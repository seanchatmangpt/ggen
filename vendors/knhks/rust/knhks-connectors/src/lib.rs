// rust/knhks-connectors/src/lib.rs
// Dark Matter 80/20 Connector Framework
// Provides typed, validated connectors for enterprise data sources

#![no_std]
extern crate alloc;

use alloc::string::String;
use alloc::vec::Vec;
use alloc::collections::BTreeMap;

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

/// Connector registry
pub struct ConnectorRegistry {
    connectors: BTreeMap<ConnectorId, Box<dyn Connector>>,
}

impl ConnectorRegistry {
    pub fn new() -> Self {
        Self {
            connectors: BTreeMap::new(),
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
        self.connectors.insert(id, connector);
        Ok(())
    }

    /// Get connector by ID
    pub fn get(&self, id: &ConnectorId) -> Option<&dyn Connector> {
        self.connectors.get(id).map(|c| c.as_ref())
    }

    /// List all connector IDs
    pub fn list(&self) -> Vec<ConnectorId> {
        self.connectors.keys().cloned().collect()
    }
}

pub mod kafka;
pub mod salesforce;
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
        // Placeholder test - will be expanded when concrete connectors are implemented
        let registry = ConnectorRegistry::new();
        assert_eq!(registry.list().len(), 0);
    }
}

