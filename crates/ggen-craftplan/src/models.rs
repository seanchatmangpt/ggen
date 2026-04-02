//! Domain models for Elixir code generation
//!
//! This module defines the data structures that represent extracted RDF entities
//! and the Elixir code artifacts to be generated.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Represents an Elixir module to be generated
///
/// Each module corresponds to a domain entity in the Craftplan ERP system
/// (e.g., Product, Order, Batch, Material, Customer).
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ElixirModule {
    /// Module name (e.g., "Craftplan.Catalog.Product")
    pub name: String,

    /// Module type determines the template to use
    pub module_type: ModuleType,

    /// Entity metadata from RDF
    pub entity: EntityMetadata,

    /// Resource fields (for Ash resources)
    pub fields: Vec<Field>,

    /// Relationships to other entities
    pub relationships: Vec<Relationship>,

    /// Actions defined on this resource
    pub actions: Vec<Action>,

    /// Validations to apply
    pub validations: Vec<Validation>,
}

/// Type of Elixir module to generate
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ModuleType {
    /// Ash Resource with PostgreSQL data layer
    AshResource,

    /// Ash Domain (groups related resources)
    AshDomain,

    /// Phoenix LiveView for UI
    LiveView,

    /// Phoenix LiveComponent
    LiveComponent,

    /// Context module (business logic)
    Context,

    /// Helper/Utility module
    Helper,
}

/// Metadata extracted from RDF entity definitions
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct EntityMetadata {
    /// IRI of the entity in the ontology
    pub iri: String,

    /// Local name (e.g., "Product")
    pub local_name: String,

    /// Human-readable description
    pub description: Option<String>,

    /// RDF class type
    pub class_type: String,

    /// Entity namespace (e.g., "Catalog", "Orders")
    pub namespace: String,
}

/// Represents a field on an Ash resource
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Field {
    /// Field name in snake_case (Elixir convention)
    pub name: String,

    /// Elixir type for the field
    pub field_type: ElixirType,

    /// Whether the field is required
    pub required: bool,

    /// Default value (if any)
    pub default: Option<String>,

    /// Human-readable label
    pub label: Option<String>,

    /// Help text for documentation
    pub help_text: Option<String>,

    /// Whether this field is primary key
    pub primary_key: bool,

    /// Whether this field is unique
    pub unique: bool,
}

/// Elixir type mapping
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ElixirType {
    /// String type
    String,

    /// Integer type
    Integer,

    /// Float/decimal type
    Float,

    /// Boolean type
    Boolean,

    /// UUID type
    Uuid,

    /// DateTime (with timezone)
    DateTime,

    /// Date (without time)
    Date,

    /// Time (without date)
    Time,

    /// Custom Ash type
    Custom(String),

    /// Relationship to another entity
    Relationship {
        /// Target module name
        target_module: String,
        /// Relationship cardinality
        cardinality: Cardinality,
    },
}

/// Relationship cardinality for Ash resources
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum Cardinality {
    /// One-to-one relationship
    OneToOne,

    /// One-to-many relationship
    OneToMany,

    /// Many-to-many relationship
    ManyToMany,

    /// Belongs to (inverse of one-to-many)
    BelongsTo,
}

/// Represents a relationship between entities
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Relationship {
    /// Relationship name
    pub name: String,

    /// Target entity IRI
    pub target_iri: String,

    /// Relationship cardinality
    pub cardinality: Cardinality,

    /// Foreign key field (if any)
    pub foreign_key: Option<String>,

    /// Join relationship for many-to-many
    pub join_relationship: Option<String>,

    /// Whether this relationship is required
    pub required: bool,
}

/// Represents an Ash action (create, read, update, destroy)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Action {
    /// Action name
    pub name: String,

    /// Action type
    pub action_type: ActionType,

    /// Arguments accepted by this action
    pub arguments: Vec<ActionArgument>,

    /// Whether this action is the primary action
    pub primary: bool,

    /// Authorization policies
    pub policies: Vec<String>,
}

/// Type of Ash action
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ActionType {
    /// Create action (insert new records)
    Create,

    /// Read action (query records)
    Read,

    /// Update action (modify existing records)
    Update,

    /// Destroy action (delete records)
    Destroy,

    /// Custom action
    Custom(String),
}

/// Argument for an Ash action
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ActionArgument {
    /// Argument name
    pub name: String,

    /// Argument type
    pub argument_type: ElixirType,

    /// Whether this argument is required
    pub required: bool,

    /// Default value
    pub default: Option<String>,
}

/// Validation rule for a field or resource
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Validation {
    /// Validation type
    pub validation_type: ValidationType,

    /// Field to validate (empty for resource-level)
    pub field: Option<String>,

    /// Validation parameters
    pub parameters: BTreeMap<String, String>,

    /// Error message on validation failure
    pub error_message: Option<String>,
}

/// Type of validation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ValidationType {
    /// Presence validation (field must be present)
    Present,

    /// Absence validation (field must not be present)
    Absent,

    /// String length validation
    StringLength {
        min: Option<usize>,
        max: Option<usize>,
    },

    /// Numeric comparison
    NumericComparison {
        operator: ComparisonOperator,
        value: String,
    },

    /// Inclusion in set
    Inclusion { list: Vec<String> },

    /// Exclusion from set
    Exclusion { list: Vec<String> },

    /// Format validation (regex)
    Format { pattern: String },

    /// Custom validation function
    Custom(String),
}

/// Comparison operator for numeric validations
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ComparisonOperator {
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Equal,
    NotEqual,
}

/// Extracted data from RDF ontology
///
/// This structure contains all entity data extracted from the RDF graph
/// via SPARQL queries during the μ₂ (Extract) stage.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExtractedData {
    /// All entities extracted from RDF
    pub entities: Vec<ElixirModule>,

    /// SPARQL query results for reference
    pub query_results: BTreeMap<String, Vec<serde_json::Value>>,

    /// Prefix mappings used in extraction
    pub prefixes: BTreeMap<String, String>,
}

impl ExtractedData {
    /// Create a new empty extraction result
    pub fn new() -> Self {
        Self {
            entities: Vec::new(),
            query_results: BTreeMap::new(),
            prefixes: BTreeMap::new(),
        }
    }

    /// Add an entity to the extraction
    pub fn add_entity(&mut self, entity: ElixirModule) {
        self.entities.push(entity);
    }

    /// Get entities by namespace
    pub fn entities_by_namespace(&self, namespace: &str) -> Vec<&ElixirModule> {
        self.entities
            .iter()
            .filter(|e| e.entity.namespace == namespace)
            .collect()
    }

    /// Check if extraction is empty
    pub fn is_empty(&self) -> bool {
        self.entities.is_empty()
    }

    /// Get count of extracted entities
    pub fn len(&self) -> usize {
        self.entities.len()
    }
}

impl Default for ExtractedData {
    fn default() -> Self {
        Self::new()
    }
}

/// Receipt generated by the μ₅ stage
///
/// Contains cryptographic proofs and audit trail for reproducibility verification.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationReceipt {
    /// Input RDF file hash
    pub input_hash: String,

    /// Hashes of all generated files
    pub output_hashes: BTreeMap<String, String>,

    /// Pipeline execution metadata
    pub metadata: ReceiptMetadata,

    /// SHA-256 hash of the entire receipt
    pub receipt_hash: String,
}

/// Metadata about the generation run
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReceiptMetadata {
    /// Timestamp of generation (ISO 8601)
    pub timestamp: String,

    /// ggen-craftplan version
    pub generator_version: String,

    /// Number of entities processed
    pub entity_count: usize,

    /// Number of files generated
    pub file_count: usize,

    /// Pipeline execution time in milliseconds
    pub duration_ms: u64,

    /// List of executed pipeline stages
    pub stages: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extracted_data_empty() {
        let data = ExtractedData::new();
        assert!(data.is_empty());
        assert_eq!(data.len(), 0);
    }

    #[test]
    fn test_extracted_data_add_entity() {
        let mut data = ExtractedData::new();
        let entity = ElixirModule {
            name: "Craftplan.Catalog.Product".to_string(),
            module_type: ModuleType::AshResource,
            entity: EntityMetadata {
                iri: "http://craftplan.org/Product".to_string(),
                local_name: "Product".to_string(),
                description: Some("A product".to_string()),
                class_type: "owl:Class".to_string(),
                namespace: "Catalog".to_string(),
            },
            fields: vec![],
            relationships: vec![],
            actions: vec![],
            validations: vec![],
        };

        data.add_entity(entity);
        assert_eq!(data.len(), 1);
        assert!(!data.is_empty());
    }

    #[test]
    fn test_entities_by_namespace() {
        let mut data = ExtractedData::new();

        let product = ElixirModule {
            name: "Craftplan.Catalog.Product".to_string(),
            module_type: ModuleType::AshResource,
            entity: EntityMetadata {
                iri: "http://craftplan.org/Product".to_string(),
                local_name: "Product".to_string(),
                description: None,
                class_type: "owl:Class".to_string(),
                namespace: "Catalog".to_string(),
            },
            fields: vec![],
            relationships: vec![],
            actions: vec![],
            validations: vec![],
        };

        let order = ElixirModule {
            name: "Craftplan.Orders.Order".to_string(),
            module_type: ModuleType::AshResource,
            entity: EntityMetadata {
                iri: "http://craftplan.org/Order".to_string(),
                local_name: "Order".to_string(),
                description: None,
                class_type: "owl:Class".to_string(),
                namespace: "Orders".to_string(),
            },
            fields: vec![],
            relationships: vec![],
            actions: vec![],
            validations: vec![],
        };

        data.add_entity(product);
        data.add_entity(order);

        let catalog_entities = data.entities_by_namespace("Catalog");
        assert_eq!(catalog_entities.len(), 1);
        assert_eq!(catalog_entities[0].entity.local_name, "Product");

        let orders_entities = data.entities_by_namespace("Orders");
        assert_eq!(orders_entities.len(), 1);
        assert_eq!(orders_entities[0].entity.local_name, "Order");
    }

    #[test]
    fn test_field_serialization() {
        let field = Field {
            name: "sku".to_string(),
            field_type: ElixirType::String,
            required: true,
            default: None,
            label: Some("SKU".to_string()),
            help_text: Some("Stock keeping unit".to_string()),
            primary_key: false,
            unique: true,
        };

        let json = serde_json::to_string(&field).unwrap();
        assert!(json.contains("sku"));
        assert!(json.contains("primary_key"));
    }

    #[test]
    fn test_relationship_cardinality() {
        let one_to_many = Relationship {
            name: "order_items".to_string(),
            target_iri: "http://craftplan.org/OrderItem".to_string(),
            cardinality: Cardinality::OneToMany,
            foreign_key: Some("order_id".to_string()),
            join_relationship: None,
            required: false,
        };

        assert_eq!(one_to_many.cardinality, Cardinality::OneToMany);
        assert!(one_to_many.foreign_key.is_some());
    }
}
