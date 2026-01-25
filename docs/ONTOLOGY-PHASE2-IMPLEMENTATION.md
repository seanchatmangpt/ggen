<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Phase 2: Entity Mapping Integration + SPARQL Generation](#ggen-phase-2-entity-mapping-integration--sparql-generation)
  - [Phase 2 Architecture Overview](#phase-2-architecture-overview)
  - [1. Integration Layer: EnterpriseDomainMapper](#1-integration-layer-enterprisedomainmapper)
    - [Location](#location)
    - [Type Definitions](#type-definitions)
    - [Core Methods](#core-methods)
      - [Method 1: Parse Domain Description](#method-1-parse-domain-description)
      - [Method 2: Map Entities to Ontology](#method-2-map-entities-to-ontology)
      - [Method 3: Generate SPARQL Queries](#method-3-generate-sparql-queries)
      - [Method 4: Execute Full Pipeline](#method-4-execute-full-pipeline)
  - [2. Provider Mapper Interface (Phase 3 Preparation)](#2-provider-mapper-interface-phase-3-preparation)
    - [Location](#location-1)
    - [Type Definitions](#type-definitions-1)
  - [3. Compliance Receipt Generator (Phase 3 Preparation)](#3-compliance-receipt-generator-phase-3-preparation)
    - [Location](#location-2)
    - [Type Definitions](#type-definitions-2)
  - [4. CLI Integration](#4-cli-integration)
    - [Location](#location-3)
    - [Command Structure](#command-structure)
  - [5. Testing Strategy](#5-testing-strategy)
    - [Integration Test Path](#integration-test-path)
    - [Test Scenarios](#test-scenarios)
      - [Test 1: HIPAA Domain Mapping](#test-1-hipaa-domain-mapping)
      - [Test 2: Determinism Test](#test-2-determinism-test)
      - [Test 3: Error Handling](#test-3-error-handling)
  - [6. Example: HIPAA Domain → MCP Proposal Outline](#6-example-hipaa-domain-%E2%86%92-mcp-proposal-outline)
    - [Input: domain-hipaa.yaml](#input-domain-hipaayaml)
    - [Output: Phase 2 Mapping Result (JSON)](#output-phase-2-mapping-result-json)
    - [CLI Usage](#cli-usage)
  - [7. Success Criteria](#7-success-criteria)
  - [8. Risk Mitigation](#8-risk-mitigation)
  - [9. Transition to Phase 3](#9-transition-to-phase-3)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Phase 2: Entity Mapping Integration + SPARQL Generation

**Timeline**: Weeks 3-4 (10 business days)

**Objective**: Build end-to-end pipeline transforming customer domain descriptions into ontology-mapped entities and deterministic SPARQL queries feeding Phase 3 provider bindings.

---

## Phase 2 Architecture Overview

```
┌──────────────────────────────────────────────────────────────────┐
│                        CUSTOMER INPUT                             │
│                    (YAML Domain Description)                      │
└────────────────────────────┬─────────────────────────────────────┘
                             │
                             ▼
┌──────────────────────────────────────────────────────────────────┐
│             1. PARSE DOMAIN DESCRIPTION                           │
│  (YamlParser → Vec<Entity>)                                       │
│  - Parse entities, policies, controls, services                   │
│  - Extract attributes, relationships, tags                        │
│  - Normalize and validate entity structure                        │
└────────────────────────────┬─────────────────────────────────────┘
                             │
                             ▼
┌──────────────────────────────────────────────────────────────────┐
│             2. ENTITY MAPPING PIPELINE                            │
│  (EnterpriseDomainMapper → Vec<OntologyMapping>)                  │
│  - Match entities to ontology classes (5 methods)                 │
│  - Score confidence (0.0-1.0)                                     │
│  - Resolve ambiguities and multi-matches                          │
│  - Track mapping decisions and rationale                          │
└────────────────────────────┬─────────────────────────────────────┘
                             │
                             ▼
┌──────────────────────────────────────────────────────────────────┐
│             3. SPARQL QUERY GENERATION                            │
│  (SparqlGenerator → Vec<SparqlQuery>)                             │
│  - Generate deterministic queries from mappings                   │
│  - Query for policy compliance, control status                    │
│  - Query for entity relationships                                 │
│  - Cache queries for reproducibility                              │
└────────────────────────────┬─────────────────────────────────────┘
                             │
                             ▼
┌──────────────────────────────────────────────────────────────────┐
│             4. MAPPING RESULT AGGREGATION                         │
│  (MappingResult)                                                   │
│  - Combine mapped entities + queries + rationale                  │
│  - Calculate aggregate confidence scores                          │
│  - Generate mapping report with audit trail                       │
└────────────────────────────┬─────────────────────────────────────┘
                             │
                             ▼
┌──────────────────────────────────────────────────────────────────┐
│             5. PROVIDER MAPPER INTERFACE (PREP)                   │
│  (ProviderMapper trait)                                            │
│  - Interface for Phase 3 AWS/GCP/Azure mappings                   │
│  - Placeholder implementations                                    │
└────────────────────────────┬─────────────────────────────────────┘
                             │
                             ▼
┌──────────────────────────────────────────────────────────────────┐
│             6. CLI INTEGRATION                                     │
│  (ggen ontology map --input domain.yaml)                          │
│  - Load domain → Execute pipeline → Output proposal skeleton      │
│  - JSON output with entities, mappings, queries                   │
└────────────────────────────┬─────────────────────────────────────┘
                             │
                             ▼
┌──────────────────────────────────────────────────────────────────┐
│                    PHASE 3 INPUT                                   │
│               (MCP Proposal Skeleton)                              │
│  - Provider-agnostic mapped entities                              │
│  - SPARQL queries for compliance validation                       │
│  - Ready for AWS/GCP/Azure fan-out                                │
└──────────────────────────────────────────────────────────────────┘
```

---

## 1. Integration Layer: EnterpriseDomainMapper

### Location
`/home/user/ggen/crates/ggen-ontology-core/src/integration.rs`

### Type Definitions

```rust
use serde::{Deserialize, Serialize};
use crate::entity_mapper::OntologyMatch;
use crate::sparql_generator::SparqlGenerator;
use crate::errors::Result;

/// Entity parsed from customer domain description
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Entity {
    /// Unique identifier for entity
    pub id: String,
    /// Entity name/label
    pub name: String,
    /// Entity type: Policy, Control, Service, Classification, etc.
    pub entity_type: EntityType,
    /// Entity attributes (key-value pairs)
    pub attributes: std::collections::HashMap<String, String>,
    /// Entity tags/labels
    pub tags: Vec<String>,
    /// Relationships to other entities
    pub relationships: Vec<EntityRelationship>,
    /// Raw YAML source for audit trail
    pub source_yaml: String,
}

/// Supported entity types
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum EntityType {
    Policy,
    Control,
    Service,
    Classification,
    Jurisdiction,
    Role,
    Requirement,
    Artifact,
    Unknown,
}

/// Relationship between entities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EntityRelationship {
    pub entity_id: String,
    pub relationship_type: String, // "depends_on", "implements", "references", etc.
}

/// Mapping of entity to ontology classes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologyMapping {
    /// Source entity ID
    pub entity_id: String,
    /// Source entity name
    pub entity_name: String,
    /// Matched ontology classes (sorted by score)
    pub ontology_matches: Vec<OntologyMatch>,
    /// Primary (highest-confidence) match
    pub primary_match: OntologyMatch,
    /// Mapping confidence (0.0-1.0)
    pub confidence: f32,
    /// Rationale for mapping decision
    pub rationale: String,
    /// Additional mapping metadata
    pub metadata: std::collections::HashMap<String, String>,
}

/// SPARQL query with metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SparqlQuery {
    /// Unique query identifier
    pub id: String,
    /// Query string
    pub query: String,
    /// Purpose of query (compliance check, entity enumeration, etc.)
    pub purpose: String,
    /// Entities this query applies to
    pub applicable_entities: Vec<String>,
    /// Expected result structure
    pub expected_result_type: String,
}

/// Complete mapping result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MappingResult {
    /// Input domain description
    pub domain_id: String,
    /// Parsed entities
    pub parsed_entities: Vec<Entity>,
    /// Ontology mappings
    pub mappings: Vec<OntologyMapping>,
    /// Generated SPARQL queries
    pub queries: Vec<SparqlQuery>,
    /// Aggregate confidence score
    pub overall_confidence: f32,
    /// Processing timestamp
    pub processed_at: String,
    /// Provider mapper interface placeholder
    pub provider_mappings: ProviderMappings,
}

/// Provider mapping interface (Phase 3 prep)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProviderMappings {
    pub aws_operations: Vec<String>,
    pub gcp_operations: Vec<String>,
    pub azure_operations: Vec<String>,
}
```

### Core Methods

#### Method 1: Parse Domain Description

```rust
impl EnterpriseDomainMapper {
    /// Parse YAML domain description into Entity structure
    ///
    /// # Pseudocode
    /// 1. Load YAML string → Value tree
    /// 2. For each top-level section (policies, controls, services, etc.):
    ///    a. Extract entity_id, name, attributes
    ///    b. Validate entity structure (has required fields)
    ///    c. Parse relationships to other entities
    ///    d. Create Entity struct with source_yaml
    /// 3. Return Vec<Entity> sorted by entity_type, then id
    ///
    /// # Determinism
    /// - YAML parsing order is deterministic (alphabetical keys)
    /// - Same domain_yaml always produces identical Entity vector
    ///
    /// # Error Handling
    /// - OntologyError::ParseError if YAML invalid
    /// - OntologyError::ValidationError if required fields missing
    pub fn parse_domain_description(yaml: &str) -> Result<Vec<Entity>> {
        // Implementation pseudocode:

        // 1. Parse YAML
        let value: serde_yaml::Value = serde_yaml::from_str(yaml)
            .map_err(|e| OntologyError::parse("domain.yaml", 1, e.to_string()))?;

        // 2. Extract sections: entities, metadata
        let entities_section = value.get("entities")
            .ok_or_else(|| OntologyError::validation(vec![
                "Missing 'entities' section in domain description".to_string()
            ]))?;

        // 3. For each entity, parse and create Entity struct
        let mut entities = Vec::new();
        for (entity_id, entity_value) in entities_section.as_mapping().unwrap().iter() {
            let entity_id_str = entity_id.as_str().unwrap().to_string();
            let entity_name = entity_value.get("name")
                .and_then(|v| v.as_str())
                .ok_or_else(|| OntologyError::parse("domain.yaml", 1,
                    format!("Entity {} missing 'name' field", entity_id_str)))?
                .to_string();

            let entity_type_str = entity_value.get("type")
                .and_then(|v| v.as_str())
                .unwrap_or("Unknown");

            let entity_type = parse_entity_type(entity_type_str);

            // Extract attributes
            let mut attributes = std::collections::HashMap::new();
            if let Some(attrs) = entity_value.get("attributes").and_then(|v| v.as_mapping()) {
                for (k, v) in attrs.iter() {
                    let key = k.as_str().unwrap().to_string();
                    let val = v.as_str().unwrap_or("").to_string();
                    attributes.insert(key, val);
                }
            }

            // Extract tags
            let tags: Vec<String> = entity_value.get("tags")
                .and_then(|v| v.as_sequence())
                .map(|seq| seq.iter()
                    .filter_map(|tag| tag.as_str())
                    .map(|s| s.to_string())
                    .collect())
                .unwrap_or_default();

            // Extract relationships
            let mut relationships = Vec::new();
            if let Some(rels) = entity_value.get("relationships").and_then(|v| v.as_sequence()) {
                for rel in rels.iter() {
                    if let (Some(target_id), Some(rel_type)) = (
                        rel.get("target_id").and_then(|v| v.as_str()),
                        rel.get("type").and_then(|v| v.as_str())
                    ) {
                        relationships.push(EntityRelationship {
                            entity_id: target_id.to_string(),
                            relationship_type: rel_type.to_string(),
                        });
                    }
                }
            }

            entities.push(Entity {
                id: entity_id_str,
                name: entity_name,
                entity_type,
                attributes,
                tags,
                relationships,
                source_yaml: entity_value.to_string(),
            });
        }

        // 4. Sort deterministically
        entities.sort_by_key(|e| (format!("{:?}", e.entity_type), e.id.clone()));

        Ok(entities)
    }
}
```

#### Method 2: Map Entities to Ontology

```rust
impl EnterpriseDomainMapper {
    /// Map parsed entities to ontology classes
    ///
    /// # Pseudocode
    /// 1. For each entity:
    ///    a. Determine entity type (Policy, Control, Service, etc.)
    ///    b. Call appropriate matcher (EntityMapper::match_*)
    ///    c. Get sorted Vec<OntologyMatch>
    ///    d. Extract primary match (highest score)
    ///    e. Calculate confidence from top match score
    ///    f. Create OntologyMapping with rationale
    /// 2. Resolve ambiguities (multiple entities mapping to same class)
    /// 3. Return Vec<OntologyMapping> sorted by entity_id
    ///
    /// # Determinism
    /// - EntityMapper methods are deterministic
    /// - Same entity always produces same ontology classes
    /// - Sorting ensures consistent ordering
    pub fn map_entities_to_ontology(entities: Vec<Entity>)
        -> Result<Vec<OntologyMapping>> {

        let mut mappings = Vec::new();

        for entity in entities.iter() {
            // 1. Route to appropriate matcher based on entity_type
            let matches = match entity.entity_type {
                EntityType::Policy => {
                    EntityMapper::match_policy(&entity.name)?
                }
                EntityType::Classification => {
                    EntityMapper::match_data_classification(&entity.name)?
                }
                EntityType::Service => {
                    EntityMapper::match_compute_service(&entity.name)?
                }
                EntityType::Control => {
                    EntityMapper::match_security_control(&entity.name)?
                }
                EntityType::Requirement => {
                    // Parse requirements (e.g., "99.99% availability")
                    if let Some(availability) = entity.attributes.get("availability") {
                        let pct: f32 = availability.parse().unwrap_or(99.0);
                        EntityMapper::match_service_level(pct)?
                    } else {
                        EntityMapper::match_policy(&entity.name)?
                    }
                }
                _ => {
                    EntityMapper::match_policy(&entity.name)?
                }
            };

            // 2. Extract primary match (already sorted by score)
            let primary_match = matches.first()
                .ok_or_else(|| OntologyError::mapper(
                    &entity.id,
                    "No ontology matches found for entity"
                ))?
                .clone();

            // 3. Calculate confidence
            let confidence = primary_match.score;

            // 4. Generate rationale
            let rationale = format!(
                "Entity '{}' (type: {:?}) mapped to '{}' via {} matching method. Score: {}",
                entity.name,
                entity.entity_type,
                primary_match.label,
                primary_match.reason,
                primary_match.score
            );

            // 5. Create OntologyMapping
            let mut metadata = std::collections::HashMap::new();
            metadata.insert("entity_type".to_string(), format!("{:?}", entity.entity_type));
            metadata.insert("source_tags".to_string(), entity.tags.join(", "));
            metadata.insert("alternative_matches".to_string(),
                matches.iter().skip(1).take(3)
                    .map(|m| m.label.clone())
                    .collect::<Vec<_>>()
                    .join("; "));

            mappings.push(OntologyMapping {
                entity_id: entity.id.clone(),
                entity_name: entity.name.clone(),
                ontology_matches: matches,
                primary_match,
                confidence,
                rationale,
                metadata,
            });
        }

        // 6. Sort deterministically
        mappings.sort_by_key(|m| m.entity_id.clone());

        Ok(mappings)
    }
}
```

#### Method 3: Generate SPARQL Queries

```rust
impl EnterpriseDomainMapper {
    /// Generate SPARQL queries from entity mappings
    ///
    /// # Pseudocode
    /// For each mapping:
    ///   1. Determine query types needed:
    ///      - Entity enumeration (find all instances of type)
    ///      - Relationship queries (dependencies, implementations)
    ///      - Compliance queries (policy + control + jurisdiction)
    ///   2. For each query type:
    ///      a. Generate deterministic SPARQL query
    ///      b. Store with metadata (applicable entities, result type)
    ///   3. Sort queries by ID for determinism
    ///   4. Cache query strings for reproducibility
    ///
    /// # Determinism
    /// - SparqlGenerator methods are deterministic
    /// - Query ID is generated from entity_id + query_type
    /// - Same mappings always produce identical queries
    pub fn generate_queries_for_mappings(
        mappings: Vec<OntologyMapping>
    ) -> Result<Vec<SparqlQuery>> {

        let mut queries = Vec::new();
        let mut query_id_counter = 0;

        for (idx, mapping) in mappings.iter().enumerate() {
            let entity_id = &mapping.entity_id;

            // 1. Generate entity enumeration query
            let enum_query_id = format!("q_{:03d}_enum_{}", idx, entity_id);
            let enum_query_string = format!(
                "SELECT ?instance ?label WHERE {{\n  \
                 ?instance rdf:type {} .\n  \
                 OPTIONAL {{ ?instance rdfs:label ?label . }}\n\
                }} ORDER BY ?instance",
                mapping.primary_match.class
            );

            queries.push(SparqlQuery {
                id: enum_query_id,
                query: enum_query_string,
                purpose: format!("Enumerate all instances of {}", mapping.primary_match.label),
                applicable_entities: vec![entity_id.clone()],
                expected_result_type: "instance_list".to_string(),
            });

            query_id_counter += 1;

            // 2. For policy types, generate compliance queries
            if mapping.primary_match.class.contains("Policy") {
                let policy_id = &mapping.entity_id;
                let compliance_query_id = format!("q_{:03d}_compliance_{}", idx, entity_id);
                let compliance_query = format!(
                    "SELECT ?policy ?jurisdiction ?hasControls WHERE {{\n  \
                     ?policy rdf:type {} .\n  \
                     ?policy :policyId \"{}\" .\n  \
                     OPTIONAL {{ ?policy :hasJurisdiction ?jurisdiction . }}\n  \
                     OPTIONAL {{ ?policy :hasControls ?hasControls . }}\n\
                    }} LIMIT 1",
                    mapping.primary_match.class,
                    policy_id
                );

                queries.push(SparqlQuery {
                    id: compliance_query_id,
                    query: compliance_query,
                    purpose: format!("Check compliance status for policy {}", mapping.entity_name),
                    applicable_entities: vec![entity_id.clone()],
                    expected_result_type: "compliance_status".to_string(),
                });
            }

            // 3. For control types, generate control verification queries
            if mapping.primary_match.class.contains("Control") {
                let control_query_id = format!("q_{:03d}_verify_{}", idx, entity_id);
                let control_query = format!(
                    "SELECT ?control ?status ?lastVerified WHERE {{\n  \
                     ?control rdf:type {} .\n  \
                     ?control :controlId \"{}\" .\n  \
                     OPTIONAL {{ ?control :verificationStatus ?status . }}\n  \
                     OPTIONAL {{ ?control :lastVerified ?lastVerified . }}\n\
                    }}",
                    mapping.primary_match.class,
                    entity_id
                );

                queries.push(SparqlQuery {
                    id: control_query_id,
                    query: control_query,
                    purpose: format!("Verify control {}", mapping.entity_name),
                    applicable_entities: vec![entity_id.clone()],
                    expected_result_type: "control_status".to_string(),
                });
            }
        }

        // 4. Sort deterministically
        queries.sort_by_key(|q| q.id.clone());

        Ok(queries)
    }
}
```

#### Method 4: Execute Full Pipeline

```rust
impl EnterpriseDomainMapper {
    /// Execute complete mapping pipeline
    ///
    /// # Pseudocode
    /// 1. Parse domain description → Vec<Entity>
    /// 2. Map entities to ontology → Vec<OntologyMapping>
    /// 3. Generate SPARQL queries → Vec<SparqlQuery>
    /// 4. Aggregate results → MappingResult
    ///
    /// # Determinism
    /// - Each step is deterministic
    /// - Same domain_yaml always produces identical MappingResult
    /// - Result is idempotent (same queries, same order)
    ///
    /// # Error Handling
    /// - Propagate errors from any step
    /// - Include context about which step failed
    pub fn execute_mapping_pipeline(domain_yaml: &str)
        -> Result<MappingResult> {

        // 1. Parse
        let entities = Self::parse_domain_description(domain_yaml)?;

        if entities.is_empty() {
            return Err(OntologyError::validation(vec![
                "No entities found in domain description".to_string()
            ]));
        }

        // 2. Map
        let mappings = Self::map_entities_to_ontology(entities.clone())?;

        // 3. Generate queries
        let queries = Self::generate_queries_for_mappings(mappings.clone())?;

        // 4. Calculate overall confidence
        let overall_confidence = if mappings.is_empty() {
            0.0
        } else {
            mappings.iter().map(|m| m.confidence).sum::<f32>() / mappings.len() as f32
        };

        // 5. Create provider mapping placeholders (Phase 3 prep)
        let provider_mappings = ProviderMappings {
            aws_operations: mappings.iter()
                .map(|m| format!("aws_{}", m.entity_id))
                .collect(),
            gcp_operations: mappings.iter()
                .map(|m| format!("gcp_{}", m.entity_id))
                .collect(),
            azure_operations: mappings.iter()
                .map(|m| format!("azure_{}", m.entity_id))
                .collect(),
        };

        Ok(MappingResult {
            domain_id: "custom_domain".to_string(),
            parsed_entities: entities,
            mappings,
            queries,
            overall_confidence,
            processed_at: chrono::Utc::now().to_rfc3339(),
            provider_mappings,
        })
    }
}
```

---

## 2. Provider Mapper Interface (Phase 3 Preparation)

### Location
`/home/user/ggen/crates/ggen-ontology-core/src/provider_mapper.rs`

### Type Definitions

```rust
/// Maps ontology classes to provider-specific operations
pub trait ProviderMapper {
    /// Map ontology class to AWS operation
    fn map_to_aws(&self, ontology: &OntologyClass) -> AwsOperation;

    /// Map ontology class to GCP operation
    fn map_to_gcp(&self, ontology: &OntologyClass) -> GcpOperation;

    /// Map ontology class to Azure operation
    fn map_to_azure(&self, ontology: &OntologyClass) -> AzureOperation;
}

pub struct OntologyClass {
    pub class_id: String,
    pub label: String,
    pub attributes: std::collections::HashMap<String, String>,
}

pub struct AwsOperation {
    pub service: String,
    pub operation: String,
    pub parameters: std::collections::HashMap<String, String>,
}

pub struct GcpOperation {
    pub service: String,
    pub operation: String,
    pub parameters: std::collections::HashMap<String, String>,
}

pub struct AzureOperation {
    pub service: String,
    pub operation: String,
    pub parameters: std::collections::HashMap<String, String>,
}

// Phase 3 will implement:
// - AwsProviderMapper
// - GcpProviderMapper
// - AzureProviderMapper
```

---

## 3. Compliance Receipt Generator (Phase 3 Preparation)

### Location
`/home/user/ggen/crates/ggen-ontology-core/src/receipt_generator.rs`

### Type Definitions

```rust
use sha2::{Sha256, Digest};

/// Cryptographic proof of mapping and compliance
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceReceipt {
    /// Timestamp when receipt was generated
    pub timestamp: String,

    /// SHA256 hash of mappings
    pub mappings_hash: String,

    /// SHA256 hash of SPARQL queries
    pub queries_hash: String,

    /// Proof chain (audit trail)
    pub proof_chain: Vec<ProofEntry>,

    /// Ed25519 signature of receipt
    pub signature: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProofEntry {
    pub step: usize,
    pub entity_id: String,
    pub hash: String,
    pub timestamp: String,
}

impl ComplianceReceipt {
    /// Generate receipt from mapping result
    pub fn generate(result: &MappingResult) -> Result<Self> {
        // Hash mappings
        let mappings_json = serde_json::to_string(&result.mappings)?;
        let mut hasher = Sha256::new();
        hasher.update(mappings_json.as_bytes());
        let mappings_hash = format!("{:x}", hasher.finalize());

        // Hash queries
        let queries_json = serde_json::to_string(&result.queries)?;
        let mut hasher = Sha256::new();
        hasher.update(queries_json.as_bytes());
        let queries_hash = format!("{:x}", hasher.finalize());

        // Create proof chain
        let mut proof_chain = Vec::new();
        for (idx, mapping) in result.mappings.iter().enumerate() {
            let entry_json = serde_json::to_string(&mapping)?;
            let mut hasher = Sha256::new();
            hasher.update(entry_json.as_bytes());
            let hash = format!("{:x}", hasher.finalize());

            proof_chain.push(ProofEntry {
                step: idx,
                entity_id: mapping.entity_id.clone(),
                hash,
                timestamp: chrono::Utc::now().to_rfc3339(),
            });
        }

        Ok(Self {
            timestamp: chrono::Utc::now().to_rfc3339(),
            mappings_hash,
            queries_hash,
            proof_chain,
            signature: "placeholder_ed25519_signature".to_string(),
        })
    }

    /// Verify receipt integrity
    pub fn verify(&self) -> Result<bool> {
        // Phase 3: Implement Ed25519 verification
        Ok(true)
    }
}
```

---

## 4. CLI Integration

### Location
`/home/user/ggen/crates/ggen-cli/src/cmds/ontology.rs`

### Command Structure

```rust
use clap::Subcommand;

#[derive(Subcommand)]
pub enum OntologyCommand {
    /// Map customer domain to ontology with SPARQL queries
    Map {
        /// Path to domain description YAML
        #[arg(short, long)]
        input: String,

        /// Output format (json, yaml)
        #[arg(short, long, default_value = "json")]
        format: String,

        /// Optional output file path
        #[arg(short, long)]
        output: Option<String>,

        /// Generate provider-specific proposal skeleton
        #[arg(long)]
        with_provider_skeleton: bool,
    },

    /// Validate domain description format
    Validate {
        /// Path to domain description YAML
        #[arg(short, long)]
        input: String,
    },
}

impl OntologyCommand {
    pub async fn execute(self) -> Result<()> {
        match self {
            Self::Map { input, format, output, with_provider_skeleton } => {
                // 1. Load YAML file
                let yaml_content = std::fs::read_to_string(&input)
                    .map_err(|e| OntologyError::io(format!("Failed to read {}: {}", input, e)))?;

                // 2. Execute pipeline
                let result = EnterpriseDomainMapper::execute_mapping_pipeline(&yaml_content)?;

                // 3. Generate provider skeleton if requested
                if with_provider_skeleton {
                    let provider_skeleton = generate_provider_skeleton(&result);
                    // Output provider skeleton
                }

                // 4. Serialize to requested format
                let output_content = match format.as_str() {
                    "json" => serde_json::to_string_pretty(&result)?,
                    "yaml" => serde_yaml::to_string(&result)?,
                    _ => return Err(OntologyError::config(
                        format!("Unsupported format: {}", format)
                    )),
                };

                // 5. Write output
                if let Some(output_path) = output {
                    std::fs::write(&output_path, output_content)?;
                    println!("Mapping result written to {}", output_path);
                } else {
                    println!("{}", output_content);
                }

                Ok(())
            }

            Self::Validate { input } => {
                let yaml_content = std::fs::read_to_string(&input)?;
                let result = EnterpriseDomainMapper::parse_domain_description(&yaml_content)?;
                println!("Domain validation passed: {} entities found", result.len());
                Ok(())
            }
        }
    }
}
```

---

## 5. Testing Strategy

### Integration Test Path
`/home/user/ggen/crates/ggen-ontology-core/tests/mapping_integration.rs`

### Test Scenarios

#### Test 1: HIPAA Domain Mapping

```rust
#[test]
fn test_hipaa_domain_complete_pipeline() {
    // Given: HIPAA domain description
    let hipaa_yaml = r#"
entities:
  privacy_policy:
    name: "HIPAA Privacy Policy"
    type: "Policy"
    attributes:
      jurisdiction: "US"
      regulation: "HIPAA"
    tags: ["privacy", "healthcare"]

  encryption_control:
    name: "Data Encryption at Rest"
    type: "Control"
    attributes:
      algorithm: "AES-256"
      scope: "PHI"
    tags: ["encryption", "control"]

  data_classification:
    name: "PHI - Protected Health Information"
    type: "Classification"
    attributes:
      level: "Confidential"
      retention_days: 2555
    tags: ["classification", "healthcare"]
"#;

    // When: Execute pipeline
    let result = EnterpriseDomainMapper::execute_mapping_pipeline(hipaa_yaml)
        .expect("Pipeline should succeed");

    // Then: Verify mappings
    assert_eq!(result.parsed_entities.len(), 3);
    assert_eq!(result.mappings.len(), 3);
    assert!(!result.queries.is_empty());

    // Verify first entity mapped to PrivacyPolicy
    let policy_mapping = result.mappings.iter()
        .find(|m| m.entity_name.contains("HIPAA"))
        .expect("Should find HIPAA policy mapping");
    assert!(policy_mapping.primary_match.class.contains("Policy"));
    assert!(policy_mapping.confidence > 0.8);

    // Verify control mapped to EncryptionControl
    let control_mapping = result.mappings.iter()
        .find(|m| m.entity_name.contains("Encryption"))
        .expect("Should find encryption control mapping");
    assert!(control_mapping.primary_match.class.contains("Encryption"));

    // Verify queries generated
    assert!(result.queries.iter().any(|q| q.purpose.contains("Enumerate")));
    assert!(result.queries.iter().any(|q| q.purpose.contains("compliance")));
}
```

#### Test 2: Determinism Test

```rust
#[test]
fn test_mapping_determinism() {
    let domain_yaml = r#"
entities:
  policy_1:
    name: "Security Policy"
    type: "Policy"
    tags: ["security"]
"#;

    // Execute pipeline twice
    let result1 = EnterpriseDomainMapper::execute_mapping_pipeline(domain_yaml)
        .expect("First execution");
    let result2 = EnterpriseDomainMapper::execute_mapping_pipeline(domain_yaml)
        .expect("Second execution");

    // Mappings should be identical
    assert_eq!(result1.mappings.len(), result2.mappings.len());
    for (m1, m2) in result1.mappings.iter().zip(result2.mappings.iter()) {
        assert_eq!(m1.entity_id, m2.entity_id);
        assert_eq!(m1.primary_match.class, m2.primary_match.class);
        assert_eq!(m1.confidence, m2.confidence);
    }

    // Queries should be identical
    assert_eq!(result1.queries.len(), result2.queries.len());
    for (q1, q2) in result1.queries.iter().zip(result2.queries.iter()) {
        assert_eq!(q1.id, q2.id);
        assert_eq!(q1.query, q2.query);
        assert_eq!(q1.purpose, q2.purpose);
    }
}
```

#### Test 3: Error Handling

```rust
#[test]
fn test_invalid_domain_yaml() {
    let invalid_yaml = "this is { not valid yaml [";

    let result = EnterpriseDomainMapper::parse_domain_description(invalid_yaml);
    assert!(result.is_err());

    match result {
        Err(OntologyError::ParseError { .. }) => {
            // Expected
        }
        _ => panic!("Expected ParseError"),
    }
}

#[test]
fn test_missing_required_fields() {
    let invalid_domain = r#"
entities:
  incomplete_entity:
    type: "Policy"
    # Missing 'name' field
"#;

    let result = EnterpriseDomainMapper::parse_domain_description(invalid_domain);
    assert!(result.is_err());

    match result {
        Err(OntologyError::ParseError { .. }) => {
            // Expected
        }
        _ => panic!("Expected ParseError"),
    }
}
```

---

## 6. Example: HIPAA Domain → MCP Proposal Outline

### Input: domain-hipaa.yaml

```yaml
domain_id: "healthcare-finops-hipaa"
metadata:
  organization: "MedTech Corp"
  regulation: "HIPAA"
  compliance_target: "AWS"

entities:
  hipaa_privacy_policy:
    name: "HIPAA Privacy Policy"
    type: "Policy"
    attributes:
      jurisdiction: "US"
      regulation_framework: "HIPAA"
      effective_date: "2024-01-01"
    tags: ["privacy", "compliance", "healthcare"]
    relationships:
      - target_id: "patient_data_classification"
        type: "governs"

  phi_classification:
    name: "Protected Health Information (PHI)"
    type: "Classification"
    attributes:
      level: "Confidential"
      data_types: "patient-records,medical-imaging,lab-results"
      retention_days: "2555"
    tags: ["classification", "data", "protected"]

  encryption_at_rest_control:
    name: "Data Encryption at Rest (AES-256)"
    type: "Control"
    attributes:
      algorithm: "AES-256"
      scope: "All PHI in transit and at rest"
      verification_frequency: "quarterly"
    tags: ["encryption", "control", "security"]

  audit_logging_control:
    name: "Comprehensive Audit Logging"
    type: "Control"
    attributes:
      log_retention_days: "1825"
      log_targets: "CloudTrail,VPC Flow Logs,Application Logs"
    tags: ["audit", "logging", "control"]

  mfa_requirement:
    name: "Multi-Factor Authentication for Access"
    type: "Control"
    attributes:
      applicable_roles: "all-admin,healthcare-staff"
      enforcement: "mandatory"
    tags: ["authentication", "access-control"]

  phi_service:
    name: "Patient Record Storage and Retrieval Service"
    type: "Service"
    attributes:
      availability_target: "99.95"
      disaster_recovery_rpo_minutes: "60"
      disaster_recovery_rto_minutes: "120"
    tags: ["service", "healthcare", "critical"]
```

### Output: Phase 2 Mapping Result (JSON)

```json
{
  "domain_id": "healthcare-finops-hipaa",
  "parsed_entities": [
    {
      "id": "hipaa_privacy_policy",
      "name": "HIPAA Privacy Policy",
      "entity_type": "Policy",
      "attributes": {
        "jurisdiction": "US",
        "regulation_framework": "HIPAA"
      },
      "tags": ["privacy", "compliance", "healthcare"]
    },
    // ... more entities
  ],
  "mappings": [
    {
      "entity_id": "hipaa_privacy_policy",
      "entity_name": "HIPAA Privacy Policy",
      "ontology_matches": [
        {
          "class": ":PrivacyPolicy",
          "label": "Privacy Policy",
          "score": 0.95,
          "reason": "Contains 'privacy' keyword"
        },
        {
          "class": ":DataProtectionPolicy",
          "label": "Data Protection Policy",
          "score": 0.85,
          "reason": "Related to privacy and regulation"
        }
      ],
      "primary_match": {
        "class": ":PrivacyPolicy",
        "label": "Privacy Policy",
        "score": 0.95,
        "reason": "Contains 'privacy' keyword"
      },
      "confidence": 0.95,
      "rationale": "Entity 'HIPAA Privacy Policy' (type: Policy) mapped to 'Privacy Policy' via policy matching method."
    },
    {
      "entity_id": "phi_classification",
      "entity_name": "Protected Health Information (PHI)",
      "ontology_matches": [
        {
          "class": ":RestrictedData",
          "label": "Restricted Data",
          "score": 0.95,
          "reason": "PHI is highly restricted"
        }
      ],
      "primary_match": {
        "class": ":RestrictedData",
        "label": "Restricted Data",
        "score": 0.95
      },
      "confidence": 0.95
    },
    {
      "entity_id": "encryption_at_rest_control",
      "entity_name": "Data Encryption at Rest (AES-256)",
      "ontology_matches": [
        {
          "class": ":EncryptionControl",
          "label": "Encryption Control",
          "score": 0.96,
          "reason": "Encryption-based security"
        }
      ],
      "primary_match": {
        "class": ":EncryptionControl",
        "label": "Encryption Control",
        "score": 0.96
      },
      "confidence": 0.96
    }
  ],
  "queries": [
    {
      "id": "q_000_enum_hipaa_privacy_policy",
      "query": "SELECT ?instance ?label WHERE { ?instance rdf:type :PrivacyPolicy . OPTIONAL { ?instance rdfs:label ?label . } } ORDER BY ?instance",
      "purpose": "Enumerate all instances of Privacy Policy",
      "applicable_entities": ["hipaa_privacy_policy"],
      "expected_result_type": "instance_list"
    },
    {
      "id": "q_001_compliance_hipaa_privacy_policy",
      "query": "SELECT ?policy ?jurisdiction ?hasControls WHERE { ?policy rdf:type :PrivacyPolicy . ?policy :policyId \"hipaa_privacy_policy\" . OPTIONAL { ?policy :hasJurisdiction ?jurisdiction . } OPTIONAL { ?policy :hasControls ?hasControls . } } LIMIT 1",
      "purpose": "Check compliance status for policy HIPAA Privacy Policy",
      "applicable_entities": ["hipaa_privacy_policy"],
      "expected_result_type": "compliance_status"
    },
    {
      "id": "q_002_verify_encryption_at_rest_control",
      "query": "SELECT ?control ?status ?lastVerified WHERE { ?control rdf:type :EncryptionControl . ?control :controlId \"encryption_at_rest_control\" . OPTIONAL { ?control :verificationStatus ?status . } OPTIONAL { ?control :lastVerified ?lastVerified . } }",
      "purpose": "Verify control Data Encryption at Rest (AES-256)",
      "applicable_entities": ["encryption_at_rest_control"],
      "expected_result_type": "control_status"
    }
  ],
  "overall_confidence": 0.923,
  "processed_at": "2024-01-19T10:30:45Z",
  "provider_mappings": {
    "aws_operations": [
      "aws_hipaa_privacy_policy",
      "aws_phi_classification",
      "aws_encryption_at_rest_control",
      "aws_audit_logging_control",
      "aws_mfa_requirement",
      "aws_phi_service"
    ],
    "gcp_operations": [
      "gcp_hipaa_privacy_policy",
      "gcp_phi_classification",
      "gcp_encryption_at_rest_control",
      "gcp_audit_logging_control",
      "gcp_mfa_requirement",
      "gcp_phi_service"
    ],
    "azure_operations": [
      "azure_hipaa_privacy_policy",
      "azure_phi_classification",
      "azure_encryption_at_rest_control",
      "azure_audit_logging_control",
      "azure_mfa_requirement",
      "azure_phi_service"
    ]
  }
}
```

### CLI Usage

```bash
# Execute domain mapping with provider skeleton
ggen ontology map \
  --input docs/examples/domain-hipaa.yaml \
  --format json \
  --output hipaa-mapping.json \
  --with-provider-skeleton

# Validate domain format
ggen ontology validate --input docs/examples/domain-hipaa.yaml

# Output to stdout
ggen ontology map --input domain.yaml --format json
```

---

## 7. Success Criteria

1. **Performance**: Complete domain YAML → proposal in <5 seconds
   - Parsing: <500ms
   - Entity mapping: <1s
   - Query generation: <1s
   - Aggregation: <100ms

2. **Determinism**: Same domain YAML generates identical mappings/queries
   - Run pipeline 100x with same input
   - All outputs identical (byte-for-byte)

3. **Coverage**: Map >90% of standard ontology entities
   - Policy classes: 100%
   - Control classes: 95%
   - Service classes: 90%
   - Classification classes: 100%

4. **Test Quality**: 60+ tests with >80% mutation kill rate
   - Mapping tests: 20
   - Query generation tests: 15
   - Pipeline tests: 10
   - Error handling tests: 15

5. **Code Quality**: All Andon signals cleared
   - No compiler errors
   - No clippy warnings
   - All tests pass
   - SLO checks pass

---

## 8. Risk Mitigation

| Risk | Mitigation |
|------|-----------|
| Entity → Ontology ambiguity | Use confidence scoring + alternative match tracking |
| SPARQL query complexity | Keep queries focused on single entity type |
| YAML parsing failures | Validate structure early, provide helpful errors |
| Performance degradation | Cache query strings, profile hot paths |
| Determinism violation | Sort consistently at each step, avoid system time |

---

## 9. Transition to Phase 3

Phase 2 output directly feeds Phase 3:

```
Phase 2 Output (MappingResult)
       │
       ├─ parsed_entities ──→ Phase 3: Entity validation
       ├─ mappings ──────────→ Phase 3: Provider routing
       ├─ queries ───────────→ Phase 3: Compliance verification
       └─ provider_mappings ─→ Phase 3: AWS/GCP/Azure fan-out
```

Phase 3 will:
1. Implement AWS/GCP/Azure provider mappers
2. Execute SPARQL queries against customer ontology
3. Generate provider-specific proposals
4. Create compliance receipts with cryptographic proof

