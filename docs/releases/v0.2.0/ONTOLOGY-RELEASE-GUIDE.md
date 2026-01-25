<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Ontology Release Guide - ggen v0.2.0](#ontology-release-guide---ggen-v020)
  - [Table of Contents](#table-of-contents)
  - [Installation](#installation)
    - [Via Cargo](#via-cargo)
    - [Via Cargo.toml](#via-cargotoml)
    - [Building from Source](#building-from-source)
    - [System Requirements](#system-requirements)
  - [Quick Start](#quick-start)
    - [1. Load an Ontology](#1-load-an-ontology)
    - [2. Query with SPARQL](#2-query-with-sparql)
    - [3. Map to Rust Types](#3-map-to-rust-types)
    - [4. Validate Entities](#4-validate-entities)
  - [Core Concepts](#core-concepts)
    - [RDF Triple Store](#rdf-triple-store)
    - [SPARQL Queries](#sparql-queries)
    - [Entity Mapping](#entity-mapping)
    - [Validators](#validators)
  - [Usage Patterns](#usage-patterns)
    - [Pattern 1: Load and Query](#pattern-1-load-and-query)
    - [Pattern 2: Map and Transform](#pattern-2-map-and-transform)
    - [Pattern 3: Validate and Filter](#pattern-3-validate-and-filter)
    - [Pattern 4: Generate Infrastructure Code](#pattern-4-generate-infrastructure-code)
    - [Pattern 5: Real-Time Validation Loop](#pattern-5-real-time-validation-loop)
  - [Domain Ontologies](#domain-ontologies)
    - [Legal Ontology](#legal-ontology)
    - [IT Infrastructure Ontology](#it-infrastructure-ontology)
    - [Cloud Security Ontology](#cloud-security-ontology)
  - [Cloud Bindings](#cloud-bindings)
    - [AWS CloudFormation Binding](#aws-cloudformation-binding)
    - [GCP Terraform Binding](#gcp-terraform-binding)
    - [Azure ARM Template Binding](#azure-arm-template-binding)
  - [Advanced Patterns](#advanced-patterns)
    - [Custom Entity Mapping](#custom-entity-mapping)
    - [Composite Validators](#composite-validators)
    - [Streaming SPARQL Results](#streaming-sparql-results)
    - [Incremental Ontology Updates](#incremental-ontology-updates)
  - [Performance Tuning](#performance-tuning)
    - [RDF Loading Optimization](#rdf-loading-optimization)
    - [Query Optimization](#query-optimization)
    - [Memory Management](#memory-management)
  - [Troubleshooting](#troubleshooting)
    - [Issue: "Entity Not Found"](#issue-entity-not-found)
    - [Issue: "SPARQL Parse Error"](#issue-sparql-parse-error)
    - [Issue: "Validation Failed"](#issue-validation-failed)
    - [Issue: "Out of Memory"](#issue-out-of-memory)
  - [Examples](#examples)
    - [Example 1: Load Legal Ontology](#example-1-load-legal-ontology)
    - [Example 2: Entity Mapping](#example-2-entity-mapping)
    - [Example 3: Cloud Generation](#example-3-cloud-generation)
    - [Example 4: Validation](#example-4-validation)
  - [API Reference](#api-reference)
    - [Core Types](#core-types)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Ontology Release Guide - ggen v0.2.0

Complete guide to using ggen-ontology-core v0.2.0 for RDF processing and deterministic code generation.

---

## Table of Contents

1. [Installation](#installation)
2. [Quick Start](#quick-start)
3. [Core Concepts](#core-concepts)
4. [Usage Patterns](#usage-patterns)
5. [Domain Ontologies](#domain-ontologies)
6. [Cloud Bindings](#cloud-bindings)
7. [Advanced Patterns](#advanced-patterns)
8. [Performance Tuning](#performance-tuning)
9. [Troubleshooting](#troubleshooting)
10. [Examples](#examples)

---

## Installation

### Via Cargo

```bash
cargo add ggen-ontology-core --version 0.2.0
```

### Via Cargo.toml

```toml
[dependencies]
ggen-ontology-core = "0.2.0"
tokio = { version = "1.47", features = ["full"] }
```

### Building from Source

```bash
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen/crates/ggen-ontology-core
cargo build --release
cargo test
cargo run --example load_legal
```

### System Requirements

- Rust 1.75+
- 50MB+ disk space
- 256MB+ RAM (minimum)
- tokio runtime or compatible async executor

---

## Quick Start

### 1. Load an Ontology

```rust
use ggen_ontology_core::prelude::*;

#[tokio::main]
async fn main() -> Result<()> {
    // Load legal ontology from Turtle file
    let ontology = load_ontology_from_file("schemas/legal.ttl")
        .await?;

    println!("Loaded ontology with {} triples",
        ontology.triple_count());

    Ok(())
}
```

### 2. Query with SPARQL

```rust
let results = ontology.query_sparql(
    "SELECT ?contract ?party WHERE {
        ?contract a :Contract ;
                  :hasParty ?party .
    }"
)?;

for binding in results {
    let contract = binding.get("contract")?;
    let party = binding.get("party")?;
    println!("Contract: {:?}, Party: {:?}", contract, party);
}
```

### 3. Map to Rust Types

```rust
#[derive(RdfEntity)]
struct Contract {
    #[rdf(subject)]
    id: String,

    #[rdf(predicate = "legal:hasParty")]
    parties: Vec<String>,

    #[rdf(predicate = "legal:startDate")]
    start_date: String,
}

let mapper = EntityMapper::<Contract>::new(&ontology);
let contracts = mapper.map_all()?;

for contract in contracts {
    println!("Contract {:?} has {} parties",
        contract.id, contract.parties.len());
}
```

### 4. Validate Entities

```rust
let validator = ContractValidator::new();

for contract in &contracts {
    match validator.validate(contract) {
        Ok(_) => println!("✓ Contract valid"),
        Err(e) => println!("✗ Validation error: {}", e),
    }
}
```

---

## Core Concepts

### RDF Triple Store

**What it is:** In-memory storage for RDF triples (Subject-Predicate-Object).

**Why:** Provides SPARQL query interface over semantic knowledge graphs.

```
Subject    │ Predicate │ Object
─────────────────────────────────────
contract:1 │ a         │ Contract
contract:1 │ hasParty  │ party:acme
contract:1 │ startDate │ "2024-01-01"
```

### SPARQL Queries

**What they are:** Declarative queries for RDF data (similar to SQL).

**Common patterns:**

```sparql
# Find all contracts
SELECT ?contract WHERE {
    ?contract a :Contract
}

# Find contracts with specific party
SELECT ?contract WHERE {
    ?contract :hasParty :party_acme
}

# Count contracts by party
SELECT ?party (COUNT(?contract) as ?count) WHERE {
    ?contract :hasParty ?party
} GROUP BY ?party
```

### Entity Mapping

**What it is:** Bidirectional mapping between RDF entities and Rust types.

**Why:** Type-safe, ergonomic access to ontology data.

```rust
// RDF triple store → Rust struct
let contract_rdf = ontology.get_entity("contract:1")?;
let contract: Contract = mapper.map_entity(contract_rdf)?;

// Rust struct → RDF triple store
let new_contract = Contract {
    id: "contract:2".to_string(),
    parties: vec!["party:acme".to_string()],
    start_date: "2024-02-01".to_string(),
};
let rdf_entity = mapper.unmap_entity(&new_contract)?;
ontology.insert_entity(rdf_entity)?;
```

### Validators

**What they are:** Rules for ensuring data quality and consistency.

**Why:** Catch invalid data before it propagates.

```rust
// Built-in validators
validator.validate_required_fields(&contract)?;
validator.validate_entity_relationships(&contract)?;
validator.validate_value_ranges(&contract)?;

// Custom validators
let my_validator = CustomValidator::new()
    .with_rule(|e| e.parties.len() > 0)
    .with_message("Contract must have at least one party");
```

---

## Usage Patterns

### Pattern 1: Load and Query

**Use case:** Explore ontology data programmatically.

```rust
#[tokio::main]
async fn main() -> Result<()> {
    // Load ontology
    let ontology = load_ontology_from_file("legal.ttl").await?;

    // Query for all contracts
    let results = ontology.query_sparql(
        "SELECT ?contract ?date WHERE {
            ?contract a :Contract ;
                      :startDate ?date
        }"
    )?;

    // Process results
    for row in results {
        println!("{:?}", row);
    }

    Ok(())
}
```

### Pattern 2: Map and Transform

**Use case:** Convert RDF entities to application domain types.

```rust
#[tokio::main]
async fn main() -> Result<()> {
    let ontology = load_ontology_from_file("legal.ttl").await?;
    let mapper = EntityMapper::<Contract>::new(&ontology);

    // Get all contracts
    let contracts = mapper.map_all()?;

    // Transform to business types
    let business_contracts: Vec<BusinessContract> = contracts
        .into_iter()
        .map(|c| BusinessContract {
            id: c.id,
            party_count: c.parties.len(),
            status: "active".to_string(),
        })
        .collect();

    // Save or serialize
    serde_json::to_string_pretty(&business_contracts)?;

    Ok(())
}
```

### Pattern 3: Validate and Filter

**Use case:** Ensure data quality before processing.

```rust
#[tokio::main]
async fn main() -> Result<()> {
    let ontology = load_ontology_from_file("legal.ttl").await?;
    let mapper = EntityMapper::<Contract>::new(&ontology);
    let validator = ContractValidator::new();

    // Get all contracts
    let mut contracts = mapper.map_all()?;

    // Filter to valid contracts only
    contracts.retain(|c| validator.validate(c).is_ok());

    println!("Valid contracts: {}", contracts.len());

    Ok(())
}
```

### Pattern 4: Generate Infrastructure Code

**Use case:** Create cloud configurations from semantic specs.

```rust
#[tokio::main]
async fn main() -> Result<()> {
    // Load cloud security ontology
    let ontology = load_ontology_from_file("cloud-security.ttl").await?;

    // Map to infrastructure specs
    let mapper = EntityMapper::<SecurityPolicy>::new(&ontology);
    let policies = mapper.map_all()?;

    // Generate AWS CloudFormation template
    let cf_template = generate_cloudformation(&policies)?;
    std::fs::write("template.yaml", cf_template)?;

    // Generate GCP Terraform config
    let tf_config = generate_terraform(&policies)?;
    std::fs::write("main.tf", tf_config)?;

    Ok(())
}
```

### Pattern 5: Real-Time Validation Loop

**Use case:** Continuous validation during interactive workflows.

```rust
#[tokio::main]
async fn main() -> Result<()> {
    let ontology = load_ontology_from_file("legal.ttl").await?;
    let validator = ContractValidator::new();

    loop {
        // Get user input
        let contract_id = prompt_user("Enter contract ID: ")?;

        // Fetch and validate
        match ontology.get_entity(&contract_id) {
            Ok(entity) => {
                match validator.validate_entity(&entity) {
                    Ok(_) => println!("✓ Valid"),
                    Err(e) => println!("✗ Error: {}", e),
                }
            }
            Err(_) => println!("✗ Not found"),
        }
    }
}
```

---

## Domain Ontologies

### Legal Ontology

**File:** `schemas/legal.ttl`

**Entities:**
- `Contract`: Purchase agreements, service contracts, NDAs
- `Party`: Organizations, individuals, legal entities
- `Clause`: Contract terms, conditions, obligations
- `Obligation`: Duties, liabilities, warranties
- `Term`: Duration specifications, renewal clauses

**Key relationships:**
```
Contract --[hasParty]--> Party
Contract --[hasClause]--> Clause
Clause --[imposes]--> Obligation
Obligation --[expires]--> Term
```

**Query example:**
```sparql
SELECT ?contract ?party WHERE {
    ?contract a :Contract ;
              :hasParty ?party ;
              :startDate ?date
    FILTER (?date >= "2024-01-01"^^xsd:date)
}
```

**Validation rules:**
- Every contract must have at least one party
- Every clause must specify its obligation
- Start date must precede end date
- Parties must be valid legal entities

### IT Infrastructure Ontology

**File:** `schemas/it-infrastructure.ttl`

**Entities:**
- `System`: Applications, microservices, platforms
- `Service`: API endpoints, background jobs, cron tasks
- `Component`: Libraries, modules, packages
- `Technology`: Programming languages, frameworks, databases
- `Dependency`: Version specifications, feature requirements

**Key relationships:**
```
System --[contains]--> Service
Service --[uses]--> Component
Component --[depends]--> Technology
Technology --[requires]--> Technology
```

**Query example:**
```sparql
SELECT ?service ?tech WHERE {
    ?system :contains ?service .
    ?service :uses ?comp .
    ?comp :builtWith ?tech .
}
```

**Validation rules:**
- No circular service dependencies
- Technology versions must be compatible
- Required components must be specified
- Services must have at least one technology

### Cloud Security Ontology

**File:** `schemas/cloud-security.ttl`

**Entities:**
- `AccessControl`: IAM policies, roles, permissions
- `EncryptionPolicy`: Encryption algorithms, key management
- `Threat`: Security risks, attack vectors
- `ComplianceFramework`: Regulations, standards (ISO, SOC2, HIPAA)

**Key relationships:**
```
AccessControl --[enforces]--> Permission
EncryptionPolicy --[protects]--> Data
Threat --[requires]--> ComplianceFramework
ComplianceFramework --[mandates]--> AccessControl
```

**Query example:**
```sparql
SELECT ?policy ?threat WHERE {
    ?threat a :Threat .
    ?policy :mitigates ?threat ;
            :uses ?encryption
}
```

**Validation rules:**
- All sensitive data must be encrypted
- All threats must have mitigation policies
- Access controls must follow principle of least privilege
- Compliance requirements must be documented

---

## Cloud Bindings

### AWS CloudFormation Binding

**Generate AWS resources from security ontology:**

```rust
use ggen_ontology_core::cloud::AwsCloudFormation;

let ontology = load_ontology_from_file("cloud-security.ttl").await?;
let generator = AwsCloudFormation::new(&ontology);

// Generate IAM policy from access control spec
let iam_policy = generator.generate_iam_policy()?;

// Generate VPC from network spec
let vpc_config = generator.generate_vpc()?;

// Generate RDS from database spec
let rds_config = generator.generate_rds()?;

// Write to CloudFormation template
let template = serde_json::json!({
    "AWSTemplateFormatVersion": "2010-09-09",
    "Resources": {
        "IAMPolicy": iam_policy,
        "VPC": vpc_config,
        "Database": rds_config,
    }
});

std::fs::write("template.json", serde_json::to_string_pretty(&template)?)?;
```

**Supported resources:**
- IAM Roles, Policies, Users
- VPC, Subnets, Security Groups
- EC2 Instances, Launch Templates
- RDS, DynamoDB, S3
- Lambda Functions, API Gateway
- CloudWatch Alarms

### GCP Terraform Binding

**Generate GCP infrastructure from specs:**

```rust
use ggen_ontology_core::cloud::GcpTerraform;

let ontology = load_ontology_from_file("cloud-security.ttl").await?;
let generator = GcpTerraform::new(&ontology);

// Generate Compute resources
let compute = generator.generate_compute_engine()?;

// Generate networking
let network = generator.generate_network()?;

// Generate Cloud Run services
let cloud_run = generator.generate_cloud_run()?;

// Write Terraform config
let tf_code = format!(
    "{}\\n{}\\n{}",
    compute, network, cloud_run
);

std::fs::write("main.tf", tf_code)?;
```

**Supported resources:**
- Compute Engine, GKE, Cloud Run
- Cloud SQL, Cloud Firestore
- VPC, Cloud NAT, Cloud DNS
- Cloud Storage, BigQuery
- Service Accounts, IAM

### Azure ARM Template Binding

**Generate Azure resources:**

```rust
use ggen_ontology_core::cloud::AzureArm;

let ontology = load_ontology_from_file("cloud-security.ttl").await?;
let generator = AzureArm::new(&ontology);

// Generate resource groups
let resource_group = generator.generate_resource_group()?;

// Generate RBAC policies
let rbac = generator.generate_rbac()?;

// Generate networking
let network = generator.generate_network()?;

// Write ARM template
let template = serde_json::json!({
    "$schema": "https://schema.management.azure.com/schemas/2019-04-01/deploymentTemplate.json#",
    "resources": [
        resource_group,
        rbac,
        network,
    ]
});

std::fs::write("template.json", serde_json::to_string_pretty(&template)?)?;
```

**Supported resources:**
- Resource Groups, Virtual Machines
- App Service, Azure Functions
- SQL Database, Cosmos DB
- Virtual Networks, Network Security Groups
- Key Vault, Managed Identity

---

## Advanced Patterns

### Custom Entity Mapping

```rust
#[derive(RdfEntity)]
#[rdf(namespace = "legal:")]
struct CustomContract {
    #[rdf(subject)]
    id: String,

    #[rdf(predicate = "hasParty", class = "Party")]
    parties: Vec<Party>,

    #[rdf(predicate = "metadata")]
    #[serde(flatten)]
    metadata: HashMap<String, String>,
}
```

### Composite Validators

```rust
let validator = CompositeValidator::new()
    .with_required_fields(vec!["id", "parties"])
    .with_relationship_validator()
    .with_value_range_validator()
    .with_custom_rule(|e| e.parties.len() > 0);
```

### Streaming SPARQL Results

```rust
let results = ontology
    .query_sparql_streaming(
        "SELECT ?contract WHERE { ?contract a :Contract }"
    )?;

// Process results one at a time
futures::pin_mut!(results);
while let Some(binding) = results.next().await? {
    process_contract(&binding)?;
}
```

### Incremental Ontology Updates

```rust
let mut ontology = load_ontology_from_file("legal.ttl").await?;

// Add new contract
ontology.insert_triple(
    "contract:new",
    "a",
    "Contract"
)?;

// Update existing relationship
ontology.remove_triple("contract:1", "hasParty", "party:old")?;
ontology.insert_triple("contract:1", "hasParty", "party:new")?;

// Save changes
ontology.save_to_file("legal.ttl").await?;
```

---

## Performance Tuning

### RDF Loading Optimization

```rust
// Disable validation during initial load
let ontology = load_ontology_from_file("large.ttl")
    .await?
    .with_validation_disabled();

// Process in chunks
let chunk_size = 1000;
for chunk in ontology.chunks(chunk_size) {
    process_chunk(&chunk)?;
}

// Re-enable validation after loading
ontology.validate_all()?;
```

### Query Optimization

```rust
// Prefer specific filters to broad queries
// ❌ SLOW: SELECT * WHERE { ?s ?p ?o }
// ✅ FAST: SELECT ?contract WHERE { ?contract a :Contract }

// Use indexed predicates
ontology.create_index("a")?;      // Entity types
ontology.create_index("hasParty")?; // Relationships

// Limit result sets
let results = ontology.query_sparql_with_limit(
    "SELECT ?contract ...",
    100
)?;
```

### Memory Management

```rust
// Enable entity cache with LRU eviction
let mapper = EntityMapper::<Contract>::new(&ontology)
    .with_cache(100_000); // Cache 100k entities

// Clear cache when switching queries
mapper.clear_cache();

// Monitor memory usage
println!("Memory: {} MB", ontology.memory_usage_mb());
```

---

## Troubleshooting

### Issue: "Entity Not Found"

**Cause:** Entity doesn't exist in ontology or wrong namespace

**Solution:**
```rust
// Check if entity exists
if ontology.has_entity("contract:1")? {
    // Proceed
} else {
    // Use full namespace
    ontology.get_entity("http://example.com/contract/1")?;
}
```

### Issue: "SPARQL Parse Error"

**Cause:** Malformed SPARQL query or undefined namespace

**Solution:**
```rust
// Validate query before execution
let valid = ontology.validate_sparql_query(query)?;

// Define namespaces explicitly
let query = r#"
PREFIX legal: <http://example.com/legal#>
SELECT ?contract WHERE { ?contract a legal:Contract }
"#;
```

### Issue: "Validation Failed"

**Cause:** Entity doesn't meet validation rules

**Solution:**
```rust
// Get detailed error messages
match validator.validate(&contract) {
    Ok(_) => {},
    Err(e) => {
        println!("Validation failed: {}", e.message);
        println!("Context: {:?}", e.context);
        println!("Suggestions: {:?}", e.suggestions);
    }
}
```

### Issue: "Out of Memory"

**Cause:** Loading very large ontologies (>100k triples)

**Solution:**
```rust
// Stream large ontologies
let reader = streaming_reader("large.ttl")?;
for triple in reader {
    process_triple(&triple)?;
}

// Or split into smaller files
// large.ttl → legal.ttl, infrastructure.ttl, security.ttl
```

---

## Examples

### Example 1: Load Legal Ontology

```bash
cargo run --example load_legal -- --file schemas/legal.ttl
```

### Example 2: Entity Mapping

```bash
cargo run --example entity_mapping -- \
    --ontology schemas/legal.ttl \
    --output contracts.json
```

### Example 3: Cloud Generation

```bash
cargo run --example cloud_generation -- \
    --ontology schemas/cloud-security.ttl \
    --platform aws \
    --output template.yaml
```

### Example 4: Validation

```bash
cargo run --example validation -- \
    --ontology schemas/legal.ttl \
    --contracts contracts.json
```

---

## API Reference

### Core Types

```rust
// RDF TripleStore
pub struct RdfTripleStore {
    pub store: MemoryStore,
}

impl RdfTripleStore {
    pub async fn load_from_file(path: &str) -> Result<Self>;
    pub fn query_sparql(&self, query: &str) -> Result<Vec<Binding>>;
    pub fn insert_triple(&mut self, s: &str, p: &str, o: &str) -> Result<()>;
    pub fn has_entity(&self, entity: &str) -> Result<bool>;
}

// Entity Mapper
pub struct EntityMapper<T: RdfEntity> {
    pub map_entity(&self, rdf: &RdfEntity) -> Result<T>;
    pub map_all(&self) -> Result<Vec<T>>;
    pub unmap_entity(&self, entity: &T) -> Result<RdfEntity>;
}

// Validator
pub trait Validator {
    fn validate(&self, entity: &RdfEntity) -> Result<()>;
    fn validate_entity<T: RdfEntity>(&self, entity: &T) -> Result<()>;
}
```

---

**For more information, see:**
- `/docs/releases/v0.2.0/RELEASE-NOTES-v0.2.0.md` - Full release notes
- `/examples/ontology/` - Working code examples
- GitHub issues - Questions and bug reports
