<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Graph Evolution and Validation Strategy](#graph-evolution-and-validation-strategy)
  - [1. Overview](#1-overview)
  - [2. Validation Architecture](#2-validation-architecture)
    - [2.1 Multi-Layer Validation](#21-multi-layer-validation)
    - [2.2 Implementation](#22-implementation)
  - [3. Layer Implementations](#3-layer-implementations)
    - [3.1 Syntax Validator](#31-syntax-validator)
    - [3.2 Schema Validator](#32-schema-validator)
    - [3.3 SPARQL Constraint Validator](#33-sparql-constraint-validator)
    - [3.4 Integrity Validator](#34-integrity-validator)
  - [4. Transaction Management and Rollback](#4-transaction-management-and-rollback)
    - [4.1 Transaction Log](#41-transaction-log)
    - [4.2 Automatic Rollback on Failure](#42-automatic-rollback-on-failure)
  - [5. Performance Optimization](#5-performance-optimization)
    - [5.1 Validation Caching](#51-validation-caching)
    - [5.2 Parallel Validation](#52-parallel-validation)
  - [6. Quality Metrics](#6-quality-metrics)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Graph Evolution and Validation Strategy

**Document**: Graph Evolution and Validation Strategy v1.0
**Date**: 2025-10-10
**Purpose**: Define deterministic validation, rollback mechanisms, and graph consistency guarantees

## 1. Overview

This strategy ensures that all graph changes are validated deterministically before commit, with complete rollback capability and zero tolerance for graph corruption.

## 2. Validation Architecture

### 2.1 Multi-Layer Validation

```
Input Graph Delta
       │
       v
┌──────────────────────────────────┐
│  Layer 1: Syntax Validation      │
│  - Valid RDF/Turtle syntax       │
│  - Well-formed URIs              │
│  - Type correctness              │
└────────┬─────────────────────────┘
         │ (pass)
         v
┌──────────────────────────────────┐
│  Layer 2: Schema Validation      │
│  - OWL/RDFS compliance           │
│  - SHACL constraints             │
│  - Domain/range restrictions     │
└────────┬─────────────────────────┘
         │ (pass)
         v
┌──────────────────────────────────┐
│  Layer 3: SPARQL Validation      │
│  - Custom constraint queries     │
│  - Business rules                │
│  - Consistency checks            │
└────────┬─────────────────────────┘
         │ (pass)
         v
┌──────────────────────────────────┐
│  Layer 4: Integrity Validation   │
│  - Referential integrity         │
│  - No dangling references        │
│  - Complete subgraphs            │
└────────┬─────────────────────────┘
         │ (pass)
         v
┌──────────────────────────────────┐
│  Layer 5: Impact Analysis        │
│  - Affected templates            │
│  - Breaking changes detection    │
│  - Dependency analysis           │
└────────┬─────────────────────────┘
         │ (pass)
         v
    COMMIT TO GRAPH
```

### 2.2 Implementation

```rust
use ggen_core::Graph;
use oxrdf::{Triple, Subject, NamedNode};
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct GraphDelta {
    pub additions: Vec<Triple>,
    pub deletions: Vec<Triple>,
    pub metadata: DeltaMetadata,
}

#[derive(Debug, Clone)]
pub struct DeltaMetadata {
    pub transaction_id: String,
    pub timestamp: DateTime<Utc>,
    pub source_agent: String,
    pub reasoning: String,
    pub confidence: f64,
}

pub struct MultiLayerValidator {
    syntax_validator: SyntaxValidator,
    schema_validator: SchemaValidator,
    sparql_validator: SparqlValidator,
    integrity_validator: IntegrityValidator,
    impact_analyzer: ImpactAnalyzer,
}

impl MultiLayerValidator {
    pub async fn validate_delta(&self, graph: &Graph, delta: &GraphDelta) -> Result<ValidationReport> {
        let mut report = ValidationReport::new();

        // Layer 1: Syntax
        let syntax_result = self.syntax_validator.validate(delta)?;
        report.add_layer_result("syntax", syntax_result.clone());
        if !syntax_result.is_valid {
            return Ok(report);
        }

        // Create test graph with delta applied
        let test_graph = self.create_test_graph(graph, delta)?;

        // Layer 2: Schema
        let schema_result = self.schema_validator.validate(&test_graph)?;
        report.add_layer_result("schema", schema_result.clone());
        if !schema_result.is_valid {
            return Ok(report);
        }

        // Layer 3: SPARQL constraints
        let sparql_result = self.sparql_validator.validate(&test_graph)?;
        report.add_layer_result("sparql", sparql_result.clone());
        if !sparql_result.is_valid {
            return Ok(report);
        }

        // Layer 4: Integrity
        let integrity_result = self.integrity_validator.validate(&test_graph)?;
        report.add_layer_result("integrity", integrity_result.clone());
        if !integrity_result.is_valid {
            return Ok(report);
        }

        // Layer 5: Impact
        let impact_result = self.impact_analyzer.analyze(graph, delta)?;
        report.add_layer_result("impact", impact_result.clone());

        Ok(report)
    }

    fn create_test_graph(&self, graph: &Graph, delta: &GraphDelta) -> Result<Graph> {
        // Clone graph
        let mut test_graph = graph.clone()?;

        // Apply deletions
        for triple in &delta.deletions {
            test_graph.remove_triple(triple)?;
        }

        // Apply additions
        for triple in &delta.additions {
            test_graph.add_triple(triple.clone())?;
        }

        Ok(test_graph)
    }
}

#[derive(Debug, Clone)]
pub struct ValidationReport {
    pub transaction_id: String,
    pub overall_valid: bool,
    pub layers: HashMap<String, LayerResult>,
    pub errors: Vec<ValidationError>,
    pub warnings: Vec<ValidationWarning>,
}

#[derive(Debug, Clone)]
pub struct LayerResult {
    pub layer_name: String,
    pub is_valid: bool,
    pub errors: Vec<String>,
    pub duration: Duration,
}
```

## 3. Layer Implementations

### 3.1 Syntax Validator

```rust
pub struct SyntaxValidator {
    allowed_namespaces: HashSet<String>,
}

impl SyntaxValidator {
    pub fn validate(&self, delta: &GraphDelta) -> Result<LayerResult> {
        let start = Instant::now();
        let mut errors = Vec::new();

        // Validate additions
        for triple in &delta.additions {
            if let Err(e) = self.validate_triple(triple) {
                errors.push(format!("Invalid triple: {}", e));
            }
        }

        // Validate deletions
        for triple in &delta.deletions {
            if let Err(e) = self.validate_triple(triple) {
                errors.push(format!("Invalid triple: {}", e));
            }
        }

        Ok(LayerResult {
            layer_name: "syntax".to_string(),
            is_valid: errors.is_empty(),
            errors,
            duration: start.elapsed(),
        })
    }

    fn validate_triple(&self, triple: &Triple) -> Result<()> {
        // 1. Validate subject (must be IRI or blank node)
        match &triple.subject {
            Subject::NamedNode(node) => {
                self.validate_iri(&node.as_str())?;
            }
            Subject::BlankNode(_) => {
                // Blank nodes are valid
            }
            _ => {
                return Err(GgenAiError::InvalidTriple("Invalid subject type".to_string()));
            }
        }

        // 2. Validate predicate (must be IRI)
        self.validate_iri(triple.predicate.as_str())?;

        // 3. Validate object
        // (can be IRI, blank node, or literal - all valid)

        Ok(())
    }

    fn validate_iri(&self, iri: &str) -> Result<()> {
        // Check if well-formed URI
        if !iri.starts_with("http://") && !iri.starts_with("https://") && !iri.starts_with("urn:") {
            return Err(GgenAiError::InvalidIri(format!("Malformed IRI: {}", iri)));
        }

        // Check if namespace is allowed
        let namespace = iri.split('#').next()
            .or_else(|| iri.rsplit('/').nth(1))
            .unwrap_or("");

        if !self.allowed_namespaces.is_empty() && !self.allowed_namespaces.contains(namespace) {
            return Err(GgenAiError::InvalidIri(format!("Disallowed namespace: {}", namespace)));
        }

        Ok(())
    }
}
```

### 3.2 Schema Validator

```rust
use shacl::ShaclValidator;
use oxigraph::model::NamedNodeRef;

pub struct SchemaValidator {
    shacl_shapes: Vec<String>,
    owl_ontology: Option<Graph>,
}

impl SchemaValidator {
    pub fn validate(&self, graph: &Graph) -> Result<LayerResult> {
        let start = Instant::now();
        let mut errors = Vec::new();

        // 1. SHACL validation
        for shape in &self.shacl_shapes {
            match self.validate_shacl_shape(graph, shape) {
                Ok(violations) => {
                    for violation in violations {
                        errors.push(format!("SHACL violation: {}", violation));
                    }
                }
                Err(e) => {
                    errors.push(format!("SHACL validation error: {}", e));
                }
            }
        }

        // 2. OWL validation
        if let Some(ontology) = &self.owl_ontology {
            match self.validate_owl_constraints(graph, ontology) {
                Ok(violations) => {
                    for violation in violations {
                        errors.push(format!("OWL violation: {}", violation));
                    }
                }
                Err(e) => {
                    errors.push(format!("OWL validation error: {}", e));
                }
            }
        }

        Ok(LayerResult {
            layer_name: "schema".to_string(),
            is_valid: errors.is_empty(),
            errors,
            duration: start.elapsed(),
        })
    }

    fn validate_shacl_shape(&self, graph: &Graph, shape: &str) -> Result<Vec<String>> {
        // Parse SHACL shape
        let shape_graph = Graph::parse(shape, "text/turtle")?;

        // Run SHACL validation
        let validator = ShaclValidator::new(&shape_graph)?;
        let report = validator.validate(graph)?;

        // Extract violations
        let violations = self.extract_shacl_violations(&report)?;

        Ok(violations)
    }

    fn validate_owl_constraints(&self, graph: &Graph, ontology: &Graph) -> Result<Vec<String>> {
        let mut violations = Vec::new();

        // Check domain/range restrictions
        violations.extend(self.check_domain_range(graph, ontology)?);

        // Check cardinality constraints
        violations.extend(self.check_cardinality(graph, ontology)?);

        // Check disjoint classes
        violations.extend(self.check_disjoint_classes(graph, ontology)?);

        Ok(violations)
    }

    fn check_domain_range(&self, graph: &Graph, ontology: &Graph) -> Result<Vec<String>> {
        let query = r#"
            SELECT ?property ?domain ?range WHERE {
                ?property rdfs:domain ?domain .
                ?property rdfs:range ?range .
            }
        "#;

        let results = ontology.query(query)?;
        let mut violations = Vec::new();

        for result in results {
            let property = result.get("property")?;
            let domain = result.get("domain")?;
            let range = result.get("range")?;

            // Check all triples using this property
            let usage_query = format!(r#"
                SELECT ?subject ?object WHERE {{
                    ?subject <{}> ?object .
                }}
            "#, property);

            let usages = graph.query(&usage_query)?;

            for usage in usages {
                let subject = usage.get("subject")?;
                let object = usage.get("object")?;

                // Validate subject is instance of domain
                if !self.is_instance_of(graph, subject, domain)? {
                    violations.push(format!(
                        "Subject {} is not of type {} for property {}",
                        subject, domain, property
                    ));
                }

                // Validate object is instance of range
                if !self.is_instance_of(graph, object, range)? {
                    violations.push(format!(
                        "Object {} is not of type {} for property {}",
                        object, range, property
                    ));
                }
            }
        }

        Ok(violations)
    }

    fn is_instance_of(&self, graph: &Graph, instance: &str, class: &str) -> Result<bool> {
        let query = format!(r#"
            ASK {{
                <{}> a <{}> .
            }}
        "#, instance, class);

        graph.query_ask(&query)
    }
}
```

### 3.3 SPARQL Constraint Validator

```rust
pub struct SparqlValidator {
    constraint_queries: Vec<SparqlConstraint>,
}

#[derive(Debug, Clone)]
pub struct SparqlConstraint {
    pub name: String,
    pub query: String,
    pub constraint_type: ConstraintType,
    pub severity: Severity,
}

#[derive(Debug, Clone)]
pub enum ConstraintType {
    /// ASK query that should return false (violation if true)
    Prohibition,
    /// ASK query that should return true (violation if false)
    Requirement,
    /// SELECT query that should return no results
    NoResults,
}

#[derive(Debug, Clone)]
pub enum Severity {
    Error,
    Warning,
    Info,
}

impl SparqlValidator {
    pub fn validate(&self, graph: &Graph) -> Result<LayerResult> {
        let start = Instant::now();
        let mut errors = Vec::new();

        for constraint in &self.constraint_queries {
            match constraint.constraint_type {
                ConstraintType::Prohibition => {
                    // Should return false
                    let result = graph.query_ask(&constraint.query)?;
                    if result {
                        errors.push(format!(
                            "[{}] Prohibition violated: {}",
                            constraint.severity, constraint.name
                        ));
                    }
                }
                ConstraintType::Requirement => {
                    // Should return true
                    let result = graph.query_ask(&constraint.query)?;
                    if !result {
                        errors.push(format!(
                            "[{}] Requirement not met: {}",
                            constraint.severity, constraint.name
                        ));
                    }
                }
                ConstraintType::NoResults => {
                    // Should return empty result set
                    let results = graph.query(&constraint.query)?;
                    if !results.is_empty() {
                        errors.push(format!(
                            "[{}] Constraint violated ({} results): {}",
                            constraint.severity,
                            results.len(),
                            constraint.name
                        ));
                    }
                }
            }
        }

        Ok(LayerResult {
            layer_name: "sparql".to_string(),
            is_valid: errors.is_empty(),
            errors,
            duration: start.elapsed(),
        })
    }

    pub fn add_constraint(&mut self, constraint: SparqlConstraint) {
        self.constraint_queries.push(constraint);
    }

    /// Generate constraint queries from ontology
    pub async fn generate_constraints_from_ontology(
        &mut self,
        graph: &Graph,
        llm_client: &dyn LlmClient,
    ) -> Result<usize> {
        // Extract all owl:Restriction and sh:Shape definitions
        let restrictions = self.extract_restrictions(graph)?;

        let mut generated_count = 0;

        for restriction in restrictions {
            // Use LLM to convert restriction to SPARQL
            let prompt = format!(
                "Convert this OWL/SHACL restriction to a SPARQL ASK or SELECT query:\n\
                 {}\n\
                 Output only the SPARQL query.",
                restriction
            );

            let response = llm_client.complete(&prompt).await?;
            let query = self.extract_sparql_from_response(&response.content)?;

            self.add_constraint(SparqlConstraint {
                name: format!("auto_generated_{}", generated_count),
                query,
                constraint_type: ConstraintType::Prohibition,
                severity: Severity::Error,
            });

            generated_count += 1;
        }

        Ok(generated_count)
    }

    fn extract_restrictions(&self, graph: &Graph) -> Result<Vec<String>> {
        let query = r#"
            SELECT ?restriction WHERE {
                { ?restriction a owl:Restriction . }
                UNION
                { ?restriction a sh:NodeShape . }
                UNION
                { ?restriction a sh:PropertyShape . }
            }
        "#;

        let results = graph.query(query)?;
        let restrictions = results.iter()
            .map(|r| r.get("restriction").unwrap_or_default())
            .collect();

        Ok(restrictions)
    }

    fn extract_sparql_from_response(&self, response: &str) -> Result<String> {
        // Extract SPARQL from markdown code blocks
        if let Some(start) = response.find("```sparql") {
            let search_start = start + 9;
            if let Some(end_offset) = response[search_start..].find("```") {
                return Ok(response[search_start..search_start + end_offset].trim().to_string());
            }
        }

        // Fallback: look for SPARQL keywords
        if response.contains("ASK") || response.contains("SELECT") {
            return Ok(response.trim().to_string());
        }

        Err(GgenAiError::SparqlGeneration("No SPARQL query found in response".to_string()))
    }
}
```

### 3.4 Integrity Validator

```rust
pub struct IntegrityValidator;

impl IntegrityValidator {
    pub fn validate(&self, graph: &Graph) -> Result<LayerResult> {
        let start = Instant::now();
        let mut errors = Vec::new();

        // 1. Check for dangling references
        errors.extend(self.check_dangling_references(graph)?);

        // 2. Check for orphaned blank nodes
        errors.extend(self.check_orphaned_blank_nodes(graph)?);

        // 3. Check for circular dependencies
        errors.extend(self.check_circular_dependencies(graph)?);

        // 4. Check for incomplete subgraphs
        errors.extend(self.check_incomplete_subgraphs(graph)?);

        Ok(LayerResult {
            layer_name: "integrity".to_string(),
            is_valid: errors.is_empty(),
            errors,
            duration: start.elapsed(),
        })
    }

    fn check_dangling_references(&self, graph: &Graph) -> Result<Vec<String>> {
        // Find all object URIs that are referenced but not defined
        let query = r#"
            SELECT DISTINCT ?object WHERE {
                ?subject ?predicate ?object .
                FILTER(isIRI(?object))
                FILTER NOT EXISTS {
                    ?object ?p ?o .
                }
            }
        "#;

        let results = graph.query(query)?;
        let violations = results.iter()
            .map(|r| format!("Dangling reference: {}", r.get("object").unwrap_or_default()))
            .collect();

        Ok(violations)
    }

    fn check_orphaned_blank_nodes(&self, graph: &Graph) -> Result<Vec<String>> {
        // Find blank nodes that are not connected to any named node
        let query = r#"
            SELECT DISTINCT ?blank WHERE {
                ?blank ?p ?o .
                FILTER(isBlank(?blank))
                FILTER NOT EXISTS {
                    ?s ?p2 ?blank .
                    FILTER(isIRI(?s))
                }
            }
        "#;

        let results = graph.query(query)?;
        let violations = results.iter()
            .map(|r| format!("Orphaned blank node: {}", r.get("blank").unwrap_or_default()))
            .collect();

        Ok(violations)
    }

    fn check_circular_dependencies(&self, graph: &Graph) -> Result<Vec<String>> {
        // Check for cycles in rdfs:subClassOf relationships
        let classes = self.get_all_classes(graph)?;
        let mut violations = Vec::new();

        for class in &classes {
            if self.has_circular_subclass(graph, class, &mut HashSet::new())? {
                violations.push(format!("Circular subclass dependency: {}", class));
            }
        }

        Ok(violations)
    }

    fn has_circular_subclass(
        &self,
        graph: &Graph,
        class: &str,
        visited: &mut HashSet<String>,
    ) -> Result<bool> {
        if visited.contains(class) {
            return Ok(true); // Cycle detected
        }

        visited.insert(class.to_string());

        // Get all superclasses
        let query = format!(r#"
            SELECT ?super WHERE {{
                <{}> rdfs:subClassOf ?super .
            }}
        "#, class);

        let results = graph.query(&query)?;

        for result in results {
            let superclass = result.get("super")?;
            if self.has_circular_subclass(graph, &superclass, visited)? {
                return Ok(true);
            }
        }

        visited.remove(class);
        Ok(false)
    }

    fn get_all_classes(&self, graph: &Graph) -> Result<Vec<String>> {
        let query = r#"
            SELECT DISTINCT ?class WHERE {
                { ?class a owl:Class . }
                UNION
                { ?class a rdfs:Class . }
            }
        "#;

        let results = graph.query(query)?;
        Ok(results.iter()
            .map(|r| r.get("class").unwrap_or_default())
            .collect())
    }

    fn check_incomplete_subgraphs(&self, graph: &Graph) -> Result<Vec<String>> {
        // Check that all referenced entities have required properties
        // (This would be customizable based on domain requirements)
        Ok(Vec::new())
    }
}
```

## 4. Transaction Management and Rollback

### 4.1 Transaction Log

```rust
pub struct TransactionLog {
    log_path: PathBuf,
    current_transaction: Option<Transaction>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Transaction {
    pub id: String,
    pub timestamp: DateTime<Utc>,
    pub delta: GraphDelta,
    pub validation_report: ValidationReport,
    pub status: TransactionStatus,
    pub applied_at: Option<DateTime<Utc>>,
    pub rolled_back_at: Option<DateTime<Utc>>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TransactionStatus {
    Pending,
    Validated,
    Applied,
    RolledBack,
    Failed,
}

impl TransactionLog {
    pub fn begin_transaction(&mut self, delta: GraphDelta) -> Result<String> {
        let transaction = Transaction {
            id: Uuid::new_v4().to_string(),
            timestamp: Utc::now(),
            delta,
            validation_report: ValidationReport::new(),
            status: TransactionStatus::Pending,
            applied_at: None,
            rolled_back_at: None,
        };

        let id = transaction.id.clone();
        self.current_transaction = Some(transaction);

        Ok(id)
    }

    pub fn record_validation(&mut self, report: ValidationReport) -> Result<()> {
        if let Some(tx) = &mut self.current_transaction {
            tx.validation_report = report;
            tx.status = if tx.validation_report.overall_valid {
                TransactionStatus::Validated
            } else {
                TransactionStatus::Failed
            };
        }

        Ok(())
    }

    pub fn commit(&mut self) -> Result<()> {
        if let Some(mut tx) = self.current_transaction.take() {
            tx.status = TransactionStatus::Applied;
            tx.applied_at = Some(Utc::now());

            // Write to log file
            self.write_transaction(&tx)?;
        }

        Ok(())
    }

    pub fn rollback(&mut self, transaction_id: &str) -> Result<GraphDelta> {
        // Load transaction from log
        let mut tx = self.load_transaction(transaction_id)?;

        // Generate inverse delta
        let inverse_delta = GraphDelta {
            additions: tx.delta.deletions.clone(),
            deletions: tx.delta.additions.clone(),
            metadata: DeltaMetadata {
                transaction_id: format!("rollback_{}", transaction_id),
                timestamp: Utc::now(),
                source_agent: "rollback_manager".to_string(),
                reasoning: format!("Rollback of transaction {}", transaction_id),
                confidence: 1.0,
            },
        };

        tx.status = TransactionStatus::RolledBack;
        tx.rolled_back_at = Some(Utc::now());

        self.write_transaction(&tx)?;

        Ok(inverse_delta)
    }

    fn write_transaction(&self, tx: &Transaction) -> Result<()> {
        let json = serde_json::to_string_pretty(tx)?;
        let file_path = self.log_path.join(format!("{}.json", tx.id));

        std::fs::write(file_path, json)?;

        Ok(())
    }

    fn load_transaction(&self, id: &str) -> Result<Transaction> {
        let file_path = self.log_path.join(format!("{}.json", id));
        let json = std::fs::read_to_string(file_path)?;

        Ok(serde_json::from_str(&json)?)
    }

    pub fn get_transaction_history(&self, limit: usize) -> Result<Vec<Transaction>> {
        let mut transactions = Vec::new();

        for entry in std::fs::read_dir(&self.log_path)? {
            let entry = entry?;
            if entry.path().extension() == Some(OsStr::new("json")) {
                let json = std::fs::read_to_string(entry.path())?;
                let tx: Transaction = serde_json::from_str(&json)?;
                transactions.push(tx);
            }
        }

        // Sort by timestamp (newest first)
        transactions.sort_by(|a, b| b.timestamp.cmp(&a.timestamp));

        // Take only requested number
        transactions.truncate(limit);

        Ok(transactions)
    }
}
```

### 4.2 Automatic Rollback on Failure

```rust
pub struct RollbackManager {
    transaction_log: Arc<RwLock<TransactionLog>>,
    graph: Arc<RwLock<Graph>>,
}

impl RollbackManager {
    pub async fn apply_delta_with_rollback(&self, delta: GraphDelta) -> Result<()> {
        // Begin transaction
        let mut log = self.transaction_log.write().await;
        let tx_id = log.begin_transaction(delta.clone())?;
        drop(log);

        // Apply delta to graph
        let result = {
            let mut graph = self.graph.write().await;
            self.apply_delta(&mut graph, &delta)
        };

        match result {
            Ok(_) => {
                // Commit transaction
                let mut log = self.transaction_log.write().await;
                log.commit()?;
                Ok(())
            }
            Err(e) => {
                // Automatic rollback on failure
                tracing::error!("Delta application failed, rolling back: {}", e);
                self.rollback_transaction(&tx_id).await?;
                Err(e)
            }
        }
    }

    fn apply_delta(&self, graph: &mut Graph, delta: &GraphDelta) -> Result<()> {
        // Apply deletions first
        for triple in &delta.deletions {
            graph.remove_triple(triple)?;
        }

        // Then additions
        for triple in &delta.additions {
            graph.add_triple(triple.clone())?;
        }

        Ok(())
    }

    pub async fn rollback_transaction(&self, tx_id: &str) -> Result<()> {
        // Get inverse delta from log
        let mut log = self.transaction_log.write().await;
        let inverse_delta = log.rollback(tx_id)?;
        drop(log);

        // Apply inverse delta (without transaction tracking to avoid recursion)
        let mut graph = self.graph.write().await;
        self.apply_delta(&mut graph, &inverse_delta)?;

        Ok(())
    }
}
```

## 5. Performance Optimization

### 5.1 Validation Caching

```rust
pub struct ValidationCache {
    cache: Arc<RwLock<HashMap<String, CachedValidation>>>,
    ttl: Duration,
}

#[derive(Debug, Clone)]
struct CachedValidation {
    delta_hash: String,
    report: ValidationReport,
    cached_at: DateTime<Utc>,
}

impl ValidationCache {
    pub async fn get_or_validate<F>(
        &self,
        delta: &GraphDelta,
        validator: F,
    ) -> Result<ValidationReport>
    where
        F: FnOnce(&GraphDelta) -> Result<ValidationReport>,
    {
        let delta_hash = self.hash_delta(delta);

        // Check cache
        {
            let cache = self.cache.read().await;
            if let Some(cached) = cache.get(&delta_hash) {
                if Utc::now().signed_duration_since(cached.cached_at) < self.ttl {
                    return Ok(cached.report.clone());
                }
            }
        }

        // Validate
        let report = validator(delta)?;

        // Store in cache
        {
            let mut cache = self.cache.write().await;
            cache.insert(delta_hash.clone(), CachedValidation {
                delta_hash,
                report: report.clone(),
                cached_at: Utc::now(),
            });
        }

        Ok(report)
    }

    fn hash_delta(&self, delta: &GraphDelta) -> String {
        // Simple hash based on triple count and content
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        delta.additions.len().hash(&mut hasher);
        delta.deletions.len().hash(&mut hasher);

        hasher.finish().to_string()
    }
}
```

### 5.2 Parallel Validation

```rust
impl MultiLayerValidator {
    pub async fn validate_delta_parallel(
        &self,
        graph: &Graph,
        delta: &GraphDelta,
    ) -> Result<ValidationReport> {
        let mut report = ValidationReport::new();

        // Create test graph once
        let test_graph = Arc::new(self.create_test_graph(graph, delta)?);

        // Run all validations in parallel
        let (syntax_result, schema_result, sparql_result, integrity_result, impact_result) = tokio::join!(
            tokio::task::spawn_blocking({
                let delta = delta.clone();
                let validator = self.syntax_validator.clone();
                move || validator.validate(&delta)
            }),
            tokio::task::spawn_blocking({
                let graph = test_graph.clone();
                let validator = self.schema_validator.clone();
                move || validator.validate(&*graph)
            }),
            tokio::task::spawn_blocking({
                let graph = test_graph.clone();
                let validator = self.sparql_validator.clone();
                move || validator.validate(&*graph)
            }),
            tokio::task::spawn_blocking({
                let graph = test_graph.clone();
                let validator = self.integrity_validator.clone();
                move || validator.validate(&*graph)
            }),
            tokio::task::spawn_blocking({
                let graph = graph.clone();
                let delta = delta.clone();
                let analyzer = self.impact_analyzer.clone();
                move || analyzer.analyze(&graph, &delta)
            })
        );

        // Collect results
        report.add_layer_result("syntax", syntax_result??);
        report.add_layer_result("schema", schema_result??);
        report.add_layer_result("sparql", sparql_result??);
        report.add_layer_result("integrity", integrity_result??);
        report.add_layer_result("impact", impact_result??);

        Ok(report)
    }
}
```

## 6. Quality Metrics

- **Validation Latency**: Target < 2 seconds for 99th percentile
- **False Positive Rate**: Target < 1% (valid changes rejected)
- **False Negative Rate**: Target < 0.01% (invalid changes accepted)
- **Rollback Success Rate**: Target 100% (all rollbacks succeed)
- **Cache Hit Rate**: Target > 50% for repeated validations

---

**Strategy Version**: 1.0.0
**Status**: Design Complete
**Last Updated**: 2025-10-10
