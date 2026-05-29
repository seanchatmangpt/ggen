# Analysis Report: OCEL v2 Self-Audit Log & Coverage Matrix

This analysis report provides a read-only investigation and design specification for implementing the OCEL v2 self-audit log generator, the coverage matrix, integration tests, and verification scripts required for the Vision 2030 GALL checkpoint promotion.

---

## 1. Executive Summary

We have established the baseline status of the repository, confirming that all existing tests compile and pass. The `crates/ggen-graph` package provides a robust substrate for deterministic RDF graph operations, SPARQL queries, knowledge hooks, and cryptographic transition receipts. 

To satisfy the **Vision 2030 GALL Checkpoint** requirements, we must implement:
1. **OCEL v2 Self-Audit Log Generator** (`src/ocel/self_audit.rs` and `src/ocel/gall_projection.rs`): Generates a qualified process log capturing the team's implementation, testing, scanning, and promotion events, outputting `audit/vision2030.self_audit.ocel.json`.
2. **Coverage Mapping Matrix** (`src/ocel/coverage.rs`): Maps requirements to files, tests, commands, and evidence, outputting `audit/vision2030.coverage.json`.
3. **Integration Tests & Validation Scripts**: Integration tests verifying self-audit and coverage invariants, and shell scripts (`scripts/gall/emit_ocel_self_audit.sh` and `scripts/gall/verify_ocel_self_audit.sh`) enforcing the 5 Completeness Rules.
4. **Derived Proof Report**: Updating the summary and final proof documentation.

We propose implementing these generators natively as standard Rust source files within the `ocel` module, and exposing them via binary targets (`src/bin/emit_audit.rs` and `src/bin/verify_audit.rs`) to ensure robust, mock-free execution.

---

## 2. Current Architecture & Projection Design

The `crates/ggen-graph/src/ocel/` module currently manages two core data representations:
1. **OCEL Log (`ocel_types.rs`)**: Captures events (`OcelEvent`), objects (`OcelObject`), and object references with qualifiers (`OcelObjectRef`).
2. **W3C PROV-DM Document (`prov_types.rs`)**: Captures entities, activities, agents, and their respective relationships.

The projection layer (`projection.rs` via `EvidenceProjector`) implements:
- `project_ocel`: Projects an `OcelLog` into the `DeterministicGraph` by generating RDF triples/quads for objects and events under the `http://ggen.dev/ocel/` namespace. Qualifiers are projected as properties prefixed with `http://www.ocel-standard.org/ns#qualifier_`.
- `extract_ocel`: Uses SPARQL queries to reconstruct a full `OcelLog` from the RDF store.
- `project_prov` & `extract_prov`: Manages the equivalent translation for PROV-DM concepts.

To support GALL promotion, our new modules must leverage this existing `EvidenceProjector` to store and query self-audit data directly within the deterministic RDF store, validating the entire execution history using SPARQL query constraints.

---

## 3. Detailed Component Designs

### A. Self-Audit Log (`self_audit.rs` & `gall_projection.rs`)
The self-audit log records the actual events and objects representing the software engineering and validation process.

#### Proposed Location:
- `crates/ggen-graph/src/ocel/self_audit.rs`
- `crates/ggen-graph/src/ocel/gall_projection.rs`

#### Data Specification:
- **Object Types**:
  - `RustCrate`, `PRDRequirement`, `ARDRequirement`, `GALLCheckpoint`, `PublicOntology`, `OntologyTerm`, `SourceFile`, `TestFile`, `ExampleFile`, `FixtureFile`, `ScriptFile`, `Command`, `CommandRun`, `EvidenceArtifact`, `GraphReceipt`, `CoverageMatrix`, `PromotionDecision`, `UnsupportedCapability`.
- **Event Types**:
  - `RequirementDeclared`, `OntologyMapped`, `FileEmitted`, `ImplementationChanged`, `FixtureCreated`, `CommandExecuted`, `TestPassed`, `TestFailed`, `ForbiddenSurfaceScanned`, `AntiFakeScanned`, `ReceiptEmitted`, `ReplayVerified`, `CoverageEvaluated`, `CheckpointEvaluated`, `CheckpointPromoted`, `CheckpointRefused`, `UnsupportedCapabilityDeclared`.
- **Qualifiers / Predicates**:
  - `--checks-->`, `--produces-->`, `--verifies-->`, `--satisfied_by-->`, `--decides-->`.

#### Implementation Sketch (`self_audit.rs`):
```rust
use std::collections::HashMap;
use chrono::Utc;
use crate::ocel::{OcelLog, OcelObject, OcelEvent, OcelObjectRef};

/// Generates the deterministic OCEL v2 self-audit log.
pub fn generate_self_audit_log() -> OcelLog {
    let mut log = OcelLog::new();

    // 1. Declare Objects
    // Requirements (R1-R6, etc.)
    log.objects.push(OcelObject {
        id: "req-r1-one-crate".to_string(),
        r#type: "PRDRequirement".to_string(),
        attributes: [("description".to_string(), "One-Crate Package Boundary".to_string())].into(),
    });
    // Add other requirements, source files, commands, etc.
    // ...

    // 2. Declare Events (RequirementDeclared, FileEmitted, etc.)
    log.events.push(OcelEvent {
        id: "event-r1-declared".to_string(),
        activity: "RequirementDeclared".to_string(),
        timestamp: Utc::now(),
        objects: vec![OcelObjectRef {
            id: "req-r1-one-crate".to_string(),
            r#type: "PRDRequirement".to_string(),
            qualifier: None,
        }],
        attributes: HashMap::new(),
    });
    // Add other events capturing the execution history
    // ...

    log
}
```

#### Implementation Sketch (`gall_projection.rs`):
`gall_projection.rs` wraps the `EvidenceProjector` to project the generated self-audit log and run SPARQL verification queries mapping custom GALL qualifiers into standard predicates.
```rust
use crate::{DeterministicGraph, GraphError};
use crate::ocel::OcelLog;
use crate::ocel::projection::EvidenceProjector;

pub struct GallProjector;

impl GallProjector {
    /// Project self-audit log into the graph.
    pub fn project_audit_log(graph: &DeterministicGraph, log: &OcelLog) -> Result<(), GraphError> {
        EvidenceProjector::project_ocel(graph, log)
    }

    /// Run a custom verification query to validate promotion criteria.
    pub fn verify_promotion_rules(graph: &DeterministicGraph) -> Result<bool, GraphError> {
        let query = r#"
            ASK WHERE {
                ?dec a <http://www.ocel-standard.org/ns#Object> ;
                     <http://www.ocel-standard.org/ns#objectType> "PromotionDecision" .
                # Add validation logic to ensure a positive decision exists with backing evidence
            }
        "#;
        let results = graph.query(query)?;
        if let oxigraph::sparql::QueryResults::Boolean(val) = results {
            Ok(val)
        } else {
            Ok(false)
        }
    }
}
```

---

### B. Coverage Matrix (`coverage.rs`)
The coverage matrix defines how each PRD/ARD requirement is satisfied and verified, mapping requirements to specific source files, test cases, execution commands, and cryptographic evidence.

#### Proposed Location:
- `crates/ggen-graph/src/ocel/coverage.rs`

#### Structure:
```rust
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RequirementCoverage {
    pub requirement_id: String,
    pub requirement_type: String, // "PRDRequirement" or "ARDRequirement"
    pub satisfied_by_files: Vec<String>,
    pub verified_by_tests: Vec<String>,
    pub evidence_commands: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageMatrix {
    pub requirements: Vec<RequirementCoverage>,
}
```

---

### C. Execution & Verification Binaries

To ensure absolute completeness and avoid mock-based testing, we propose adding two lightweight CLI binaries directly in the library workspace. Under Cargo, files located in `src/bin/*.rs` are automatically treated as executable targets.

#### 1. `src/bin/emit_audit.rs`:
Generates the self-audit log and coverage matrix, then writes them to their respective paths (`audit/vision2030.self_audit.ocel.json` and `audit/vision2030.coverage.json`).
```rust
use std::fs::File;
use std::io::Write;
use ggen_graph::ocel::self_audit::generate_self_audit_log;
use ggen_graph::ocel::coverage::generate_coverage_matrix;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Generate and write self-audit log
    let audit_log = generate_self_audit_log();
    let audit_json = serde_json::to_string_pretty(&audit_log)?;
    let mut file = File::create("audit/vision2030.self_audit.ocel.json")?;
    file.write_all(audit_json.as_bytes())?;

    // Generate and write coverage matrix
    let coverage_matrix = generate_coverage_matrix();
    let coverage_json = serde_json::to_string_pretty(&coverage_matrix)?;
    let mut file = File::create("audit/vision2030.coverage.json")?;
    file.write_all(coverage_json.as_bytes())?;

    println!("Audit artifacts emitted successfully.");
    Ok(())
}
```

#### 2. `src/bin/verify_audit.rs`:
Performs structural analysis of the generated JSON files to enforce the **5 Completeness Rules**:
1. **Requirements have evidence**: Checks that every requirement ID is associated with a file, test, or command.
2. **Checkpoints have Command evidence**: Checks that evaluating checkpoints is linked to execution events of validation commands.
3. **Prior evaluations exist**: Verifies that any promotion event is preceded chronologically by evaluation events.
4. **Anti-fake is audited**: Verifies that `AntiFakeScanned` and `ForbiddenSurfaceScanned` events are present and point to verified runs.
5. **Unsupported capabilities are linked**: Validates that all declared unsupported capabilities are mapped directly back to their originating constraints.

```rust
fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load JSON logs and execute completeness rules
    // ...
    println!("OCEL self-audit log complies with all 5 Completeness Rules.");
    Ok(())
}
```

---

## 4. Integration Verification Plan

### Integration Tests
We will add two files to `crates/ggen-graph/tests/`:
1. `tests/ocel_self_audit.rs`:
   - Projects the self-audit log into a `DeterministicGraph` using `GallProjector`.
   - Runs SPARQL queries to verify that the projected event sequence matches expected variants.
   - Evaluates overall graph state hashing and transition determinism.
2. `tests/vision2030_coverage.rs`:
   - Checks the coverage matrix schema correctness.
   - Asserts that 100% of the PRD/ARD requirements listed in `ORIGINAL_REQUEST.md` map to at least one valid source and test file.

### Shell Verification Scripts
1. `scripts/gall/emit_ocel_self_audit.sh`:
   ```bash
   #!/usr/bin/env bash
   set -euo pipefail
   cargo run -p ggen-graph --bin emit_audit
   ```
2. `scripts/gall/verify_ocel_self_audit.sh`:
   ```bash
   #!/usr/bin/env bash
   set -euo pipefail
   cargo run -p ggen-graph --bin verify_audit
   ```

---

## 5. Potential Pitfalls and Constraints

1. **Deterministic Hashing**: Because we serialize the output to JSON, any hash calculation over the audit files must guarantee field sorting and key stabilization to satisfy replay verifiers.
2. **Completeness Enforcement**: Under the `AGENTS.md` and `GEMINI.md` anti-cheating mandates, placeholders like `"TODO"`, `""`, or `"hash_placeholder"` will fail both the Rust verification checks and the AST scanners. Real BLAKE3 hashes of files and actual command stdout logs must be compiled into the generated log structure.
3. **Network Isolation**: All implementation tools and validation scripts must run locally without downloading resources or relying on remote registries.
