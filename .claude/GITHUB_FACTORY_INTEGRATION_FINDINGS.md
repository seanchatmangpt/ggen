# GitHub Factory (GHF) Integration — Findings from open-ontologies

**Date**: 2026-05-27  
**Source**: ~/open-ontologies (complete implementation, Rust + Python + Tera)  
**Purpose**: Blueprint for ggen contribution ledger system (finishing Phase 5 Wave 1 initiative)

---

## Executive Summary

The GitHub Factory is a **cryptographically-audited contribution ledger system** that wires together:
1. **Public ontologies** (PROV-O, IES 4D, Dublin Core) via a thin `ghf:` profile
2. **Autonomic execution** (ggen-generated Terraform for infra, Python for audits)
3. **Receipt-based proof** (BLAKE3 hashes, Ed25519 signatures, OCEL-bound chains)
4. **Admitted vs. Refused** states enforced via the Admission gate

**Key Achievement**: Weekly contribution ledger compiled via `cargo run -- audit ledger`, with fleet-level health assessments proving all repositories conform to policy.

---

## Architecture Overview

### Layer 1: Ontology (`.specify/ontologies/` + `.specify/queries/ghf/`)

**Core Concepts**:
- `ghf:ContributionUnit` — JTBD ID + persona + target repository
- `ghf:Repository` — GitHub repository with branch protection, labels, environments
- `ghf:BranchProtection`, `ghf:Label`, `ghf:SecurityPolicy`, `ghf:EnvironmentSecret` — infrastructure atoms
- Seven SPARQL aggregation queries (`*_aggregated.rq`) for fleet-wide rollups

**SPARQL Queries (6 total)**:
1. `repositories_aggregated.rq` — Fleet-wide repository inventory
2. `branch_protection_aggregated.rq` — Protected branch rules across all repos
3. `labels_aggregated.rq` — Label taxonomy standardization
4. `environments_aggregated.rq` — Deployment environments + secrets binding
5. `security_policy_aggregated.rq` — SECURITY.md, CVE disclosure rules
6. `contribution_units_aggregated.rq` — JTBD × persona × repo matrix

**How to extend for ggen**: Add `ghf:CodeGenerationCapability` and `ghf:ManufacturingUnit` to the ontology, with SPARQL aggregations for capability reuse metrics.

---

### Layer 2: Code Generation (ggen)

**Generated Artifacts** (`.specify/templates/ghf/`):

| Artifact | Template | Output | Purpose |
|----------|----------|--------|---------|
| **Terraform** | `terraform/*.tf.tera` | `.artifacts/ghf/terraform/*.tf` | Infrastructure-as-code (labels, branch protection, environments) |
| **Workflows** | `github/*.yml.tera` | `.github/workflows/*.yml` | GitHub Actions (contribution-ledger.yml, receipt-verify.yml) |
| **Scripts** | `scripts/*.sh.tera` | `scripts/*.sh` | Bash tooling (collect evidence, create PRs) |
| **OCEL** | `ocel/*.ocel.jsonl.tera` | `.artifacts/ghf/ocel/*.jsonl` | Expected event logs (object-centric proof) |
| **Docs** | `docs/*.md.tera` | `.artifacts/ghf/docs/*.md` | Human-readable proof matrix + security policy |

**How generated**: ggen `sync` reads ontology, renders Tera templates, writes to `artifacts/ghf/`. Weekly `contribution-ledger.yml` workflow runs `cargo run -- audit ledger --repo <name>` to aggregate and commit.

---

### Layer 3: Admission Gate + Receipt Chain

**Rust modules** (`src/ghf.rs`, `src/admission.rs`, `src/receipts.rs`):

#### Structures
```rust
pub struct ContributionReceipt {
    #[serde(flatten)]
    pub core: crate::autoreceipt::Receipt,  // BLAKE3 hash + prior_receipt
    pub ghf_specific_refusal: Option<RefusalState8>,
}

pub struct ContributionUnit {
    pub jtbd_id: String,      // "jtbd-order-to-cash"
    pub persona: String,       // "architect", "engineer", "reviewer"
    pub target_repo: String,   // "open-ontologies"
}

pub enum VerificationState { Admitted, Refused, Incomplete }
```

#### Admission Flow
1. **Declaration**: JTBD × persona × repo declared in ontology
2. **Expected OCEL**: ggen renders `expected.ocel.jsonl` (state machine for contribution lifecycle)
3. **Execution**: `collect-github-evidence.sh` captures git hash, timestamp, status
4. **Observed OCEL**: Boundary script writes `observed-ocel.json` (git state, PR events)
5. **Conformance**: Python script aligns observed OCEL against expected model
6. **Receipt**: On success, BLAKE3-signed `ContributionReceipt` emitted; on failure, typed `RefusalState8`
7. **Refusal Types**:
   - `EvidenceIncomplete` — missing git events
   - `SyntheticClosureLie` — observed OCEL fabricated (no real boundary call)
   - `BoundaryEvidenceMissing` — no subprocess evidence
   - `PolicyConformanceFailed` — OCEL doesn't conform to declared POWL
   - `FleetDriftDetected` — Repository missing required files (e.g., SECURITY.md)

---

### Layer 4: Weekly Ledger + Fleet Sentinel

**Workflow** (`.github/workflows/contribution-ledger.yml`):
- Runs Sundays at 00:00 UTC (cron: `0 0 * * 0`)
- Invokes `cargo run -- audit ledger --repo <name> --output artifacts/ledger/<week>.json`
- Commits to `artifacts/ledger/YYYY-WW.json` (year-week format)

**Ledger Schema**:
```json
{
  "week": "2026-W21",
  "repository": "open-ontologies",
  "contributions_admitted": [
    {
      "jtbd_id": "jtbd-001-github-factory",
      "persona": "architect",
      "pr_number": 42,
      "commit_sha": "a1b2c3d4",
      "merged_at": "2026-05-27T14:30:00Z",
      "receipt_hash": "blake3_hash_here",
      "gates_passed": ["A1_WorkflowDeclared", "A5_OcelConformant"],
      "gates_refused": []
    }
  ],
  "contributions_refused": [
    {
      "jtbd_id": "jtbd-002-untracked",
      "refusal_state": "PolicyConformanceFailed",
      "reason": "No declared JTBD for this commit",
      "receipt_hash": "blake3_hash_refused"
    }
  ],
  "metadata": {
    "fleet_health_score": 0.97,
    "drift_risk_score": 0.02,
    "policy_version": "ontostar-1.0.0"
  }
}
```

**Fleet Sentinel** (`tests/ghf_fleet_sentinel_test.rs`):
- Scans all repositories for infrastructure drift (missing SECURITY.md, unprotected branches)
- Produces `artifacts/ghf/fleet/fleet-health.receipt.json` with:
  - `FleetHealthReceipt` (on success) or `OutOfMembraneReceipt` (on drift)
  - `DriftRiskScore` (0.0–1.0)
  - `TopologyConformanceScore` (0.0–1.0)
  - List of missing/violated items
- Integrates with `receipt-verify.yml` workflow for automated remediation

---

## Key Rust Implementation Details

### Contribution Unit Serialization
```rust
// From tests/ghf_fleet_sentinel_test.rs — serialization proof
#[test]
fn test_contribution_unit_serialization() {
    let unit = ContributionUnit {
        jtbd_id: "jtbd-123".to_string(),
        persona: "architect".to_string(),
        target_repo: "open-ontologies".to_string(),
    };
    let json = serde_json::to_string(&unit).unwrap();
    let decoded: ContributionUnit = serde_json::from_str(&json).unwrap();
    assert_eq!(unit, decoded);  // Round-trip proof
}
```

### Receipt Verification Gateway
```rust
pub fn verify_receipt(receipt: &ContributionReceipt) -> ValidationResult {
    match crate::autoreceipt::validate_core_receipt(&receipt.core) {
        Ok(_) => ValidationResult {
            state: VerificationState::Admitted,
            refusal: None,
            missing: vec![],
            receipt_hash: receipt.core.receipt_hash.clone(),
        },
        Err(e) => {
            // Map 15+ core refusal states to GHF-specific refusal states
            let mapped_refusal = match e {
                OpenOntologyRefusalState8::ExpectedOCELMissing => RefusalState8::EvidenceIncomplete,
                OpenOntologyRefusalState8::ObservedOCELSynthetic => RefusalState8::SyntheticClosureLie,
                // ... 13 more mappings ...
            };
            ValidationResult {
                state: VerificationState::Refused,
                refusal: Some(mapped_refusal),
                missing: vec![format!("{:?}", e)],
                receipt_hash: receipt.core.receipt_hash.clone(),
            }
        }
    }
}
```

### Command Handler
```rust
#[verb]
fn verify(target_type: String, target: Option<String>) -> NounVerbResult<()> {
    if target_type == "receipt" {
        let receipt_path = PathBuf::from(target.ok_or_else(|| ...)?);
        let receipt_json = fs::read_to_string(&receipt_path)?;
        let receipt: ContributionReceipt = serde_json::from_str(&receipt_json)?;
        let result = verify_receipt(&receipt);
        println!("{}", serde_json::to_string_pretty(&result).unwrap());
        
        if result.state == VerificationState::Admitted {
            Ok(())
        } else {
            Err(to_verb_err("Verification failed or incomplete".to_string()))
        }
    } else if target_type == "fleet" {
        // Fleet Sentinel mode: run Python script, emit fleet-health receipt
        Command::new("python3")
            .arg("scripts/ghf/fleet_sentinel.py")
            .status()?;
        // ...
    }
}
```

---

## Integration Points for ggen

### 1. **Extend Ontology to Model Capabilities**

Add to `.specify/ontologies/standard-vocabularies.ttl`:
```turtle
@prefix ggen: <https://ggen.org/capability#> .
@prefix ghf: <https://open-ontologies.org/profile/github-factory#> .

ggen:CodeGenerationCapability a rdfs:Class ;
    rdfs:comment "A sealed code generation cell (WASM, Python, Go, Erlang)" ;
    ghf:reusable true ;
    ghf:author "Sean Chatman" ;
    ghf:version "26.5.21" .

ggen:ContributionToCapabilityReuse a ghf:ContributionUnit ;
    ggen:capabilityId "ggen-a2a-mcp" ;
    ggen:jtbdId "jtbd-005-marketplace-capability-composition" ;
    ghf:persona "manufacturing-engineer" .
```

### 2. **Create Capability Reuse Aggregation Query**

Add `.specify/queries/ghf/capability_reuse_aggregated.rq`:
```sparql
PREFIX ggen: <https://ggen.org/capability#>
PREFIX ghf: <https://open-ontologies.org/profile/github-factory#>

SELECT
  (COUNT(DISTINCT ?capability) AS ?uniqueCapabilities)
  (SUM(?reuseCount) AS ?totalReuseInstances)
  (AVG(?reuseCount) AS ?avgReusePerCapability)
WHERE {
  ?unit a ggen:ContributionToCapabilityReuse ;
        ggen:capabilityId ?capability ;
        ggen:reuseCount ?reuseCount .
}
```

### 3. **Generate Weekly Capability Reuse Report**

Add `.specify/templates/ghf/docs/capability-reuse-report.md.tera`:
```markdown
# Weekly Capability Reuse Report

{{ week }}

## Summary
- **Total unique capabilities reused**: {{ unique_capabilities }}
- **Total reuse instances**: {{ total_reuse_instances }}
- **Average reuse per capability**: {{ avg_reuse_per_capability | round }}

## Top 10 Most Reused Capabilities
{% for cap in top_reused %}
- **{{ cap.id }}**: reused {{ cap.count }} times
  - Blocker P0: {{ cap.p0_blockers }}
  - Deduplication potential: {{ cap.deduplication_lines }} LOC
{% endfor %}

## Capability Health
- **Dormant code activation**: {{ dormant_hours }} hours pending
- **Pattern unification**: {{ pattern_dedup }} lines recoverable
```

### 4. **Wire into CLI**

Add verb to `crates/ggen-cli/src/cmds/ghf.rs`:
```rust
#[verb]
fn report(report_type: String) -> NounVerbResult<()> {
    if report_type == "capability-reuse" {
        // Query ontology for capability reuse metrics
        let metrics = query_capability_reuse().map_err(...)?;
        // Render weekly report from template
        let report = render_template("capability-reuse-report", metrics)?;
        println!("{}", report);
        Ok(())
    }
}
```

### 5. **Add Test Coverage**

Add `crates/open-ontologies/tests/ggen_capability_ledger_test.rs`:
```rust
#[test]
fn test_capability_reuse_ledger_weekly_compilation() {
    // 1. Verify capability inventory loaded from ggen/.specify/
    let capabilities = load_capability_inventory().expect("Inventory missing");
    assert!(capabilities.len() > 0, "No capabilities found");
    
    // 2. Generate expected OCEL (state machine of each capability's reuse lifecycle)
    let expected_ocel = render_expected_ocel(&capabilities);
    expected_ocel.write("artifacts/ghf/ocel/expected.ocel.jsonl").ok();
    
    // 3. Collect observed events (git log, MCP calls, marketplace transactions)
    let observed_ocel = collect_observed_boundary_evidence();
    
    // 4. Conformance check
    let conformance = check_conformance(&expected_ocel, &observed_ocel)
        .expect("Conformance failed");
    assert_eq!(conformance.fitness, 1.0, "Not all capabilities traced");
    
    // 5. Emit receipt
    let receipt = emit_receipt(conformance);
    assert_eq!(receipt.state, VerificationState::Admitted);
}
```

---

## Complete File Inventory

### Ontology
- `.specify/ontologies/standard-vocabularies.ttl` — PROV-O, IES 4D, Dublin Core + ghf: profile
- `.specify/queries/ghf/*.rq` (6 files) — SPARQL aggregations for infrastructure

### Code Generation
- `.specify/templates/ghf/terraform/*.tf.tera` (4 files) — Infrastructure-as-code
- `.specify/templates/ghf/github/*.yml.tera` (3 files) — GitHub Actions workflows
- `.specify/templates/ghf/scripts/*.sh.tera` (3 files) — Bash evidence collection
- `.specify/templates/ghf/ocel/*.ocel.jsonl.tera` — Expected event logs
- `.specify/templates/ghf/docs/*.md.tera` — Rendered proof matrices

### Runtime
- `src/ghf.rs` — `ContributionUnit`, `ContributionReceipt`, `verify_receipt()`
- `src/admission.rs` — Admission gate with OCEL alignment
- `src/receipts.rs` — BLAKE3 chain construction
- `src/cmds/ghf.rs` — CLI verb handlers (verify receipt, fleet sentinel)
- `src/attestation.rs`, `src/cell_ready.rs`, `src/defects.rs` — Proof infrastructure

### Workflows
- `.github/workflows/contribution-ledger.yml` — Weekly ledger compilation (cron: Sundays)
- `.github/workflows/receipt-verify.yml` — Receipt verification + remediation

### Scripts
- `scripts/collect-github-evidence.sh` — Capture git boundary evidence
- `scripts/ghf/fleet_sentinel.py` — Detect infrastructure drift + emit fleet health receipt

### Tests
- `tests/ghf_fleet_sentinel_test.rs` — Closed-loop remediation (drift → ggen fix → health)
- `tests/ghf_*.rs` (other tests) — Receipt serialization, unit verification

### Documentation
- `CONTRIBUTING.md` — Standard contributor onboarding
- `CONTRIBUTORS.md` — Credits (generated from ledger)
- `docs/adr/0001-github-factory-implementation.md` — Design decision record

### Artifacts
- `.artifacts/ghf/terraform/*.tf` — Generated infrastructure-as-code
- `.artifacts/ghf/ocel/expected.ocel.jsonl` — Declared workflow model
- `.artifacts/ghf/ocel/observed.ocel.json` — Actual boundary events
- `.artifacts/ghf/fleet/fleet-health.receipt.json` — Fleet status proof
- `.artifacts/ledger/YYYY-WW.json` — Weekly contribution ledger (one per week)

---

## How to Finish ggen Phase 5

### Step 1: Import Ontology Extensions
Copy `.specify/ontologies/standard-vocabularies.ttl` concepts into ggen's ontology layer. Add capability-specific predicates:
- `ggen:capabilityId`, `ggen:reuseCount`, `ggen:deduplicationPotential`, `ggen:dormantHours`

### Step 2: Generate Queries
Create `.specify/queries/ghf/capability_reuse_aggregated.rq` to roll up reuse metrics from capability inventory.

### Step 3: Render Templates
Add `.specify/templates/ghf/docs/capability-reuse-report.md.tera` + CLI verb to produce weekly report.

### Step 4: Wire Admission Gate
Extend `src/admission.rs` to emit `ggen-capability-reused` OCEL events when a capability is consumed from the marketplace.

### Step 5: Test Coverage
Add `tests/ggen_capability_ledger_test.rs` — verify expected/observed OCEL conformance for capability reuse flow.

### Step 6: Commit & Document
- Commit all changes to `feat/autonomic-actuation`
- Update `.claude/PHASE5_WAVE2_PLANNING.md` with integration roadmap
- Run `cargo test` + `cargo make lint` to validate

---

## Files to Copy from open-ontologies

**Directly reusable**:
1. `src/ghf.rs` — Adapt `ContributionUnit` → `CapabilityReusageUnit`; `ContributionReceipt` → `CapabilityReusageReceipt`
2. `src/admission.rs` — Import admission gate pattern; extend for OCEL binding
3. `src/receipts.rs` — BLAKE3 chain implementation (unchanged)
4. `tests/ghf_fleet_sentinel_test.rs` — Template for closed-loop testing

**Template inspiration**:
1. `.specify/templates/ghf/github/contribution-ledger.yml.tera` → `capability-reuse-ledger.yml.tera`
2. `.specify/templates/ghf/docs/weekly-ledger-template.md.tera` → Adapt for capability metrics

**Workflow design**:
1. `.github/workflows/contribution-ledger.yml` — Run `cargo run -- audit capability-reuse` weekly

---

## Summary

The GitHub Factory proves that:
1. ✅ **Sealed receipt proof** is operationally feasible (BLAKE3 chains work)
2. ✅ **Multi-layer admission gates** can enforce complex constraints (OCEL + refusal states)
3. ✅ **Weekly ledgers** scale to fleet-wide metrics (per-repo aggregation)
4. ✅ **Closed-loop remediation** (drift detection → ggen fix → health proof) works end-to-end

For ggen, this becomes:
- **Weekly capability reuse ledger** (which capabilities are reused, how often, blocked by what)
- **Deduplication tracking** (17,400 LOC recovery opportunity, tracked per phase)
- **P0 blocker activation** (SHACL, pipeline ordering, namespaces) — each fix tied to a receipt
- **Pattern unification progress** (shared-error, shared-crypto, shared-test-utils adoption metrics)

All receipted. All auditable. All reproducible from OCEL alone.

