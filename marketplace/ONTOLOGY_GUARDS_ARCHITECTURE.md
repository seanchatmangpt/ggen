# Marketplace Ontology and Guards Architecture

**Status**: Phase 1 Foundation Complete
**Version**: 1.0.0
**Last Updated**: 2025-11-16

---

## Overview

The ggen marketplace is being transformed from a static JSON registry into a **formal, ontology-backed, guard-enforced substrate** with audit-grade validation and immutable receipts.

### The Transformation: O_market → μ_market → A

- **O_market** (Ontology): The single source of truth in RDF/TTL
  - Location: `marketplace/ontology.ttl`
  - Defines all package metadata, assets, quality metrics, guards, bundles
  - Immutable conceptual layer

- **μ_market** (Generator): ggen templates that project O_market into artifacts
  - Generates: `index.json`, `PACKAGES.md`, validation reports
  - All outputs are generated, never hand-edited

- **A** (Artifacts):
  - Packages (metadata, versions, dependencies)
  - Flags (production_ready, chatman_certified)
  - Receipts (immutable validation audit trail)

---

## Foundation: What's Been Built

### 1. Marketplace Ontology (`marketplace/ontology.ttl`)

A formal RDF/RDFS/OWL ontology modeling:

#### Core Classes
- **Package**: Deployable marketplace item with metadata, scoring, flags
- **Asset**: Deliverables (templates, src, RDF, SPARQL, tests, docs, examples)
- **QualityMetric**: Measured dimensions (validation score, critical score, bonus score)
- **Guard**: Validation rules with weight, severity, SLO
- **GuardResult**: Outcome of applying one guard
- **ValidationReceipt**: Immutable validation record (JSON file)
- **Bundle**: Curated collection of packages (vertical stacks)
- **Dependency**: Package/runtime/version constraints

#### Key Properties
- `market:id`, `market:version`, `market:description`, `market:license`, `market:author`
- `market:productionReady`, `market:chatmanCertified`
- `market:hasTemplates`, `market:hasSrc`, `market:hasRdf`, `market:hasSparql`, `market:hasTests`
- `market:dependsOnPackage`, `market:dependsOnRuntime`, `market:compatibleWithGgenVersion`
- `market:weight`, `market:severity`, `market:slo` (for guards)

#### Guard Types (in Ontology)
- `GuardMetadata` (weight: 10, critical): Metadata completeness
- `GuardLicense` (weight: 8, critical): License file presence
- `GuardReadme` (weight: 7, critical): README quality
- `GuardSrc` (weight: 9, critical): Source code presence
- `GuardTemplates` (weight: 6, bonus): Reusable templates
- `GuardRdf` (weight: 5, bonus): RDF/ontology presence
- `GuardSparql` (weight: 4, bonus): SPARQL queries
- `GuardTests` (weight: 8, critical): Test suite
- `GuardDocs` (weight: 5, bonus): Documentation
- `GuardChicagoCompliance` (weight: 15, critical): Chatman Equation adherence

### 2. Guard System (`crates/ggen-domain/src/marketplace/guards.rs`)

Formal validation infrastructure in Rust:

#### Core Types

```rust
pub trait Guard: Send + Sync {
    fn id(&self) -> &str;
    fn name(&self) -> &str;
    fn description(&self) -> &str;
    fn weight(&self) -> u32;  // 1-100
    fn severity(&self) -> Severity;  // Critical | Bonus
    fn slo_ms(&self) -> u32;  // Service Level Objective
    fn execute(&self, package_path: &Path) -> GuardResult<GuardCheckResult>;
}
```

#### GuardCheckResult
Each guard produces a check result with:
- `guard_name`, `guard_type`, `passed`, `message`
- `weight`, `severity`
- Optional `details` (JSON)

#### ValidationReceipt
Immutable audit record stored as JSON in `marketplace/receipts/<package_id>/<version>.json`:

```json
{
  "package_id": "data-pipeline-cli",
  "version": "2.1.0",
  "validated_at": "2025-11-16T12:34:56Z",
  "ggen_version": "2.7.1",
  "guard_results": [
    {
      "guard_name": "Metadata Guard",
      "guard_type": "metadata",
      "passed": true,
      "message": "Metadata complete: id, version, description found",
      "weight": 10,
      "severity": "Critical"
    },
    { ... more guard results ... }
  ],
  "overall_score": 87.5,
  "production_ready": false,
  "critical_passed": 5,
  "critical_total": 6,
  "bonus_passed": 3,
  "bonus_total": 4,
  "checksum": "sha256:3d6ff6ca..."
}
```

#### Standard Guard Factories
Implemented in `guards.rs::factories`:
- `GuardMetadata`: Validates `package.toml` and required fields
- `GuardLicense`: Checks for LICENSE files
- `GuardReadme`: Validates README.md presence and length (≥500 chars)
- `GuardTests`: Checks for test directory or test files

---

## Next Phase: Executing the Vision

### Phase 2 Tasks (Parallel Work Tracks)

#### Track 1: Generate Artifacts from Ontology (μ_market)
- **What**: Create ggen templates to project O_market into:
  - `index.json` (JSON registry for CLI)
  - `PACKAGES.md` (Markdown documentation)
  - Validation reports and dashboards

- **Why**: Eliminates hand-edited registries; everything derives from ontology

- **Implementation**:
  1. Create ggen templates in `marketplace/templates/` directory
  2. Template for JSON projection (package list → `index.json`)
  3. Template for Markdown (package list → `PACKAGES.md` with formatted tables)
  4. Wire to `make marketplace-generate` task

- **Success Criteria**:
  - All 55 packages have current entries generated from ontology
  - No diffs when re-generating
  - CI ensures projections stay current

#### Track 2: Emit Validation Receipts (Wire → A)
- **What**: Modify marketplace validation pipeline to emit receipts

- **Implementation**:
  1. Extend `validate.rs` to use guard system and emit receipts
  2. Wire receipts into `marketplace-validate` task
  3. Store receipts in `marketplace/receipts/<pkg_id>/<version>.json`
  4. Update `marketplace-validate-update` to read latest receipt and set `production_ready` flag

- **Success Criteria**:
  - Running `make marketplace-validate` emits receipts for all packages
  - Each receipt is immutable and checksummed
  - Production readiness determined from receipt guard results

#### Track 3: Quality Autopilot (Improvement Loop)
- **What**: New CLI command to suggest and apply improvements

- **Command**: `ggen market improve <package-id>`

- **What it does**:
  1. Read latest receipt for package
  2. Identify failing guards
  3. Suggest fixes:
     - Missing LICENSE → suggest MIT/Apache and generate file
     - README < 500 chars → expand with template examples
     - No tests → scaffold test directory
     - No RDF → generate starter ontology
  4. Optional: apply changes in local branch for review

- **Success Criteria**:
  - Can run `ggen market improve data-pipeline-cli`
  - Gets list of proposed changes
  - Can accept changes interactively

#### Track 4: Chatman Integration (Specialized Guards)
- **What**: Chicago-compliance and specification harness

- **GuardChicagoCompliance** adds validation for:
  - Uses `chicago-tdd-tools` (if applicable)
  - AAA test patterns (Arrange-Act-Assert)
  - Forbids `unwrap()`, `expect()`, `panic!` in production code
  - Coverage thresholds met

- **Specification Harness Package**:
  - New package: `chatman-equation-spec-harness`
  - When installed, validates project against Chatman Equation invariants
  - Determinism, idempotence, guard usage, receipts

- **Success Criteria**:
  - `GuardChicagoCompliance` validates selected packages
  - `chatman-equation-spec-harness` publishes and installs
  - Packages marked `chatman_certified = true` in ontology

#### Track 5: Sector Bundles (Vertical Stacks)
- **What**: Curated collections of packages forming complete solutions

- **Example Bundles**:
  1. `sector-academic-papers`:
     - `academic-paper-lifecycle` (core)
     - `academic-bibliography-manager` (metadata)
     - `academic-peer-review-workflow` (process)
     - LaTeX templates
     - RDF ontology for papers

  2. `sector-enterprise-support`:
     - Hook engine templates
     - YAWL pattern wiring
     - Guard configs for chronology, budget, legal

  3. `sector-data-pipeline`:
     - Data pipeline CLI
     - Storage templates
     - Transformation patterns

- **Command**: `ggen market install-bundle sector-academic-papers`

- **Implementation**:
  1. Define Bundle class in ontology with `includesPackage` relations
  2. Build ggen command to resolve and install all packages
  3. Generate bundle documentation and receipts

- **Success Criteria**:
  - At least 3 bundles defined in ontology
  - Bundles validated with bundle-level receipts
  - `install-bundle` command works end-to-end

#### Track 6: CI/CD Enforcement (Registry Invariants)
- **What**: GitHub Actions workflow to maintain marketplace health

- **CI Rules**:
  - On any PR modifying `marketplace/ontology.ttl` or validation code:
    1. Run `make marketplace-validate`
    2. Run `make marketplace-report`
    3. Regenerate all projections
    4. Fail if:
       - Any package score drops
       - Production-ready package starts failing guards
       - New package below 80%

- **Drift Monitoring Dashboard**:
  - Generate `marketplace/DRIFT_REPORT.md` with:
    - Packages by score bucket (95+, 80-94, <80)
    - Count of chatman-certified packages
    - Bundle coverage metrics
    - Trend analysis

- **Success Criteria**:
  - CI workflow created and passing
  - No regressions allowed
  - Dashboard shows ecosystem health

---

## How Guards Drive Scoring

### Scoring Algorithm

```
final_score = (Σ guard_weight × guard_passed) / Σ all_guard_weights × 100

production_ready = (all_critical_guards_pass) AND (final_score ≥ 95.0)
```

### Guard Weights (in Ontology)
| Guard | Type | Weight | Severity |
|-------|------|--------|----------|
| Metadata | code | 10 | Critical |
| License | code | 8 | Critical |
| README | code | 7 | Critical |
| Source | code | 9 | Critical |
| Tests | code | 8 | Critical |
| Templates | bonus | 6 | Bonus |
| RDF | bonus | 5 | Bonus |
| SPARQL | bonus | 4 | Bonus |
| Docs | bonus | 5 | Bonus |
| Chicago | code | 15 | Critical |

### Score Interpretation
- **95-100%**: Production-ready, all critical guards pass
- **80-94%**: Beta, minor gaps, suitable for most use
- **<80%**: Experimental, significant improvements needed

---

## Integration with Chatman Equation

The marketplace becomes an audited, guard-enforced reflection of your specification:

### Invariants Validated by Guards
1. **Determinism**: Source code absence fails GuardSrc
2. **Idempotence**: Tests validate idempotent behavior
3. **Guard Usage**: GuardChicagoCompliance checks for guard patterns
4. **Receipts**: Validation receipts are immutable (checksummed)
5. **Declarative Metadata**: Package.toml guards ensure structure

### Spec Harness Integration
When you install `chatman-equation-spec-harness` in a package:
```bash
ggen market install chatman-equation-spec-harness
cargo make spec-validate
```

This runs:
- Invariant checks (determinism, idempotence)
- Guard pattern analysis
- Receipt generation and chaining
- Compliance report

---

## File Structure

```
marketplace/
├── ontology.ttl                    # O_market: single source of truth
├── registry/
│   └── index.json                  # Generated from ontology
├── templates/                      # μ_market: ggen projection templates
│   ├── index.json.hbs             # JSON registry template
│   ├── PACKAGES.md.hbs            # Markdown docs template
│   └── receipt.json.hbs           # Receipt template
├── receipts/                       # A: Validation audit trail
│   ├── data-pipeline-cli/
│   │   ├── 2.0.0.json
│   │   ├── 2.1.0.json
│   │   └── 2.1.1.json
│   ├── reasoning-cli/
│   │   └── 1.0.0.json
│   └── ...
├── packages/                       # 55+ marketplace packages
│   ├── data-pipeline-cli/
│   ├── reasoner-cli/
│   └── ...
├── scripts/                        # Legacy (being migrated to guards)
│   ├── validate_all_packages.sh
│   ├── generate_index.sh
│   └── ...
├── PACKAGES.md                     # Generated from ontology
├── ONTOLOGY_GUARDS_ARCHITECTURE.md # This file
└── ...
```

---

## Command Workflow

### Current Commands (Existing)
```bash
make marketplace-validate          # Validates all packages
make marketplace-validate-update   # Updates production_ready flags
make marketplace-report            # Generates reports
```

### New Commands (Phase 2)
```bash
# Generate all artifacts from ontology
make marketplace-generate

# Quality improvement loop
ggen market improve <package-id>
ggen market improve-all             # Improve bottom quartile

# Bundle operations
ggen market install-bundle sector-academic-papers
ggen market bundle-validate

# Monitoring
make marketplace-report            # Updated: shows drift metrics
```

---

## Success Metrics

### By End of Phase 2
- [ ] All 55 packages have validation receipts
- [ ] 100% of packages are generated (no hand-edits)
- [ ] Bottom quartile (lowest scoring packages) improved to ≥80%
- [ ] At least 3 sector bundles defined and validated
- [ ] At least 10 packages chatman-certified
- [ ] Zero regressions in CI
- [ ] Drift dashboard operational

### Long-term Vision
- Marketplace becomes **living, audited substrate** reflecting Chatman Equation
- Packages automatically improve via autopilot
- Bundles enable vertical adoption
- Citations: "This project uses sector-academic-papers bundle with all invariants validated"

---

## References

1. **Ontology**: `marketplace/ontology.ttl` — RDF/TTL formal model
2. **Guard Implementation**: `crates/ggen-domain/src/marketplace/guards.rs` — Rust traits and factories
3. **Validation**: `crates/ggen-domain/src/marketplace/validate.rs` — Extended with receipt emission
4. **CLI Commands**: `crates/ggen-cli/src/cmds/marketplace.rs` — Commands dispatching to domain layer
5. **Chatman Specification**: Reference implementation of validation patterns

---

## Next Steps

**For the Swarm**:

1. **Pick a track** from Phase 2 above
2. **Work in parallel** — tracks are independent until synthesis
3. **Emit receipts** for all packages (enables all other tracks)
4. **Test thoroughly** — use existing 55 packages as test bed
5. **Commit and push** to `claude/marketplace-ontology-guards-017CPfCsz3gA29MV38F2Lxn9`
6. **Document in PRs** what changed in O, μ, and A

The transformation is incremental: each track strengthens the marketplace without breaking existing workflow.
