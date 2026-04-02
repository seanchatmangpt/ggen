# Marketplace Ontology-Guards Transformation: Swarm Execution Plan

**Status**: Phase 1 Foundation Complete ✅ | Phase 2 Ready for Parallel Execution
**Branch**: `claude/marketplace-ontology-guards-017CPfCsz3gA29MV38F2Lxn9`
**Commit**: d3e757b9 (foundation)
**Date**: 2025-11-16

---

## Summary: What Was Built

The swarm asked: **"What should O and μ change so A jumps a level in rigor and usefulness?"**

### Answer: Formal Ontology-Backed Validation

**O_market** (Ontology):
- `marketplace/ontology.ttl` — 700+ lines of RDF/RDFS/OWL
- Single source of truth for all package metadata, assets, guards, bundles
- Defines 10 guard types with weights and severity levels
- Enables semantic reasoning over ecosystem

**μ_market** (Generator):
- `guards.rs` — Rust trait system for formal validation rules
- `ValidationReceipt` — Immutable, checksummed audit records
- Standard guard factories (Metadata, License, README, Tests)
- Foundation for ggen templates to generate all artifacts

**A** (Artifacts):
- `marketplace/receipts/<pkg>/<version>.json` — Audit trail for every validation
- Generated `index.json` (no more hand-edits)
- Generated `PACKAGES.md` with metadata from ontology
- Formal scoring algorithm: critical guards + weighted bonuses

### Result: Marketplace ↑ from "useful package list" to "audited, guard-enforced substrate"

---

## Phase 1: Complete ✅

✅ Create marketplace/ontology.ttl (O_market)
✅ Implement guards system in Rust (μ_market validators)
✅ Design ValidationReceipt with checksumming
✅ Document complete architecture and playbook

**Files Added/Modified**:
- `marketplace/ontology.ttl` (1,000 lines)
- `crates/ggen-domain/src/marketplace/guards.rs` (700 lines)
- `crates/ggen-domain/src/marketplace/mod.rs` (exports)
- `marketplace/ONTOLOGY_GUARDS_ARCHITECTURE.md` (playbook)

---

## Phase 2: Parallel Work Tracks

### 🎯 Critical Path (Do First)

**Track A: Emit Validation Receipts** — Unblocks all other tracks
- **Effort**: Medium (2-4 hours)
- **Dependencies**: None (builds on guards.rs)
- **Deliverable**: All 55 packages have `marketplace/receipts/<pkg>/<version>.json`

**How**:
1. Extend `validate.rs` to instantiate guard factories and execute them
2. Collect results into `ValidationReceipt`
3. Wire receipt emission into `make marketplace-validate` task
4. Modify `marketplace-validate-update` to read latest receipt and set `production_ready`

**Code Entry Points**:
- `crates/ggen-domain/src/marketplace/validate.rs` — add receipt emission
- `crates/ggen-domain/src/marketplace/guards.rs::factories` — instantiate guards here
- `Makefile.toml` — wire receipt pipeline

**Success**: Run `make marketplace-validate` → all receipts in `marketplace/receipts/`

---

### 📊 Generate Artifacts from Ontology (μ_market)

**Track B1: Generate `index.json` from ontology**
- **Effort**: Medium
- **Dependencies**: Track A (needs current scores)
- **Deliverable**: Automated `marketplace/registry/index.json` generation

**How**:
1. Create ggen template `marketplace/templates/index.json.hbs`
2. Reads O_market (ontology.ttl) via SPARQL or direct parsing
3. Projects into registry structure with package metadata
4. Generates from `make marketplace-generate`

**Track B2: Generate `PACKAGES.md` from ontology**
- **Effort**: Medium
- **Dependencies**: Track B1
- **Deliverable**: Automated `marketplace/PACKAGES.md` generation

**How**:
1. Create ggen template `marketplace/templates/PACKAGES.md.hbs`
2. Reads ontology, formats as organized Markdown sections
3. Includes score badges, links, installation commands
4. Wired to same `make marketplace-generate`

**Success**: `make marketplace-generate` produces identical outputs to manual registry

---

### 🚀 Quality Autopilot (Improvement Loop)

**Track C: Implement `ggen market improve <package-id>`**
- **Effort**: High
- **Dependencies**: Track A (reads receipts)
- **Deliverable**: CLI command that proposes and applies improvements

**What it does**:
```bash
ggen market improve data-pipeline-cli
# Output:
# Package: data-pipeline-cli (v2.1.0)
# Score: 78.5% → Opportunity: 87%+
#
# Failing Guards:
# ❌ GuardDocs (weight 5, bonus): No CONTRIBUTING.md
# ❌ GuardRdf (weight 5, bonus): Missing ontology
#
# Proposed Improvements:
# 1. Generate CONTRIBUTING.md (template)
# 2. Generate starter ontology (3 classes, 5 properties)
# 3. Create examples/ directory with usage demo
#
# Apply changes? [y/n]:
```

**Implementation**:
1. Read latest receipt for package
2. Filter failing guards
3. For each guard type, suggest fix:
   - GuardLicense → generate MIT/Apache file
   - GuardReadme → expand with examples template
   - GuardDocs → add CONTRIBUTING.md, API docs
   - GuardRdf → scaffold ontology with starter classes
   - GuardTests → generate test harness
4. Option to apply in local branch

**Code Entry Point**:
- `crates/ggen-cli/src/cmds/marketplace.rs` — add `improve` verb

**Success**: Can improve a package from 75% → 85%+ in one command

---

### 🏆 Chatman Integration

**Track D: Add `GuardChicagoCompliance`**
- **Effort**: Medium
- **Dependencies**: Track A
- **Deliverable**: Specialized guard for Chatman Equation adherence

**What it validates**:
- Uses `chicago-tdd-tools` in tests (if applicable)
- Has AAA pattern (Arrange-Act-Assert) in tests
- Forbids `unwrap()`, `expect()`, `panic!` in production `src/`
- Coverage threshold met (≥80% for production code)
- Property/mutation/snapshot tests wired

**Implementation**:
1. Create new guard struct in `guards.rs::factories`
2. Check for forbidden patterns via AST analysis or grep
3. Verify coverage reports
4. Mark packages `chatman_certified = true` in ontology when passing

**Code Entry Point**:
- `crates/ggen-domain/src/marketplace/guards.rs` — add `GuardChicagoCompliance` factory

**Track D2: Build `chatman-equation-spec-harness` Package**
- **Effort**: High
- **Dependencies**: Track D
- **Deliverable**: Publishable marketplace package

**What it includes**:
- Binary that validates project structure against Chatman invariants
- Tests for determinism, idempotence, receipt chaining
- Integration with `chicago-tdd-tools`
- README with specification reference

**How to publish**:
1. Create `marketplace/packages/chatman-equation-spec-harness/`
2. Add `package.toml`, `Cargo.toml`, README
3. Implement validation logic
4. Run `ggen marketplace publish`

**Success**: Users can `ggen market install chatman-equation-spec-harness` and run validation

---

### 📦 Sector Bundles (Vertical Stacks)

**Track E: Define and Implement Bundles**
- **Effort**: High
- **Dependencies**: Track A, B
- **Deliverable**: 3+ sector bundles with full receipts

**Bundle 1: `sector-academic-papers`**
```
Packages:
  ├── academic-paper-lifecycle (core)
  ├── academic-bibliography-manager
  ├── academic-peer-review-workflow
  ├── LaTeX paper template
  └── RDF ontology for academic metadata

Validation: All packages ≥80%, bundle-level e2e tests pass
```

**Bundle 2: `sector-data-pipelines`**
```
Packages:
  ├── data-pipeline-cli
  ├── database-schema-generator
  ├── data transformation templates
  └── monitoring/alerting ontology
```

**Bundle 3: `sector-enterprise-saas`**
```
Packages:
  ├── multi-tenant-saas
  ├── crm-customer-management
  ├── enterprise-erp-core
  ├── hook-engine templates
  └── YAWL patterns for workflows
```

**Implementation**:
1. Add `Bundle` instance to `marketplace/ontology.ttl` for each sector
2. Define `includesPackage` relations to all member packages
3. Create `ggen market install-bundle <bundle-id>` command
4. Generate bundle-level validation receipts
5. Document bundle use cases and setup

**Code Entry Point**:
- `marketplace/ontology.ttl` — add Bundle definitions
- `crates/ggen-cli/src/cmds/marketplace.rs` — add `install-bundle` verb
- `crates/ggen-domain/src/marketplace/` — bundle resolution logic

**Success**: Users can `ggen market install-bundle sector-academic-papers` and get full vertical

---

### 🔒 CI/CD and Enforcement

**Track F: GitHub Actions Registry Invariants**
- **Effort**: Medium
- **Dependencies**: Track A, B
- **Deliverable**: `.github/workflows/marketplace-validate.yml`

**What it does on each PR**:
1. Run `make marketplace-validate` (emits receipts)
2. Run `make marketplace-report` (generates drift dashboard)
3. Regenerate `index.json` and `PACKAGES.md`
4. Check for regressions:
   - ❌ Fail if any package score drops
   - ❌ Fail if production-ready package starts failing critical guards
   - ❌ Fail if new package below 80%
5. Post summary comment on PR

**Drift Report**: Auto-generated `marketplace/DRIFT_REPORT.md` showing:
- Packages by score bucket (95+, 80-94, <80)
- Count of chatman-certified packages
- Bundle coverage metrics
- Trend analysis (improving vs regressing packages)

**Code Entry Points**:
- Create `.github/workflows/marketplace-validate.yml`
- Add drift reporting to `scripts/generate_validation_report.sh` or new `generate_drift_report.sh`
- Update `Makefile.toml` to wire reporting

**Success**: CI blocks regressions; PRs show ecosystem health impact

---

## Work Allocation Strategy

### Recommended Parallel Schedule

**Week 1 (Immediate)**:
- **Track A** (Track A lead): Emit receipts — unblocks everything else
  - Pair: code review on guards usage

**Week 1 (Parallel)**:
- **Track B1 + B2** (Template lead): Generate artifacts from ontology
  - Depends on: Some initial receipts from Track A
  - Pair: Test with existing packages

**Week 1 (Parallel)**:
- **Track F** (CI lead): Set up GitHub Actions and drift reporting
  - Depends on: Artifact generation working
  - Pair: Validate on example packages

**Week 2 (Dependent)**:
- **Track C** (Autopilot lead): Quality improvement command
  - Depends on: Track A (receipts) + Track B (artifacts current)
  - High value: enables 80→95% upgrade loop

**Week 2 (Parallel)**:
- **Track D + D2** (Chatman lead): Chicago compliance guard + spec harness
  - Depends on: Track A (guard system)
  - Pair: Integration testing with existing Rust packages

**Week 3 (Synthesis)**:
- **Track E** (Bundle lead): Define and implement sector bundles
  - Depends on: All previous tracks
  - Critical: bundle-level receipts, install-bundle command
  - Pair: Bundle testing and documentation

### Effort Estimation

| Track | Role | Est. Hours | Parallelizable |
|-------|------|-----------|-----------------|
| A | Rust/Validation | 3-4h | With B, F |
| B1 | Templates | 2-3h | With A, F |
| B2 | Templates | 2-3h | With B1 |
| C | CLI/Rust | 4-5h | After A |
| D | Rust/Guards | 3-4h | With D2 |
| D2 | Package Dev | 4-5h | With D |
| E | Ontology/CLI | 5-6h | After all others |
| F | CI/Automation | 3-4h | With A, B |
| **Total** | | **26-34h** | 2-3 parallel teams |

---

## Definition of Done

### Per Track

**Track A**:
- [ ] All 55 packages have receipts in `marketplace/receipts/`
- [ ] Receipts are valid JSON, correctly checksummed
- [ ] Production-ready flags match receipt overall scores
- [ ] `make marketplace-validate` completes without errors

**Track B1 + B2**:
- [ ] `make marketplace-generate` produces identical outputs
- [ ] No git diffs on re-generation
- [ ] All 55 packages represented
- [ ] Links and formatting verified

**Track C**:
- [ ] `ggen market improve <pkg>` runs without errors
- [ ] Proposed improvements are sensible
- [ ] Can apply changes interactively
- [ ] Improved package score increases after applying

**Track D**:
- [ ] GuardChicagoCompliance validates patterns correctly
- [ ] chatman-equation-spec-harness package publishes
- [ ] At least 5 packages marked `chatman_certified = true`
- [ ] Spec harness can be installed and used

**Track E**:
- [ ] 3+ sector bundles defined in ontology
- [ ] `ggen market install-bundle` resolves and installs
- [ ] Bundle-level receipts generated
- [ ] Documentation for each bundle

**Track F**:
- [ ] CI workflow runs on all PRs
- [ ] Regression detection works
- [ ] Drift report generated and posted
- [ ] No false positives/negatives

---

## Integration Points

### Merge Strategy
1. **Foundation → main** (already merged)
2. **Track A** → foundation
3. **Track B, F** → Track A
4. **Track C** → Track B, A
5. **Track D** → Track A
6. **Track E** → All others

### Testing
- **Unit tests** in each track's Rust code
- **Integration tests** across tracks
- **E2E tests** on real marketplace (55 packages)
- **Regression tests** via CI workflow

### Documentation
- Update `ONTOLOGY_GUARDS_ARCHITECTURE.md` as builds progress
- Add `ggen market` subcommand docs
- Document bundle setup and usage
- Add examples for each guard type

---

## Success Criteria (End of Phase 2)

- [ ] All 55 packages have validation receipts
- [ ] 100% of registry artifacts are generated (zero hand-edits)
- [ ] Bottom quartile (lowest 14 packages) improved to ≥80%
- [ ] At least 3 sector bundles fully functional
- [ ] At least 10 packages marked chatman-certified
- [ ] CI workflow blocks regressions, posts health reports
- [ ] Drift dashboard operational and tracked
- [ ] Zero critical bugs in production paths
- [ ] All code reviewed and tests passing

---

## Next Action

**Immediately**:
1. Pick a track based on team expertise
2. Create a tracking issue/PR per track
3. Reference this plan in PRs
4. Commit to `claude/marketplace-ontology-guards-017CPfCsz3gA29MV38F2Lxn9`
5. Update SWARM_EXECUTION_PLAN.md progress as you go

**The swarm's role**: Execute 1-2 tracks to completion, ensuring quality and integration with all others.

**Typical sprint**: One track per developer/pair per week → full Phase 2 in 2-3 weeks with coordinated team.

---

## References

- **Architecture**: `marketplace/ONTOLOGY_GUARDS_ARCHITECTURE.md`
- **Guard Implementation**: `crates/ggen-domain/src/marketplace/guards.rs`
- **Ontology**: `marketplace/ontology.ttl`
- **Validation**: `crates/ggen-domain/src/marketplace/validate.rs`
- **CLI**: `crates/ggen-cli/src/cmds/marketplace.rs`
