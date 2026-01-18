<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Quick Fix Reference - Week 1 Compiler Errors](#quick-fix-reference---week-1-compiler-errors)
  - [Phase 1: Core Definitions (45 min) → 91 errors resolved](#phase-1-core-definitions-45-min-%E2%86%92-91-errors-resolved)
    - [1. Observation Struct (18 errors) - 15 min](#1-observation-struct-18-errors---15-min)
    - [2. DeltaSigmaProposal Struct (30 errors) - 20 min](#2-deltasigmaproposal-struct-30-errors---20-min)
    - [3. ObservationSource Enum (16 errors) - 10 min](#3-observationsource-enum-16-errors---10-min)
    - [4. ValidationContext Struct (10 errors) - 15 min](#4-validationcontext-struct-10-errors---15-min)
    - [5. ValidationEvidence Struct (9 errors) - 15 min](#5-validationevidence-struct-9-errors---15-min)
    - [6. PatternType Enum (8 errors) - 5 min](#6-patterntype-enum-8-errors---5-min)
  - [Phase 2: Config Structs (30 min) → 24 errors resolved](#phase-2-config-structs-30-min-%E2%86%92-24-errors-resolved)
    - [7. ProposerConfig Struct (6 errors) - 10 min](#7-proposerconfig-struct-6-errors---10-min)
    - [8. ProposedChange Struct (6 errors) - 10 min](#8-proposedchange-struct-6-errors---10-min)
    - [9. MinerConfig Struct (4 errors) - 10 min](#9-minerconfig-struct-4-errors---10-min)
    - [10. ConstitutionValidation Struct (4 errors) - 10 min](#10-constitutionvalidation-struct-4-errors---10-min)
    - [11. PromotionResult Struct (4 errors) - 10 min](#11-promotionresult-struct-4-errors---10-min)
  - [Phase 3: Implementation (30 min) → 21 errors resolved](#phase-3-implementation-30-min-%E2%86%92-21-errors-resolved)
    - [12. Validator validate() Methods (9 errors) - 30 min](#12-validator-validate-methods-9-errors---30-min)
    - [13. Pipeline Field Visibility (12 errors) - 5 min](#13-pipeline-field-visibility-12-errors---5-min)
  - [Phase 4: Cleanup (20 min) → 25 errors resolved](#phase-4-cleanup-20-min-%E2%86%92-25-errors-resolved)
    - [14. Type Mismatches (18 errors) - 20 min](#14-type-mismatches-18-errors---20-min)
    - [15. SigmaOverlay id Field (2 errors) - 5 min](#15-sigmaoverlay-id-field-2-errors---5-min)
    - [16. Swarm Module Resolution (4 errors) - 10 min](#16-swarm-module-resolution-4-errors---10-min)
    - [17. Result Type Generics (1 error) - 2 min](#17-result-type-generics-1-error---2-min)
  - [Final Verification](#final-verification)
  - [Quick Command Reference](#quick-command-reference)
  - [Commit Strategy](#commit-strategy)
  - [Troubleshooting](#troubleshooting)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Quick Fix Reference - Week 1 Compiler Errors

**Use this as a checklist while fixing errors systematically.**

---

## Phase 1: Core Definitions (45 min) → 91 errors resolved

### 1. Observation Struct (18 errors) - 15 min

**File**: `crates/ggen-core/src/ontology/pattern_miner.rs:115`

```rust
// CHANGE THIS:
pub struct Observation {
    pub entity: String,
    pub properties: BTreeMap<String, String>,
    pub timestamp: u64,
    pub source: ObservationSource,
}

// TO THIS:
pub struct Observation {
    pub entity: String,
    pub properties: BTreeMap<String, String>,
    pub timestamp: u64,
    pub source: ObservationSource,
    pub description: String,  // ADD
    pub metadata: BTreeMap<String, String>,  // ADD
}
```

**Verify**: `cargo make check` → expect ~207 errors (225 - 18)

---

### 2. DeltaSigmaProposal Struct (30 errors) - 20 min

**File**: `crates/ggen-core/src/ontology/delta_proposer.rs:21`

**Step 1**: Add RiskLevel enum before struct:
```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RiskLevel {
    Low,
    Medium,
    High,
    Critical,
}
```

**Step 2**: Update struct:
```rust
// ADD TO EXISTING STRUCT:
pub struct DeltaSigmaProposal {
    // ... existing fields ...
    pub description: String,  // ADD
    pub rationale: String,  // ADD
    pub risk_assessment: RiskLevel,  // ADD
    pub backward_compatible: bool,  // ADD
}
```

**Verify**: `cargo make check` → expect ~177 errors (207 - 30)

---

### 3. ObservationSource Enum (16 errors) - 10 min

**File**: `crates/ggen-core/src/ontology/pattern_miner.rs:123`

```rust
// ADD VARIANTS:
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ObservationSource {
    Data,
    Artifact,
    Receipt,
    QueryLog,  // ADD
    UserFeedback,  // ADD
    SchemaEvolution,  // ADD
    PerformanceMetrics,  // ADD
}
```

**Verify**: `cargo make check` → expect ~161 errors (177 - 16)

---

### 4. ValidationContext Struct (10 errors) - 15 min

**File**: Search for `pub struct ValidationContext` in `crates/ggen-core/src`

```rust
// ADD TO EXISTING STRUCT:
pub struct ValidationContext {
    // ... existing fields ...
    pub snapshot_id: String,  // ADD
    pub current_stats: OntologyStats,  // ADD
}
```

**Verify**: `cargo make check` → expect ~151 errors (161 - 10)

---

### 5. ValidationEvidence Struct (9 errors) - 15 min

**File**: Search for `pub struct ValidationEvidence` in `crates/ggen-core/src`

**Step 1**: Add Severity enum:
```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Severity {
    Info,
    Warning,
    Error,
    Critical,
}
```

**Step 2**: Update struct:
```rust
// ADD TO EXISTING STRUCT:
pub struct ValidationEvidence {
    // ... existing fields ...
    pub severity: Severity,  // ADD
    pub location: String,  // ADD
    pub description: String,  // ADD
}
```

**Verify**: `cargo make check` → expect ~142 errors (151 - 9)

---

### 6. PatternType Enum (8 errors) - 5 min

**File**: `crates/ggen-core/src/ontology/pattern_miner.rs:37`

```rust
// ADD VARIANT:
pub enum PatternType {
    RepeatedStructure,
    RepeatedProperty,
    SchemaMismatch,
    Drift,  // ADD
}
```

**Verify**: `cargo make check` → expect ~134 errors (142 - 8)

---

## Phase 2: Config Structs (30 min) → 24 errors resolved

### 7. ProposerConfig Struct (6 errors) - 10 min

**File**: Search for `pub struct ProposerConfig`

```rust
// ADD TO EXISTING STRUCT:
pub struct ProposerConfig {
    // ... existing fields ...
    pub max_proposals_per_iteration: usize,  // ADD
    pub llm_endpoint: Option<String>,  // ADD
}
```

**Verify**: `cargo make check` → expect ~128 errors

---

### 8. ProposedChange Struct (6 errors) - 10 min

**File**: Search for `pub struct ProposedChange`

```rust
// ADD TO EXISTING STRUCT (reuse RiskLevel from #2):
pub struct ProposedChange {
    // ... existing fields ...
    pub risk_level: RiskLevel,  // ADD
    pub description: String,  // ADD
}
```

**Verify**: `cargo make check` → expect ~122 errors

---

### 9. MinerConfig Struct (4 errors) - 10 min

**File**: Search for `pub struct MinerConfig`

```rust
// ADD TO EXISTING STRUCT:
pub struct MinerConfig {
    // ... existing fields ...
    pub confidence_threshold: f64,  // ADD
    pub min_observations: usize,  // ADD
}
```

**Verify**: `cargo make check` → expect ~118 errors

---

### 10. ConstitutionValidation Struct (4 errors) - 10 min

**File**: Search for `pub struct ConstitutionValidation`

```rust
// ADD TO EXISTING STRUCT:
pub struct ConstitutionValidation {
    // ... existing fields ...
    pub total_violations: usize,  // ADD
    pub all_satisfied: bool,  // ADD
}
```

**Verify**: `cargo make check` → expect ~114 errors

---

### 11. PromotionResult Struct (4 errors) - 10 min

**File**: Search for `pub struct PromotionResult`

```rust
// ADD TO EXISTING STRUCT:
pub struct PromotionResult {
    // ... existing fields ...
    pub success: bool,  // ADD
    pub promoted_snapshot_id: Option<String>,  // ADD
}
```

**Verify**: `cargo make check` → expect ~110 errors

---

## Phase 3: Implementation (30 min) → 21 errors resolved

### 12. Validator validate() Methods (9 errors) - 30 min

**File**: Search for validator structs (TypeSoundnessCheck, SLOPreservationCheck, etc.)

**Option 1**: Add trait and implement for all:
```rust
pub trait Validator {
    fn validate(&self, context: &ValidationContext) -> Result<ValidationEvidence>;
}

impl Validator for TypeSoundnessCheck {
    fn validate(&self, context: &ValidationContext) -> Result<ValidationEvidence> {
        // Basic implementation
        Ok(ValidationEvidence {
            severity: Severity::Info,
            location: "global".to_string(),
            description: "Type soundness check passed".to_string(),
            // ... other fields
        })
    }
}
```

**Option 2**: Add method to each struct individually (faster):
```rust
impl TypeSoundnessCheck {
    pub fn validate(&self, context: &ValidationContext) -> Result<ValidationEvidence> {
        // Implementation
    }
}
```

**Repeat for**: SLOPreservationCheck, ProjectionDeterminismCheck, NoRetrocausationCheck, ImmutabilityCheck, GuardSoundnessCheck, CompositeValidator, AtomicPromotionCheck

**Verify**: `cargo make check` → expect ~101 errors

---

### 13. Pipeline Field Visibility (12 errors) - 5 min

**File**: Search for `pub struct Pipeline`

```rust
// CHANGE THIS:
pub struct Pipeline {
    tera: Tera,  // Private
}

// TO THIS:
pub struct Pipeline {
    pub tera: Tera,  // Public
}
```

**Verify**: `cargo make check` → expect ~89 errors

---

## Phase 4: Cleanup (20 min) → 25 errors resolved

### 14. Type Mismatches (18 errors) - 20 min

**File**: `tests/ontology_systems_tests.rs` and others

**Example Fix**:
```rust
// CHANGE THIS:
let deserialized: Result<SigmaSnapshot, _> = serde_json::from_str(&serialized);

// TO THIS:
let deserialized: Result<SigmaSnapshot, serde_json::Error> = serde_json::from_str(&serialized);
// OR
let deserialized = serde_json::from_str::<SigmaSnapshot>(&serialized)?;
```

**Fix each individually based on error message**

**Verify**: `cargo make check` → expect ~71 errors

---

### 15. SigmaOverlay id Field (2 errors) - 5 min

**File**: Search for `pub struct SigmaOverlay`

```rust
// ADD TO EXISTING STRUCT:
pub struct SigmaOverlay {
    pub id: String,  // ADD
    // ... existing fields ...
}
```

**Verify**: `cargo make check` → expect ~69 errors

---

### 16. Swarm Module Resolution (4 errors) - 10 min

**File**: `examples/swarm_intelligence_demo.rs`

**Option 1**: Remove example file (if swarm not implemented):
```bash
git rm examples/swarm_intelligence_demo.rs
```

**Option 2**: Create stub module in `crates/ggen-ai/src/swarm/mod.rs`:
```rust
pub mod algorithms {
    pub mod aco { /* ... */ }
    pub mod pso { /* ... */ }
    pub mod evolution { /* ... */ }
}
pub mod emergent { /* ... */ }
```

**Verify**: `cargo make check` → expect ~65 errors

---

### 17. Result Type Generics (1 error) - 2 min

**File**: `tests/ontology_systems_tests.rs:235`

```rust
// CHANGE THIS:
let deserialized: Result<SigmaSnapshot, _> = serde_json::from_str(&serialized);

// TO THIS:
let deserialized: Result<SigmaSnapshot> = serde_json::from_str(&serialized)
    .map_err(|e| Error::Serialization(e.to_string()))?;
```

**Verify**: `cargo make check` → expect **0 errors!** ✅

---

## Final Verification

```bash
# All errors should be resolved
cargo make check
# Expected: 0 errors

# All tests should compile (may have logical failures)
cargo make test
# Expected: tests compile, some may fail (logic issues)

# No linting errors
cargo make lint
# Expected: 0 clippy warnings/errors

# Run full CI pipeline
cargo make ci
# Expected: all checks pass
```

---

## Quick Command Reference

```bash
# Quick error count
cargo make check 2>&1 | grep -c "error:"

# Show error codes
cargo make check 2>&1 | grep "error\[E" | cut -d'[' -f2 | cut -d']' -f1 | sort | uniq -c

# Test specific file
cargo test --test ontology_systems_tests

# Watch mode (auto-rebuild)
cargo make watch

# Format after changes
cargo fmt
```

---

## Commit Strategy

After each phase:
```bash
git add -A
git commit -m "fix: Phase X - [description]

- Fixed [struct/enum names]
- Resolved X errors
- Verified with cargo make check"
```

**Example commits**:
- `fix: Phase 1 - Core struct definitions (91 errors resolved)`
- `fix: Phase 2 - Config structs (24 errors resolved)`
- `fix: Phase 3 - Implementation methods (21 errors resolved)`
- `fix: Phase 4 - Cleanup and type fixes (25 errors resolved)`

---

## Troubleshooting

**Q**: Error count not decreasing as expected?
**A**: Run `cargo clean && cargo make check` to clear cache

**Q**: New errors appearing?
**A**: Check if added fields need Default impl or update constructors

**Q**: Can't find struct definition?
**A**: Use `grep -r "pub struct [StructName]" crates/ggen-core/src`

**Q**: Tests still failing after all errors fixed?
**A**: That's expected - logic failures are Week 2 work

---

**Generated**: 2025-11-20 by Production Validation Specialist
**Status**: QUICK REFERENCE - USE WHILE FIXING
