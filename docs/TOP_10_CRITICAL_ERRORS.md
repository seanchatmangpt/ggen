# Top 10 Critical Errors - Week 1 Validation

**Priority**: Fix these first → Resolve 82.7% of all compiler errors

---

## 1. DeltaSigmaProposal Struct API Mismatch

**Error Count**: 30 errors (13.3% of total)
**Severity**: CRITICAL
**Estimated Fix Time**: 20 minutes

### Problem

Test code expects fields that don't exist in struct definition:
- `description: String` → 6 E0560 errors
- `rationale: String` → 6 E0560 errors
- `risk_assessment: String/Enum` → 6 E0560 errors
- `backward_compatible: bool` → 6 E0560 errors

Plus 6 corresponding E0609 field access errors when tests try to read these fields.

### Current Definition

```rust
// crates/ggen-core/src/ontology/delta_proposer.rs:21
pub struct DeltaSigmaProposal {
    pub id: String,
    pub change_type: String,
    pub target_element: String,
    // Missing 4 fields!
}
```

### Expected Definition

Tests expect this API:

```rust
pub struct DeltaSigmaProposal {
    pub id: String,
    pub change_type: String,
    pub target_element: String,
    pub description: String,  // ADD: Human-readable description of change
    pub rationale: String,  // ADD: Why this change is proposed
    pub risk_assessment: RiskLevel,  // ADD: Risk level (Low/Medium/High/Critical)
    pub backward_compatible: bool,  // ADD: Whether change is backward compatible
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RiskLevel {
    Low,
    Medium,
    High,
    Critical,
}
```

### Fix Steps

1. **Define RiskLevel enum** in `delta_proposer.rs`:
   ```rust
   #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
   pub enum RiskLevel {
       Low,
       Medium,
       High,
       Critical,
   }
   ```

2. **Update struct definition**:
   ```rust
   pub struct DeltaSigmaProposal {
       pub id: String,
       pub change_type: String,
       pub target_element: String,
       pub description: String,
       pub rationale: String,
       pub risk_assessment: RiskLevel,
       pub backward_compatible: bool,
   }
   ```

3. **Update constructor/builders** to accept new fields

4. **Update tests** that create `DeltaSigmaProposal` instances

5. **Run verification**:
   ```bash
   cargo make check
   # Expect 30 fewer errors (225 → 195)
   ```

### Impact

- **Unblocks**: ProposerConfig tests, PromotionResult tests
- **Dependencies**: None (independent fix)
- **Risk**: Low (additive change, no breaking modifications)

---

## 2. Observation Struct API Mismatch

**Error Count**: 18 errors (8% of total)
**Severity**: CRITICAL
**Estimated Fix Time**: 15 minutes

### Problem

Test code expects `description` and `metadata` fields:
- `description: String` → 6 E0560 errors
- `metadata: BTreeMap<String, String>` → 6 E0560 errors

Plus 6 corresponding E0609 field access errors.

### Current Definition

```rust
// crates/ggen-core/src/ontology/pattern_miner.rs:115
pub struct Observation {
    pub entity: String,
    pub properties: BTreeMap<String, String>,
    pub timestamp: u64,
    pub source: ObservationSource,
}
```

### Expected Definition

```rust
pub struct Observation {
    pub entity: String,
    pub properties: BTreeMap<String, String>,
    pub timestamp: u64,
    pub source: ObservationSource,
    pub description: String,  // ADD: Human-readable description
    pub metadata: BTreeMap<String, String>,  // ADD: Additional context
}
```

### Fix Steps

1. **Update struct**:
   ```rust
   pub struct Observation {
       pub entity: String,
       pub properties: BTreeMap<String, String>,
       pub timestamp: u64,
       pub source: ObservationSource,
       pub description: String,
       pub metadata: BTreeMap<String, String>,
   }
   ```

2. **Update constructor**:
   ```rust
   impl Observation {
       pub fn new(
           entity: String,
           properties: BTreeMap<String, String>,
           source: ObservationSource,
       ) -> Self {
           Self {
               entity,
               properties,
               timestamp: std::time::SystemTime::now()
                   .duration_since(std::time::UNIX_EPOCH)
                   .unwrap()
                   .as_secs(),
               source,
               description: String::new(),  // Default empty
               metadata: BTreeMap::new(),  // Default empty
           }
       }
   }
   ```

3. **Add builder methods**:
   ```rust
   impl Observation {
       pub fn with_description(mut self, description: String) -> Self {
           self.description = description;
           self
       }

       pub fn with_metadata(mut self, metadata: BTreeMap<String, String>) -> Self {
           self.metadata = metadata;
           self
       }
   }
   ```

4. **Run verification**:
   ```bash
   cargo make check
   # Expect 18 fewer errors
   ```

### Impact

- **Unblocks**: ValidationEvidence tests, MinerConfig tests
- **Dependencies**: None (independent fix)
- **Risk**: Low (additive change)

---

## 3. ObservationSource Enum Missing Variants

**Error Count**: 16 errors (7.1% of total)
**Severity**: CRITICAL
**Estimated Fix Time**: 10 minutes

### Problem

Tests expect 4 additional enum variants:
- `QueryLog` → 7 E0599 errors
- `UserFeedback` → 1 E0599 error
- `SchemaEvolution` → 1 E0599 error
- `PerformanceMetrics` → 1 E0599 error

Plus additional usage errors.

### Current Definition

```rust
// crates/ggen-core/src/ontology/pattern_miner.rs:123
#[derive(Debug, Clone, Copy)]
pub enum ObservationSource {
    Data,      // From O (observation data)
    Artifact,  // From A (generated artifacts)
    Receipt,   // From operator receipts/logs
}
```

### Expected Definition

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObservationSource {
    Data,      // From O (observation data)
    Artifact,  // From A (generated artifacts)
    Receipt,   // From operator receipts/logs
    QueryLog,  // ADD: From query execution logs
    UserFeedback,  // ADD: From user feedback/issues
    SchemaEvolution,  // ADD: From schema change tracking
    PerformanceMetrics,  // ADD: From performance monitoring
}
```

### Fix Steps

1. **Add variants** to enum:
   ```rust
   #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
   pub enum ObservationSource {
       Data,
       Artifact,
       Receipt,
       QueryLog,
       UserFeedback,
       SchemaEvolution,
       PerformanceMetrics,
   }
   ```

2. **Update match arms** in any code that matches on `ObservationSource`

3. **Add documentation**:
   ```rust
   /// Source of observation data for pattern mining
   #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
   pub enum ObservationSource {
       /// From ontology data (O in MAPE-K)
       Data,
       /// From generated artifacts (A in MAPE-K)
       Artifact,
       /// From operator receipts/logs
       Receipt,
       /// From query execution logs (performance analysis)
       QueryLog,
       /// From user feedback and issue reports
       UserFeedback,
       /// From schema evolution tracking
       SchemaEvolution,
       /// From performance metrics collection
       PerformanceMetrics,
   }
   ```

4. **Run verification**:
   ```bash
   cargo make check
   # Expect 16 fewer errors
   ```

### Impact

- **Unblocks**: Pattern mining tests, observation logging
- **Dependencies**: None (independent fix)
- **Risk**: Low (additive change, existing variants unchanged)

---

## 4. ValidationContext Struct Missing Fields

**Error Count**: 10 errors (4.4% of total)
**Severity**: HIGH
**Estimated Fix Time**: 15 minutes

### Problem

Tests expect fields that don't exist:
- `snapshot_id: String` → 5 E0560 errors
- `current_stats: OntologyStats` → 5 E0560 errors

### Current Definition (Need to Locate)

Search for `pub struct ValidationContext` in `ggen-core`.

### Expected Definition

```rust
pub struct ValidationContext {
    pub snapshot_id: String,  // ADD: ID of snapshot being validated
    pub current_stats: OntologyStats,  // ADD: Current ontology statistics
    // ... existing fields
}
```

### Fix Steps

1. **Locate struct** in codebase
2. **Add missing fields**
3. **Update constructor/builder**
4. **Run verification**:
   ```bash
   cargo make check
   # Expect 10 fewer errors
   ```

### Impact

- **Depends on**: Observation struct (validation uses observations)
- **Unblocks**: ConstitutionValidation tests
- **Risk**: Medium (may affect validation logic)

---

## 5. ValidationEvidence Struct Missing Fields

**Error Count**: 9 errors (4% of total)
**Severity**: HIGH
**Estimated Fix Time**: 15 minutes

### Problem

Tests expect fields:
- `severity: Severity` → 3 E0560 + 3 E0609 errors
- `location: String` → 3 E0560 errors
- `description: String` → 3 E0560 errors

### Expected Definition

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Severity {
    Info,
    Warning,
    Error,
    Critical,
}

pub struct ValidationEvidence {
    pub severity: Severity,  // ADD: Severity level
    pub location: String,  // ADD: Where in ontology
    pub description: String,  // ADD: What was found
    // ... existing fields
}
```

### Fix Steps

1. **Define Severity enum**
2. **Add fields to struct**
3. **Update validator code**
4. **Run verification**

### Impact

- **Depends on**: Observation struct
- **Unblocks**: Validator validate() methods
- **Risk**: Medium (core validation type)

---

## 6. PatternType Enum Missing Drift Variant

**Error Count**: 8 errors (3.6% of total)
**Severity**: HIGH
**Estimated Fix Time**: 5 minutes

### Problem

Tests expect `Drift` variant:
- `PatternType::Drift` → 4 E0599 errors
- Additional usage errors

### Current Definition

```rust
// crates/ggen-core/src/ontology/pattern_miner.rs:37
pub enum PatternType {
    RepeatedStructure,
    RepeatedProperty,
    SchemaMismatch,
    // Missing: Drift
}
```

### Expected Definition

```rust
pub enum PatternType {
    RepeatedStructure,
    RepeatedProperty,
    SchemaMismatch,
    Drift,  // ADD: Schema drift detection
}
```

### Fix Steps

1. **Add variant**:
   ```rust
   pub enum PatternType {
       RepeatedStructure,
       RepeatedProperty,
       SchemaMismatch,
       Drift,
   }
   ```

2. **Update match arms** in pattern analysis code

3. **Run verification**

### Impact

- **Depends on**: ObservationSource enum
- **Unblocks**: Pattern analysis tests
- **Risk**: Low (simple additive change)

---

## 7. ProposerConfig Struct Missing Fields

**Error Count**: 6 errors (2.7% of total)
**Severity**: HIGH
**Estimated Fix Time**: 10 minutes

### Problem

Tests expect:
- `max_proposals_per_iteration: usize` → 3 E0560 + 3 E0609 errors
- `llm_endpoint: String` → 3 E0560 errors (likely)

### Expected Definition

```rust
pub struct ProposerConfig {
    pub max_proposals_per_iteration: usize,  // ADD: Limit proposals
    pub llm_endpoint: Option<String>,  // ADD: LLM API endpoint
    // ... existing fields
}
```

### Fix Steps

1. **Add fields**
2. **Update constructor with defaults**:
   ```rust
   impl Default for ProposerConfig {
       fn default() -> Self {
           Self {
               max_proposals_per_iteration: 10,
               llm_endpoint: None,
               // ... existing defaults
           }
       }
   }
   ```

3. **Run verification**

### Impact

- **Depends on**: DeltaSigmaProposal struct
- **Unblocks**: Proposal generation tests
- **Risk**: Low (config change)

---

## 8. ProposedChange Struct Missing Fields

**Error Count**: 6 errors (2.7% of total)
**Severity**: HIGH
**Estimated Fix Time**: 10 minutes

### Problem

Tests expect:
- `risk_level: RiskLevel` → 3 E0560 + 3 E0609 errors
- `description: String` → 3 E0560 errors (likely)

### Expected Definition

```rust
pub struct ProposedChange {
    pub risk_level: RiskLevel,  // ADD: Risk assessment
    pub description: String,  // ADD: Change description
    // ... existing fields
}
```

### Fix Steps

1. **Reuse RiskLevel enum** from DeltaSigmaProposal
2. **Add fields to struct**
3. **Run verification**

### Impact

- **Depends on**: DeltaSigmaProposal struct (reuses RiskLevel)
- **Unblocks**: Change tracking tests
- **Risk**: Low

---

## 9. Missing validate() Methods on Validators

**Error Count**: 9 errors (4% of total)
**Severity**: MEDIUM
**Estimated Fix Time**: 30 minutes

### Problem

Tests call `.validate()` on validator structs, but method doesn't exist:
- `TypeSoundnessCheck::validate()` → E0599
- `SLOPreservationCheck::validate()` → E0599
- `ProjectionDeterminismCheck::validate()` → E0599
- `NoRetrocausationCheck::validate()` → E0599
- `ImmutabilityCheck::validate()` → E0599
- `GuardSoundnessCheck::validate()` → E0599
- `CompositeValidator::validate()` → E0599
- `AtomicPromotionCheck::validate()` → E0599

### Expected API

```rust
pub trait Validator {
    fn validate(&self, context: &ValidationContext) -> Result<ValidationEvidence>;
}

impl Validator for TypeSoundnessCheck {
    fn validate(&self, context: &ValidationContext) -> Result<ValidationEvidence> {
        // Implementation
        Ok(ValidationEvidence {
            severity: Severity::Info,
            location: "global".to_string(),
            description: "Type soundness check passed".to_string(),
        })
    }
}

// Repeat for all 8 validators
```

### Fix Steps

1. **Define Validator trait**
2. **Implement for each validator struct**
3. **Use ValidationContext and return ValidationEvidence**
4. **Run verification**

### Impact

- **Depends on**: ValidationContext, ValidationEvidence structs
- **Unblocks**: All validator tests
- **Risk**: Medium (core validation logic)

---

## 10. Pipeline Struct Private Field Access

**Error Count**: 12 errors (5.3% of total)
**Severity**: MEDIUM
**Estimated Fix Time**: 5 minutes

### Problem

Tests try to access `pipeline.tera` but field is private:
- `field 'tera' of struct 'Pipeline' is private` → 12 E0616 errors

### Current Definition

```rust
pub struct Pipeline {
    tera: Tera,  // Private!
}
```

### Fix Options

**Option 1**: Make field public (fastest):
```rust
pub struct Pipeline {
    pub tera: Tera,  // Now public
}
```

**Option 2**: Add getter method (better encapsulation):
```rust
impl Pipeline {
    pub fn tera(&self) -> &Tera {
        &self.tera
    }

    pub fn tera_mut(&mut self) -> &mut Tera {
        &mut self.tera
    }
}
```

### Recommended Fix

Use **Option 1** for speed - make field public. This is a testing/internal API, not production library surface.

### Fix Steps

1. **Make field public**: `pub tera: Tera`
2. **Run verification**:
   ```bash
   cargo make check
   # Expect 12 fewer errors
   ```

### Impact

- **Dependencies**: None (independent)
- **Unblocks**: Template rendering tests
- **Risk**: Very Low (internal API)

---

## Summary: Top 10 Fixes

| Priority | Error | Count | Time | Impact |
|----------|-------|-------|------|--------|
| 1 | DeltaSigmaProposal | 30 | 20m | 13.3% |
| 2 | Observation | 18 | 15m | 8.0% |
| 3 | ObservationSource | 16 | 10m | 7.1% |
| 4 | ValidationContext | 10 | 15m | 4.4% |
| 5 | ValidationEvidence | 9 | 15m | 4.0% |
| 6 | PatternType | 8 | 5m | 3.6% |
| 7 | ProposerConfig | 6 | 10m | 2.7% |
| 8 | ProposedChange | 6 | 10m | 2.7% |
| 9 | validate() methods | 9 | 30m | 4.0% |
| 10 | Pipeline visibility | 12 | 5m | 5.3% |
| **TOTAL** | **124** | **2h 15m** | **55.1%** |

**Fix these 10 → Resolve 55% of all errors in ~2 hours**

Add the next 5 fixes (MinerConfig, ConstitutionValidation, PromotionResult, etc.) → **82.7% of errors resolved**

---

**Generated**: 2025-11-20 by Production Validation Specialist
**Status**: TOP 10 IDENTIFIED - READY FOR SYSTEMATIC FIXES
