# Data Model - Feature 004: Test Quality Audit & Optimization

## Overview

This data model supports comprehensive test quality analysis, value-based scoring, mutation testing integration, and performance budget tracking for the ggen test suite. All entities are designed with Type-First thinking (Constitution V) and stored as JSON in `.ggen/test-metadata/`.

---

## Entity Definitions

### 1. TestCase (Primary Entity)

**Purpose**: Core representation of a single test case with all metadata attributes.

**Rust Type Definition**:
```rust
use serde::{Deserialize, Serialize};
use std::time::Duration;

/// Unique identifier for a test case (module path + test name)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TestId(String);

impl TestId {
    pub fn new(module: &str, name: &str) -> Self {
        Self(format!("{}::{}", module, name))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Test classification by scope and purpose
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TestType {
    Unit,
    Integration,
    EndToEnd,
    Performance,
    Security,
}

/// Criticality weight for test value calculation
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum CriticalityWeight {
    Low,      // 1.0
    Medium,   // 2.0
    High,     // 3.0
    Critical, // 5.0
}

impl CriticalityWeight {
    pub fn multiplier(&self) -> f64 {
        match self {
            Self::Low => 1.0,
            Self::Medium => 2.0,
            Self::High => 3.0,
            Self::Critical => 5.0,
        }
    }
}

/// Primary test case entity with all metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestCase {
    /// Unique test identifier (module::test_name)
    pub id: TestId,

    /// Test classification
    pub test_type: TestType,

    /// Number of times test has failed in historical window (30 days)
    pub failure_count: u32,

    /// Total number of times test has been executed (30 days)
    pub run_count: u32,

    /// Unique code paths covered by this test (lines/blocks)
    pub unique_coverage: u32,

    /// Average execution time in milliseconds
    pub exec_time_ms: u64,

    /// Criticality weight for value scoring
    pub criticality_weight: CriticalityWeight,

    /// Timestamp of last metadata update (ISO 8601)
    pub last_updated: String,
}

impl TestCase {
    /// Calculate failure frequency ratio (0.0 to 1.0)
    pub fn failure_frequency(&self) -> f64 {
        if self.run_count == 0 {
            0.0
        } else {
            self.failure_count as f64 / self.run_count as f64
        }
    }

    /// Get execution time as Duration
    pub fn exec_time(&self) -> Duration {
        Duration::from_millis(self.exec_time_ms)
    }
}
```

**JSON Schema** (`.ggen/test-metadata/test-cases.json`):
```json
{
  "test_id": "ggen_core::graph::store::tests::test_lockfile_upsert",
  "test_type": "unit",
  "failure_count": 3,
  "run_count": 150,
  "unique_coverage": 47,
  "exec_time_ms": 125,
  "criticality_weight": "high",
  "last_updated": "2025-12-11T10:30:00Z"
}
```

---

### 2. TestValueScore (Composite Scoring)

**Purpose**: Multi-dimensional value score for prioritizing test execution and optimization.

**Rust Type Definition**:
```rust
/// Individual scoring components (0.0 to 1.0 normalized)
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct ScoreComponents {
    /// Failure frequency score: Higher = more valuable (frequent failures)
    /// Formula: failure_count / run_count
    pub failure_freq_score: f64,

    /// Coverage score: Higher = more valuable (unique coverage)
    /// Formula: unique_coverage / max_unique_coverage
    pub coverage_score: f64,

    /// Speed score: Higher = more valuable (faster tests preferred)
    /// Formula: 1.0 - (exec_time_ms / max_exec_time_ms)
    pub speed_score: f64,

    /// Criticality score: Higher = more valuable (critical tests)
    /// Formula: criticality_weight.multiplier() / 5.0 (max critical)
    pub criticality_score: f64,
}

/// Budget penalty tracking for performance violations
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct BudgetPenalty {
    /// Performance budget in milliseconds (from test metadata)
    pub budget_ms: u64,

    /// Actual execution time in milliseconds
    pub actual_ms: u64,

    /// Excess time over budget (0 if within budget)
    pub excess_ms: u64,

    /// Penalty multiplier applied to composite score (0.0 to 1.0)
    /// Formula: 1.0 - min(1.0, excess_ms / budget_ms)
    pub penalty_multiplier: f64,
}

impl BudgetPenalty {
    pub fn new(budget_ms: u64, actual_ms: u64) -> Self {
        let excess_ms = actual_ms.saturating_sub(budget_ms);
        let penalty_multiplier = if budget_ms == 0 {
            1.0
        } else {
            1.0 - (excess_ms as f64 / budget_ms as f64).min(1.0)
        };

        Self {
            budget_ms,
            actual_ms,
            excess_ms,
            penalty_multiplier,
        }
    }

    pub fn is_violated(&self) -> bool {
        self.excess_ms > 0
    }
}

/// Complete test value score with composite calculation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestValueScore {
    /// Test identifier
    pub test_id: TestId,

    /// Individual scoring components
    pub components: ScoreComponents,

    /// Budget penalty (if applicable)
    pub budget_penalty: Option<BudgetPenalty>,

    /// Final composite value score (0.0 to 100.0)
    /// Formula: weighted_sum(components) * criticality_weight * budget_penalty
    pub composite_value: f64,

    /// Timestamp of score calculation
    pub calculated_at: String,
}

impl TestValueScore {
    /// Calculate composite value score
    /// Weights: failure_freq=30%, coverage=30%, speed=20%, criticality=20%
    pub fn calculate_composite(
        components: ScoreComponents,
        criticality_weight: CriticalityWeight,
        budget_penalty: Option<BudgetPenalty>,
    ) -> f64 {
        let weighted_sum =
            components.failure_freq_score * 0.30 +
            components.coverage_score * 0.30 +
            components.speed_score * 0.20 +
            components.criticality_score * 0.20;

        let criticality_mult = criticality_weight.multiplier();
        let budget_mult = budget_penalty.map(|p| p.penalty_multiplier).unwrap_or(1.0);

        weighted_sum * criticality_mult * budget_mult * 100.0
    }
}
```

**JSON Schema** (`.ggen/test-metadata/test-value-scores.json`):
```json
{
  "test_id": "ggen_core::graph::store::tests::test_lockfile_upsert",
  "components": {
    "failure_freq_score": 0.02,
    "coverage_score": 0.85,
    "speed_score": 0.92,
    "criticality_score": 0.60
  },
  "budget_penalty": {
    "budget_ms": 100,
    "actual_ms": 125,
    "excess_ms": 25,
    "penalty_multiplier": 0.75
  },
  "composite_value": 68.5,
  "calculated_at": "2025-12-11T10:30:00Z"
}
```

---

### 3. MutationResult (cargo-mutants Integration)

**Purpose**: Mutation testing results for assessing test effectiveness.

**Rust Type Definition**:
```rust
/// Mutation testing outcome for a single mutation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum MutationOutcome {
    Killed,   // Test caught the mutation
    Survived, // Test did not catch the mutation (quality gap)
    Timeout,  // Mutation caused timeout
    Skipped,  // Mutation was skipped
}

/// Individual mutation record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Mutation {
    /// Mutation identifier from cargo-mutants
    pub mutation_id: String,

    /// Source location (file:line:col)
    pub location: String,

    /// Type of mutation (e.g., "replace + with -")
    pub mutation_type: String,

    /// Outcome of mutation test
    pub outcome: MutationOutcome,

    /// Test(s) that killed this mutation (if killed)
    pub killed_by: Vec<TestId>,
}

/// Mutation testing results for a module or crate
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MutationResult {
    /// Module or crate path
    pub target: String,

    /// Total mutations introduced by cargo-mutants
    pub mutations_introduced: u32,

    /// Mutations killed by tests
    pub mutations_killed: u32,

    /// Mutations that survived (test quality gap)
    pub mutations_survived: u32,

    /// Mutations that timed out
    pub mutations_timeout: u32,

    /// Kill rate percentage (killed / introduced * 100)
    pub kill_rate: f64,

    /// Individual mutation records
    pub mutations: Vec<Mutation>,

    /// Test cases that participated in killing mutations
    pub participating_tests: Vec<TestId>,

    /// Timestamp of mutation testing run
    pub run_at: String,
}

impl MutationResult {
    /// Calculate kill rate
    pub fn calculate_kill_rate(&self) -> f64 {
        if self.mutations_introduced == 0 {
            0.0
        } else {
            (self.mutations_killed as f64 / self.mutations_introduced as f64) * 100.0
        }
    }

    /// Get survived mutations (quality gaps)
    pub fn survived_mutations(&self) -> Vec<&Mutation> {
        self.mutations.iter()
            .filter(|m| m.outcome == MutationOutcome::Survived)
            .collect()
    }
}
```

**JSON Schema** (`.ggen/test-metadata/mutation-results.json`):
```json
{
  "target": "ggen_core::graph::store",
  "mutations_introduced": 45,
  "mutations_killed": 38,
  "mutations_survived": 5,
  "mutations_timeout": 2,
  "kill_rate": 84.4,
  "mutations": [
    {
      "mutation_id": "mut_001",
      "location": "crates/ggen-core/src/graph/store.rs:123:15",
      "mutation_type": "replace + with -",
      "outcome": "killed",
      "killed_by": ["ggen_core::graph::store::tests::test_lockfile_upsert"]
    }
  ],
  "participating_tests": [
    "ggen_core::graph::store::tests::test_lockfile_upsert",
    "ggen_core::graph::store::tests::test_cache_invalidation"
  ],
  "run_at": "2025-12-11T10:30:00Z"
}
```

---

### 4. TestMetadata (Historical Data)

**Purpose**: 30-day rolling window of test execution history and failure patterns.

**Rust Type Definition**:
```rust
use std::collections::VecDeque;

/// Single test execution record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestExecution {
    /// Execution timestamp (ISO 8601)
    pub timestamp: String,

    /// Execution outcome
    pub passed: bool,

    /// Execution time in milliseconds
    pub exec_time_ms: u64,

    /// Git commit hash (for correlation)
    pub commit_hash: String,

    /// CI/local execution context
    pub execution_context: ExecutionContext,
}

/// Execution context (CI vs local)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ExecutionContext {
    CI,
    Local,
}

/// Failure pattern analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailurePattern {
    /// Most recent failure date (ISO 8601)
    pub last_failure_date: String,

    /// Failure streak (consecutive failures)
    pub consecutive_failures: u32,

    /// Most common failure reason (if available)
    pub common_failure_reason: Option<String>,

    /// Flakiness indicator (failures without code changes)
    pub is_flaky: bool,
}

/// Historical test metadata (30-day rolling window)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestMetadata {
    /// Test identifier
    pub test_id: TestId,

    /// Execution history (max 30 days, FIFO)
    /// Limited to ~1000 entries for performance
    pub execution_history: VecDeque<TestExecution>,

    /// Failure pattern analysis
    pub failure_pattern: Option<FailurePattern>,

    /// First recorded execution date
    pub first_seen: String,

    /// Last execution date
    pub last_executed: String,

    /// Total executions in 30-day window
    pub total_executions: u32,

    /// Total failures in 30-day window
    pub total_failures: u32,
}

impl TestMetadata {
    /// Add new execution record (maintains 30-day window)
    pub fn add_execution(&mut self, execution: TestExecution) {
        // Remove executions older than 30 days
        let cutoff_date = chrono::Utc::now() - chrono::Duration::days(30);
        while let Some(oldest) = self.execution_history.front() {
            if chrono::DateTime::parse_from_rfc3339(&oldest.timestamp)
                .ok()
                .map(|dt| dt.with_timezone(&chrono::Utc) < cutoff_date)
                .unwrap_or(false)
            {
                self.execution_history.pop_front();
            } else {
                break;
            }
        }

        // Add new execution
        self.execution_history.push_back(execution);
        self.total_executions = self.execution_history.len() as u32;
        self.total_failures = self.execution_history.iter()
            .filter(|e| !e.passed)
            .count() as u32;
    }

    /// Detect flakiness (failures without code changes)
    pub fn detect_flakiness(&self) -> bool {
        let mut last_commit = None;
        let mut failures_same_commit = 0;

        for exec in self.execution_history.iter().rev() {
            if !exec.passed {
                if Some(&exec.commit_hash) == last_commit.as_ref() {
                    failures_same_commit += 1;
                }
                last_commit = Some(&exec.commit_hash);
            }
        }

        failures_same_commit >= 2
    }
}
```

**JSON Schema** (`.ggen/test-metadata/test-history.json`):
```json
{
  "test_id": "ggen_core::graph::store::tests::test_lockfile_upsert",
  "execution_history": [
    {
      "timestamp": "2025-12-11T10:30:00Z",
      "passed": true,
      "exec_time_ms": 125,
      "commit_hash": "8997106b",
      "execution_context": "ci"
    }
  ],
  "failure_pattern": {
    "last_failure_date": "2025-12-09T14:22:00Z",
    "consecutive_failures": 0,
    "common_failure_reason": "assertion failed: cache.len() == 1",
    "is_flaky": false
  },
  "first_seen": "2025-11-11T10:00:00Z",
  "last_executed": "2025-12-11T10:30:00Z",
  "total_executions": 150,
  "total_failures": 3
}
```

---

### 5. BudgetViolation (Performance Tracking)

**Purpose**: Track tests exceeding performance budgets for optimization prioritization.

**Rust Type Definition**:
```rust
/// Severity level for budget violations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ViolationSeverity {
    Minor,    // 1-25% over budget
    Moderate, // 26-50% over budget
    Major,    // 51-100% over budget
    Critical, // >100% over budget
}

impl ViolationSeverity {
    pub fn from_excess_percentage(excess_pct: f64) -> Self {
        if excess_pct <= 25.0 {
            Self::Minor
        } else if excess_pct <= 50.0 {
            Self::Moderate
        } else if excess_pct <= 100.0 {
            Self::Major
        } else {
            Self::Critical
        }
    }
}

/// Performance budget violation record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BudgetViolation {
    /// Test identifier
    pub test_id: TestId,

    /// Actual execution time in milliseconds
    pub exec_time_ms: u64,

    /// Performance budget in milliseconds
    pub budget_ms: u64,

    /// Excess time over budget
    pub excess_ms: u64,

    /// Excess percentage (excess / budget * 100)
    pub excess_percentage: f64,

    /// Violation severity
    pub severity: ViolationSeverity,

    /// Timestamp of violation detection
    pub detected_at: String,

    /// Number of consecutive violations (indicates trend)
    pub consecutive_violations: u32,
}

impl BudgetViolation {
    pub fn new(
        test_id: TestId,
        exec_time_ms: u64,
        budget_ms: u64,
        detected_at: String,
    ) -> Option<Self> {
        if exec_time_ms <= budget_ms {
            return None; // No violation
        }

        let excess_ms = exec_time_ms - budget_ms;
        let excess_percentage = (excess_ms as f64 / budget_ms as f64) * 100.0;
        let severity = ViolationSeverity::from_excess_percentage(excess_percentage);

        Some(Self {
            test_id,
            exec_time_ms,
            budget_ms,
            excess_ms,
            excess_percentage,
            severity,
            detected_at,
            consecutive_violations: 1,
        })
    }
}

/// Performance budget violation tracking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BudgetViolationLog {
    /// All violations in current window
    pub violations: Vec<BudgetViolation>,

    /// Summary statistics
    pub summary: BudgetSummary,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BudgetSummary {
    pub total_tests_tracked: u32,
    pub total_violations: u32,
    pub minor_violations: u32,
    pub moderate_violations: u32,
    pub major_violations: u32,
    pub critical_violations: u32,
    pub compliance_rate: f64, // (tracked - violations) / tracked * 100
}
```

**JSON Schema** (`.ggen/test-metadata/budget-violations.json`):
```json
{
  "violations": [
    {
      "test_id": "ggen_core::graph::store::tests::test_lockfile_upsert",
      "exec_time_ms": 125,
      "budget_ms": 100,
      "excess_ms": 25,
      "excess_percentage": 25.0,
      "severity": "minor",
      "detected_at": "2025-12-11T10:30:00Z",
      "consecutive_violations": 2
    }
  ],
  "summary": {
    "total_tests_tracked": 150,
    "total_violations": 12,
    "minor_violations": 8,
    "moderate_violations": 3,
    "major_violations": 1,
    "critical_violations": 0,
    "compliance_rate": 92.0
  }
}
```

---

## Entity Relationships

### Relationship Diagram

```
TestCase (1) ←──→ (0..1) TestMetadata
    │                        │
    │                        └── execution_history: Vec<TestExecution>
    │                        └── failure_pattern: Option<FailurePattern>
    │
    ├──→ (1) TestValueScore
    │        │
    │        ├── components: ScoreComponents
    │        └── budget_penalty: Option<BudgetPenalty>
    │
    ├──→ (0..1) BudgetViolation
    │
    └──→ (0..*) MutationResult (via participating_tests)
             │
             └── mutations: Vec<Mutation>
                      └── killed_by: Vec<TestId>
```

### Relationship Types

1. **TestCase → TestMetadata** (1:0..1)
   - Each TestCase MAY have historical metadata
   - Metadata tracks 30-day execution window
   - Join key: `test_id`

2. **TestCase → TestValueScore** (1:1)
   - Each TestCase MUST have a value score
   - Score calculated from TestCase attributes + TestMetadata
   - Join key: `test_id`

3. **TestCase → BudgetViolation** (1:0..1)
   - Each TestCase MAY have budget violations
   - Violation created when exec_time > budget
   - Join key: `test_id`

4. **TestCase → MutationResult** (N:M via participating_tests)
   - Multiple tests can kill same mutation
   - Multiple mutations can be killed by same test
   - Join keys: `test_id` in `participating_tests`, `killed_by`

5. **TestValueScore → BudgetPenalty** (1:0..1)
   - Score MAY include budget penalty
   - Penalty applied if budget violated
   - Embedded relationship (not separate entity)

---

## Storage Format

### Directory Structure

```
.ggen/test-metadata/
├── test-cases.json           # All TestCase entities (array)
├── test-value-scores.json    # All TestValueScore entities (array)
├── test-history.json         # All TestMetadata entities (array)
├── mutation-results.json     # All MutationResult entities (array)
├── budget-violations.json    # BudgetViolationLog entity
└── schema-version.json       # Schema version for migrations
```

### Schema Version

```json
{
  "version": "1.0.0",
  "created_at": "2025-12-11T10:00:00Z",
  "last_updated": "2025-12-11T10:30:00Z"
}
```

### File Format Guidelines

1. **Encoding**: UTF-8
2. **Format**: Pretty-printed JSON (4-space indent)
3. **Arrays**: Top-level array for collections
4. **Atomicity**: Write to `.tmp` then rename for atomic updates
5. **Backup**: Keep `.backup` copy before updates
6. **Size Limits**:
   - `test-cases.json`: Max 10MB (~50K tests)
   - `test-history.json`: Max 50MB (30-day window, ~1000 executions/test)
   - `mutation-results.json`: Max 20MB (~100K mutations)

---

## Type Safety Guarantees

### Newtypes (Prevent Primitive Obsession)

```rust
// ✅ CORRECT: Type-safe identifier
pub struct TestId(String);

// ❌ WRONG: Primitive obsession
pub type TestId = String;
```

### Enums (Exhaustive Matching)

```rust
// ✅ CORRECT: Compiler-enforced exhaustiveness
match test_type {
    TestType::Unit => { /* ... */ },
    TestType::Integration => { /* ... */ },
    TestType::EndToEnd => { /* ... */ },
    TestType::Performance => { /* ... */ },
    TestType::Security => { /* ... */ },
    // Compiler error if any variant missing
}
```

### Result Types (Error Handling)

```rust
// ✅ CORRECT: Explicit error handling
pub fn load_test_cases() -> Result<Vec<TestCase>, MetadataError> {
    let file = File::open(".ggen/test-metadata/test-cases.json")?;
    let cases = serde_json::from_reader(file)?;
    Ok(cases)
}

// ❌ WRONG: Unwrap in production code
pub fn load_test_cases() -> Vec<TestCase> {
    let file = File::open(".ggen/test-metadata/test-cases.json").unwrap();
    serde_json::from_reader(file).unwrap()
}
```

### Invariants (Constructor Validation)

```rust
impl BudgetPenalty {
    pub fn new(budget_ms: u64, actual_ms: u64) -> Self {
        // Invariant: excess_ms = max(0, actual - budget)
        let excess_ms = actual_ms.saturating_sub(budget_ms);

        // Invariant: penalty_multiplier ∈ [0.0, 1.0]
        let penalty_multiplier = if budget_ms == 0 {
            1.0
        } else {
            1.0 - (excess_ms as f64 / budget_ms as f64).min(1.0)
        };

        Self { budget_ms, actual_ms, excess_ms, penalty_multiplier }
    }
}
```

---

## Data Integrity Constraints

### 1. Referential Integrity

- All `TestId` references MUST exist in `test-cases.json`
- All `test_id` in `test-value-scores.json` MUST exist in `test-cases.json`
- All `test_id` in `budget-violations.json` MUST exist in `test-cases.json`
- All `TestId` in `participating_tests` MUST exist in `test-cases.json`

### 2. Value Constraints

- `failure_count` ≤ `run_count` (cannot fail more than ran)
- `exec_time_ms` ≥ 0 (no negative time)
- `kill_rate` ∈ [0.0, 100.0] (percentage bounds)
- `composite_value` ∈ [0.0, 100.0] (normalized score)
- `penalty_multiplier` ∈ [0.0, 1.0] (bounded multiplier)

### 3. Temporal Constraints

- `last_updated` ≥ `first_seen` (cannot update before creation)
- `last_executed` ≥ `first_seen` (cannot execute before creation)
- `execution_history` sorted chronologically (oldest first)
- 30-day window enforced (remove executions older than cutoff)

### 4. Uniqueness Constraints

- `test_id` UNIQUE in `test-cases.json` (primary key)
- `test_id` UNIQUE in `test-value-scores.json` (1:1 relationship)
- `mutation_id` UNIQUE within `MutationResult.mutations` array

---

## Migration Strategy

### Version 1.0.0 → 1.1.0 (Example)

```rust
pub fn migrate_v1_0_to_v1_1(data: &mut TestMetadata) -> Result<(), MetadataError> {
    // Add new field with default value
    if data.failure_pattern.is_none() {
        data.failure_pattern = Some(FailurePattern {
            last_failure_date: data.last_executed.clone(),
            consecutive_failures: 0,
            common_failure_reason: None,
            is_flaky: false,
        });
    }
    Ok(())
}
```

### Schema Evolution Rules

1. **Additive Changes**: Add optional fields (backward compatible)
2. **Renaming**: Deprecate old field, add new field, migrate data
3. **Removal**: Mark deprecated, migrate away, remove in next major version
4. **Breaking Changes**: Increment major version, provide migration tool

---

## Performance Considerations

### Indexing Strategy

```rust
use std::collections::HashMap;

/// In-memory index for fast lookups
pub struct TestMetadataIndex {
    test_cases: HashMap<TestId, TestCase>,
    value_scores: HashMap<TestId, TestValueScore>,
    history: HashMap<TestId, TestMetadata>,
}

impl TestMetadataIndex {
    /// O(1) lookup by test_id
    pub fn get_test_case(&self, test_id: &TestId) -> Option<&TestCase> {
        self.test_cases.get(test_id)
    }

    /// O(1) lookup by test_id
    pub fn get_value_score(&self, test_id: &TestId) -> Option<&TestValueScore> {
        self.value_scores.get(test_id)
    }
}
```

### Lazy Loading

```rust
/// Load only what's needed
pub struct LazyTestMetadata {
    test_id: TestId,
    history_loaded: bool,
    history: Option<TestMetadata>,
}

impl LazyTestMetadata {
    pub fn load_history(&mut self) -> Result<&TestMetadata, MetadataError> {
        if !self.history_loaded {
            self.history = Some(load_test_history(&self.test_id)?);
            self.history_loaded = true;
        }
        Ok(self.history.as_ref().unwrap())
    }
}
```

### Incremental Updates

```rust
/// Update single test case without rewriting entire file
pub fn update_test_case(
    test_id: &TestId,
    updater: impl FnOnce(&mut TestCase),
) -> Result<(), MetadataError> {
    let mut cases = load_test_cases()?;

    if let Some(case) = cases.iter_mut().find(|c| c.id == *test_id) {
        updater(case);
        save_test_cases(&cases)?;
    }

    Ok(())
}
```

---

## Security Considerations

### 1. Input Validation

```rust
impl TestId {
    pub fn new(module: &str, name: &str) -> Result<Self, ValidationError> {
        // Prevent path traversal
        if module.contains("..") || name.contains("..") {
            return Err(ValidationError::InvalidPath);
        }

        // Prevent excessively long identifiers
        if module.len() + name.len() > 512 {
            return Err(ValidationError::TooLong);
        }

        Ok(Self(format!("{}::{}", module, name)))
    }
}
```

### 2. File System Safety

```rust
/// Atomic write with backup
pub fn save_test_cases(cases: &[TestCase]) -> Result<(), MetadataError> {
    let path = ".ggen/test-metadata/test-cases.json";
    let tmp_path = format!("{}.tmp", path);
    let backup_path = format!("{}.backup", path);

    // Write to temporary file
    let tmp_file = File::create(&tmp_path)?;
    serde_json::to_writer_pretty(tmp_file, cases)?;

    // Backup existing file
    if Path::new(path).exists() {
        fs::copy(path, &backup_path)?;
    }

    // Atomic rename
    fs::rename(&tmp_path, path)?;

    Ok(())
}
```

### 3. Data Size Limits

```rust
/// Prevent DoS via excessive history
impl TestMetadata {
    const MAX_HISTORY_ENTRIES: usize = 1000;

    pub fn add_execution(&mut self, execution: TestExecution) {
        if self.execution_history.len() >= Self::MAX_HISTORY_ENTRIES {
            self.execution_history.pop_front();
        }
        self.execution_history.push_back(execution);
    }
}
```

---

## Usage Examples

### Example 1: Calculate Test Value Score

```rust
use ggen_test_metadata::*;

fn calculate_test_value(test_id: &TestId) -> Result<TestValueScore, MetadataError> {
    // Load test case and metadata
    let test_case = load_test_case(test_id)?;
    let metadata = load_test_metadata(test_id)?;

    // Calculate score components
    let components = ScoreComponents {
        failure_freq_score: test_case.failure_frequency(),
        coverage_score: test_case.unique_coverage as f64 / max_coverage,
        speed_score: 1.0 - (test_case.exec_time_ms as f64 / max_exec_time),
        criticality_score: test_case.criticality_weight.multiplier() / 5.0,
    };

    // Check budget penalty
    let budget_penalty = BudgetPenalty::new(100, test_case.exec_time_ms);

    // Calculate composite score
    let composite_value = TestValueScore::calculate_composite(
        components,
        test_case.criticality_weight,
        budget_penalty,
    );

    Ok(TestValueScore {
        test_id: test_id.clone(),
        components,
        budget_penalty,
        composite_value,
        calculated_at: chrono::Utc::now().to_rfc3339(),
    })
}
```

### Example 2: Detect Budget Violations

```rust
fn detect_violations(test_cases: &[TestCase]) -> Vec<BudgetViolation> {
    let mut violations = Vec::new();

    for test_case in test_cases {
        let budget_ms = get_budget_for_test(&test_case.id);

        if let Some(violation) = BudgetViolation::new(
            test_case.id.clone(),
            test_case.exec_time_ms,
            budget_ms,
            chrono::Utc::now().to_rfc3339(),
        ) {
            violations.push(violation);
        }
    }

    violations
}
```

### Example 3: Analyze Mutation Results

```rust
fn analyze_mutation_gaps(result: &MutationResult) -> Vec<TestId> {
    // Find tests that need improvement (didn't kill survived mutations)
    let survived = result.survived_mutations();

    let mut weak_tests = Vec::new();
    for mutation in survived {
        // Tests that should have killed this mutation but didn't
        // (based on coverage overlap)
        let should_kill = find_tests_covering_location(&mutation.location);
        weak_tests.extend(should_kill);
    }

    weak_tests
}
```

---

## Summary

This data model provides:

1. **Type Safety**: Newtypes, enums, Result types prevent common errors
2. **Referential Integrity**: Validated relationships between entities
3. **Performance**: Indexed lookups, lazy loading, incremental updates
4. **Security**: Input validation, atomic writes, size limits
5. **Extensibility**: Migration strategy, versioned schema
6. **Observability**: Historical tracking, trend analysis, quality scoring

All entities align with Constitution V (Type-First Thinking) and support the goals of Feature 004 test quality optimization.
