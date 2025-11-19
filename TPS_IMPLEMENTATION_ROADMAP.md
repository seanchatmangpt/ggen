# TPS Implementation Roadmap for ggen

## Overview
This document outlines a strategic roadmap for implementing Toyota Production System principles into ggen, building on the existing architecture.

## Current TPS Maturity Assessment

| Principle | Current | Target | Priority |
|-----------|---------|--------|----------|
| **JIT (Just-In-Time)** | 70% | 95% | High |
| **Jidoka (Error Detection)** | 85% | 98% | High |
| **Heijunka (Load Leveling)** | 75% | 90% | Medium |
| **Genchi Genbutsu (Verification)** | 80% | 95% | High |
| **Nemawashi (Consensus)** | 70% | 85% | Medium |
| **Hansei (Reflection)** | 65% | 90% | Medium |

---

## 1. JIT (Just-In-Time) Updates - 70% → 95%

### Current State
- ✅ Phase caching with SHA256
- ✅ Lazy RDF loading
- ✅ SPARQL result caching with epoch invalidation
- ❌ No delta-driven regeneration
- ❌ No file-level incremental generation
- ❌ No watch mode

### Phase 1: Delta-Driven Regeneration (2-3 weeks)
**Goal**: Detect changed fields in ontology, regenerate only affected templates

**Implementation**:
```rust
// Extend ggen-core/delta.rs
pub struct FieldDelta {
    class: String,
    property: String,
    change_type: ChangeType,  // Added, Removed, Modified
}

pub struct TemplateSelector {
    // Given a set of FieldDeltas, return affected templates
    fn select(&self, deltas: &[FieldDelta]) -> Vec<PathBuf>;
}
```

**Tasks**:
- [ ] Extend DeltaType to field-level changes
- [ ] Implement TemplateSelector trait
- [ ] Add template metadata (which fields it uses)
- [ ] Integrate into pipeline for selective regeneration
- [ ] Test with large ontologies

**Files to Modify**:
- `crates/ggen-core/src/delta.rs`
- `crates/ggen-core/src/template.rs`
- `crates/ggen-core/src/generator.rs`
- `crates/ggen-domain/src/template/mod.rs`

### Phase 2: Incremental File Generation (3-4 weeks)
**Goal**: Skip writing unchanged files, hash-based invalidation

**Implementation**:
```rust
// New: ggen-core/src/incremental.rs
pub struct FileHash {
    path: PathBuf,
    content_hash: String,  // SHA256
    metadata_hash: String,  // permissions, etc.
}

pub struct IncrementalGenerator {
    // Compare generated content against previous generation
    // Skip writing if content identical
    fn should_generate(&self, new: &str, previous: &FileHash) -> bool;
}
```

**Tasks**:
- [ ] Create file hash store (.ggen/file_hashes.json)
- [ ] Implement content comparison
- [ ] Add stats reporting (X files written, Y skipped)
- [ ] Integrate with Generator
- [ ] Test with multi-file templates

**Files to Create**:
- `crates/ggen-core/src/incremental.rs`

**Files to Modify**:
- `crates/ggen-core/src/generator.rs`
- `crates/ggen-core/src/pipeline.rs`

### Phase 3: Watch Mode (2-3 weeks)
**Goal**: Automatic regeneration on ontology changes

**Implementation**:
```rust
// New: ggen-cli/src/cmds/watch.rs
#[verb]
async fn watch(
    #[arg(long)] ontology: PathBuf,
    #[arg(long)] templates_dir: PathBuf,
) -> NounVerbResult<()> {
    use notify::{Watcher, RecommendedWatcher};
    // Watch for ontology changes
    // Trigger delta-driven regeneration
}
```

**Dependencies**:
- `notify` crate for file watching
- `debounce` for coalescing rapid changes

**Tasks**:
- [ ] Add `notify` dependency to Cargo.toml
- [ ] Implement file watcher
- [ ] Debounce rapid changes (100ms default)
- [ ] Show regeneration status in real-time
- [ ] Handle errors gracefully (malformed RDF, template errors)

**Files to Create**:
- `crates/ggen-cli/src/cmds/watch.rs`

**Files to Modify**:
- `crates/ggen-cli/src/cmds/mod.rs`
- `Cargo.toml` (add notify dependency)

---

## 2. Jidoka (Error Detection) - 85% → 98%

### Current State
- ✅ Compile-time state machine
- ✅ Poka-yoke types (NonEmptyPath, NonEmptyString)
- ✅ Hook validation (circular dependencies)
- ✅ SHACL validation
- ✅ Delta analysis
- ❌ Semantic validation (orphaned references)
- ❌ Cross-language type consistency
- ❌ Runtime invariant assertions
- ❌ Automatic rollback

### Phase 1: Semantic Validation (3-4 weeks)
**Goal**: Detect orphaned references and consistency issues

**Implementation**:
```rust
// New: ggen-core/src/semantic_validator.rs
pub struct SemanticValidator {
    graph: Graph,
}

impl SemanticValidator {
    pub fn validate_references(&self) -> Result<Vec<ValidationIssue>> {
        // Find all rdfs:range references - are they defined?
        // Find all sh:targetClass - do classes exist?
        // Find all object properties - do ranges exist?
    }
    
    pub fn detect_unused_classes(&self) -> Result<Vec<String>> {
        // Classes defined but never instantiated or referenced
    }
    
    pub fn check_cardinality(&self) -> Result<Vec<CardinalityIssue>> {
        // Properties with sh:minCount > actual instance count
    }
}
```

**SPARQL Queries** to implement:
```sparql
# Orphaned range references
SELECT ?property ?range WHERE {
  ?property sh:range ?range .
  FILTER NOT EXISTS { ?range a owl:Class . }
}

# Unused classes
SELECT ?class WHERE {
  ?class a owl:Class .
  FILTER NOT EXISTS { ?instance a ?class . }
  FILTER NOT EXISTS { ?class sh:targetClass . }
}

# Cardinality violations
SELECT ?class ?property ?minCount ?actualCount WHERE {
  ?class a owl:Class .
  ?propertyShape sh:path ?property .
  ?propertyShape sh:minCount ?minCount .
  {
    SELECT (COUNT(*) as ?actualCount)
    WHERE { ?instance a ?class ; ?property ?value . }
  }
  FILTER (?actualCount < ?minCount)
}
```

**Tasks**:
- [ ] Create semantic validator module
- [ ] Implement reference validation
- [ ] Implement unused detection
- [ ] Implement cardinality checking
- [ ] Integrate with `ggen graph validate` command
- [ ] Test with real ontologies

**Files to Create**:
- `crates/ggen-core/src/semantic_validator.rs`

**Files to Modify**:
- `crates/ggen-core/src/lib.rs` (export)
- `crates/ggen-domain/src/graph/mod.rs` (add validation command)
- `crates/ggen-cli/src/cmds/graph.rs` (wire up command)

### Phase 2: Cross-Language Type Validation (3-4 weeks)
**Goal**: Ensure generated types are consistent across languages

**Implementation**:
```rust
// New: ggen-core/src/type_validator.rs
pub struct TypeConsistencyValidator {
    templates: Vec<TemplateMetadata>,
    graph: Graph,
}

impl TypeConsistencyValidator {
    pub fn validate(&self) -> Result<Vec<TypeMismatch>> {
        // For each property in graph:
        //   - Check all templates that generate this property
        //   - Ensure types match (e.g., xsd:integer → i32 in Rust, number in TS)
        //   - Validate generated code compiles
    }
    
    pub fn cross_compile_check(&self, languages: &[Language]) -> Result<()> {
        // Generate all code, attempt to compile in all languages
        // Report failures
    }
}
```

**Type Mapping Reference**:
```
xsd:string     → String (Rust), string (TS), str (Python)
xsd:decimal    → f64 (Rust), number (TS), Decimal (Python)
xsd:integer    → i32 (Rust), number (TS), int (Python)
xsd:boolean    → bool (Rust), boolean (TS), bool (Python)
xsd:dateTime   → DateTime (Rust), Date (TS), datetime (Python)
rdfs:Class     → struct (Rust), interface (TS), class (Python)
```

**Tasks**:
- [ ] Create type mapping store
- [ ] Implement per-language type validation
- [ ] Add compilation check (cargo check, tsc, mypy)
- [ ] Report mismatches clearly
- [ ] Test with multi-language projects

**Files to Create**:
- `crates/ggen-core/src/type_validator.rs`
- `crates/ggen-core/src/type_mapping.rs`

### Phase 3: Runtime Invariant Assertions (2-3 weeks)
**Goal**: Catch invariant violations at generation time

**Implementation**:
```rust
// Extend: ggen-core/src/lifecycle/state_machine.rs
pub struct InvariantAssertion {
    name: String,
    predicate: Box<dyn Fn(&LifecycleState) -> bool>,
    error_message: String,
}

impl InvariantAssertion {
    pub fn check(&self, state: &LifecycleState) -> Result<()> {
        if !(self.predicate)(state) {
            Err(Error::new(&format!("Invariant violated: {}", self.error_message)))
        } else {
            Ok(())
        }
    }
}

// Example assertions
let assertions = vec![
    InvariantAssertion {
        name: "output_paths_exist".to_string(),
        predicate: Box::new(|state| {
            // All output paths must be within project root
            true
        }),
        error_message: "Output paths must be within project".to_string(),
    },
];
```

**Common Invariants**:
- Output paths are within project root
- Generated files are readable/writable
- Phase order is respected
- No circular hooks
- All referenced phases exist
- Ontology valid before generation

**Tasks**:
- [ ] Create invariant assertion framework
- [ ] Define 10+ critical invariants
- [ ] Check invariants after each phase
- [ ] Report violations clearly
- [ ] Test with adversarial inputs

**Files to Modify**:
- `crates/ggen-core/src/lifecycle/state_machine.rs`
- `crates/ggen-core/src/lifecycle/exec.rs`

### Phase 4: Automatic Rollback (2-3 weeks)
**Goal**: Restore previous state on generation failure

**Implementation**:
```rust
// Extend: ggen-core/src/snapshot.rs
pub struct RollbackManager {
    checkpoint_dir: PathBuf,
}

impl RollbackManager {
    pub fn create_checkpoint(&self) -> Result<CheckpointId> {
        // Save current state (files, lifecycle_state.json)
    }
    
    pub fn rollback(&self, checkpoint_id: CheckpointId) -> Result<()> {
        // Restore files from checkpoint
        // Restore lifecycle state
    }
}

// Usage in generator
let mut generator = Generator::new(pipeline, ctx);
let checkpoint = generator.create_checkpoint()?;
match generator.generate() {
    Ok(path) => { /* success */ },
    Err(e) => {
        generator.rollback(checkpoint)?;
        return Err(e);
    }
}
```

**Checkpoint Storage**:
- `.ggen/checkpoints/<timestamp>/`
- Compressed backup of generated files
- JSON metadata (what changed)

**Tasks**:
- [ ] Extend snapshot manager with checkpoints
- [ ] Implement checkpoint creation/deletion
- [ ] Integrate with Generator
- [ ] Test rollback on various failures
- [ ] Document checkpointing behavior

**Files to Modify**:
- `crates/ggen-core/src/snapshot.rs`
- `crates/ggen-core/src/generator.rs`

---

## 3. Heijunka (Load Leveling) - 75% → 90%

### Current State
- ✅ Universal lifecycle phases
- ✅ Phase parallelism (workspace-aware)
- ✅ Multi-layer configuration
- ❌ Rate limiting for marketplace
- ❌ Resource quotas
- ❌ Adaptive scheduling

### Phase 1: Marketplace Rate Limiting (2-3 weeks)
**Goal**: Prevent API overload when searching marketplace

**Implementation**:
```rust
// Extend: ggen-marketplace/src/search.rs
pub struct RateLimiter {
    requests_per_second: u32,
    burst_size: u32,
}

impl RateLimiter {
    pub async fn acquire(&self) -> Result<()> {
        // Token bucket algorithm
        // Wait if necessary
    }
}

// Usage
let limiter = RateLimiter::new(100, 10);  // 100 req/s, burst of 10
limiter.acquire().await?;
registry.search(query).await?;
```

**Tasks**:
- [ ] Add rate limiter to search engine
- [ ] Make limits configurable (env vars)
- [ ] Add metrics for API usage
- [ ] Test with concurrent searches
- [ ] Document rate limiting

**Files to Create**:
- `crates/ggen-marketplace/src/rate_limiter.rs`

**Files to Modify**:
- `crates/ggen-marketplace/src/search/mod.rs`
- `crates/ggen-marketplace/src/lib.rs`

### Phase 2: Resource Quotas (2-3 weeks)
**Goal**: Limit memory/disk/time for large ontologies

**Implementation**:
```rust
// New: ggen-core/src/resource_limits.rs
pub struct ResourceQuotas {
    max_memory_mb: u64,
    max_disk_mb: u64,
    max_generation_time_secs: u64,
}

impl ResourceQuotas {
    pub fn check_memory(&self) -> Result<()> {
        // Check current process memory
    }
    
    pub fn check_disk(&self, path: &Path) -> Result<()> {
        // Check available disk space
    }
    
    pub fn check_time_budget(&self, elapsed: Duration) -> Result<()> {
        // Check elapsed time
    }
}
```

**Configuration**:
```toml
[resource_limits]
max_memory_mb = 2048
max_disk_mb = 5000
max_generation_time_secs = 60
```

**Tasks**:
- [ ] Create resource limits module
- [ ] Implement memory checking (via procfs/Windows API)
- [ ] Implement disk checking
- [ ] Implement timeout checking
- [ ] Integrate with Generator
- [ ] Add status reporting during generation

**Files to Create**:
- `crates/ggen-core/src/resource_limits.rs`

### Phase 3: Adaptive Scheduling (3-4 weeks)
**Goal**: Schedule phases based on system load

**Implementation**:
```rust
// New: ggen-core/src/adaptive_scheduler.rs
pub struct AdaptiveScheduler {
    phases: Vec<Phase>,
    system_monitor: SystemMonitor,
}

impl AdaptiveScheduler {
    pub fn schedule(&self) -> Vec<ScheduleEntry> {
        // Consider:
        // - CPU load
        // - Memory available
        // - Disk I/O patterns
        // - Phase dependencies
        // Optimize for throughput and responsiveness
    }
}
```

**Heuristics**:
- If CPU > 80%, skip parallelization
- If memory < 512MB, increase garbage collection
- If disk queue > 10, throttle I/O
- Run quick phases during high load

**Tasks**:
- [ ] Create system monitor
- [ ] Implement load detection
- [ ] Create scheduling algorithm
- [ ] Test on various system loads
- [ ] Add metrics reporting

**Files to Create**:
- `crates/ggen-core/src/adaptive_scheduler.rs`
- `crates/ggen-core/src/system_monitor.rs`

---

## 4. Genchi Genbutsu (Verification) - 80% → 95%

### Current State
- ✅ Dry-run mode
- ✅ Delta tracking
- ✅ Snapshots/baselines
- ✅ Error context
- ✅ Deterministic output
- ❌ Interactive diff viewer
- ❌ Audit trail with actors
- ❌ Change approval workflow
- ❌ Visual diff

### Phase 1: Interactive Diff Viewer (3-4 weeks)
**Goal**: Show before/after changes in terminal

**Implementation**:
```rust
// New: ggen-cli/src/diff_viewer.rs
pub struct DiffViewer {
    old_snapshot: Snapshot,
    new_snapshot: Snapshot,
}

impl DiffViewer {
    pub fn show_interactive(&self) -> Result<()> {
        // Display files changed
        // Show line-by-line diffs
        // Allow scrolling/filtering
        // Accept/reject individual changes
    }
}
```

**Dependencies**:
- `similar-asserts` or `prettydiff` for diff generation
- `ratatui` or `ncurses` for TUI
- `indicatif` for progress

**Tasks**:
- [ ] Generate file-level diffs
- [ ] Generate line-level diffs
- [ ] Create interactive viewer
- [ ] Add filtering (show only added/removed/modified)
- [ ] Add approval/rejection
- [ ] Test with large diffs

**Files to Create**:
- `crates/ggen-cli/src/diff_viewer.rs`

**Files to Modify**:
- `crates/ggen-cli/src/cmds/template.rs` (add --interactive flag)

### Phase 2: Audit Trail (2-3 weeks)
**Goal**: Track who generated what, when, and why

**Implementation**:
```rust
// New: ggen-core/src/audit.rs
pub struct AuditEntry {
    timestamp: DateTime<Utc>,
    actor: String,  // username or service
    action: AuditAction,
    ontology_version: String,
    template_set: Vec<String>,
    status: ExecutionStatus,
    changes: Vec<FileChange>,
}

pub enum AuditAction {
    Generate { dry_run: bool },
    Regenerate { delta_type: DeltaType },
    Rollback { checkpoint_id: String },
}

pub struct AuditLog {
    entries: Vec<AuditEntry>,
}

impl AuditLog {
    pub fn append(&mut self, entry: AuditEntry) -> Result<()> {
        // Append to .ggen/audit.json
    }
    
    pub fn query(&self, actor: &str) -> Vec<&AuditEntry> {
        // Find entries by actor
    }
}
```

**Storage**:
```json
[
  {
    "timestamp": "2025-01-15T10:30:00Z",
    "actor": "alice@example.com",
    "action": "Generate",
    "ontology_version": "2.1.0",
    "status": "success",
    "changes": [
      {
        "path": "src/models.rs",
        "type": "modified",
        "lines_added": 42,
        "lines_removed": 12
      }
    ]
  }
]
```

**Tasks**:
- [ ] Create audit module
- [ ] Track actor (via $USER or service account)
- [ ] Log all generation activities
- [ ] Implement audit queries
- [ ] Rotate audit logs (keep last 90 days)
- [ ] Test with multiple users

**Files to Create**:
- `crates/ggen-core/src/audit.rs`

### Phase 3: Change Approval Workflow (3-4 weeks)
**Goal**: Require approval before applying changes

**Implementation**:
```rust
// New: ggen-core/src/approval_workflow.rs
pub struct ApprovalRequest {
    id: String,
    generated_files: Vec<FileChange>,
    created_by: String,
    status: ApprovalStatus,
}

pub enum ApprovalStatus {
    Pending,
    Approved { by: String, at: DateTime<Utc> },
    Rejected { by: String, reason: String },
    Expired,
}

pub struct ApprovalWorkflow {
    pending_requests: Vec<ApprovalRequest>,
}

impl ApprovalWorkflow {
    pub async fn request_approval(&mut self, request: ApprovalRequest) -> Result<String> {
        // Store request
        // Send notification
        // Return request ID
    }
    
    pub async fn approve(&mut self, request_id: &str, approver: &str) -> Result<()> {
        // Update status
        // Trigger generation
    }
}
```

**Workflow**:
```
Developer runs: ggen template generate --dry-run
  ↓
Review diff
  ↓
Request approval: ggen template request-approval <diff-id>
  ↓
Reviewer sees: ggen approval list
  ↓
Reviewer approves: ggen approval approve <request-id>
  ↓
Generation applies changes
```

**Integration Points**:
- Slack/email notifications (via webhooks)
- GitHub PR integration (comment with diff)
- CLI-based approval

**Tasks**:
- [ ] Create approval request model
- [ ] Implement request persistence
- [ ] Implement approval logic
- [ ] Add CLI commands (request, list, approve, reject)
- [ ] Add webhook support (optional)
- [ ] Test end-to-end workflow

**Files to Create**:
- `crates/ggen-core/src/approval_workflow.rs`
- `crates/ggen-cli/src/cmds/approval.rs` (new)

---

## 5. Nemawashi (Consensus) - 70% → 85%

### Current State
- ✅ Hook system
- ✅ Trait-based design
- ✅ Marketplace composability
- ✅ Configuration precedence
- ❌ Collaborative ontology editing
- ❌ Comment/annotation system
- ❌ Rollback for disputes
- ❌ Team approval workflows

### Phase 1: Collaborative Ontology Editor (4-5 weeks)
**Goal**: Web UI for team ontology editing

**Technology Stack**:
- Frontend: React + Monaco Editor
- Backend: Web API exposing ggen-domain
- Storage: Git for version control

**Implementation**:
```rust
// New: crates/ggen-web/
// Web API wrapper around ggen-domain
pub struct OntologyEditor {
    graph: Graph,
    git_repo: git2::Repository,
}

impl OntologyEditor {
    pub async fn add_class(
        &mut self, class_name: String, description: String,
    ) -> Result<()> {
        // Add class to graph
        // Commit to git
    }
    
    pub async fn get_history(&self) -> Result<Vec<Commit>> {
        // Get edit history
    }
    
    pub async fn compare(&self, v1: &str, v2: &str) -> Result<Diff> {
        // Show changes between versions
    }
}
```

**Features**:
- [ ] Add/edit/delete classes
- [ ] Add/edit/delete properties
- [ ] Define constraints (SHACL)
- [ ] Version control (git)
- [ ] Comment on changes
- [ ] Live preview of generated code
- [ ] Team collaboration (users, roles)

**Files to Create**:
- `crates/ggen-web/src/lib.rs`
- `crates/ggen-web/src/api/ontology.rs`

### Phase 2: Comment/Annotation System (2-3 weeks)
**Goal**: Allow inline comments on generated code and ontology

**Implementation**:
```rust
// New: ggen-core/src/annotations.rs
pub struct Annotation {
    id: String,
    anchor: AnnotationAnchor,  // File line, or RDF triple
    text: String,
    author: String,
    created_at: DateTime<Utc>,
    resolved: bool,
}

pub enum AnnotationAnchor {
    FileLine { file: PathBuf, line: usize },
    RdfTriple { subject: String, predicate: String, object: String },
}

pub struct AnnotationStore {
    annotations: Vec<Annotation>,
}

impl AnnotationStore {
    pub fn add_annotation(&mut self, annotation: Annotation) -> Result<()> {
        // Store in .ggen/annotations.json
    }
    
    pub fn get_for_file(&self, file: &Path) -> Vec<&Annotation> {
        // Get all annotations for a file
    }
}
```

**Storage** (.ggen/annotations.json):
```json
[
  {
    "id": "ann-001",
    "anchor": {
      "type": "FileLine",
      "file": "src/models.rs",
      "line": 42
    },
    "text": "@alice: This field needs validation",
    "author": "bob@example.com",
    "created_at": "2025-01-15T10:30:00Z",
    "resolved": false
  }
]
```

**Tasks**:
- [ ] Create annotation model
- [ ] Implement persistence
- [ ] Add CLI commands (add, list, resolve)
- [ ] Show annotations during diff viewing
- [ ] Link annotations to issues/PRs (optional)

**Files to Create**:
- `crates/ggen-core/src/annotations.rs`

---

## 6. Hansei (Reflection) - 65% → 90%

### Current State
- ✅ Production readiness tracking
- ✅ Quality metrics (marketplace)
- ✅ Performance profiling
- ✅ E2E testing
- ✅ Version tracking
- ❌ Metrics dashboard
- ❌ Code quality analysis
- ❌ AI suggestions
- ❌ Retrospectives

### Phase 1: Metrics Dashboard (3-4 weeks)
**Goal**: Web dashboard showing generation trends

**Technology Stack**:
- Frontend: React + Recharts
- Backend: Web API (ggen-web)
- Storage: SQLite or InfluxDB

**Implementation**:
```rust
// Extend: crates/ggen-web/
pub struct MetricsCollector {
    db: Database,
}

impl MetricsCollector {
    pub fn record_generation(&self, event: GenerationEvent) -> Result<()> {
        // Record:
        // - Generation time
        // - Files generated
        // - Errors/warnings
        // - Code complexity changes
    }
    
    pub fn get_trends(&self, days: u32) -> Result<Trends> {
        // Get trends over time (generation speed, error rate, etc.)
    }
}

pub struct GenerationEvent {
    timestamp: DateTime<Utc>,
    duration_ms: u64,
    files_generated: usize,
    errors: usize,
    warnings: usize,
    code_lines: usize,
}

pub struct Trends {
    avg_generation_time: Duration,
    avg_files_per_generation: f64,
    error_rate: f64,
    code_growth_rate: f64,
}
```

**Dashboard Widgets**:
- [ ] Generation speed over time (line chart)
- [ ] File count trends (area chart)
- [ ] Error rate (bar chart)
- [ ] Code complexity growth (scatter plot)
- [ ] Most-changed templates (pie chart)
- [ ] Generation failure rate (gauge)

**Files to Create**:
- `crates/ggen-web/src/metrics.rs`
- `crates/ggen-web/ui/pages/Dashboard.tsx`

### Phase 2: Code Quality Analysis (3-4 weeks)
**Goal**: Analyze generated code for complexity, coverage, issues

**Implementation**:
```rust
// New: ggen-core/src/quality_analyzer.rs
pub struct CodeQualityAnalyzer {
    generated_files: Vec<PathBuf>,
}

impl CodeQualityAnalyzer {
    pub fn analyze_rust(&self) -> Result<RustQuality> {
        // Run clippy
        // Calculate cyclomatic complexity
        // Count unsafe blocks
    }
    
    pub fn analyze_typescript(&self) -> Result<TypeScriptQuality> {
        // Run eslint
        // Check type coverage
        // Find any/unknown types
    }
    
    pub fn analyze_python(&self) -> Result<PythonQuality> {
        // Run mypy
        // Calculate test coverage
        // Check type hints
    }
}

pub struct CodeQualityReport {
    language: String,
    complexity_score: f64,  // 0-100
    maintainability: f64,
    test_coverage: f64,
    security_issues: Vec<String>,
    style_violations: Vec<String>,
}
```

**Metrics**:
- Cyclomatic complexity (target: < 10)
- Cognitive complexity (target: < 15)
- Test coverage (target: > 80%)
- Type coverage (target: > 95%)
- Dependency count
- Security vulnerabilities

**Tasks**:
- [ ] Run language-specific linters
- [ ] Calculate complexity metrics
- [ ] Aggregate results
- [ ] Create quality report
- [ ] Compare against previous generation
- [ ] Surface regressions

**Files to Create**:
- `crates/ggen-core/src/quality_analyzer.rs`

### Phase 3: AI-Powered Suggestions (4-5 weeks)
**Goal**: Use AI to suggest ontology improvements

**Implementation**:
```rust
// New: ggen-ai/src/suggestion_engine.rs
pub struct SuggestionEngine {
    client: GenAiClient,
    analytics: CodeQualityAnalyzer,
}

impl SuggestionEngine {
    pub async fn suggest_refactorings(&self) -> Result<Vec<Suggestion>> {
        // Analyze generated code
        // Ask Claude: "How can we improve this generated code?"
        // Extract suggestions
    }
    
    pub async fn suggest_ontology_improvements(&self) -> Result<Vec<Suggestion>> {
        // Analyze ontology usage
        // Ask Claude: "How can we improve this ontology?"
        // Extract suggestions (add missing fields, split classes, etc.)
    }
    
    pub async fn analyze_generation_patterns(&self) -> Result<Patterns> {
        // What patterns appear repeatedly in generated code?
        // Are there common refactoring needs?
    }
}

pub struct Suggestion {
    category: SuggestionCategory,  // Refactoring, Modeling, Performance
    priority: Priority,             // Critical, High, Medium, Low
    description: String,
    example: String,
    estimated_effort: Duration,
}
```

**Integration**:
```
ggen ai suggest-refactorings --generated-dir ./src
  ↓
Analyze code quality
  ↓
Ask Claude for improvements
  ↓
Create issues/PRs with suggestions
```

**Tasks**:
- [ ] Create suggestion engine
- [ ] Implement code analysis prompts
- [ ] Implement ontology improvement prompts
- [ ] Extract suggestions from AI responses
- [ ] Create issues/PRs automatically
- [ ] Track suggestion implementation rate

**Files to Create**:
- `crates/ggen-ai/src/suggestion_engine.rs`

### Phase 4: Retrospectives (2-3 weeks)
**Goal**: Generate team learning reports

**Implementation**:
```rust
// New: crates/ggen-web/src/retrospectives.rs
pub struct RetrospectiveGenerator {
    metrics: MetricsCollector,
    analyzer: CodeQualityAnalyzer,
}

impl RetrospectiveGenerator {
    pub async fn generate_sprint_report(&self, days: u32) -> Result<SprintReport> {
        // Summarize what was generated
        // Highlight successes
        // Identify improvements
        // Suggest action items
    }
}

pub struct SprintReport {
    period: DateRange,
    total_generations: u32,
    total_files_generated: u32,
    avg_generation_time: Duration,
    error_rate: f64,
    top_generated_components: Vec<String>,
    improvements_implemented: Vec<String>,
    improvements_suggested: Vec<String>,
    action_items: Vec<ActionItem>,
}

pub struct ActionItem {
    title: String,
    owner: String,
    due_date: Date,
    priority: Priority,
}
```

**Report Sections**:
- [ ] Executive summary
- [ ] Metrics snapshot
- [ ] What worked well
- [ ] What could improve
- [ ] Action items for next sprint
- [ ] Recommended experiments

**Files to Create**:
- `crates/ggen-web/src/retrospectives.rs`

---

## Implementation Schedule

### Q1 2025 (Jan-Mar)
- **Week 1-3**: JIT Phase 1 (Delta-driven regeneration)
- **Week 4-6**: JIT Phase 2 (Incremental file generation)
- **Week 7-9**: Jidoka Phase 1 (Semantic validation)
- **Week 10-12**: Jidoka Phase 2 (Cross-language type validation)
- **Week 13**: Planning & Review

### Q2 2025 (Apr-Jun)
- **Week 1-3**: JIT Phase 3 (Watch mode)
- **Week 4-6**: Jidoka Phase 3 (Runtime invariant assertions)
- **Week 7-9**: Genchi Phase 1 (Interactive diff viewer)
- **Week 10-12**: Genchi Phase 2 (Audit trail)
- **Week 13**: Planning & Review

### Q3 2025 (Jul-Sep)
- **Week 1-3**: Jidoka Phase 4 (Automatic rollback)
- **Week 4-6**: Genchi Phase 3 (Change approval workflow)
- **Week 7-9**: Heijunka Phase 1-2 (Rate limiting + resource quotas)
- **Week 10-12**: Hansei Phase 1 (Metrics dashboard)
- **Week 13**: Planning & Review

### Q4 2025 (Oct-Dec)
- **Week 1-3**: Nemawashi Phase 1 (Collaborative editor)
- **Week 4-6**: Hansei Phase 2-3 (Code quality analysis + AI suggestions)
- **Week 7-9**: Hansei Phase 4 (Retrospectives)
- **Week 10-12**: Polish, documentation, testing
- **Week 13**: Final release planning

---

## Success Metrics

### Adoption
- [ ] 50% of teams using at least one new TPS feature
- [ ] Average generation time < 2 seconds (maintain)
- [ ] Error recovery rate > 95%

### Quality
- [ ] Generated code complexity < 10 (cyclomatic)
- [ ] Type consistency > 99%
- [ ] Audit trail 100% completeness

### User Satisfaction
- [ ] Team spends 50% less time reviewing generated code (via approval workflow)
- [ ] Ontology changes validated in < 5 seconds
- [ ] 80% of suggested improvements adopted

### System Health
- [ ] Generation failures < 1%
- [ ] Rollback success rate > 99%
- [ ] No data loss incidents

---

## Risk Mitigation

### Technical Risks
- **Large ontology performance**: Mitigate with resource quotas, streaming
- **Multi-language type mismatches**: Mitigate with cross-compile check
- **Circular rollback failures**: Mitigate with checkpoint validation

### Organizational Risks
- **Adoption resistance**: Mitigate with clear documentation, demos, training
- **Data privacy (audit trail)**: Mitigate with encryption, retention policies
- **Approval workflow delays**: Mitigate with 30-min SLA, escalation

---

## Success Story: Toyota Production System Excellence

Upon completion, ggen will exemplify all six TPS principles:

**JIT**: Watch mode regenerates only when ontology changes, using delta-driven selection
**Jidoka**: Invariant assertions and rollback prevent invalid states
**Heijunka**: Adaptive scheduling levels system load  
**Genchi Genbutsu**: Audit trail and diff viewer show all changes
**Nemawashi**: Approval workflow and collaborative editor build consensus
**Hansei**: Metrics dashboard and retrospectives enable continuous improvement

This roadmap transforms ggen from a code generation tool into a **continuous ontology-driven development platform** aligned with Toyota's continuous improvement philosophy.
