# Intent-Driven Architecture: GGEN Refactoring Guide

**Document Purpose**: This document describes what each component SHOULD DO (intent/purpose), not what it currently does. Use this as guidance for refactoring to align implementation with architectural intent.

**Last Updated**: 2025-10-10
**Target Audience**: Developers refactoring and improving the GGEN codebase

---

## Table of Contents

<!-- toc -->

- [1. Autonomous Graph Evolution System](#1-autonomous-graph-evolution-system)
  * [1.1 CLI Command: `autonomous.rs`](#11-cli-command-autonomousrs)
  * [1.2 Orchestrator: `orchestrator.rs`](#12-orchestrator-orchestratorrs)
  * [1.3 Deployment Automation: `deployment.rs`](#13-deployment-automation-deploymentrs)
  * [1.4 Delta Detection: `delta_detector.rs`](#14-delta-detection-delta_detectorrs)
  * [1.5 Event System: `events.rs`](#15-event-system-eventsrs)
  * [1.6 Validator: `validator.rs`](#16-validator-validatorrs)
- [2. AI-Powered Generation System](#2-ai-powered-generation-system)
  * [2.1 Ontology Generator: `ontology.rs`](#21-ontology-generator-ontologyrs)
- [3. Marketplace System](#3-marketplace-system)
  * [3.1 Package Search: `market/search.rs`](#31-package-search-marketsearchrs)
  * [3.2 Package Listing: `market/list.rs`](#32-package-listing-marketlistrs)
  * [3.3 Category Browser: `market/categories.rs`](#33-category-browser-marketcategoriesrs)
- [4. Swarm & Agent Management](#4-swarm--agent-management)
  * [4.1 Swarm CLI: `swarm.rs`](#41-swarm-cli-swarmrs)
- [5. Shell Integration](#5-shell-integration)
  * [5.1 Shell Initialization: `shell/init.rs`](#51-shell-initialization-shellinitrs)
- [6. Development Workflow Integration](#6-development-workflow-integration)
  * [6.1 TOC Validation Workflow: `.github/workflows/toc.yml`](#61-toc-validation-workflow-githubworkflowstocyml)
  * [6.2 Pre-Commit TOC Hook: `scripts/git-hooks/pre-commit-toc`](#62-pre-commit-toc-hook-scriptsgit-hookspre-commit-toc)
- [7. Cross-Cutting Architectural Concerns](#7-cross-cutting-architectural-concerns)
  * [7.1 Error Handling Strategy](#71-error-handling-strategy)
  * [7.2 Testing Strategy](#72-testing-strategy)
  * [7.3 Dependency Injection](#73-dependency-injection)
  * [7.4 Logging vs. User Output](#74-logging-vs-user-output)
- [8. Refactoring Priorities](#8-refactoring-priorities)
- [9. Technical Debt Registry](#9-technical-debt-registry)

<!-- tocstop -->

---

## 1. Autonomous Graph Evolution System

The autonomous system should enable self-evolving knowledge graphs through AI-powered analysis, validation, and deployment. It represents the core innovation of GGEN: graphs that update themselves based on natural language requirements.

### 1.1 CLI Command: `autonomous.rs`

**File**: `cli/src/cmds/autonomous.rs`

#### PURPOSE
Provide a human-friendly command-line interface for autonomous graph evolution that translates user intent into graph operations without requiring deep RDF knowledge.

#### RESPONSIBILITY
- **Accept natural language requirements** and translate them to graph operations
- **Orchestrate the evolution pipeline**: parsing → validation → delta detection → regeneration → deployment
- **Manage governance workflows**: approvals, rollbacks, snapshots
- **Report progress and results** in human-readable formats (text/JSON)
- **Handle errors gracefully** with informative messages and automatic rollback

#### CONSTRAINTS
- MUST NOT contain business logic (delegate to `ggen-ai` crate)
- MUST validate all user input before passing to AI
- SHOULD support both interactive and non-interactive modes
- MUST respect confidence thresholds for autonomous decisions
- SHOULD provide --dry-run for all destructive operations

#### DEPENDENCIES
- `ggen-ai` crate for all AI operations
- `ggen-utils` for error handling
- CLI argument parsing (clap)
- Configuration management (AI provider, model selection)

#### INVARIANTS
- Evolution operations are atomic (commit or rollback, no partial state)
- History is always recorded before applying changes
- Snapshots are immutable once created
- Approval workflows cannot be bypassed without explicit `--force`

#### ERROR HANDLING
**SHOULD handle**:
- Invalid natural language requirements → Inform user and suggest examples
- AI provider failures → Retry with backoff, fallback to mock in tests
- Validation failures → Display violations with suggested fixes
- Confidence below threshold → Require manual approval or `--force`
- Network errors → Graceful degradation with cached data

**Error messages SHOULD**:
- Explain what went wrong in user-friendly language
- Suggest corrective action (not just dump error trace)
- Indicate whether retry might succeed

#### TESTING STRATEGY
- **Unit tests**: Argument parsing, input validation, output formatting
- **Integration tests**: End-to-end evolution with mock LLM client
- **Property tests**: Ensure rollback always restores previous state
- **Smoke tests**: Verify all commands run without panic

#### REFACTORING NOTES
**Current Issues**:
- Mock client setup duplicated across test functions
- Error conversion between anyhow and ggen_utils::Error is verbose
- Output formatting scattered through run() functions

**Improvements Needed**:
1. Extract mock client setup to test fixture
2. Create dedicated output formatter types (TextFormatter, JsonFormatter)
3. Move configuration loading to dedicated module
4. Add telemetry for tracking evolution success rates
5. Implement progress bars for long-running operations

**Architecture Recommendation**:
```rust
// Desired pattern: Command → Service → Domain
autonomous::run(args)  // CLI orchestration
  → EvolutionService::evolve()  // Business logic
    → GraphEvolutionEngine::evolve_from_nl()  // Core domain logic
```

---

### 1.2 Orchestrator: `orchestrator.rs`

**File**: `ggen-ai/src/autonomous/orchestrator.rs`

#### PURPOSE
Coordinate machine-timescale autonomous regeneration with parallel execution, health monitoring, and adaptive optimization. Think of this as the "brain" that decides when and how to regenerate artifacts.

#### RESPONSIBILITY
- **Detect when regeneration is needed** (threshold-based on delta size)
- **Parallelize regeneration work** across available CPU cores
- **Monitor system health** and performance metrics
- **Adapt behavior** based on telemetry (e.g., increase parallelism if behind target)
- **Coordinate with deployment** system for automated rollout

#### CONSTRAINTS
- MUST respect `max_concurrent` to prevent resource exhaustion
- SHOULD complete cycles within `target_cycle_ms` (30s default)
- MUST maintain thread safety (all state in Arc<RwLock<T>>)
- SHOULD degrade gracefully under resource pressure
- MUST preserve event ordering within a single subject

#### DEPENDENCIES
- `RegenerationEngine` for artifact generation
- `DeploymentAutomation` for deployment
- `TelemetryCollector` for metrics
- `GraphChangeNotifier` for event pub/sub
- Tokio runtime for async execution

#### INVARIANTS
- Only ONE orchestrator instance should run per process
- Cycle statistics are monotonically increasing
- Events are never lost (failures recorded in telemetry)
- Health checks run at regular intervals when orchestrator is running

#### ERROR HANDLING
**SHOULD handle**:
- Event processing failures → Log, increment failure counter, continue
- Cycle time exceeded → Log warning, trigger optimization
- Health check failures → Alert (future: integrate with monitoring system)
- Resource exhaustion → Reduce parallelism temporarily

**Recovery Strategy**:
- Failed regeneration tasks should NOT block other tasks
- Retries should use exponential backoff
- Critical failures should trigger circuit breaker

#### TESTING STRATEGY
- **Unit tests**: Cycle execution, event processing, statistics tracking
- **Concurrency tests**: Verify thread safety with high concurrency
- **Performance tests**: Measure actual cycle time under load
- **Fault injection**: Test behavior when regeneration fails

#### REFACTORING NOTES
**Current Issues**:
- Health check loop couples monitoring with orchestration
- No circuit breaker for cascading failures
- Adaptive optimization is too simplistic (single heuristic)

**Improvements Needed**:
1. Extract health monitoring to separate `HealthMonitor` component
2. Implement circuit breaker pattern for external dependencies
3. Add priority queuing for high-priority changes
4. Implement backpressure when regeneration queue grows too large
5. Add distributed tracing for debugging parallel execution

**Architecture Recommendation**:
```rust
// Desired pattern: Orchestrator → Worker Pool → Task Queue
Orchestrator::execute_cycle()
  → WorkerPool::process_parallel()
    → TaskQueue::pop_batch(max_concurrent)
      → RegenerationWorker::execute()
```

---

### 1.3 Deployment Automation: `deployment.rs`

**File**: `ggen-ai/src/autonomous/deployment.rs`

#### PURPOSE
Automatically deploy generated artifacts to configured environments with validation, testing, and rollback capabilities. This should enable zero-downtime deployments with confidence.

#### RESPONSIBILITY
- **Validate artifacts** before deployment (syntax, security, performance)
- **Run integration tests** in target environment
- **Deploy atomically** with rollback on failure
- **Execute pre/post-deployment hooks** for custom logic
- **Maintain deployment history** for auditing and rollback
- **Support multiple environments** (dev, staging, prod) with different policies

#### CONSTRAINTS
- MUST validate before deploying to production
- SHOULD use atomic file operations (write-then-rename)
- MUST backup before destructive operations
- SHOULD timeout long-running deployments
- MUST respect environment-specific policies (e.g., prod requires manual approval)

#### DEPENDENCIES
- File system operations (tokio::fs)
- Shell command execution for hooks
- Validation modules (syntax, security, performance checkers)

#### INVARIANTS
- Deployment is atomic (either fully succeeds or fully rolls back)
- Production deployments always run all validations
- History is append-only (never delete deployment records)
- Backups are created before every deployment

#### ERROR HANDLING
**SHOULD handle**:
- Validation failures → Abort before deployment, return detailed violations
- Pre-deploy hook failures → Abort, rollback
- File copy failures → Rollback to backup
- Post-deploy hook failures → Rollback if RollbackStrategy::Automatic
- Integration test failures → Rollback or keep based on environment policy

**Rollback Strategies**:
- `Automatic`: Rollback immediately on any failure
- `Manual`: Keep both versions, require manual intervention
- `BlueGreen`: Keep old version running alongside new
- `None`: Never rollback (for idempotent deployments)

#### TESTING STRATEGY
- **Unit tests**: Each validation function independently
- **Integration tests**: Full deployment flow with temp directories
- **Property tests**: Rollback always restores exact previous state
- **Security tests**: Ensure dangerous patterns are detected

#### REFACTORING NOTES
**Current Issues**:
- Validation logic scattered across multiple functions
- File copying is not truly recursive
- No support for remote deployments
- Backup/rollback uses rename (won't work across filesystems)

**Improvements Needed**:
1. Extract validators to plugin architecture (ValidatorPlugin trait)
2. Use proper recursive copy library (e.g., `fs_extra`)
3. Add remote deployment support (SSH, S3, container registries)
4. Implement backup with tar/gzip for cross-filesystem support
5. Add deployment progress tracking
6. Support partial deployments (only changed files)
7. Add deployment verification step (smoke tests after deployment)

**Architecture Recommendation**:
```rust
// Desired pattern: Pipeline with pluggable stages
DeploymentPipeline::new()
  .add_stage(ValidationStage::new(validators))
  .add_stage(BackupStage::new())
  .add_stage(DeploymentStage::new())
  .add_stage(VerificationStage::new())
  .execute(context)
```

---

### 1.4 Delta Detection: `delta_detector.rs`

**File**: `ggen-ai/src/autonomous/delta_detector.rs`

#### PURPOSE
Compute precise differences between graph states to trigger regeneration only when necessary. This is the "diff" engine for RDF graphs.

#### RESPONSIBILITY
- **Detect additions, deletions, modifications** in RDF triples
- **Track affected subjects and predicates** for targeted regeneration
- **Maintain baseline state** for incremental diff computation
- **Compute statistics** (total changes, affected entities)
- **Preserve evolution history** for auditing and debugging

#### CONSTRAINTS
- MUST handle large graphs efficiently (10k+ triples)
- SHOULD use memory-efficient data structures (HashSet, not Vec)
- MUST preserve triple ordering for deterministic diffs
- SHOULD support configurable significance thresholds
- MUST handle malformed triples gracefully

#### DEPENDENCIES
- Oxigraph Store for RDF storage and querying
- HashMap/HashSet for efficient diff computation

#### INVARIANTS
- Baseline state is never mutated (except by apply_delta)
- History is append-only
- Delta stats are always consistent with operations
- Applying delta then re-computing should yield empty delta

#### ERROR HANDLING
**SHOULD handle**:
- Malformed triples → Skip and log warning
- Memory pressure → Stream large graphs instead of loading all
- Corrupted baseline → Reset to empty and rebuild

**Edge Cases**:
- Empty baseline (all additions)
- Empty new state (all deletions)
- Identical states (no changes)
- Circular references

#### TESTING STRATEGY
- **Unit tests**: Each delta operation type
- **Property tests**:
  - `apply_delta(compute_delta(A, B), A) == B`
  - `compute_delta(A, A) == empty`
  - `compute_delta(A, B).stats.total == additions + deletions + modifications`
- **Performance tests**: 10k triple graph diff completes in <100ms
- **Fuzzing**: Random graph generation to find edge cases

#### REFACTORING NOTES
**Current Issues**:
- Simple string-based parsing (fragile for complex RDF)
- No support for blank nodes
- No namespace/prefix handling
- Modifications not actually detected (empty vec)

**Improvements Needed**:
1. Use Oxigraph's native triple representation instead of strings
2. Implement proper modification detection (same subject/predicate, different object)
3. Support blank nodes and their identity semantics
4. Add configurable diff algorithms (structural vs. semantic)
5. Implement streaming diff for large graphs
6. Add support for named graphs
7. Compute semantic diff (e.g., equivalent but syntactically different)

**Architecture Recommendation**:
```rust
// Desired pattern: Strategy pattern for different diff algorithms
trait DiffAlgorithm {
    fn compute_delta(&self, baseline: &Graph, new_state: &Graph) -> GraphDelta;
}

struct StructuralDiff;
struct SemanticDiff;
struct StreamingDiff;

DeltaDetector::with_algorithm(Box::new(SemanticDiff))
```

---

### 1.5 Event System: `events.rs`

**File**: `ggen-ai/src/autonomous/events.rs`

#### PURPOSE
Provide event-driven change notification to decouple graph operations from regeneration triggers. This enables reactive, real-time system behavior.

#### RESPONSIBILITY
- **Define event types** for all graph change operations
- **Publish events** to all subscribers
- **Filter events** based on subscriber interests
- **Maintain event history** for replay and debugging
- **Support both broadcast and directed notification**

#### CONSTRAINTS
- MUST not block publishers (use async broadcast)
- SHOULD preserve event ordering for single subject
- MUST handle slow subscribers gracefully (drop or buffer)
- SHOULD limit history size to prevent memory leaks
- MUST be thread-safe (multiple publishers/subscribers)

#### DEPENDENCIES
- Tokio broadcast channel for pub/sub
- RwLock for subscriber registry
- Chrono for timestamps

#### INVARIANTS
- Events are immutable once created
- Event IDs are unique (UUIDs)
- History never exceeds `max_history` size
- Subscribers never receive events they filtered out

#### ERROR HANDLING
**SHOULD handle**:
- Subscriber failures → Log and continue, don't fail other subscribers
- Full broadcast channel → Log warning, event is lost for that receiver
- Slow subscribers → Detect and warn (future: auto-unsubscribe)

**Failure Modes**:
- Publisher continues even if no subscribers
- Subscriber error doesn't affect other subscribers
- History is best-effort (may lose events under memory pressure)

#### TESTING STRATEGY
- **Unit tests**: Event creation, filtering, history
- **Concurrency tests**: Multiple publishers/subscribers
- **Integration tests**: Event flow through full system
- **Performance tests**: Event throughput (>10k events/sec)

#### REFACTORING NOTES
**Current Issues**:
- No event persistence (lost on restart)
- Simple string-based delta detection (duplicated logic)
- No event batching or aggregation
- History is unbounded per-notifier (memory leak risk)

**Improvements Needed**:
1. Add event persistence (SQLite or log file)
2. Implement event sourcing pattern (rebuild state from events)
3. Add event batching to reduce overhead
4. Implement event aggregation (combine multiple changes into one)
5. Add event replay capability for testing/debugging
6. Support distributed event bus (Redis, Kafka)
7. Add event schema versioning

**Architecture Recommendation**:
```rust
// Desired pattern: Event sourcing with CQRS
EventStore::append(event)  // Write side
  → EventLog::persist()
  → EventNotifier::broadcast()

EventStore::replay()  // Read side
  → EventProjection::rebuild_state()
```

---

### 1.6 Validator: `validator.rs`

**File**: `ggen-ai/src/autonomous/validator.rs`

#### PURPOSE
Automatically generate and execute validation queries to ensure graph consistency before committing changes. This is the "compiler" for knowledge graphs.

#### RESPONSIBILITY
- **Generate SPARQL validation queries** from ontology
- **Execute validation queries** against RDF store
- **Detect constraint violations** (domain, range, cardinality)
- **Learn common validation patterns** for reuse
- **Provide actionable fix suggestions** for violations

#### CONSTRAINTS
- MUST validate all SHACL-style constraints
- SHOULD complete validation in <1s for typical graphs
- MUST handle custom validation rules
- SHOULD cache generated queries
- MUST be deterministic (same input → same validation result)

#### DEPENDENCIES
- LLM client for query generation
- Oxigraph Store for SPARQL execution
- Ontology parser for constraint extraction

#### INVARIANTS
- Valid graphs produce zero violations
- Same violation always has same severity
- Learned patterns are never lost (unless explicitly cleared)
- Validation is idempotent

#### ERROR HANDLING
**SHOULD handle**:
- Invalid SPARQL queries from AI → Log and skip
- Timeout on long queries → Kill query and report error
- Store corruption → Attempt rebuild from triples
- Missing ontology → Use default validation rules

**Violation Reporting**:
- Group violations by type (easier to fix systematically)
- Sort by severity (Critical > Warning > Info)
- Provide context (which rule was violated and why)
- Suggest fixes when possible

#### TESTING STRATEGY
- **Unit tests**: Query extraction, violation detection
- **Integration tests**: End-to-end validation with known violations
- **Golden tests**: Known-good ontologies should always pass
- **Adversarial tests**: Malformed RDF should be caught

#### REFACTORING NOTES
**Current Issues**:
- Query generation is one-shot (no iterative refinement)
- No caching of generated queries
- Limited SHACL support (only basic constraints)
- Violations lack rich context

**Improvements Needed**:
1. Implement full SHACL validation
2. Cache generated queries in filesystem
3. Add custom validation rule DSL
4. Improve violation messages with examples
5. Add auto-fix capability for common violations
6. Support incremental validation (only changed parts)
7. Add validation timing metrics

**Architecture Recommendation**:
```rust
// Desired pattern: Rule engine with pluggable validators
trait ValidationRule {
    fn check(&self, graph: &Graph) -> Vec<Violation>;
}

Validator::new()
  .add_rule(DomainRangeValidator)
  .add_rule(CardinalityValidator)
  .add_rule(CustomRule::from_sparql(query))
  .validate(graph)
```

---

## 2. AI-Powered Generation System

### 2.1 Ontology Generator: `ontology.rs`

**File**: `ggen-ai/src/generators/ontology.rs`

#### PURPOSE
Generate RDF ontologies from natural language descriptions using LLMs, making ontology engineering accessible to non-experts.

#### RESPONSIBILITY
- **Accept domain descriptions** in natural language
- **Generate valid RDF/OWL ontologies** (Turtle format)
- **Support streaming generation** for large ontologies
- **Extract ontology content** from AI responses (handle code blocks)
- **Validate generated ontology** before returning
- **Evolve existing graphs** based on requirements

#### CONSTRAINTS
- MUST generate syntactically valid Turtle
- SHOULD follow OWL best practices (domains, ranges, labels)
- MUST handle code block variations (```turtle, ```ttl, ```rdf)
- SHOULD validate before returning to user
- MUST be deterministic given same requirements (for testing)

#### DEPENDENCIES
- LLM client (with configurable provider)
- Prompt builder for ontology generation
- Oxigraph for validation

#### INVARIANTS
- Generated ontologies are always valid Turtle
- Required classes/properties appear in output
- Generated URIs use configured base URI
- Confidence scores are in [0.0, 1.0] range

#### ERROR HANDLING
**SHOULD handle**:
- LLM returns non-Turtle content → Extract from code blocks
- No code blocks found → Attempt to parse entire response
- Multiple code blocks → Use first valid Turtle block
- Invalid Turtle → Return detailed parse error with line number
- LLM refuses request → Return helpful error with suggested rephrase

**Graceful Degradation**:
- If streaming fails → Fall back to complete generation
- If extraction fails → Return raw response with warning
- If validation fails → Return syntax error details

#### TESTING STRATEGY
- **Unit tests**: Content extraction from various response formats
- **Integration tests**: End-to-end generation with mock LLM
- **Golden tests**: Known domains should produce valid ontologies
- **Snapshot tests**: Track evolution of generated ontologies

#### REFACTORING NOTES
**Current Issues**:
- Graph evolution methods (evolve_graph, apply_evolution) are incomplete stubs
- No support for partial generation (incremental builds)
- Statistics extraction uses hardcoded queries
- No caching of common ontology patterns

**Improvements Needed**:
1. Complete graph evolution implementation
2. Add ontology composition (combine multiple ontologies)
3. Implement incremental generation (add to existing graph)
4. Cache common patterns (User, Organization, etc.)
5. Add ontology refactoring operations (rename, merge classes)
6. Support multiple output formats (JSON-LD, N-Triples)
7. Add ontology quality metrics (completeness, consistency)

**Architecture Recommendation**:
```rust
// Desired pattern: Builder with validation pipeline
OntologyGenerator::new()
  .with_domain("user management")
  .with_requirements(vec!["users have emails", "roles"])
  .with_base_uri("http://example.org/")
  .validate_with(ShaclValidator)
  .generate()
```

---

## 3. Marketplace System

The marketplace should provide npm-like package management for reusable RDF patterns and templates.

### 3.1 Package Search: `market/search.rs`

**File**: `cli/src/cmds/market/search.rs`

#### PURPOSE
Enable users to discover relevant gpacks through flexible search with ranking, filtering, and suggestions.

#### RESPONSIBILITY
- **Execute search queries** against marketplace registry
- **Rank results** by relevance, popularity, or recency
- **Apply filters** (category, author, license, stars, downloads)
- **Provide search suggestions** for typos and related terms
- **Format results** for human readability (CLI) or machine consumption (JSON)
- **Validate search input** to prevent injection attacks

#### CONSTRAINTS
- MUST validate query length (max 1000 chars)
- MUST validate result limit (max 100)
- SHOULD complete searches in <500ms for good UX
- MUST sanitize input to prevent command injection
- SHOULD support fuzzy matching for typos

#### DEPENDENCIES
- Marketplace client (HTTP API or local registry)
- Search ranking algorithm
- Output formatting utilities

#### INVARIANTS
- Search results are always sorted by specified field
- Filters are always applied before sorting
- JSON output is always valid JSON
- Search never returns more than requested limit

#### ERROR HANDLING
**SHOULD handle**:
- Empty query → Return validation error with examples
- Query too long → Truncate with warning
- Limit too high → Clamp to max with warning
- Registry unavailable → Suggest offline mode or cached results
- Invalid filter values → List valid options

**User Experience**:
- Progressive enhancement (show cached results while fetching)
- Spell-check suggestions for common typos
- Related searches based on query

#### TESTING STRATEGY
- **Unit tests**: Input validation, filter application
- **Integration tests**: Search with mock marketplace client
- **Property tests**: All results match applied filters
- **Performance tests**: 1000-package registry search <500ms

#### REFACTORING NOTES
**Current Issues**:
- Mock data instead of real marketplace integration
- No caching of search results
- Fuzzy search is placeholder (simple substring matching)
- No pagination support

**Improvements Needed**:
1. Implement real marketplace API client
2. Add search result caching (Redis or local SQLite)
3. Implement proper fuzzy matching (Levenshtein distance)
4. Add pagination for large result sets
5. Support advanced query syntax (tags:auth,oauth)
6. Add search history and favorites
7. Implement recommendation engine (users who searched X also liked Y)

**Architecture Recommendation**:
```rust
// Desired pattern: Repository pattern with caching
trait MarketplaceRepository {
    fn search(&self, query: SearchQuery) -> Result<SearchResults>;
}

struct CachedMarketplace {
    cache: Arc<SearchCache>,
    backend: Box<dyn MarketplaceRepository>,
}
```

---

### 3.2 Package Listing: `market/list.rs`

**File**: `cli/src/cmds/market/list.rs`

#### PURPOSE
Show installed gpacks with version information for dependency tracking and auditing.

#### RESPONSIBILITY
- **Read lockfile** (.ggen/lock.json) to get installed packages
- **Display package metadata** (version, source, integrity hash)
- **Support detailed view** with full dependency tree
- **Detect outdated packages** (compare with registry)
- **Format for human and machine consumption**

#### CONSTRAINTS
- MUST read from lockfile (source of truth)
- SHOULD show transitive dependencies in detailed mode
- MUST verify lockfile integrity
- SHOULD highlight security vulnerabilities
- MUST handle missing lockfile gracefully

#### DEPENDENCIES
- Lockfile parser
- Marketplace client (for update checks)
- Output formatters

#### INVARIANTS
- Listed packages always match lockfile content
- Version strings follow semver format
- Integrity hashes are always SHA-256

#### ERROR HANDLING
**SHOULD handle**:
- Missing lockfile → Explain how to install packages
- Corrupted lockfile → Offer to rebuild
- Registry unavailable → Show installed packages without update info

#### TESTING STRATEGY
- **Unit tests**: Lockfile parsing, formatting
- **Integration tests**: List with mock lister
- **Golden tests**: Known lockfile → expected output

#### REFACTORING NOTES
**Current Issues**:
- Lockfile format not defined
- No dependency tree visualization
- No update checking
- Mock data only

**Improvements Needed**:
1. Define lockfile format (JSON schema)
2. Implement dependency tree visualization (ASCII art)
3. Add update checking against registry
4. Show security audit results inline
5. Support filtering by status (outdated, vulnerable)
6. Add export to various formats (CSV, Markdown)

**Architecture Recommendation**:
```rust
// Desired pattern: Domain model with projections
struct InstalledPackages {
    packages: Vec<Package>,
    lockfile_version: String,
}

impl InstalledPackages {
    fn to_tree_view(&self) -> String;
    fn to_detailed_view(&self) -> String;
    fn find_vulnerabilities(&self) -> Vec<Vulnerability>;
}
```

---

### 3.3 Category Browser: `market/categories.rs`

**File**: `cli/src/cmds/market/categories.rs`

#### PURPOSE
Help users discover gpacks by browsing popular categories and understanding marketplace organization.

#### RESPONSIBILITY
- **List categories** with package counts
- **Show popular categories** based on downloads/stars
- **Enable category-based search** as next action
- **Display category metadata** (description, icon)

#### CONSTRAINTS
- SHOULD cache category list (changes infrequently)
- MUST show accurate package counts
- SHOULD update counts in background

#### DEPENDENCIES
- Marketplace client
- Category registry

#### INVARIANTS
- Category names are unique
- Package counts are non-negative
- Categories are sorted by requested field

#### ERROR HANDLING
**SHOULD handle**:
- Registry unavailable → Show cached categories
- Stale cache → Show with timestamp

#### TESTING STRATEGY
- **Unit tests**: Category sorting, formatting
- **Integration tests**: Fetch with mock lister

#### REFACTORING NOTES
**Current Issues**:
- Mock data only
- No caching
- No category hierarchy

**Improvements Needed**:
1. Implement real category API
2. Add category hierarchy (parent/child)
3. Show trending categories
4. Add category search
5. Display featured packages per category

**Architecture Recommendation**:
```rust
// Desired pattern: Tree structure for category hierarchy
struct Category {
    name: String,
    parent: Option<String>,
    children: Vec<Category>,
    package_count: usize,
}

impl Category {
    fn to_tree(&self) -> String;
    fn flatten(&self) -> Vec<&Category>;
}
```

---

## 4. Swarm & Agent Management

### 4.1 Swarm CLI: `swarm.rs`

**File**: `cli/src/cmds/swarm.rs`

#### PURPOSE
Provide CLI interface for managing ultrathink swarm agents and WIP integration for autonomous development workflows.

#### RESPONSIBILITY
- **Start/stop MCP swarm server** for agent coordination
- **Register agents** with capabilities
- **Submit tasks** to swarm for processing
- **Query swarm status** and agent health
- **Manage WIP integration** for autonomous work item processing
- **Configure swarm parameters** (max agents, sync interval)

#### CONSTRAINTS
- MUST delegate to ggen-ai for actual implementation
- SHOULD provide placeholder functionality until implementation is complete
- MUST show clear warnings when features are not yet implemented
- SHOULD guide users to alternative commands

#### DEPENDENCIES
- ggen-ai (when implemented) for swarm coordination
- WIP integration library (when implemented)
- MCP server infrastructure (when implemented)

#### INVARIANTS
- Commands never panic (return errors gracefully)
- All placeholders clearly marked with warning messages
- Configuration is validated before saving

#### ERROR HANDLING
**SHOULD handle**:
- Unimplemented features → Show helpful placeholder message
- Invalid configuration → Validate and show errors
- Port conflicts → Suggest alternative port

#### TESTING STRATEGY
- **Unit tests**: Argument parsing
- **Integration tests**: (Deferred until implementation)

#### REFACTORING NOTES
**Current Issues**:
- All functionality is placeholder
- No integration with ggen-ai swarm types
- Types are commented out (not implemented)

**Improvements Needed**:
1. Implement SwarmCoordinator in ggen-ai
2. Implement WipDiscoveryAgent in ggen-ai
3. Implement McpSwarmServer in ggen-ai
4. Connect CLI commands to implementations
5. Add real-time status monitoring
6. Implement task queue visualization

**Architecture Recommendation**:
```rust
// Desired pattern: CLI → Service → Domain
swarm::run(args)
  → SwarmService::manage()
    → SwarmCoordinator::coordinate()
      → Agent::execute_task()
```

---

## 5. Shell Integration

### 5.1 Shell Initialization: `shell/init.rs`

**File**: `cli/src/cmds/shell/init.rs`

#### PURPOSE
Set up shell integration (completions, aliases, hooks) and initialize development environments to improve developer experience.

#### RESPONSIBILITY
- **Initialize shell completions** for bash/zsh/fish/powershell
- **Configure shell integration** (PATH, aliases, hooks)
- **Initialize project structure** from templates
- **Set up development tools** (git hooks, editor config)
- **Validate shell compatibility** before installation

#### CONSTRAINTS
- MUST detect shell type automatically when possible
- SHOULD back up existing configuration before modifying
- MUST support manual configuration path override
- SHOULD verify installation success
- MUST provide rollback for failed installations

#### DEPENDENCIES
- Shell initialization delegated to cargo-make
- File system operations for config modification
- Process execution for running initialization commands

#### INVARIANTS
- Shell config is never corrupted (backup exists)
- Installation is idempotent (can run multiple times safely)
- Force flag bypasses "already configured" check

#### ERROR HANDLING
**SHOULD handle**:
- Unsupported shell → List supported shells
- Missing cargo-make → Provide installation instructions
- Permission denied → Explain required permissions
- Config file readonly → Request override or sudo

**Recovery**:
- Always create backup before modifying
- Provide explicit rollback command
- Verify modifications before completion

#### TESTING STRATEGY
- **Unit tests**: Argument validation, shell type detection
- **Integration tests**: Full initialization with mock shell initializer
- **End-to-end tests**: Actual shell initialization in CI (container)

#### REFACTORING NOTES
**Current Issues**:
- Delegates all work to cargo-make (tight coupling)
- No direct shell config manipulation
- No verification of successful installation
- Error messages come from cargo-make (not user-friendly)

**Improvements Needed**:
1. Implement direct shell config manipulation (reduce cargo-make dependency)
2. Add shell completion generation using clap
3. Verify completions work after installation
4. Add interactive installer with prompts
5. Support custom installation locations
6. Add uninstall command
7. Support multiple shells on same system

**Architecture Recommendation**:
```rust
// Desired pattern: Strategy pattern for shell-specific logic
trait ShellIntegration {
    fn detect(&self) -> Result<ShellType>;
    fn install_completion(&self, config_path: &Path) -> Result<()>;
    fn verify_installation(&self) -> Result<bool>;
}

struct BashIntegration;
struct ZshIntegration;
struct FishIntegration;
```

---

## 6. Development Workflow Integration

### 6.1 TOC Validation Workflow: `.github/workflows/toc.yml`

**File**: `.github/workflows/toc.yml`

#### PURPOSE
Ensure documentation table of contents stays synchronized with content through automated CI checks.

#### RESPONSIBILITY
- **Validate TOC** is up-to-date on every PR modifying docs
- **Run on specific paths** (README.md, docs/**/*.md)
- **Fail PR checks** if TOC is outdated
- **Provide clear error message** with fix instructions
- **Run efficiently** (cancel outdated runs, timeout quickly)

#### CONSTRAINTS
- SHOULD complete in <30 seconds
- MUST cancel redundant runs (concurrency group)
- MUST timeout if stuck (5 minute maximum)
- SHOULD run only when docs change (path filter)

#### DEPENDENCIES
- technote-space/toc-generator@v4 action
- GitHub Actions infrastructure

#### INVARIANTS
- Only runs on docs changes
- Always uses same TOC generator as pre-commit hook
- Never modifies files (check-only mode)

#### ERROR HANDLING
**SHOULD handle**:
- Timeout → Fail with clear message
- Generator fails → Show error from tool
- Network issues → Retry automatically

**User Communication**:
- Clear PR comment explaining how to fix
- Link to documentation on TOC generation
- Show diff of expected vs actual TOC

#### TESTING STRATEGY
- **Unit tests**: N/A (workflow configuration)
- **Integration tests**: Trigger workflow with test PR

#### REFACTORING NOTES
**Current Issues**:
- Depends on external GitHub Action (risk of breaking changes)
- No custom error messages
- No slack/discord notification on failure

**Improvements Needed**:
1. Add custom error message formatting
2. Post PR comment with instructions when fails
3. Consider self-hosting TOC generator
4. Add notification for repeated failures
5. Cache dependencies to speed up runs

---

### 6.2 Pre-Commit TOC Hook: `scripts/git-hooks/pre-commit-toc`

**File**: `scripts/git-hooks/pre-commit-toc`

#### PURPOSE
Automatically generate/update TOC in markdown files before commit to prevent CI failures and maintain consistency.

#### RESPONSIBILITY
- **Detect markdown files** in commit
- **Check for TOC markers** (<!-- toc -->)
- **Generate/update TOC** using markdown-toc
- **Stage updated files** automatically
- **Skip gracefully** if tools unavailable (don't block commits)

#### CONSTRAINTS
- MUST only modify files with TOC markers
- SHOULD complete in <2 seconds for good DX
- MUST stage updated files automatically
- SHOULD fail gracefully if npx unavailable
- MUST never corrupt markdown files

#### DEPENDENCIES
- npx (Node.js) for markdown-toc
- git for staging updated files
- Shell environment

#### INVARIANTS
- Only processes staged markdown files
- Only modifies files with <!-- toc --> marker
- Always stages modified files
- Never fails commit (warnings only)

#### ERROR HANDLING
**SHOULD handle**:
- npx not installed → Warning message, skip TOC generation
- markdown-toc fails → Warning, continue commit
- File write failure → Warning, show error

**Graceful Degradation**:
- Missing npx → Inform user, don't block commit
- TOC generation fails → Warn but allow commit
- CI will catch if TOC is incorrect

#### TESTING STRATEGY
- **Unit tests**: N/A (shell script)
- **Integration tests**: Run hook on test repo with various scenarios
- **Manual tests**: Install hook and test with real commits

#### REFACTORING NOTES
**Current Issues**:
- Uses npx (requires Node.js)
- Simple error handling (ignores failures)
- No configuration options
- No dry-run mode

**Improvements Needed**:
1. Add configuration file (.ggen-toc.yml)
2. Support multiple TOC generators
3. Add --verify flag (check without modifying)
4. Better error messages with fix suggestions
5. Add support for custom TOC markers
6. Respect .gitignore patterns

**Architecture Recommendation**:
```bash
# Desired pattern: Configurable hook with multiple backends
.ggen-toc.yml:
  generator: markdown-toc
  markers: ["<!-- toc -->", "<!-- doctoc -->"]
  options:
    maxdepth: 3
```

---

## 7. Cross-Cutting Architectural Concerns

### 7.1 Error Handling Strategy

#### PRINCIPLES
1. **Errors should be informative**: Explain what went wrong and how to fix it
2. **Use type-safe errors**: Prefer `Result<T, E>` over panic
3. **Context is critical**: Add context when propagating errors
4. **Fail fast**: Validate early, fail quickly
5. **Recovery over failure**: Try to recover or degrade gracefully

#### PATTERNS TO FOLLOW
```rust
// ✅ GOOD: Rich error with context
Err(GgenAiError::validation(format!(
    "Triple validation failed for subject '{}': {}",
    subject, reason
)))

// ❌ BAD: Generic error without context
Err(GgenAiError::validation("Validation failed"))

// ✅ GOOD: Graceful degradation
let results = match client.search(query) {
    Ok(results) => results,
    Err(e) => {
        warn!("Search failed, using cache: {}", e);
        cache.get(query)?
    }
};

// ❌ BAD: Silent failure
let _ = client.search(query);
```

#### ERROR CONVERSION
- Use `From` trait for error conversion between crates
- Add context when converting errors
- Preserve error chain for debugging

---

### 7.2 Testing Strategy

#### TEST PYRAMID
```
    ┌─────────────┐
    │    E2E      │  10%  - Full system tests
    ├─────────────┤
    │ Integration │  30%  - Component interaction
    ├─────────────┤
    │    Unit     │  60%  - Pure functions
    └─────────────┘
```

#### TEST CATEGORIES

**Unit Tests** (Fast, Isolated):
- Pure functions without I/O
- Business logic
- Validation rules
- Formatting/parsing

**Integration Tests** (Medium Speed):
- Database interactions
- File system operations
- Component coordination
- API contracts

**End-to-End Tests** (Slow, Realistic):
- Full CLI commands
- Complete workflows
- Real external dependencies

#### TESTING PATTERNS
```rust
// ✅ Use dependency injection for testability
async fn run_with_deps(args: &Args, client: &dyn Client) -> Result<()> {
    // Implementation uses client trait
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_with_mock() {
        let mut mock = MockClient::new();
        mock.expect_search()
            .returning(|_| Ok(vec![]));

        let result = run_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }
}

// ✅ Property-based testing for invariants
#[quickcheck]
fn apply_delta_is_inverse(baseline: Vec<String>, new: Vec<String>) -> bool {
    let delta = compute_delta(&baseline, &new);
    let result = apply_delta(&baseline, &delta);
    result == new
}
```

---

### 7.3 Dependency Injection

#### WHY
- Enables testing with mocks
- Reduces coupling
- Supports multiple implementations
- Improves modularity

#### PATTERNS
```rust
// ✅ GOOD: Trait-based dependency injection
trait MarketplaceClient {
    fn search(&self, query: &str) -> Result<Vec<Package>>;
}

async fn search_command(args: &Args, client: &dyn MarketplaceClient) -> Result<()> {
    let results = client.search(&args.query)?;
    // ...
}

// Production
let client = HttpMarketplaceClient::new();
search_command(&args, &client).await?;

// Testing
let mock = MockMarketplaceClient::new();
search_command(&args, &mock).await?;
```

#### WHERE TO INJECT
- External services (HTTP, database)
- File system operations
- Time/randomness sources
- Configuration providers

---

### 7.4 Logging vs. User Output

#### PRINCIPLE
**Logging is for developers, output is for users. Never mix them.**

#### RULES
```rust
// ✅ GOOD: Separate concerns
use tracing::{info, warn, error, debug};

pub async fn deploy(args: &Args) -> Result<()> {
    // User-facing output (always visible)
    println!("🚀 Starting deployment...");

    // Developer logs (controlled by RUST_LOG)
    info!("Deploying to {}", args.environment);
    debug!("Config: {:?}", args);

    if let Err(e) = validate() {
        // User error message
        eprintln!("❌ Validation failed: {}", e);
        // Developer context
        error!("Validation error details: {:?}", e);
        return Err(e);
    }

    println!("✅ Deployment complete");
    Ok(())
}

// ❌ BAD: Mixing concerns
println!("[INFO] Starting deployment");  // Not a log, not user output
```

#### LOGGING LEVELS
- `error!()`: Something failed, requires attention
- `warn!()`: Unexpected but handled
- `info!()`: Important state changes
- `debug!()`: Detailed diagnostic information
- `trace!()`: Very verbose, performance-impacting

---

## 8. Refactoring Priorities

### HIGH PRIORITY (P0) - Core Functionality
1. **Complete autonomous graph evolution**
   - Implement missing delta application in ontology.rs
   - Add real validation instead of placeholders
   - Connect all pieces of the evolution pipeline

2. **Implement marketplace client**
   - Real HTTP API client instead of mocks
   - Proper error handling for network failures
   - Caching layer for performance

3. **Fix error handling**
   - Consistent error types across crates
   - Rich error context throughout
   - User-friendly error messages

### MEDIUM PRIORITY (P1) - Quality & Performance
4. **Add comprehensive testing**
   - Property-based tests for invariants
   - Integration tests with real dependencies
   - Performance benchmarks

5. **Improve delta detection**
   - Use Oxigraph native types (not strings)
   - Implement semantic diff
   - Support streaming for large graphs

6. **Extract deployment validators**
   - Plugin architecture for validators
   - Community-contributed validators
   - Configurable validation rules

### LOW PRIORITY (P2) - Developer Experience
7. **Better CLI output formatting**
   - Rich terminal UI (progress bars, tables)
   - Colored output for readability
   - Machine-readable JSON mode

8. **Documentation generation**
   - Auto-generate from code
   - Interactive examples
   - Troubleshooting guides

9. **Developer tools**
   - REPL for ontology development
   - Graph visualizer
   - Debugging utilities

---

## 9. Technical Debt Registry

### Code Smells
| Location | Issue | Impact | Priority |
|----------|-------|--------|----------|
| autonomous.rs | anyhow → ggen_utils::Error conversion | Verbose, error-prone | P1 |
| ontology.rs | Stub methods (apply_evolution) | Incomplete feature | P0 |
| search.rs | Mock data instead of real API | Testing only | P0 |
| delta_detector.rs | String-based triple parsing | Fragile, slow | P1 |
| deployment.rs | Incomplete rollback logic | Risk of data loss | P0 |
| validator.rs | No query caching | Performance issue | P1 |

### Architecture Issues
| Issue | Description | Recommendation | Priority |
|-------|-------------|----------------|----------|
| Tight coupling to cargo-make | Shell init delegates everything | Implement directly | P1 |
| No distributed tracing | Hard to debug parallel operations | Add OpenTelemetry | P2 |
| Mixed logging/output | Confuses users and developers | Separate strictly | P1 |
| No event persistence | Events lost on restart | Add event store | P1 |
| Mock types in production code | Increases binary size | Feature-gate mocks | P2 |

### Missing Features
| Feature | Use Case | Priority |
|---------|----------|----------|
| Incremental graph diff | Large graph performance | P0 |
| Distributed deployment | Multi-server deployments | P2 |
| Event replay | Debugging and testing | P1 |
| Custom validation DSL | Domain-specific rules | P1 |
| Template composition | Reusable patterns | P1 |
| Swarm implementation | Autonomous development | P0 |

---

## How to Use This Document

### For Refactoring
1. **Before changing code**: Read the relevant section to understand intent
2. **While refactoring**: Ensure changes align with stated purpose and constraints
3. **After refactoring**: Update this document if intent has changed

### For New Features
1. **Start with "PURPOSE"**: What problem are we solving?
2. **Define "RESPONSIBILITY"**: What should this do (and NOT do)?
3. **List "CONSTRAINTS"**: What rules must we follow?
4. **Document "INVARIANTS"**: What should always be true?

### For Code Review
1. **Check intent alignment**: Does implementation match stated purpose?
2. **Verify constraints**: Are all constraints respected?
3. **Test invariants**: Are all invariants tested?
4. **Improve documentation**: Update this document with insights

### For Onboarding
1. **Read architecture sections** relevant to your work
2. **Study cross-cutting concerns** (error handling, testing, DI)
3. **Review technical debt** to understand current state
4. **Ask questions** if intent is unclear

---

## Document Maintenance

**When to Update**:
- Architecture changes (new components, major refactors)
- Intent clarification (discovered through issues/bugs)
- New patterns discovered (add to cross-cutting concerns)
- Technical debt resolved (remove from registry)

**Who Should Update**:
- Anyone making architectural changes
- Code reviewers who spot misalignment
- Maintainers during refactoring efforts

**Review Schedule**:
- Quarterly: Review all sections for accuracy
- After major releases: Update with lessons learned
- When onboarding new contributors: Check for clarity

---

**Remember**: This document describes the *intended* architecture, not the current implementation. Use it as a north star to guide refactoring efforts toward a coherent, maintainable system.
