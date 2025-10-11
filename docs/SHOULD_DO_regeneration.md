# RegenerationEngine - SHOULD DO Documentation

**File**: `ggen-ai/src/autonomous/regeneration.rs`
**Purpose**: Core regeneration engine with delta-driven template regeneration
**Last Updated**: 2025-10-10

---

## 🎯 Core Purpose

**WHAT IT SHOULD DO**: Listen for change events from the knowledge graph, intelligently identify affected templates through dependency analysis, and trigger incremental, parallel regeneration of code artifacts across multiple target languages while tracking statistics and maintaining consistency.

**NOT**: A simple code generator or batch processor. It should be an event-driven, delta-aware regeneration system that minimizes work through incremental builds and dependency tracking.

---

## 🏗️ Architectural Intent

### Design Principles

1. **Delta-Driven**: Should only regenerate what changed (incremental builds)
2. **Dependency-Aware**: Should understand and track template dependencies
3. **Parallel Execution**: Should regenerate multiple templates concurrently
4. **Event-Driven**: Should react to change events from knowledge graph
5. **Multi-Language**: Should generate code for multiple target languages
6. **Versioned**: Should track artifact versions automatically
7. **Observable**: Should emit detailed statistics and logs

---

## 📋 Component Contracts

### RegenerationConfig

**SHOULD SPECIFY**:
- `incremental: bool` - Enable incremental builds (default: true)
- `parallel_workers: usize` - Number of concurrent regenerations (default: num_cpus)
- `target_languages: Vec<String>` - Languages to generate (rust, typescript, python)
- `template_dirs: Vec<PathBuf>` - Template source directories
- `output_dir: PathBuf` - Generated artifact output location
- `auto_version: bool` - Automatically increment versions (default: true)
- `track_dependencies: bool` - Maintain dependency graph (default: true)

**SHOULD VALIDATE**:
- parallel_workers > 0
- template_dirs exist and are readable
- output_dir is writable
- target_languages are supported

**SHOULD NOT**:
- Allow unbounded parallelism
- Use hardcoded paths
- Ignore missing directories

---

### RegenerationEngine

**SHOULD DO**:
- Subscribe to change events from GraphChangeNotifier
- Maintain in-memory dependency graph
- Track artifact registry (templates → generated files)
- Execute parallel regeneration workflows
- Update artifact metadata (version, last_regenerated)
- Collect regeneration statistics
- Provide snapshot API for stats and artifacts

**SHOULD MAINTAIN**:
- `DependencyGraph`: Bidirectional template dependencies
- `ArtifactRegistry`: Map of template_id → AffectedArtifact
- `RegenerationStats`: Success/failure counts, timing, events processed
- `TemplateGenerator`: For actual code generation

**SHOULD NOT**:
- Modify the knowledge graph
- Deploy generated artifacts (delegate to DeploymentAutomation)
- Block on synchronous I/O
- Process events while starting up

---

## 🔄 Change Event Processing Contract

### Event Subscription

**SHOULD DO**:
1. Register as EventSubscriber with GraphChangeNotifier on start()
2. Receive change events via on_event() callback
3. Filter events using filter() predicate (ignore self-generated events)
4. Process events asynchronously
5. Continue processing despite individual event failures

**SHOULD FILTER OUT**:
- Events from source "regeneration" (avoid loops)
- Events for unsupported graph operations
- Duplicate events (cache recent event IDs)

---

### Process Change Event

**SHOULD DO**:
1. Record start time
2. Update stats: events_processed++
3. Identify affected templates using change type and dependency graph
4. Return early if no templates affected
5. Create DeltaChange with affected templates and dependencies
6. Trigger regeneration for affected templates
7. Calculate and record execution time
8. Update total_time_ms in stats

**SHOULD LOG**:
- Event ID and change type at INFO level
- Number of affected templates at INFO
- Duration at INFO
- Errors at ERROR level with context

**SHOULD NOT**:
- Fail entire process if single template fails
- Block on template regeneration (use async)
- Skip dependency expansion
- Ignore performance metrics

---

## 🎯 Dependency Tracking Contract

### DependencyGraph

**SHOULD MAINTAIN**:
- `dependencies`: Map<template_id, Set<depends_on>>
- `dependents`: Map<template_id, Set<dependent_templates>>

**SHOULD PROVIDE**:
- `add_dependency(template_id, depends_on)`: Bidirectional tracking
- `get_affected(template_id)`: Transitive closure of dependents

**ALGORITHM SHOULD**:
1. Start with changed template
2. Find all templates that depend on it (direct dependents)
3. Recursively find dependents of dependents
4. Return complete set of affected templates
5. Detect and handle cycles gracefully

**SHOULD NOT**:
- Create circular dependencies (validate on add)
- Allow orphaned entries
- Use unbounded recursion (limit depth)

---

## 🔍 Affected Template Identification

### By Change Type

**NodeAdded / NodeUpdated / NodeRemoved**:
- SHOULD: Find templates referencing this node by URI
- SHOULD: Use namespace matching (same base URI)
- SHOULD: Include templates with SPARQL queries over this node

**EdgeAdded / EdgeUpdated / EdgeRemoved**:
- SHOULD: Find templates using this predicate
- SHOULD: Find templates connecting subject and object
- SHOULD: Include templates with SPARQL joins

**SchemaChanged**:
- SHOULD: Invalidate ALL templates (structural change)
- SHOULD: Prioritize by usage frequency
- SHOULD: Consider breaking changes vs additions

**TemplateChanged**:
- SHOULD: Include only the specified template
- SHOULD: Include all dependents of that template

**SHOULD EXPAND**:
- Use dependency graph to find transitive dependents
- Deduplicate template list
- Sort by priority/dependency order

**SHOULD NOT**:
- Miss affected templates (conservative approach)
- Regenerate unaffected templates (efficiency)
- Ignore dependency chains

---

## ⚡ Regeneration Execution Contract

### Regenerate Affected

**SHOULD CHOOSE**:
- Parallel regeneration if: incremental=true AND parallel_workers>1
- Sequential regeneration otherwise

**SHOULD DO**:
1. Update total_regenerations stat
2. Choose parallel or sequential strategy
3. Execute regeneration
4. Update success/failure stats
5. Log completion summary

---

### Parallel Regeneration

**SHOULD DO**:
1. Use futures::stream with buffer_unordered
2. Limit concurrency to parallel_workers
3. Clone self for each async task (Arc)
4. Regenerate each template across all target languages
5. Collect all results (don't short-circuit on failure)
6. Count successes and failures
7. Update stats atomically
8. Log summary (success/failed counts)

**PERFORMANCE TARGETS**:
- Should achieve near-linear speedup up to parallel_workers
- Should complete 10 templates in ~10 seconds (1s each)
- Should handle 100+ templates without memory issues

**SHOULD NOT**:
- Starve some templates (fair scheduling)
- Exceed parallel_workers limit
- Block on slow templates (use timeout)
- Fail entire batch on single failure

---

### Sequential Regeneration

**SHOULD DO**:
1. Iterate through affected templates
2. Regenerate each template
3. Update stats immediately after each
4. Continue despite failures
5. Log errors but don't panic

**WHEN TO USE**:
- Not in incremental mode
- Testing/debugging
- Single-threaded environments
- Low concurrency systems

---

### Regenerate Template

**SHOULD DO**:
1. Log template start
2. For each target language:
   - Call regenerate_for_language()
   - Handle language-specific errors
3. Update artifact metadata:
   - Set last_regenerated timestamp
   - Increment version if auto_version=true
4. Return success

**SHOULD UPDATE**:
- AffectedArtifact.last_regenerated
- AffectedArtifact.version (semantic versioning)

**SHOULD NOT**:
- Skip languages on failure
- Generate invalid code
- Update metadata on failure

---

### Regenerate For Language

**SHOULD DO**:
1. Log template + language
2. Generate template using TemplateGenerator
3. Validate generated code (syntax check)
4. Write to output_dir/{language}/{template_id}
5. Log success

**SHOULD GENERATE**:
- Syntactically valid code
- Properly formatted code
- Code with appropriate imports/dependencies
- Code matching template intent

**SHOULD HANDLE**:
- Language-specific formatting
- Language-specific types
- Language-specific conventions

**SHOULD NOT**:
- Generate code without validation
- Overwrite without backup
- Skip error handling

---

## 📊 Artifact Management Contract

### AffectedArtifact

**SHOULD TRACK**:
- `id`: Unique artifact identifier
- `template_id`: Source template
- `language`: Target language (rust, typescript, python)
- `output_path`: File path in output_dir
- `version`: Semantic version (major.minor.patch)
- `dependencies`: Other artifacts this depends on
- `last_regenerated`: Timestamp

**VERSION INCREMENT RULES**:
- PATCH: Bug fixes, template refinements
- MINOR: New features, backward-compatible changes
- MAJOR: Breaking changes (should be explicit)

**SHOULD PROVIDE**:
- Registration API: register_artifact()
- Query API: Get artifact by ID or template
- Dependency tracking: Link artifacts

---

## 📈 Statistics Contract

### RegenerationStats

**SHOULD TRACK**:
- `total_regenerations`: Count of all regeneration attempts
- `successful_regenerations`: Count of successes
- `failed_regenerations`: Count of failures
- `total_time_ms`: Cumulative execution time
- `events_processed`: Count of change events handled

**SHOULD CALCULATE**:
- Success rate: successful / total
- Average regeneration time: total_time_ms / total_regenerations
- Events per second: events_processed / uptime_seconds

**SHOULD PROVIDE**:
- get_stats(): Snapshot API
- Thread-safe access (RwLock)
- Serialization support

**SHOULD EMIT**:
- Log warnings if success rate < 90%
- Log warnings if avg time > threshold
- Telemetry events for dashboards

---

## 🧪 Testing Contract

### Unit Tests SHOULD:
- Test dependency graph operations (add, get_affected)
- Test affected template identification for each change type
- Test version increment logic
- Test artifact registration
- Mock LlmClient and GraphChangeNotifier

### Integration Tests SHOULD:
- Test end-to-end event processing
- Test parallel regeneration with real templates
- Verify file output creation
- Test dependency chain regeneration
- Measure actual parallelism

### Property Tests SHOULD:
- Verify dependency graph consistency
- Test with random change events
- Ensure no duplicates in affected templates
- Validate version semantics

---

## 🔒 Concurrency & Safety Contract

**SHOULD ENSURE**:
- Dependencies protected by RwLock
- Artifacts protected by RwLock
- Stats protected by RwLock
- All Arc references properly cloned
- No data races

**LOCKING STRATEGY**:
- Read locks for queries (identify_affected_templates)
- Write locks for mutations (update stats, register artifacts)
- Hold locks for minimal duration
- Avoid nested locks

**SHOULD NOT**:
- Hold locks across await points
- Create deadlocks
- Allow concurrent modification without locks

---

## 🎨 Error Handling Contract

**SHOULD DO**:
- Log errors with template_id, language, error details
- Continue processing other templates on failure
- Track failure counts in stats
- Return Result from all fallible operations
- Provide context in error messages

**SHOULD NOT**:
- Panic on template failures
- Silently drop errors
- Fail entire batch on single error
- Lose error information

---

## 🚀 Future Evolution Intent

### Phase 1: Basic Regeneration (Current)
- Event-driven regeneration
- Dependency tracking
- Parallel execution

### Phase 2: Incremental Optimization (Next)
- SHOULD: Cache partial results
- SHOULD: Skip unchanged templates
- SHOULD: Use incremental compilation
- SHOULD: Detect no-op changes

### Phase 3: Intelligent Regeneration (Future)
- SHOULD: Predict which templates will change
- SHOULD: Pre-generate common patterns
- SHOULD: Learn from regeneration patterns
- SHOULD: Optimize dependency graphs

### Phase 4: Distributed Regeneration (Long-term)
- SHOULD: Distribute across multiple nodes
- SHOULD: Use distributed dependency graph
- SHOULD: Coordinate via consensus protocol
- SHOULD: Support massive scale (10,000+ templates)

---

## 📝 Code Quality Standards

**Functions SHOULD**:
- Be <50 lines (identify_affected_templates is 55 - borderline)
- Have single responsibility
- Use descriptive names
- Include doc comments
- Return Result for fallible operations

**Algorithms SHOULD**:
- Document time complexity
- Avoid nested loops where possible
- Use efficient data structures
- Handle edge cases explicitly

**Configuration SHOULD**:
- Use builder pattern
- Validate on construction
- Provide sensible defaults
- Be serializable

---

## 🎯 Success Criteria

A well-implemented RegenerationEngine SHOULD:

✅ **Correctness**: Generate valid code 100% of the time
✅ **Efficiency**: Only regenerate affected templates
✅ **Performance**: Complete regeneration in <30 seconds for typical changes
✅ **Reliability**: 99%+ success rate
✅ **Observability**: Track all regenerations
✅ **Scalability**: Handle 1000+ templates
✅ **Testability**: Have 90%+ test coverage

---

## 🔧 Refactoring Guidance

When refactoring this file, preserve these key behaviors:

1. **Event subscription** - Always listen to GraphChangeNotifier
2. **Dependency tracking** - Maintain bidirectional dependency graph
3. **Parallel execution** - Process templates concurrently
4. **Version management** - Auto-increment versions
5. **Statistics tracking** - Record all operations

Improve these areas:

1. **Add caching** - Cache analysis results for common patterns
2. **Add validation** - Validate generated code before writing
3. **Improve identification** - Better heuristics for affected templates
4. **Add priority** - Regenerate critical templates first
5. **Add rollback** - Ability to revert failed regenerations
6. **Improve error recovery** - Retry transient failures
7. **Add progress tracking** - Report regeneration progress
8. **Add cost tracking** - Monitor resource usage

---

**END OF SHOULD DO DOCUMENTATION**
