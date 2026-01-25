# Pseudocode: Parallel Build Orchestration DAG

**SPARC Phase**: Pseudocode
**Algorithm**: Parallel Task Dependency Resolution
**Complexity**: O(n log n) where n = number of build tasks
**Status**: Formal specification complete

---

## Algorithm 1: Parallel Build Task Orchestration

### Function Signature
```
FUNCTION parallelize_build_tasks(tasks: Vec<BuildTask>, max_parallel: uint) -> Result<BuildExecution>
    Precondition: tasks.len() > 0
    Postcondition: All tasks executed respecting dependencies
    Returns: BuildExecution with timing and results
```

### Pseudocode (Detailed)

```
FUNCTION parallelize_build_tasks(tasks: Vec<BuildTask>, max_parallel: uint) -> Result<BuildExecution>
    // Phase 1: Build Dependency Graph
    graph = build_dependency_graph(tasks)  // O(n log n)

    IF has_cycle(graph):
        RETURN Error("Circular dependency detected")
    END IF

    ready_tasks = find_tasks_with_no_dependencies(graph)  // O(n)
    execution = BuildExecution.new()
    in_flight = Set<TaskId>.new()
    completed = Set<TaskId>.new()

    // Phase 2: Execute with Parallelism
    WHILE completed.size() < tasks.len():
        // Spawn up to max_parallel tasks
        can_spawn = max_parallel - in_flight.size()
        to_spawn = ready_tasks.take(can_spawn)  // O(max_parallel)

        IF to_spawn.is_empty() AND in_flight.is_empty():
            RETURN Error("Deadlock: tasks remain but none ready")
        END IF

        FOR EACH task IN to_spawn:
            handle = spawn_task_async(task)  // Start concurrent execution
            in_flight.insert(task.id)
            ready_tasks.remove(task.id)
        END FOR

        // Wait for at least one task to complete
        (completed_id, result) = wait_for_any(in_flight)  // O(1) amortized
        in_flight.remove(completed_id)
        completed.insert(completed_id)

        // Handle result
        IF result.is_error():
            // Decision: Fail-fast or fail-late
            IF fail_fast_enabled:
                RETURN Error(result)  // Stop immediately
            ELSE:
                log_error(completed_id, result)
                // Continue with other tasks
            END IF
        ELSE:
            execution.record_success(completed_id, result)
        END IF

        // Update ready_tasks: find newly-ready tasks
        new_ready = find_dependents(graph, completed_id)  // O(degree)
        FOR EACH dependent IN new_ready:
            IF all_dependencies_satisfied(graph, dependent, completed):
                ready_tasks.insert(dependent)
            END IF
        END FOR
    END WHILE

    // Phase 3: Return execution summary
    RETURN execution.finalize()  // Compute total time, etc.
END FUNCTION
```

### Complexity Analysis

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| Build dependency graph | O(n log n) | Topological sort |
| Initial ready detection | O(n) | Single pass |
| Task spawning (per cycle) | O(max_parallel) | Bounded by concurrency limit |
| Wait for completion | O(1) amortized | OS event queue |
| Dependent detection | O(degree) | For each completed task |
| **Total (n tasks, p parallel)** | **O(n log n + (n/p) * degree)** | Ideal: O(n log n) + O(n) *= O(n log n)* |

### Data Structures

```
STRUCT BuildTask:
    id: TaskId
    name: String
    command: String
    timeout_ms: u64
    dependencies: Set<TaskId>
    retries: u32
    criticality: Criticality  // Low, Medium, High, Critical

ENUM Criticality:
    Low      // Can skip if time-constrained
    Medium   // Should run
    High     // Must run
    Critical // Fail-fast if error

STRUCT BuildExecution:
    start_time: Timestamp
    end_time: Timestamp
    tasks_completed: u32
    tasks_failed: u32
    results: Map<TaskId, ExecutionResult>
    total_duration_ms: u64
    critical_path_ms: u64

STRUCT ExecutionResult:
    task_id: TaskId
    status: Status  // Success, Failure, Timeout, Skipped
    duration_ms: u64
    output: String
    exit_code: i32
```

---

## Algorithm 2: Critical Path Identification

### Purpose
Identify the longest path through the DAG to predict minimum execution time

### Function Signature
```
FUNCTION identify_critical_path(graph: DependencyGraph) -> Vec<TaskId>
    Precondition: No cycles in graph
    Postcondition: Returns longest path by execution time
    Returns: Sequence of task IDs on critical path
```

### Pseudocode

```
FUNCTION identify_critical_path(graph: DependencyGraph) -> Vec<TaskId>
    // Dynamic programming approach: topological sort + memoization
    topo_order = topological_sort(graph)  // O(n log n)
    critical_lengths = Map<TaskId, u64>.new()
    predecessors = Map<TaskId, TaskId>.new()

    // Iterate in topological order (reverse)
    FOR EACH task_id IN reverse(topo_order):
        task = graph.get_task(task_id)

        // Find max critical length among dependencies
        max_dep_length = 0
        max_dep_task = null

        FOR EACH dependency_id IN task.dependencies:
            dep_length = critical_lengths.get(dependency_id)
            IF dep_length > max_dep_length:
                max_dep_length = dep_length
                max_dep_task = dependency_id
            END IF
        END FOR

        // Critical length = max dependency length + this task's duration
        critical_lengths.insert(task_id, max_dep_length + task.duration_ms)
        predecessors.insert(task_id, max_dep_task)
    END FOR

    // Reconstruct critical path
    start_task = task_with_max_critical_length(critical_lengths)
    critical_path = Vec::new()
    current = start_task

    WHILE current != null:
        critical_path.push(current)
        current = predecessors.get(current)
    END WHILE

    RETURN critical_path
END FUNCTION
```

### Critical Path Example (ggen workspace)

```
Typical ggen pre-commit workflow:
├─ fmt (5s) ─┐
├─ lint (90s)├─ all_pass (0s) ─┐
├─ test-unit (45s)─┤           │
└─ test-doc (15s) ─┘           └─ complete

Critical Path: fmt → lint → all_pass → complete
Total: 5 + 90 + 0 = 95s (not 5 + 90 + 45 + 15 = 155s sequential)

Parallelization Gain: 155s / 95s = 1.63x speedup
Theoretical Max: If all could run in parallel = 90s (limited by lint)
Achieved: 95s (lint bottleneck)
Efficiency: 90/95 = 94.7% (excellent)
```

---

## Algorithm 3: Timeout Management with Cascading Fallbacks

### Purpose
Handle timeout scenarios gracefully with retry logic

### Function Signature
```
FUNCTION execute_task_with_timeout(task: BuildTask, fallback_strategies: Vec<Strategy>) -> Result<ExecutionResult>
    Precondition: task.timeout_ms > 0
    Postcondition: Task executed with retry fallbacks
    Returns: ExecutionResult or Error
```

### Pseudocode

```
FUNCTION execute_task_with_timeout(task: BuildTask, strategies: Vec<Strategy>) -> Result<ExecutionResult>

    FOR EACH (attempt, strategy) IN strategies.enumerate():
        adjusted_timeout = task.timeout_ms * strategy.timeout_multiplier

        TRY:
            // Execute with adjusted timeout
            process = spawn_process(task.command, adjusted_timeout)
            result = wait_for_process(process, adjusted_timeout)

            IF result.is_success():
                RETURN Ok(result)
            ELSE IF result.is_timeout() AND attempt < strategies.len() - 1:
                log_warning("Task timeout on attempt {}, trying strategy: {}",
                           attempt + 1, strategy.name)
                CONTINUE  // Try next strategy
            ELSE:
                RETURN Err(result.error)
            END IF
        CATCH timeout_exception:
            IF attempt < strategies.len() - 1:
                log_warning("Timeout caught, retrying with {} strategy", strategy.name)
                CONTINUE
            ELSE:
                RETURN Err("Task exceeded maximum timeout")
            END IF
        END TRY
    END FOR

    RETURN Err("All timeout strategies exhausted")
END FUNCTION

STRUCT Strategy:
    name: String                    // "quick", "normal", "patient"
    timeout_multiplier: f64         // 1.0x, 2.0x, 4.0x
    description: String
```

### Example: ggen cargo make check

```
Base timeout: 15s
Strategies:
1. Quick (1.0x) = 15s
2. Normal (4.0x) = 60s
3. Patient (8.0x) = 120s

Scenario: Lock contention on 30-crate workspace
├─ Attempt 1: 15s timeout → TIMEOUT
├─ Fall back to Normal strategy
├─ Attempt 2: 60s timeout → SUCCESS (51s actual)
└─ Result: SUCCESS after 1 retry
```

---

## Algorithm 4: Cache Invalidation Decision Tree

### Purpose
Determine when to rebuild tasks based on source changes

### Function Signature
```
FUNCTION should_rebuild_task(task: BuildTask, source_cache: SourceHashCache) -> bool
    Precondition: task has defined source_patterns (e.g., "*.rs")
    Postcondition: Returns true if rebuild needed
    Returns: bool indicating rebuild necessity
```

### Pseudocode

```
FUNCTION should_rebuild_task(task: BuildTask, source_cache: SourceHashCache) -> bool

    // Step 1: Check if cache exists
    cached_hash = source_cache.get(task.id)
    IF cached_hash == null:
        RETURN true  // No cache = always rebuild
    END IF

    // Step 2: Collect all source files
    source_files = glob_all(task.source_patterns)  // E.g., "crates/ggen-core/src/**/*.rs"

    // Step 3: Compute current source hash
    current_hash = sha256_combine([
        sha256_file(f) FOR EACH f IN source_files
    ])  // O(num_files * avg_file_size)

    // Step 4: Compare hashes
    IF current_hash != cached_hash:
        source_cache.update(task.id, current_hash)
        RETURN true  // Source changed = rebuild
    END IF

    // Step 5: Check dependencies
    // If any dependency was rebuilt, this task must rebuild too
    FOR EACH dep_task_id IN task.dependencies:
        IF was_rebuilt(dep_task_id):
            RETURN true
        END IF
    END FOR

    // Step 6: All checks passed
    RETURN false  // No rebuild needed
END FUNCTION
```

### Cache Format

```
JSON file: .cargo/.ggen-build-cache

{
  "version": "1.0",
  "timestamp": "2026-01-25T12:00:00Z",
  "entries": {
    "fmt": {
      "hash": "abc123def456...",
      "timestamp": "2026-01-25T11:55:00Z",
      "source_pattern": "**/*.rs"
    },
    "lint": {
      "hash": "xyz789...",
      "timestamp": "2026-01-25T11:56:00Z",
      "source_pattern": "**/*.rs"
    },
    "test-unit": {
      "hash": "def456...",
      "timestamp": "2026-01-25T11:57:00Z",
      "source_pattern": ["**/*.rs", "**/tests/**/*.rs"]
    }
  }
}
```

---

## Performance Predictions

### Scenario 1: Clean Build (Worst Case)
```
Tasks: fmt (5s), lint (90s), test-unit (45s), test-doc (15s)
Parallelism: 4 concurrent
Strategy: Execute all in parallel

Timeline:
├─ t=0s:    Spawn all 4 tasks
├─ t=5s:    fmt completes
├─ t=15s:   test-doc completes
├─ t=45s:   test-unit completes
├─ t=90s:   lint completes (critical path)
└─ t=90s:   All complete

Total: 90s (vs 155s sequential)
Speedup: 1.7x
Efficiency: 155s / (4 × 90s) = 43% CPU utilization (4 cores busy 90s = 360 core-seconds)
```

### Scenario 2: Incremental Build (Cached)
```
Assume: No source changes, cache hits for fmt, test-unit, test-doc

Timeline:
├─ t=0s:    Spawn lint (cannot cache)
├─ t=90s:   lint completes (critical path, cannot skip)
└─ t=90s:   All complete (skipped 3 tasks)

Total: 90s (best case with current strategy)
Note: Lint cannot be cached due to dependency on all source files
Optimization: Consider lint-result caching for deterministic projects
```

### Scenario 3: Failure Path (Fast Fail)
```
Assume: fmt passes, lint fails early (15s into execution)

Timeline:
├─ t=0s:    Spawn all 4 tasks
├─ t=5s:    fmt completes
├─ t=15s:   lint fails (fast failure)
└─ t=15s:   Abort execution (fail-fast)

Total: 15s
Behavior: Stops all in-flight tasks, returns error immediately
Benefit: Developers see failures 6x faster than sequential approach
```

---

## Implementation Notes

### For ggen Makefile.toml
- Task dependencies defined via `run_task` and `dependencies` fields
- Timeout enforcement via `timeout` command wrapper
- Parallelism via concurrent task spawning

### Critical Considerations
1. **Lock Contention**: Cargo.lock file access serializes some builds
2. **Memory Pressure**: Too many parallel tasks can exceed system limits
3. **Cache Invalidation**: Conservative approach (always rebuild on any change)
4. **Idempotency**: All tasks must be idempotent (safe to run multiple times)

### Testing Strategy
- Benchmark clean build: Should match theoretical minimum
- Benchmark incremental: Verify cache effectiveness
- Benchmark failure: Verify fail-fast behavior
- Stress test: 100+ task DAG, verify no deadlocks

---

## References

- **Wikipedia**: https://en.wikipedia.org/wiki/Directed_acyclic_graph
- **Topological Sort**: O(n + e) where e = edges
- **Critical Path Method (CPM)**: Project management technique using DAGs
- **Cargo Incremental Compilation**: Rust's built-in caching mechanism
