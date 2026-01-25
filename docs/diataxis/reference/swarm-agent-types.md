<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Reference: Swarm Agent Types](#reference-swarm-agent-types)
  - [The Hive Mind Hierarchy](#the-hive-mind-hierarchy)
  - [Agent Type: Queen](#agent-type-queen)
    - [Role](#role)
    - [Responsibilities](#responsibilities)
    - [Characteristics](#characteristics)
    - [Responsibilities in Detail](#responsibilities-in-detail)
    - [Failure Modes](#failure-modes)
    - [Success Criteria](#success-criteria)
  - [Agent Type: Colony Leader](#agent-type-colony-leader)
    - [Role](#role-1)
    - [Responsibilities](#responsibilities-1)
    - [Characteristics](#characteristics-1)
    - [Typical Domains (in EPIC 9)](#typical-domains-in-epic-9)
    - [Colony Leader Lifecycle](#colony-leader-lifecycle)
    - [Responsibilities in Detail](#responsibilities-in-detail-1)
    - [Failure Modes](#failure-modes-1)
    - [Success Criteria](#success-criteria-1)
  - [Agent Type: Worker](#agent-type-worker)
    - [Role](#role-2)
    - [Responsibilities](#responsibilities-2)
    - [Characteristics](#characteristics-2)
    - [Worker Types (Examples)](#worker-types-examples)
    - [Worker Lifecycle](#worker-lifecycle)
    - [Worker Independence Requirements](#worker-independence-requirements)
    - [Failure Modes](#failure-modes-2)
    - [Success Criteria](#success-criteria-2)
  - [Agent Responsibilities Matrix](#agent-responsibilities-matrix)
  - [Communication Patterns](#communication-patterns)
    - [Queen ↔ Colony Leader](#queen--colony-leader)
    - [Colony Leader ↔ Worker](#colony-leader--worker)
    - [Across Colonies (Rare)](#across-colonies-rare)
  - [Scaling Considerations](#scaling-considerations)
    - [Small Tasks (1-2 hours)](#small-tasks-1-2-hours)
    - [Medium Tasks (4-8 hours)](#medium-tasks-4-8-hours)
    - [Large Tasks (Full feature implementation)](#large-tasks-full-feature-implementation)
  - [Performance Characteristics](#performance-characteristics)
    - [Ideal Performance (No Failures)](#ideal-performance-no-failures)
    - [With Failures](#with-failures)
  - [Comparison to Other Models](#comparison-to-other-models)
  - [Example: Feature Implementation with Hive Mind](#example-feature-implementation-with-hive-mind)
  - [Key Takeaways](#key-takeaways)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Reference: Swarm Agent Types

**Queen, Colony Leader, and Worker roles in multi-agent orchestration**

---

## The Hive Mind Hierarchy

```
        QUEEN
         / \
        /   \
    CL1     CL2     CL3
    /  \    /  \    / \
   W1  W2  W3  W4  W5 W6
```

- **Queen:** 1 (Decision authority)
- **Colony Leaders:** 3-5 (Tactical coordinators)
- **Workers:** Many (Task executors)

---

## Agent Type: Queen

### Role
Strategic coordinator, final decision maker, orchestrates entire operation

### Responsibilities
- **Plan** - Define overall strategy and task allocation
- **Assign** - Distribute work to colony leaders
- **Monitor** - Track progress and detect failures
- **Decide** - Make final decisions when disagreements arise
- **Synthesize** - Combine colony outputs into unified result

### Characteristics
| Trait | Value |
|-------|-------|
| Count | 1 |
| Failure Mode | Critical (system pauses) |
| Authority | Absolute |
| Scope | Strategic |
| Latency | Medium (makes decisions) |

### Responsibilities in Detail

**Planning Phase:**
```
"I need to implement Feature X"
  ↓
Analyze scope and complexity
  ↓
Break into independent sub-tasks
  ↓
Assign to 3 colonies
  ↓
Set success criteria
```

**Monitoring Phase:**
```
Track each colony's progress
  ↓
Detect blocked colonies
  ↓
Reallocate resources
  ↓
Adjust timeline
```

**Decision Phase:**
```
All colonies finish
  ↓
Results might disagree
  ↓
Evaluate quality/correctness
  ↓
Pick best or synthesize
```

### Failure Modes

| Failure | Impact | Recovery |
|---------|--------|----------|
| Queen dies | All work stops | Restart Queen, colonies buffer results |
| Queen is slow | Bottleneck | Add concurrent decision-making |
| Queen makes bad decision | Wrong direction | Improve scoring metrics |

### Success Criteria
- Clear strategy communicated
- Timely decisions (no stalling)
- Fair resource allocation
- Quality-based selection

---

## Agent Type: Colony Leader

### Role
Tactical coordinator for a specific domain, manages workers, reports to queen

### Responsibilities
- **Spawn** - Create and initialize worker agents
- **Assign** - Distribute subtasks to workers
- **Monitor** - Track worker progress and health
- **Report** - Status updates to Queen
- **Retry** - Reassign failed tasks
- **Buffer** - Hold results until Queen collects

### Characteristics
| Trait | Value |
|-------|-------|
| Count | 3-5 |
| Failure Mode | Recoverable (Queen reassigns) |
| Authority | Tactical (within domain) |
| Scope | Domain-specific |
| Latency | Low (parallel execution) |

### Typical Domains (in EPIC 9)

**Architecture Colony:**
- Designs system structure
- Spawns 3-5 architecture worker agents
- Each agent designs independently
- Returns best design to Queen

**Implementation Colony:**
- Writes code
- Spawns 3-5 rust-coder agents
- Each implements feature
- Returns implementations to Queen

**Testing Colony:**
- Writes and runs tests
- Spawns 3-5 test-engineer agents
- Each writes test suite
- Returns test coverage to Queen

**Review Colony:**
- Reviews code quality
- Spawns 3-5 reviewer agents
- Each audits different aspects
- Returns findings to Queen

### Colony Leader Lifecycle

```
START
  ↓
Receive assignment from Queen
  ↓
Analyze task complexity
  ↓
Spawn worker agents
  ↓
Monitor progress
  ├─ Worker succeeds → Buffer result
  ├─ Worker fails → Reassign
  └─ Timeout → Declare failure
  ↓
Collect all results
  ↓
Report to Queen
  ↓
END
```

### Responsibilities in Detail

**Worker Spawning:**
```rust
// Colony leader spawns workers for parallel execution
for i in 0..worker_count {
    let worker_id = WorkerId(i);
    let task = Task {
        feature: "implement_feature_x",
        approach: approaches[i],
    };
    spawn_worker(worker_id, task);
}
```

**Progress Monitoring:**
```rust
// Track each worker's status
loop {
    for worker in workers {
        match worker.status() {
            Status::Running => continue,
            Status::Success(result) => buffer_result(result),
            Status::Failed(err) => {
                reassign_to_backup_worker();
                continue;
            }
            Status::Timeout => declare_failed(),
        }
    }

    if all_workers_done() {
        break;
    }
}
```

**Reporting:**
```rust
// Report to Queen with results and metadata
let report = ColonyReport {
    colony: ColonyType::Implementation,
    status: Status::Success,
    result_count: 3,
    results: [result1, result2, result3],
    execution_time: 45.seconds(),
    failures: 0,
};

queen.report(report);
```

### Failure Modes

| Failure | Impact | Recovery |
|---------|--------|----------|
| Worker dies | Task incomplete | Reassign to backup worker |
| All workers fail | Colony fails | Queen reassigns to another colony |
| Slow workers | Bottleneck | Increase parallelism |
| Bad task allocation | Uneven load | Better work distribution |

### Success Criteria
- All workers complete on time
- Minimize worker failures
- Maximize result quality
- Clear reporting to Queen

---

## Agent Type: Worker

### Role
Execute specific, well-defined task independently

### Responsibilities
- **Execute** - Perform assigned task
- **Report** - Communicate results
- **Handle errors** - Graceful failure
- **Stay independent** - Don't depend on other workers

### Characteristics
| Trait | Value |
|-------|-------|
| Count | Many (3-10 per colony) |
| Failure Mode | Contained (reassignable) |
| Authority | None (execute only) |
| Scope | Task-specific |
| Latency | Low (specialized) |

### Worker Types (Examples)

**Implementation Worker:**
```
Input: Feature spec + implementation approach
Task: Write Rust code for feature
Output: Completed implementation
Constraints: Must compile, pass tests, zero warnings
```

**Test Engineer Worker:**
```
Input: Feature spec + code
Task: Write comprehensive test suite
Output: Tests with high coverage + property-based tests
Constraints: Tests must pass, no flakiness
```

**Reviewer Worker:**
```
Input: Code to review + review aspect (security/performance/style)
Task: Audit code for aspect
Output: Findings and recommendations
Constraints: Must follow rubric, be objective
```

**Architect Worker:**
```
Input: Design problem + architectural approach
Task: Design system structure
Output: Architecture document
Constraints: Must consider tradeoffs
```

### Worker Lifecycle

```
START
  ↓
Receive task from Colony Leader
  ↓
Understand requirements
  ↓
Execute task
  ├─ All good → Return success
  ├─ Problem → Return error
  └─ Timeout → Get killed
  ↓
Report results to Colony Leader
  ↓
END (ready for next task or shutdown)
```

### Worker Independence Requirements

Workers **must be independent:**

```
❌ Worker A depends on Worker B's output
   Problem: Can't run in parallel

✅ Each worker has all needed inputs
   Benefit: Fully parallel
```

### Failure Modes

| Failure | Impact | Recovery |
|---------|--------|----------|
| Worker timeout | Task incomplete | Reassign to next worker |
| Worker crashes | Task lost | Reassign |
| Poor quality result | Wrong direction | Queen picks better result |
| Worker is slow | Delays report | Other workers finish first |

### Success Criteria
- Task completed
- Output meets specification
- Completes within time budget
- Clear, actionable results

---

## Agent Responsibilities Matrix

| Task | Queen | Colony Leader | Worker |
|------|-------|---|--------|
| Plan strategy | ✓ | - | - |
| Spawn agents | ✓ | ✓ | - |
| Assign work | ✓ | ✓ | - |
| Execute task | - | - | ✓ |
| Monitor progress | ✓ | ✓ | - |
| Handle failures | ✓ | ✓ | Partially |
| Report results | ✓ | ✓ | ✓ |
| Make decisions | ✓ | Limited | - |

---

## Communication Patterns

### Queen ↔ Colony Leader
```
Queen → "Implement feature X, spawn 5 workers"
Queen → Monitor progress
Colony → "3 workers done, 2 still running"
Colony → "All done, 3 implementations ready"
Queen → "Evaluate and pick best"
```

### Colony Leader ↔ Worker
```
Leader → "Implement feature X using approach A"
Leader → Monitor progress
Worker → "Error on line 45, retrying"
Worker → "Success, 347 tests passing"
Leader → Buffer result
```

### Across Colonies (Rare)
```
Typically: NO direct worker-to-worker communication
Exception: Status reporting to Queen for cross-colony decisions
```

---

## Scaling Considerations

### Small Tasks (1-2 hours)
```
1 Queen
└─ 1 Colony Leader
   └─ 3 Workers
```

### Medium Tasks (4-8 hours)
```
1 Queen
├─ 3 Colony Leaders
│  ├─ 5 Workers each
```

### Large Tasks (Full feature implementation)
```
1 Queen
├─ Architecture Colony (3 workers)
├─ Implementation Colony (5 workers)
├─ Testing Colony (5 workers)
└─ Review Colony (3 workers)
Total: 1 Queen + 4 Leaders + 16 Workers
```

---

## Performance Characteristics

### Ideal Performance (No Failures)
```
Sequential (1 agent): T
Parallel (10 workers): T/10
Hive Mind (1Q + 4CL + 10W): T/10 (with orchestration overhead)

Overhead: ~5-10% for coordination
Speedup: 8-9x vs sequential
```

### With Failures
```
Failure rate: 10%
Without retry: 1 of 10 fails, group fails
With Colony Leader retry: Reassign to backup
With Queen oversight: Reallocate work

Recovery time: 1-2 seconds per failure
```

---

## Comparison to Other Models

| Model | Parallelism | Fault Tolerance | Coordination Overhead | Best For |
|-------|---|---|---|---|
| Sequential | None | N/A | N/A | Simple tasks |
| Peer (all equal) | High | High | Very High | Leaderless consensus |
| Hive Mind | High | Medium | Medium | Parallel + predictable |
| Master-Worker | High | Low | Low | Embarrassingly parallel |

---

## Example: Feature Implementation with Hive Mind

```
QUEEN: "Implement authentication feature"
  ↓
Architecture Colony Leader spawns 3 architects:
  - Architect 1: JWT-based approach
  - Architect 2: Session-based approach
  - Architect 3: OAuth approach
  ↓
Implementation Colony Leader spawns 5 engineers:
  - Engineer 1: Implement architecture A variant 1
  - Engineer 2: Implement architecture A variant 2
  - Engineer 3: Implement architecture B variant 1
  - Engineer 4: Implement architecture B variant 2
  - Engineer 5: Implement architecture C
  ↓
Testing Colony Leader spawns 5 testers:
  - Tester 1: Unit tests
  - Tester 2: Integration tests
  - Tester 3: Security tests
  - Tester 4: Performance tests
  - Tester 5: Property-based tests
  ↓
Review Colony Leader spawns 3 reviewers:
  - Reviewer 1: Security audit
  - Reviewer 2: Performance audit
  - Reviewer 3: Code style audit
  ↓
QUEEN collects results:
  - 3 architecture proposals
  - 5 implementations
  - Comprehensive test coverage
  - 3 review reports
  ↓
QUEEN synthesizes:
  - Picks best architecture
  - Picks best implementation
  - Confirms test coverage
  - Addresses review findings
  ↓
Result: Complete, tested, reviewed feature
```

---

## Key Takeaways

1. **Queen = Strategic Authority** - Makes decisions, orchestrates
2. **Colony Leader = Tactical Coordinator** - Manages workers, reports progress
3. **Worker = Task Executor** - Focused, independent, specialized
4. **Communication flows up the hierarchy** - Workers → Leaders → Queen
5. **Independence enables parallelism** - Workers don't depend on each other
6. **Failures are recoverable** - Colonies retry, Queen reallocates

---

## References

- [Hive Mind Swarm 101](../tutorials/01-hive-mind-swarm-101.md)
- [Why Hive Mind Coordinates](../explanations/why-hive-mind-coordinates.md)
- [Byzantine Fault Tolerance](https://en.wikipedia.org/wiki/Byzantine_fault)
