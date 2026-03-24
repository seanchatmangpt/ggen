# Migration Guide: Adopting Agent-Based Systems

## Table of Contents

1. [Architectural Differences](#architectural-differences)
2. [Development Process Changes](#development-process-changes)
3. [Testing Strategy Migration](#testing-strategy-migration)
4. [Deployment Considerations](#deployment-considerations)
5. [Performance Expectations](#performance-expectations)
6. [Common Challenges](#common-challenges)

---

## Executive Summary

Migrating from traditional (request-response, centralized) systems to agent-based systems is **a paradigm shift**, not just a code refactor. This guide helps teams understand the differences, plan migration strategy, and avoid common pitfalls.

**Timeline:** 3-6 months for full adoption (depends on team size and complexity)

**Expected Benefits:**
- Increased resilience (Byzantine fault tolerance)
- Better scalability (distributed execution)
- Observable behavior (cryptographic receipts)
- Lower latency (parallel task execution)
- Improved development velocity (generated code, TDD early)

**Migration Costs:**
- Learning curve (~2-4 weeks)
- Test rewrite (~20% of effort)
- Operational complexity (consensus, domains)
- Monitoring/observability investment

---

## Architectural Differences

### Traditional (Monolithic)

```
┌─────────────────────────────────────────┐
│          Monolithic Application         │
├─────────────────────────────────────────┤
│  User Request Handler                   │
│      ↓ (synchronous)                    │
│  Business Logic Layer                   │
│      ↓ (synchronous)                    │
│  Database Layer                         │
│      ↓ (synchronous)                    │
│  Response to User                       │
└─────────────────────────────────────────┘

Characteristics:
- Single process, single point of failure
- Synchronous request-response
- Tight coupling between layers
- Database as single source of truth
- Vertical scaling (bigger servers)
- Easy to debug (full stack trace)
```

### Agent-Based (Distributed)

```
┌──────────────────────────────────────────────────────────┐
│              Distributed Agent System                    │
├────────┬─────────┬─────────┬────────────┬────────────────┤
│ Agent 1│ Agent 2 │ Agent 3 │ Consensus │ Life Domain    │
│        │         │         │ Cluster   │ Executor       │
├────────┴─────────┴─────────┴────────────┴────────────────┤
│ Task Queue | Message Bus | Event Stream | RDF Ontology  │
├──────────────────────────────────────────────────────────┤
│ Persistent Receipt Log | Cryptographic Verification      │
└──────────────────────────────────────────────────────────┘

Characteristics:
- Multiple agents, distributed execution
- Asynchronous message-oriented
- Loose coupling, well-defined interfaces
- RDF specifications as source of truth
- Horizontal scaling (more agents)
- Harder to debug (distributed tracing needed)
```

### Key Differences Table

| Aspect | Traditional | Agent-Based |
|--------|-------------|------------|
| **Control Flow** | Synchronous (stack traces) | Asynchronous (message-driven) |
| **Failure Model** | Crashes entire request | Graceful degradation |
| **Scaling** | Vertical (bigger machine) | Horizontal (more agents) |
| **Source of Truth** | Database schema | RDF specifications |
| **Testing** | Integration heavy | Chicago TDD (unit/state) |
| **Debugging** | Simple (full stack trace) | Complex (distributed tracing) |
| **Verification** | ACID transactions | Byzantine consensus |
| **Observability** | Logs + metrics | Logs + metrics + receipts |
| **Development** | Code-first | Spec-first |

---

## Development Process Changes

### Traditional Development Cycle

```
1. Write requirements (English)
   ↓
2. Design database schema (SQL DDL)
   ↓
3. Write business logic (Java/Python/C#)
   ↓
4. Write integration tests (mocks + DB)
   ↓
5. Deploy to production
   ↓
6. Debug in production (logs + metrics)
```

**Issues:**
- Requirements diverge from code
- Schema changes break compatibility
- Tests use mocks (not real behavior)
- Debugging reactive (after failure)

### Agent-Based Development Cycle (ggen)

```
1. Write RDF specification (.ttl file)
   └─ Single source of truth
   └─ Expresses semantic relationships
   └─ SPARQL queries extract features
   ↓
2. Write Tera templates for code generation
   └─ Define structure once
   └─ Reused across languages
   ↓
3. Run code generation pipeline (μ₁-μ₅)
   └─ Parse → Validate → Query → Transform → Codegen
   ↓
4. Write Chicago TDD tests
   └─ Real collaborators (no mocks)
   └─ State-based assertions
   └─ AAA pattern (Arrange/Act/Assert)
   ↓
5. Run full validation suite
   └─ Compilation check
   └─ All tests pass
   └─ Performance SLOs met
   └─ Receipt verification
   ↓
6. Deploy with cryptographic proof
   └─ Receipt generated
   └─ Consensus verification
   └─ Audit trail created
```

**Benefits:**
- Spec stays in sync with code (generated)
- Tests use real objects (Chicago TDD)
- Debugging proactive (receipts + consensus)
- Generation prevents mistakes

### Migration Strategy for Development

**Phase 1 (Week 1-2): Learning**
- Team studies ggen examples (ARCHITECTURE.md, PATTERNS.md)
- Learn RDF/TTL basics (simple examples first)
- Learn Chicago TDD (vs mocks)
- Read GETTING_STARTED.md

**Phase 2 (Week 3-4): Pilot Project**
- Choose smallest module to rewrite as agent-based
- Write RDF spec (understand what matters)
- Generate code from spec
- Write Chicago TDD tests
- Deploy and monitor

**Phase 3 (Month 2-3): Expand Scope**
- Migrate 2-3 more modules
- Train additional team members
- Establish patterns and conventions
- Build shared template library

**Phase 4 (Month 4-6): Full Migration**
- Migrate remaining modules
- Consolidate learning
- Document team patterns
- Optimize based on production experience

---

## Testing Strategy Migration

### Traditional Testing

```
Unit Tests (70%)
├─ Mock everything external
├─ Fast execution
├─ Don't test real behavior
└─ High maintenance (mock updates)

Integration Tests (20%)
├─ Real database
├─ Mock external services
├─ Slower execution
└─ Test seams between components

E2E Tests (10%)
├─ Full system
├─ Against real external services
├─ Slow execution
├─ Brittle (timing dependencies)
└─ Only critical paths

Problem: Low confidence in real behavior
Most bugs found in production, not tests
```

### Chicago TDD (Agent-Based)

```
Unit Tests (85%)
├─ Real collaborators (no mocks)
├─ State-based assertions
├─ Fast execution
├─ High confidence in real behavior
└─ Low maintenance

Integration Tests (10%)
├─ Full workflow testing
├─ Byzantine fault injection
├─ Consensus verification
├─ Deterministic reproduction

Property Tests (5%)
├─ Edge case discovery
├─ Invariant verification
├─ Random input generation
└─ Find corner cases

Benefit: High confidence, fewer production bugs
```

### Testing Migration Steps

**Step 1: Learn Chicago TDD**

```rust
// Bad: Mock-based (London TDD)
#[test]
fn test_user_creation() {
    let mock_db = MockUserDb::new();
    mock_db.expect_save.times(1).return(Ok(()));
    let user = create_user(&mock_db, "Alice");
    assert!(mock_db.save.called());  // Interaction test
}

// Good: State-based (Chicago TDD)
#[test]
fn test_user_creation() {
    // Real in-memory database (no mocks)
    let db = InMemoryUserDb::new();
    
    // Act
    create_user(&db, "Alice").expect("creation failed");
    
    // Assert on state
    let user = db.get("Alice").expect("not found");
    assert_eq!(user.name, "Alice");
}
```

**Step 2: Replace Mocks Incrementally**

```rust
// Transition: Keep mock for external only
#[tokio::test]
async fn test_agent_task_execution() {
    // Real: In-process task executor
    let executor = TaskExecutor::new();
    
    // Real: In-memory domain storage
    let domain = LifeDomain::new("health");
    
    // Mock: Only real external (network)
    let mock_consensus = MockConsensus::new();
    mock_consensus.expect_verify.return(Ok(true));
    
    // Act
    executor.execute(&domain, &mock_consensus).await.expect("failed");
    
    // Assert
    assert_eq!(domain.tasks.len(), 1);
    assert_eq!(domain.tasks[0].state, TaskState::Completed);
}
```

**Step 3: Add Consensus Verification**

```rust
#[tokio::test]
async fn test_agent_with_real_consensus() {
    // Create test consensus cluster
    let consensus = TestConsensus::new(4);  // 4 replicas
    consensus.inject_byzantine_node(1);     // Node 1 is Byzantine
    
    let executor = TaskExecutor::new();
    let domain = LifeDomain::new("health");
    
    // Execute task
    executor.execute(&domain, &consensus).await.expect("failed");
    
    // Verify consensus reached (despite Byzantine node)
    let receipt = consensus.get_receipt(&domain.task_id()).unwrap();
    assert!(receipt.verify_signatures());
    assert_eq!(receipt.committed_replicas, 3);  // 2f+1 = 3
}
```

---

## Deployment Considerations

### Traditional Deployment

```
1. Build artifact (Docker image or binary)
2. Push to repository
3. Deploy to staging
4. Manual testing
5. Deploy to production
6. Monitor logs and metrics
7. Rollback if needed (manual)

Issues:
- Slow feedback (hours)
- No cryptographic proof
- Manual rollback error-prone
- Difficult to audit "what ran"
```

### Agent-Based Deployment

```
1. Update RDF specification (.ttl)
2. Run ggen validate (SHACL constraints)
3. Run ggen generate (code generation)
4. Run cargo make pre-commit (all tests pass)
5. Run ggen sync --audit true (full pipeline)
   └─ Byzantine consensus verification
   └─ Receipt generation
   └─ Audit trail creation
6. Deploy with receipt
   └─ Cryptographic proof of what ran
   └─ Immutable audit trail
7. Monitor + automatic rollback on SLO breach

Benefits:
- Fast feedback (minutes)
- Cryptographic proof
- Automatic verification
- Complete audit trail
```

### Kubernetes Deployment Changes

**Traditional:**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: api-service
spec:
  replicas: 3
  template:
    spec:
      containers:
      - name: api
        image: myapp:v1.2.3
        ports:
        - containerPort: 8080
```

**Agent-Based:**
```yaml
apiVersion: apps/v1
kind: StatefulSet  # Agents need identity
metadata:
  name: ggen-agents
spec:
  serviceName: ggen-agents
  replicas: 5
  template:
    spec:
      containers:
      - name: agent
        image: ggen-agent:v6.0.0
        env:
        - name: AGENT_ROLE
          value: "executor"  # or "consensus"
        - name: DOMAIN
          value: "health"
        - name: RECEIPT_ENABLED
          value: "true"
        ports:
        - name: task
          containerPort: 8080
        - name: consensus
          containerPort: 5000
        livenessProbe:
          httpGet:
            path: /health/alive
            port: task
          initialDelaySeconds: 10
          periodSeconds: 5
---
apiVersion: v1
kind: Service
metadata:
  name: ggen-agents
spec:
  clusterIP: None  # Headless service for peer discovery
  selector:
    app: ggen-agents
  ports:
  - port: 8080
    name: task
  - port: 5000
    name: consensus
```

---

## Performance Expectations

### Traditional System Performance

```
Operation                    Traditional  Target
────────────────────────────────────────────────
HTTP Request                 100-500ms    < 1s
Database query               10-100ms     < 100ms
Transaction (multi-step)     500-2000ms   < 2s
Throughput (requests/sec)    100-1000     N/A
────────────────────────────────────────────────

Characteristics:
- Low latency for simple operations
- Higher latency for complex operations
- Single point of failure (cascading)
- Vertical scaling limits
```

### Agent-Based System Performance

```
Operation                           Agent    Target
───────────────────────────────────────────────────
Task creation                       50ms     < 100ms
Task assignment to agent            10ms     < 50ms
Task execution (parallel)           20-100ms < 500ms
Consensus round (3f+1 replicas)     100-300ms < 1s
Receipt verification (Ed25519)      5-20ms   < 50ms
Total workflow (goal)               300-1000ms < 2s
Throughput (tasks/sec)              50-500   > 100
───────────────────────────────────────────────────

Characteristics:
- More latency overhead (messaging)
- But parallel execution reduces end-to-end
- Byzantine resilience adds consensus delay
- Scales horizontally (add agents)
- No single point of failure
```

### Performance Optimization Tips

**Agent-Based systems optimize differently:**

| Optimization | Traditional | Agent-Based |
|--------------|-------------|------------|
| **Caching** | Session cache, Redis | Task result cache, domain state |
| **Batching** | Database bulk ops | Consensus round batching (10 tasks) |
| **Load balancing** | Round-robin HTTP | Work queue based on domain load |
| **Scaling** | Add server RAM/CPU | Add agents horizontally |
| **Monitoring** | APM (New Relic, etc.) | Receipt + consensus metrics |

---

## Common Challenges

### Challenge 1: "Async is Hard"

**Problem:** Developers used to synchronous code struggle with async.

**Traditional:**
```rust
let user = db.get_user(id);           // Blocks until ready
let orders = db.get_orders(user.id);  // Then this runs
let total = sum_orders(&orders);      // Stack trace shows all
```

**Agent-Based:**
```rust
// Must think differently - futures, spawning, awaiting
let user_handle = tokio::spawn(fetch_user(id));
let orders_handle = tokio::spawn(fetch_orders(id));
let (user, orders) = tokio::join!(user_handle, orders_handle);
// Stack trace harder to follow
```

**Solution:**
- Start with examples
- Use `#[tokio::test]` for deterministic testing
- Learn futures combinator patterns
- Use IDE with async debugging support
- **Key:** Async is worth it for resilience

### Challenge 2: "Distributed Tracing is Complex"

**Problem:** Debugging distributed systems requires different tools.

**Traditional:** Print stack trace, read logs

**Agent-Based:** Distributed tracing with correlation IDs

**Solution:**
```bash
# Enable tracing
RUST_LOG=ggen_a2a=debug TRACE_ID=abc123 cargo run

# Follow same correlation ID across services
grep "TRACE_ID=abc123" /var/log/ggen/*.log

# Use OpenTelemetry for production
# Trace spans through consensus, domains, tasks
```

### Challenge 3: "Consensus Adds Latency"

**Problem:** Byzantine consensus rounds are slower than single-node verification.

**Traditional:** Trust single database

**Agent-Based:** Verify with 3f+1 replicas

**Solution:**
- Batch tasks into consensus rounds (reduces overhead)
- Use regional consensus clusters
- Accept higher latency for Byzantine tolerance trade-off
- Example: `1 task = 100ms latency` vs `10 tasks = 120ms` (only 20% overhead for 10x throughput)

### Challenge 4: "Monitoring is Different"

**Problem:** Need new metrics and observability.

**Metrics to Track:**
```bash
# Task-level
tasks_created_total
tasks_completed_total
tasks_failed_total
task_latency_histogram
task_state_transitions

# Consensus-level
consensus_rounds_total
consensus_view_changes_total
pbft_prepare_msgs_count
pbft_commit_msgs_count
receipt_verification_time

# Domain-level
domain_active_count
domain_pending_tasks
domain_executor_utilization

# Health
agent_alive_count
agent_crash_restarts_total
network_latency_to_replicas
```

### Challenge 5: "Team Onboarding Takes Time"

**Problem:** Agent-based systems are paradigm shift; learning curve real.

**Timeline:**
- Week 1-2: Learn concepts, run examples
- Week 3-4: First real project, mistakes
- Month 2: Getting productive
- Month 3-6: Expert level

**Strategies:**
- Pair programming first projects
- Code reviews by domain experts
- Weekly knowledge-sharing sessions
- Invest in training and documentation
- Celebrate first successful deployment

---

## Migration Roadmap Template

```
Quarter 1 (Months 1-3):
├─ Week 1-2: Team training (ARCHITECTURE.md, examples)
├─ Week 3-4: Pilot project (smallest module)
├─ Month 2: 2-3 more modules, expand team knowledge
└─ Month 3: Lessons learned, refine process

Quarter 2 (Months 4-6):
├─ Migrate 50% of system
├─ Build shared template library
├─ Establish monitoring and alerting
└─ First production deployment

Quarter 3+ (Months 7+):
├─ Migrate remaining 50%
├─ Optimize based on production metrics
├─ Build internal tools and processes
└─ Become experts, mentor other teams
```

---

## Success Criteria

**After 6-month migration, you should achieve:**

- ✓ All code generated from RDF specs
- ✓ All tests use Chicago TDD (no mocks)
- ✓ 87%+ test coverage
- ✓ Zero production panics
- ✓ Byzantine resilience (3f+1 consensus)
- ✓ Cryptographic audit trail
- ✓ < 1s end-to-end latency for 95% of operations
- ✓ Horizontal scaling (add agents for throughput)
- ✓ Team comfortable with async/distributed systems
- ✓ Maintenance velocity improved (less debugging)

---

## Conclusion

Migrating to agent-based systems is a **journey, not a sprint**. Team skills, infrastructure, and processes all change. Budget 3-6 months, invest in training, celebrate wins, and learn from failures.

The payoff: more resilient, scalable, and maintainable systems with strong guarantees and complete audit trails.

**Start small. Build confidence. Scale gradually. Succeed together.**

