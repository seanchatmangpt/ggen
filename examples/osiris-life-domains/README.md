# OSIRIS Life Domains Example - Wave 4: Autonomous Agent Management

This is a comprehensive Wave 4 example demonstrating autonomous agent management of life domains with consensus-based balancing and self-improvement mechanisms. It exemplifies Joe Armstrong's "Let it Crash" AGI principles applied to personal life optimization.

## Overview

The system manages 6 autonomous domain agents that coordinate to maintain life balance:

- **Health Agent**: Manages physical, mental, and spiritual wellness
- **Career Agent**: Manages professional growth and advancement
- **Relationship Agent**: Manages personal and professional connections
- **Finance Agent**: Manages financial resources and wealth
- **Learning Agent**: Manages continuous growth and skill development
- **Spirituality Agent**: Manages mindfulness and purpose clarity

## Architecture

### 1. Autonomous Reasoning Engine

Each agent independently:
1. Analyzes its domain state
2. Discovers goals based on health score
3. Plans concrete actions
4. Calls MCP tools to execute
5. Learns from outcomes

```rust
// Example: Health agent discovers its own goals
AutonomousReasoning::analyze(&status) -> {
    domain_id: "health",
    health_score: 0.65,
    recommended_goals: ["increase_exercise", "improve_sleep"],
    action_plan: ["workout", "sleep_8hrs"],
    priority_level: "high"
}
```

### 2. Domain Balancing via Consensus

The system detects imbalances and reallocates resources through:
- **Consensus Voting**: Weighted voting on resource allocation
- **Imbalance Detection**: Identifies neglected domains (score < 0.65)
- **Rebalancing Strategy**: Proposes allocation adjustments
- **Enforcement**: Agents accept agreed-upon allocations

```rust
// Domain consensus example
let scores = {
    "health": 0.5,      // Imbalanced
    "career": 0.8,      // Healthy
    "learning": 0.6     // Slightly low
}

// System detects imbalance, votes on allocation
// Health domain gets 1.5x resource boost for recovery
```

### 3. Self-Improvement Mechanisms

Agents learn through:
- **Outcome Tracking**: Record what actions produced results
- **Effectiveness Learning**: Build action → outcome mapping
- **Agent Learning Sharing**: Successful patterns shared across domains
- **Adaptive Action Selection**: Prefer proven effective actions

```rust
// Self-improvement example
tracker.record_action("health", "workout", "success")
    .then(() -> get_action_effectiveness("health", "workout") = 3.5)
    .then(() -> share_learning("health", "career"))  // Share techniques
```

### 4. MCP Tool Integration

Agents autonomously discover and use 20+ tools across domains:

**Health Tools**:
- recommend_workout
- plan_meals
- sleep_analysis
- stress_management

**Career Tools**:
- search_jobs
- assess_skills
- career_pathing
- network_building

**Finance Tools**:
- create_budget
- savings_calculation
- investment_analysis
- debt_planning

**Learning Tools**:
- recommend_courses
- generate_reading_list
- practice_exercises
- skill_mapping

**Spirituality Tools**:
- guide_meditation
- reflection_prompts
- purpose_clarification
- mindfulness_exercises

**Relationship Tools**:
- event_scheduling
- message_templates
- connection_tracking
- community_networking

## Key Design Decisions

### 1. Agent Independence
Each agent is fully autonomous with:
- Own state machine
- Independent reasoning
- Direct MCP tool access
- No central controller

This enables fault tolerance: if one agent fails, others continue operating.

### 2. Type-First Design
Health scores and domain states are encoded in types:
```rust
pub struct HealthScore {
    score: f64,        // 0.0-1.0
    is_balanced: bool, // >= 0.65?
    trend: ScoreTrend, // Improving/Declining/Stable
}
```

Constraints are enforced at compile time, not runtime.

### 3. Async-First Coordination
All inter-agent communication is async to:
- Enable concurrent reasoning
- Prevent blocking waits
- Support real-time responsiveness
- Scale to larger agent networks

### 4. Distributed Learning
Each agent learns independently but shares discoveries:
```rust
improvement_tracker.share_learning("health", "career")
    // Health's meditation effectiveness -> Career's stress management
```

## Example Workflow: A Day in the System

```
08:00 - System Startup
├── Initialize 6 agents
├── Load previous day's state
└── Begin reasoning cycle

08:15 - Autonomous Reasoning Phase
├── Health Agent analyzes state
│  ├── Discovers: exercise_minutes=0, sleep_hours=6
│  ├── Calculates: health_score=0.55 (IMBALANCED)
│  ├── Sets goals: [increase_exercise, improve_sleep]
│  └── Plans actions: [workout 30min, sleep 8hrs]
│
├── Career Agent analyzes
│  ├── Discovers: skills=5, network_size=20
│  ├── Calculates: career_score=0.65 (borderline)
│  └── Plans: [learn_skill, network_event]
│
└── Other agents analyze their domains...

09:00 - Action Execution Phase
├── Health Agent calls MCP tool: recommend_workout
│  └── Gets: 30-min cardio plan
├── Career Agent calls: search_jobs
│  └── Gets: 5 matching opportunities
└── Learning Agent calls: recommend_courses
   └── Gets: 3 Rust courses

09:30 - Domain Balancing Phase
├── Collect health scores from all agents
├── Run consensus voting on resource allocation
├── Detect imbalances: health_score=0.55 < threshold
├── Propose rebalancing: allocate +50% resources to health
└── All agents acknowledge and adjust priorities

10:00 - Self-Improvement Learning Phase
├── Record outcomes from morning actions
│  ├── Health: workout succeeded (effectiveness +1)
│  ├── Career: networking increased connections (effectiveness +1)
│  └── Learning: course started (effectiveness +1)
│
├── Analyze patterns: what worked?
├── Share learnings across agents
│  └── Health's meditation technique -> Spirituality domain
│
└── Update action preferences for next cycle

17:00 - Status Check
├── Query current health scores
│  ├── Health: 0.65 -> 0.72 (improved!)
│  ├── Career: 0.65 -> 0.70 (improved!)
│  └── Learning: 0.68 -> 0.75 (strong!)
│
├── Detect new imbalances
└── Propose adjustments

22:00 - End of Day
├── Save system state to disk
├── Log outcomes and learnings
└── Prepare for next reasoning cycle
```

## Test Coverage

**6 comprehensive test suites with 50+ tests**:

1. **domain_tests.rs** (14 tests)
   - Agent creation and initialization
   - Health score computation
   - Goal setting and recommendations
   - Action execution for all 6 agents

2. **agent_tests.rs** (8 tests)
   - Multi-agent initialization
   - System status computation
   - Health score validation
   - Concurrent status queries

3. **reasoning_tests.rs** (7 tests)
   - Autonomous goal discovery
   - Priority level calculation
   - Action plan generation
   - Domain-specific reasoning

4. **consensus_tests.rs** (9 tests)
   - Imbalance detection
   - Consensus voting
   - Rebalancing strategy generation
   - Weighted voting mechanisms

5. **improvement_tests.rs** (8 tests)
   - Action outcome tracking
   - Effectiveness learning
   - Cross-domain learning sharing
   - Top action identification

6. **e2e_tests.rs** (9 tests)
   - Complete system workflows
   - Multi-cycle operations
   - State consistency validation
   - Interleaved operations

**Total**: 55 tests covering all major features

## How This Demonstrates Joe Armstrong AGI Principles

### 1. Autonomous Reasoning
Agents don't wait for human direction. They:
- Analyze own domain state autonomously
- Set own goals based on metrics
- Plan action sequences
- Call tools independently
- Adapt based on outcomes

No central controller, no task queue from humans.

### 2. Distributed Coordination
6 agents negotiate resource allocation through:
- Consensus voting (democratic)
- Majority with constraints
- Graceful handling of disagreement
- No single point of control

One agent failing doesn't stop the system.

### 3. Self-Improvement
The system learns and optimizes:
- Actions are rated by effectiveness
- High-performing actions are favored
- Patterns are shared across agents
- Adaptation happens automatically

No human retraining needed.

### 4. Fault Tolerance
If one agent fails:
- Others continue operating
- State is preserved
- System recovers automatically
- Graceful degradation

The "Let it Crash" philosophy: fail one agent, system survives.

### 5. MCP Tool Discovery
Agents discover available tools:
- Registry of 20+ tools per domain
- Agents learn which tools work best
- Tools are called autonomously
- New tools can be added dynamically

No hardcoding of tool mappings.

## Running the Example

```bash
# Build the example
cd examples/osiris-life-domains
cargo build

# Run the example
cargo run

# Run all tests
cargo test

# Run specific test suite
cargo test --test e2e_tests

# Run with logging
RUST_LOG=info cargo run
```

## Example Output

```
Initializing OSIRIS Life Domains System
Registered 6 domain agents

=== Initial System Status ===
{
  "domain_statuses": {
    "health": { "health_score": 0.65, "goals": [] },
    "career": { "health_score": 0.70, "goals": [] },
    ...
  },
  "balance": { "consensus_threshold": 0.67 },
  "timestamp": "2026-03-24T10:00:00Z"
}

=== Running Autonomous Reasoning Cycle ===
Health Agent: setting goals: ["increase_exercise", "improve_sleep"]
Career Agent: setting goals: ["develop_skills"]
...

=== Balancing Domains ===
Running consensus voting with 6 domains
Detected imbalances in: ["health"]
Proposing rebalancing for 1 domains

=== Final System Status ===
{
  "domain_statuses": {
    "health": { "health_score": 0.72 },
    "career": { "health_score": 0.72 },
    ...
  }
}
```

## Files Structure

```
osiris-life-domains/
├── Cargo.toml
├── ontology/
│   └── domains.ttl          # RDF ontology defining all domains
├── src/
│   ├── lib.rs               # Main system
│   ├── main.rs              # Example execution
│   ├── agents/
│   │   ├── mod.rs
│   │   ├── base.rs          # AgentBase trait
│   │   ├── health.rs
│   │   ├── career.rs
│   │   ├── relationships.rs
│   │   ├── finance.rs
│   │   ├── learning.rs
│   │   └── spirituality.rs
│   ├── reasoning.rs         # Autonomous reasoning engine
│   ├── balancing.rs         # Consensus & domain balancing
│   ├── improvement.rs       # Self-improvement & learning
│   ├── mcp_tools.rs         # MCP tool registry
│   ├── metrics.rs           # Health score computation
│   ├── persistence.rs       # State save/load
│   └── lib.rs               # System integration
└── tests/
    ├── domain_tests.rs      # Domain agent tests
    ├── agent_tests.rs       # Multi-agent tests
    ├── reasoning_tests.rs   # Reasoning tests
    ├── consensus_tests.rs   # Balancing tests
    ├── improvement_tests.rs # Learning tests
    └── e2e_tests.rs         # End-to-end workflows
```

## Dependencies

- **tokio**: Async runtime
- **serde/serde_json**: Serialization
- **tracing**: Structured logging
- **async-trait**: Async trait implementation
- **chrono**: Timestamps
- **uuid**: Unique identifiers

## Future Enhancements

1. **Persistence Layer**: Save/restore full system state
2. **Persistent Storage**: SQLite/PostgreSQL for outcomes
3. **External APIs**: Real calendar, job boards, course platforms
4. **Advanced Learning**: Bayesian networks for causality inference
5. **Human Feedback**: Accept corrections and preferences
6. **Multi-period Analysis**: Monthly, quarterly, yearly reviews
7. **Goal Cascading**: Sub-goals and dependencies
8. **Cross-domain Synergies**: Detect and exploit relationships

## References

- Joe Armstrong's "Let it Crash" philosophy for erlang/distributed systems
- Actor model for autonomous agents
- Consensus algorithms (RAFT, Byzantine)
- Reinforcement learning for action selection
- Domain-driven design for autonomous agents

---

**Version**: 0.1.0  
**Status**: Example Implementation  
**Test Coverage**: 55 tests across 6 test suites
