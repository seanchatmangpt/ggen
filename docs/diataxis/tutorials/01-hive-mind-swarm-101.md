# Tutorial: Hive Mind Swarm 101

**Learn how multi-agent swarm orchestration works with Queen-Colony-Worker architecture**

---

## Learning Objectives

By the end of this tutorial, you will:
- Understand the 3-tier hierarchy: Queen → Colony Leaders → Workers
- Know when to use swarm coordination vs single-agent execution
- Be able to spawn a basic swarm for a development task
- Understand Byzantine fault-tolerant consensus mechanisms

**Estimated Time:** 20 minutes
**Difficulty:** Beginner
**Prerequisites:** Basic understanding of distributed systems

---

## What is a Hive Mind Swarm?

A **Hive Mind Swarm** is a multi-agent coordination system inspired by insect colonies. Instead of a single AI agent working alone, multiple specialized agents work together with:

- **Distributed decision-making** (no single point of failure)
- **Specialized roles** (each agent has expertise)
- **Consensus mechanisms** (Byzantine fault tolerance)
- **Persistent memory** (shared knowledge across agents)

**Real-World Analogy:** A beehive doesn't have a "boss bee" commanding every action. Worker bees specialize (foragers, nurses, guards), coordinate through pheromone signals, and reach consensus on decisions like new hive locations.

---

## Architecture: 3-Tier Hierarchy

### Tier 1: The Queen (Strategic Coordination)

```
┌─────────────────────────────────────┐
│  Queen Seraphina                    │
│  - Strategic planning               │
│  - Resource allocation              │
│  - Swarm health monitoring          │
│  - Conflict resolution               │
└─────────────────────────────────────┘
```

**Role:** High-level orchestration, not task execution
**Example:** "We need to refactor the CLI. Assign 3 colonies: one for parsing, one for execution, one for testing."

### Tier 2: Colony Leaders (Tactical Execution)

```
┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│  Colony 1    │  │  Colony 2    │  │  Colony 3    │
│  (Parser)    │  │  (Executor)  │  │  (Tester)    │
└──────────────┘  └──────────────┘  └──────────────┘
```

**Role:** Coordinate worker agents, implement tactics
**Example:** Colony 1 Leader assigns 5 workers to parse different command types.

### Tier 3: Workers (Specialized Tasks)

```
Colony 1 Workers:
├── Worker A: Parse `ggen ontology` commands
├── Worker B: Parse `ggen lifecycle` commands
├── Worker C: Parse `ggen graph` commands
├── Worker D: Handle flags (-f, --output)
└── Worker E: Validate command syntax
```

**Role:** Execute specific, narrow tasks with expertise
**Example:** Worker A only parses ontology-related commands, becomes expert at that.

---

## Step-by-Step: Your First Swarm

### Step 1: Initialize Swarm Topology

```bash
# Choose a topology based on task complexity
npx claude-flow@alpha swarm init --topology mesh --max-agents 8
```

**Topologies:**
- **Mesh:** Peer-to-peer, all agents communicate (best for collaboration)
- **Hierarchical:** Tree structure, clear reporting lines (best for large tasks)
- **Ring:** Circular communication (best for sequential workflows)
- **Star:** Central coordinator (best for simple coordination)

**When to choose Mesh:** Your task requires agents to share context frequently (e.g., refactoring interconnected code).

### Step 2: Spawn Specialized Agents

```bash
# Spawn agents with specific capabilities
npx claude-flow@alpha agent spawn --type researcher --name "API-Researcher"
npx claude-flow@alpha agent spawn --type coder --name "Rust-Implementer"
npx claude-flow@alpha agent spawn --type tester --name "Test-Engineer"
```

**Agent Types:**
- `researcher`: Analyzes requirements, patterns, best practices
- `coder`: Writes implementation code
- `tester`: Creates test suites
- `reviewer`: Reviews code quality
- `architect`: Designs system architecture

### Step 3: Orchestrate a Task

```bash
# Assign a task with strategy
npx claude-flow@alpha task orchestrate \
  --task "Refactor CLI from builder to derive pattern" \
  --strategy adaptive \
  --priority high
```

**Strategies:**
- **Parallel:** All agents work simultaneously (fastest, needs independence)
- **Sequential:** Agents work one after another (slowest, handles dependencies)
- **Adaptive:** Swarm decides based on task graph (recommended)

### Step 4: Monitor Progress

```bash
# Check swarm status
npx claude-flow@alpha swarm status

# Get detailed agent metrics
npx claude-flow@alpha agent metrics --agent-id "Rust-Implementer"
```

**Expected Output:**
```json
{
  "swarm_id": "swarm-mesh-001",
  "topology": "mesh",
  "active_agents": 3,
  "tasks_completed": 12,
  "tasks_in_progress": 4,
  "health": "healthy"
}
```

---

## Worked Example: CLI Refactoring

**Scenario:** Upgrade ggen CLI from clap builder pattern to derive pattern without breaking functionality.

### Step 1: Queen Plans Strategy

```
Queen Seraphina analyzes task:
- Scope: 3 CLI files (cli.rs, main.rs, command_runner.rs)
- Risk: High (public API changes)
- Agents needed: 6 (2 per file: coder + tester)
```

### Step 2: Colony Leaders Assign Workers

```
Colony 1 (cli.rs):
├── Worker A: Refactor builder → derive
└── Worker B: Write integration tests

Colony 2 (main.rs):
├── Worker C: Update main() function
└── Worker D: Test CLI help output

Colony 3 (command_runner.rs):
├── Worker E: Migrate command dispatch
└── Worker F: Add regression tests
```

### Step 3: Workers Execute Tasks

```rust
// Worker A's output (cli.rs refactor)
// BEFORE (builder pattern)
let matches = App::new("ggen")
    .version("0.1.0")
    .arg(Arg::with_name("output")
        .short("o")
        .long("output"))
    .get_matches();

// AFTER (derive pattern)
#[derive(Parser)]
#[command(name = "ggen", version = "0.1.0")]
struct Cli {
    #[arg(short, long)]
    output: Option<PathBuf>,
}
```

### Step 4: Consensus & Integration

Workers report completion → Colony Leaders verify consistency → Queen validates 0 regressions.

**Result:** 100% test pass rate, 0 breaking changes, 3 files refactored in parallel.

---

## Byzantine Fault Tolerance (BFT)

**Problem:** What if an agent gives incorrect information?

**Solution:** Require 2/3 consensus before accepting any decision.

```
Task: "Is the CLI refactor safe to merge?"

Agent A: ✅ Yes (all tests pass)
Agent B: ✅ Yes (no breaking changes)
Agent C: ❌ No (found regression in --help)

Consensus: 2/3 approve, but 1 dissent triggers investigation.
→ Queen orders Colony Leader to investigate Agent C's concern.
```

**Key Principle:** No single agent can unilaterally make critical decisions.

---

## When to Use Swarm vs Single Agent

### Use Swarm When:
- ✅ Task spans multiple domains (backend + frontend + testing)
- ✅ Need parallel execution (refactor 10 files simultaneously)
- ✅ High risk (critical production code)
- ✅ Requires consensus (architectural decisions)

### Use Single Agent When:
- ❌ Simple, isolated task (fix a typo)
- ❌ Sequential-only workflow (must finish A before B)
- ❌ Low complexity (update documentation)
- ❌ Learning/experimentation (exploring a new API)

**Rule of Thumb:** If the task can be parallelized into 3+ independent subtasks, use a swarm.

---

## Glossary

| Term | Definition |
|------|------------|
| **Queen** | Top-level strategic coordinator (Seraphina) |
| **Colony Leader** | Mid-level tactical coordinator managing workers |
| **Worker** | Bottom-level specialized agent executing tasks |
| **Topology** | Communication pattern (mesh, hierarchical, ring, star) |
| **BFT** | Byzantine Fault Tolerance (2/3 consensus) |
| **Adaptive Strategy** | Swarm decides parallel vs sequential based on task dependencies |
| **Swarm Memory** | Persistent knowledge shared across all agents |

---

## Next Steps

Now that you understand swarm basics:

1. **[Try Tutorial 02: Clap-Noun-Verb Upgrade](02-clap-noun-verb-upgrade.md)** - See swarm coordination in action on a real refactor
2. **[Read Why Hive Mind Coordinates](../explanations/why-hive-mind-coordinates.md)** - Understand the theoretical foundation
3. **[Reference Swarm Agent Types](../reference/swarm-agent-types.md)** - Look up all available agent specializations

**Practice Exercise:** Spawn a 3-agent swarm to implement a new feature in your project. Use mesh topology and adaptive strategy. Monitor with `swarm status` every 5 minutes.

---

**Tutorial Complete!** You now understand multi-agent swarm coordination fundamentals.
