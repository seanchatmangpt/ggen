# Claude Code Configuration - SPARC Development Environment

## 🚨 CRITICAL: CONCURRENT EXECUTION & FILE MANAGEMENT

**ABSOLUTE RULES**:
1. ALL operations MUST be concurrent/parallel in a single message
2. **NEVER save working files, text/mds and tests to the root folder**
3. ALWAYS organize files in appropriate subdirectories
4. **USE CLAUDE CODE'S TASK TOOL** for spawning agents concurrently, not just MCP

### ⚡ GOLDEN RULE: "1 MESSAGE = ALL RELATED OPERATIONS"

**MANDATORY PATTERNS:**
- **TodoWrite**: ALWAYS batch ALL todos in ONE call (5-10+ todos minimum)
- **Task tool (Claude Code)**: ALWAYS spawn ALL agents in ONE message with full instructions
- **File operations**: ALWAYS batch ALL reads/writes/edits in ONE message
- **Bash commands**: ALWAYS batch ALL terminal operations in ONE message
- **Memory operations**: ALWAYS batch ALL memory store/retrieve in ONE message

### 🎯 CRITICAL: Claude Code Task Tool for Agent Execution

**Claude Code's Task tool is the PRIMARY way to spawn agents:**
```javascript
// ✅ CORRECT: Use Claude Code's Task tool for parallel agent execution
[Single Message]:
  Task("Research agent", "Analyze requirements and patterns...", "researcher")
  Task("Coder agent", "Implement core features...", "coder")
  Task("Tester agent", "Create comprehensive tests...", "tester")
  Task("Reviewer agent", "Review code quality...", "reviewer")
  Task("Architect agent", "Design system architecture...", "system-architect")
```

**MCP tools are ONLY for coordination setup:**
- `mcp__claude-flow__swarm_init` - Initialize coordination topology
- `mcp__claude-flow__agent_spawn` - Define agent types for coordination
- `mcp__claude-flow__task_orchestrate` - Orchestrate high-level workflows

### 📁 File Organization Rules

**NEVER save to root folder. Use these directories:**
- `/src` - Source code files
- `/tests` - Test files
- `/docs` - Documentation and markdown files
- `/config` - Configuration files
- `/scripts` - Utility scripts
- `/examples` - Example code

## Project Overview

This project uses SPARC (Specification, Pseudocode, Architecture, Refinement, Completion) methodology with Claude-Flow orchestration for systematic Test-Driven Development.

## SPARC Commands

### Core Commands
- `npx claude-flow sparc modes` - List available modes
- `npx claude-flow sparc run <mode> "<task>"` - Execute specific mode
- `npx claude-flow sparc tdd "<feature>"` - Run complete TDD workflow
- `npx claude-flow sparc info <mode>` - Get mode details

### Batchtools Commands
- `npx claude-flow sparc batch <modes> "<task>"` - Parallel execution
- `npx claude-flow sparc pipeline "<task>"` - Full pipeline processing
- `npx claude-flow sparc concurrent <mode> "<tasks-file>"` - Multi-task processing

### Build Commands
- `npm run build` - Build project
- `npm run test` - Run tests
- `npm run lint` - Linting
- `npm run typecheck` - Type checking

## SPARC Workflow Phases

1. **Specification** - Requirements analysis (`sparc run spec-pseudocode`)
2. **Pseudocode** - Algorithm design (`sparc run spec-pseudocode`)
3. **Architecture** - System design (`sparc run architect`)
4. **Refinement** - TDD implementation (`sparc tdd`)
5. **Completion** - Integration (`sparc run integration`)

## Code Style & Best Practices

- **Modular Design**: Files under 500 lines
- **Environment Safety**: Never hardcode secrets
- **Test-First**: Write tests before implementation
- **Clean Architecture**: Separate concerns
- **Documentation**: Keep updated

## 🚀 Available Agents (54 Total)

### Core Development
`coder`, `reviewer`, `tester`, `planner`, `researcher`

### Swarm Coordination
`hierarchical-coordinator`, `mesh-coordinator`, `adaptive-coordinator`, `collective-intelligence-coordinator`, `swarm-memory-manager`

### Consensus & Distributed
`byzantine-coordinator`, `raft-manager`, `gossip-coordinator`, `consensus-builder`, `crdt-synchronizer`, `quorum-manager`, `security-manager`

### Performance & Optimization
`perf-analyzer`, `performance-benchmarker`, `task-orchestrator`, `memory-coordinator`, `smart-agent`

### GitHub & Repository
`github-modes`, `pr-manager`, `code-review-swarm`, `issue-tracker`, `release-manager`, `workflow-automation`, `project-board-sync`, `repo-architect`, `multi-repo-swarm`

### SPARC Methodology
`sparc-coord`, `sparc-coder`, `specification`, `pseudocode`, `architecture`, `refinement`

### Specialized Development
`backend-dev`, `mobile-dev`, `ml-developer`, `cicd-engineer`, `api-docs`, `system-architect`, `code-analyzer`, `base-template-generator`

### Testing & Validation
`tdd-london-swarm`, `production-validator`

### Migration & Planning
`migration-planner`, `swarm-init`

## 🎯 Claude Code vs MCP Tools

### Claude Code Handles ALL EXECUTION:
- **Task tool**: Spawn and run agents concurrently for actual work
- File operations (Read, Write, Edit, MultiEdit, Glob, Grep)
- Code generation and programming
- Bash commands and system operations
- Implementation work
- Project navigation and analysis
- TodoWrite and task management
- Git operations
- Package management
- Testing and debugging

### MCP Tools ONLY COORDINATE:
- Swarm initialization (topology setup)
- Agent type definitions (coordination patterns)
- Task orchestration (high-level planning)
- Memory management
- Neural features
- Performance tracking
- GitHub integration

**KEY**: MCP coordinates the strategy, Claude Code's Task tool executes with real agents.

## 🚀 Quick Setup

```bash
# Add MCP servers (Claude Flow required, others optional)
claude mcp add claude-flow npx claude-flow@alpha mcp start
claude mcp add ruv-swarm npx ruv-swarm mcp start  # Optional: Enhanced coordination
claude mcp add flow-nexus npx flow-nexus@latest mcp start  # Optional: Cloud features
```

## MCP Tool Categories

### Coordination
`swarm_init`, `agent_spawn`, `task_orchestrate`

### Monitoring
`swarm_status`, `agent_list`, `agent_metrics`, `task_status`, `task_results`

### Memory & Neural
`memory_usage`, `neural_status`, `neural_train`, `neural_patterns`

### GitHub Integration
`github_swarm`, `repo_analyze`, `pr_enhance`, `issue_triage`, `code_review`

### System
`benchmark_run`, `features_detect`, `swarm_monitor`

### Flow-Nexus MCP Tools (Optional Advanced Features)
Flow-Nexus extends MCP capabilities with 70+ cloud-based orchestration tools:

**Key MCP Tool Categories:**
- **Swarm & Agents**: `swarm_init`, `swarm_scale`, `agent_spawn`, `task_orchestrate`
- **Sandboxes**: `sandbox_create`, `sandbox_execute`, `sandbox_upload` (cloud execution)
- **Templates**: `template_list`, `template_deploy` (pre-built project templates)
- **Neural AI**: `neural_train`, `neural_patterns`, `seraphina_chat` (AI assistant)
- **GitHub**: `github_repo_analyze`, `github_pr_manage` (repository management)
- **Real-time**: `execution_stream_subscribe`, `realtime_subscribe` (live monitoring)
- **Storage**: `storage_upload`, `storage_list` (cloud file management)

**Authentication Required:**
- Register: `mcp__flow-nexus__user_register` or `npx flow-nexus@latest register`
- Login: `mcp__flow-nexus__user_login` or `npx flow-nexus@latest login`
- Access 70+ specialized MCP tools for advanced orchestration

## 🚀 Agent Execution Flow with Claude Code

### The Correct Pattern:

1. **Optional**: Use MCP tools to set up coordination topology
2. **REQUIRED**: Use Claude Code's Task tool to spawn agents that do actual work
3. **REQUIRED**: Each agent runs hooks for coordination
4. **REQUIRED**: Batch all operations in single messages

### Example Full-Stack Development:

```javascript
// Single message with all agent spawning via Claude Code's Task tool
[Parallel Agent Execution]:
  Task("Backend Developer", "Build REST API with Express. Use hooks for coordination.", "backend-dev")
  Task("Frontend Developer", "Create React UI. Coordinate with backend via memory.", "coder")
  Task("Database Architect", "Design PostgreSQL schema. Store schema in memory.", "code-analyzer")
  Task("Test Engineer", "Write Jest tests. Check memory for API contracts.", "tester")
  Task("DevOps Engineer", "Setup Docker and CI/CD. Document in memory.", "cicd-engineer")
  Task("Security Auditor", "Review authentication. Report findings via hooks.", "reviewer")
  
  // All todos batched together
  TodoWrite { todos: [...8-10 todos...] }
  
  // All file operations together
  Write "backend/server.js"
  Write "frontend/App.jsx"
  Write "database/schema.sql"
```

## 📋 Agent Coordination Protocol

### Every Agent Spawned via Task Tool MUST:

**1️⃣ BEFORE Work:**
```bash
npx claude-flow@alpha hooks pre-task --description "[task]"
npx claude-flow@alpha hooks session-restore --session-id "swarm-[id]"
```

**2️⃣ DURING Work:**
```bash
npx claude-flow@alpha hooks post-edit --file "[file]" --memory-key "swarm/[agent]/[step]"
npx claude-flow@alpha hooks notify --message "[what was done]"
```

**3️⃣ AFTER Work:**
```bash
npx claude-flow@alpha hooks post-task --task-id "[task]"
npx claude-flow@alpha hooks session-end --export-metrics true
```

## 🎯 Concurrent Execution Examples

### ✅ CORRECT WORKFLOW: MCP Coordinates, Claude Code Executes

```javascript
// Step 1: MCP tools set up coordination (optional, for complex tasks)
[Single Message - Coordination Setup]:
  mcp__claude-flow__swarm_init { topology: "mesh", maxAgents: 6 }
  mcp__claude-flow__agent_spawn { type: "researcher" }
  mcp__claude-flow__agent_spawn { type: "coder" }
  mcp__claude-flow__agent_spawn { type: "tester" }

// Step 2: Claude Code Task tool spawns ACTUAL agents that do the work
[Single Message - Parallel Agent Execution]:
  // Claude Code's Task tool spawns real agents concurrently
  Task("Research agent", "Analyze API requirements and best practices. Check memory for prior decisions.", "researcher")
  Task("Coder agent", "Implement REST endpoints with authentication. Coordinate via hooks.", "coder")
  Task("Database agent", "Design and implement database schema. Store decisions in memory.", "code-analyzer")
  Task("Tester agent", "Create comprehensive test suite with 90% coverage.", "tester")
  Task("Reviewer agent", "Review code quality and security. Document findings.", "reviewer")
  
  // Batch ALL todos in ONE call
  TodoWrite { todos: [
    {id: "1", content: "Research API patterns", status: "in_progress", priority: "high"},
    {id: "2", content: "Design database schema", status: "in_progress", priority: "high"},
    {id: "3", content: "Implement authentication", status: "pending", priority: "high"},
    {id: "4", content: "Build REST endpoints", status: "pending", priority: "high"},
    {id: "5", content: "Write unit tests", status: "pending", priority: "medium"},
    {id: "6", content: "Integration tests", status: "pending", priority: "medium"},
    {id: "7", content: "API documentation", status: "pending", priority: "low"},
    {id: "8", content: "Performance optimization", status: "pending", priority: "low"}
  ]}
  
  // Parallel file operations
  Bash "mkdir -p app/{src,tests,docs,config}"
  Write "app/package.json"
  Write "app/src/server.js"
  Write "app/tests/server.test.js"
  Write "app/docs/API.md"
```

### ❌ WRONG (Multiple Messages):
```javascript
Message 1: mcp__claude-flow__swarm_init
Message 2: Task("agent 1")
Message 3: TodoWrite { todos: [single todo] }
Message 4: Write "file.js"
// This breaks parallel coordination!
```

## Performance Benefits

- **84.8% SWE-Bench solve rate**
- **32.3% token reduction**
- **2.8-4.4x speed improvement**
- **27+ neural models**

## Hooks Integration

### Pre-Operation
- Auto-assign agents by file type
- Validate commands for safety
- Prepare resources automatically
- Optimize topology by complexity
- Cache searches

### Post-Operation
- Auto-format code
- Train neural patterns
- Update memory
- Analyze performance
- Track token usage

### Session Management
- Generate summaries
- Persist state
- Track metrics
- Restore context
- Export workflows

## Advanced Features (v2.0.0)

- 🚀 Automatic Topology Selection
- ⚡ Parallel Execution (2.8-4.4x speed)
- 🧠 Neural Training
- 📊 Bottleneck Analysis
- 🤖 Smart Auto-Spawning
- 🛡️ Self-Healing Workflows
- 💾 Cross-Session Memory
- 🔗 GitHub Integration

## Integration Tips

1. Start with basic swarm init
2. Scale agents gradually
3. Use memory for context
4. Monitor progress regularly
5. Train patterns from success
6. Enable hooks automation
7. Use GitHub tools first

## Support

- Documentation: https://github.com/ruvnet/claude-flow
- Issues: https://github.com/ruvnet/claude-flow/issues
- Flow-Nexus Platform: https://flow-nexus.ruv.io (registration required for cloud features)

---

Remember: **Claude Flow coordinates, Claude Code creates!**

## 🧪 MANDATORY: Testing Requirements

### Critical Rule: NO CODE WITHOUT TESTS

**ABSOLUTE TESTING REQUIREMENTS:**
1. **NEVER implement features without corresponding tests**
2. **NEVER skip subsystems** - If code exists, tests MUST exist
3. **ALWAYS verify test execution** - 100% pass rate required
4. **ALWAYS organize tests properly** - Mirror source structure in /tests
5. **ALWAYS update package.json** - Add test scripts for new modules

### 📋 Mandatory Test Checklist

**Before marking ANY task complete, verify:**
- [ ] Unit tests written for core functionality (20% critical paths)
- [ ] Integration tests for system boundaries
- [ ] Performance tests for time-critical operations
- [ ] Security tests for authentication/authorization
- [ ] ALL tests passing (npm test shows 100% success)
- [ ] Test files organized in proper /tests structure
- [ ] Package.json updated with test scripts
- [ ] OTEL spans/traces verified (not just CLI help output)

### 🎯 Test Coverage Requirements by Component Type

**Backend/API Components:**
- Unit tests: Core business logic
- Integration tests: Database, external APIs
- Performance tests: Response times, throughput
- Security tests: Auth, input validation, injection prevention

**CLI Components:**
- Unit tests: Argument parsing, validation
- Integration tests: Command execution, file operations
- Performance tests: Startup time, command latency
- Security tests: Path traversal, command injection

**Domain Logic:**
- Unit tests: Pure functions, transformations
- Integration tests: Cross-module interactions
- Performance tests: Algorithm complexity
- Security tests: Data validation, sanitization

**Infrastructure:**
- Unit tests: Configuration parsing
- Integration tests: Service startup, health checks
- Performance tests: Resource usage, container startup
- Security tests: Secrets handling, network isolation

### 🚨 Gap Prevention Protocol

**When implementing features, ALWAYS follow this sequence:**

```javascript
// Step 1: Identify ALL subsystems that need testing
[Single Message - Gap Analysis]:
  Glob "src/**/*.{rs,ts,js}"        // Find all source files
  Grep "pub fn|export|class"        // Identify public APIs
  Bash "find tests -name '*.test.*'" // Check existing tests

// Step 2: Create comprehensive test plan
[Single Message - Test Planning]:
  Task("Test Architect", "Design test suite covering all public APIs", "system-architect")
  Task("Gap Analyzer", "Identify missing tests by comparing src vs tests", "code-analyzer")

  TodoWrite { todos: [
    {content: "Unit tests for module A", status: "pending"},
    {content: "Unit tests for module B", status: "pending"},
    {content: "Integration tests for A+B", status: "pending"},
    {content: "Performance benchmarks", status: "pending"},
    {content: "Security validation", status: "pending"},
    ...all subsystems listed...
  ]}

// Step 3: Implement tests in parallel
[Single Message - Test Implementation]:
  Task("Tester 1", "Write unit tests for module A", "tester")
  Task("Tester 2", "Write unit tests for module B", "tester")
  Task("Tester 3", "Write integration tests", "tester")
  Task("Performance Engineer", "Write benchmarks", "performance-benchmarker")
  Task("Security Auditor", "Write security tests", "reviewer")

  // Create ALL test files together
  Write "tests/unit/module-a.test.js"
  Write "tests/unit/module-b.test.js"
  Write "tests/integration/a-b.test.js"
  Write "tests/performance/benchmarks.test.js"
  Write "tests/security/validation.test.js"

// Step 4: Verify and validate
[Single Message - Validation]:
  Bash "npm test"  // Run all tests
  Bash "npm run test:coverage"  // Check coverage
  Bash "npx claude-flow@alpha hooks post-task --task-id 'testing-complete'"
```

### 🤖 Automated Gap Detection

**MANDATORY: Run before completing any feature work:**

```bash
# 1. Find source files without tests
find src -name '*.rs' -o -name '*.ts' -o -name '*.js' | while read file; do
  basename="${file##*/}"
  testfile="tests/${basename%.{rs,ts,js}}.test.${file##*.}"
  [ ! -f "$testfile" ] && echo "MISSING: $testfile for $file"
done

# 2. Verify test execution
npm test 2>&1 | grep -E "(FAIL|ERROR|0 passing)" && echo "TESTS FAILING" || echo "TESTS PASSING"

# 3. Check OTEL instrumentation (not just CLI help)
npx claude-flow@alpha hooks session-end --export-metrics true | grep "spans_created" || echo "NO OTEL SPANS DETECTED"
```

### 📊 Test Organization Structure

**REQUIRED directory structure:**
```
/tests
  /unit              # Pure function tests
    /domain          # Domain logic
    /utils           # Utilities
    /core            # Core functionality
  /integration       # Cross-module tests
    /api             # API integration
    /database        # DB integration
    /external        # External service mocks
  /performance       # Benchmarks
    /latency         # Response time tests
    /throughput      # Load tests
    /memory          # Memory usage
  /security          # Security validation
    /auth            # Authentication
    /input           # Input validation
    /injection       # Injection prevention
  /e2e               # End-to-end scenarios
  /common            # Shared test utilities
    /mocks           # Mock implementations
    /fixtures        # Test data
    /helpers         # Test helpers
```

### ⚠️ Common Testing Gaps to Avoid

**CRITICAL FAILURES FROM PAST WORK:**

1. **Subsystem Neglect**
   - ❌ Testing only "main" module, ignoring dependencies
   - ✅ Test ALL modules discovered by Glob/Grep analysis

2. **False Positive Validation**
   - ❌ Trusting CLI help output as proof of functionality
   - ✅ Require OTEL spans/traces showing actual execution

3. **Incomplete Test Suites**
   - ❌ Only unit tests, no integration/performance/security
   - ✅ All 4 test categories for every component

4. **Disorganized Tests**
   - ❌ Tests in root folder or random locations
   - ✅ Proper /tests structure mirroring /src

5. **Skipped Verification**
   - ❌ Writing tests but not running them
   - ✅ npm test with 100% pass rate verified

6. **Missing Package Scripts**
   - ❌ Tests exist but no npm scripts to run them
   - ✅ package.json updated with test:unit, test:integration, etc.

### 🎯 Agent-Specific Testing Responsibilities

**When spawning agents, ALWAYS include testing requirements:**

```javascript
// ✅ CORRECT: Testing integrated into all agent tasks
[Parallel Agent Execution]:
  Task("Backend Developer", "Build REST API. INCLUDE unit + integration tests.", "backend-dev")
  Task("System Architect", "Design architecture. INCLUDE test strategy.", "system-architect")
  Task("Performance Benchmarker", "Benchmark API. WRITE performance tests.", "performance-benchmarker")
  Task("Production Validator", "Validate deployment. VERIFY test coverage.", "production-validator")
  Task("Tester", "Write comprehensive test suite. ENSURE 100% pass rate.", "tester")

  TodoWrite { todos: [
    {content: "Implement API endpoints", status: "in_progress"},
    {content: "Write API unit tests", status: "pending"},  // ← ALWAYS add test todos
    {content: "Write integration tests", status: "pending"},
    {content: "Write performance tests", status: "pending"},
    {content: "Write security tests", status: "pending"},
    {content: "Run and verify all tests", status: "pending"},
    ...
  ]}
```

### 🔍 Pre-Completion Testing Validation

**MANDATORY: Before responding to user with "done":**

```bash
# 1. Check for untested code
echo "=== SEARCHING FOR UNTESTED MODULES ==="
find src -type f -name '*.{rs,ts,js}' | wc -l
find tests -type f -name '*.test.*' | wc -l
# If src count >> test count, TESTS ARE MISSING

# 2. Verify test execution
echo "=== RUNNING ALL TESTS ==="
npm test || echo "CRITICAL: TESTS FAILING"

# 3. Check test organization
echo "=== VALIDATING TEST STRUCTURE ==="
[ -d "tests/unit" ] || echo "MISSING: tests/unit"
[ -d "tests/integration" ] || echo "MISSING: tests/integration"
[ -d "tests/performance" ] || echo "MISSING: tests/performance"
[ -d "tests/security" ] || echo "MISSING: tests/security"

# 4. Verify OTEL instrumentation
echo "=== CHECKING OTEL SPANS ==="
npx claude-flow@alpha hooks session-end --export-metrics true | grep -q "spans_created: [1-9]" || echo "CRITICAL: NO OTEL SPANS"
```

### 📈 Progressive Testing Strategy (80/20 Rule)

**For large codebases, prioritize testing effort:**

**Phase 1: Critical 20% (MANDATORY)**
- Entry points (main, init, bootstrap)
- Public APIs (exported functions/classes)
- Security boundaries (auth, validation)
- Data transformations (core business logic)

**Phase 2: Integration 30% (HIGH PRIORITY)**
- Cross-module interactions
- Database operations
- External API calls
- File system operations

**Phase 3: Edge Cases 30% (MEDIUM PRIORITY)**
- Error handling paths
- Boundary conditions
- Concurrent operations
- Resource cleanup

**Phase 4: Comprehensive 20% (LOW PRIORITY)**
- Internal helpers
- Utility functions
- Logging/metrics
- Configuration parsing

**Apply this strategy but NEVER skip Phase 1 for ANY subsystem.**

# important-instruction-reminders
Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.
Never save working files, text/mds and tests to the root folder.
