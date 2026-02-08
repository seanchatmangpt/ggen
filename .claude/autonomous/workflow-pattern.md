# Phased Agent Workflow Pattern

## Usage Pattern

```bash
# Phase 1: Explore (5 agents discover codebase)
launch 5 explore agents to search for X patterns

# Phase 2: Plan (5 agents create implementation plans)
launch 5 planning agents to design solutions for findings

# Phase 3: Execute (20 agents implement the work)
launch 20 agents to implement all planned changes
```

## State File Format

**Location:** `.claude/autonomous/workflow-state.json`

```json
{
  "workflow_id": "workflow-2026-02-08-123456",
  "current_phase": "execute",
  "phases": {
    "explore": {
      "status": "completed",
      "agents": 5,
      "completed_at": "2026-02-08T12:34:56Z",
      "outputs": [
        "Found 23 instances of pattern X",
        "Identified 7 optimization opportunities"
      ]
    },
    "plan": {
      "status": "completed",
      "agents": 5,
      "completed_at": "2026-02-08T12:45:00Z",
      "outputs": [
        "Created implementation plan for optimization",
        "Designed test strategy"
      ]
    },
    "execute": {
      "status": "in_progress",
      "agents": 20,
      "started_at": "2026-02-08T12:46:00Z",
      "progress": {
        "completed": 8,
        "total": 20,
        "in_progress": 12
      }
    }
  }
}
```

## Resume Capability

**On restart:**
1. Load `workflow-state.json`
2. Check `current_phase`
3. If phase incomplete → Resume with remaining work
4. If phase complete → Start next phase

**Example:**
```bash
# Initial run (gets interrupted)
launch 5 explore agents to analyze codebase
# ... system stops after 3 agents complete ...

# Resume (automatic on restart)
# System detects 3/5 explore agents completed
# Launches 2 more explore agents to finish phase
# Then automatically proceeds to plan phase
```

## Workflow Templates

### Template 1: Research → Design → Implement
```bash
# 1. Research (Explore agents)
launch 5 explore agents to:
- Search for relevant code patterns
- Find similar implementations
- Identify constraints and requirements

# 2. Design (Plan agents)
launch 5 planning agents to:
- Create architecture design
- Plan implementation steps
- Design test strategy

# 3. Implement (General-purpose agents)
launch 20 agents to:
- Implement features from plans
- Write tests
- Update documentation
```

### Template 2: Audit → Fix → Verify
```bash
# 1. Audit (Explore agents)
launch 5 explore agents to:
- Search for security vulnerabilities
- Find deprecated APIs
- Identify technical debt

# 2. Fix (Plan agents)
launch 5 planning agents to:
- Prioritize issues
- Design fix strategies
- Plan rollout

# 3. Verify (General-purpose agents)
launch 20 agents to:
- Implement fixes
- Run security tests
- Update audit documentation
```

### Template 3: Analyze → Optimize → Benchmark
```bash
# 1. Analyze (Explore agents)
launch 5 explore agents to:
- Profile performance bottlenecks
- Search for optimization opportunities
- Find similar optimizations in codebase

# 2. Optimize (Plan agents)
launch 5 planning agents to:
- Design optimization strategies
- Plan testing approach
- Estimate performance gains

# 3. Benchmark (General-purpose agents)
launch 20 agents to:
- Implement optimizations
- Run benchmarks
- Document improvements
```

## Integration with CLAUDE.md

Add to CLAUDE.md:

```markdown
## Phased Agent Workflows

**Pattern:** Explore → Plan → Execute (with auto-resume)

**Example:**
```bash
# Phase 1: Discover (5 Explore agents)
launch 5 explore agents to search for optimization opportunities

# Phase 2: Design (5 Plan agents)
launch 5 planning agents to create implementation plans

# Phase 3: Implement (20 agents)
launch 20 agents to implement all optimizations
```

**Auto-Resume:** System saves state after each phase. On restart, continues from last incomplete phase.

**State:** `.claude/autonomous/workflow-state.json`
```

## Commands

```bash
# Start new workflow
ggen workflow start "workflow-name"

# Resume from checkpoint
ggen workflow resume

# Show status
ggen workflow status

# Reset workflow
ggen workflow reset
```

## Implementation Notes

**Keep it simple:**
- Use basic Task tool with Explore, Plan, general-purpose agents
- State saved to JSON file
- No complex orchestration - just sequential phases
- Manual trigger for each phase OR auto-advance when phase completes

**State persistence:**
- Save after each agent completes
- Atomic writes (write to .tmp, then rename)
- 3 backup copies (rolling)

**Error handling:**
- Agent fails → Retry up to 3 times
- Phase fails → Stop, save state, wait for manual fix
- System crash → Resume from last saved state

**No external dependencies:**
- Pure Claude Code Task tool
- Standard Rust + Tokio
- JSON state files
- Systemd/launchd for restart
