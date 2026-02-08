# Claude Code Best Practices 2026 - Comprehensive Research Report

**Generated**: 2026-02-08
**Research Agents**: 5 parallel claude-code-guide agents
**Target Project**: ggen v6.0.0 (Specification-driven Rust code generation)

---

## Executive Summary

Based on comprehensive research across 5 specialized domains, here are the **bleeding edge Claude Code best practices for 2026** that align with your ggen project's advanced requirements:

### Top 10 High-Impact Patterns

1. **Agent Teams (Experimental)** - Parallel coordination with 5-20 agents per phase
2. **Async Hooks** - Non-blocking automation for CI/CD pipelines
3. **Persistent Subagent Memory** - Cross-session learning and pattern recognition
4. **MCP Tool Search** - Automatic discovery in large tool ecosystems
5. **Phased Workflows** - Explore → Plan → Execute with auto-resume
6. **Extended Thinking + Effort Levels** - Optimized reasoning allocation
7. **Session Teleportation** - Seamless web ↔ CLI switching
8. **Prompt Caching** - 90% token savings on repeated content
9. **Circuit Breaker Patterns** - Resilient error handling
10. **Quality Gate Automation** - Stop hooks with Andon protocol

---

## 1. Advanced Features & Capabilities

### Agent Teams (Experimental - Enable with CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS=1)

**Architecture**: One lead + N independent teammates with full context windows

```bash
# For your 30-crate project
Create an agent team to refactor crate architecture:
- Teammate 1: Analyze crates/{ggen-core,ggen-cli}
- Teammate 2: Analyze crates/{ggen-domain,ggen-utils}
- Teammate 3: Design module restructuring
- Teammate 4: Validate zero-cost abstractions
- Teammate 5: Update workspace Cargo.toml

Have them discuss findings before proposing changes.
```

**When to Use**:
- Parallel code reviews (security/performance/testing reviewers)
- Competing hypotheses debugging
- Cross-layer implementation (each teammate owns module)
- Large refactors with clear file boundaries

**Cost**: 2-7× token usage depending on coordination overhead

### Persistent Memory Scopes

| Scope | Location | Use Case |
|-------|----------|----------|
| `user` | `~/.claude/agent-memory/<agent>/` | Cross-project learning |
| `project` | `.claude/agent-memory/<agent>/` | Codebase-specific patterns |
| `local` | `.claude/agent-memory-local/<agent>/` | Not in git |

**Example**: Create rust-reviewer agent with memory
```yaml
---
name: rust-reviewer
description: Rust expert specialized in Chicago TDD, type-first design
memory: project
model: sonnet
---

Review code for:
- Type safety and ownership semantics
- Zero-cost abstraction violations
- Macro strategies and derive traits
- Chicago TDD compliance

Update your memory with architectural patterns discovered.
```

### Extended Context Windows (1M Tokens)

```bash
# For 30-crate monorepo analysis
/model sonnet[1m]

# Or with environment
ANTHROPIC_MODEL=claude-sonnet-4-5-20250929[1m] claude
```

### Session Teleportation

```bash
# In CLI: Resume web sessions
/teleport

# In web: Get environment for CLI resumption
/remote-env
```

**Use Case**: Start complex task on web (diff view), resume in CLI for deep work

---

## 2. Performance Optimization

### Token Budget Management

**Your 200K Budget Allocation**:
```
CLAUDE.md (rules, hooks, agents):    5-8K tokens (persistent)
Extended thinking budget:             32K tokens (output billing)
Available for conversation:           ~160K tokens
Safe operational ceiling:             140K tokens (70% = auto-compact)
```

### Prompt Caching (90% Savings)

**High-ROI Cache Targets for ggen**:

1. **RDF Specifications** (.specify/**/*.ttl) - 50-100K tokens, 90% savings
2. **Cargo.toml Graph** - 5-10K tokens, 80% savings on workspace structure
3. **Test Fixtures** (tests/fixtures/**) - 10-20K tokens, 85% savings
4. **Chicago TDD Patterns** - 2-5K tokens, 90% savings
5. **.claude/rules/** - 20-30K tokens, 90% savings

**TTL**: 5 minutes, refreshes on each hit (stays alive indefinitely if accessed frequently)

### Auto-Compaction Strategy

**Trigger**: 70% context capacity (140K tokens)
**Preserves**: Code samples, test results, key decisions
**Summarizes**: Earlier turns, verbose outputs

```bash
# Custom compaction
/compact Focus on: current branch changes, test failures, type errors

# Or in CLAUDE.md:
# Compaction Instructions
When auto-compacting, prioritize:
- All modified .rs files and line ranges
- Test execution commands (cargo make test-unit, test-integration)
- Chicago TDD failure messages and type errors
- Andon signals (compiler errors, failed tests)
```

### Tool Cost Hierarchy (Context Impact)

| Tool | Tokens | Optimization |
|------|--------|-------------|
| **Bash** | 50-500 | Filter output with hooks |
| **Read** | 100-2000 | Limit to 2000 line chunks |
| **Glob** | 10-50 | Chain patterns; avoid `**/*` |
| **Grep** | 50-200 | Use `-l` for paths only |
| **Edit** | 100-300 | Batch edits in one call |
| **MCP tools** | 200-1000 | Load on-demand via tool search |

### Parallel Tool Execution (Critical!)

```bash
# ❌ Sequential (3 requests)
Read file A → Read file B → Edit both

# ✅ Parallel (1 request, 40% latency reduction)
Read A, B simultaneously + batch edit both
```

### MCP Tool Search

```bash
# Enable when tools exceed 5% context
export ENABLE_TOOL_SEARCH=auto:5
```

**For your multi-server setup**: Automatically identifies relevant tools from large sets

---

## 3. MCP Server Integration

### Official Rust SDK Pattern (Type-Safe)

```toml
[dependencies]
rmcp = { version = "0.8+", features = ["server", "transport-io", "macros"] }
tokio = { version = "1.35", features = ["full"] }
schemars = "0.8"  # JSON Schema generation
```

### Complete Example: Specification MCP Server for ggen

```rust
use rmcp::{tool, resource, Server};
use tokio::process::Command;

pub struct SpecificationServer {
    workspace_root: String,
    oxigraph_endpoint: String,
}

impl SpecificationServer {
    #[tool(description = "Validate TTL against SHACL shapes")]
    async fn validate_spec(&self, #[tool(param)] path: String)
        -> Result<ToolResult, rmcp::Error>
    {
        let validated_path = self.sanitize_path(&path)?;
        let report = self.run_validation(&validated_path).await?;

        Ok(ToolResult {
            content: vec![Content::text(report)],
            is_error: false,
        })
    }

    #[tool(description = "Generate markdown from TTL specifications")]
    async fn render_spec(&self, #[tool(param)] ttl_path: String)
        -> Result<ToolResult, rmcp::Error>
    {
        let output = Command::new("cargo")
            .args(&["make", "speckit-render"])
            .current_dir(&self.workspace_root)
            .output()
            .await?;

        Ok(ToolResult {
            content: vec![Content::text(String::from_utf8_lossy(&output.stdout).to_string())],
            is_error: !output.status.success(),
        })
    }

    fn sanitize_path(&self, path: &str) -> Result<String, rmcp::Error> {
        if path.contains("..") || path.starts_with('/') {
            return Err(rmcp::Error::invalid_request("Invalid path"));
        }
        Ok(format!("{}/.specify/{}", self.workspace_root, path))
    }
}
```

### Configuration (.mcp.json)

```json
{
  "mcpServers": {
    "specification": {
      "command": "cargo",
      "args": ["run", "--release", "-p", "specification-mcp-server"],
      "env": {
        "OXIGRAPH_ENDPOINT": "http://localhost:7878",
        "WORKSPACE_ROOT": "/home/user/ggen",
        "RUST_LOG": "specification_mcp=debug"
      }
    }
  }
}
```

### Security Patterns

**Input Validation Layer**:
```rust
fn validate_input(&self, input: &str) -> Result<(), Error> {
    // 1. Length limits
    if input.len() > 10_000 {
        return Err(Error::server_error("Input too large"));
    }

    // 2. Allowlist validation
    const ALLOWED_EXTENSIONS: &[&str] = &["ttl", "sparql"];
    // ... validation logic

    // 3. No path traversal
    if input.contains("..") || input.contains("//") {
        return Err(Error::server_error("Invalid path"));
    }

    Ok(())
}
```

### Multi-Server Coordination

**Dependency Graph**:
```yaml
specification-mcp ──> rdf-tools (Oxigraph)
                  ──> git (spec history)

code-quality-mcp ──> bash (cargo make lint)
                  ──> specification (validate specs)

test-orchestrator ──> bash (cargo make test)
                  ──> code-quality (lint after test)
```

---

## 4. Advanced Tool Usage Patterns

### Task Tool Decision Tree

```
Is it READ-ONLY exploration?
  ├─ YES → Explore subagent (Haiku, fast, context-efficient)
  └─ NO → Is it architecture/planning?
            ├─ YES → Plan subagent (inherit model, read-only)
            └─ NO → Is it parallel implementation?
                      ├─ YES, 5+ agents → Agent teams (20 per phase)
                      └─ NO → General-purpose subagent
```

### Agent Selection Matrix

| Agent | Model | Tools | Best For | Context Cost |
|-------|-------|-------|----------|--------------|
| Explore | Haiku | Read-only | Discovery | Low |
| Plan | inherit | Read-only | Architecture | Medium |
| General | inherit | All | Research + action | Medium |
| Teams | inherit | All/Scoped | Parallel work | High |

### Phased Execution with State Persistence

**Pattern**: Explore → Plan → Execute (auto-resume on restart)

```json
{
  "workflow_id": "unique-2026-02-08",
  "current_phase": "execute",
  "phases": {
    "explore": {
      "status": "completed",
      "agents": 5,
      "completed_at": "ISO-8601",
      "outputs": ["Finding 1", "Finding 2"]
    },
    "plan": {
      "status": "completed",
      "agents": 5,
      "outputs": ["Plan 1", "Plan 2"]
    },
    "execute": {
      "status": "in_progress",
      "agents": 20,
      "agents_completed": 12
    }
  }
}
```

**Saved to**: `.claude/autonomous/workflow-state.json`

**Auto-Resume**: On session restart, loads state and continues from last incomplete phase

### ggen Workflow Implementation

```
Phase 1: 5 Explore agents → Search patterns, find opportunities
Phase 2: 5 Plan agents → Design strategies, create specs
Phase 3: 20 Task agents → Implement with collision detection
```

### Chicago TDD Pattern (Mandatory for ggen)

**Single-Message TDD Cycle**:

```bash
"Implement `SpecificationValidator::validate()`

RED: Write failing test
    #[test]
    fn test_invalid_spec_returns_error() {
        let spec = invalid_rdf_doc();
        assert!(SpecificationValidator::new().validate(&spec).is_err());
    }

GREEN: Implement minimal code
    pub fn validate(&self, spec: &RdfDoc) -> Result<(), ValidationError> {
        Err(ValidationError::InvalidFormat)
    }

REFACTOR: Full implementation
    pub fn validate(&self, spec: &RdfDoc) -> Result<(), ValidationError> {
        self.validate_shapes(spec)?;
        self.validate_closure(spec)?;
        Ok(())
    }

Then run: cargo make test-unit
"
```

---

## 5. Automation & Workflow Optimization

### Advanced Hooks System (2026)

**Complete Hook Lifecycle**:

```
SESSION:       SessionStart → [work] → SessionEnd
PROMPT:        UserPromptSubmit → Stop → TaskCompleted
TOOL:          PreToolUse → [execute] → PostToolUse/PostToolUseFailure
SUBAGENT:      SubagentStart → [work] → SubagentStop
NOTIFICATION:  Desktop alerts, Slack integration
COMPACTION:    PreCompact → [cleanup] → SessionStart(compact)
```

### Async Hooks (Non-Blocking)

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "cargo make lint --timeout 60",
            "async": true,
            "timeout": 30
          }
        ]
      }
    ]
  }
}
```

**Benefit**: Runs in background, doesn't block Claude

### Agent-Based Hooks (Multi-Turn Verification)

```json
{
  "hooks": {
    "Stop": [
      {
        "hooks": [
          {
            "type": "agent",
            "prompt": "Run full test suite and verify SLO compliance. Check: 1) All tests pass 2) Build <15s 3) Coverage >80%",
            "timeout": 120
          }
        ]
      }
    ]
  }
}
```

### Quality Gate Automation

**4-Gate Definition of Done (ggen pattern)**:

```bash
✅ cargo make timeout-check   # Verify timeout wrapper exists
✅ cargo make check           # No compiler errors
✅ cargo make test            # All tests pass
✅ cargo make lint            # No warnings
✅ cargo make slo-check       # Performance SLOs met
```

**Hook Integration**:
```bash
#!/bin/bash
# .claude/hooks/quality-gate-validator.sh
# Hooked to: Stop event

# Run all gates in sequence
GATES=("timeout-check" "check" "test" "lint" "slo-check")
FAILED=()

for gate in "${GATES[@]}"; do
  if ! cargo make "$gate"; then
    FAILED+=("$gate")
  fi
done

if [[ ${#FAILED[@]} -gt 0 ]]; then
  echo '{
    "decision": "continue",
    "additionalContext": "Quality gates failed: '"${FAILED[*]}"'. Fix before stopping."
  }'
else
  exit 0  # Allow stop
fi
```

### Andon Protocol (Stop the Line)

**Signal Levels**:
- 🔴 **CRITICAL**: `error[E...]` (compiler errors)
- 🔴 **CRITICAL**: `test ... FAILED` (test failures)
- 🟡 **HIGH**: `warning:` (clippy/warnings)

**Workflow**:
1. **Monitor** - Run checks
2. **Stop** - Detect signal → HALT immediately
3. **Investigate** - 5 Whys root cause analysis
4. **Fix** - Address root cause, not symptom
5. **Verify** - Re-run until cleared

### Batch Operations (10+ Todos Minimum)

```bash
TodoWrite {
  todos: [
    { content: "Issue 1", status: "pending", activeForm: "Fixing issue 1" },
    { content: "Issue 2", status: "pending", activeForm: "Fixing issue 2" },
    ... (minimum 10)
  ]
}
```

**Why**: Prevents context thrashing, ensures comprehensive capture

---

## 6. CI/CD Integration

### GitHub Actions Example

```yaml
name: Claude Code Workflow

on:
  push:
    branches: [main, develop]
    paths: ['.specify/**', 'crates/**']

jobs:
  claude-code-workflow:
    runs-on: ubuntu-latest
    timeout-minutes: 120

    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - name: Restore workflow state
        run: |
          if [[ -f .claude/autonomous/workflow-state.json ]]; then
            echo "Resuming from: $(jq -r '.current_phase' .claude/autonomous/workflow-state.json)"
          fi

      - name: Run 3-phase workflow
        run: |
          claude --agent executor << 'EOF'
          Launch 3-phase workflow:

          Phase 1 (Explore): 3 agents identify issues
          Phase 2 (Plan): 2 agents design fixes
          Phase 3 (Execute): 5 agents implement with Chicago TDD

          Save state to .claude/autonomous/workflow-state.json
          EOF

      - name: Quality gates
        run: |
          cargo make timeout-check
          cargo make check
          cargo make test
          cargo make lint
          cargo make slo-check
```

### Error Recovery with Circuit Breaker

```bash
#!/bin/bash
# .claude/hooks/error-recovery.sh

MAX_RETRIES=3
CIRCUIT_BREAKER_FILE=".claude/circuit-breaker/${TOOL}.state"

check_circuit() {
  # States: CLOSED (normal), OPEN (blocked), HALF_OPEN (testing)
  local state=$(jq -r '.state' "$CIRCUIT_BREAKER_FILE")
  local failures=$(jq -r '.failure_count' "$CIRCUIT_BREAKER_FILE")
  local last_failure=$(jq -r '.last_failure_time' "$CIRCUIT_BREAKER_FILE")

  # Reset after 5 minutes
  if [[ "$state" == "OPEN" ]] && (( $(date +%s) - last_failure > 300 )); then
    echo "HALF_OPEN"
  else
    echo "$state"
  fi
}

# Retry with exponential backoff if circuit allows
if [[ "$(check_circuit)" == "CLOSED" ]]; then
  BACKOFF=$((2 ** ATTEMPT))  # 1s, 2s, 4s
  sleep "$BACKOFF"
  # Retry operation
fi
```

---

## 7. High-Impact Recommendations for ggen

### Immediate Implementations (Week 1)

1. **Enable Agent Teams**
   ```bash
   export CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS=1
   ```

2. **Add Async Hooks for cargo make**
   ```json
   {
     "hooks": {
       "PostToolUse": [
         {
           "matcher": "Write|Edit",
           "hooks": [{
             "type": "command",
             "command": "cargo fmt --check $FILE_PATH",
             "async": true
           }]
         }
       ]
     }
   }
   ```

3. **Create Persistent Subagents**
   ```yaml
   ---
   name: rust-reviewer
   memory: project
   model: sonnet
   ---
   ```

### Medium-Term (Week 2-4)

4. **Implement MCP Specification Server**
   - Create `crates/specification-mcp-server/`
   - Add tools: `validate_spec`, `render_spec`, `list_specs`
   - Configure in `.mcp.json`

5. **Setup Phased Workflow State**
   - Create `.claude/autonomous/workflow-state.json` schema
   - Implement orchestrator in `SessionStart` hook
   - Add auto-resume logic

6. **Configure Quality Gate Hooks**
   - Create `.claude/hooks/quality-gate-validator.sh`
   - Hook to `Stop` event
   - Integrate with Andon protocol

### Advanced (Week 5+)

7. **CI/CD Integration**
   - Add GitHub Actions workflow
   - Implement state persistence across runs
   - Add evidence collection

8. **Monitoring & Observability**
   - Export Prometheus metrics
   - Create Grafana dashboards
   - Track session duration, tool usage, quality gates

9. **Error Recovery Patterns**
   - Implement circuit breaker
   - Add exponential backoff
   - Create retry strategies

10. **RDF-Driven Workflows**
    - Embed phase definitions in TTL specs
    - Auto-generate workflow from ontologies
    - Link to cargo make targets

---

## 8. Configuration Templates

### Enhanced settings.json

```json
{
  "env": {
    "CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS": "1",
    "CLAUDE_AUTOCOMPACT_PCT_OVERRIDE": "70",
    "ENABLE_TOOL_SEARCH": "auto:5"
  },
  "model": "opusplan",
  "effortLevel": "high",
  "extendedThinking": true,
  "hooks": {
    "SessionStart": [
      {
        "matcher": "startup|resume",
        "hooks": [{"type": "command", "command": "./.claude/hooks/session-init.sh"}]
      }
    ],
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [{
          "type": "command",
          "command": "cargo fmt --check $FILE_PATH",
          "async": true
        }]
      }
    ],
    "Stop": [
      {
        "hooks": [{
          "type": "command",
          "command": "./.claude/hooks/quality-gate-validator.sh"
        }]
      }
    ],
    "PreCompact": [
      {
        "matcher": "auto",
        "hooks": [{
          "type": "command",
          "command": "./.claude/hooks/save-workflow-state.sh"
        }]
      }
    ]
  }
}
```

### Workflow State Schema

```json
{
  "workflow_id": "UUID",
  "current_phase": "explore|plan|execute|verify|complete",
  "checksum": "sha256",
  "phases": {
    "explore": {
      "status": "pending|in_progress|completed|failed",
      "agent_type": "Explore",
      "agents_count": 5,
      "timeout_seconds": 1800,
      "retry_count": 0,
      "max_retries": 3,
      "outputs": [],
      "errors": []
    }
  }
}
```

---

## 9. Key Metrics & SLOs

### Session Performance Targets

```yaml
Budget Management:
  - Base context: <30K tokens before feature work
  - Compact at: 70% (140K tokens)
  - Reset after: 20 turns if unrelated tasks

Token Efficiency:
  - Per unit test: <200 tokens
  - Per integration test: <500 tokens
  - Per feature: <3K tokens

Build/Test Feedback:
  - Test output: <100 tokens (filtered)
  - Cargo check: <200 tokens
  - Type errors: Immediate

Context Preservation:
  - Session state: Auto-saved every 10 turns
  - Caching: .ttl, Cargo.toml, rules/
  - Recovery: /resume from last session
```

### Quality Gates (ggen SLOs)

| Metric | Target | Validation |
|--------|--------|------------|
| First build | ≤15s | `cargo make slo-check` |
| Incremental | ≤2s | `cargo make slo-check` |
| RDF processing | ≤5s/1k+ triples | `ggen sync --audit` |
| Memory | ≤100MB | Runtime profiling |
| CLI scaffolding | ≤3s | Integration tests |
| Test coverage | ≥80% | `cargo tarpaulin` |
| Mutation score | ≥60% | `cargo make test-mutation` |

---

## 10. Anti-Patterns to Avoid

### ❌ Kitchen Sink Session
```
Start feature A
Ask unrelated question B
Resume feature A
→ Context filled with irrelevant conversation
```

**Fix**: `/clear` between unrelated tasks

### ❌ Over-Specified CLAUDE.md
```
1500+ lines of detailed instructions
→ Claude reads first 500, ignores critical rules in lines 1000-1500
```

**Fix**: Keep <500 lines; move specialties to skills

### ❌ Sequential File Operations
```
Message 1: Read A
Message 2: Read B
Message 3: Edit D
→ 3 messages × 300 tokens overhead = 900 tokens wasted
```

**Fix**: Parallel reads + batch edits = 1 message

### ❌ Correcting Over and Over
```
"Fix the bug" → Wrong direction
"Actually, try this" → Still wrong
"No, I meant..." → Context polluted
```

**Fix**: After 2 corrections, `/clear` + rewrite prompt with learnings

---

## 11. Resources

### Documentation
- [Claude Code 2.1.0 Release Notes](https://venturebeat.com/orchestration/claude-code-2-1-0-arrives-with-smoother-workflows-and-smarter-agents/)
- [Agent Teams](https://code.claude.com/docs/en/agent-teams.md)
- [Subagents & Memory](https://code.claude.com/docs/en/sub-agents.md)
- [Hooks Guide](https://code.claude.com/docs/en/hooks-guide.md)
- [MCP Documentation](https://code.claude.com/docs/en/mcp.md)
- [Best Practices](https://code.claude.com/docs/en/best-practices.md)
- [Cost Management](https://code.claude.com/docs/en/costs)

### MCP & Rust
- [MCP Specification 2025-11-25](https://modelcontextprotocol.io/specification/2025-11-25)
- [Official Rust SDK](https://github.com/modelcontextprotocol/rust-sdk)
- [MCP Servers Repository](https://github.com/modelcontextprotocol/servers)
- [Building MCP Servers in Rust](https://mcpcat.io/guides/building-mcp-server-rust/)

### ggen Project
- **Repository**: https://github.com/seanchatmangpt/ggen
- **Documentation**: /home/user/ggen/docs/
- **Rules**: /home/user/ggen/.claude/rules/
- **Research**: /home/user/ggen/docs/research/

---

## Conclusion

The 2026 Claude Code landscape provides unprecedented automation capabilities perfectly aligned with your specification-driven development workflow. The key is leveraging:

1. **Agent coordination** (teams + subagents + phased workflows)
2. **Deterministic automation** (hooks + quality gates)
3. **Context optimization** (caching + compaction + tool search)
4. **Error resilience** (circuit breakers + retries + Andon protocol)
5. **Type-safe integrations** (MCP servers in Rust)

Your ggen project is already a reference implementation with its phased-plan-executor pattern, autonomous workflow state persistence, and Chicago TDD enforcement. The recommendations above will enhance what's already a production-grade setup.

**Next Steps**: Start with async hooks and agent teams (Week 1), then build out the MCP specification server and workflow orchestration (Weeks 2-4), finally adding monitoring and advanced error recovery (Weeks 5+).
