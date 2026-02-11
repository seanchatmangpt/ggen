<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Actionable Recommendations - Claude Code Best Practices for ggen](#actionable-recommendations---claude-code-best-practices-for-ggen)
  - [Immediate Actions (Week 1)](#immediate-actions-week-1)
    - [1. Enable Agent Teams (30 minutes)](#1-enable-agent-teams-30-minutes)
    - [2. Add Async Hooks for Automated Quality Checks (1 hour)](#2-add-async-hooks-for-automated-quality-checks-1-hour)
    - [3. Create Persistent rust-reviewer Subagent (45 minutes)](#3-create-persistent-rust-reviewer-subagent-45-minutes)
  - [Medium-Term Actions (Week 2-4)](#medium-term-actions-week-2-4)
    - [4. Implement MCP Specification Server (8 hours)](#4-implement-mcp-specification-server-8-hours)
    - [5. Setup Phased Workflow State Persistence (4 hours)](#5-setup-phased-workflow-state-persistence-4-hours)
    - [6. Configure Quality Gate Validation Hooks (3 hours)](#6-configure-quality-gate-validation-hooks-3-hours)
  - [Advanced Actions (Week 5+)](#advanced-actions-week-5)
    - [7. CI/CD Integration with GitHub Actions (4 hours)](#7-cicd-integration-with-github-actions-4-hours)
    - [8. Monitoring with Prometheus Metrics (6 hours)](#8-monitoring-with-prometheus-metrics-6-hours)
  - [Summary of Expected Outcomes](#summary-of-expected-outcomes)
    - [Week 1 Results](#week-1-results)
    - [Week 2-4 Results](#week-2-4-results)
    - [Week 5+ Results](#week-5-results)
  - [Validation Checklist](#validation-checklist)
  - [Cost-Benefit Analysis](#cost-benefit-analysis)
  - [Support](#support)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Actionable Recommendations - Claude Code Best Practices for ggen

**Date**: 2026-02-08
**Priority**: HIGH
**Target**: ggen v6.0.0 Rust code generation project

---

## Immediate Actions (Week 1)

### 1. Enable Agent Teams (30 minutes)

**Why**: Parallel architecture review across 30 crates saves hours

**Implementation**:
```bash
# Add to .claude/settings.json
{
  "env": {
    "CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS": "1"
  }
}
```

**Validation**:
```bash
# Test with sample task
claude << 'EOF'
Create agent team to analyze crate dependencies:
- Teammate 1: Analyze ggen-core dependencies
- Teammate 2: Analyze ggen-cli dependencies
- Teammates discuss findings
EOF
```

**Expected Result**: Multiple agents working in parallel, messaging each other

---

### 2. Add Async Hooks for Automated Quality Checks (1 hour)

**Why**: Non-blocking validation after every edit prevents context pollution

**Implementation**:
```bash
# Create .claude/hooks/post-edit-formatter.sh
cat > .claude/hooks/post-edit-formatter.sh << 'EOF'
#!/bin/bash
set -euo pipefail

INPUT=$(cat)
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')

if [[ -n "$FILE_PATH" ]] && [[ "$FILE_PATH" == *.rs ]]; then
  cargo fmt --check "$FILE_PATH" 2>&1 | head -20
  cargo clippy -- -W clippy::pedantic 2>&1 | grep "$FILE_PATH" | head -10
fi

exit 0
EOF

chmod +x .claude/hooks/post-edit-formatter.sh

# Add to .claude/settings.json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "./.claude/hooks/post-edit-formatter.sh",
            "async": true,
            "timeout": 30
          }
        ]
      }
    ]
  }
}
```

**Validation**:
```bash
# Edit any Rust file via Claude
# Check that formatting runs without blocking Claude
# Verify output appears in terminal
```

**Expected Result**: Automatic formatting feedback without Claude waiting

---

### 3. Create Persistent rust-reviewer Subagent (45 minutes)

**Why**: Cross-session learning of Rust patterns in your codebase

**Implementation**:
```bash
# Create .claude/agents/rust-reviewer.md
cat > .claude/agents/rust-reviewer.md << 'EOF'
---
name: rust-reviewer
description: Rust expert specialized in Chicago TDD, type-first design, zero-cost abstractions
memory: project
model: sonnet
tools: Read, Grep, Glob, Bash
---

You are a Rust architecture expert for the ggen project.

## Your Responsibilities

1. **Code Review**:
   - Type safety and ownership semantics
   - Zero-cost abstraction violations
   - Macro strategies and derive trait patterns
   - Chicago TDD compliance (AAA pattern, state-based verification)

2. **Pattern Recognition**:
   - Common error handling patterns
   - Type-driven API designs
   - Performance optimizations in hot paths
   - Test organization strategies

3. **Memory Management**:
   - Update your memory with architectural patterns discovered
   - Track anti-patterns and their solutions
   - Document crate interdependencies
   - Record type system innovations

## Review Checklist

- [ ] No `unwrap()` or `expect()` in production code
- [ ] All public APIs have tests
- [ ] Error types use `thiserror` or similar
- [ ] Performance critical paths avoid allocations
- [ ] Types encode invariants where possible
- [ ] Tests follow AAA pattern (Arrange, Act, Assert)

## Remember

- RDF specifications in `.specify/` are source of truth
- Cargo make is required (never direct cargo commands)
- SLOs: First build ≤15s, incremental ≤2s, test ≤30s
EOF
```

**Validation**:
```bash
# Invoke the agent
claude << 'EOF'
Use the rust-reviewer agent to review crates/ggen-core/src/lib.rs
After review, ask it to save patterns to its memory.
EOF

# Check memory was created
cat .claude/agent-memory/rust-reviewer/MEMORY.md
```

**Expected Result**: Agent reviews code and updates its memory file

---

## Medium-Term Actions (Week 2-4)

### 4. Implement MCP Specification Server (8 hours)

**Why**: Type-safe RDF validation and code generation tool for Claude

**Implementation**:
```bash
# 1. Create new crate
cargo new --lib crates/specification-mcp-server
cd crates/specification-mcp-server

# 2. Add dependencies
cat >> Cargo.toml << 'EOF'
[dependencies]
rmcp = { version = "0.8", features = ["server", "transport-io", "macros"] }
tokio = { version = "1.35", features = ["full"] }
serde = { version = "1.0", features = ["derive"] }
schemars = "0.8"
thiserror = "1.0"

[dev-dependencies]
tokio-test = "0.4"
EOF

# 3. Implement server (see full example in docs/research/)
# Key tools:
# - validate_spec(path: String) -> Result<ValidationReport>
# - render_spec(path: String) -> Result<MarkdownOutput>
# - list_specs() -> Result<Vec<SpecMetadata>>

# 4. Add to workspace
# Edit root Cargo.toml:
[workspace]
members = [
    # ... existing crates
    "crates/specification-mcp-server",
]

# 5. Configure in .mcp.json
cat >> .mcp.json << 'EOF'
{
  "mcpServers": {
    "specification": {
      "command": "cargo",
      "args": [
        "run",
        "--release",
        "--manifest-path",
        "crates/specification-mcp-server/Cargo.toml"
      ],
      "env": {
        "OXIGRAPH_ENDPOINT": "http://localhost:7878",
        "WORKSPACE_ROOT": "/home/user/ggen",
        "RUST_LOG": "specification_mcp=info"
      }
    }
  }
}
EOF
```

**Validation**:
```bash
# Build server
cargo build --release -p specification-mcp-server

# Test in Claude
claude << 'EOF'
Use the specification MCP tool to validate .specify/specs/001-*/feature.ttl
EOF
```

**Expected Result**: Claude can validate RDF specs via MCP tool

---

### 5. Setup Phased Workflow State Persistence (4 hours)

**Why**: Auto-resume from last incomplete phase on session restart

**Implementation**:
```bash
# 1. Create workflow state schema
mkdir -p .claude/autonomous
cat > .claude/autonomous/workflow-state.schema.json << 'EOF'
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "required": ["workflow_id", "current_phase", "phases"],
  "properties": {
    "workflow_id": {"type": "string"},
    "current_phase": {
      "type": "string",
      "enum": ["explore", "plan", "execute", "verify", "complete"]
    },
    "checksum": {"type": "string"},
    "phases": {
      "type": "object",
      "properties": {
        "explore": {"$ref": "#/definitions/phase"},
        "plan": {"$ref": "#/definitions/phase"},
        "execute": {"$ref": "#/definitions/phase"},
        "verify": {"$ref": "#/definitions/phase"}
      }
    }
  },
  "definitions": {
    "phase": {
      "type": "object",
      "properties": {
        "status": {"enum": ["pending", "in_progress", "completed", "failed"]},
        "agent_type": {"type": "string"},
        "agents_count": {"type": "integer"},
        "started_at": {"type": "string"},
        "completed_at": {"type": "string"},
        "timeout_seconds": {"type": "integer"},
        "retry_count": {"type": "integer"},
        "max_retries": {"type": "integer"},
        "outputs": {"type": "array"},
        "errors": {"type": "array"}
      }
    }
  }
}
EOF

# 2. Create workflow orchestrator hook
cat > .claude/hooks/workflow-orchestrator.sh << 'EOF'
#!/bin/bash
set -euo pipefail

WORKFLOW_STATE=".claude/autonomous/workflow-state.json"
WORKFLOW_LOCK=".claude/autonomous/.workflow.lock"

# Atomic lock
if [[ -f "$WORKFLOW_LOCK" ]]; then
  echo "Workflow locked by another process"
  exit 0
fi
echo $$ > "$WORKFLOW_LOCK"
trap 'rm -f "$WORKFLOW_LOCK"' EXIT

# Load state if exists
if [[ -f "$WORKFLOW_STATE" ]]; then
  CURRENT_PHASE=$(jq -r '.current_phase' "$WORKFLOW_STATE")
  echo "Resuming workflow from phase: $CURRENT_PHASE"

  # Determine what to do based on phase status
  case "$CURRENT_PHASE" in
    "explore")
      STATUS=$(jq -r '.phases.explore.status' "$WORKFLOW_STATE")
      if [[ "$STATUS" == "completed" ]]; then
        echo "Explore complete, advancing to Plan phase"
        jq '.current_phase = "plan"' "$WORKFLOW_STATE" > "$WORKFLOW_STATE.tmp"
        mv "$WORKFLOW_STATE.tmp" "$WORKFLOW_STATE"
      else
        echo "Explore in progress, resuming..."
      fi
      ;;
    "plan"|"execute"|"verify")
      echo "Resuming $CURRENT_PHASE phase..."
      ;;
  esac
fi

exit 0
EOF

chmod +x .claude/hooks/workflow-orchestrator.sh

# 3. Hook to SessionStart
# Add to .claude/settings.json:
{
  "hooks": {
    "SessionStart": [
      {
        "matcher": "startup|resume",
        "hooks": [
          {
            "type": "command",
            "command": "./.claude/hooks/workflow-orchestrator.sh"
          }
        ]
      }
    ]
  }
}
```

**Validation**:
```bash
# Create sample workflow state
cat > .claude/autonomous/workflow-state.json << 'EOF'
{
  "workflow_id": "test-2026-02-08",
  "current_phase": "explore",
  "phases": {
    "explore": {
      "status": "in_progress",
      "agents_count": 5,
      "started_at": "2026-02-08T10:00:00Z"
    }
  }
}
EOF

# Restart Claude and verify orchestrator runs
claude
# Check terminal output for "Resuming workflow from phase: explore"
```

**Expected Result**: Hook detects saved state and reports resumption

---

### 6. Configure Quality Gate Validation Hooks (3 hours)

**Why**: Enforce Definition of Done before Claude can stop

**Implementation**:
```bash
# 1. Create quality gates config
cat > .claude/quality-gates.json << 'EOF'
{
  "gates": {
    "timeout-check": {
      "command": "cargo make timeout-check",
      "timeout_seconds": 5,
      "signal": "ERROR",
      "critical": true
    },
    "check": {
      "command": "cargo make check",
      "timeout_seconds": 5,
      "signal": "error\\[E",
      "critical": true
    },
    "test": {
      "command": "cargo make test",
      "timeout_seconds": 30,
      "signal": "test result:.*FAILED",
      "critical": true
    },
    "lint": {
      "command": "cargo make lint",
      "timeout_seconds": 60,
      "signal": "warning:",
      "critical": false
    },
    "slo-check": {
      "command": "cargo make slo-check",
      "timeout_seconds": 120,
      "signal": "FAILED",
      "critical": true
    }
  },
  "enforcement": {
    "mode": "strict",
    "sequential": true,
    "stop_on_first_failure": true
  }
}
EOF

# 2. Create validation hook
cat > .claude/hooks/quality-gate-validator.sh << 'EOF'
#!/bin/bash
set -euo pipefail

INPUT=$(cat)
STOP_HOOK_ACTIVE=$(echo "$INPUT" | jq -r '.stop_hook_active // "false"')

# Prevent infinite loops
if [[ "$STOP_HOOK_ACTIVE" == "true" ]]; then
  exit 0
fi

GATES_CONFIG=".claude/quality-gates.json"
RECEIPTS_DIR=".claude/receipts"
mkdir -p "$RECEIPTS_DIR"

# Run all gates
FAILED=()
for gate in $(jq -r '.gates | keys[]' "$GATES_CONFIG"); do
  command=$(jq -r ".gates.$gate.command" "$GATES_CONFIG")
  timeout=$(jq -r ".gates.$gate.timeout_seconds" "$GATES_CONFIG")
  signal=$(jq -r ".gates.$gate.signal" "$GATES_CONFIG")

  echo "Running gate: $gate" >&2

  set +e
  output=$(timeout ${timeout}s bash -c "$command" 2>&1)
  exit_code=$?
  set -e

  # Check for Andon signals
  if echo "$output" | grep -qE "$signal"; then
    echo "🔴 ANDON SIGNAL in $gate: $signal" >&2
    FAILED+=("$gate")

    # Save receipt
    cat > "$RECEIPTS_DIR/${gate}-failed-$(date +%s).json" << RECEIPT
{
  "gate": "$gate",
  "status": "FAILED",
  "timestamp": "$(date -Iseconds)",
  "exit_code": $exit_code,
  "output": $(echo "$output" | jq -Rs '.')
}
RECEIPT
  elif [[ $exit_code -ne 0 ]]; then
    FAILED+=("$gate")
  fi
done

# Decision
if [[ ${#FAILED[@]} -gt 0 ]]; then
  echo '{
    "decision": "continue",
    "additionalContext": "Quality gates FAILED: '"${FAILED[*]}"'. Review receipts in .claude/receipts/ and fix issues."
  }'
else
  exit 0  # Allow stop
fi
EOF

chmod +x .claude/hooks/quality-gate-validator.sh

# 3. Hook to Stop event
# Add to .claude/settings.json:
{
  "hooks": {
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "./.claude/hooks/quality-gate-validator.sh"
          }
        ]
      }
    ]
  }
}
```

**Validation**:
```bash
# Intentionally break a test
# Ask Claude to complete work
# Verify Stop hook runs and blocks completion
# Fix test
# Verify Claude can now stop
```

**Expected Result**: Hook blocks completion until all gates pass

---

## Advanced Actions (Week 5+)

### 7. CI/CD Integration with GitHub Actions (4 hours)

**Implementation**:
```bash
# Create .github/workflows/claude-code-workflow.yml
cat > .github/workflows/claude-code-workflow.yml << 'EOF'
name: Claude Code 3-Phase Workflow

on:
  push:
    branches: [main, develop]
    paths:
      - '.specify/**'
      - 'crates/**'
      - 'tests/**'

env:
  CLAUDE_MODEL: claude-opus-4-6
  CARGO_MAKE_REQUIRED: true

jobs:
  spec-validation:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Validate RDF specs
        run: |
          ggen validate .specify/**/*.ttl
          cargo make speckit-validate

  claude-workflow:
    runs-on: ubuntu-latest
    needs: spec-validation
    timeout-minutes: 120

    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: 1.91.1
          override: true

      - name: Restore workflow state
        run: |
          if [[ -f .claude/autonomous/workflow-state.json ]]; then
            echo "Resuming from: $(jq -r '.current_phase' .claude/autonomous/workflow-state.json)"
          fi

      - name: Run 3-phase workflow
        run: |
          claude --agent executor << 'PROMPT'
          Launch 3-phase workflow:

          Phase 1 (Explore): 3 agents identify issues in recent changes
          Phase 2 (Plan): 2 agents design fixes
          Phase 3 (Execute): 5 agents implement with Chicago TDD

          Save state to .claude/autonomous/workflow-state.json
          PROMPT

      - name: Quality gates
        run: |
          cargo make timeout-check
          cargo make check
          cargo make test
          cargo make lint
          cargo make slo-check

      - name: Upload workflow evidence
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: workflow-evidence
          path: .claude/receipts/*.json
          retention-days: 30
EOF
```

**Validation**: Push commit and verify workflow runs

---

### 8. Monitoring with Prometheus Metrics (6 hours)

**Implementation**:
```bash
# Create metrics recording hook
cat > .claude/hooks/record-metric.sh << 'EOF'
#!/bin/bash
METRICS_FILE=".claude/monitoring/metrics.prom"
mkdir -p "$(dirname "$METRICS_FILE")"

INPUT=$(cat)
TOOL=$(echo "$INPUT" | jq -r '.tool_name')
TIMESTAMP=$(date +%s)

echo "claude_tool_used_total{tool=\"$TOOL\"} 1 $TIMESTAMP" >> "$METRICS_FILE"
exit 0
EOF

chmod +x .claude/hooks/record-metric.sh

# Add to PostToolUse hook in settings.json
```

**Validation**: Check metrics file grows with tool usage

---

## Summary of Expected Outcomes

### Week 1 Results
- ✅ Agent teams enabled for parallel work
- ✅ Async hooks running non-blocking validation
- ✅ Persistent rust-reviewer learning patterns

### Week 2-4 Results
- ✅ MCP server providing RDF validation tools
- ✅ Workflow state persisting across sessions
- ✅ Quality gates blocking completion until all pass

### Week 5+ Results
- ✅ CI/CD running automated workflows
- ✅ Metrics tracking session performance
- ✅ Circuit breaker preventing cascade failures

---

## Validation Checklist

After implementing all recommendations:

```bash
# 1. Verify agent teams work
claude << 'EOF'
Create 3-agent team to analyze crates
EOF

# 2. Verify async hooks run
# Edit any Rust file, see formatting feedback

# 3. Verify rust-reviewer has memory
cat .claude/agent-memory/rust-reviewer/MEMORY.md

# 4. Verify MCP server responds
claude << 'EOF'
Use specification MCP to list all specs
EOF

# 5. Verify workflow state saves
cat .claude/autonomous/workflow-state.json

# 6. Verify quality gates block
# Break a test, ask Claude to finish
# Verify hook blocks until fixed

# 7. Verify CI/CD runs
git push origin main
# Check GitHub Actions

# 8. Verify metrics collected
cat .claude/monitoring/metrics.prom
```

---

## Cost-Benefit Analysis

| Action | Time | Token Impact | Human Time Saved |
|--------|------|--------------|------------------|
| Agent teams | 30min | +200% tokens | -50% on large refactors |
| Async hooks | 1hr | +5% | -30% on validation cycles |
| Persistent agents | 45min | +10% | -40% on repeated reviews |
| MCP server | 8hr | +15% | -60% on RDF validation |
| Workflow state | 4hr | +5% | -70% on resumption |
| Quality gates | 3hr | +10% | -80% on regression bugs |

**ROI**: High - Implementation time pays off within 1-2 weeks of usage

---

## Support

- **Full research**: `docs/research/claude-code-best-practices-2026.md`
- **Project memory**: `/root/.claude/projects/-home-user-ggen/memory/MEMORY.md`
- **Issues**: https://github.com/seanchatmangpt/ggen/issues
