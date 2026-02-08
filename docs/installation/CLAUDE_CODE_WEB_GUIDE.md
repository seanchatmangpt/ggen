<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Claude Code Web Agent Integration](#claude-code-web-agent-integration)
  - [Overview](#overview)
  - [Agent Initialization](#agent-initialization)
    - [SessionStart Hook Configuration](#sessionstart-hook-configuration)
  - [Workflow Integration](#workflow-integration)
    - [Workflow 1: Specification-Driven Generation](#workflow-1-specification-driven-generation)
    - [Workflow 2: Multi-Agent Collaboration](#workflow-2-multi-agent-collaboration)
    - [Workflow 3: Continuous Generation](#workflow-3-continuous-generation)
  - [Agent Invocation Patterns](#agent-invocation-patterns)
    - [Pattern A: Direct Execution](#pattern-a-direct-execution)
    - [Pattern B: JSON Output for Processing](#pattern-b-json-output-for-processing)
    - [Pattern C: Dry-Run for Planning](#pattern-c-dry-run-for-planning)
  - [Error Handling for Agents](#error-handling-for-agents)
    - [Error Detection](#error-detection)
    - [Root Cause Analysis](#root-cause-analysis)
  - [Receipt Verification (Determinism Guarantee)](#receipt-verification-determinism-guarantee)
  - [Performance Optimization](#performance-optimization)
    - [Caching Strategy](#caching-strategy)
    - [Resource Constraints](#resource-constraints)
    - [Parallel Agent Coordination](#parallel-agent-coordination)
  - [Integration with Agent Memory](#integration-with-agent-memory)
  - [Command Reference (Agent-Focused)](#command-reference-agent-focused)
  - [Safety Constraints](#safety-constraints)
    - [What Agents MUST Do](#what-agents-must-do)
    - [What Agents MUST NOT Do](#what-agents-must-not-do)
  - [Troubleshooting for Agents](#troubleshooting-for-agents)
    - [Installation Timeout](#installation-timeout)
    - [SPARQL Query Failures](#sparql-query-failures)
    - [Template Rendering Failures](#template-rendering-failures)
  - [Next Steps for Agent Developers](#next-steps-for-agent-developers)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Claude Code Web Agent Integration

**ggen as an agent capability for Claude Code Web systems**

## Overview

ggen is a deterministic code generation system designed for AI agent automation. Claude Code Web agents can invoke ggen to transform RDF specifications into reproducible code artifacts.

## Agent Initialization

### SessionStart Hook Configuration

```json
{
  ".claude/settings.json": {
    "hooks": {
      "SessionStart": [
        {
          "matcher": "startup",
          "hooks": [
            {
              "type": "command",
              "command": "cargo install ggen --locked",
              "timeout": 120000,
              "onError": "warn"
            },
            {
              "type": "command",
              "command": "ggen init",
              "timeout": 30000
            }
          ]
        }
      ]
    }
  }
}
```

## Workflow Integration

### Workflow 1: Specification-Driven Generation

**Agent Steps**:
1. Receive specification in natural language
2. Convert to RDF ontology (.ttl)
3. Validate with `ggen sync --validate_only true`
4. Generate code with `ggen sync --audit true`
5. Return artifacts + receipt for verification

### Workflow 2: Multi-Agent Collaboration

**Coordinator Agent**:
```bash
# Master orchestrates multiple agents
ggen sync --dry_run true  # Preview
# Coordinate with specialized agents:
# - RDF Agent: Refines ontology
# - Template Agent: Creates/updates templates
# - Validation Agent: Checks quality gates
# - Verification Agent: Confirms receipts match
```

### Workflow 3: Continuous Generation

**Monitor Agent**:
```bash
# Watch for ontology changes
ggen sync --watch true

# Automatically regenerate on:
# - .specify/specs/**/*.ttl changes
# - templates/** changes
# - ggen.toml modifications
```

## Agent Invocation Patterns

### Pattern A: Direct Execution

```bash
# From agent code/terminal
ggen sync --audit true

# Returns:
# - Exit code 0 (success) or non-zero (failure)
# - Generated artifacts in src/
# - Receipt in .ggen/receipts/latest.json
```

### Pattern B: JSON Output for Processing

```bash
# Use for machine parsing
ggen sync --audit true --output json

# Returns: Structured JSON with:
# - status: "success"|"failed"
# - files_generated: [list]
# - receipt: {metadata}
# - errors: [if any]
```

### Pattern C: Dry-Run for Planning

```bash
# Preview without writing
ggen sync --dry_run true

# Safe for:
# - Pre-generation validation
# - Estimating scope
# - Agent decision-making
```

## Error Handling for Agents

### Error Detection

```bash
ggen sync --audit true
EXIT_CODE=$?

if [ $EXIT_CODE -ne 0 ]; then
  case $EXIT_CODE in
    2) echo "Validation failed" ;;
    4) echo "SPARQL error - ontology syntax" ;;
    5) echo "Template error" ;;
    *) echo "Unknown error" ;;
  esac
fi
```

### Root Cause Analysis

When generation fails, agent should:

1. Check exit code (indicates failure type)
2. Read logs: `cat .ggen/ggen.log`
3. Validate ontology: `ggen sync --validate_only true`
4. Report structured error for coordinator agent

## Receipt Verification (Determinism Guarantee)

Every agent invocation generates proof:

```bash
# Inspect receipt after generation
cat .ggen/receipts/latest.json

# Contains:
# {
#   "execution_id": "abc123",
#   "timestamp": "2026-01-29T...",
#   "manifest_hash": "sha256:...",
#   "ontology_hash": "sha256:...",
#   "files": [{path, hash, size}, ...],
#   "generation_time_ms": 1234
# }
```

**Verification**: Same ontology + same templates = identical receipt

## Performance Optimization

### Caching Strategy

```bash
# Cache enabled by default
ggen sync --cache true

# First run: ~15 seconds
# Subsequent runs: ~2-3 seconds (6x faster)
```

### Resource Constraints

Agent should respect:
- **CPU**: 2 cores recommended
- **RAM**: 2 GB for large ontologies (1k+ triples)
- **Timeout**: 120 seconds for network-limited environments
- **Disk**: 500 MB for artifacts + cache

### Parallel Agent Coordination

```bash
# Coordinator spawns multiple agents in parallel
# Each agent gets isolated .ggen/ directories
# Merge results in coordinator

export GGEN_HOME=.ggen-agent-$AGENT_ID
ggen sync  # Isolated execution
```

## Integration with Agent Memory

Store generation metadata for future reference:

```json
{
  "generation_events": [
    {
      "timestamp": "2026-01-29T10:00:00Z",
      "ontology_hash": "sha256:...",
      "receipt_id": "abc123",
      "artifacts": ["src/api.rs", "src/models.rs"],
      "agent": "code-generator-v1",
      "status": "success"
    }
  ]
}
```

## Command Reference (Agent-Focused)

| Command | Use Case | Returns |
|---------|----------|---------|
| `ggen sync` | Generate code | Exit code + artifacts |
| `ggen sync --dry_run true` | Preview | Exit code + plan |
| `ggen sync --validate_only true` | Validate specs | Exit code only |
| `ggen sync --audit true` | Generate + proof | Exit code + receipt |
| `ggen init` | Setup project | Creates directory structure |
| `ggen --version` | Version check | Version string |

## Safety Constraints

### What Agents MUST Do

- ✅ Use `--dry_run true` before committing changes
- ✅ Verify exit codes - don't assume success
- ✅ Store receipts for audit trails
- ✅ Respect timeout limits (120s max)
- ✅ Validate ontologies before generation

### What Agents MUST NOT Do

- ❌ Edit generated files directly (regenerate from ontology)
- ❌ Ignore generation failures
- ❌ Run without `--audit true` for reproducibility
- ❌ Modify .ttl files without validation
- ❌ Assume identical output without receipt verification

## Troubleshooting for Agents

### Installation Timeout

```bash
# If cargo install takes > 120s:
# Option A: Use pre-built binary
curl -L https://github.com/seanchatmangpt/ggen/releases/download/v6.0.0/ggen-x86_64-unknown-linux-gnu.tar.gz | tar xz

# Option B: Increase timeout in SessionStart hook
"timeout": 180000  # 3 minutes
```

### SPARQL Query Failures

```bash
# Ontology syntax error - validate before generation
ggen sync --validate_only true
# If fails: Check .ttl syntax, namespace declarations
```

### Template Rendering Failures

```bash
# Template variable mismatch - debug output
GGEN_LOG_LEVEL=debug ggen sync --dry_run true
# Look for: "template context" in output
```

## Next Steps for Agent Developers

- Review: `/docs/reference/01-commands.md` - Full command API
- Study: `/docs/explanation/01-concepts.md` - Ontology/RDF/SPARQL primer
- Build: `/docs/how-to/01-common-tasks.md` - Real generation patterns

---

**Agent System Information**

- **Designed for**: Claude Code Web agents, AI automation systems
- **Invocation**: Shell commands (bash) or direct cargo invocation
- **Output format**: Exit codes, JSON, file artifacts
- **Determinism**: 100% reproducible with receipt verification
- **Timeout SLO**: 15 seconds typical, 120 seconds max
