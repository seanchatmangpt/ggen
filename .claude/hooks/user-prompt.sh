#!/bin/bash
# Timeout: 3s
# Purpose: Validate user prompts and detect anti-patterns.
#          Non-blocking (exit 0 always) -- surfaces violations to stderr.

set -euo pipefail

PROMPT="${1:-}"

timeout 3s bash -c '
  PROMPT="$1"

  # Detect workspace root from git
  WORKSPACE_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || echo "")"

  # Check for empty prompt
  if [[ -z "$PROMPT" ]]; then
    exit 0
  fi

  # DETECT: Self-certification without evidence
  if echo "$PROMPT" | grep -qiE "(looks correct|should work|seems fine|looks good)" && \
     ! echo "$PROMPT" | grep -qiE "(test|verify|check|otel|span|trace|evidence|proof|cargo make)"; then
    echo "WARNING: Self-certification without evidence detected" >&2
    echo "  Claims like \"looks correct\" require test results or OTEL spans as proof" >&2
  fi

  # DETECT: Narration instead of verification
  if echo "$PROMPT" | grep -qiE "(should have|would be|probably|likely|I think)" && \
     echo "$PROMPT" | grep -qiE "(done|complete|finished|working|ready)"; then
    echo "WARNING: Narration detected -- verify before claiming completion" >&2
    echo "  Replace \"should work\" with: cargo make test && cargo make check" >&2
  fi

  # DETECT: Completion claim without validation keywords
  if echo "$PROMPT" | grep -qiE "\b(done|complete|finished|ready)\b" && \
     ! echo "$PROMPT" | grep -qiE "(test|verify|check|validate|prove|run cargo|cargo make)"; then
    echo "WARNING: Completion claim without validation" >&2
    echo "  Definition of Done: cargo make check && cargo make lint && cargo make test" >&2
  fi

  # DETECT: Requests to skip safety checks
  if echo "$PROMPT" | grep -qiE "(skip|ignore|bypass|disable).*(test|check|lint|validation)"; then
    echo "WARNING: Request to skip safety checks detected" >&2
    echo "  All checks are required per CLAUDE.md" >&2
  fi

  # DETECT: Direct cargo command requests
  if echo "$PROMPT" | grep -qE "run\s+(cargo\s+(check|test|build|clippy|fmt))"; then
    echo "WARNING: Direct cargo command requested" >&2
    echo "  Use cargo make instead: cargo make <target>" >&2
  fi

  # DETECT: Requests to save to root folder
  if echo "$PROMPT" | grep -qiE "(save|write|create).*\s+(to|in|at)\s+root"; then
    echo "WARNING: Request to save to root folder detected" >&2
    echo "  Use subdirectories: crates/*/src/, docs/, tests/, etc." >&2
  fi

  # DETECT: Relative paths (should use absolute)
  if echo "$PROMPT" | grep -qE "\./|\.\./" && ! echo "$PROMPT" | grep -q "example"; then
    echo "WARNING: Relative path detected in prompt" >&2
    if [[ -n "$WORKSPACE_ROOT" ]]; then
      echo "  Use absolute paths: ${WORKSPACE_ROOT}/..." >&2
    fi
  fi

  # DETECT: Dangerous destructive patterns
  if echo "$PROMPT" | grep -qiE "(delete all|remove everything|wipe|nuke)"; then
    echo "WARNING: Destructive operation requested" >&2
    echo "  Ensure this is intentional before proceeding" >&2
  fi

  # DETECT: TodoWrite batch size reminder
  if echo "$PROMPT" | grep -qiE "todo|task" && echo "$PROMPT" | grep -qiE "create|write|add"; then
    echo "REMINDER: TodoWrite requires 10+ todos in ONE batch" >&2
  fi

  # DETECT: Agent execution reminder
  if echo "$PROMPT" | grep -qiE "(agent|swarm|spawn|coordinate)"; then
    echo "REMINDER: Use Claude Code Task tool for agent execution" >&2
    echo "  MCP only for coordination topology" >&2
  fi

  # DETECT: Spec edits targeting markdown instead of TTL
  if echo "$PROMPT" | grep -qiE "(spec|ontology|rdf|ttl).*\.(md|markdown)"; then
    echo "WARNING: Request involves markdown specs" >&2
    echo "  RDF (.ttl) is source of truth, .md is generated" >&2
    echo "  Edit .specify/*.ttl, then: cargo make speckit-render" >&2
  fi

  # DETECT: unwrap/expect requests in production
  if echo "$PROMPT" | grep -qiE "unwrap|expect" && echo "$PROMPT" | grep -qiE "production|src/"; then
    echo "WARNING: unwrap/expect in production code" >&2
    echo "  Use Result<T, E> instead. unwrap() only allowed in tests." >&2
  fi

  exit 0
' bash "$PROMPT"

exit $?
