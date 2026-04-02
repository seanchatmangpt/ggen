#!/bin/bash
# Timeout: 3s
# Purpose: Validate user inputs and sanitize prompts

set -euo pipefail

PROMPT="${1:-}"

timeout 3s bash -c '
  PROMPT="$1"

  # Check for empty prompt
  if [[ -z "$PROMPT" ]]; then
    echo "⚠️  Empty prompt detected" >&2
    exit 0
  fi

  # Check for relative paths (should use absolute paths)
  if echo "$PROMPT" | grep -qE "\./|\.\./" && ! echo "$PROMPT" | grep -q "example"; then
    echo "⚠️  WARNING: Relative path detected in prompt" >&2
    echo "   → ggen requires absolute paths: /home/user/ggen/..." >&2
  fi

  # Check for dangerous patterns
  if echo "$PROMPT" | grep -qiE "(delete all|remove everything|wipe|nuke)"; then
    echo "⚠️  WARNING: Destructive operation requested" >&2
    echo "   → Ensure this is intentional before proceeding" >&2
  fi

  # Check for requests to skip safety checks
  if echo "$PROMPT" | grep -qiE "(skip|ignore|bypass|disable).*(test|check|lint|validation)"; then
    echo "❌ WARNING: Request to skip safety checks detected" >&2
    echo "   → All checks are required per CLAUDE.md" >&2
    echo "   → cargo make enforces quality gates" >&2
  fi

  # Check for direct cargo command requests
  if echo "$PROMPT" | grep -qE "run\s+(cargo\s+(check|test|build|clippy|fmt))"; then
    echo "⚠️  WARNING: Direct cargo command requested" >&2
    echo "   → Use cargo make instead: cargo make <target>" >&2
  fi

  # Check for requests to save to root folder
  if echo "$PROMPT" | grep -qiE "(save|write|create).*\s+(to|in|at)\s+(root|/home/user/ggen/[^/]+\.(rs|toml))"; then
    echo "⚠️  WARNING: Request to save to root folder detected" >&2
    echo "   → Use subdirectories: crates/*/src/, docs/, tests/, etc." >&2
  fi

  # Check for TodoWrite batch size
  if echo "$PROMPT" | grep -qiE "todo|task" && echo "$PROMPT" | grep -qiE "create|write|add"; then
    echo "ℹ️  REMINDER: TodoWrite requires 10+ todos in ONE batch" >&2
  fi

  # Check for agent execution requests
  if echo "$PROMPT" | grep -qiE "(agent|swarm|spawn|coordinate)"; then
    echo "ℹ️  REMINDER: Use Claude Code Task tool for agent execution" >&2
    echo "   → MCP only for coordination topology" >&2
  fi

  # Check for specification requests
  if echo "$PROMPT" | grep -qiE "(spec|ontology|rdf|ttl).*\.(md|markdown)"; then
    echo "⚠️  WARNING: Request involves markdown specs" >&2
    echo "   → RDF (.ttl) is source of truth, .md is generated" >&2
    echo "   → Edit .specify/*.ttl, then: cargo make speckit-render" >&2
  fi

  # Validate file operations
  if echo "$PROMPT" | grep -qiE "(read|edit|write|delete).*file"; then
    if ! echo "$PROMPT" | grep -qE "/home/user/ggen/"; then
      echo "⚠️  WARNING: File operation without absolute path" >&2
      echo "   → Specify full path: /home/user/ggen/..." >&2
    fi
  fi

  # Check for completion claims without validation
  if echo "$PROMPT" | grep -qiE "(done|complete|finished|ready)" && \
     ! echo "$PROMPT" | grep -qiE "(test|check|verify|validate)"; then
    echo "ℹ️  REMINDER: Definition of Done requires validation" >&2
    echo "   → cargo make check → lint → test → slo-check" >&2
  fi

  # Check for unwrap/expect requests
  if echo "$PROMPT" | grep -qiE "unwrap|expect" && echo "$PROMPT" | grep -qiE "production|src/"; then
    echo "⚠️  WARNING: unwrap/expect in production code" >&2
    echo "   → Use Result<T, E> instead" >&2
    echo "   → unwrap() only allowed in tests" >&2
  fi

  # Success - prompt validated
  exit 0
' bash "$PROMPT"

exit $?
