# OTEL Verification Guide

How to verify OpenTelemetry spans and traces for ggen MCP tools at runtime.

## Philosophy

**Tests passing is not sufficient.** For tools involving file I/O, parsing, or external services, OTEL spans prove the operation actually occurred. If spans are missing, the tool did not execute its real logic.

## Prerequisites

```bash
# Enable trace logging for the MCP server
export RUST_LOG=trace,ggen_a2a_mcp=trace
```

## Quick Verification

Run all MCP tests and capture OTEL output:

```bash
cargo test -p ggen-a2a-mcp --test validation_e2e -- --nocapture 2>&1 | tee otel_output.txt
```

## Required Spans by Tool

### All Tools (Baseline)
Every MCP tool must emit:

| Attribute | Value | Verified by |
|-----------|-------|-------------|
| `service.name` | `ggen-mcp-server` | `grep 'service.name' otel_output.txt` |
| `mcp.tool.name` | Tool name (e.g., `validate_project`) | `grep 'mcp.tool.name' otel_output.txt` |
| `service.version` | Crate version | `grep 'service.version' otel_output.txt` |

### Generation Tools
`generate`, `sync`

| Attribute | Example | Grep |
|-----------|---------|------|
| `mcp.tool.name` | `generate` | `grep 'mcp.tool.name=generate'` |
| `mcp.project_path` | `/path/to/project` | `grep 'mcp.project_path'` |

### Validation Tools
`validate`, `validate_sparql`, `validate_templates`, `validate_pipeline`

| Attribute | Example | Grep |
|-----------|---------|------|
| `mcp.tool.name` | `validate_ttl_syntax` | `grep 'mcp.tool.name=validate'` |
| `mcp.project_path` | `/path/to/project` | `grep 'mcp.project_path'` |
| `is_valid` | `true`/`false` | `grep 'is_valid'` |

### Orchestration Tools
`validate_project`, `validate_incremental`, `validate_dependency_graph`

| Attribute | Example | Grep |
|-----------|---------|------|
| `validation.level` | `all` | `grep 'validation.level'` |
| `validation.tools_executed_count` | `6` | `grep 'validation.tools_executed_count'` |
| `validation.total_duration_ms` | `29` | `grep 'validation.total_duration_ms'` |
| `validation.files_changed_count` | `3` | `grep 'validation.files_changed_count'` |
| `graph.nodes_count` | `12` | `grep 'graph.nodes_count'` |
| `graph.edges_count` | `15` | `grep 'graph.edges_count'` |
| `graph.cycles_count` | `0` | `grep 'graph.cycles_count'` |

### Error Spans
All tools must set on failure:

| Attribute | Value |
|-----------|-------|
| `error` | `true` |
| `error.type` | Classification (e.g., `cycle_detection_error`) |

## Verification Script

```bash
#!/bin/bash
# verify_otel_spans.sh — Verify OTEL instrumentation for all MCP tools
set -euo pipefail

echo "=== OTEL Span Verification ==="

export RUST_LOG=trace,ggen_a2a_mcp=trace

OUTPUT=$(mktemp)
cargo test -p ggen-a2a-mcp --test validation_e2e -- --nocapture 2>&1 | tee "$OUTPUT"

echo ""
echo "=== Checking Required Attributes ==="

check_attr() {
    local attr="$1"
    if grep -q "$attr" "$OUTPUT"; then
        echo "  [PASS] $attr found"
    else
        echo "  [FAIL] $attr NOT found"
        return 1
    fi
}

FAILED=0

echo "Baseline attributes:"
check_attr "service.name" || ((FAILED++))
check_attr "mcp.tool.name" || ((FAILED++))
check_attr "service.version" || ((FAILED++))

echo ""
echo "Tool-specific attributes:"
check_attr "validation.level" || ((FAILED++))
check_attr "validation.tools_executed_count" || ((FAILED++))
check_attr "validation.total_duration_ms" || ((FAILED++))
check_attr "graph.nodes_count" || ((FAILED++))
check_attr "graph.edges_count" || ((FAILED++))
check_attr "graph.cycles_count" || ((FAILED++))

echo ""
if [ "$FAILED" -eq 0 ]; then
    echo "=== All OTEL checks PASSED ==="
else
    echo "=== $FAILED OTEL checks FAILED ==="
    exit 1
fi

rm "$OUTPUT"
```

## Interpretation Guide

### What OTEL Spans Prove

| Observation | Conclusion |
|-------------|------------|
| `mcp.tool.name=validate_project` present | Tool was actually invoked via MCP protocol |
| `validation.tools_executed_count=6` | All 6 validation layers ran |
| `validation.total_duration_ms > 0` | Real wall time measured (not mocked) |
| `graph.nodes_count > 0` | Dependency graph was actually built from file parsing |
| `graph.cycles_count=0 | No cycles detected (real DFS result) |
| `error=true` | Tool failed and recorded the error span |

### What Missing Spans Mean

| Missing | Implication |
|---------|-------------|
| No `mcp.tool.name` | Tool was never called — test is not actually exercising MCP protocol |
| No `validation.*` attributes | Orchestration tool executed but inner logic was skipped |
| No `graph.*` attributes | Dependency parsing was not performed |
| No error span on failure | Error path not instrumented — failures are silently swallowed |

## Common Mistakes

### "Tests pass, so it's working"
Tests may mock the MCP transport. OTEL spans prove the tool executed through the real protocol.

### "I saw a log line, so OTEL works"
`info!()` log lines are not OTEL spans. Spans have structured attributes via `tracing::Span::current().record()`.

### "The tool returns successfully"
A successful return doesn't mean the OTEL attributes were recorded. Check for both.

## Integration with CI

Add to your CI pipeline after tests pass:

```yaml
# .github/workflows/ci.yml (excerpt)
- name: OTEL Verification
  env:
    RUST_LOG: trace,ggen_a2a_mcp=trace
  run: |
    cargo test -p ggen-a2a-mcp --test validation_e2e -- --nocapture 2>&1 | tee otel_output.txt
    grep -q 'service.name' otel_output.txt
    grep -q 'mcp.tool.name' otel_output.txt
    grep -q 'validation.tools_executed_count' otel_output.txt
    grep -q 'graph.nodes_count' otel_output.txt
```

## Checklist

Before claiming any MCP tool feature is complete:

- [ ] `RUST_LOG=trace` shows tool invocation spans
- [ ] `mcp.tool.name` appears for every tool
- [ ] `service.name` and `service.version` present
- [ ] Tool-specific attributes populated (see tables above)
- [ ] Error spans set `error=true` on failure paths
- [ ] Duration attributes are reasonable (not 0ms, not hours)
- [ ] Count attributes are reasonable (not 0 for populated graphs)
