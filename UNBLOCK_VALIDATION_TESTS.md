# Unblock Validation Tests - Quick Reference

## Problem

Two blockers prevent validation test execution:

1. **Cargo.toml duplicate keys** (lines 349, 455, 477, etc.)
2. **Test API signature mismatch** (validation_e2e.rs uses outdated rmcp API)

## Solution

### Step 1: Fix Cargo.toml Duplicates

```bash
# Backup original
cp /Users/sac/ggen/Cargo.toml /Users/sac/ggen/Cargo.toml.backup

# Extract and deduplicate clippy lints
awk '/^\[workspace.lints.clippy\]/,0' /Users/sac/ggen/Cargo.toml | \
  sort -u > /tmp/clippy_lints.txt

# Find line numbers of workspace.lints.clippy section
grep -n "^\[workspace.lints.clippy\]" /Users/sac/ggen/Cargo.toml
# Output: 302:[workspace.lints.clippy]

# Find line numbers of next section
grep -n "^\[features\]" /Users/sac/ggen/Cargo.toml
# Output: 406:[features]

# Delete old clippy section (lines 302-405)
sed -i.bak '302,405d' /Users/sac/ggen/Cargo.toml

# Insert deduplicated section after line 301
sed -i.tmp "301r /tmp/clippy_lints.txt" /Users/sac/ggen/Cargo.toml

# Verify no duplicates
grep -o "^[a-z_]* = " /Users/sac/ggen/Cargo.toml | sort | uniq -d
# Should return empty if successful

# Verify compilation
cargo check -p ggen-a2a-mcp
```

### Step 2: Fix Test API Signatures

```bash
# Backup test file
cp /Users/sac/ggen/crates/ggen-a2a-mcp/tests/validation_e2e.rs \
   /Users/sac/ggen/crates/ggen-a2a-mcp/tests/validation_e2e.rs.backup

# Use Perl to fix all call_tool invocations
perl -i -pe '
  s/call_tool\(
      \s*"([^"]+)",
      \s*None,
      \s*None,
      \s*Some\(([^)]+)\),
      \s*None
    \)/call_tool(CallToolRequestParams::new("$1".to_string()).with_arguments($2))/gx
' /Users/sac/ggen/crates/ggen-a2a-mcp/tests/validation_e2e.rs

# Verify the changes
grep -A 2 "call_tool" /Users/sac/ggen/crates/ggen-a2a-mcp/tests/validation_e2e.rs | head -20
```

Expected output after fix:
```rust
client.call_tool(
    CallToolRequestParams::new("validate_manifest_parse".to_string())
        .with_arguments(serde_json::json!({ "manifest": valid_toml }))
)
```

### Step 3: Verify Compilation

```bash
# Should compile without errors
cargo check -p ggen-a2a-mcp

# Should show compilation success
# Finished `dev` profile [unoptimized + debuginfo] target(s) in X.XXs
```

### Step 4: Run Tests

```bash
# Run all 18 validation tests
cargo test -p ggen-a2a-mcp --test validation_e2e -- --nocapture

# Expected output:
# running 18 tests
# test test_validate_manifest_parse_success ... ok
# test test_validate_manifest_parse_missing_required_field ... ok
# ...
# test result: ok. 18 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

### Step 5: Verify OTEL Spans

```bash
# Enable trace logging
export RUST_LOG=trace,ggen_a2a_mcp=trace

# Run tests with output capture
cargo test -p ggen-a2a-mcp --test validation_e2e -- --nocapture 2>&1 | tee /tmp/otel_test_output.txt

# Verify OTEL spans exist
grep -E "mcp\.tool\.(call|response)" /tmp/otel_test_output.txt

# Expected output:
# INFO mcp.tool.call mcp.tool.name=validate_manifest_parse
# INFO mcp.tool.response mcp.tool.duration_ms=45 mcp.tool.result=pass
# INFO mcp.tool.call mcp.tool.name=validate_ttl_syntax
# INFO mcp.tool.response mcp.tool.duration_ms=123 mcp.tool.result=pass
# ...
```

## Troubleshooting

### If Cargo.toml still has duplicates:

```bash
# Find all duplicates
grep -o "^[a-z_]* = " /Users/sac/ggen/Cargo.toml | sort | uniq -c | awk '$1 > 1'

# Manual deduplication (if automated fails)
# Edit Cargo.toml, search for each duplicate, keep first occurrence
```

### If tests still fail with API errors:

```bash
# Check rmcp version
cargo tree -p ggen-a2a-mcp | grep rmcp

# Should show: rmcp v1.3.0
# If different, update Cargo.toml:
# rmcp = { version = "1.3.0", features = ["server", "client", "macros", "transport-io"] }

# Check example working test
grep -A 5 "call_tool" /Users/sac/ggen/crates/ggen-a2a-mcp/tests/multi_mcp_otel_self_play.rs

# Match the pattern in validation_e2e.rs
```

### If OTEL spans are missing:

```bash
# Verify tracing is enabled
RUST_LOG=trace cargo test -p ggen-a2a-mcp --test validation_e2e 2>&1 | grep "TRACE"

# Check if tracing instrumentation is present
grep -r "span!" /Users/sac/ggen/crates/ggen-a2a-mcp/src/ggen_server.rs

# Should find multiple span! macro calls
```

## Success Criteria

- [ ] `cargo check -p ggen-a2a-mcp` succeeds
- [ ] `cargo test -p ggen-a2a-mcp --test validation_e2e` runs 18 tests
- [ ] All 18 tests pass
- [ ] OTEL spans visible in test output
- [ ] No duplicate keys in Cargo.toml

## Time Estimate

- Step 1 (Cargo.toml): 30-60 minutes
- Step 2 (Test API): 30-60 minutes
- Step 3 (Compilation): 5 minutes
- Step 4 (Tests): 10 minutes
- Step 5 (OTEL): 10 minutes

**Total:** 1.5-3 hours

## References

- Full report: `/Users/sac/ggen/VALIDATION_MCP_TOOLS_COMPLETION_REPORT.md`
- Test file: `/Users/sac/ggen/crates/ggen-a2a-mcp/tests/validation_e2e.rs`
- MCP server: `/Users/sac/ggen/crates/ggen-a2a-mcp/src/ggen_server.rs`
- OTEL rules: `/Users/sac/ggen/.claude/rules/otel-validation.md`
