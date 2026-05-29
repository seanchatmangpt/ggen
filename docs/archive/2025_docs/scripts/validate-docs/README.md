# Documentation Validation Scripts

Automated validation scripts that ensure all documentation examples, tutorials, and guides actually work with the current version of ggen.

## Why This Matters

Documentation that doesn't work is worse than no documentation at all. These scripts ensure:

- ✅ Every command in the docs actually works
- ✅ All SPARQL queries return expected results
- ✅ Tutorials can be followed start-to-finish
- ✅ Examples match current CLI behavior
- ✅ Documentation stays in sync with code

## Quick Start

### Run All Validation Tests

```bash
./scripts/validate-docs/validate-all.sh
```

**Output**: Colored terminal output + Markdown report (`validation-report.md`)

### Run Individual Tests

```bash
# Quick Start Tutorial
./scripts/validate-docs/validate-quick-start.sh

# SPARQL Query Guide
./scripts/validate-docs/validate-sparql-guide.sh

# CLI Command Reference
./scripts/validate-docs/validate-cli-reference.sh

# Watch Mode functionality
./scripts/validate-docs/validate-watch-mode.sh
```

### 👀 Live Documentation Validation (Watch Mode)

**Automatically validate docs as you write them**:

```bash
./scripts/validate-docs/watch-docs-live.sh
```

This watches the `docs/` directory and automatically runs validation whenever you save a file. Perfect for documentation development!

**Features**:
- ✅ Auto-runs validation on file changes
- ✅ Debounced (waits for you to finish editing)
- ✅ Colored pass/fail feedback
- ✅ Shows validation report on failures
- ✅ Uses ggen's built-in watch capabilities (with fswatch fallback)

**Usage**:
```bash
# Watch with default settings (500ms debounce)
./scripts/validate-docs/watch-docs-live.sh

# Watch with custom debounce
./scripts/validate-docs/watch-docs-live.sh --debounce 1000

# Get help
./scripts/validate-docs/watch-docs-live.sh --help
```

**Optional**: Install `fswatch` for better performance:
```bash
# macOS
brew install fswatch

# Ubuntu/Debian
apt-get install fswatch
```

## What Gets Tested

### 1. Quick Start Tutorial (`validate-quick-start.sh`)

**Tests**: `docs/getting-started/quick-start.md`

Validates the 10-minute tutorial works end-to-end:
- Installation verification
- Create RDF ontology file
- Load data with `ggen graph load`
- Query with SPARQL (`ggen graph query`)
- Extract schema (`ggen ontology extract`)
- Verify JSON schema output
- Template list command

**Test Count**: ~12 individual tests

### 2. SPARQL Query Guide (`validate-sparql-guide.sh`)

**Tests**: `docs/how-to/generation/query-rdf-sparql.md`

Validates all SPARQL query examples:
- Basic SELECT queries
- Filtering with FILTER and price comparisons
- In-stock filtering
- COUNT and GROUP BY aggregation
- LIMIT clauses
- Common query patterns

**Test Count**: ~20 individual tests

### 3. CLI Command Reference (`validate-cli-reference.sh`)

**Tests**: `docs/reference/commands/complete-cli-reference.md`

Validates all documented CLI commands work:
- Template commands (list, show, lint)
- Graph commands (load, query, export, visualize)
- Ontology commands (extract, validate, generate)
- Project commands (init, gen, watch, etc.)
- Global options (--help, --version)

**Test Count**: ~18 individual tests

## Test Output

### Success Output

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Quick Start Tutorial
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

✓ ggen is installed and executable
✓ Created product-ontology.ttl
✓ Ontology contains Product class definition
✓ Loaded RDF into graph
✓ Loaded 13 triples (expected ~13)
✓ SPARQL query executed successfully

Total Tests Run:    12
Tests Passed:       12
Tests Failed:       0

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✓ Quick Start Tutorial: ALL TESTS PASSED
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

### Failure Output

```
✗ SPARQL query failed: Error message here
✗ Schema JSON not created

Total Tests Run:    10
Tests Passed:       8
Tests Failed:       2

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✗ Quick Start Tutorial: TESTS FAILED
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

## Generated Report

After running `validate-all.sh`, a Markdown report is generated:

**File**: `scripts/validate-docs/validation-report.md`

**Contents**:
- Summary table (suites passed/failed, tests passed/failed)
- Individual test suite results
- Recommendations for fixing failures
- Instructions for re-running tests

**Example**:

```markdown
# ggen Documentation Validation Report

**Generated**: 2025-12-10 14:30:00
**ggen Version**: 3.4.1
**Duration**: 15s

## Summary

| Metric | Count |
|--------|-------|
| **Test Suites Run** | 3 |
| **Suites Passed** | ✅ 3 |
| **Suites Failed** | ❌ 0 |
| **Individual Tests Run** | 50 |
| **Tests Passed** | ✅ 50 |
| **Tests Failed** | ❌ 0 |

**Overall Status**: ✅ **PASS**
```

## Environment Variables

### `GGEN_BIN`

Path to ggen binary (default: `ggen` in PATH)

```bash
# Use custom ggen binary
GGEN_BIN=/path/to/custom/ggen ./scripts/validate-docs/validate-all.sh

# Test against development build
GGEN_BIN=./target/debug/ggen ./scripts/validate-docs/validate-all.sh
```

### `REPORT_FILE`

Custom path for validation report (default: `scripts/validate-docs/validation-report.md`)

```bash
REPORT_FILE=/tmp/validation-report.md ./scripts/validate-docs/validate-all.sh
```

## CI/CD Integration

### GitHub Actions

See `.github/workflows/validate-docs.yml` for automated validation on:
- Every pull request
- Pushes to main branch
- Nightly builds

### Pre-Release Checklist

Before releasing a new version of ggen:

```bash
# 1. Run validation
./scripts/validate-docs/validate-all.sh

# 2. Check report
cat scripts/validate-docs/validation-report.md

# 3. Fix any failures
# ... update code or docs ...

# 4. Re-run until all pass
./scripts/validate-docs/validate-all.sh
```

## Adding New Validation Tests

### 1. Create New Script

```bash
cat > scripts/validate-docs/validate-my-guide.sh << 'EOF'
#!/usr/bin/env bash
set -e

# ... validation logic ...
EOF

chmod +x scripts/validate-docs/validate-my-guide.sh
```

### 2. Add to Master Script

Edit `validate-all.sh`:

```bash
# Add new validation
run_validation "validate-my-guide.sh" "My New Guide"
```

### 3. Test

```bash
./scripts/validate-docs/validate-my-guide.sh
./scripts/validate-docs/validate-all.sh
```

## Troubleshooting

### Script Permission Denied

```bash
chmod +x scripts/validate-docs/*.sh
```

### ggen Not Found

```bash
# Install ggen
cargo install --path crates/ggen-cli

# Or specify path
GGEN_BIN=/path/to/ggen ./scripts/validate-docs/validate-all.sh
```

### Timeout Issues

Scripts use temporary directories that are automatically cleaned up. If a script hangs:

```bash
# Kill the process
pkill -f validate-

# Clean up temp directories
rm -rf /tmp/tmp.*
```

### Test Failures

1. Read the test output carefully - it shows exactly what failed
2. Run the failing command manually to reproduce
3. Check if documentation is outdated or command changed
4. Update documentation or fix command
5. Re-run validation

## Best Practices

### When Writing Documentation

1. **Write validation script FIRST** - TDD for docs!
2. **Test every command** - If it's in the docs, it must work
3. **Use real data** - Not fake/placeholder data
4. **Verify outputs** - Check return codes AND content
5. **Keep tests fast** - Use LIMIT in queries, minimal data

### When Updating ggen

1. **Run validation before committing** - Catch breaking changes early
2. **Update docs with code** - Documentation is code
3. **Check CI results** - Don't merge if validation fails
4. **Version documentation** - Match docs to releases

## Performance

**Typical runtime**:
- `validate-quick-start.sh`: ~3-5 seconds
- `validate-sparql-guide.sh`: ~4-6 seconds
- `validate-cli-reference.sh`: ~2-3 seconds
- `validate-all.sh`: ~10-15 seconds total

**Optimization tips**:
- Scripts run in parallel where possible
- Use `LIMIT` in SPARQL queries
- Use minimal test data
- Clean up temp files automatically

## Exit Codes

All scripts use standard exit codes:
- `0` - All tests passed
- `1` - One or more tests failed

Perfect for CI/CD pipelines:

```bash
if ./scripts/validate-docs/validate-all.sh; then
  echo "Documentation validated!"
else
  echo "Validation failed - check docs"
  exit 1
fi
```

## License

MIT License - same as ggen

## Contributing

1. Add validation for your docs
2. Ensure all tests pass
3. Submit PR with validation script
4. Documentation + validation = complete feature

---

**Remember**: Documentation that's tested is documentation that's trusted.
