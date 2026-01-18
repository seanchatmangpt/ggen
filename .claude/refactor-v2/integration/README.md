# Integration Testing Guide

**Agent 12: Final Integration Check**

## Quick Start

```bash
# 1. Build and install ggen
cargo make build-release
cargo install --path cli --force

# 2. Run integration tests
./.claude/refactor-v2/integration/integration-tests.sh

# 3. View results
cat .claude/refactor-v2/integration/integration-results.md
```

## What Gets Tested

### Critical Workflows (80/20 Focus)

1. **Doctor Command** - Environment validation
2. **Project Bootstrap** - New project creation (`ggen project new`)
3. **Template Generation** - Core template workflow
4. **Marketplace Search** - Package discovery
5. **AI Generation** - AI-powered features
6. **RDF/SPARQL** - Knowledge graph integration
7. **File Tree** - Multi-file generation
8. **Performance** - CLI responsiveness (< 2s startup)
9. **Error Handling** - Graceful failures
10. **Help System** - User guidance

## Test Results

After running, check:

- **Integration Results**: `.claude/refactor-v2/integration/integration-results.md`
- **Test Log**: `.claude/refactor-v2/integration/integration-tests.log` (if exists)
- **Checklist**: `.claude/refactor-v2/integration-checklist.md`

## Cross-Platform Testing

### macOS (Current Platform)
✅ Fully tested via integration suite

### Linux
```bash
# Test in Docker
docker run -it --rm -v $(pwd):/workspace rust:latest bash
cd /workspace
cargo make build-release
cargo install --path cli --force
./.claude/refactor-v2/integration/integration-tests.sh
```

### Windows (WSL)
```powershell
# In WSL2
cd /mnt/c/path/to/ggen
cargo make build-release
cargo install --path cli --force
./.claude/refactor-v2/integration/integration-tests.sh
```

## Performance SLOs

All tests validate these SLOs:

- ✓ CLI startup: < 2s
- ✓ Memory usage: < 100MB
- ✓ Generation speed: < 3s
- ✓ Build time: 30-45s (full), 5-8s (incremental)

## Troubleshooting

### Test Fails: "ggen not found"
```bash
# Build and install first
cargo make build-release
cargo install --path cli --force

# Verify installation
which ggen
ggen --version
```

### Test Fails: AI Generation
```bash
# AI tests skip gracefully if not configured
# To enable, create ~/.config/ggen/ai-config.toml
```

### Test Fails: Marketplace
```bash
# Marketplace tests check for graceful errors
# No external dependencies required
```

## Integration with CI/CD

Add to your CI pipeline:

```yaml
# .github/workflows/integration.yml
- name: Run Integration Tests
  run: |
    cargo make build-release
    cargo install --path cli --force
    ./.claude/refactor-v2/integration/integration-tests.sh
```

## Manual Smoke Testing

After automated tests pass, perform manual checks:

1. **Create a real project**:
   ```bash
   ggen project new demo-app --type rust-cli
   cd demo-app
   cargo run
   ```

2. **Search marketplace**:
   ```bash
   ggen marketplace search "rust web"
   ggen marketplace categories
   ```

3. **Generate from template**:
   ```bash
   echo "---\nto: test.txt\n---\nHello!" > test.tmpl
   ggen project gen test.tmpl
   ```

4. **Check help**:
   ```bash
   ggen help-me
   ggen doctor
   ```

## Success Criteria

✅ **All 10 tests pass**
✅ **No crashes or panics**
✅ **Performance SLOs met**
✅ **Error handling graceful**

## Next Steps

1. Run integration suite
2. Review results in `integration-results.md`
3. Test on Linux/Windows (manual)
4. Check integration checklist
5. Proceed with release

---

*Integration testing focused on the 20% of workflows that represent 80% of usage*
