# Golden Files: E2E Testing Reference Outputs

This directory contains golden (expected) output files for E2E tests. Golden files serve as the source of truth for comparing generated output across platforms.

## Directory Structure

```
golden/
├── minimal/           # Minimal project fixture
│   └── output/
├── thesis-gen/        # PhD thesis generation example
│   └── output/
└── README.md          # This file
```

## Maintaining Golden Files

### When to Update

Update golden files when **intentional changes** are made to ggen output (e.g., output format changes, template updates):

```bash
# Update a single fixture
UPDATE_GOLDEN=1 cargo test --package ggen-e2e -- thesis_gen

# Update all golden files
UPDATE_GOLDEN=1 cargo make test-e2e
```

### Verifying Changes

Always review golden file changes before committing:

```bash
# See what changed
git diff tests/e2e/golden/

# Verify change is intentional
git show --stat [previous_commit]
```

### Line Endings

All golden files use **LF line endings** (enforced by `.gitattributes`). This ensures cross-platform determinism:
- macOS: LF (native)
- Linux: LF (native)
- Windows: LF (converted by git)

### Checksums

Each golden file has a SHA256 checksum stored in the test contracts (see `specs/011-e2e-testcontainers/contracts/test-fixtures.yaml`).

**Important**: Checksums are informational only during development. They verify file integrity but don't block tests.

## Platform-Specific Differences

E2E tests ensure **byte-for-byte identical output** across all platforms:
- `linux-x86_64` (container)
- `darwin-x86_64` (native)
- `darwin-arm64` (native)

If you see platform differences in test failures:
1. **Line endings**: Check `.gitattributes` enforcement
2. **Timestamps**: Ensure templates don't embed timestamps
3. **Paths**: Verify all paths use `/` separator (not `\`)

## Adding New Fixtures

1. **Create fixture directory**:
   ```bash
   mkdir -p tests/e2e/fixtures/my-fixture/{ontology,templates}
   ```

2. **Add ggen project files** (ggen.toml, ontologies, templates)

3. **Generate golden files**:
   ```bash
   cd tests/e2e/fixtures/my-fixture
   ggen sync
   cp -r output ../../golden/my-fixture/
   ```

4. **Add to test suite**:
   ```rust
   // tests/e2e/e2e_examples.rs
   #[test]
   fn test_my_fixture() {
       run_fixture("my-fixture").unwrap();
   }
   ```

5. **Update contracts**:
   ```yaml
   # specs/011-e2e-testcontainers/contracts/test-fixtures.yaml
   fixtures:
     - name: my-fixture
       ...
   ```

## Testing Without Docker

Golden files allow testing without Docker installed:

```bash
# Runs platform-native tests only
cargo make test-e2e-macos

# Skips Linux container tests if Docker unavailable
cargo make test-e2e
```

## Troubleshooting

### "Golden file not found"

The test fixture was added to `tests/e2e/fixtures/` but golden files don't exist in `tests/e2e/golden/`:

```bash
UPDATE_GOLDEN=1 cargo test --package ggen-e2e -- fixture_name
```

### "Golden file mismatch"

Output differs from expected:

```bash
# Review exact diff
diff -u tests/e2e/golden/thesis-gen/output/thesis.tex \
        target/e2e/thesis-gen/output/thesis.tex

# If intentional, update
UPDATE_GOLDEN=1 cargo test --package ggen-e2e -- thesis_gen
```

### Cross-platform differences

Same fixture produces different output on different platforms - likely issue:
- Timestamps in output (templates shouldn't embed these)
- Path separators (use `/` everywhere)
- Line ending handling (.gitattributes)

See [Troubleshooting Guide](../../../specs/011-e2e-testcontainers/quickstart.md#troubleshooting) for more.

## References

- [Quickstart Guide](../../../specs/011-e2e-testcontainers/quickstart.md)
- [Data Model](../../../specs/011-e2e-testcontainers/data-model.md)
- [Test Fixtures Schema](../../../specs/011-e2e-testcontainers/contracts/test-fixtures.yaml)
