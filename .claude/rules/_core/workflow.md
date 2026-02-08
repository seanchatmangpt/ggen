---
auto_load: true
priority: high
version: 6.0.0
---

# 🔧 Development Workflow (4 Steps)

## 1. Create RDF Spec
```bash
mkdir -p .specify/specs/NNN-feature
vim .specify/specs/NNN-feature/feature.ttl  # Edit TTL (source)
ggen validate .specify/specs/NNN-feature/feature.ttl
cargo make speckit-render  # Generate markdown
```

## 2. Chicago TDD
```bash
vim crates/*/tests/feature_test.rs  # Write failing test (RED)
cargo make test-unit                # Verify fails
vim crates/*/src/feature.rs         # Implement (GREEN)
cargo make test-unit                # Verify passes
cargo make pre-commit               # Refactor (maintain GREEN)
```

## 3. Generate from Ontology
```bash
ggen sync --dry_run true   # Preview
ggen sync --audit true     # Full sync with audit
```

## 4. Commit with Evidence
```bash
cargo make pre-commit
git commit -m "feat(NNN): Implement feature

[Receipt] cargo make pre-commit: ✓ 3/3 gates
[Receipt] cargo make test: ✓ 347/347 tests"
```
