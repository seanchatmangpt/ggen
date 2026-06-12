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
ggen sync --dry_run true   # Preview markdown generation
```

## 2. Chicago TDD
```bash
vim crates/*/tests/feature_test.rs  # Write failing test (RED)
just test-unit                      # Verify fails
vim crates/*/src/feature.rs         # Implement (GREEN)
just test-unit                      # Verify passes
just pre-commit                     # Refactor (maintain GREEN)
```

## 3. Generate from Ontology
```bash
just sync-dry   # Preview
just sync       # Full sync with audit
```

## 4. Commit with Evidence
```bash
just pre-commit
git commit -m "feat(NNN): Implement feature

[Receipt] just pre-commit: ✓ 3/3 gates
[Receipt] just test: ✓ 347/347 tests"
```
