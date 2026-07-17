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
ggen graph validate --files .specify/specs/NNN-feature/feature.ttl  # bare `ggen validate` no longer exists
ggen sync run --dry-run   # Preview generation (`ggen sync --dry_run true` no longer works)
```

## 2. Chicago TDD
```bash
# ggen-engine is the live crate; ggen-core is disconnected and should not
# receive new tests/features (see .claude/rules/architecture.md)
vim crates/ggen-engine/tests/feature_test.rs  # Write failing test (RED)
just test                                     # Verify fails (test-lib only runs --lib, not tests/)
vim crates/ggen-engine/src/feature.rs         # Implement (GREEN)
just test                                     # Verify passes
just pre-commit                               # Refactor (maintain GREEN)
```

## 3. Generate from Ontology
```bash
ggen sync run --dry-run   # Preview (`just sync-dry` currently runs a broken command internally)
ggen sync run             # Full sync with cryptographic receipt (`just sync` also currently broken)
```

## 4. Commit with Evidence
```bash
just pre-commit
git commit -m "feat(NNN): Implement feature

[Receipt] just pre-commit: ✓ 3/3 gates
[Receipt] just test: ✓ 347/347 tests"
```
