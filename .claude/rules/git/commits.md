# 📝 Git Commit Standards

## Format

```
<type>(<scope>): <description>

[Receipt] cargo make pre-commit: ✓ N/N gates
[Receipt] cargo make test: ✓ NNN/NNN tests
```

**Types:** `feat` | `fix` | `refactor` | `test` | `docs` | `chore` | `perf`
**Scope:** crate name or feature area (e.g., `ggen-core`, `rdf-pipeline`, `cli`)

## Evidence Receipts (Required)

Every commit MUST include receipts from quality gates:

```bash
cargo make pre-commit  # Generates: check + lint + test-unit receipt
cargo make test        # Generates: full test suite receipt
```

Include output counts in commit body. NEVER fabricate receipts.

## Branch Naming

| Type | Pattern | Example |
|------|---------|---------|
| Feature | `feature/NNN-short-desc` | `feature/042-rdf-validator` |
| Fix | `fix/NNN-short-desc` | `fix/013-pipeline-timeout` |
| Claude | `claude/task-name-ID` | `claude/upgrade-claude-docs-tAv85` |

## Rules

- NEVER use `--no-verify` (skip pre-commit hooks)
- NEVER use `--no-gpg-sign` unless explicitly requested
- NEVER amend published commits (create new commit instead)
- NEVER force-push to `main`/`master`
- ALWAYS run `cargo make pre-commit` before committing
- ALWAYS include receipt block in commit body

## Push Protocol

```bash
git push -u origin <branch-name>
# On network failure: retry up to 4× with exponential backoff (2s, 4s, 8s, 16s)
```
