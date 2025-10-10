<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Chapter: Knowledge Hooks - Reactive Graph Maintenance](#chapter-knowledge-hooks---reactive-graph-maintenance)
  - [From Manual to Autonomic](#from-manual-to-autonomic)
  - [Core Pattern: Pattern 021 (Knowledge Hooks)](#core-pattern-pattern-021-knowledge-hooks)
    - [Hook Types](#hook-types)
    - [Anatomy of a Pre-Commit Hook](#anatomy-of-a-pre-commit-hook)
    - [Example: CI/CD Hook (GitHub Actions)](#example-cicd-hook-github-actions)
  - [Development Workflow Integration](#development-workflow-integration)
    - [File System Watcher (Dev Mode)](#file-system-watcher-dev-mode)
    - [Hook Installation](#hook-installation)
  - [Configuration](#configuration)
  - [Advanced Patterns](#advanced-patterns)
    - [Selective Regeneration](#selective-regeneration)
    - [Rollback on Failure](#rollback-on-failure)
    - [Performance Monitoring](#performance-monitoring)
  - [Anti-Patterns](#anti-patterns)
    - [❌ Full Rebuild on Every Commit](#-full-rebuild-on-every-commit)
    - [❌ Ignoring Hook Failures](#-ignoring-hook-failures)
    - [❌ No Validation](#-no-validation)
  - [Testing Hooks](#testing-hooks)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Chapter: Knowledge Hooks - Reactive Graph Maintenance

## From Manual to Autonomic

Traditional knowledge graph generation requires explicit invocation:

```bash
# Manual workflow (the old way)
$ vim src/feature.rs        # Edit code
$ cargo test                # Run tests
$ ggen graph build          # ⚠️  Easy to forget!
$ git commit -m "..."       # Might commit stale graph
```

This manual approach breaks down at scale. Developers forget to regenerate graphs, leading to:

- **Temporal drift** between code and knowledge representation
- **Cognitive overhead** remembering when to regenerate
- **Integration friction** with CI/CD pipelines
- **Staleness bugs** in tools depending on graphs

**Knowledge hooks** solve this by making graph updates automatic and event-driven:

```bash
# Autonomic workflow (with hooks)
$ vim src/feature.rs        # Edit code
$ git commit -m "..."       # Hook auto-regenerates graph
✅ Graph automatically updated, staged, and committed
```

## Core Pattern: Pattern 021 (Knowledge Hooks)

The fundamental insight: **Treat graph regeneration as a side effect of code changes.**

### Hook Types

**1. Git Hooks** (Local automation)

- `pre-commit`: Regenerate before committing
- `post-merge`: Rebuild after merging branches
- `post-checkout`: Update when switching branches
- `post-rewrite`: Handle rebases and amendments

**2. File System Watchers** (Development mode)

- Monitor source directories for changes
- Trigger incremental updates on save
- Enable real-time graph-backed tools (LSP, live queries)

**3. CI/CD Hooks** (Remote automation)

- GitHub Actions, GitLab CI, Jenkins
- Validate graph consistency on PRs
- Deploy updated graphs to documentation sites
- Trigger downstream workflows on graph changes

### Anatomy of a Pre-Commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

# 1. Detect changed files
STAGED_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep '\.rs$')

if [ -z "$STAGED_FILES" ]; then
  exit 0  # No code changes, nothing to do
fi

# 2. Filter substantive changes (skip whitespace/comments)
SUBSTANTIVE=false
for file in $STAGED_FILES; do
  CHANGED_LINES=$(git diff --cached "$file" | grep '^+' | grep -v '^\+\s*//' | wc -l)
  if [ "$CHANGED_LINES" -gt 0 ]; then
    SUBSTANTIVE=true
    break
  fi
done

if [ "$SUBSTANTIVE" = false ]; then
  exit 0  # Only comments/formatting changed
fi

# 3. Regenerate graph incrementally
echo "🔄 Regenerating knowledge graphs..."
ggen graph --incremental --changed $STAGED_FILES

# 4. Validate output
if ! ggen validate graphs/main.ttl; then
  echo "❌ Generated graph is invalid, blocking commit"
  exit 1
fi

# 5. Stage updated graphs
git add graphs/*.ttl

echo "✅ Graphs updated and staged"
exit 0
```

**Key design principles:**

- **Fast**: Incremental updates only (see Pattern 022)
- **Safe**: Validate before committing
- **Transparent**: Clear feedback to developer
- **Non-blocking**: Skip if no substantive changes

### Example: CI/CD Hook (GitHub Actions)

```yaml
# .github/workflows/knowledge-graph.yml
name: Knowledge Graph Maintenance

on:
  push:
    branches: [main, develop]
  pull_request:

jobs:
  update-graph:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          fetch-depth: 0  # Need history for diffs

      - name: Install ggen
        run: cargo install ggen

      - name: Detect changed files
        id: changes
        run: |
          CHANGED=$(git diff --name-only ${{ github.event.before }} ${{ github.sha }} | grep '\.rs$' || true)
          echo "files=$CHANGED" >> $GITHUB_OUTPUT

      - name: Regenerate graphs
        if: steps.changes.outputs.files != ''
        run: |
          echo "${{ steps.changes.outputs.files }}" | xargs -I {} ggen parse {} --output graphs/
          ggen graph build --incremental

      - name: Validate consistency
        run: |
          ggen graph --full --output /tmp/test-graph.ttl
          diff -q graphs/main.ttl /tmp/test-graph.ttl || {
            echo "❌ Graph inconsistent with source, failing build"
            exit 1
          }

      - name: Commit updated graphs
        if: github.event_name == 'push'
        uses: stefanzweifel/git-auto-commit-action@v4
        with:
          commit_message: "chore: auto-update knowledge graphs [skip ci]"
          file_pattern: "graphs/*.ttl"
```

This ensures:
- **PRs** validate graph consistency
- **Merges to main** auto-update and commit graphs
- **Skip loops** via `[skip ci]` marker

## Development Workflow Integration

### File System Watcher (Dev Mode)

For interactive development, use a file watcher:

```bash
$ ggen watch src/
🔍 Watching src/ for changes...
  • Auto-regeneration: ON
  • Incremental mode: ON
  • Live reload: ON

[11:23:45] Change detected: src/parser.rs
[11:23:45] 🔄 Regenerating graphs...
[11:23:46] ✅ Graph updated (120ms)
```

This enables **real-time graph-backed tools**:

- LSP servers query live graphs for semantic info
- Documentation sites auto-refresh on changes
- Analysis dashboards show current state

### Hook Installation

Provide a simple installation command:

```bash
$ ggen hooks install
✅ Installed 4 Git hooks:
  • pre-commit: Incremental graph regeneration
  • post-merge: Full graph rebuild
  • post-checkout: Branch sync
  • post-rewrite: Rebase-safe updates

Run 'ggen hooks status' to verify
Run 'ggen hooks uninstall' to remove
```

Under the hood:

```rust
// ggen/src/hooks/install.rs
pub fn install_hooks() -> Result<()> {
    let hooks = [
        ("pre-commit", include_str!("../hooks/pre-commit.sh")),
        ("post-merge", include_str!("../hooks/post-merge.sh")),
        ("post-checkout", include_str!("../hooks/post-checkout.sh")),
        ("post-rewrite", include_str!("../hooks/post-rewrite.sh")),
    ];

    for (name, content) in &hooks {
        let path = Path::new(".git/hooks").join(name);
        fs::write(&path, content)?;
        make_executable(&path)?;  // chmod +x
    }

    Ok(())
}
```

## Configuration

Allow customization via `.ggen/hooks.toml`:

```toml
[hooks]
enabled = true
incremental = true
auto_commit = true
skip_on_trivial = true  # Skip if only comments/whitespace changed

[hooks.pre_commit]
enabled = true
backup = true  # Backup before regeneration
timeout_secs = 30
validate = true

[hooks.post_merge]
enabled = true
full_rebuild = true  # Always full rebuild after merge

[hooks.watch]
enabled = true
debounce_ms = 200  # Wait 200ms after last change
ignore_patterns = ["*.test.rs", "benches/*"]
```

## Advanced Patterns

### Selective Regeneration

Only regenerate graphs for changed modules:

```bash
# Hook detects changed modules
CHANGED_MODULES=$(echo "$STAGED_FILES" | xargs dirname | sort -u)

for module in $CHANGED_MODULES; do
  ggen parse "$module" --output "graphs/modules/$(basename $module).ttl"
done

# Merge module graphs into main graph
ggen graph merge graphs/modules/*.ttl --output graphs/main.ttl
```

### Rollback on Failure

Protect against invalid graphs:

```bash
# Backup before regeneration
BACKUP_DIR=".git/graph-backups/$(date +%s)"
mkdir -p "$BACKUP_DIR"
cp -r graphs/*.ttl "$BACKUP_DIR/"

# Regenerate
if ggen graph build --incremental; then
  git add graphs/*.ttl
  rm -rf "$BACKUP_DIR"  # Success, cleanup
  exit 0
else
  # Failure: rollback to backup
  cp "$BACKUP_DIR"/*.ttl graphs/
  rm -rf "$BACKUP_DIR"
  echo "❌ Regeneration failed, rolled back"
  exit 1
fi
```

### Performance Monitoring

Track hook performance:

```bash
START=$(date +%s%N)

ggen graph --incremental --changed $STAGED_FILES

END=$(date +%s%N)
DURATION=$(( (END - START) / 1000000 ))  # Convert to milliseconds

echo "⏱  Graph update took ${DURATION}ms"

# Warn if too slow (>1s)
if [ "$DURATION" -gt 1000 ]; then
  echo "⚠️  Hook took longer than 1s, consider optimizing"
fi
```

## Anti-Patterns

### ❌ Full Rebuild on Every Commit

```bash
# DON'T: Slow, blocks commits
ggen graph --full  # Takes 10+ seconds on large codebases
```

**Instead**: Use incremental updates (Pattern 022)

### ❌ Ignoring Hook Failures

```bash
# DON'T: Hides graph errors
ggen graph build || true  # Swallows errors!
```

**Instead**: Fail loudly, block commit on invalid graphs

### ❌ No Validation

```bash
# DON'T: Commits invalid graphs
ggen graph build
git add graphs/*.ttl  # No validation!
```

**Instead**: Always validate before staging

## Testing Hooks

Provide a test mode:

```bash
$ ggen hooks test
🧪 Testing installed hooks...

✅ pre-commit hook:
   • Detected 2 changed files
   • Regenerated in 180ms
   • Graph valid

✅ post-merge hook:
   • Full rebuild completed
   • 5000 triples
   • No errors

✅ All hooks working correctly
```

## Summary

Knowledge hooks transform ggen from a **manual tool** into an **autonomic system**:

| Aspect | Manual | Autonomic (Hooks) |
|--------|--------|-------------------|
| Trigger | Explicit `ggen graph` | Automatic on code change |
| Staleness | Common | Impossible |
| Overhead | High (remember to run) | Zero (invisible) |
| Scale | Doesn't scale | Scales with incremental updates |
| CI/CD | Separate step | Integrated automatically |

By treating graph regeneration as a **reactive side effect** of code changes, hooks eliminate an entire class of maintenance burden and enable always-current knowledge graphs.

**Next**: Pattern 022 (Delta-Driven Regeneration) — the performance foundation that makes hooks viable at scale.

---

**Chapter Status**: ✅ Complete
**Related Patterns**: 021, 022, 024
**Prerequisites**: Git, basic hook understanding
**Complexity**: Medium
