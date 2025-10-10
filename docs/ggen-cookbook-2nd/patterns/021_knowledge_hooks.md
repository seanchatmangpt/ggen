# Pattern 021: Knowledge Hooks

## Intent

Trigger automatic graph regeneration in response to system events, transforming ggen from a manual tool into a reactive autonomic system that maintains knowledge graphs continuously.

## Also Known As

- Event-Driven Generation
- Reactive Knowledge Maintenance
- Hook-Based Automation

## Motivation

Manual graph regeneration creates staleness and cognitive overhead. Developers must remember to run ggen after changes, leading to:

- **Temporal Drift**: Graphs lag behind code reality
- **Cognitive Burden**: "Did I regenerate after that change?"
- **Integration Friction**: Graph updates require explicit workflow steps
- **Scale Barriers**: Large codebases make manual updates impractical

Knowledge hooks eliminate this friction by making graph updates automatic and invisible.

## Applicability

Use Knowledge Hooks when:

- Code changes frequently (active development)
- Multiple contributors modify the codebase
- Graph staleness causes confusion or errors
- CI/CD pipelines need current graph data
- Documentation must stay synchronized with code
- Analysis tools depend on up-to-date graphs

## Structure

```
┌──────────────────┐
│  System Event    │
│  (Git, FS, CI)   │
└────────┬─────────┘
         │ triggers
         ▼
┌──────────────────┐
│   Hook Script    │
│  - Validation    │
│  - Filtering     │
│  - Invocation    │
└────────┬─────────┘
         │ executes
         ▼
┌──────────────────┐
│  GGen Command    │
│  - Parse changed │
│  - Regenerate    │
│  - Commit result │
└────────┬─────────┘
         │ produces
         ▼
┌──────────────────┐
│  Updated Graph   │
│  (auto-committed)│
└──────────────────┘
```

### Key Components

1. **Hook Trigger**: Git hook, file watcher, or CI event
2. **Change Detection**: Identifies what changed (files, scope)
3. **Selective Regeneration**: Updates only affected graphs
4. **Commit Integration**: Automatically commits graph updates
5. **Error Handling**: Graceful failures, rollback support

## Implementation

### Git Pre-Commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit
# Regenerate graphs for staged files before commit

STAGED_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep '\.rs$')

if [ -z "$STAGED_FILES" ]; then
  exit 0  # No Rust files staged
fi

echo "🔄 Regenerating knowledge graphs for changed files..."

# Parse changed files
for file in $STAGED_FILES; do
  ggen parse "$file" --output graphs/
done

# Regenerate relationship graphs
ggen graph build --incremental --changed "$STAGED_FILES"

# Stage updated graphs
git add graphs/*.ttl

echo "✅ Graphs updated and staged"
exit 0
```

### Git Post-Merge Hook

```bash
#!/bin/bash
# .git/hooks/post-merge
# Rebuild full graph after merge to catch cross-file changes

echo "🔄 Post-merge: Rebuilding complete knowledge graph..."

# Full rebuild ensures consistency after merge
ggen graph build --full --output graphs/merged.ttl

# Commit the merged graph
git add graphs/merged.ttl
git commit -m "chore: rebuild knowledge graph post-merge [skip ci]"

echo "✅ Knowledge graph synchronized"
```

### CI/CD Pipeline Hook (GitHub Actions)

```yaml
# .github/workflows/knowledge-graph.yml
name: Knowledge Graph Automation

on:
  push:
    branches: [main, develop]
  pull_request:
    types: [opened, synchronize]

jobs:
  update-graph:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          fetch-depth: 0  # Full history for diff

      - name: Install ggen
        run: cargo install ggen

      - name: Detect changed files
        id: changes
        run: |
          CHANGED=$(git diff --name-only HEAD~1 HEAD | grep '\.rs$' || true)
          echo "files=$CHANGED" >> $GITHUB_OUTPUT

      - name: Regenerate graphs
        if: steps.changes.outputs.files != ''
        run: |
          echo "${{ steps.changes.outputs.files }}" | \
            xargs -I {} ggen parse {} --output graphs/
          ggen graph build --incremental

      - name: Commit updated graphs
        uses: stefanzweifel/git-auto-commit-action@v4
        with:
          commit_message: "chore: auto-update knowledge graphs [skip ci]"
          file_pattern: "graphs/*.ttl"
```

### File System Watcher (Development Mode)

```rust
// examples/watch_mode.rs
use notify::{Watcher, RecursiveMode, Result};
use std::sync::mpsc::channel;
use std::time::Duration;

fn main() -> Result<()> {
    let (tx, rx) = channel();
    let mut watcher = notify::watcher(tx, Duration::from_secs(2))?;

    // Watch source directories
    watcher.watch("src/", RecursiveMode::Recursive)?;

    println!("🔍 Watching for changes...");

    loop {
        match rx.recv() {
            Ok(event) => {
                if let Some(path) = extract_rust_file(&event) {
                    println!("🔄 Detected change: {:?}", path);
                    regenerate_graph_for_file(path)?;
                }
            }
            Err(e) => eprintln!("Watch error: {}", e),
        }
    }
}

fn regenerate_graph_for_file(path: &Path) -> Result<()> {
    // Incremental graph update
    Command::new("ggen")
        .args(&["parse", path.to_str().unwrap(), "--output", "graphs/"])
        .status()?;

    Command::new("ggen")
        .args(&["graph", "build", "--incremental"])
        .status()?;

    println!("✅ Graph updated for {:?}", path);
    Ok(())
}
```

## Sample Code

### Smart Hook with Change Analysis

```bash
#!/bin/bash
# .git/hooks/pre-commit-intelligent
# Only regenerate if substantive code changes detected

STAGED_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep '\.rs$')

if [ -z "$STAGED_FILES" ]; then
  exit 0
fi

# Check if changes are substantive (not just comments/whitespace)
SUBSTANTIVE_CHANGES=false
for file in $STAGED_FILES; do
  # Count non-comment, non-whitespace changed lines
  CHANGED_LINES=$(git diff --cached "$file" | \
    grep '^+' | \
    grep -v '^\+\s*//' | \
    grep -v '^\+\s*$' | \
    wc -l)

  if [ "$CHANGED_LINES" -gt 0 ]; then
    SUBSTANTIVE_CHANGES=true
    break
  fi
done

if [ "$SUBSTANTIVE_CHANGES" = false ]; then
  echo "ℹ️  No substantive changes, skipping graph regeneration"
  exit 0
fi

echo "🔄 Regenerating knowledge graphs..."

# Incremental update
ggen graph build --incremental --changed "$STAGED_FILES" --output graphs/

# Stage updated graphs
git add graphs/*.ttl

echo "✅ Graphs updated ($(echo "$STAGED_FILES" | wc -l) files)"
exit 0
```

### Rollback-Safe Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit-safe
# Backup graphs before regeneration, rollback on failure

BACKUP_DIR=".git/graph-backups/$(date +%s)"
mkdir -p "$BACKUP_DIR"

# Backup existing graphs
cp -r graphs/*.ttl "$BACKUP_DIR/" 2>/dev/null || true

# Regenerate
if ggen graph build --incremental --changed "$(git diff --cached --name-only)"; then
  git add graphs/*.ttl
  echo "✅ Graphs regenerated successfully"
  rm -rf "$BACKUP_DIR"  # Cleanup backup
  exit 0
else
  echo "❌ Graph regeneration failed, rolling back..."
  cp "$BACKUP_DIR"/*.ttl graphs/ 2>/dev/null || true
  rm -rf "$BACKUP_DIR"
  exit 1  # Block commit on failure
fi
```

## Consequences

### Benefits

1. **Zero Cognitive Overhead**: Developers never think about graph updates
2. **Guaranteed Freshness**: Graphs always reflect current code state
3. **Reduced Errors**: Eliminates stale graph issues
4. **Seamless Integration**: Works within existing Git workflows
5. **Audit Trail**: Graph updates tied to commits automatically

### Drawbacks

1. **Hook Management**: Requires hook installation across team
2. **Performance Impact**: Hooks add latency to Git operations
3. **Debugging Complexity**: Hook failures can be opaque
4. **Partial Updates Risk**: Incremental updates may miss cross-file changes
5. **Conflict Potential**: Auto-commits can complicate rebasing

### Mitigations

- **Performance**: Use incremental regeneration, parallel processing
- **Installation**: Provide setup script: `ggen hooks install`
- **Debugging**: Add `--verbose` mode, log to `.git/ggen-hooks.log`
- **Consistency**: Periodic full rebuilds (nightly CI)
- **Conflicts**: Use `[skip ci]` markers, clear commit messages

## Implementation Notes

### Hook Installation Automation

```rust
// ggen/src/hooks/install.rs
pub fn install_hooks() -> Result<()> {
    let hooks = [
        ("pre-commit", include_str!("../hooks/pre-commit.sh")),
        ("post-merge", include_str!("../hooks/post-merge.sh")),
    ];

    for (name, content) in &hooks {
        let path = Path::new(".git/hooks").join(name);
        fs::write(&path, content)?;

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = fs::metadata(&path)?.permissions();
            perms.set_mode(0o755);  // Make executable
            fs::set_permissions(&path, perms)?;
        }
    }

    println!("✅ Installed {} Git hooks", hooks.len());
    Ok(())
}
```

### Configuration for Hook Behavior

```toml
# .ggen/hooks.toml
[hooks]
enabled = true
incremental = true  # Use incremental rebuilds
auto_commit = true  # Auto-commit graph changes
skip_on_trivial = true  # Skip if only comments changed

[hooks.pre_commit]
enabled = true
backup = true  # Backup before regeneration
timeout_secs = 30

[hooks.post_merge]
enabled = true
full_rebuild = true  # Always full rebuild after merge

[hooks.ci]
enabled = true
platforms = ["github", "gitlab"]
```

## Related Patterns

- **Pattern 022 (Delta-Driven Regeneration)**: Efficient incremental updates
- **Pattern 024 (Git-as-Runtime)**: Git as execution substrate
- **Pattern 018 (Graph Versioning)**: Managing graph evolution over time
- **Pattern 012 (Template Live Reload)**: Similar reactive approach for templates

## Known Uses

### Linux Kernel SPARQL Interface

Uses post-commit hooks to regenerate queryable graphs of kernel subsystems after each commit.

### Rust Compiler Documentation

rustdoc hooks regenerate cross-reference graphs automatically during CI builds.

### Enterprise Code Search

File system watchers maintain real-time code graphs for instant semantic search.

## Example: Complete Hook Suite

```bash
# ggen hooks install --all
# Creates .git/hooks/ with:

# pre-commit: Fast incremental updates
# post-commit: Log generation stats
# post-merge: Full consistency rebuild
# post-rewrite: Handle rebases
# prepare-commit-msg: Add graph stats to commit message
```

**Output:**
```
✅ Installed 5 Git hooks
  • pre-commit: Incremental graph regeneration
  • post-merge: Full graph rebuild
  • post-rewrite: Rebase-safe updates
  • post-commit: Statistics logging
  • prepare-commit-msg: Auto-annotate commits

Run 'ggen hooks status' to verify installation
Run 'ggen hooks uninstall' to remove
```

---

**Pattern Status**: ✅ Complete
**Cookbook Chapter**: Part IV - Autonomic Patterns
**Dependencies**: Git, file system events, CI/CD platform
**Complexity**: Medium
**Maintenance**: Low (self-maintaining once installed)
