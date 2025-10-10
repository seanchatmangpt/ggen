# Chapter: Git-as-Runtime - Version Control as Execution Substrate

## The Central Insight

Most knowledge graph systems treat version control as an afterthought:

```
Code (Git) → Generate → Graphs (Database/Files) → ??? → Deploy
```

This creates friction:
- Separate storage for code and graphs
- Manual synchronization
- Versioning inconsistencies
- Complex deployment

**Git-as-Runtime** inverts this: **Git is not just storage, it's the execution substrate.**

```
Code (Git) ←→ Graphs (Git) → Git Operations Trigger Regeneration
```

## Pattern 024: Git-as-Runtime

### Core Principles

1. **Co-Location**: Graphs live in the repository alongside code
2. **Atomic Commits**: Code + graph changes committed together
3. **Hook-Driven**: Git operations trigger graph operations
4. **Single Source of Truth**: Git history = complete evolution of code and knowledge
5. **Time Travel**: Any commit → exact code + graph state

### Repository Structure

```
my-project/
├── src/                    # Code
│   ├── lib.rs
│   └── parser.rs
├── graphs/                 # Graphs (versioned alongside code)
│   ├── main.ttl           # Full project graph
│   ├── modules/
│   │   ├── parser.ttl     # Per-module graphs
│   │   └── generator.ttl
│   └── .gitattributes     # LFS config for large graphs
├── .ggen/
│   └── config.toml
└── .git/
    └── hooks/             # Automation
        ├── pre-commit     # Regenerate on commit
        ├── post-merge     # Rebuild after merge
        └── post-checkout  # Sync on branch switch
```

**Key insight**: The `graphs/` directory is **not** `.gitignore`d. It's versioned.

### Why This Works

**1. Reproducibility**

```bash
$ git checkout v1.0.0
# You now have:
# - Code as of v1.0.0
# - Graph as of v1.0.0
# Perfect synchronization, no external dependencies
```

**2. Collaboration**

```bash
# Reviewer sees both code and graph changes in PR
$ git diff main..feature-branch

# Code changes
diff --git a/src/parser.rs b/src/parser.rs
+fn validate_utf8(input: &[u8]) -> Result<()> { ... }

# Graph changes (semantic diff)
diff --git a/graphs/main.ttl b/graphs/main.ttl
+:UTF8Validator a :Function ;
+    :definedIn :parser_module ;
+    :signature "validate_utf8(&[u8]) -> Result<()>" .
```

**3. Deployment Simplicity**

```bash
# No external database required
$ git clone repo.git
$ cd repo
# Graphs immediately available in graphs/
```

## Git Operations as Computation Triggers

### Pre-Commit: Incremental Updates

```bash
#!/bin/bash
# .git/hooks/pre-commit
# Triggered on: git commit

STAGED_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep '\.rs$')

if [ -n "$STAGED_FILES" ]; then
  echo "🔄 Regenerating graphs for changed files..."

  # Incremental update (Pattern 022)
  ggen graph --incremental --changed $STAGED_FILES

  # Validate
  ggen validate graphs/main.ttl || exit 1

  # Stage updated graphs
  git add graphs/*.ttl

  echo "✅ Graphs updated and staged"
fi

exit 0
```

**Effect**: Every commit bundles code + graph changes atomically.

### Post-Merge: Consistency Rebuilds

```bash
#!/bin/bash
# .git/hooks/post-merge
# Triggered on: git pull, git merge

echo "🔄 Post-merge: Rebuilding complete graph..."

# Full rebuild ensures consistency after merge
ggen graph --full --output graphs/main.ttl

# Commit if changed
if ! git diff --quiet graphs/main.ttl; then
  git add graphs/main.ttl
  git commit -m "chore: rebuild knowledge graph post-merge [skip ci]"
  echo "✅ Graph reconciled after merge"
fi
```

**Why full rebuild?** Merges can create cross-file semantic changes that incremental updates might miss.

### Post-Checkout: Branch Synchronization

```bash
#!/bin/bash
# .git/hooks/post-checkout
# Triggered on: git checkout, git switch

PREV_HEAD=$1
NEW_HEAD=$2
BRANCH_SWITCH=$3

if [ "$BRANCH_SWITCH" = "1" ]; then
  echo "🔄 Branch switch detected, syncing graphs..."

  # If new branch lacks graphs, generate them
  if [ ! -f graphs/main.ttl ]; then
    ggen graph --full --output graphs/main.ttl
    git add graphs/main.ttl
    git commit -m "chore: initialize graphs for branch [skip ci]"
  fi
fi
```

**Effect**: Each branch maintains its own graph state, just like code.

## Git History as Graph Evolution

### Time Travel Queries

```bash
# View graph at specific commit
$ ggen git show --commit v1.0.0 --graph graphs/main.ttl > /tmp/v1-graph.ttl

# Compare graph evolution across versions
$ ggen git diff --from v1.0.0 --to v2.0.0 --graph graphs/main.ttl
```

**Implementation:**

```rust
// ggen/src/git/versioning.rs
pub struct GitGraphVersioning {
    repo: Repository,
}

impl GitGraphVersioning {
    pub fn graph_at_commit(&self, commit: &str, path: &str) -> Result<String> {
        let commit = self.repo.find_commit(Oid::from_str(commit)?)?;
        let tree = commit.tree()?;
        let entry = tree.get_path(Path::new(path))?;
        let blob = self.repo.find_blob(entry.id())?;

        Ok(String::from_utf8(blob.content().to_vec())?)
    }

    pub fn graph_diff(&self, from: &str, to: &str, path: &str)
        -> Result<SemanticDiff>
    {
        let from_graph = parse_graph(&self.graph_at_commit(from, path)?)?;
        let to_graph = parse_graph(&self.graph_at_commit(to, path)?)?;

        // Semantic diff: entities added/removed/modified
        Ok(compute_semantic_diff(&from_graph, &to_graph))
    }
}
```

### Graph History Log

```bash
$ ggen git history --graph graphs/main.ttl --limit 5
📜 Recent changes to graphs/main.ttl:

[abc1234] 2 hours ago - Jane Doe
  chore: rebuild graph post-merge

[def5678] 1 day ago - John Smith
  feat: add semantic search module
  +75 triples (3 new entities)

[ghi9012] 2 days ago - Jane Doe
  refactor: reorganize module structure
  ~120 triples modified

[jkl3456] 3 days ago - CI Bot
  chore: auto-update knowledge graph
  +5 triples
```

## Handling Large Graphs: Git LFS

For graphs >1MB, use Git Large File Storage:

```bash
# .gitattributes
graphs/**/*.ttl filter=lfs diff=lfs merge=lfs -text
graphs/**/*.nt filter=lfs diff=lfs merge=lfs -text
```

**Benefits:**
- Repository stays lightweight
- Large graphs stored efficiently
- LFS handles bandwidth optimization

**Setup:**

```bash
$ git lfs install
$ git lfs track "graphs/*.ttl"
$ git add .gitattributes
$ git commit -m "chore: configure LFS for graphs"
```

## CI/CD Integration

### Graph Validation Pipeline

```yaml
# .github/workflows/validate-graphs.yml
name: Validate Knowledge Graphs

on:
  pull_request:
  push:
    branches: [main]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          lfs: true  # Pull LFS objects

      - name: Install ggen
        run: cargo install ggen

      - name: Validate graph consistency
        run: |
          # Rebuild from source
          ggen graph --full --output /tmp/rebuilt.ttl

          # Compare with committed graph
          if ! diff -q graphs/main.ttl /tmp/rebuilt.ttl; then
            echo "❌ Committed graph doesn't match source"
            echo "Run: ggen graph --full"
            exit 1
          fi

          echo "✅ Graph consistent with source"

      - name: Semantic validation
        run: |
          # Run SPARQL test queries
          ggen query graphs/main.ttl \
            --sparql tests/queries/*.rq \
            --expect tests/expected/*.json
```

**This ensures:**
- PRs with inconsistent graphs are rejected
- Manual graph edits (bypassing ggen) are caught
- Graph meets semantic contracts (via test queries)

### Auto-Update on Merge

```yaml
# .github/workflows/auto-update-graphs.yml
name: Auto-Update Graphs

on:
  push:
    branches: [main, develop]

jobs:
  update:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: cargo install ggen

      - name: Detect changes
        id: changes
        run: |
          CHANGED=$(git diff --name-only HEAD~1 HEAD | grep '\.rs$' || true)
          echo "files=$CHANGED" >> $GITHUB_OUTPUT

      - name: Regenerate if needed
        if: steps.changes.outputs.files != ''
        run: ggen graph --incremental --changed "${{ steps.changes.outputs.files }}"

      - name: Commit updated graphs
        uses: stefanzweifel/git-auto-commit-action@v4
        with:
          commit_message: "chore: auto-update graphs [skip ci]"
          file_pattern: "graphs/*.ttl"
```

## Merge Conflict Resolution

**Problem**: What if both branches modify the same graph?

```
<<<<<<< HEAD
:FunctionA :callCount 5 .
=======
:FunctionA :callCount 3 .
>>>>>>> feature-branch
```

**Solution**: Auto-rebuild on conflict.

```bash
# .git/hooks/post-merge
if [ -f graphs/main.ttl.orig ]; then
  # Conflict detected (Git created .orig backup)
  echo "⚠️  Graph conflict detected, rebuilding from source..."

  rm graphs/main.ttl.orig
  ggen graph --full --output graphs/main.ttl

  git add graphs/main.ttl
  git commit -m "chore: resolve graph conflict via rebuild [skip ci]"
fi
```

**Principle**: Graphs are **derived artifacts**. On conflict, regenerate from code (source of truth).

## Semantic Diff Tooling

Raw Turtle diffs are unreadable. Provide semantic diffs:

```bash
$ ggen git diff --from v1.0.0 --to v2.0.0 --semantic
📊 Semantic changes from v1.0.0 to v2.0.0:

Added Entities (3):
  • :UTF8Validator (Function)
  • :CharsetDetector (Struct)
  • :EncodingError (Enum)

Removed Entities (1):
  • :LegacyParser (deprecated)

Modified Entities (12):
  • :Parser
    + Added property: :supports_utf8 = true
    ~ Modified: :version "1.0" → "2.0"
  • :Tokenizer
    + Added method: :validate_encoding
  ...

Relationship Changes:
  + :Parser :uses :UTF8Validator
  - :Parser :depends_on :LegacyParser
```

**Implementation**: Compare entity sets and properties, ignore Turtle formatting.

## Benefits Recap

| Aspect | Traditional | Git-as-Runtime |
|--------|-------------|----------------|
| Storage | Code (Git) + Graphs (DB) | Code + Graphs (Git) |
| Sync | Manual | Automatic (hooks) |
| Versioning | Code only | Code + graphs atomic |
| Deployment | Complex (DB setup) | Simple (git clone) |
| Reproducibility | Difficult | Built-in (checkout commit) |
| Audit Trail | Fragmented | Unified Git history |
| Collaboration | Graph changes opaque | Graph changes in PRs |

## Limitations and Mitigations

**Limitation**: Repository size grows with graph history.
**Mitigation**: Git LFS for large graphs, periodic garbage collection.

**Limitation**: Merge conflicts in graphs.
**Mitigation**: Auto-rebuild from source on conflict.

**Limitation**: Hook management across team.
**Mitigation**: `ggen hooks install` command, documented in README.

**Limitation**: Binary diffs less readable.
**Mitigation**: `ggen git diff --semantic` for human-readable changes.

## Summary

Git-as-Runtime elevates Git from passive storage to active execution substrate:

1. **Graphs versioned alongside code** (co-location)
2. **Git operations trigger graph operations** (hooks)
3. **Single source of truth** (Git history)
4. **Time travel built-in** (checkout = code + graph state)
5. **Deployment = git clone** (no external dependencies)

This pattern is the foundation of **autonomic knowledge graphs**: systems that maintain themselves through the natural workflow of version control.

**Next**: Scheduling chapter — temporal patterns for periodic graph maintenance (nightly rebuilds, cache refresh, etc.)

---

**Chapter Status**: ✅ Complete
**Related Patterns**: 024, 021, 022, 018
**Prerequisites**: Git, basic hook understanding
**Complexity**: Medium
**Team Coordination**: High (hooks must be installed consistently)
