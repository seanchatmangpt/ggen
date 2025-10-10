# Pattern 024: Git-as-Runtime

## Intent

Use Git's version control infrastructure as an execution substrate for graph operations, treating commits as computation triggers and the repository as a persistent graph database.

## Also Known As

- Repository-as-Runtime
- VCS-Driven Execution
- Git-Native Automation

## Motivation

Git is already the source of truth for code. Why not also for graphs?

**Traditional Approach:**
- Separate graph storage (databases, files)
- Manual synchronization with code
- Disconnect between code versions and graph versions
- Complex deployment and state management

**Git-as-Runtime Approach:**
- Graphs versioned alongside code
- Automatic synchronization via hooks
- Single source of truth
- Git operations trigger graph operations

This unifies code and knowledge evolution in a single version-controlled timeline.

## Applicability

Use Git-as-Runtime when:

- Version control is mandatory (team projects, open source)
- Reproducibility is critical (research, compliance)
- Deployment simplicity is valued (no external databases)
- Audit trails are required (financial, medical)
- Graph history matters (evolution analysis, debugging)

## Structure

```
┌─────────────────────────────────────┐
│          Git Repository             │
│  ┌──────────────┬────────────────┐ │
│  │ Code Files   │ Graph Files    │ │
│  │ src/*.rs     │ graphs/*.ttl   │ │
│  └──────┬───────┴────────┬───────┘ │
│         │                 │          │
│         │  Git Operations │          │
│         ▼                 ▼          │
│  ┌──────────────────────────────┐  │
│  │      Commit Timeline         │  │
│  │  [C1]──[C2]──[C3]──[C4]...  │  │
│  │   │     │     │     │         │  │
│  │  code  hook  code  merge     │  │
│  └──────┬───────────────────────┘  │
└─────────┼──────────────────────────┘
          │ triggers
          ▼
┌─────────────────────────┐
│   Hook Automation       │
│  - Parse code changes   │
│  - Regenerate graphs    │
│  - Commit graph updates │
│  - Push to remote       │
└─────────────────────────┘
```

### Key Components

1. **Code-Graph Co-Location**: Graphs stored in repository
2. **Hook Automation**: Git hooks trigger graph operations
3. **Atomic Commits**: Code + graph changes bundled
4. **Branch Synchronization**: Graph branches track code branches
5. **Remote Backing**: Git remote serves as graph backup/distribution

## Implementation

### Repository Structure

```
my-project/
├── src/                    # Source code
│   ├── lib.rs
│   └── modules/
│       ├── parser.rs
│       └── generator.rs
├── graphs/                 # Generated knowledge graphs
│   ├── main.ttl           # Full project graph
│   ├── modules/           # Per-module graphs
│   │   ├── parser.ttl
│   │   └── generator.ttl
│   └── .gitattributes     # LFS config for large graphs
├── .ggen/                 # GGen configuration
│   ├── config.toml
│   └── templates/
└── .git/
    └── hooks/             # Git hooks for automation
        ├── pre-commit     # Regenerate on commit
        ├── post-merge     # Rebuild after merge
        └── post-checkout  # Update on branch switch
```

### Git Hooks Integration

#### Pre-Commit Hook (Graph Generation)

```bash
#!/bin/bash
# .git/hooks/pre-commit
# Automatically regenerate graphs before committing code changes

set -e  # Exit on error

STAGED_CODE_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep '\.rs$' || true)

if [ -z "$STAGED_CODE_FILES" ]; then
  # No code changes, check if graph changes are valid
  STAGED_GRAPH_FILES=$(git diff --cached --name-only | grep 'graphs/.*\.ttl$' || true)

  if [ -n "$STAGED_GRAPH_FILES" ]; then
    echo "⚠️  Graph files staged without code changes"
    echo "   This is unusual. Continue? [y/N]"
    read -r response
    if [ "$response" != "y" ]; then
      exit 1
    fi
  fi

  exit 0
fi

echo "🔄 Code changes detected, regenerating graphs..."

# Incremental graph update
ggen graph --incremental --changed $STAGED_CODE_FILES

# Validate generated graphs
if ! ggen validate graphs/main.ttl; then
  echo "❌ Generated graph is invalid, blocking commit"
  exit 1
fi

# Stage updated graphs
git add graphs/*.ttl

echo "✅ Graphs regenerated and staged"
exit 0
```

#### Post-Merge Hook (Consistency Rebuild)

```bash
#!/bin/bash
# .git/hooks/post-merge
# Rebuild complete graph after merge to ensure consistency

echo "🔄 Post-merge: Rebuilding knowledge graph..."

# Full rebuild (merges can have complex cross-file effects)
ggen graph --full --output graphs/main.ttl

# Check if rebuild changed anything
if git diff --quiet graphs/main.ttl; then
  echo "✅ Graph unchanged after merge"
else
  echo "📝 Graph updated after merge, creating fixup commit..."

  git add graphs/main.ttl
  git commit -m "chore: rebuild knowledge graph post-merge

This commit reconciles the knowledge graph after merging branches.
Graph differences indicate cross-branch semantic changes.

[skip ci]"

  echo "✅ Graph rebuild committed"
fi
```

#### Post-Checkout Hook (Branch Sync)

```bash
#!/bin/bash
# .git/hooks/post-checkout
# Update graphs when switching branches

PREV_HEAD=$1
NEW_HEAD=$2
BRANCH_SWITCH=$3

if [ "$BRANCH_SWITCH" != "1" ]; then
  exit 0  # Not a branch switch, skip
fi

echo "🔄 Branch switch detected, syncing graphs..."

# Check if graphs exist in new branch
if [ ! -f graphs/main.ttl ]; then
  echo "📝 No graphs in this branch, generating..."
  ggen graph --full --output graphs/main.ttl

  git add graphs/main.ttl
  git commit -m "chore: initialize knowledge graph for branch

[skip ci]"
else
  echo "✅ Graphs present and current"
fi
```

### Git-LFS for Large Graphs

```bash
# .gitattributes
# Use Git LFS for large Turtle files to avoid bloating repository

# Track large graphs with LFS
graphs/**/*.ttl filter=lfs diff=lfs merge=lfs -text
graphs/**/*.nt filter=lfs diff=lfs merge=lfs -text
graphs/**/*.rdf filter=lfs diff=lfs merge=lfs -text

# Regular tracking for small config files
.ggen/**/*.toml -filter -diff -merge text
```

### CI/CD Integration

```yaml
# .github/workflows/graph-validation.yml
name: Knowledge Graph Validation

on:
  push:
    branches: [main, develop]
  pull_request:

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
          # Rebuild graph from source
          ggen graph --full --output /tmp/rebuilt.ttl

          # Compare with committed graph
          if ! diff -q graphs/main.ttl /tmp/rebuilt.ttl; then
            echo "❌ Committed graph does not match source code"
            echo "   Run 'ggen graph --full' and commit the result"
            exit 1
          fi

          echo "✅ Graph is consistent with source"

      - name: Run graph queries
        run: |
          # Test critical queries against committed graph
          ggen query graphs/main.ttl \
            --sparql queries/test-suite.rq \
            --expect queries/expected.json
```

## Sample Code

### Graph Versioning CLI

```rust
// ggen/src/git/versioning.rs
use git2::Repository;

pub struct GitGraphVersioning {
    repo: Repository,
}

impl GitGraphVersioning {
    /// Get graph state at specific commit
    pub fn graph_at_commit(&self, commit_hash: &str, graph_path: &str)
        -> Result<String>
    {
        let commit = self.repo.find_commit(
            git2::Oid::from_str(commit_hash)?
        )?;

        let tree = commit.tree()?;
        let entry = tree.get_path(Path::new(graph_path))?;
        let blob = self.repo.find_blob(entry.id())?;

        Ok(String::from_utf8(blob.content().to_vec())?)
    }

    /// Compare graphs between two commits
    pub fn graph_diff(&self, from: &str, to: &str, graph_path: &str)
        -> Result<GraphDiff>
    {
        let from_graph = self.graph_at_commit(from, graph_path)?;
        let to_graph = self.graph_at_commit(to, graph_path)?;

        compute_graph_diff(&from_graph, &to_graph)
    }

    /// List all commits that modified a graph
    pub fn graph_history(&self, graph_path: &str) -> Result<Vec<CommitInfo>> {
        let mut revwalk = self.repo.revwalk()?;
        revwalk.push_head()?;

        let mut history = Vec::new();

        for oid in revwalk {
            let commit = self.repo.find_commit(oid?)?;

            // Check if this commit touched the graph file
            if self.commit_touches_file(&commit, graph_path)? {
                history.push(CommitInfo {
                    hash: commit.id().to_string(),
                    message: commit.message().unwrap_or("").to_string(),
                    timestamp: commit.time().seconds(),
                    author: commit.author().name().unwrap_or("").to_string(),
                });
            }
        }

        Ok(history)
    }
}
```

### CLI Commands for Git Integration

```rust
// ggen/src/main.rs
#[derive(Parser)]
enum Command {
    /// Git integration commands
    Git {
        #[command(subcommand)]
        subcmd: GitCommand,
    },
}

#[derive(Subcommand)]
enum GitCommand {
    /// Show graph at specific commit
    Show {
        #[arg(long)]
        commit: String,

        #[arg(long, default_value = "graphs/main.ttl")]
        graph: String,
    },

    /// Compare graphs between commits
    Diff {
        #[arg(long)]
        from: String,

        #[arg(long)]
        to: String,

        #[arg(long, default_value = "graphs/main.ttl")]
        graph: String,
    },

    /// Show graph evolution history
    History {
        #[arg(long, default_value = "graphs/main.ttl")]
        graph: String,

        #[arg(long, default_value = "10")]
        limit: usize,
    },
}
```

**Usage Examples:**

```bash
# View graph at specific commit
$ ggen git show --commit abc123 --graph graphs/main.ttl > /tmp/historic-graph.ttl

# Compare graph evolution
$ ggen git diff --from v1.0.0 --to v2.0.0 --graph graphs/main.ttl
📊 Graph changes from v1.0.0 to v2.0.0:
   • +150 triples (new entities)
   • -23 triples (removed entities)
   • Modified: 45 entities
   • Key changes:
     - Added module: src/new_feature.rs
     - Removed deprecated: old_api.rs
     - Refactored: 12 function signatures

# Show graph commit history
$ ggen git history --graph graphs/main.ttl --limit 5
📜 Recent changes to graphs/main.ttl:

[abc1234] 2 hours ago - Jane Doe
  chore: rebuild graph post-merge

[def5678] 1 day ago - John Smith
  feat: add semantic search module
  +75 triples

[ghi9012] 2 days ago - Jane Doe
  refactor: reorganize module structure
  ~120 triples modified

[jkl3456] 3 days ago - CI Bot
  chore: auto-update knowledge graph
  +5 triples

[mno7890] 5 days ago - John Smith
  feat: implement graph caching
  +200 triples
```

## Consequences

### Benefits

1. **Single Source of Truth**: Code and graphs versioned together
2. **Automatic Backup**: Git remote serves as graph backup
3. **Time Travel**: Access graphs at any historical commit
4. **Reproducibility**: Checkout commit → get matching graph
5. **Collaboration**: Graph changes reviewed via pull requests
6. **Audit Trail**: Complete graph evolution history
7. **Deployment Simplicity**: No external database required

### Drawbacks

1. **Repository Size**: Large graphs inflate repo size (mitigate with LFS)
2. **Merge Conflicts**: Graph conflicts harder to resolve than code
3. **Hook Management**: Team must install hooks consistently
4. **Performance**: Git operations slower with large files
5. **Binary Diffs**: Graph diffs less readable than code diffs

### Mitigations

- **Size**: Use Git LFS for graphs >1MB, periodic graph compaction
- **Conflicts**: Auto-rebuild on conflict, prefer full regeneration
- **Hooks**: Provide `ggen hooks install` command, document in README
- **Performance**: Incremental updates, background regeneration
- **Readability**: Provide semantic diff tool: `ggen git diff --semantic`

## Implementation Notes

### Semantic Graph Diffs

```rust
// Provide human-readable graph diffs instead of raw Turtle diffs
pub fn semantic_graph_diff(from: &Graph, to: &Graph) -> SemanticDiff {
    let added_entities = to.entities().difference(&from.entities());
    let removed_entities = from.entities().difference(&to.entities());
    let common_entities = from.entities().intersection(&to.entities());

    let mut modified = Vec::new();
    for entity in common_entities {
        let from_triples = from.triples_for_entity(entity);
        let to_triples = to.triples_for_entity(entity);

        if from_triples != to_triples {
            modified.push(EntityChange {
                uri: entity.clone(),
                added_properties: to_triples.difference(&from_triples).count(),
                removed_properties: from_triples.difference(&to_triples).count(),
            });
        }
    }

    SemanticDiff {
        added_entities: added_entities.cloned().collect(),
        removed_entities: removed_entities.cloned().collect(),
        modified_entities: modified,
    }
}
```

### Hook Installation Script

```rust
// ggen hooks install
pub fn install_git_hooks() -> Result<()> {
    let repo = Repository::open(".")?;
    let hooks_dir = repo.path().join("hooks");

    let hooks = [
        ("pre-commit", include_str!("../hooks/pre-commit.sh")),
        ("post-merge", include_str!("../hooks/post-merge.sh")),
        ("post-checkout", include_str!("../hooks/post-checkout.sh")),
    ];

    for (name, content) in &hooks {
        let path = hooks_dir.join(name);

        // Backup existing hook if present
        if path.exists() {
            fs::rename(&path, path.with_extension("backup"))?;
        }

        fs::write(&path, content)?;
        make_executable(&path)?;

        println!("✅ Installed {}", name);
    }

    println!("\n📝 Hooks installed. Graphs will auto-update on Git operations.");
    Ok(())
}
```

## Related Patterns

- **Pattern 021 (Knowledge Hooks)**: Hook mechanisms this pattern leverages
- **Pattern 022 (Delta-Driven Regeneration)**: Efficient updates for Git hooks
- **Pattern 018 (Graph Versioning)**: Graph evolution over time
- **Pattern 013 (Multi-Format Output)**: Different graph serializations

## Known Uses

### Linux Kernel Documentation

Uses Git-tracked graphs for subsystem relationships, regenerated on each kernel release.

### Rust Language Reference

Code examples and semantic graphs versioned together in rust-lang/reference repository.

### Scientific Reproducibility

Research papers with companion code repositories version graphs alongside data pipelines.

## Example: Complete Workflow

```bash
# Developer workflow with Git-as-Runtime

# 1. Make code changes
$ vim src/parser.rs

# 2. Commit (hook auto-regenerates graph)
$ git commit -m "feat: add UTF-8 validation to parser"
🔄 Code changes detected, regenerating graphs...
✅ Graphs regenerated and staged
[main abc1234] feat: add UTF-8 validation to parser
 2 files changed, 45 insertions(+), 10 deletions(-)
 modified:   src/parser.rs
 modified:   graphs/main.ttl

# 3. Push (graph goes to remote with code)
$ git push origin main

# 4. Teammate pulls and gets matching graph
$ git pull origin main
🔄 Post-merge: Rebuilding knowledge graph...
✅ Graph unchanged after merge

# 5. Review graph evolution
$ ggen git diff --from HEAD~5 --to HEAD
📊 Graph changes (last 5 commits):
   • +89 triples
   • 3 new entities (UTF8Validator, CharsetDetector, EncodingError)
   • 12 modified function signatures
```

---

**Pattern Status**: ✅ Complete
**Cookbook Chapter**: Part IV - Autonomic Patterns
**Dependencies**: Git, Git LFS (optional), Git hooks
**Complexity**: Medium
**Maintenance**: Low (self-maintaining via hooks)
**Adoption**: Team coordination required for hook installation
