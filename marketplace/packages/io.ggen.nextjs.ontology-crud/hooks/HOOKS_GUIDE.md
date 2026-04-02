# Complete Guide: Ontology-Driven Git Hooks

Automatic code regeneration system that keeps your TypeScript codebase in sync with RDF ontologies.

## Quick Start

```bash
# 1. Install hooks
./docs/examples/scripts/install-hooks.sh

# 2. Test installation
./docs/examples/scripts/test-hooks.sh

# 3. Create ontology
vim docs/examples/ontology/my-app.ttl

# 4. Commit (triggers automatic regeneration)
git add docs/examples/ontology/my-app.ttl
git commit -m "feat: add ontology"
```

## System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Git Workflow                             │
└─────────────────────────────────────────────────────────────┘
                           │
                           ▼
              ┌─────────────────────────┐
              │   Modify .ttl file      │
              │   git add ontology.ttl  │
              └─────────────────────────┘
                           │
                           ▼
              ┌─────────────────────────┐
              │   git commit -m "..."   │
              └─────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                  Pre-Commit Hook                             │
│  1. Detect .ttl/.rdf/.owl changes                           │
│  2. Validate ontologies (ggen graph validate)               │
│  3. Load into graph (ggen graph load)                       │
│  4. Generate TypeScript types                               │
│  5. Generate API routes                                     │
│  6. Generate CRUD components                                │
│  7. Run TypeScript type check                               │
│  8. Stage generated files (git add)                         │
└─────────────────────────────────────────────────────────────┘
                           │
                           ▼
              ┌─────────────────────────┐
              │  Commit proceeds with   │
              │  ontology + generated   │
              │  code together          │
              └─────────────────────────┘
```

## Hook Details

### Pre-Commit Hook (`pre-commit.sh`)

**Purpose:** Ensure ontology and code stay synchronized in every commit.

**Trigger:** When you commit files that include `.ttl`, `.rdf`, or `.owl` files.

**Steps:**
1. Detect ontology changes in staging area
2. Validate each ontology file
3. Call regeneration script
4. Stage generated files
5. Allow commit to proceed

**Timing:** ~3-5 seconds for typical ontologies

**Benefits:**
- ✅ Prevents commits with invalid ontologies
- ✅ Keeps generated code in sync automatically
- ✅ Reduces manual regeneration work
- ✅ Catches TypeScript errors early

### Post-Merge Hook (`post-merge.sh`)

**Purpose:** Regenerate code after pulling changes with ontology updates.

**Trigger:** After `git pull` or `git merge` completes.

**Steps:**
1. Check if ontologies changed in merge
2. Run regeneration script if needed
3. Report uncommitted changes
4. Prompt developer to review and commit

**Timing:** ~5-8 seconds for typical ontologies

**Benefits:**
- ✅ Automatic regeneration after pulls
- ✅ Clear notification of what changed
- ✅ Prevents running with stale generated code
- ✅ Maintains consistency across team

**Why Merge Instead of Rebase:**

This workflow uses merge operations rather than rebasing for several critical reasons:

- **History Preservation**: Merge commits preserve the complete development history, showing when and how changes were integrated. Rebasing rewrites history, making it impossible to see the actual sequence of events.

- **Collaborative Safety**: Rebasing requires force-pushing, which disrupts other developers who have already pulled the branch. Merge operations work seamlessly with multiple developers on the same branch without requiring force-pushes.

- **Conflict Resolution Tracking**: Merge commits maintain a clear record of conflict resolution decisions. Rebasing loses this context by creating new commits with different SHAs.

- **Debugging Stability**: Commit SHAs remain stable with merges, allowing CI/CD pipelines, issue trackers, and debugging tools to reliably reference specific commits. Rebasing changes commit hashes, breaking these references.

- **CI/CD Compatibility**: Automated systems that reference commit hashes (build artifacts, deployment pipelines, test results) break when commits are rebased. Merge operations maintain stable commit references.

- **Reduced Conflicts**: When multiple developers work on the same branch, rebasing creates unnecessary conflicts as each developer must rebase their work on top of others' rebased commits. Merging handles parallel development naturally.

- **Audit Trail**: Merge commits provide a clear audit trail showing when features were integrated and by whom. This is essential for compliance, debugging, and understanding project evolution.

### Regeneration Script (`regenerate-from-ontology.sh`)

**Purpose:** Core logic for code generation from ontologies.

**Modes:**
- `--quiet`: Minimal output (for automation)
- Normal: Standard progress messages
- `--verbose`: Detailed logging (for debugging)

**Process:**
```bash
For each ontology file:
  1. Validate syntax and semantics
  2. Load into graph store
  3. Generate TypeScript types (*.types.ts)
  4. Generate API routes (*.routes.ts)
  5. Generate CRUD components (*.component.tsx)
  6. Run TypeScript type checking
  7. Report changes
```

**Output:**
```
generated/
├── types/
│   ├── task-management.types.ts
│   ├── user-profile.types.ts
│   └── comment-system.types.ts
├── api/
│   ├── task-management.routes.ts
│   ├── user-profile.routes.ts
│   └── comment-system.routes.ts
└── components/
    ├── task-management.component.tsx
    ├── user-profile.component.tsx
    └── comment-system.component.tsx
```

## Usage Examples

### Example 1: Adding a New Entity

**Scenario:** Add a `Priority` entity to task management ontology.

```bash
# 1. Edit ontology
vim docs/examples/ontology/task-management.ttl

# Add:
ex:Priority a rdfs:Class ;
    rdfs:label "Priority" ;
    rdfs:comment "Task priority level" .

ex:hasPriority a rdf:Property ;
    rdfs:domain ex:Task ;
    rdfs:range ex:Priority .

# 2. Stage and commit
git add docs/examples/ontology/task-management.ttl
git commit -m "feat: add Priority entity to task ontology"

# Hook runs automatically:
# [pre-commit] Validating ontologies...
# [pre-commit]   ✓ task-management.ttl is valid
# [pre-commit] Running code regeneration...
# [pre-commit] Generated/modified files:
#   - generated/types/task-management.types.ts
#   - generated/api/task-management.routes.ts
#   - generated/components/task-management.component.tsx
# [pre-commit] ✓ Code regeneration completed in 3s
```

### Example 2: Team Collaboration

**Scenario:** Teammate pushes ontology changes.

```bash
# 1. Pull changes
git pull origin main

# Output:
# Updating abc123..def456
# Fast-forward
#  docs/examples/ontology/task-management.ttl | 10 ++++++++++
#  1 file changed, 10 insertions(+)

# [post-merge] Ontology changes detected in merge:
#   - docs/examples/ontology/task-management.ttl
# [post-merge] Running code regeneration...
# [post-merge] ✓ Code regeneration completed successfully (4s)
# [post-merge] ⚠ Code regeneration created uncommitted changes:
#   M generated/types/task-management.types.ts
#   M generated/api/task-management.routes.ts

# 2. Review changes
git diff generated/

# 3. Commit if appropriate
git add generated/
git commit -m "chore: regenerate from ontology updates"
git push origin main
```

### Example 3: Fixing Invalid Ontology

**Scenario:** Commit attempt with syntax error.

```bash
# 1. Edit ontology (introduce error)
vim docs/examples/ontology/task-management.ttl

# Missing closing dot:
ex:BadEntity a rdfs:Class

# 2. Try to commit
git add docs/examples/ontology/task-management.ttl
git commit -m "feat: add entity"

# Hook blocks commit:
# [pre-commit] Validating ontologies...
# [pre-commit]   ✗ Validation failed for task-management.ttl
# [pre-commit] Ontology validation failed. Please fix errors before committing.

# 3. Fix syntax
vim docs/examples/ontology/task-management.ttl
# Add missing dot:
ex:BadEntity a rdfs:Class .

# 4. Commit succeeds
git add docs/examples/ontology/task-management.ttl
git commit -m "feat: add entity"
# ✓ Success
```

## Configuration

### Customizing Paths

Edit `regenerate-from-ontology.sh`:

```bash
# Default paths
ONTOLOGY_DIR="${PROJECT_ROOT}/docs/examples/ontology"
OUTPUT_DIR="${PROJECT_ROOT}/generated"
TEMPLATES_DIR="${PROJECT_ROOT}/docs/examples/templates"

# Custom paths (example)
ONTOLOGY_DIR="${PROJECT_ROOT}/ontologies"
OUTPUT_DIR="${PROJECT_ROOT}/src/generated"
TEMPLATES_DIR="${PROJECT_ROOT}/templates"
```

### Customizing Templates

Create custom templates in `docs/examples/templates/`:

**`types.ts.hbs`** - TypeScript interfaces
```handlebars
// Generated from {{ontologyFile}}
{{#each classes}}
export interface {{name}} {
  {{#each properties}}
  {{name}}: {{type}};
  {{/each}}
}
{{/each}}
```

**`api-routes.ts.hbs`** - REST API endpoints
```handlebars
// Generated from {{ontologyFile}}
import { Router } from 'express';

{{#each classes}}
router.get('/{{pluralName}}', get{{name}}List);
router.post('/{{pluralName}}', create{{name}});
router.get('/{{pluralName}}/:id', get{{name}}ById);
router.put('/{{pluralName}}/:id', update{{name}});
router.delete('/{{pluralName}}/:id', delete{{name}});
{{/each}}
```

**`crud-component.tsx.hbs`** - React components
```handlebars
// Generated from {{ontologyFile}}
{{#each classes}}
export function {{name}}List() {
  const [items, setItems] = useState<{{name}}[]>([]);
  // ... CRUD logic
}
{{/each}}
```

### Adjusting Performance

**For faster execution:**
```bash
# In regenerate-from-ontology.sh

# Skip type checking for speed
# Comment out:
# if command -v tsc &> /dev/null; then
#   tsc --noEmit ...
# fi

# Or use faster validation
ggen graph validate --fast --file "$ONTOLOGY_FILE"
```

**For more thorough checks:**
```bash
# Add linting
eslint "$OUTPUT_DIR" --fix

# Add formatting
prettier "$OUTPUT_DIR" --write

# Add additional validation
ggen graph validate --strict --file "$ONTOLOGY_FILE"
```

## Advanced Features

### Selective Regeneration

Only regenerate specific parts:

```bash
# In regenerate-from-ontology.sh, comment out unwanted sections:

# Generate only types (skip API and components)
# if [ -f "$API_TEMPLATE" ]; then
#   # ... commented out
# fi
```

### Multi-Template Support

Generate multiple variants:

```bash
# Add in regenerate-from-ontology.sh:

# Generate React components
ggen template generate-rdf \
  --ontology "$ONTOLOGY_FILE" \
  --template "$TEMPLATES_DIR/react-component.tsx.hbs" \
  --output "$OUTPUT_DIR/react/${BASENAME}.component.tsx"

# Generate Vue components
ggen template generate-rdf \
  --ontology "$ONTOLOGY_FILE" \
  --template "$TEMPLATES_DIR/vue-component.vue.hbs" \
  --output "$OUTPUT_DIR/vue/${BASENAME}.component.vue"
```

### Environment-Specific Generation

```bash
# In regenerate-from-ontology.sh:

# Check environment
ENV="${NODE_ENV:-development}"

if [ "$ENV" = "production" ]; then
  # Production: strict validation, optimized output
  ggen graph validate --strict --file "$ONTOLOGY_FILE"
  ggen template generate-rdf --minify --optimize ...
else
  # Development: fast validation, readable output
  ggen graph validate --fast --file "$ONTOLOGY_FILE"
  ggen template generate-rdf --pretty ...
fi
```

## Troubleshooting

### Common Issues

**1. Hook not executing**
```bash
# Check executable bit
ls -la .git/hooks/pre-commit

# Fix permissions
chmod +x .git/hooks/pre-commit
```

**2. "ggen: command not found"**
```bash
# Install ggen
cargo install --path .

# Or add to PATH
export PATH="$HOME/.cargo/bin:$PATH"
```

**3. TypeScript errors after generation**
```bash
# Check generated code
cat generated/types/your-ontology.types.ts

# Run type checker manually
tsc --noEmit --project generated/tsconfig.json

# Fix template if needed
vim docs/examples/templates/types.ts.hbs
```

**4. Slow hook execution**
```bash
# Profile the regeneration script
time ./docs/examples/scripts/regenerate-from-ontology.sh --verbose

# Optimize by:
# - Reducing number of templates
# - Skipping type checking
# - Using --fast validation
```

### Debug Mode

Run hooks manually with verbose output:

```bash
# Test pre-commit hook
bash -x .git/hooks/pre-commit

# Test regeneration script
./docs/examples/scripts/regenerate-from-ontology.sh --verbose

# Test with specific ontology
ggen graph validate --verbose --file docs/examples/ontology/task-management.ttl
```

## CI/CD Integration

### GitHub Actions

```yaml
# .github/workflows/validate-ontology.yml
name: Validate Ontology

on:
  pull_request:
    paths:
      - 'docs/examples/ontology/**/*.ttl'
      - 'docs/examples/ontology/**/*.rdf'

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      - name: Install ggen
        run: cargo install --path .

      - name: Run regeneration
        run: ./docs/examples/scripts/regenerate-from-ontology.sh

      - name: Check for uncommitted changes
        run: |
          if ! git diff --exit-code generated/; then
            echo "::error::Generated code is out of sync with ontology"
            echo "Run ./docs/examples/scripts/regenerate-from-ontology.sh locally"
            exit 1
          fi

      - name: Run TypeScript type check
        run: |
          npm install
          npm run typecheck
```

### GitLab CI

```yaml
# .gitlab-ci.yml
validate-ontology:
  stage: test
  only:
    changes:
      - docs/examples/ontology/**/*.ttl
      - docs/examples/ontology/**/*.rdf
  script:
    - cargo install --path .
    - ./docs/examples/scripts/regenerate-from-ontology.sh
    - git diff --exit-code generated/ || exit 1
```

## Best Practices

1. **Always review generated code** - Don't blindly commit
2. **Keep templates simple** - Easier to maintain and debug
3. **Test templates** - Ensure they produce valid code
4. **Version control templates** - Track template changes
5. **Document ontology changes** - Use clear commit messages
6. **Run hooks locally first** - Before pushing to CI/CD
7. **Keep ontologies small** - Better performance
8. **Use semantic versioning** - For ontology changes

## Performance Optimization

### Caching Strategies

```bash
# Cache ggen graph loads
GRAPH_CACHE_DIR="${HOME}/.cache/ggen/graphs"
mkdir -p "$GRAPH_CACHE_DIR"

# Check cache before loading
CACHE_KEY=$(md5sum "$ONTOLOGY_FILE" | awk '{print $1}')
if [ -f "$GRAPH_CACHE_DIR/$CACHE_KEY" ]; then
  # Use cached graph
  cp "$GRAPH_CACHE_DIR/$CACHE_KEY" /tmp/graph.db
else
  # Load and cache
  ggen graph load --graph "$GRAPH_NAME" --file "$ONTOLOGY_FILE"
  cp /tmp/graph.db "$GRAPH_CACHE_DIR/$CACHE_KEY"
fi
```

### Parallel Processing

```bash
# Generate files in parallel
for ONTOLOGY_FILE in $ONTOLOGY_FILES; do
  (
    # Process in subshell for parallelization
    generate_code "$ONTOLOGY_FILE"
  ) &
done
wait
```

## Migration Guide

### From Manual Regeneration

```bash
# Before: Manual workflow
vim ontology.ttl
ggen graph validate --file ontology.ttl
ggen template generate-rdf --ontology ontology.ttl --template types.ts.hbs
git add ontology.ttl generated/types.ts
git commit

# After: Automatic workflow
vim ontology.ttl
git add ontology.ttl
git commit  # ← Hook handles everything
```

### From Other Tools

**If using TypeORM:**
```typescript
// Keep existing TypeORM entities
// Add RDF ontology alongside
// Generate DTOs from RDF
// Map between TypeORM and RDF types
```

**If using GraphQL:**
```typescript
// Keep existing GraphQL schema
// Generate GraphQL types from RDF
// Use ontology as source of truth
```

## FAQ

**Q: Do hooks slow down commits?**
A: Typically 3-5s overhead. Use `--no-verify` to bypass if needed.

**Q: What if I don't want auto-generation?**
A: Remove hooks or use `git commit --no-verify`.

**Q: Can I customize the templates?**
A: Yes! Edit files in `docs/examples/templates/`.

**Q: How do I handle conflicts?**
A: Resolve ontology conflicts first, then regenerate.

**Q: What about monorepos?**
A: Configure paths per package in regeneration script.

## Resources

- [RDF Primer](https://www.w3.org/TR/rdf11-primer/)
- [Turtle Syntax](https://www.w3.org/TR/turtle/)
- [ggen Documentation](../../README.md)
- [Handlebars Templates](https://handlebarsjs.com/)
- [Git Hooks](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks)

## License

Same as parent project (see root LICENSE file).
