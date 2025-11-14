# Git Hooks for Ontology-Driven Code Generation

Automatic code regeneration hooks that maintain consistency between RDF ontologies and generated TypeScript code.

## Overview

These hooks ensure that whenever ontology files (`.ttl`, `.rdf`, `.owl`) are modified, the corresponding TypeScript types, API routes, and CRUD components are automatically regenerated.

## Available Hooks

### 1. `pre-commit.sh` - Validate & Regenerate Before Commit

**What it does:**
- ✅ Validates ontology files before commit
- ✅ Regenerates TypeScript code if ontologies changed
- ✅ Automatically stages generated files
- ✅ Fails commit if validation errors exist
- ⚡ Fast (<5s typical execution)

**Installation:**
```bash
cp docs/examples/hooks/pre-commit.sh .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit
```

**Example workflow:**
```bash
# Modify ontology
vim docs/examples/ontology/task-management.ttl

# Stage changes
git add docs/examples/ontology/task-management.ttl

# Commit (hook runs automatically)
git commit -m "feat: add priority field to Task entity"
# → Hook validates ontology
# → Hook regenerates types, API routes, components
# → Hook stages generated files
# → Commit includes both ontology and generated code
```

### 2. `post-merge.sh` - Regenerate After Merge

**What it does:**
- 🔍 Detects ontology changes in merged commits
- 🔄 Regenerates all derived code
- 📢 Notifies developer of uncommitted changes
- 🛡️ Safe (only runs if ontologies changed)

**Installation:**
```bash
cp docs/examples/hooks/post-merge.sh .git/hooks/post-merge
chmod +x .git/hooks/post-merge
```

**Example workflow:**
```bash
# Pull changes from remote
git pull origin main
# → Hook detects ontology changes in merge
# → Hook regenerates code
# → Hook reports any uncommitted changes

# Review and commit generated changes
git status
git diff
git add generated/
git commit -m "chore: regenerate code from ontology changes"
```

## Regeneration Script

### `regenerate-from-ontology.sh`

Core script that handles the actual code generation.

**Usage:**
```bash
# Normal output
./docs/examples/scripts/regenerate-from-ontology.sh

# Quiet mode (minimal output)
./docs/examples/scripts/regenerate-from-ontology.sh --quiet

# Verbose mode (detailed logging)
./docs/examples/scripts/regenerate-from-ontology.sh --verbose
```

**What it does:**

1. **Validates ontologies** with `ggen graph validate`
2. **Loads into graph store** with `ggen graph load`
3. **Generates TypeScript types** from ontology
4. **Generates API routes** for each entity
5. **Generates CRUD components** for each entity
6. **Runs type checking** to catch errors
7. **Reports changes** to developer

**Configuration:**

Edit paths in the script:
```bash
ONTOLOGY_DIR="${PROJECT_ROOT}/docs/examples/ontology"
OUTPUT_DIR="${PROJECT_ROOT}/generated"
TEMPLATES_DIR="${PROJECT_ROOT}/docs/examples/templates"
```

## Template Structure

The regeneration script expects these templates:

```
docs/examples/templates/
├── types.ts.hbs           # TypeScript type definitions
├── api-routes.ts.hbs      # Express/NestJS API routes
└── crud-component.tsx.hbs # React CRUD components
```

See `../templates/README.md` for template documentation.

## Hook Behavior

### Pre-Commit Hook

**Triggers when:** Ontology files (`.ttl`, `.rdf`, `.owl`) are staged

**Success path:**
```
1. Detect staged .ttl files ✓
2. Validate ontologies ✓
3. Run regeneration script ✓
4. Stage generated files ✓
5. Commit proceeds ✓
```

**Failure path:**
```
1. Detect staged .ttl files ✓
2. Validate ontologies ✗
3. Show validation errors
4. Block commit
```

**Performance:** Typically <5s for small ontologies

### Post-Merge Hook

**Triggers when:** Git merge/pull completes

**Success path:**
```
1. Detect ontology changes in merge ✓
2. Run regeneration script ✓
3. Report uncommitted changes ✓
4. Developer reviews and commits ✓
```

**Failure path:**
```
1. Detect ontology changes ✓
2. Run regeneration script ✗
3. Report errors
4. Developer fixes manually
```

## Safety Features

### Idempotency
- ✅ Can run multiple times safely
- ✅ Only regenerates what changed
- ✅ Doesn't corrupt existing code

### Validation
- ✅ Validates ontologies before regeneration
- ✅ Runs TypeScript type checking after generation
- ✅ Fails fast on errors

### Informative Output
```
[pre-commit] Checking for ontology changes...
[pre-commit] Ontology changes detected:
  - docs/examples/ontology/task-management.ttl
[pre-commit] Validating ontologies...
[pre-commit]   ✓ task-management.ttl is valid
[pre-commit] Running code regeneration...
[pre-commit] Generated/modified files:
  - generated/types/task-management.types.ts
  - generated/api/task-management.routes.ts
  - generated/components/task-management.component.tsx
[pre-commit] ✓ Code regeneration completed in 3s
```

## Troubleshooting

### Hook not running

```bash
# Check if hook is executable
ls -la .git/hooks/pre-commit

# Make executable if needed
chmod +x .git/hooks/pre-commit
```

### Regeneration script not found

```bash
# Check script path in hook
cat .git/hooks/pre-commit | grep REGEN_SCRIPT

# Ensure script exists and is executable
ls -la docs/examples/scripts/regenerate-from-ontology.sh
chmod +x docs/examples/scripts/regenerate-from-ontology.sh
```

### Validation errors

```bash
# Run validation manually
ggen graph validate --file docs/examples/ontology/task-management.ttl

# Check ontology syntax
rapper -i turtle docs/examples/ontology/task-management.ttl
```

### TypeScript errors after regeneration

```bash
# Run type checking manually
tsc --noEmit --project generated/tsconfig.json

# Check generated code
cat generated/types/task-management.types.ts
```

## Disabling Hooks Temporarily

```bash
# Skip pre-commit hook for one commit
git commit --no-verify -m "wip: work in progress"

# Disable hook temporarily
chmod -x .git/hooks/pre-commit

# Re-enable later
chmod +x .git/hooks/pre-commit
```

## Best Practices

1. **Commit ontology and generated code together** - This keeps them in sync
2. **Review generated code** - Don't blindly commit, check diffs
3. **Run regeneration manually when needed** - Don't rely only on hooks
4. **Keep templates up to date** - Update templates when patterns change
5. **Test generated code** - Ensure it compiles and passes tests

## Examples

### Adding a new entity to ontology

```bash
# 1. Edit ontology
vim docs/examples/ontology/task-management.ttl

# Add new entity
ex:Comment a rdfs:Class ;
    rdfs:label "Comment" ;
    rdfs:comment "User comment on a task" .

# 2. Stage and commit
git add docs/examples/ontology/task-management.ttl
git commit -m "feat: add Comment entity"
# → Hook validates ontology ✓
# → Hook generates Comment types, routes, components ✓
# → Hook stages generated files ✓
# → Commit includes everything ✓
```

### Pulling changes with ontology updates

```bash
# 1. Pull from remote
git pull origin main
# → Post-merge hook detects ontology changes ✓
# → Hook regenerates all code ✓
# → Hook reports uncommitted changes

# 2. Review changes
git status
git diff generated/

# 3. Commit if needed
git add generated/
git commit -m "chore: regenerate from ontology updates"
```

## Performance Benchmarks

Typical execution times:

| Ontology Size | Entities | Pre-commit | Post-merge |
|---------------|----------|------------|------------|
| Small         | 1-5      | <3s        | <5s        |
| Medium        | 6-20     | <5s        | <8s        |
| Large         | 21+      | <10s       | <15s       |

## Integration with CI/CD

```yaml
# .github/workflows/validate-ontology.yml
name: Validate Ontology

on: [pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Install ggen
        run: cargo install --path .
      - name: Run regeneration
        run: ./docs/examples/scripts/regenerate-from-ontology.sh
      - name: Check for uncommitted changes
        run: |
          git diff --exit-code || \
          (echo "Generated code out of sync with ontology" && exit 1)
```

## License

Same as parent project (see root LICENSE file).
