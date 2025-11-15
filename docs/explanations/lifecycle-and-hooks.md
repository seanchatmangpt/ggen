# Understanding Lifecycle and Hooks

## What are Lifecycle Hooks?

Lifecycle hooks are automated actions that execute at specific points in ggen's workflow. They allow you to integrate quality checks, validation, and custom logic into your development process without manual intervention.

Think of hooks as "if this happens, then do that":
- **If** code is about to be committed → **then** validate the ontology
- **If** ontology changes → **then** regenerate code
- **If** template is used → **then** apply code formatting rules

## Why Hooks Matter

### Without Hooks: Manual Workflow
```
1. Edit ontology
2. Remember to run validation
3. Generate code manually
4. Run tests manually
5. Commit code
6. Watch for inconsistencies
```

Problems:
- Easy to forget steps
- Time-consuming repetition
- Human error (missing validation)
- Code drift between languages
- Slow feedback loop

### With Hooks: Automated Workflow
```
1. Edit ontology
2. [Hook triggers]
   → Validates ontology automatically
   → Regenerates code automatically
   → Runs tests automatically
   → Updates code formatting
3. Commit happens (or fails if validation fails)
```

Benefits:
- **Consistency:** Same checks always run
- **Speed:** No manual command execution
- **Safety:** Can't skip validation
- **Feedback:** Immediate error detection
- **Documentation:** Clear process definition

## Hook Types and Timing

### Commit Hooks (Pre-Commit)

Runs **before** code is committed to git:

```
git commit
  ↓
Pre-commit hooks run
  ├─ Validate ontology
  ├─ Check for panic points
  ├─ Lint code
  ├─ Run tests
  ↓
If all pass: commit succeeds
If any fail: commit blocked (fix issues then retry)
```

**Example Use Cases:**
- Prevent invalid ontologies from being committed
- Block commits with unresolved panic points
- Ensure code formatting before commit
- Validate SHACL constraints

**Command:**
```bash
ggen hook create pre-commit --name validate-ontology
```

### Post-Merge Hooks

Runs **after** code is merged into main:

```
git merge
  ↓
Post-merge hooks run
  ├─ Regenerate code from merged ontology
  ├─ Update documentation
  ├─ Run full test suite
  ├─ Update dependencies
  ↓
Merge complete with all updates
```

**Example Use Cases:**
- Automatically regenerate code after merge conflicts resolve
- Update cross-language code when ontology merges
- Synchronize generated files

### Lifecycle Workflows (Custom Hooks)

User-defined sequences that run based on triggers:

```bash
# Define a custom lifecycle
ggen lifecycle create validate-and-deploy \
  --steps validate-ontology \
  --steps generate-code \
  --steps run-tests \
  --steps deploy

# Run it manually
ggen lifecycle run validate-and-deploy

# Or make it automatic with hooks
ggen hook create post-commit --run validate-and-deploy
```

## Hook Execution Model

### Sequential Execution

Hooks run in a defined order. Each hook must complete before the next starts:

```
Hook 1: Validate ontology
  ↓ (passes or fails)
Hook 2: Generate code
  ↓ (passes or fails)
Hook 3: Format code
  ↓ (passes or fails)
Hook 4: Run tests
  ↓ (final result)
```

If any hook fails, the sequence stops and the command (e.g., commit) is blocked.

### Error Handling

Hooks can be configured as:
- **Required:** Failure blocks the operation
- **Optional:** Failure is logged but doesn't block
- **Warning:** Failure is logged as a warning

```yaml
hooks:
  - name: validate-ontology
    required: true      # Block commit if fails
  - name: format-code
    required: false     # Just warn if fails
  - name: lint
    required: true      # Block commit if fails
```

## Hook Scope and Isolation

### What Hooks Can Access

Hooks have access to:
- Files being committed (for pre-commit)
- Environment variables
- Git context (branch, commit message, etc.)
- ggen configuration

### What Hooks Cannot Do

Hooks cannot:
- Access files outside the repository
- Make arbitrary network requests
- Modify system configuration
- Run with elevated privileges

This ensures **security**: hooks are sandboxed and cannot break your system.

## Hook Composition: Building Complex Workflows

Simple hooks compose into complex workflows:

### Example: Ontology Evolution Workflow

```
1. Developer edits domain.ttl
2. Pre-commit hook triggers:
   a. Validate ontology (SHACL)
   b. Check backward compatibility
   c. Generate code for all languages
   d. Run polyglot test suite
   e. Format generated code
3. If all pass: commit succeeds
4. If any fail: Developer sees detailed error, fixes, and retries
```

### Example: Production Deployment Workflow

```
1. Code is merged to main
2. Post-merge hook triggers:
   a. Run full test suite
   b. Check performance benchmarks
   c. Generate deployment artifacts
   d. Create release notes
   e. Update documentation
3. If all pass: deployment ready
4. If any fail: Maintainer reviews and decides
```

## Integration with Git Workflow

### Feature Branch Development

```
git checkout -b feature/new-entity

# Developer works on feature
edit domain.ttl
ggen regenerate

# Every commit validates
git commit -m "Add Product entity"
  → Pre-commit hook validates domain.ttl
  → Regenerates code
  → Runs tests

# Ready to push
git push -u origin feature/new-entity
```

### Merge Conflicts in Ontologies

```
git merge other-branch

# Conflict in domain.ttl detected
# Manual merge required

# After resolving conflict
git add domain.ttl

# Post-merge hook triggers
  → Validates merged ontology
  → Regenerates code from merged ontology
  → Ensures both branches' changes are included
  → All languages stay in sync

git commit
```

## Hook Configuration

### Default Hook Locations

Hooks are configured in:
```
.ggen/hooks/
├── pre-commit/
├── post-commit/
├── post-merge/
└── lifecycle/
```

### Enabling/Disabling Hooks

```bash
# Enable hook
ggen hook enable pre-commit validate-ontology

# Disable hook (e.g., during emergency)
ggen hook disable pre-commit validate-ontology

# List all hooks
ggen hook list

# View hook details
ggen hook show pre-commit validate-ontology

# Remove hook
ggen hook remove pre-commit validate-ontology
```

## Advanced: Custom Hook Scripts

While ggen provides pre-built hooks, you can create custom ones:

```bash
# Create custom hook script
ggen hook create pre-commit \
  --name my-custom-check \
  --script ./scripts/my-check.sh
```

Hook script (`./scripts/my-check.sh`):
```bash
#!/bin/bash
set -e

# Your custom validation logic
echo "Running my custom check..."

# Check if domain.ttl exists
if [ ! -f domain.ttl ]; then
  echo "ERROR: domain.ttl not found"
  exit 1
fi

# Check file size
size=$(wc -c < domain.ttl)
if [ $size -gt 1000000 ]; then
  echo "WARNING: Ontology is large ($(($size/1024))KB)"
fi

echo "Custom check passed!"
exit 0
```

## Troubleshooting: Hook Issues

### Hook Runs But Doesn't Validate Correctly

**Problem:** Hook executes but misses errors

**Solution:**
1. Check hook configuration: `ggen hook show <hook-name>`
2. Run hook manually: `ggen <hook-command>`
3. Review hook output for details

### Hook is Too Slow

**Problem:** Hook takes too long, slowing down development

**Solutions:**
1. Run only critical checks in pre-commit
2. Move heavy validation to post-merge
3. Parallelize independent checks
4. Use `--fast` flag for quick validation

```bash
# Fast validation (skip some checks)
ggen hook create pre-commit \
  --name validate-ontology-fast \
  --fast
```

### Hook Never Runs

**Problem:** Hook is installed but never executes

**Solutions:**
1. Check if hook is enabled: `ggen hook list`
2. Verify git hook is installed: `ls .git/hooks/`
3. Check hook permissions: `chmod +x .git/hooks/pre-commit`
4. Test manually: `ggen validate domain.ttl`

## Hook Philosophy

Hooks embody ggen's philosophy:

1. **Automation Over Manual:** Computers should check, not humans
2. **Fast Feedback:** Errors caught immediately, not in CI/CD
3. **Consistency:** Same checks, every time
4. **Fail Fast:** Stop early, fix immediately
5. **Zero Friction:** Developers never think about it

As you become familiar with ggen, hooks become invisible infrastructure—they just work.

## See Also

- [Configure Hooks Guide](../how-to-guides/configure-hooks.md) - How to set up hooks
- [Architecture Explanation](architecture.md) - System design overview
- [Ontology-Driven Development](ontology-driven.md) - Why this matters
