# Ggen Dogfooding Guide: Using Ggen to Fix Ggen

## 🎯 Philosophy: Eating Our Own Dog Food

**Core Principle:** Use ggen's own marketplace, lifecycle, templates, and AI tools to solve ggen's problems.

This ensures:
- **Tools work in practice** - We experience what users experience
- **Quality feedback loop** - Issues get fixed faster
- **Credibility** - We trust our own tools for production
- **Innovation** - We push boundaries by using our own features

## 🚨 The Problem: 403 Panic Points

**Current State (October 2025):**
- ❌ 72 `.expect()` calls in production code
- ❌ 331 `.unwrap()` calls in production code
- ❌ Total: 403 potential crash points

**Solution:** Use ggen's own tools to fix this systematically.

## 🛠️ Dogfooding Tools Created

### 1. Automatic Panic Point Fixer
**Location:** `scripts/fix-panic-points.rs`

**What it does:**
- Scans Rust code for `.expect()` and `.unwrap()` calls
- Automatically replaces with safe error handling
- Generates proper `Result<T>` patterns
- Preserves safe patterns (`.unwrap_or()`, `.unwrap_or_else()`)

**Usage:**
```bash
# Dry run (see what would be fixed)
cargo script scripts/fix-panic-points.rs --dry-run

# Fix all panic points
cargo script scripts/fix-panic-points.rs

# Fix specific directories
cargo script scripts/fix-panic-points.rs cli/src ggen-core/src
```

### 2. Safe Error Handling Template
**Location:** `templates/safe-error-handling.tmpl`

**What it does:**
- Generates safe error handling patterns
- Provides template for production-ready code
- Includes common safe patterns library

**Usage:**
```bash
# Generate safe error handling module
ggen template generate templates/safe-error-handling.tmpl \
  --output src/safe_error_handling.rs

# Generate with custom function
ggen template generate templates/safe-error-handling.tmpl \
  --vars function_name=load_config \
  --vars return_type=Config \
  --output src/config_loader.rs
```

### 3. Pre-Commit Hook
**Location:** `.githooks/pre-commit`

**What it does:**
- Blocks commits containing panic points
- Provides fix suggestions
- Enforces production safety standards

**Setup:**
```bash
# Install hooks (run once)
ggen lifecycle run setup-git-hooks

# Or manually
mkdir -p .git/hooks
cp .githooks/pre-commit .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit
```

### 4. Panic Point Detection Script
**Location:** `scripts/check-no-panic-points.sh`

**What it does:**
- Scans production code for panic points
- Reports detailed statistics
- Exits with error if panic points found

**Usage:**
```bash
# Check entire codebase
./scripts/check-no-panic-points.sh

# Integrate with CI/CD
ggen lifecycle run validate-safety
```

### 5. Production Lifecycle
**Location:** `make.toml`

**What it provides:**
- Automated safety validation
- Integrated testing pipeline
- Dogfooding phases

**Usage:**
```bash
# Run safety validation
ggen lifecycle run validate-safety

# Run full production pipeline
ggen lifecycle run production-validate

# Use dogfooding pipeline
ggen lifecycle run dogfood
```

## 📋 Complete Dogfooding Workflow

### Step 1: Identify Problems
```bash
# Scan for panic points
./scripts/check-no-panic-points.sh

# Output shows:
# 🔍 Scanning for panic points in production code...
# Results:
#   .expect() calls: 72
#   .unwrap() calls: 331
#   Total panic points: 403
```

### Step 2: Analyze Impact
```bash
# See which files have most panic points
./scripts/check-no-panic-points.sh | grep "Top 10"

# Top 10 offenders:
# cli/src/cmds/remove.rs:58
# ggen-core/src/lifecycle/dag.rs:57
# ggen-core/src/lifecycle/exec.rs:142
# ...
```

### Step 3: Fix Automatically
```bash
# Preview fixes (dry run)
cargo script scripts/fix-panic-points.rs --dry-run --verbose

# Apply fixes to critical paths first (80/20 rule)
cargo script scripts/fix-panic-points.rs cli/src/cmds ggen-core/src/lifecycle

# Verify fixes
./scripts/check-no-panic-points.sh

# Build and test
cargo build --release
cargo test --all-features
```

### Step 4: Use Templates for New Code
```bash
# Generate safe error handling for new module
ggen template generate templates/safe-error-handling.tmpl \
  --vars function_name=process_marketplace_package \
  --vars return_type=Package \
  --vars parameters='[{"name":"path","type":"&Path"},{"name":"validate","type":"bool"}]' \
  --output src/marketplace/safe_package_loader.rs
```

### Step 5: Prevent Future Panic Points
```bash
# Install pre-commit hooks
ggen lifecycle run setup-git-hooks

# Try to commit code with panic points
echo 'fn bad() { Some(1).unwrap(); }' > test.rs
git add test.rs
git commit -m "test"

# Output:
# ❌ COMMIT BLOCKED: Found 1 panic point(s)
# Production code must not contain .expect() or .unwrap() calls.
```

### Step 6: Validate Production Readiness
```bash
# Run complete validation
ggen lifecycle run production-validate

# Checks:
# ✅ No panic points
# ✅ Clippy warnings = 0
# ✅ Tests passing
# ✅ Security audit clean
```

## 🎯 80/20 Dogfooding Strategy

### Phase 1: Critical Safety (Week 1) - 80% Impact

**Focus:** Fix 20% of files with 80% of panic points

```bash
# 1. Identify critical paths
./scripts/check-no-panic-points.sh > panic-report.txt
grep -A 5 "Top 10 offenders" panic-report.txt

# 2. Fix critical files first
cargo script scripts/fix-panic-points.rs \
  cli/src/main.rs \
  cli/src/cmds/mod.rs \
  ggen-core/src/lifecycle/exec.rs \
  ggen-core/src/template.rs

# 3. Validate
./scripts/check-no-panic-points.sh
cargo test --all-features
```

### Phase 2: Automation (Week 2) - 15% Impact

**Focus:** Prevent future panic points

```bash
# 1. Install pre-commit hooks
ggen lifecycle run setup-git-hooks

# 2. Add lifecycle validation
ggen lifecycle run validate-safety

# 3. Update CI/CD
cat >> .github/workflows/ci.yml <<EOF
      - name: Check Panic Points
        run: ./scripts/check-no-panic-points.sh
EOF
```

### Phase 3: Templates (Week 3) - 4% Impact

**Focus:** Generate safe code patterns

```bash
# 1. Use safe error handling template for new code
ggen template generate templates/safe-error-handling.tmpl

# 2. Create project-specific templates
ggen ai generate \
  --description "Safe marketplace package loader" \
  --output templates/marketplace-loader.tmpl

# 3. Document patterns
ggen template generate templates/safe-error-handling.tmpl \
  --output docs/SAFE_PATTERNS.md
```

### Phase 4: Continuous Improvement (Week 4) - 1% Impact

**Focus:** Monitor and improve

```bash
# 1. Regular checks
ggen lifecycle run dogfood

# 2. Metrics tracking
./scripts/check-no-panic-points.sh | tee metrics/panic-points-$(date +%Y%m%d).txt

# 3. Update templates based on learnings
ggen ai generate \
  --description "Update safe error handling template based on common patterns"
```

## 📊 Success Metrics

### Before Dogfooding
- ❌ 403 panic points
- ❌ No automated validation
- ❌ Manual code review required
- ❌ Inconsistent error handling

### After Dogfooding (Target)
- ✅ 0-5 panic points (with SAFE comments)
- ✅ Automated validation in CI/CD
- ✅ Pre-commit hooks prevent new panic points
- ✅ Consistent safe error handling patterns
- ✅ Templates generate safe code by default

## 🔄 Continuous Dogfooding Practices

### Daily Development
```bash
# Before starting work
ggen lifecycle run validate-safety

# During development (use templates)
ggen template generate templates/safe-error-handling.tmpl

# Before committing (automatic via hook)
git commit -m "feat: add marketplace sync"
# → Pre-commit hook validates automatically

# After commit
ggen lifecycle run test
```

### Weekly Reviews
```bash
# Check progress
./scripts/check-no-panic-points.sh > weekly-report.txt

# Compare with previous week
diff weekly-report.txt previous-week-report.txt

# Update templates if patterns emerge
ggen ai generate \
  --description "Update templates based on weekly patterns"
```

### Monthly Improvements
```bash
# Full dogfooding pipeline
ggen lifecycle run dogfood

# Generate improvements
ggen ai generate \
  --description "Suggest improvements to panic point prevention"

# Update tooling
cargo script scripts/fix-panic-points.rs --update-patterns
```

## 🚀 Advanced Dogfooding Techniques

### 1. AI-Powered Pattern Learning
```bash
# Analyze existing safe patterns
ggen ai analyze \
  --input "src/**/*.rs" \
  --filter "safe error handling" \
  --output patterns/learned-safe-patterns.json

# Generate template from learned patterns
ggen template generate \
  --from-patterns patterns/learned-safe-patterns.json \
  --output templates/learned-safe-handling.tmpl
```

### 2. SPARQL-Driven Code Analysis
```bash
# Create knowledge graph of code structure
ggen graph analyze src/ --output code-graph.ttl

# Query for panic-prone patterns
ggen graph query \
  --sparql "SELECT ?function WHERE { ?function :hasPanicPoint true }" \
  --graph code-graph.ttl

# Generate fixes using graph data
ggen ai generate \
  --from-graph code-graph.ttl \
  --description "Generate safe alternatives for panic-prone functions"
```

### 3. Marketplace Integration
```bash
# Package safe error handling as marketplace item
ggen market package \
  --name "safe-error-handling" \
  --templates templates/safe-error-handling.tmpl \
  --scripts scripts/fix-panic-points.rs \
  --output marketplace/packages/safe-error-handling

# Publish to marketplace
ggen market publish safe-error-handling

# Install in other projects
ggen market add safe-error-handling
```

## 🎓 Learning From Dogfooding

### What We Learned

1. **Automation is Critical**
   - Manual code review misses panic points
   - Pre-commit hooks catch issues immediately
   - Automated fixers save hours of work

2. **Templates Ensure Consistency**
   - Developers copy-paste existing patterns
   - Templates encode best practices
   - AI generates safe code from templates

3. **Lifecycle Integration Works**
   - Validation becomes automatic
   - No extra steps for developers
   - Continuous assurance of quality

4. **80/20 Rule Applies**
   - 20% of files have 80% of panic points
   - Fixing critical paths first = fast progress
   - Templates prevent issues in new code

### Best Practices Discovered

1. **Use `?` operator instead of `.expect()`**
2. **Use `.unwrap_or_default()` for Options**
3. **Add context with `.with_context()`**
4. **Document truly safe `.unwrap()` with `// SAFE:`**
5. **Generate safe patterns with templates**

## 📚 Resources

### Documentation
- `docs/PRODUCTION_READINESS_8020.md` - Production checklist
- `docs/DOGFOODING_EXAMPLES.md` - More examples
- `templates/safe-error-handling.tmpl` - Safe patterns template

### Scripts
- `scripts/fix-panic-points.rs` - Automatic fixer
- `scripts/check-no-panic-points.sh` - Validation script
- `.githooks/pre-commit` - Git hook for prevention

### Lifecycle
- `make.toml` - Complete lifecycle configuration
- Run `ggen lifecycle list` to see all phases
- Run `ggen lifecycle run dogfood` for complete workflow

## 🔥 Quick Start

**5-Minute Dogfooding Setup:**

```bash
# 1. Install hooks
ggen lifecycle run setup-git-hooks

# 2. Scan current state
./scripts/check-no-panic-points.sh

# 3. Fix critical paths
cargo script scripts/fix-panic-points.rs cli/src

# 4. Validate
ggen lifecycle run validate-safety

# 5. Done! Panic points prevented automatically
```

---

**Remember: The best way to prove ggen works is to use it to make ggen better.**

## 🎯 Next Steps

1. Run `./scripts/check-no-panic-points.sh` to see current state
2. Fix critical paths with `cargo script scripts/fix-panic-points.rs`
3. Install hooks with `ggen lifecycle run setup-git-hooks`
4. Use templates for new code
5. Monitor progress weekly
6. Share improvements via marketplace

**Target: 0 panic points in production code by Q1 2026**
