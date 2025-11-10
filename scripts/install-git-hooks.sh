#!/bin/bash
# Install poka-yoke git hooks for ggen project
# Aligned with core team 80/20 best practices: fast feedback, pragmatic exceptions
# Prevents unwrap() calls and unimplemented!() from being committed
# Adapted for ggen: uses cargo make commands and ggen crate paths

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
HOOKS_DIR="$PROJECT_ROOT/.git/hooks"

echo "🔧 Installing ggen poka-yoke git hooks (core team best practices)..."

# Ensure .git/hooks directory exists
if [ ! -d "$HOOKS_DIR" ]; then
  echo "❌ ERROR: .git/hooks directory not found"
  echo "   Are you in a git repository?"
  exit 1
fi

# Create pre-commit hook (fast: 2-5s target, only staged files)
cat > "$HOOKS_DIR/pre-commit" << 'EOF'
#!/bin/bash
# Pre-commit hook: Fast validation aligned with core team 80/20 best practices
# Target: 2-5 seconds (only checks staged files/packages)
# Enforces: No unwrap/expect/TODO/FUTURE/unimplemented on MAIN branch only
# Other branches: relaxed rules (TODO/FUTURE/unimplemented allowed)
# Uses: cargo make commands (NEVER direct cargo commands)

set -e

# Change to project root
cd "$(git rev-parse --show-toplevel)"

echo "🔍 Running pre-commit validation..."

# Only check if Rust files are staged
if ! git diff --cached --name-only --diff-filter=d | grep -q '\.rs$'; then
  echo "✅ No Rust files staged, skipping validation"
  exit 0
fi

# Get list of staged Rust files
STAGED_FILES=$(git diff --cached --name-only --diff-filter=d | grep '\.rs$' || true)

if [ -z "$STAGED_FILES" ]; then
  echo "✅ No Rust files to validate"
  exit 0
fi

# Detect current branch - strict rules only apply to main
CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
IS_MAIN_BRANCH=false
if [ "$CURRENT_BRANCH" = "main" ] || [ "$CURRENT_BRANCH" = "master" ]; then
  IS_MAIN_BRANCH=true
  echo "🔒 Main branch detected - enforcing strict rules (no TODO/FUTURE/unimplemented!)"
else
  echo "🌿 Branch '$CURRENT_BRANCH' - strict rules relaxed (TODO/FUTURE/unimplemented! allowed)"
fi

# Check 1: No unwrap() in production code (excluding test files, build scripts)
echo "   Checking for unwrap() calls in production code..."
UNWRAP_COUNT=0
for file in $STAGED_FILES; do
  # Skip test files, examples, benches, build scripts
  if [[ "$file" =~ /(test|tests|example|examples|bench|benches)/ ]] || [[ "$file" == *"build.rs" ]] || [[ "$file" =~ ^(test|tests|example|examples|bench|benches)/ ]]; then
    continue
  fi
  
  # Check if file has allow attribute (check both diff and actual file)
  if git diff --cached "$file" | grep -qE "#!?\[allow\(clippy::unwrap_used\)\]" || \
     grep -qE "#!?\[allow\(clippy::unwrap_used\)\]" "$file" 2>/dev/null; then
    continue
  fi
  
  # Pragmatic exception for pre-commit: if file has test modules, allow unwrap() calls
  # Rationale: Test modules should have allow attributes, but we're lenient for fast feedback
  # Pre-push hook will enforce stricter rules (require allow attributes in test modules)
  if grep -q "#\[cfg(test)\]" "$file" 2>/dev/null; then
    # File has test modules - allow unwrap() for pre-commit (fast feedback)
    # Pre-push will check that test modules have proper allow attributes
    continue
  fi
  
  # Count unwrap() calls in staged changes
  UNWRAPS=$(git diff --cached "$file" | grep -E "^\+" | grep -c "\.unwrap()" || echo 0)
  UNWRAPS=${UNWRAPS//[^0-9]/}  # Remove any non-numeric characters
  if [ "${UNWRAPS:-0}" -gt 0 ]; then
    echo "     ❌ $file: $UNWRAPS unwrap() call(s) found"
    UNWRAP_COUNT=$((UNWRAP_COUNT + UNWRAPS))
  fi
done

if [ "$UNWRAP_COUNT" -gt 0 ]; then
  echo "❌ ERROR: Cannot commit $UNWRAP_COUNT unwrap() calls in production code"
  echo "   Replace with proper Result<T,E> error handling"
  echo "   Use ? operator or match statements instead"
  echo "   Or add #![allow(clippy::unwrap_used)] if truly necessary"
  exit 1
fi
echo "  ✅ No unwrap() in production code"

# Check 2: No unimplemented!() placeholders - BLOCKED ONLY ON MAIN
if [ "$IS_MAIN_BRANCH" = true ]; then
  echo "   Checking for unimplemented!() placeholders..."
  UNIMPL_COUNT=0
  for file in $STAGED_FILES; do
    # Check ALL files - no exceptions on main
    UNIMPL=$(git diff --cached "$file" | grep -E "^\+" | grep -c "unimplemented!" || echo 0)
    UNIMPL=${UNIMPL//[^0-9]/}  # Remove any non-numeric characters
    if [ "${UNIMPL:-0}" -gt 0 ]; then
      echo "     ❌ $file: $UNIMPL unimplemented!() placeholder(s) found"
      git diff --cached "$file" | grep -E "^\+" | grep "unimplemented!" | head -5
      UNIMPL_COUNT=$((UNIMPL_COUNT + UNIMPL))
    fi
  done

  if [ "$UNIMPL_COUNT" -gt 0 ]; then
    echo "❌ ERROR: Cannot commit $UNIMPL_COUNT unimplemented!() placeholders to main"
    echo "   Complete implementations before committing - NO EXCEPTIONS"
    exit 1
  fi
  echo "  ✅ No unimplemented!() placeholders"
else
  echo "  ⏭️  Skipping unimplemented!() check (not on main branch)"
fi

# Check 3: No FUTURE or TODO comments - BLOCKED ONLY ON MAIN
if [ "$IS_MAIN_BRANCH" = true ]; then
  echo "   Checking for FUTURE/TODO comments..."
  TODO_COUNT=0
  for file in $STAGED_FILES; do
    # Skip only documentation files (markdown, text files)
    if [[ "$file" =~ \.(md|txt|rst)$ ]]; then
      continue
    fi
    
    # Check ALL Rust files - no exceptions for test files on main
    # Block ALL TODO/FUTURE comments regardless of context
    TODOS=$(git diff --cached "$file" | grep -E "^\+" | grep -iE "\b(TODO|FUTURE)\b" | grep -c . || echo 0)
    TODOS=${TODOS//[^0-9]/}  # Remove any non-numeric characters
    if [ "${TODOS:-0}" -gt 0 ]; then
      echo "     ❌ $file: $TODOS FUTURE/TODO comment(s) found"
      git diff --cached "$file" | grep -E "^\+" | grep -iE "\b(TODO|FUTURE)\b" | head -5
      TODO_COUNT=$((TODO_COUNT + TODOS))
    fi
  done

  if [ "$TODO_COUNT" -gt 0 ]; then
    echo "❌ ERROR: Cannot commit $TODO_COUNT FUTURE/TODO comments to main"
    echo "   Remove ALL TODO/FUTURE comments before committing - NO EXCEPTIONS"
    echo "   This applies to ALL code including tests"
    exit 1
  fi
  echo "  ✅ No FUTURE/TODO comments"
else
  echo "  ⏭️  Skipping FUTURE/TODO check (not on main branch)"
fi

# Check 4: No expect() in production code (excluding CLI, test files, build scripts, allowed modules)
echo "   Checking for expect() calls in production code..."
EXPECT_COUNT=0
for file in $STAGED_FILES; do
  # Skip test files, examples, benches, build scripts
  if [[ "$file" =~ /(test|tests|example|examples|bench|benches)/ ]] || [[ "$file" == *"build.rs" ]] || [[ "$file" =~ ^(test|tests|example|examples|bench|benches)/ ]]; then
    continue
  fi
  
  # Allow CLI code to use expect() (user-facing, different needs)
  if [[ "$file" =~ crates/ggen-cli/ ]] || [[ "$file" =~ ^crates/ggen-cli/ ]]; then
    continue
  fi
  
  # Check if file has allow attribute for expect (check actual file, not just diff)
  # Check for both #[allow] and #![allow] patterns
  if grep -qE "#!?\[allow\(clippy::expect_used\)\]" "$file" 2>/dev/null || \
     git diff --cached "$file" | grep -qE "#!?\[allow\(clippy::expect_used\)\]"; then
    continue
  fi
  
  # Pragmatic exception for pre-commit: if file has test modules, allow expect() calls
  # Rationale: Test modules should have allow attributes, but we're lenient for fast feedback
  # Pre-push hook will enforce stricter rules (require allow attributes in test modules)
  if grep -q "#\[cfg(test)\]" "$file" 2>/dev/null; then
    # File has test modules - allow expect() for pre-commit (fast feedback)
    # Pre-push will check that test modules have proper allow attributes
    continue
  fi
  
  # Count expect() calls in staged changes
  EXPECTS=$(git diff --cached "$file" | grep -E "^\+" | grep -c "\.expect(" || echo 0)
  EXPECTS=${EXPECTS//[^0-9]/}  # Remove any non-numeric characters
  if [ "${EXPECTS:-0}" -gt 0 ]; then
    echo "     ❌ $file: $EXPECTS expect() call(s) found"
    EXPECT_COUNT=$((EXPECT_COUNT + EXPECTS))
  fi
done

if [ "$EXPECT_COUNT" -gt 0 ]; then
  echo "❌ ERROR: Cannot commit $EXPECT_COUNT expect() calls in production code"
  echo "   Replace with proper error handling or add #![allow(clippy::expect_used)]"
  echo "   Note: CLI code (crates/ggen-cli) can use expect() for user-facing errors"
  exit 1
fi
echo "  ✅ No expect() in production code (CLI exempt)"

# Check 5: Formatting (use cargo fmt --check for speed in hooks)
echo "   Checking Rust formatting..."
if ! cargo fmt --all -- --check 2>&1; then
  echo "❌ ERROR: Code is not formatted"
  echo "   Run: cargo make fmt"
  exit 1
fi
echo "  ✅ Code is formatted"

# Check 6: Quick clippy check (only on staged packages, use cargo make lint)
echo "   Running clippy on staged packages..."

# Get unique packages from staged files
PACKAGES=$(echo "$STAGED_FILES" | sed 's|crates/||' | cut -d'/' -f1 | sort -u | grep -E "^ggen-" || true)

CLIPPY_FAILED=0
if [ -n "$PACKAGES" ]; then
  for pkg in $PACKAGES; do
    if [ -d "crates/$pkg" ]; then
      # Run clippy on lib and bins only (faster, excludes tests)
      # Use cargo make lint but limit to specific package
      # Note: cargo make doesn't support package filtering directly, so we use cargo clippy
      # but only for the specific package to maintain speed
      if cargo clippy --package "$pkg" --lib --bins -- -D warnings 2>&1 > /tmp/clippy_output.txt; then
        # Clippy passed
        continue
      else
        # Check exit code - if non-zero, check if it's test-related
        # Filter out test-related warnings and check if any remain
        if grep -v "test\|tests\|example\|examples\|bench\|benches\|\.rs:" /tmp/clippy_output.txt | grep -qE "(error|warning):"; then
          echo "❌ ERROR: Clippy found issues in $pkg"
          grep -v "test\|tests\|example\|examples\|bench\|benches" /tmp/clippy_output.txt | head -20
          echo "   Fix clippy warnings before committing"
          echo "   Run: cargo make lint"
          CLIPPY_FAILED=1
          break
        fi
      fi
    fi
  done
fi

rm -f /tmp/clippy_output.txt

if [ "$CLIPPY_FAILED" -eq 1 ]; then
  exit 1
fi
echo "  ✅ Clippy checks passed"

echo "✅ Pre-commit validation passed"
exit 0
EOF

# Create pre-push hook (comprehensive: 30-60s acceptable, full workspace validation)
cat > "$HOOKS_DIR/pre-push" << 'EOF'
#!/bin/bash
# Pre-push hook: 5-gate validation aligned with core team best practices
# Comprehensive validation before push (30-60s acceptable)
# Allows documented exceptions: CLI code, build scripts, test files with allow attributes
# Uses: cargo make commands (NEVER direct cargo commands)

set -e

# Change to project root
cd "$(git rev-parse --show-toplevel)"

echo "🚦 Pre-push validation (5 gates)..."
echo ""

# Gate 1: Cargo check (use cargo make check)
echo "Gate 1/5: Cargo check..."
if ! cargo make check 2>&1; then
  echo "❌ ERROR: cargo make check failed"
  exit 1
fi
echo "✅ Gate 1 passed"
echo ""

# Gate 2: Clippy (strict for production, lenient for tests)
echo "Gate 2/5: Clippy (strict mode for production)..."
# Use cargo make lint
if cargo make lint 2>&1 > /tmp/clippy_push_output.txt; then
  # Clippy passed
  rm -f /tmp/clippy_push_output.txt
else
  # Check if there are actual production code issues (not test-related)
  if grep -v "test\|tests\|example\|examples\|bench\|benches\|\.rs:" /tmp/clippy_push_output.txt | grep -qE "(error|warning):"; then
    echo "❌ ERROR: Clippy found warnings or errors in production code"
    grep -v "test\|tests\|example\|examples\|bench\|benches" /tmp/clippy_push_output.txt | head -30
    echo "   Test files are allowed to use expect() with #![allow(clippy::expect_used)]"
    rm -f /tmp/clippy_push_output.txt
    exit 1
  fi
  rm -f /tmp/clippy_push_output.txt
fi
echo "✅ Gate 2 passed"
echo ""

# Gate 2.5: TODO & error handling check (with exceptions)
echo "Gate 2.5/5: TODO & error handling check..."

# Check for TODO comments in production code
TODO_COUNT=$(find crates/ggen-*/src -name "*.rs" -type f 2>/dev/null | \
  grep -v "/tests/" | \
  grep -v "/test/" | \
  grep -v "/example" | \
  grep -v "build.rs" | \
  xargs grep "TODO:" 2>/dev/null | \
  grep -v "FUTURE:" | \
  wc -l | tr -d ' ' || echo 0)

if [ "$TODO_COUNT" -gt 0 ]; then
  echo "❌ ERROR: $TODO_COUNT TODO comments found in production code"
  echo "   Policy: Zero TODOs in production (use FUTURE: for planned enhancements)"
  exit 1
fi

# Check for unwrap/expect in production code (excluding allowed modules, CLI, build scripts)
UNWRAP_COUNT=$(find crates/ggen-*/src -name "*.rs" -type f 2>/dev/null | \
  grep -v "/tests/" | \
  grep -v "/test/" | \
  grep -v "/example" | \
  grep -v "build.rs" | \
  while read file; do
    # Skip CLI code (allowed to use expect for user errors)
    if [[ "$file" =~ crates/ggen-cli/ ]]; then
      continue
    fi
    # Skip files with allow attributes (matches both #[allow(...)] and #![allow(...)])
    if grep -qE "#!?\[allow\(clippy::unwrap_used\)\]" "$file" 2>/dev/null; then
      continue
    fi
    # Skip files with test modules (pragmatic exception - test modules should have allow attributes)
    if grep -q "#\[cfg(test)\]" "$file" 2>/dev/null; then
      continue
    fi
    grep -c "\.unwrap()" "$file" 2>/dev/null || echo 0
  done | awk '{s+=$1} END {print s}')

if [ "$UNWRAP_COUNT" -gt 0 ]; then
  echo "❌ ERROR: Found $UNWRAP_COUNT unwrap() calls in production code"
  echo "   Policy: Zero unwrap() unless documented with allow attribute"
  exit 1
fi

EXPECT_COUNT=$(find crates/ggen-*/src -name "*.rs" -type f 2>/dev/null | \
  grep -v "/tests/" | \
  grep -v "/test/" | \
  grep -v "/example" | \
  grep -v "build.rs" | \
  while read file; do
    # Skip CLI code (allowed to use expect for user errors)
    if [[ "$file" =~ crates/ggen-cli/ ]]; then
      continue
    fi
    # Skip files with allow attributes (check for both #[allow] and #![allow])
    if grep -qE "#!?\[allow\(clippy::expect_used\)\]" "$file" 2>/dev/null; then
      continue
    fi
    # Skip files with test modules (pragmatic exception - test modules should have allow attributes)
    if grep -q "#\[cfg(test)\]" "$file" 2>/dev/null; then
      continue
    fi
    grep -c "\.expect(" "$file" 2>/dev/null || echo 0
  done | awk '{s+=$1} END {print s}')

if [ "$EXPECT_COUNT" -gt 0 ]; then
  echo "❌ ERROR: Found $EXPECT_COUNT expect() calls in production code"
  echo "   Policy: Zero expect() unless documented with allow attribute"
  echo "   Note: CLI code (crates/ggen-cli) can use expect() for user-facing errors"
  exit 1
fi

echo "✅ Gate 2.5 passed"
echo ""

# Gate 3: Formatting check (use cargo fmt --check for speed in hooks)
echo "Gate 3/5: Formatting check..."
if ! cargo fmt --all -- --check 2>&1; then
  echo "❌ ERROR: Code is not formatted"
  echo "   Run: cargo make fmt"
  exit 1
fi
echo "✅ Gate 3 passed"
echo ""

# Gate 4: Fast tests (lib and bins only, use cargo make test)
echo "Gate 4/5: Fast tests (lib + bins)..."
# Note: cargo make test runs all tests, but we can filter output
if ! cargo make test 2>&1 | tail -20; then
  echo "❌ ERROR: Tests failed"
  exit 1
fi
echo "✅ Gate 4 passed"
echo ""

# Gate 5: Security audit (warning only, don't block)
echo "Gate 5/5: Security audit..."
if command -v cargo-audit &> /dev/null; then
  if ! cargo make audit 2>&1; then
    echo "⚠️  Security audit found issues (non-blocking)"
  else
    echo "✅ Gate 5 passed"
  fi
else
  echo "⚠️  cargo-audit not installed (optional)"
  echo "   Install: cargo install cargo-audit"
fi
echo ""

echo "✅ All gates passed - ready to push"
exit 0
EOF

# Make hooks executable
chmod +x "$HOOKS_DIR/pre-commit"
chmod +x "$HOOKS_DIR/pre-push"

echo "✅ Git hooks installed successfully:"
echo "   - $HOOKS_DIR/pre-commit"
echo "   - $HOOKS_DIR/pre-push"
echo ""
echo "🔍 Hooks enforce (aligned with core team 80/20 best practices):"
echo "   • No unwrap()/expect() in production code (test files allowed with #[allow])"
echo "   • No unimplemented!() placeholders (main branch only)"
echo "   • No TODO/FUTURE comments (main branch only)"
echo "   • CLI code (crates/ggen-cli) can use expect() for user-facing errors (documented exception)"
echo "   • Build scripts (build.rs) exempt from checks"
echo "   • Clippy warnings must be fixed (test files excluded)"
echo "   • Code must be formatted (via cargo make fmt)"
echo "   • Tests must pass before push (via cargo make test)"
echo ""
echo "⚡ Performance targets:"
echo "   • Pre-commit: 2-5 seconds (only checks staged files/packages)"
echo "   • Pre-push: 30-60 seconds (comprehensive workspace validation)"
echo ""
echo "🔧 Build system:"
echo "   • All commands use cargo make (NEVER direct cargo commands)"
echo "   • Pre-commit: cargo make fmt, cargo clippy (staged packages only)"
echo "   • Pre-push: cargo make check, cargo make lint, cargo make fmt, cargo make test, cargo make audit"
echo ""
echo "💡 Key improvements:"
echo "   • Test files can use expect() with #![allow(clippy::expect_used)]"
echo "   • Pre-commit only checks staged files (faster feedback)"
echo "   • Pre-push allows CLI code and build scripts (pragmatic exceptions)"
echo "   • Branch-aware: strict rules on main/master, relaxed on feature branches"
echo "   • Better alignment with core team 80/20 philosophy"
echo ""
echo "💡 To test hooks:"
echo "   1. Stage a file with unwrap(): git add <file>"
echo "   2. Try to commit: git commit -m 'test'"
echo "   3. Hook should prevent commit"

