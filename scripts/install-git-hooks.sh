#!/usr/bin/env bash
# Install git hooks with gap detection
# Integrates test gap detection into pre-commit workflow

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

log() {
    echo -e "${BLUE}[INSTALL]${NC} $1"
}

success() {
    echo -e "${GREEN}✓${NC} $1"
}

log "Installing git hooks with gap detection..."

# Create hooks directory if it doesn't exist
mkdir -p .git/hooks

# Install pre-commit hook
log "Installing pre-commit hook..."

# Check if Rust binary exists
if [ -f "target/debug/git_hook_pre_commit" ] || [ -f "target/release/git_hook_pre_commit" ]; then
    # Use compiled binary
    if [ -f "target/release/git_hook_pre_commit" ]; then
        HOOK_BINARY="target/release/git_hook_pre_commit"
    else
        HOOK_BINARY="target/debug/git_hook_pre_commit"
    fi

    cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
# Pre-commit hook with gap detection

# Run main pre-commit validation
if [ -f "target/release/git_hook_pre_commit" ]; then
    target/release/git_hook_pre_commit
elif [ -f "target/debug/git_hook_pre_commit" ]; then
    target/debug/git_hook_pre_commit
else
    # Fallback to shell script
    .git/hooks/pre-commit.sh
fi

MAIN_EXIT=$?

if [ $MAIN_EXIT -ne 0 ]; then
    exit $MAIN_EXIT
fi

# Run test coverage enforcement (fast check)
if [ -f "scripts/enforce-test-coverage.sh" ]; then
    scripts/enforce-test-coverage.sh
    COVERAGE_EXIT=$?

    if [ $COVERAGE_EXIT -ne 0 ]; then
        exit $COVERAGE_EXIT
    fi
fi

exit 0
EOF
    chmod +x .git/hooks/pre-commit
    success "Installed binary-based pre-commit hook with coverage enforcement"
else
    # Fallback to shell script
    cp .git/hooks/pre-commit.sh .git/hooks/pre-commit 2>/dev/null || \
    cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
# Pre-commit hook with gap detection (fallback)
set -e

# Run main validation
.git/hooks/pre-commit.sh || exit 1

# Run test coverage enforcement
scripts/enforce-test-coverage.sh || exit 1

exit 0
EOF
    chmod +x .git/hooks/pre-commit
    success "Installed shell-based pre-commit hook with coverage enforcement"
fi

# Install pre-push hook (full gap detection)
log "Installing pre-push hook with full gap detection..."

cat > .git/hooks/pre-push << 'EOF'
#!/bin/bash
# Pre-push hook with comprehensive gap detection

set -e

echo "🔍 Running comprehensive gap detection before push..."

# Run full gap detection
if [ -f "scripts/detect-test-gaps.sh" ]; then
    scripts/detect-test-gaps.sh

    if [ $? -ne 0 ]; then
        echo ""
        echo "❌ Gap detection failed. Fix gaps before pushing."
        echo "   Run: scripts/detect-test-gaps.sh"
        exit 1
    fi
fi

# Run full test suite
echo "🧪 Running full test suite..."
if ! cargo test --workspace --no-default-features; then
    echo ""
    echo "❌ Tests failed. Fix failing tests before pushing."
    exit 1
fi

echo "✅ All checks passed - proceeding with push"
exit 0
EOF
chmod +x .git/hooks/pre-push
success "Installed pre-push hook with full gap detection"

# Make scripts executable
chmod +x scripts/detect-test-gaps.sh
chmod +x scripts/enforce-test-coverage.sh

success "Git hooks installed successfully"
echo ""
echo "Hooks installed:"
echo "  - Pre-commit: Fast validation + test coverage enforcement"
echo "  - Pre-push: Full gap detection + test suite"
echo ""
echo "To skip hooks (not recommended):"
echo "  git commit --no-verify"
echo "  git push --no-verify"
