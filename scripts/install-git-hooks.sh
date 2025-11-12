#!/bin/bash
# Install Rust-based git hooks for ggen project
# Aligned with chicago-tdd-tools pattern: Rust binaries, no bash wrappers
# Prevents unwrap() calls and unimplemented!() from being committed
# Uses: cargo make commands (NEVER direct cargo commands)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "🔧 Installing ggen git hooks (Rust binaries, chicago-tdd-tools pattern)..."

# Ensure we're in a git repository
if [ ! -d "$PROJECT_ROOT/.git" ]; then
  echo "❌ ERROR: .git directory not found"
  echo "   Are you in a git repository?"
  exit 1
fi

# Change to project root
cd "$PROJECT_ROOT"

# Run Rust installer
if cargo run --bin git_hook_installer --package ggen-utils 2>&1; then
  echo ""
  echo "✅ Git hooks installed successfully"
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
  echo "   • Pre-commit: cargo fmt, cargo clippy (staged packages only)"
  echo "   • Pre-push: cargo make check, cargo make lint, cargo make fmt, cargo make test, cargo make audit"
  echo ""
  echo "💡 To test hooks:"
  echo "   1. Stage a file with unwrap(): git add <file>"
  echo "   2. Try to commit: git commit -m 'test'"
  echo "   3. Hook should prevent commit"
else
  echo "❌ ERROR: Failed to install git hooks"
  echo "   Make sure you can build ggen-utils: cargo build --package ggen-utils"
  exit 1
fi
