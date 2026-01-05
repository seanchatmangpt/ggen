#!/bin/bash
# ggen v5.2.0 Session Init - Poka-Yoke Environment Verification
set -e
echo "═══ ggen v5.2.0 Session Init ═══"

# 1. Verify Rust (MSRV 1.75+)
RUST_V=$(rustc --version | grep -oP '\d+\.\d+') || { echo "❌ Rust not found"; exit 1; }
echo "✓ Rust $RUST_V"

# 2. Verify cargo-make (CRITICAL: never direct cargo)
command -v cargo-make &>/dev/null || { echo "❌ cargo-make required"; exit 1; }
echo "✓ cargo-make"

# 3. Git status
[ -d .git ] && echo "✓ Branch: $(git branch --show-current)"

# 4. Quick SLO check
timeout 5s cargo make check &>/dev/null && echo "✓ SLO: check <5s" || echo "⚠ check timeout"

echo "═══ THREE PARADIGM SHIFTS ═══"
echo "1. Big Bang 80/20: /speckit-verify → single-pass"
echo "2. EPIC 9: 10 agents parallel (2.8-4.4x speedup)"
echo "3. Receipts replace review: cargo make pre-commit"
echo "═══ CONSTITUTIONAL RULES ═══"
echo "• cargo make ONLY (never direct cargo)"
echo "• Result<T,E> in production (no unwrap)"
echo "• Andon: RED=stop, YELLOW=investigate"
echo "════════════════════════════"
