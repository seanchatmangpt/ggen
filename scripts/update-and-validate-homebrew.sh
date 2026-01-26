#!/usr/bin/env bash
# One-command script to update and validate Homebrew formula
# Script-based only - no GitHub Actions dependency

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Homebrew Formula Update and Validation"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Step 1: Initialize submodule if needed
echo "Step 1: Checking submodule..."
if [[ ! -d "$REPO_ROOT/vendors/homebrew-ggen" ]]; then
  echo "Initializing submodule..."
  cd "$REPO_ROOT"
  git submodule update --init --recursive vendors/homebrew-ggen
else
  echo "✅ Submodule exists"
fi

echo ""

# Step 2: Update formula
echo "Step 2: Updating Homebrew formula..."
echo ""
"$SCRIPT_DIR/update-homebrew-formula-e2e.sh" --skip-push

echo ""

# Step 3: Validate formula
echo "Step 3: Validating Homebrew formula..."
echo ""
"$SCRIPT_DIR/validate-homebrew-formula.sh"

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "✅ Complete! Formula updated and validated."
echo ""
echo "Next steps:"
echo "  1. Review the formula: $REPO_ROOT/vendors/homebrew-ggen/Formula/ggen.rb"
echo "  2. If satisfied, push changes:"
echo "     cd $REPO_ROOT/vendors/homebrew-ggen"
echo "     git push origin main"
echo "  3. Update main repo submodule reference (optional):"
echo "     cd $REPO_ROOT"
echo "     git add vendors/homebrew-ggen"
echo "     git commit -m 'chore: update homebrew-ggen submodule'"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

