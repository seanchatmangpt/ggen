#!/usr/bin/env bash
set -euo pipefail

# post-merge hook for automatic code regeneration after merges
# Install: cp docs/examples/hooks/post-merge.sh .git/hooks/post-merge && chmod +x .git/hooks/post-merge

PROJECT_ROOT="$(git rev-parse --show-toplevel)"
REGEN_SCRIPT="${PROJECT_ROOT}/docs/examples/scripts/regenerate-from-ontology.sh"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}[post-merge]${NC} Checking for ontology changes in merge..."

# Get the merge base and current HEAD
MERGE_HEAD=$(git rev-parse MERGE_HEAD 2>/dev/null || echo "")
if [ -z "$MERGE_HEAD" ]; then
    # Not a merge, possibly a fast-forward pull
    MERGE_HEAD=$(git rev-parse HEAD^1 2>/dev/null || echo "HEAD~1")
fi

# Check if any ontology files changed in the merge
CHANGED_ONTOLOGIES=$(git diff --name-only "$MERGE_HEAD" HEAD | grep -E '\.(ttl|rdf|owl)$' || true)

if [ -z "$CHANGED_ONTOLOGIES" ]; then
    echo -e "${GREEN}[post-merge]${NC} No ontology changes in merge, skipping regeneration"
    exit 0
fi

echo -e "${YELLOW}[post-merge]${NC} Ontology changes detected in merge:"
echo "$CHANGED_ONTOLOGIES" | sed 's/^/  - /'

# Check if regeneration script exists
if [ ! -x "$REGEN_SCRIPT" ]; then
    echo -e "${YELLOW}[post-merge]${NC} Regeneration script not found: $REGEN_SCRIPT"
    echo -e "${YELLOW}[post-merge]${NC} Please run manual regeneration if needed"
    exit 0
fi

# Run regeneration
echo -e "${GREEN}[post-merge]${NC} Running code regeneration..."
START_TIME=$(date +%s)

if ! "$REGEN_SCRIPT" --verbose; then
    echo -e "${RED}[post-merge]${NC} Code regeneration failed"
    echo -e "${RED}[post-merge]${NC} Please resolve issues manually"
    exit 1
fi

END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

# Check for uncommitted changes
UNCOMMITTED=$(git status --porcelain | grep -E '^\s*(M|A|\?\?)' || true)

if [ -n "$UNCOMMITTED" ]; then
    echo -e "${YELLOW}[post-merge]${NC} ⚠ Code regeneration created uncommitted changes:"
    git status --short | grep -E '^\s*(M|A|\?\?)' | sed 's/^/  /'

    echo ""
    echo -e "${YELLOW}[post-merge]${NC} Please review and commit these changes:"
    echo -e "  ${BLUE}git status${NC}"
    echo -e "  ${BLUE}git diff${NC}"
    echo -e "  ${BLUE}git add <files>${NC}"
    echo -e "  ${BLUE}git commit -m 'chore: regenerate code from ontology changes'${NC}"
else
    echo -e "${GREEN}[post-merge]${NC} ✓ Code regeneration completed successfully (${DURATION}s)"
    echo -e "${GREEN}[post-merge]${NC} No uncommitted changes"
fi

exit 0
