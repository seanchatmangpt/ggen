#!/usr/bin/env bash
set -euo pipefail

# pre-commit hook for automatic code regeneration from ontology changes
# Install: cp docs/examples/hooks/pre-commit.sh .git/hooks/pre-commit && chmod +x .git/hooks/pre-commit

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(git rev-parse --show-toplevel)"
REGEN_SCRIPT="${PROJECT_ROOT}/docs/examples/scripts/regenerate-from-ontology.sh"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}[pre-commit]${NC} Checking for ontology changes..."

# Check if any .ttl or .rdf files are staged
ONTOLOGY_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(ttl|rdf|owl)$' || true)

if [ -z "$ONTOLOGY_FILES" ]; then
    echo -e "${GREEN}[pre-commit]${NC} No ontology changes detected, skipping regeneration"
    exit 0
fi

echo -e "${YELLOW}[pre-commit]${NC} Ontology changes detected:"
echo "$ONTOLOGY_FILES" | sed 's/^/  - /'

# Validate ontologies first
echo -e "${GREEN}[pre-commit]${NC} Validating ontologies..."
VALIDATION_FAILED=0

for FILE in $ONTOLOGY_FILES; do
    if [ -f "$PROJECT_ROOT/$FILE" ]; then
        echo -e "${GREEN}[pre-commit]${NC}   Validating $FILE..."
        if ! ggen graph validate --file "$PROJECT_ROOT/$FILE" 2>&1 | grep -q "valid"; then
            echo -e "${RED}[pre-commit]${NC}   ✗ Validation failed for $FILE"
            VALIDATION_FAILED=1
        else
            echo -e "${GREEN}[pre-commit]${NC}   ✓ $FILE is valid"
        fi
    fi
done

if [ $VALIDATION_FAILED -eq 1 ]; then
    echo -e "${RED}[pre-commit]${NC} Ontology validation failed. Please fix errors before committing."
    exit 1
fi

# Run regeneration script
echo -e "${GREEN}[pre-commit]${NC} Running code regeneration..."
START_TIME=$(date +%s)

if [ -x "$REGEN_SCRIPT" ]; then
    if ! "$REGEN_SCRIPT" --quiet; then
        echo -e "${RED}[pre-commit]${NC} Code regeneration failed"
        exit 1
    fi
else
    echo -e "${YELLOW}[pre-commit]${NC} Regeneration script not found or not executable: $REGEN_SCRIPT"
    echo -e "${YELLOW}[pre-commit]${NC} Skipping automatic regeneration"
    exit 0
fi

END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

# Check if any files were generated
GENERATED_FILES=$(git status --porcelain | grep -E '^\s*(M|A)' | awk '{print $2}' | grep -E '\.(ts|tsx|js)$' || true)

if [ -n "$GENERATED_FILES" ]; then
    echo -e "${GREEN}[pre-commit]${NC} Generated/modified files:"
    echo "$GENERATED_FILES" | sed 's/^/  - /'

    # Add generated files to staging area
    echo "$GENERATED_FILES" | xargs git add

    echo -e "${GREEN}[pre-commit]${NC} ✓ Code regeneration completed in ${DURATION}s"
    echo -e "${YELLOW}[pre-commit]${NC} Generated files have been added to commit"
else
    echo -e "${GREEN}[pre-commit]${NC} ✓ No code changes needed (${DURATION}s)"
fi

exit 0
