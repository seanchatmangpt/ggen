#!/usr/bin/env bash
set -euo pipefail

# Regenerate code from ontology files
# Usage: ./regenerate-from-ontology.sh [--quiet|--verbose]

PROJECT_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
ONTOLOGY_DIR="${PROJECT_ROOT}/docs/examples/ontology"
OUTPUT_DIR="${PROJECT_ROOT}/generated"
TEMPLATES_DIR="${PROJECT_ROOT}/docs/examples/templates"

# Parse arguments
VERBOSITY="normal"
while [[ $# -gt 0 ]]; do
    case $1 in
        --quiet|-q)
            VERBOSITY="quiet"
            shift
            ;;
        --verbose|-v)
            VERBOSITY="verbose"
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--quiet|--verbose]"
            exit 1
            ;;
    esac
done

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    if [ "$VERBOSITY" != "quiet" ]; then
        echo -e "${GREEN}[regenerate]${NC} $1"
    fi
}

log_warn() {
    echo -e "${YELLOW}[regenerate]${NC} $1" >&2
}

log_error() {
    echo -e "${RED}[regenerate]${NC} $1" >&2
}

log_verbose() {
    if [ "$VERBOSITY" = "verbose" ]; then
        echo -e "${BLUE}[regenerate]${NC} $1"
    fi
}

# Check if ggen is available
if ! command -v ggen &> /dev/null; then
    log_error "ggen command not found. Please install ggen first."
    exit 1
fi

START_TIME=$(date +%s)

# Create output directory
mkdir -p "$OUTPUT_DIR"/{types,api,components}

log_info "Starting code regeneration from ontology..."

# Find all ontology files
ONTOLOGY_FILES=$(find "$ONTOLOGY_DIR" -name "*.ttl" -o -name "*.rdf" -o -name "*.owl" 2>/dev/null || true)

if [ -z "$ONTOLOGY_FILES" ]; then
    log_warn "No ontology files found in $ONTOLOGY_DIR"
    exit 0
fi

TOTAL_FILES=0
REGENERATED_FILES=0
FAILED_FILES=0

# Process each ontology file
while IFS= read -r ONTOLOGY_FILE; do
    TOTAL_FILES=$((TOTAL_FILES + 1))
    BASENAME=$(basename "$ONTOLOGY_FILE" .ttl)

    log_verbose "Processing $BASENAME..."

    # 1. Validate ontology
    log_verbose "  Validating ontology..."
    if ! ggen graph validate --file "$ONTOLOGY_FILE" > /dev/null 2>&1; then
        log_error "  ✗ Validation failed for $ONTOLOGY_FILE"
        FAILED_FILES=$((FAILED_FILES + 1))
        continue
    fi
    log_verbose "  ✓ Ontology valid"

    # 2. Load ontology into graph
    log_verbose "  Loading ontology into graph..."
    GRAPH_NAME="${BASENAME}-graph"
    if ! ggen graph load --graph "$GRAPH_NAME" --file "$ONTOLOGY_FILE" > /dev/null 2>&1; then
        log_error "  ✗ Failed to load ontology"
        FAILED_FILES=$((FAILED_FILES + 1))
        continue
    fi

    # 3. Generate TypeScript types
    log_verbose "  Generating TypeScript types..."
    TYPES_TEMPLATE="${TEMPLATES_DIR}/types.ts.hbs"
    TYPES_OUTPUT="${OUTPUT_DIR}/types/${BASENAME}.types.ts"

    if [ -f "$TYPES_TEMPLATE" ]; then
        if ggen template generate-rdf \
            --ontology "$ONTOLOGY_FILE" \
            --template "$TYPES_TEMPLATE" \
            --output "$TYPES_OUTPUT" > /dev/null 2>&1; then
            log_verbose "  ✓ Generated types: $TYPES_OUTPUT"
        else
            log_warn "  ⚠ Failed to generate types"
        fi
    else
        log_verbose "  ⊘ Types template not found: $TYPES_TEMPLATE"
    fi

    # 4. Generate API routes
    log_verbose "  Generating API routes..."
    API_TEMPLATE="${TEMPLATES_DIR}/api-routes.ts.hbs"
    API_OUTPUT="${OUTPUT_DIR}/api/${BASENAME}.routes.ts"

    if [ -f "$API_TEMPLATE" ]; then
        if ggen template generate-rdf \
            --ontology "$ONTOLOGY_FILE" \
            --template "$API_TEMPLATE" \
            --output "$API_OUTPUT" > /dev/null 2>&1; then
            log_verbose "  ✓ Generated API routes: $API_OUTPUT"
        else
            log_warn "  ⚠ Failed to generate API routes"
        fi
    else
        log_verbose "  ⊘ API template not found: $API_TEMPLATE"
    fi

    # 5. Generate CRUD components
    log_verbose "  Generating CRUD components..."
    CRUD_TEMPLATE="${TEMPLATES_DIR}/crud-component.tsx.hbs"
    CRUD_OUTPUT="${OUTPUT_DIR}/components/${BASENAME}.component.tsx"

    if [ -f "$CRUD_TEMPLATE" ]; then
        if ggen template generate-rdf \
            --ontology "$ONTOLOGY_FILE" \
            --template "$CRUD_TEMPLATE" \
            --output "$CRUD_OUTPUT" > /dev/null 2>&1; then
            log_verbose "  ✓ Generated CRUD component: $CRUD_OUTPUT"
        else
            log_warn "  ⚠ Failed to generate CRUD component"
        fi
    else
        log_verbose "  ⊘ CRUD template not found: $CRUD_TEMPLATE"
    fi

    REGENERATED_FILES=$((REGENERATED_FILES + 1))
    log_verbose "  ✓ Completed $BASENAME"

done <<< "$ONTOLOGY_FILES"

# 6. Run TypeScript type checking if tsconfig exists
if [ -f "$OUTPUT_DIR/tsconfig.json" ] || [ -f "$PROJECT_ROOT/tsconfig.json" ]; then
    log_info "Running TypeScript type checking..."

    if command -v tsc &> /dev/null; then
        if tsc --noEmit --project "${OUTPUT_DIR}/tsconfig.json" 2>&1 | tee /tmp/tsc-output.log | grep -q "error TS"; then
            log_warn "TypeScript errors found:"
            if [ "$VERBOSITY" = "verbose" ]; then
                cat /tmp/tsc-output.log
            else
                grep "error TS" /tmp/tsc-output.log | head -5
                echo "  ... (run with --verbose for full output)"
            fi
        else
            log_info "✓ TypeScript type checking passed"
        fi
    else
        log_verbose "TypeScript compiler not found, skipping type check"
    fi
fi

END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

# Summary
log_info "Regeneration complete in ${DURATION}s"
log_info "  Total ontologies: $TOTAL_FILES"
log_info "  Successfully processed: $REGENERATED_FILES"
if [ $FAILED_FILES -gt 0 ]; then
    log_warn "  Failed: $FAILED_FILES"
fi

# List changed files
if [ "$VERBOSITY" != "quiet" ]; then
    CHANGED_FILES=$(git status --porcelain "$OUTPUT_DIR" 2>/dev/null | grep -E '^\s*(M|A)' | awk '{print $2}' || true)
    if [ -n "$CHANGED_FILES" ]; then
        log_info "Changed files:"
        echo "$CHANGED_FILES" | sed 's/^/  - /'
    else
        log_info "No file changes detected"
    fi
fi

if [ $FAILED_FILES -gt 0 ]; then
    exit 1
fi

exit 0
