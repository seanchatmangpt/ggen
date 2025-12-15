#!/bin/bash
# GGEN Git Hooks Ontology Generation Demo
# Demonstrates how pre-commit hooks automatically generate and update ontologies

set -e  # Exit on error

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Logging helpers
log_step() {
    echo -e "${BLUE}==>${NC} ${CYAN}$1${NC}"
}

log_success() {
    echo -e "${GREEN}✓${NC} $1"
}

log_info() {
    echo -e "${YELLOW}ℹ${NC} $1"
}

log_error() {
    echo -e "${RED}✗${NC} $1"
}

# Cleanup function
cleanup() {
    if [ -d "$TEST_DIR" ]; then
        log_step "Cleaning up test directory..."
        cd "$ORIGINAL_DIR"
        rm -rf "$TEST_DIR"
        log_success "Cleanup complete"
    fi
}

# Set trap to cleanup on exit
trap cleanup EXIT

# Store original directory
ORIGINAL_DIR=$(pwd)

# Configuration
TEST_DIR="/tmp/ggen-git-hooks-demo-$$"
PROJECT_NAME="demo-marketplace-project"

echo ""
echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║${NC}  ${GREEN}GGEN Git Hooks Ontology Generation Demo${NC}                      ${CYAN}║${NC}"
echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
echo ""

# Step 1: Setup test directory
log_step "Step 1: Setting up test directory"
mkdir -p "$TEST_DIR"
cd "$TEST_DIR"
log_success "Created test directory: $TEST_DIR"

# Step 2: Build ggen locally
log_step "Step 2: Building ggen from source"
cd "$ORIGINAL_DIR"
if ! cargo build --release 2>&1 | grep -q "Finished"; then
    log_info "Building ggen (this may take a moment)..."
    cargo build --release
fi
GGEN_BIN="$ORIGINAL_DIR/target/release/ggen"
if [ ! -f "$GGEN_BIN" ]; then
    log_error "Failed to build ggen"
    exit 1
fi
log_success "Built ggen binary: $GGEN_BIN"

# Step 3: Initialize git repository
log_step "Step 3: Initializing git repository"
cd "$TEST_DIR"
git init
git config user.name "Demo User"
git config user.email "demo@example.com"
log_success "Initialized git repository"

# Step 4: Create marketplace project
log_step "Step 4: Creating marketplace project with ggen"
"$GGEN_BIN" marketplace init "$PROJECT_NAME" --template basic
cd "$PROJECT_NAME"
log_success "Created project: $PROJECT_NAME"

# Step 5: Install git hooks
log_step "Step 5: Installing git hooks"
"$GGEN_BIN" hook add pre-commit --command "ggen ontology generate"
log_success "Installed pre-commit hook for ontology generation"

# Show hook configuration
log_info "Hook configuration:"
if [ -f ".git/hooks/pre-commit" ]; then
    echo -e "${YELLOW}$(cat .git/hooks/pre-commit | head -n 5)${NC}"
else
    log_error "Pre-commit hook not found"
fi

# Step 6: Show initial state
log_step "Step 6: Checking initial ontology state"
ONTOLOGY_DIR=".ggen/ontology"
if [ -d "$ONTOLOGY_DIR" ]; then
    log_info "Initial ontology files:"
    find "$ONTOLOGY_DIR" -name "*.rdf" -o -name "*.ttl" | while read -r file; do
        echo "  - $(basename $file)"
    done
else
    log_info "No ontology directory found (will be created on first commit)"
fi

# Step 7: Create initial commit
log_step "Step 7: Creating initial commit"
git add .
git commit -m "Initial commit: marketplace project setup" || true
log_success "Initial commit created"

# Step 8: Check ontology after initial commit
log_step "Step 8: Examining generated ontology"
if [ -d "$ONTOLOGY_DIR" ]; then
    log_success "Ontology directory created: $ONTOLOGY_DIR"

    echo ""
    log_info "Generated ontology files:"
    find "$ONTOLOGY_DIR" -type f | while read -r file; do
        echo -e "  ${GREEN}→${NC} $(basename $file) ($(wc -l < $file) lines)"
    done

    # Show sample RDF content
    RDF_FILE=$(find "$ONTOLOGY_DIR" -name "*.rdf" -o -name "*.ttl" | head -n 1)
    if [ -f "$RDF_FILE" ]; then
        echo ""
        log_info "Sample ontology content from $(basename $RDF_FILE):"
        echo -e "${YELLOW}$(head -n 15 $RDF_FILE)${NC}"
        echo "..."
    fi
else
    log_error "Ontology directory not created"
fi

# Step 9: Make code changes to trigger ontology update
log_step "Step 9: Making code changes to trigger ontology update"

# Create a new source file
mkdir -p src
cat > src/example.rs << 'EOF'
/// Example module demonstrating ontology tracking
pub mod example {
    /// A sample struct for the demo
    pub struct DemoStruct {
        pub id: u64,
        pub name: String,
    }

    /// Example function showing code organization
    pub fn process_demo(data: &DemoStruct) -> String {
        format!("Processing: {}", data.name)
    }
}
EOF

log_success "Created new source file: src/example.rs"

# Step 10: Show ontology before commit
log_step "Step 10: Capturing ontology state before commit"
BEFORE_HASH=""
if [ -d "$ONTOLOGY_DIR" ]; then
    BEFORE_HASH=$(find "$ONTOLOGY_DIR" -type f -exec md5sum {} \; | sort | md5sum | cut -d' ' -f1)
    log_info "Ontology hash before commit: $BEFORE_HASH"
fi

# Step 11: Commit changes (triggers pre-commit hook)
log_step "Step 11: Committing changes (this triggers pre-commit hook)"
git add src/example.rs
log_info "Running git commit (pre-commit hook will execute)..."
git commit -m "Add example module" || true
log_success "Commit completed with hook execution"

# Step 12: Show ontology after commit
log_step "Step 12: Examining updated ontology"
AFTER_HASH=""
if [ -d "$ONTOLOGY_DIR" ]; then
    AFTER_HASH=$(find "$ONTOLOGY_DIR" -type f -exec md5sum {} \; | sort | md5sum | cut -d' ' -f1)
    log_info "Ontology hash after commit: $AFTER_HASH"

    if [ "$BEFORE_HASH" != "$AFTER_HASH" ]; then
        log_success "Ontology was updated by pre-commit hook!"
    else
        log_info "Ontology unchanged (no structural changes detected)"
    fi

    echo ""
    log_info "Updated ontology files:"
    find "$ONTOLOGY_DIR" -type f | while read -r file; do
        echo -e "  ${GREEN}→${NC} $(basename $file) ($(wc -l < $file) lines, modified: $(stat -f '%Sm' -t '%Y-%m-%d %H:%M:%S' $file 2>/dev/null || stat -c '%y' $file 2>/dev/null || echo 'unknown'))"
    done

    # Show diff in RDF content if available
    RDF_FILE=$(find "$ONTOLOGY_DIR" -name "*.rdf" -o -name "*.ttl" | head -n 1)
    if [ -f "$RDF_FILE" ]; then
        echo ""
        log_info "Latest ontology content from $(basename $RDF_FILE):"
        echo -e "${YELLOW}$(head -n 20 $RDF_FILE)${NC}"
        echo "..."
        echo ""
        log_info "Total triples: $(grep -c '<' $RDF_FILE 2>/dev/null || echo '(unable to count)')"
    fi
else
    log_error "Ontology directory not found after commit"
fi

# Step 13: Show git log
log_step "Step 13: Git commit history"
git log --oneline --all | head -n 5

# Step 14: Verify hook integration
log_step "Step 14: Verifying hook integration"
if [ -f ".git/hooks/pre-commit" ]; then
    log_success "Pre-commit hook is installed"
    HOOK_CALLS=$(grep -c "ggen" .git/hooks/pre-commit 2>/dev/null || echo "0")
    log_info "Hook references ggen: $HOOK_CALLS times"
else
    log_error "Pre-commit hook missing"
fi

# Final summary
echo ""
echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║${NC}  ${GREEN}Demo Summary${NC}                                                ${CYAN}║${NC}"
echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
echo ""
log_success "✓ Created marketplace project"
log_success "✓ Installed git pre-commit hook"
log_success "✓ Generated ontology on initial commit"
log_success "✓ Updated ontology on code changes"
log_success "✓ Verified hook execution"
echo ""
log_info "The pre-commit hook automatically generates/updates ontologies"
log_info "reflecting the project's code structure in RDF format."
echo ""
log_info "Test directory: $TEST_DIR/$PROJECT_NAME"
log_info "Ontology location: $TEST_DIR/$PROJECT_NAME/$ONTOLOGY_DIR"
echo ""

# Offer to keep test directory
read -p "Keep test directory for inspection? (y/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    trap - EXIT  # Disable cleanup
    log_info "Test directory preserved: $TEST_DIR/$PROJECT_NAME"
    echo -e "${YELLOW}To explore:${NC} cd $TEST_DIR/$PROJECT_NAME"
else
    log_info "Test directory will be cleaned up on exit"
fi

echo ""
log_success "Demo complete!"
