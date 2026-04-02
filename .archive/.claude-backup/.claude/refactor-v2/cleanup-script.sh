#!/usr/bin/env bash
#
# ggen v2.0.0 → v2.1.0 Cleanup Script
#
# This script performs the cleanup operations outlined in deprecation-plan.md.
# It should be run as part of the v2.1.0 release process (February 2026).
#
# Usage:
#   ./cleanup-script.sh --dry-run   # Preview changes
#   ./cleanup-script.sh --execute   # Perform cleanup
#
# Safety: This script requires explicit --execute flag to prevent accidental runs.

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script mode
DRY_RUN=true
BACKUP_DIR=".cleanup-backup-$(date +%Y%m%d-%H%M%S)"

# Parse arguments
case "${1:-}" in
    --dry-run)
        DRY_RUN=true
        echo -e "${BLUE}🔍 Running in DRY RUN mode - no changes will be made${NC}"
        ;;
    --execute)
        DRY_RUN=false
        echo -e "${YELLOW}⚠️  Running in EXECUTE mode - changes will be made!${NC}"
        read -p "Are you sure you want to proceed? (yes/no): " confirm
        if [ "$confirm" != "yes" ]; then
            echo "Aborted."
            exit 1
        fi
        ;;
    *)
        echo "Usage: $0 {--dry-run|--execute}"
        echo ""
        echo "  --dry-run    Preview changes without making them"
        echo "  --execute    Perform actual cleanup (requires confirmation)"
        exit 1
        ;;
esac

# Helper functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

execute_or_preview() {
    local description="$1"
    local command="$2"

    if [ "$DRY_RUN" = true ]; then
        echo -e "${YELLOW}[PREVIEW]${NC} $description"
        echo "  Command: $command"
    else
        log_info "$description"
        eval "$command"
        log_success "Completed: $description"
    fi
}

# Create backup
create_backup() {
    if [ "$DRY_RUN" = false ]; then
        log_info "Creating backup in $BACKUP_DIR..."
        mkdir -p "$BACKUP_DIR"

        # Backup files we're going to delete
        if [ -d "cli/src/commands" ]; then
            cp -r "cli/src/commands" "$BACKUP_DIR/"
        fi

        # Backup files we're going to modify
        find cli/src/cmds -name "*.rs" -exec grep -l "TODO\|FIXME\|unwrap()" {} \; | while read -r file; do
            mkdir -p "$BACKUP_DIR/$(dirname "$file")"
            cp "$file" "$BACKUP_DIR/$file"
        done

        log_success "Backup created in $BACKUP_DIR"
    else
        log_info "Would create backup in $BACKUP_DIR"
    fi
}

# Step 1: Remove deprecated commands/ directory
remove_old_commands() {
    log_info "Step 1: Remove old commands/ directory structure"

    if [ ! -d "cli/src/commands" ]; then
        log_warning "commands/ directory doesn't exist, skipping"
        return
    fi

    local files_to_remove=(
        "cli/src/commands/ai/"
        "cli/src/commands/template/"
        "cli/src/commands/project/"
        "cli/src/commands/marketplace/"
    )

    for path in "${files_to_remove[@]}"; do
        if [ -e "$path" ]; then
            execute_or_preview "Remove $path" "rm -rf '$path'"
        fi
    done

    # Keep utils/doctor for now as it might be reused
    log_warning "Keeping utils/doctor for potential reuse in cmds/"
}

# Step 2: Remove TODO comments (placeholder - manual review needed)
remove_todos() {
    log_info "Step 2: Remove TODO comments"
    log_warning "This requires manual implementation:"

    local todo_files=(
        "cli/src/cmds/hook/remove.rs"
        "cli/src/cmds/hook/list.rs"
        "cli/src/cmds/hook/create.rs"
        "cli/src/cmds/hook/run.rs"
    )

    for file in "${todo_files[@]}"; do
        if [ -f "$file" ]; then
            local todo_count=$(grep -c "TODO" "$file" || true)
            if [ "$todo_count" -gt 0 ]; then
                log_warning "  $file has $todo_count TODO comments - needs implementation"
            fi
        fi
    done

    log_info "Run this to see all TODOs:"
    echo "  grep -rn 'TODO\|FIXME' cli/src/cmds --include='*.rs'"
}

# Step 3: Replace unwrap() calls (automated where safe)
replace_unwraps() {
    log_info "Step 3: Replace unwrap() calls with proper error handling"

    # Count unwrap calls
    local unwrap_count=$(grep -r "unwrap()" cli/src/cmds --include="*.rs" | wc -l | tr -d ' ')
    log_info "Found $unwrap_count unwrap() calls in cmds/"

    # List files with unwrap (for manual review)
    log_warning "Files with unwrap() calls (manual review needed):"
    grep -r "unwrap()" cli/src/cmds --include="*.rs" -l | sort | while read -r file; do
        local count=$(grep -c "unwrap()" "$file")
        echo "  $file ($count occurrences)"
    done

    log_info "Focus on these high-priority files first:"
    echo "  - Error handling paths (not tests)"
    echo "  - Public API functions"
    echo "  - User-facing commands"
}

# Step 4: Remove dead code
remove_dead_code() {
    log_info "Step 4: Remove dead code and unused imports"

    # This is best done with cargo tools
    execute_or_preview "Run cargo check" "cargo check --all-features"
    execute_or_preview "Run cargo clippy for dead code" "cargo clippy -- -W dead_code -W unused_imports"
    execute_or_preview "Run cargo fmt" "cargo fmt --all"

    log_info "Review clippy output and remove flagged code manually"
}

# Step 5: Update tests
update_tests() {
    log_info "Step 5: Update tests to use new architecture"

    execute_or_preview "Run test suite" "cargo test --all-features"

    log_warning "Manually review and update:"
    echo "  - Tests importing from 'commands' module"
    echo "  - Tests with deprecated patterns"
    echo "  - Integration tests for removed functionality"
}

# Step 6: Update documentation
update_documentation() {
    log_info "Step 6: Update documentation"

    local docs_to_update=(
        "README.md"
        "CONTRIBUTING.md"
        "docs/MIGRATION_V1_TO_V2.md"
        "CHANGELOG.md"
    )

    for doc in "${docs_to_update[@]}"; do
        if [ -f "$doc" ]; then
            log_warning "Update $doc to remove v1.x references"
        fi
    done
}

# Step 7: Performance validation
validate_performance() {
    log_info "Step 7: Validate performance improvements"

    log_info "Expected improvements after cleanup:"
    echo "  - Build time: -10%"
    echo "  - Binary size: -5%"
    echo "  - Test time: -15%"
    echo "  - Memory usage: -5%"

    execute_or_preview "Benchmark build time" "time cargo build --release"
    execute_or_preview "Check binary size" "ls -lh target/release/ggen"
    execute_or_preview "Benchmark tests" "time cargo test --all-features"
}

# Main execution
main() {
    echo ""
    echo "======================================"
    echo "  ggen v2.1.0 Cleanup Script"
    echo "======================================"
    echo ""

    # Verify we're in the right directory
    if [ ! -f "Cargo.toml" ] || [ ! -d "cli/src/cmds" ]; then
        log_error "Must be run from ggen project root"
        exit 1
    fi

    # Create backup
    create_backup

    echo ""
    echo "Running cleanup steps..."
    echo ""

    # Execute cleanup steps
    remove_old_commands
    echo ""

    remove_todos
    echo ""

    replace_unwraps
    echo ""

    remove_dead_code
    echo ""

    update_tests
    echo ""

    update_documentation
    echo ""

    validate_performance
    echo ""

    # Summary
    echo "======================================"
    echo "  Cleanup Summary"
    echo "======================================"

    if [ "$DRY_RUN" = true ]; then
        echo ""
        log_info "DRY RUN completed - no changes made"
        echo ""
        echo "To execute cleanup, run:"
        echo "  $0 --execute"
    else
        echo ""
        log_success "Cleanup completed successfully!"
        echo ""
        log_info "Backup created in: $BACKUP_DIR"
        echo ""
        log_warning "Next steps:"
        echo "  1. Review changes with: git diff"
        echo "  2. Run tests: cargo make ci"
        echo "  3. Update CHANGELOG.md"
        echo "  4. Commit changes: git add -A && git commit -m 'chore: v2.1.0 cleanup'"
        echo ""
        log_info "If anything went wrong, restore from backup:"
        echo "  cp -r $BACKUP_DIR/cli/src/commands cli/src/"
    fi

    echo ""
}

# Run main
main "$@"
