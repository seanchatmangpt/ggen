#!/usr/bin/env bash
# Complete release workflow with Homebrew update

# Source common library
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

# Script setup
setup_script "release-brew" "Complete release workflow with Homebrew update"

# Configuration
readonly DEFAULT_REPO="seanchatmangpt/ggen"
readonly DEFAULT_WAIT_SECONDS="120"
readonly DEFAULT_CARGO_FILE="Cargo.toml"

# Environment variables
readonly REPO="${GITHUB_REPO:-$DEFAULT_REPO}"
readonly WAIT_SECONDS="${RELEASE_WAIT_SECONDS:-$DEFAULT_WAIT_SECONDS}"
readonly CARGO_FILE="${CARGO_FILE:-$DEFAULT_CARGO_FILE}"

# Global variables
VERBOSE=false
FORCE=false
YES=false
VERSION=""

# Help function
show_help() {
    show_help_template \
        "[VERSION] [OPTIONS]" \
        "Complete release workflow with Homebrew update. Creates a git tag, waits for GitHub release, and updates the Homebrew formula." \
        "    -h, --help          Show this help message
    -v, --verbose        Enable verbose output
    -y, --yes            Skip confirmation prompt
    -f, --force          Force release without confirmation
    -w, --wait SECONDS   Wait time for builds to complete (default: 120)
    -c, --cargo FILE     Cargo.toml file to read version from (default: Cargo.toml)" \
        "    $SCRIPT_NAME                    # Release with version from Cargo.toml
    $SCRIPT_NAME 1.2.3               # Release specific version
    $SCRIPT_NAME --yes                # Skip confirmation prompt
    $SCRIPT_NAME --verbose            # Enable verbose output
    $SCRIPT_NAME --wait 300          # Wait 5 minutes for builds" \
        "    GITHUB_REPO          GitHub repository (default: $DEFAULT_REPO)
    RELEASE_WAIT_SECONDS Wait time for builds (default: $DEFAULT_WAIT_SECONDS)
    CARGO_FILE           Cargo.toml file to read version from (default: $DEFAULT_CARGO_FILE)
    FORCE                Force release without confirmation (default: false)
    YES                  Skip confirmation prompt (default: false)
    DEBUG                Enable debug output (default: false)"
}

# Validation functions
validate_inputs() {
    log_debug "Validating inputs..."
    
    if ! validate_repo_format "$REPO"; then
        return $EXIT_VALIDATION_FAILED
    fi
    
    if [ -z "$VERSION" ]; then
        log_error "Version cannot be empty"
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! validate_version "$VERSION"; then
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! [[ "$WAIT_SECONDS" =~ ^[0-9]+$ ]] || [ "$WAIT_SECONDS" -lt 30 ] || [ "$WAIT_SECONDS" -gt 1800 ]; then
        log_error "Invalid wait time: $WAIT_SECONDS"
        log_error "Wait time must be between 30 and 1800 seconds"
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! validate_file_exists "$CARGO_FILE"; then
        return $EXIT_VALIDATION_FAILED
    fi
    
    return $EXIT_SUCCESS
}

# Release functions
check_tag_exists() {
    local version="$1"
    log_debug "Checking if tag exists: v$version"
    
    if git rev-parse "v$version" >/dev/null 2>&1; then
        log_error "Tag v$version already exists"
        log_error "Delete with: git tag -d v$version && git push origin :refs/tags/v$version"
        return $EXIT_VALIDATION_FAILED
    fi
    
    return $EXIT_SUCCESS
}

create_and_push_tag() {
    local version="$1"
    log_debug "Creating and pushing tag: v$version"
    
    if [ "$VERBOSE" = true ]; then
        log_info "Creating tag v$version..."
    fi
    
    if git tag "v$version"; then
        log_success "Tag v$version created"
    else
        log_error "Failed to create tag v$version"
        return $EXIT_ERROR
    fi
    
    if [ "$VERBOSE" = true ]; then
        log_info "Pushing tag v$version to origin..."
    fi
    
    if git push origin "v$version"; then
        log_success "Tag v$version pushed to origin"
    else
        log_error "Failed to push tag v$version"
        log_error "You may need to push manually: git push origin v$version"
        return $EXIT_ERROR
    fi
    
    return $EXIT_SUCCESS
}

wait_for_release() {
    local version="$1"
    log_debug "Waiting for GitHub release to complete..."
    
    if [ "$VERBOSE" = true ]; then
        log_info "Waiting for GitHub release to complete..."
        log_info "Monitor at: https://github.com/$REPO/actions"
        log_info "Release URL: https://github.com/$REPO/releases/tag/v$version"
        echo ""
        log_info "⏳ Waiting $WAIT_SECONDS seconds for builds to complete..."
    else
        log_info "⏳ Waiting $WAIT_SECONDS seconds for builds to complete..."
    fi
    
    sleep "$WAIT_SECONDS"
    
    if [ "$VERBOSE" = true ]; then
        log_success "Wait period completed"
    fi
}

update_homebrew_formula() {
    local version="$1"
    log_debug "Updating Homebrew formula for version: $version"
    
    if [ "$VERBOSE" = true ]; then
        log_info "Updating Homebrew formula..."
    fi
    
    if [ -f "./scripts/update-homebrew-formula.sh" ]; then
        if ./scripts/update-homebrew-formula.sh "$version"; then
            log_success "Homebrew formula updated"
        else
            log_error "Failed to update Homebrew formula"
            return $EXIT_ERROR
        fi
    else
        log_error "Homebrew update script not found: ./scripts/update-homebrew-formula.sh"
        return $EXIT_ERROR
    fi
    
    return $EXIT_SUCCESS
}

show_completion_message() {
    local version="$1"
    
    if [ "$VERBOSE" = true ]; then
        echo ""
        log_success "Release complete!"
        echo ""
        log_info "📦 Users can install with:"
        log_info "   brew install seanchatmangpt/tap/ggen"
        echo ""
        log_info "🔗 Release: https://github.com/$REPO/releases/tag/v$version"
        log_info "🔗 Actions: https://github.com/$REPO/actions"
    else
        log_success "Release complete!"
        echo ""
        log_info "📦 Users can install with: brew install seanchatmangpt/tap/ggen"
        log_info "🔗 Release: https://github.com/$REPO/releases/tag/v$version"
    fi
}

# Main function
main() {
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_help
                exit $EXIT_SUCCESS
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -y|--yes)
                YES=true
                shift
                ;;
            -f|--force)
                FORCE=true
                shift
                ;;
            -w|--wait)
                WAIT_SECONDS="$2"
                shift 2
                ;;
            -c|--cargo)
                CARGO_FILE="$2"
                shift 2
                ;;
            -*)
                log_error "Unknown option: $1"
                show_help
                exit $EXIT_INVALID_ARGS
                ;;
            *)
                if [ -z "$VERSION" ]; then
                    VERSION="$1"
                else
                    log_error "Multiple versions specified: $VERSION and $1"
                    exit $EXIT_INVALID_ARGS
                fi
                shift
                ;;
        esac
    done
    
    # Get version from Cargo.toml if not provided
    if [ -z "$VERSION" ]; then
        VERSION=$(get_version_from_cargo "$CARGO_FILE")
        if [ $? -ne $EXIT_SUCCESS ] || [ -z "$VERSION" ]; then
            exit $EXIT_VALIDATION_FAILED
        fi
    fi
    
    # Validate inputs
    if ! validate_inputs; then
        exit $EXIT_VALIDATION_FAILED
    fi
    
    # Check dependencies
    if ! check_dependencies "git"; then
        exit $EXIT_MISSING_DEPS
    fi
    
    if [ "$VERBOSE" = true ]; then
        log_info "Starting release workflow"
        log_info "Repository: $REPO"
        log_info "Version: $VERSION"
        log_info "Wait time: $WAIT_SECONDS seconds"
        log_info "Cargo file: $CARGO_FILE"
        echo ""
    fi
    
    # Check if tag already exists
    if ! check_tag_exists "$VERSION"; then
        exit $EXIT_VALIDATION_FAILED
    fi
    
    # Confirmation prompt
    if [ "$FORCE" = false ] && [ "$YES" = false ]; then
        if ! confirm "Create release v$VERSION and update Homebrew formula?" "n"; then
            log_info "Release cancelled"
            exit $EXIT_SUCCESS
        fi
    fi
    
    # Step 1: Create and push tag
    log_info "1️⃣  Creating and pushing tag v$VERSION..."
    if ! create_and_push_tag "$VERSION"; then
        exit $EXIT_ERROR
    fi
    
    # Step 2: Wait for release
    log_info "2️⃣  Waiting for GitHub release to complete..."
    wait_for_release "$VERSION"
    
    # Step 3: Update Homebrew formula
    log_info "3️⃣  Updating Homebrew formula..."
    if ! update_homebrew_formula "$VERSION"; then
        log_error "Release created but Homebrew update failed"
        log_error "You may need to update the formula manually"
        exit $EXIT_ERROR
    fi
    
    # Show completion message
    show_completion_message "$VERSION"
    
    if [ "$VERBOSE" = true ]; then
        log_success "Release workflow completed"
    fi
}

# Script entry point
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
