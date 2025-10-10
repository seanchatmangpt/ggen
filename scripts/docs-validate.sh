#!/usr/bin/env bash
# Validate documentation structure and links

# Source common library
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

# Script setup
setup_script "docs-validate" "Validate documentation structure and links"

# Configuration
readonly DEFAULT_DOCS_DIR="docs/book"
readonly DEFAULT_REQUIRED_FILES="index.html,marketplace.html"
readonly DEFAULT_TIMEOUT="10"

# Environment variables
DOCS_DIR="${DOCS_DIR:-$DEFAULT_DOCS_DIR}"
REQUIRED_FILES="${REQUIRED_FILES:-$DEFAULT_REQUIRED_FILES}"
TIMEOUT="${TIMEOUT:-$DEFAULT_TIMEOUT}"

# Global variables
VERBOSE=false
JSON_OUTPUT=false
FIX_ISSUES=false
CHECK_LINKS=false
ERRORS=0
WARNINGS=0

# Help function
show_help() {
    show_help_template \
        "[OPTIONS]" \
        "Validate documentation structure and links. Checks for required files, broken links, and missing images." \
        "    -h, --help          Show this help message
    -v, --verbose        Enable verbose output
    -j, --json           Output results in JSON format
    -f, --fix            Attempt to fix common issues automatically
    -l, --links           Check for broken links
    -d, --docs-dir DIR    Docs directory to check (default: docs/book)
    --required-files LIST Comma-separated list of required files
    -t, --timeout SEC    HTTP request timeout for link checking (default: 10)" \
        "    $SCRIPT_NAME                    # Validate with defaults
    $SCRIPT_NAME --verbose              # Enable verbose output
    $SCRIPT_NAME --json                 # Output in JSON format
    $SCRIPT_NAME --fix                  # Attempt to fix issues
    $SCRIPT_NAME --links                # Check for broken links
    $SCRIPT_NAME --docs-dir docs/build  # Use custom docs directory" \
        "    DOCS_DIR             Docs directory to check (default: $DEFAULT_DOCS_DIR)
    REQUIRED_FILES       Comma-separated list of required files (default: $DEFAULT_REQUIRED_FILES)
    TIMEOUT              HTTP request timeout for link checking (default: $DEFAULT_TIMEOUT)
    DEBUG                Enable debug output (default: false)"
}

# Validation functions
validate_inputs() {
    log_debug "Validating inputs..."
    
    if ! validate_directory_exists "$DOCS_DIR"; then
        log_error "Docs directory not found: $DOCS_DIR"
        log_error "Run: cargo make docs-build"
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! [[ "$TIMEOUT" =~ ^[0-9]+$ ]] || [ "$TIMEOUT" -lt 1 ] || [ "$TIMEOUT" -gt 300 ]; then
        log_error "Invalid timeout: $TIMEOUT"
        log_error "Timeout must be between 1 and 300 seconds"
        return $EXIT_VALIDATION_FAILED
    fi
    
    return $EXIT_SUCCESS
}

# Documentation validation functions
check_required_files() {
    log_debug "Checking required files..."
    
    IFS=',' read -ra required_files_array <<< "$REQUIRED_FILES"
    local missing_files=()
    local found_files=()
    
    for file in "${required_files_array[@]}"; do
        local file_path="$DOCS_DIR/$file"
        if [ -f "$file_path" ]; then
            found_files+=("$file")
        else
            missing_files+=("$file")
            ERRORS=$((ERRORS + 1))
        fi
    done
    
    if [ "$JSON_OUTPUT" = true ]; then
        if [ ${#missing_files[@]} -eq 0 ]; then
            echo "{\"check\":\"required_files\",\"status\":\"pass\",\"message\":\"All required files found\",\"found\":$(printf '%s\n' "${found_files[@]}" | jq -R . | jq -s .)}"
        else
            echo "{\"check\":\"required_files\",\"status\":\"fail\",\"message\":\"Missing ${#missing_files[@]} required files\",\"missing\":$(printf '%s\n' "${missing_files[@]}" | jq -R . | jq -s .),\"found\":$(printf '%s\n' "${found_files[@]}" | jq -R . | jq -s .)}"
        fi
    else
        if [ ${#missing_files[@]} -eq 0 ]; then
            log_success "All required files found"
            for file in "${found_files[@]}"; do
                log_info "      ✅ $file"
            done
        else
            log_failure "Missing ${#missing_files[@]} required files"
            for file in "${missing_files[@]}"; do
                log_info "      ❌ $file"
            done
            for file in "${found_files[@]}"; do
                log_info "      ✅ $file"
            done
        fi
    fi
    
    return $EXIT_SUCCESS
}

check_file_sizes() {
    log_debug "Checking file sizes..."
    
    local large_files=()
    local empty_files=()
    
    # Use process substitution to avoid subshell issues
    while IFS= read -r file; do
        local size
        size=$(wc -c < "$file" | tr -d ' ')
        local filename
        filename=$(basename "$file")
        
        if [ "$size" -eq 0 ]; then
            empty_files+=("$filename")
            WARNINGS=$((WARNINGS + 1))
        elif [ "$size" -gt 1048576 ]; then
            large_files+=("$filename")
            WARNINGS=$((WARNINGS + 1))
        fi
    done < <(find "$DOCS_DIR" -type f -name "*.html")
    
    if [ "$JSON_OUTPUT" = true ]; then
        if [ ${#large_files[@]} -eq 0 ] && [ ${#empty_files[@]} -eq 0 ]; then
            echo "{\"check\":\"file_sizes\",\"status\":\"pass\",\"message\":\"All files have reasonable sizes\"}"
        else
            echo "{\"check\":\"file_sizes\",\"status\":\"warn\",\"message\":\"Found ${#large_files[@]} large files and ${#empty_files[@]} empty files\",\"large_files\":$(printf '%s\n' "${large_files[@]}" | jq -R . | jq -s .),\"empty_files\":$(printf '%s\n' "${empty_files[@]}" | jq -R . | jq -s .)}"
        fi
    else
        if [ ${#large_files[@]} -eq 0 ] && [ ${#empty_files[@]} -eq 0 ]; then
            log_success "All files have reasonable sizes"
        else
            log_warn "Found ${#large_files[@]} large files and ${#empty_files[@]} empty files"
            for file in "${large_files[@]}"; do
                log_info "      ⚠️  Large file: $file"
            done
            for file in "${empty_files[@]}"; do
                log_info "      ⚠️  Empty file: $file"
            done
        fi
    fi
    
    return $EXIT_SUCCESS
}

check_broken_links() {
    log_debug "Checking for broken links..."
    
    if [ "$CHECK_LINKS" = false ]; then
        if [ "$JSON_OUTPUT" = true ]; then
            echo "{\"check\":\"broken_links\",\"status\":\"skip\",\"message\":\"Link checking disabled\"}"
        else
            log_info "Link checking disabled (use --links to enable)"
        fi
        return $EXIT_SUCCESS
    fi
    
    if ! command -v curl &> /dev/null; then
        if [ "$JSON_OUTPUT" = true ]; then
            echo "{\"check\":\"broken_links\",\"status\":\"warn\",\"message\":\"curl not available for link checking\"}"
        else
            log_warn "curl not available for link checking"
        fi
        return $EXIT_ERROR
    fi
    
    local broken_links=()
    local total_links=0
    
    # Find all HTML files and extract links
    while IFS= read -r url; do
        total_links=$((total_links + 1))
        local http_status
        http_status=$(curl -s -o /dev/null -w "%{http_code}" --max-time "$TIMEOUT" "$url" 2>/dev/null || echo "000")
        
        if [ "$http_status" != "200" ]; then
            broken_links+=("$url")
            ERRORS=$((ERRORS + 1))
        fi
    done < <(find "$DOCS_DIR" -name "*.html" -exec grep -o 'href="[^"]*"' {} \; | \
        sed 's/href="//;s/"//' | \
        grep -E '^https?://' | \
        sort -u)
    
    if [ "$JSON_OUTPUT" = true ]; then
        if [ ${#broken_links[@]} -eq 0 ]; then
            echo "{\"check\":\"broken_links\",\"status\":\"pass\",\"message\":\"All $total_links links are working\"}"
        else
            echo "{\"check\":\"broken_links\",\"status\":\"fail\",\"message\":\"Found ${#broken_links[@]} broken links out of $total_links\",\"broken_links\":$(printf '%s\n' "${broken_links[@]}" | jq -R . | jq -s .)}"
        fi
    else
        if [ ${#broken_links[@]} -eq 0 ]; then
            log_success "All $total_links links are working"
        else
            log_failure "Found ${#broken_links[@]} broken links out of $total_links"
            for link in "${broken_links[@]}"; do
                log_info "      ❌ $link"
            done
        fi
    fi
    
    return $EXIT_SUCCESS
}

check_missing_images() {
    log_debug "Checking for missing images..."
    
    local missing_images=()
    local total_images=0
    
    # Find all HTML files and extract image references
    while IFS= read -r img_path; do
        total_images=$((total_images + 1))
        local full_path="$DOCS_DIR/$img_path"
        
        if [ ! -f "$full_path" ]; then
            missing_images+=("$img_path")
            ERRORS=$((ERRORS + 1))
        fi
    done < <(find "$DOCS_DIR" -name "*.html" -exec grep -o 'src="[^"]*"' {} \; | \
        sed 's/src="//;s/"//' | \
        grep -E '\.(png|jpg|jpeg|gif|svg|webp)$' | \
        sort -u)
    
    if [ "$JSON_OUTPUT" = true ]; then
        if [ ${#missing_images[@]} -eq 0 ]; then
            echo "{\"check\":\"missing_images\",\"status\":\"pass\",\"message\":\"All $total_images images found\"}"
        else
            echo "{\"check\":\"missing_images\",\"status\":\"fail\",\"message\":\"Found ${#missing_images[@]} missing images out of $total_images\",\"missing_images\":$(printf '%s\n' "${missing_images[@]}" | jq -R . | jq -s .)}"
        fi
    else
        if [ ${#missing_images[@]} -eq 0 ]; then
            log_success "All $total_images images found"
        else
            log_failure "Found ${#missing_images[@]} missing images out of $total_images"
            for img in "${missing_images[@]}"; do
                log_info "      ❌ $img"
            done
        fi
    fi
    
    return $EXIT_SUCCESS
}

fix_common_issues() {
    log_debug "Attempting to fix common issues..."
    
    if [ "$FIX_ISSUES" = false ]; then
        return $EXIT_SUCCESS
    fi
    
    local fixes_applied=0
    
    # Fix empty HTML files by rebuilding docs
    if find "$DOCS_DIR" -name "*.html" -size 0 | grep -q .; then
        log_info "Rebuilding documentation to fix empty files..."
        if cargo make docs-build &>/dev/null; then
            log_success "Documentation rebuilt successfully"
            fixes_applied=$((fixes_applied + 1))
        else
            log_error "Failed to rebuild documentation"
        fi
    fi
    
    if [ "$JSON_OUTPUT" = true ]; then
        echo "{\"check\":\"fix_issues\",\"status\":\"pass\",\"message\":\"Applied $fixes_applied fixes\"}"
    else
        if [ $fixes_applied -gt 0 ]; then
            log_success "Applied $fixes_applied fixes"
        else
            log_info "No fixes were needed"
        fi
    fi
    
    return $EXIT_SUCCESS
}

show_summary() {
    if [ "$JSON_OUTPUT" = true ]; then
        local exit_code=0
        if [ $ERRORS -gt 0 ]; then
            exit_code=1
        elif [ $WARNINGS -gt 0 ]; then
            exit_code=0
        fi
        
        echo "{\"summary\":{\"errors\":$ERRORS,\"warnings\":$WARNINGS,\"total_checks\":4,\"exit_code\":$exit_code}}"
    else
        echo ""
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "📊 Summary:"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "Errors:   $ERRORS"
        echo "Warnings: $WARNINGS"
        echo ""
        
        if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
            log_success "All documentation validation checks passed!"
        elif [ $ERRORS -eq 0 ]; then
            log_warn "Documentation is functional but has warnings"
        else
            log_failure "Documentation has errors that need to be fixed"
            echo ""
            log_info "💡 Next steps:"
            log_info "   1. Fix missing files and broken links"
            log_info "   2. Rebuild documentation: cargo make docs-build"
            log_info "   3. Check for missing images and assets"
        fi
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
            -j|--json)
                JSON_OUTPUT=true
                shift
                ;;
            -f|--fix)
                FIX_ISSUES=true
                shift
                ;;
            -l|--links)
                CHECK_LINKS=true
                shift
                ;;
            -d|--docs-dir)
                DOCS_DIR="$2"
                shift 2
                ;;
            --required-files)
                REQUIRED_FILES="$2"
                shift 2
                ;;
            -t|--timeout)
                TIMEOUT="$2"
                shift 2
                ;;
            *)
                log_error "Unknown option: $1"
                show_help
                exit $EXIT_INVALID_ARGS
                ;;
        esac
    done
    
    # Validate inputs
    if ! validate_inputs; then
        exit $EXIT_VALIDATION_FAILED
    fi
    
    # Check dependencies
    if [ "$CHECK_LINKS" = true ] && ! check_dependencies "curl"; then
        exit $EXIT_MISSING_DEPS
    fi
    
    if [ "$VERBOSE" = true ] && [ "$JSON_OUTPUT" = false ]; then
        log_info "Starting documentation validation"
        log_info "Docs directory: $DOCS_DIR"
        log_info "Required files: $REQUIRED_FILES"
        log_info "Check links: $CHECK_LINKS"
        log_info "Fix issues: $FIX_ISSUES"
        echo ""
    fi
    
    # Perform checks
    check_required_files
    check_file_sizes
    check_broken_links
    check_missing_images
    
    # Attempt fixes if requested
    if [ "$FIX_ISSUES" = true ]; then
        fix_common_issues
    fi
    
    # Show summary
    show_summary
    
    if [ "$VERBOSE" = true ] && [ "$JSON_OUTPUT" = false ]; then
        log_success "Documentation validation completed"
    fi
    
    # Exit with appropriate code
    if [ $ERRORS -gt 0 ]; then
        exit $EXIT_ERROR
    elif [ $WARNINGS -gt 0 ]; then
        exit $EXIT_SUCCESS
    else
        exit $EXIT_SUCCESS
    fi
}

# Script entry point
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
