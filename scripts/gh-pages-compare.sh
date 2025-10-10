#!/usr/bin/env bash
# Compare local docs build with deployed version

# Source common library
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

# Script setup
setup_script "gh-pages-compare" "Compare local docs build with deployed version"

# Configuration
readonly DEFAULT_LOCAL_DIR="docs/book"
readonly DEFAULT_REMOTE_URL="https://seanchatmangpt.github.io/ggen"
readonly DEFAULT_KEY_FILES="index.html,marketplace.html,searchindex.js"
readonly DEFAULT_TIMEOUT="30"

# Environment variables
readonly LOCAL_DIR="${LOCAL_DIR:-$DEFAULT_LOCAL_DIR}"
readonly REMOTE_URL="${GITHUB_PAGES_URL:-$DEFAULT_REMOTE_URL}"
readonly KEY_FILES="${KEY_FILES:-$DEFAULT_KEY_FILES}"
readonly TIMEOUT="${TIMEOUT:-$DEFAULT_TIMEOUT}"

# Global variables
VERBOSE=false
JSON_OUTPUT=false
SHOW_DIFF=false
TEMP_DIR=""

# Help function
show_help() {
    show_help_template \
        "[OPTIONS]" \
        "Compare local docs build with deployed version. Downloads the deployed version and compares key files, sizes, and structure." \
        "    -h, --help          Show this help message
    -v, --verbose        Enable verbose output
    -j, --json           Output results in JSON format
    -d, --diff           Show detailed file differences
    -l, --local DIR      Local docs directory (default: docs/book)
    -r, --remote URL     Remote GitHub Pages URL
    -k, --key-files LIST Comma-separated list of key files to compare
    -t, --timeout SEC    Download timeout in seconds (default: 30)" \
        "    $SCRIPT_NAME                    # Compare with defaults
    $SCRIPT_NAME --verbose              # Enable verbose output
    $SCRIPT_NAME --json                 # Output in JSON format
    $SCRIPT_NAME --diff                 # Show detailed differences
    $SCRIPT_NAME --local docs/build     # Use custom local directory
    $SCRIPT_NAME --key-files \"a.html,b.html\" # Check specific files" \
        "    LOCAL_DIR            Local docs directory (default: $DEFAULT_LOCAL_DIR)
    GITHUB_PAGES_URL     Remote GitHub Pages URL (default: $DEFAULT_REMOTE_URL)
    KEY_FILES            Comma-separated list of key files (default: $DEFAULT_KEY_FILES)
    TIMEOUT              Download timeout in seconds (default: $DEFAULT_TIMEOUT)
    DEBUG                Enable debug output (default: false)"
}

# Validation functions
validate_inputs() {
    log_debug "Validating inputs..."
    
    if ! validate_directory_exists "$LOCAL_DIR"; then
        log_error "Local build not found at $LOCAL_DIR"
        log_error "Run: cargo make docs-build"
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! validate_url "$REMOTE_URL"; then
        return $EXIT_VALIDATION_FAILED
    fi
    
    if ! [[ "$TIMEOUT" =~ ^[0-9]+$ ]] || [ "$TIMEOUT" -lt 1 ] || [ "$TIMEOUT" -gt 300 ]; then
        log_error "Invalid timeout: $TIMEOUT"
        log_error "Timeout must be a number between 1 and 300 seconds"
        return $EXIT_VALIDATION_FAILED
    fi
    
    return $EXIT_SUCCESS
}

# File comparison functions
check_remote_accessibility() {
    log_debug "Checking remote site accessibility..."
    
    local http_status
    http_status=$(curl -s -o /dev/null -w "%{http_code}" --max-time "$TIMEOUT" "$REMOTE_URL" 2>/dev/null || echo "000")
    
    if [ "$http_status" != "200" ]; then
        log_error "Remote site not accessible (HTTP $http_status): $REMOTE_URL"
        log_error "Run: cargo make gh-pages-status"
        return $EXIT_NETWORK_ERROR
    fi
    
    return $EXIT_SUCCESS
}

download_remote_version() {
    log_debug "Downloading deployed version..."
    
    TEMP_DIR=$(create_temp_dir "gh-pages-compare")
    add_temp_dir "$TEMP_DIR"
    
    if [ "$VERBOSE" = true ]; then
        log_info "Downloading from: $REMOTE_URL"
        log_info "Saving to: $TEMP_DIR"
    fi
    
    if command -v wget &> /dev/null; then
        wget -q -r -np -nH -P "$TEMP_DIR" "$REMOTE_URL" 2>&1 || log_warn "Some files could not be downloaded"
    elif command -v curl &> /dev/null; then
        # Fallback to curl if wget is not available
        log_warn "wget not available, using curl (limited functionality)"
        curl -s --max-time "$TIMEOUT" "$REMOTE_URL" > "$TEMP_DIR/index.html" 2>/dev/null || log_warn "Could not download main page"
    else
        log_error "Neither wget nor curl available for downloading"
        return $EXIT_MISSING_DEPS
    fi
    
    return $EXIT_SUCCESS
}

compare_file_counts() {
    log_debug "Comparing file counts..."
    
    local local_count
    local remote_count
    local_count=$(find "$LOCAL_DIR" -type f | wc -l | tr -d ' ')
    remote_count=$(find "$TEMP_DIR" -type f | wc -l | tr -d ' ')
    
    if [ "$JSON_OUTPUT" = true ]; then
        echo "{\"local_files\":$local_count,\"remote_files\":$remote_count,\"difference\":$((local_count - remote_count))}"
    else
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "📊 File Comparison:"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "Local files:  $local_count"
        echo "Remote files: $remote_count"
        echo ""
    fi
}

compare_key_files() {
    log_debug "Comparing key files..."
    
    # Convert comma-separated string to array
    IFS=',' read -ra key_files_array <<< "$KEY_FILES"
    
    if [ "$JSON_OUTPUT" = true ]; then
        local json_results=()
        for file in "${key_files_array[@]}"; do
            local local_file="$LOCAL_DIR/$file"
            local remote_file="$TEMP_DIR/$file"
            local result=""
            
            if [ -f "$local_file" ] && [ -f "$remote_file" ]; then
                local local_size
                local remote_size
                local_size=$(wc -c < "$local_file" | tr -d ' ')
                remote_size=$(wc -c < "$remote_file" | tr -d ' ')
                
                if [ "$local_size" -eq "$remote_size" ]; then
                    result="{\"file\":\"$file\",\"status\":\"same\",\"local_size\":$local_size,\"remote_size\":$remote_size}"
                else
                    result="{\"file\":\"$file\",\"status\":\"different\",\"local_size\":$local_size,\"remote_size\":$remote_size}"
                fi
            elif [ -f "$local_file" ]; then
                result="{\"file\":\"$file\",\"status\":\"local_only\"}"
            elif [ -f "$remote_file" ]; then
                result="{\"file\":\"$file\",\"status\":\"remote_only\"}"
            else
                result="{\"file\":\"$file\",\"status\":\"missing\"}"
            fi
            
            json_results+=("$result")
        done
        
        # Output as JSON array
        echo "["
        local first=true
        for result in "${json_results[@]}"; do
            if [ "$first" = true ]; then
                first=false
            else
                echo ","
            fi
            echo -n "$result"
        done
        echo ""
        echo "]"
    else
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "🔍 Key File Comparison:"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        
        for file in "${key_files_array[@]}"; do
            local local_file="$LOCAL_DIR/$file"
            local remote_file="$TEMP_DIR/$file"
            
            if [ -f "$local_file" ] && [ -f "$remote_file" ]; then
                local local_size
                local remote_size
                local_size=$(wc -c < "$local_file" | tr -d ' ')
                remote_size=$(wc -c < "$remote_file" | tr -d ' ')
                
                if [ "$local_size" -eq "$remote_size" ]; then
                    log_success "$file: Same size ($local_size bytes)"
                else
                    log_warn "$file: Different sizes (Local: $local_size, Remote: $remote_size)"
                fi
            elif [ -f "$local_file" ]; then
                log_warn "$file: Only in local build"
            elif [ -f "$remote_file" ]; then
                log_warn "$file: Only in remote deployment"
            else
                log_failure "$file: Missing in both"
            fi
        done
        echo ""
    fi
}

show_detailed_diff() {
    if [ "$SHOW_DIFF" = true ] && [ "$JSON_OUTPUT" = false ]; then
        log_info "💡 Detailed diff available at: $TEMP_DIR"
        
        if command -v diff &> /dev/null; then
            echo ""
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            echo "🔍 Detailed File Differences:"
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            
            IFS=',' read -ra key_files_array <<< "$KEY_FILES"
            for file in "${key_files_array[@]}"; do
                local local_file="$LOCAL_DIR/$file"
                local remote_file="$TEMP_DIR/$file"
                
                if [ -f "$local_file" ] && [ -f "$remote_file" ]; then
                    local local_size
                    local remote_size
                    local_size=$(wc -c < "$local_file" | tr -d ' ')
                    remote_size=$(wc -c < "$remote_file" | tr -d ' ')
                    
                    if [ "$local_size" -ne "$remote_size" ]; then
                        echo "--- $file (Local: $local_size bytes, Remote: $remote_size bytes) ---"
                        diff "$local_file" "$remote_file" || true
                        echo ""
                    fi
                fi
            done
        else
            log_warn "diff command not available for detailed comparison"
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
            -d|--diff)
                SHOW_DIFF=true
                shift
                ;;
            -l|--local)
                LOCAL_DIR="$2"
                shift 2
                ;;
            -r|--remote)
                REMOTE_URL="$2"
                shift 2
                ;;
            -k|--key-files)
                KEY_FILES="$2"
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
    if ! check_dependencies "curl"; then
        exit $EXIT_MISSING_DEPS
    fi
    
    if [ "$VERBOSE" = true ]; then
        log_info "Starting docs comparison"
        log_info "Local directory: $LOCAL_DIR"
        log_info "Remote URL: $REMOTE_URL"
        log_info "Key files: $KEY_FILES"
        log_info "Timeout: $TIMEOUT"
        echo ""
    fi
    
    # Check remote accessibility
    if ! check_remote_accessibility; then
        exit $EXIT_NETWORK_ERROR
    fi
    
    # Download remote version
    if ! download_remote_version; then
        exit $EXIT_NETWORK_ERROR
    fi
    
    # Perform comparisons
    compare_file_counts
    compare_key_files
    show_detailed_diff
    
    if [ "$VERBOSE" = true ]; then
        log_success "Docs comparison completed"
    fi
}

# Script entry point
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
