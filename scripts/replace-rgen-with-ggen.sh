#!/bin/bash

# Replace ggen with ggen - Case Preserving Script
# This script replaces "ggen" with "ggen" while preserving case variations

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to replace with case preservation
replace_case_preserving() {
    local file="$1"
    local temp_file=$(mktemp)
    
    # Use sed to replace with case preservation
    # ggen -> ggen
    # Ggen -> Ggen  
    # GGEN -> GGEN
    # gGen -> gGen
    # GGen -> GGen
    sed -E '
        s/ggen/ggen/g
        s/Ggen/Ggen/g
        s/GGEN/GGEN/g
        s/gGen/gGen/g
        s/GGen/GGen/g
    ' "$file" > "$temp_file"
    
    # Check if file was actually changed
    if ! cmp -s "$file" "$temp_file"; then
        mv "$temp_file" "$file"
        return 0  # File was changed
    else
        rm "$temp_file"
        return 1  # File was not changed
    fi
}

# Function to process a single file
process_file() {
    local file="$1"
    
    # Skip binary files and certain directories
    if [[ "$file" =~ \.(png|jpg|jpeg|gif|ico|svg|woff|woff2|ttf|eot|pdf|zip|tar|gz)$ ]]; then
        return 0
    fi
    
    # Skip certain directories
    if [[ "$file" =~ /(target/|\.git/|node_modules/|\.cargo/) ]]; then
        return 0
    fi
    
    # Check if file contains "ggen" (case insensitive)
    if grep -qi "ggen" "$file" 2>/dev/null; then
        print_status "Processing: $file"
        
        if replace_case_preserving "$file"; then
            print_success "Updated: $file"
            return 0
        else
            print_warning "No changes needed: $file"
            return 1
        fi
    fi
    
    return 1
}

# Function to show usage
show_usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Replace 'ggen' with 'ggen' while preserving case variations"
    echo ""
    echo "Options:"
    echo "  -h, --help     Show this help message"
    echo "  -d, --dry-run  Show what would be changed without making changes"
    echo "  -v, --verbose  Verbose output"
    echo "  --check        Check for remaining 'ggen' references"
    echo ""
    echo "Examples:"
    echo "  $0                    # Replace all instances"
    echo "  $0 --dry-run          # Show what would be changed"
    echo "  $0 --check            # Check for remaining references"
}

# Parse command line arguments
DRY_RUN=false
VERBOSE=false
CHECK_ONLY=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            show_usage
            exit 0
            ;;
        -d|--dry-run)
            DRY_RUN=true
            shift
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        --check)
            CHECK_ONLY=true
            shift
            ;;
        *)
            print_error "Unknown option: $1"
            show_usage
            exit 1
            ;;
    esac
done

# Main execution
main() {
    print_status "Starting ggen -> ggen replacement"
    
    if [[ "$CHECK_ONLY" == "true" ]]; then
        print_status "Checking for remaining 'ggen' references..."
        
        # Find all files with "ggen" references
        local files_with_ggen
        files_with_ggen=$(find . -type f -not -path "./target/*" -not -path "./.git/*" -not -path "./node_modules/*" -not -path "./.cargo/*" -exec grep -l -i "ggen" {} \; 2>/dev/null || true)
        
        if [[ -n "$files_with_ggen" ]]; then
            print_warning "Found files with 'ggen' references:"
            echo "$files_with_ggen" | while read -r file; do
                echo "  $file"
                if [[ "$VERBOSE" == "true" ]]; then
                    grep -n -i "ggen" "$file" | head -5 | sed 's/^/    /'
                fi
            done
            exit 1
        else
            print_success "No 'ggen' references found!"
            exit 0
        fi
    fi
    
    if [[ "$DRY_RUN" == "true" ]]; then
        print_status "DRY RUN MODE - No files will be modified"
    fi
    
    # Find all files to process
    local files_to_process
    files_to_process=$(find . -type f -not -path "./target/*" -not -path "./.git/*" -not -path "./node_modules/*" -not -path "./.cargo/*" -exec grep -l -i "ggen" {} \; 2>/dev/null || true)
    
    if [[ -z "$files_to_process" ]]; then
        print_success "No files with 'ggen' references found!"
        exit 0
    fi
    
    local total_files=0
    local changed_files=0
    
    # Count total files
    total_files=$(echo "$files_to_process" | wc -l)
    print_status "Found $total_files files with 'ggen' references"
    
    # Process each file
    echo "$files_to_process" | while read -r file; do
        if [[ "$DRY_RUN" == "true" ]]; then
            print_status "Would process: $file"
            if [[ "$VERBOSE" == "true" ]]; then
                grep -n -i "ggen" "$file" | head -3 | sed 's/^/    /'
            fi
        else
            if process_file "$file"; then
                ((changed_files++))
            fi
        fi
    done
    
    if [[ "$DRY_RUN" == "true" ]]; then
        print_status "Dry run completed. $total_files files would be processed."
    else
        print_success "Replacement completed!"
        print_status "Processed $total_files files"
    fi
    
    # Final check
    print_status "Running final check..."
    local remaining_refs
    remaining_refs=$(find . -type f -not -path "./target/*" -not -path "./.git/*" -not -path "./node_modules/*" -not -path "./.cargo/*" -exec grep -l -i "ggen" {} \; 2>/dev/null | wc -l)
    
    if [[ "$remaining_refs" -eq 0 ]]; then
        print_success "All 'ggen' references have been replaced with 'ggen'!"
    else
        print_warning "Still found $remaining_refs files with 'ggen' references"
        if [[ "$VERBOSE" == "true" ]]; then
            find . -type f -not -path "./target/*" -not -path "./.git/*" -not -path "./node_modules/*" -not -path "./.cargo/*" -exec grep -l -i "ggen" {} \; 2>/dev/null | head -5
        fi
    fi
}

# Run main function
main "$@"
