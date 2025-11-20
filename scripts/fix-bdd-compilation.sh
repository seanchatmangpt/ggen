#!/bin/bash
# Fix compilation errors in BDD step definitions

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
STEPS_DIR="$PROJECT_ROOT/tests/bdd/steps"

echo "🔧 Fixing BDD compilation errors..."

# Fix regex patterns with quotes
echo "Fixing regex patterns..."
find "$STEPS_DIR" -name "*.rs" -exec sed -i.bak \
    -e 's/r"^I have an RDF file "([^"]+)"$"/r#"^I have an RDF file "([^"]+)"$"#/g' \
    -e 's/r"^I have a template "([^"]+)" with content:$"/r#"^I have a template "([^"]+)" with content:$"#/g' \
    -e 's/r"^I have a template "([^"]+)" with "([^"]+)"$"/r#"^I have a template "([^"]+)" with "([^"]+)"$"#/g' \
    {} \;

# Fix string literals
echo "Fixing string literals..."
find "$STEPS_DIR" -name "*.rs" -exec sed -i.bak \
    -e 's/"Failed to write file content"/"Failed to write file content"/g' \
    -e 's/"Failed to create template directory"/"Failed to create template directory"/g' \
    {} \;

# Remove duplicate functions in marketplace_steps.rs
echo "Removing duplicate functions in marketplace_steps.rs..."
MARKETPLACE_FILE="$STEPS_DIR/marketplace_steps.rs"

# Remove the second set of duplicate functions (lines 110-310)
if [[ -f "$MARKETPLACE_FILE" ]]; then
    # Create a backup
    cp "$MARKETPLACE_FILE" "$MARKETPLACE_FILE.backup"
    
    # Remove duplicate functions by keeping only the first occurrence
    awk '
    /^fn run_ggen_search\(/ && seen_search++ { skip=1; next }
    /^fn run_ggen_categories\(/ && seen_categories++ { skip=1; next }
    /^fn run_ggen_show\(/ && seen_show++ { skip=1; next }
    /^fn have_installed_package\(/ && seen_installed++ { skip=1; next }
    /^}$/ && skip { skip=0; next }
    !skip { print }
    ' "$MARKETPLACE_FILE.backup" > "$MARKETPLACE_FILE"
    
    echo "✅ Removed duplicate functions from marketplace_steps.rs"
fi

# Clean up backup files
find "$STEPS_DIR" -name "*.bak" -delete
rm -f "$MARKETPLACE_FILE.backup"

echo "🎯 BDD compilation fixes complete!"
