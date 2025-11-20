#!/bin/bash
# Fix async step definitions in BDD tests
# Convert async functions to sync where Cucumber doesn't support async

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
STEPS_DIR="$PROJECT_ROOT/tests/bdd/steps"

echo "🔧 Converting async step definitions to sync..."

# Files to convert
FILES=(
    "project_steps.rs"
    "template_management_steps.rs" 
    "market_noun_verb_steps.rs"
    "graph_steps.rs"
)

for file in "${FILES[@]}"; do
    file_path="$STEPS_DIR/$file"
    if [[ -f "$file_path" ]]; then
        echo "Processing $file..."
        
        # Convert async fn to fn (but keep async where needed for command execution)
        # Only convert simple setup functions, not command execution functions
        sed -i.bak \
            -e 's/^async fn \([a-zA-Z_][a-zA-Z0-9_]*\)(world: &mut GgenWorld)$/fn \1(world: \&mut GgenWorld)/' \
            -e 's/^async fn \([a-zA-Z_][a-zA-Z0-9_]*\)(world: &mut GgenWorld, \([^)]*\))$/fn \1(world: \&mut GgenWorld, \2)/' \
            -e 's/^async fn \([a-zA-Z_][a-zA-Z0-9_]*\)(_world: &mut GgenWorld)$/fn \1(_world: \&mut GgenWorld)/' \
            -e 's/^async fn \([a-zA-Z_][a-zA-Z0-9_]*\)(_world: &mut GgenWorld, \([^)]*\))$/fn \1(_world: \&mut GgenWorld, \2)/' \
            "$file_path"
        
        # Remove backup files
        rm -f "$file_path.bak"
        
        echo "✅ Converted $file"
    else
        echo "⚠️  File not found: $file_path"
    fi
done

echo "🎯 Async step conversion complete!"
echo ""
echo "Note: Command execution functions (run_ggen_*_command) remain async"
echo "as they need to execute external processes."
