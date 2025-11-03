#!/bin/bash
# Script to systematically remove clap from domain files
# Replace Args structs with Input structs
# Remove run() functions that use clap

set -e

echo "Removing clap from domain files..."

# Find all files with clap
FILES=$(find crates/ggen-domain/src -name "*.rs" -exec grep -l "clap::\|use clap" {} \;)

for file in $FILES; do
    echo "Processing $file..."
    # Remove clap imports
    sed -i '' '/^use clap::/d' "$file"
    # Replace Args with Input in struct names
    sed -i '' 's/#\[derive(Debug, Args)\]/#[derive(Debug, Clone, Default, Serialize, Deserialize)]/g' "$file"
    sed -i '' 's/#\[derive(Debug, Clone, Args)\]/#[derive(Debug, Clone, Default, Serialize, Deserialize)]/g' "$file"
    sed -i '' 's/#\[derive(Debug, Args, Clone)\]/#[derive(Debug, Clone, Default, Serialize, Deserialize)]/g' "$file"
    # Remove arg attributes
    sed -i '' '/#\[arg(/d' "$file"
    # Replace Args with Input in type names
    sed -i '' 's/pub struct \(.*\)Args/pub struct \1Input/g' "$file"
    sed -i '' 's/Args>/Input>/g' "$file"
    # Replace clap::Subcommand with enum
    sed -i '' 's/#\[derive(Debug, Clone, clap::Subcommand)\]/#[derive(Debug, Clone)]/g' "$file"
    sed -i '' 's/#\[derive(Debug, clap::Subcommand)\]/#[derive(Debug, Clone)]/g' "$file"
done

echo "Done removing clap!"

