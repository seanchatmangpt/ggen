#!/bin/bash
# Fix compilation errors in new command implementations

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
CLI_DIR="$PROJECT_ROOT/cli/src/cmds"

echo "🔧 Fixing command compilation errors..."

# Fix error types and logger calls
find "$CLI_DIR" -name "*.rs" -exec sed -i.bak \
    -e 's/ggen_utils::error::Error::CommandFailed(/ggen_utils::error::Error::new(/g' \
    -e 's/ggen_utils::error::Error::InvalidInput(/ggen_utils::error::Error::new(/g' \
    -e 's/ggen_utils::error::Error::Io(/ggen_utils::error::Error::new(/g' \
    -e 's/ggen_utils::error::Error::Serialization(/ggen_utils::error::Error::new(/g' \
    -e 's/ggen_utils::error::Error::Deserialization(/ggen_utils::error::Error::new(/g' \
    -e 's/logger::info(/println!(/g' \
    -e 's/logger::warn(/println!(/g' \
    {} \;

# Clean up backup files
find "$CLI_DIR" -name "*.bak" -delete

echo "🎯 Command compilation fixes complete!"
