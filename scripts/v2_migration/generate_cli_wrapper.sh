#!/bin/bash
# generate_cli_wrapper.sh
# Auto-generates CLI wrapper for a command following v2.0.0 pattern
#
# Usage: ./generate_cli_wrapper.sh <noun> <verb>
# Example: ./generate_cli_wrapper.sh template show
#
# This script:
# 1. Creates cli/src/commands/{noun}/{verb}.rs
# 2. Extracts domain logic reference from existing code
# 3. Generates clap Args struct with #[verb] pattern
# 4. Creates execute() function calling domain layer
# 5. Adds unit tests

set -euo pipefail

NOUN="$1"
VERB="$2"

# Paths
CLI_DIR="cli/src/commands/${NOUN}"
DOMAIN_DIR="cli/src/domain/${NOUN}"
OUTPUT_FILE="${CLI_DIR}/${VERB}.rs"

# Create directory if needed
mkdir -p "${CLI_DIR}"

# Check if domain logic exists
DOMAIN_FILE="${DOMAIN_DIR}/${VERB}.rs"
if [ ! -f "${DOMAIN_FILE}" ]; then
    echo "⚠️  Warning: Domain file not found at ${DOMAIN_FILE}"
    echo "    You may need to create domain logic first."
fi

# Generate CLI wrapper
cat > "${OUTPUT_FILE}" << 'EOF'
//! {NOUN_TITLE} {VERB} command - CLI layer
//!
//! This module provides the CLI interface for {VERB}ing {NOUN}s.
//! Uses clap-noun-verb v3.0.0 #[verb] pattern with Chicago TDD (Classicist School).

use clap::Args;
use ggen_utils::error::Result;
use crate::runtime;

/// {DESCRIPTION}
///
/// # Examples
///
/// ```bash
/// ggen {NOUN} {VERB}
/// ggen {NOUN} {VERB} --detailed
/// ggen {NOUN} {VERB} --json
/// ```
#[derive(Args, Debug)]
#[command(name = "{VERB}", about = "{DESCRIPTION}")]
pub struct {VERB_PASCAL}Args {
    /// Show detailed information
    #[arg(long)]
    pub detailed: bool,

    /// Output as JSON
    #[arg(long)]
    pub json: bool,

    // TODO: Add command-specific arguments here
}

/// Execute {NOUN} {VERB} command
pub fn run(args: &{VERB_PASCAL}Args) -> Result<()> {
    runtime::execute(async {
        crate::domain::{NOUN}::{VERB}::{VERB}_and_display(
            args.detailed,
            args.json
        ).await
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_{VERB}_args_parsing() {
        let args = {VERB_PASCAL}Args {
            detailed: true,
            json: false,
        };

        assert!(args.detailed);
        assert!(!args.json);
    }

    #[test]
    fn test_{VERB}_args_defaults() {
        let args = {VERB_PASCAL}Args {
            detailed: false,
            json: false,
        };

        assert!(!args.detailed);
        assert!(!args.json);
    }
}
EOF

# Replace placeholders
NOUN_TITLE="$(echo ${NOUN} | sed 's/./\U&/')"
VERB_PASCAL="$(echo ${VERB} | sed 's/./\U&/')"
DESCRIPTION="${VERB_PASCAL} ${NOUN}"

sed -i '' "s/{NOUN}/${NOUN}/g" "${OUTPUT_FILE}"
sed -i '' "s/{VERB}/${VERB}/g" "${OUTPUT_FILE}"
sed -i '' "s/{NOUN_TITLE}/${NOUN_TITLE}/g" "${OUTPUT_FILE}"
sed -i '' "s/{VERB_PASCAL}/${VERB_PASCAL}/g" "${OUTPUT_FILE}"
sed -i '' "s/{DESCRIPTION}/${DESCRIPTION}/g" "${OUTPUT_FILE}"

echo "✅ Generated CLI wrapper: ${OUTPUT_FILE}"
echo ""
echo "📝 Next steps:"
echo "   1. Review generated file and customize arguments"
echo "   2. Ensure domain logic exists at ${DOMAIN_FILE}"
echo "   3. Add to mod.rs: pub mod ${VERB};"
echo "   4. Update noun command to include verb"
echo "   5. Run tests: cargo test ${NOUN}_${VERB}"
echo ""
echo "💡 Tip: Use ./validate_migration.sh to check compilation"
