#!/bin/bash
# migrate_remaining_commands.sh
# Migrates all 67 remaining commands in priority order
#
# Usage: ./migrate_remaining_commands.sh [--dry-run]
#
# Priority Groups:
# 1. HIGH (User-facing, critical): 30 commands
# 2. MEDIUM (Important, frequently used): 25 commands
# 3. LOW (Utility, admin): 12 commands

set -euo pipefail

DRY_RUN=false
if [ "${1:-}" = "--dry-run" ]; then
    DRY_RUN=true
    echo "🔍 DRY RUN MODE - No files will be modified"
    echo ""
fi

# Tracking
TOTAL_COMMANDS=67
MIGRATED=0
FAILED=0
SKIPPED=0

# Log file
LOG_FILE="scripts/v2_migration/migration_log_$(date +%Y%m%d_%H%M%S).txt"
mkdir -p scripts/v2_migration

exec > >(tee -a "${LOG_FILE}") 2>&1

echo "========================================="
echo "ggen v2.0.0 Command Migration Automation"
echo "========================================="
echo "Start time: $(date)"
echo "Total commands to migrate: ${TOTAL_COMMANDS}"
echo ""

# HIGH PRIORITY (30 commands) - User-facing, critical
HIGH_PRIORITY=(
    "template:show"
    "template:lint"
    "template:regenerate"
    "template:generate-tree"
    "marketplace:search"
    "marketplace:install"
    "marketplace:update"
    "marketplace:publish"
    "project:new"
    "project:gen"
    "project:plan"
    "project:apply"
    "project:init"
    "project:build"
    "graph:query"
    "graph:load"
    "graph:export"
    "ai:analyze"
    "ai:chat"
    "ai:suggest"
    "ai:optimize"
    "ai:review"
    "hook:add"
    "hook:remove"
    "hook:list"
    "lifecycle:start"
    "lifecycle:stop"
    "lifecycle:status"
    "audit:security"
    "audit:compliance"
)

# MEDIUM PRIORITY (25 commands) - Important, frequently used
MEDIUM_PRIORITY=(
    "ci:workflow"
    "ci:setup"
    "ci:validate"
    "shell:completion"
    "shell:init"
    "utils:doctor"
    "utils:env"
    "utils:config"
    "template:validate"
    "template:diff"
    "template:merge"
    "marketplace:unpublish"
    "marketplace:versions"
    "marketplace:stats"
    "project:status"
    "project:clean"
    "project:archive"
    "graph:validate"
    "graph:merge"
    "graph:diff"
    "ai:config"
    "ai:models"
    "hook:enable"
    "hook:disable"
    "audit:report"
)

# LOW PRIORITY (12 commands) - Utility, admin
LOW_PRIORITY=(
    "template:archive"
    "template:restore"
    "marketplace:cache-clear"
    "marketplace:migrate"
    "project:migrate"
    "graph:optimize"
    "graph:stats"
    "ai:benchmark"
    "lifecycle:restart"
    "lifecycle:logs"
    "ci:cleanup"
    "utils:version-check"
)

migrate_command() {
    local cmd="$1"
    local priority="$2"

    IFS=':' read -r noun verb <<< "$cmd"

    echo "[$priority] Migrating: $noun $verb"

    # Check if already migrated
    if [ -f "cli/src/commands/${noun}/${verb}.rs" ]; then
        echo "  ⏭️  SKIPPED: Already exists"
        ((SKIPPED++))
        return 0
    fi

    # Check if domain logic exists
    if [ ! -f "cli/src/domain/${noun}/${verb}.rs" ]; then
        echo "  ⚠️  WARNING: No domain logic found at cli/src/domain/${noun}/${verb}.rs"
        echo "  Creating placeholder domain logic..."

        if [ "$DRY_RUN" = false ]; then
            mkdir -p "cli/src/domain/${noun}"
            cat > "cli/src/domain/${noun}/${verb}.rs" << EOF
//! ${noun^} ${verb} domain logic
//! TODO: Implement actual business logic

use ggen_utils::error::Result;

/// Execute ${verb} operation for ${noun}
pub async fn ${verb}_and_display(detailed: bool, json: bool) -> Result<()> {
    // TODO: Implement domain logic
    if json {
        println!("{{}}");
    } else {
        println!("${noun^} ${verb} - Not yet implemented");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_${verb}_basic() {
        let result = ${verb}_and_display(false, false).await;
        assert!(result.is_ok());
    }
}
EOF
        fi
    fi

    # Generate CLI wrapper
    if [ "$DRY_RUN" = false ]; then
        if ./scripts/v2_migration/generate_cli_wrapper.sh "$noun" "$verb" 2>&1 | grep -q "✅"; then
            echo "  ✅ SUCCESS: CLI wrapper created"
            ((MIGRATED++))
        else
            echo "  ❌ FAILED: CLI wrapper generation failed"
            ((FAILED++))
            return 1
        fi
    else
        echo "  [DRY RUN] Would generate: cli/src/commands/${noun}/${verb}.rs"
        ((MIGRATED++))
    fi
}

# Migrate HIGH priority first
echo "========================================="
echo "Phase 1: HIGH Priority (30 commands)"
echo "========================================="
for cmd in "${HIGH_PRIORITY[@]}"; do
    migrate_command "$cmd" "HIGH"
    echo ""
done

# Migrate MEDIUM priority
echo "========================================="
echo "Phase 2: MEDIUM Priority (25 commands)"
echo "========================================="
for cmd in "${MEDIUM_PRIORITY[@]}"; do
    migrate_command "$cmd" "MEDIUM"
    echo ""
done

# Migrate LOW priority
echo "========================================="
echo "Phase 3: LOW Priority (12 commands)"
echo "========================================="
for cmd in "${LOW_PRIORITY[@]}"; do
    migrate_command "$cmd" "LOW"
    echo ""
done

# Summary
echo "========================================="
echo "Migration Summary"
echo "========================================="
echo "Total commands: ${TOTAL_COMMANDS}"
echo "Migrated: ${MIGRATED}"
echo "Skipped (already exist): ${SKIPPED}"
echo "Failed: ${FAILED}"
echo "Success rate: $(awk "BEGIN {printf \"%.1f\", (${MIGRATED}/${TOTAL_COMMANDS})*100}")%"
echo ""
echo "End time: $(date)"
echo "Log file: ${LOG_FILE}"
echo ""

if [ $FAILED -gt 0 ]; then
    echo "⚠️  Some commands failed to migrate. Review the log file."
    exit 1
else
    echo "✅ All commands migrated successfully!"
    echo ""
    echo "Next steps:"
    echo "  1. Run: ./scripts/v2_migration/validate_migration.sh"
    echo "  2. Review generated files for TODOs"
    echo "  3. Implement domain logic for placeholders"
    echo "  4. Run full test suite: cargo test --all"
fi
