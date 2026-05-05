#!/usr/bin/env bash
# ConfigChange Auditor - Logs all changes to Claude Code configuration
# Inputs: JSON via stdin with keys: setting_key, old_value, new_value
# Output: Appends to .claude/evidence/config_changes.jsonl

set -euo pipefail

# Import shared utilities
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/utils.sh
source "${SCRIPT_DIR}/lib/utils.sh"

# Risk classification based on setting_key patterns
classify_risk() {
    local setting_key=$1

    # CRITICAL: Changes to hooks or verifier config
    if [[ "$setting_key" =~ ^\.claude/hooks/ ]] || \
       [[ "$setting_key" =~ verifier ]] || \
       [[ "$setting_key" =~ truth-gate ]]; then
        echo "CRITICAL"
    # HIGH: Changes to settings.json (adding/removing hooks)
    elif [[ "$setting_key" == "settings.json" ]] || \
         [[ "$setting_key" =~ hooks ]]; then
        echo "HIGH"
    # MEDIUM: Changes to skills/
    elif [[ "$setting_key" =~ ^\.claude/skills/ ]]; then
        echo "MEDIUM"
    # LOW: Changes to CLAUDE.md
    elif [[ "$setting_key" == "CLAUDE.md" ]]; then
        echo "LOW"
    else
        echo "LOW"
    fi
}

# Main audit function
audit_config_change() {
    local json_input

    # Read JSON from stdin
    json_input=$(cat)

    # Validate JSON
    if ! validate_json "$json_input"; then
        echo "Error: Invalid JSON input to config_change_auditor" >&2
        exit 1
    fi

    # Extract fields using jq
    local setting_key old_value new_value
    setting_key=$(echo "$json_input" | jq -r '.setting_key // empty')
    old_value=$(echo "$json_input" | jq -r '.old_value // empty')
    new_value=$(echo "$json_input" | jq -r '.new_value // empty')

    # Validate required fields
    if [[ -z "$setting_key" ]]; then
        echo "Error: Missing required field 'setting_key'" >&2
        exit 1
    fi

    # Classify risk level
    local risk_level
    risk_level=$(classify_risk "$setting_key")

    # Get metadata
    local timestamp actor_id
    timestamp=$(get_timestamp)
    actor_id=$(get_session_id)

    # Create JSONL record
    local jsonl_record
    jsonl_record=$(jq -n \
        --arg ts "$timestamp" \
        --arg key "$setting_key" \
        --arg old "$old_value" \
        --arg new "$new_value" \
        --arg risk "$risk_level" \
        --arg actor "$actor_id" \
        '{
            timestamp: $ts,
            setting_key: $key,
            old_value: $old,
            new_value: $new,
            risk_level: $risk,
            actor_id: $actor
        }')

    # Ensure evidence directory exists and append to JSONL
    local evidence_dir evidence_file
    evidence_dir=$(ensure_evidence_dir)
    evidence_file="${evidence_dir}/config_changes.jsonl"

    append_jsonl "$evidence_file" "$jsonl_record"

    # Output summary to stdout
    echo "Config change audited: $setting_key (risk: $risk_level)"
}

# Execute main function
audit_config_change "$@"
