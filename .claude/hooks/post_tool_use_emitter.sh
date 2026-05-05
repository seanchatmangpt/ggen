#!/usr/bin/env bash
# PostToolUse Evidence Emitter
# Records work unit metadata to evidence log
# Input: JSON via stdin with tool_name, result, file_paths, timestamp, actor_id
# Output: Appends to .claude/evidence/events.jsonl

set -euo pipefail

# Import shared utilities
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/utils.sh
source "${SCRIPT_DIR}/lib/utils.sh"

# Main emission function
emit_evidence() {
    local json_input

    # Read JSON from stdin
    json_input=$(cat)

    # Validate JSON
    if ! validate_json "$json_input"; then
        log_event "ERROR" "Invalid JSON input to post_tool_use_emitter"
        exit 1
    fi

    # Extract fields
    local tool_name result timestamp actor_id
    tool_name=$(extract_tool_name "$json_input")
    result=$(echo "$json_input" | jq -r '.result // "success"')
    timestamp=$(echo "$json_input" | jq -r '.timestamp // empty')
    actor_id=$(echo "$json_input" | jq -r '.actor_id // empty')

    # Use current timestamp if not provided
    if [[ -z "$timestamp" ]]; then
        timestamp=$(get_timestamp)
    fi

    # Use session ID as actor_id if not provided
    if [[ -z "$actor_id" ]]; then
        actor_id=$(get_session_id)
    fi

    # Extract file paths
    local file_paths_json
    file_paths_json=$(echo "$json_input" | jq -r '.file_paths // []')

    # Generate work unit ID
    local work_unit_id
    work_unit_id="wu-$(date -u +"%Y%m%d%H%M%S")-$$-${RANDOM}"

    # Compute git diff hash
    local git_diff_hash
    git_diff_hash=$(get_git_diff_hash)

    # Ensure evidence directory exists
    local evidence_dir evidence_file
    evidence_dir=$(ensure_evidence_dir)
    evidence_file="${evidence_dir}/events.jsonl"

    # Create JSONL record
    local jsonl_record
    jsonl_record=$(jq -n \
        --arg wuid "$work_unit_id" \
        --arg tool "$tool_name" \
        --arg result "$result" \
        --arg ts "$timestamp" \
        --arg actor "$actor_id" \
        --arg hash "$git_diff_hash" \
        --argjson paths "$file_paths_json" \
        '{
            work_unit_id: $wuid,
            tool_name: $tool,
            result: $result,
            file_paths: $paths,
            git_diff_hash: $hash,
            timestamp: $ts,
            actor_id: $actor
        }')

    # Append to evidence log
    append_jsonl "$evidence_file" "$jsonl_record"

    # Output summary
    log_event "EVIDENCE" "Emitted: ${work_unit_id} (${tool_name})"

    # Return success
    exit 0
}

# Execute main function
emit_evidence "$@"
