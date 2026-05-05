#!/usr/bin/env bash
# PreToolUse Guard - Protects critical surfaces from direct editing
# Input: JSON via stdin with tool_name, parameters
# Output: Exit code 0 (allow) or 1 (deny)

set -euo pipefail

# Import shared utilities
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/utils.sh
source "${SCRIPT_DIR}/lib/utils.sh"

# Check if tool should be blocked
check_tool_blocked() {
    local tool_name=$1

    # Block Edit, Write, Bash on protected surfaces
    if [[ "$tool_name" == "Edit" ]] || \
       [[ "$tool_name" == "Write" ]] || \
       [[ "$tool_name" == "Bash" ]]; then
        return 0  # Tool needs checking
    fi

    return 1  # Tool allowed
}

# Extract file paths from parameters
extract_file_paths() {
    local json_input=$1
    local file_paths=()

    # Edit tool: file_path (direct field in input)
    local edit_path
    edit_path=$(echo "$json_input" | jq -r '.file_path // empty')
    if [[ -n "$edit_path" ]]; then
        file_paths+=("$edit_path")
    fi

    # Write tool: file_path (direct field in input)
    local write_path
    write_path=$(echo "$json_input" | jq -r '.file_path // empty')
    if [[ -n "$write_path" ]]; then
        file_paths+=("$write_path")
    fi

    # Bash tool: command (may contain paths)
    local bash_cmd
    bash_cmd=$(echo "$json_input" | jq -r '.command // empty')
    if [[ -n "$bash_cmd" ]]; then
        # Extract potential file paths from command
        # This is basic - could be enhanced
        while IFS= read -r path; do
            [[ -n "$path" ]] && file_paths+=("$path")
        done < <(echo "$bash_cmd" | grep -oE '[a-zA-Z0-9_/\.-]+\.[a-z]+' || true)
    fi

    printf '%s\n' "${file_paths[@]}"
}

# Main guard function
guard_pre_tool_use() {
    local json_input

    # Read JSON from stdin
    json_input=$(cat)

    # Validate JSON
    if ! validate_json "$json_input"; then
        log_event "ERROR" "Invalid JSON input to pre_tool_use_guard"
        exit 1
    fi

    # Extract tool name
    local tool_name
    tool_name=$(extract_tool_name "$json_input")

    # Check if tool needs guarding
    if ! check_tool_blocked "$tool_name"; then
        # Tool not blocked, allow
        exit 0
    fi

    # Extract file paths (pass full input, not just params)
    local file_paths
    mapfile -t file_paths < <(extract_file_paths "$json_input")

    # Check each path
    for file_path in "${file_paths[@]}"; do
        if is_protected_path "$file_path"; then
            log_event "BLOCK" "PreToolUse denied: ${file_path} (${tool_name})"

            # Write denial to evidence log
            local evidence_dir evidence_file
            evidence_dir=$(ensure_evidence_dir)
            evidence_file="${evidence_dir}/denied_attempts.jsonl"

            local denial_record
            denial_record=$(jq -n \
                --arg ts "$(get_timestamp)" \
                --arg tool "$tool_name" \
                --arg path "$file_path" \
                --arg session "$(get_session_id)" \
                '{
                    timestamp: $ts,
                    tool_name: $tool,
                    file_path: $path,
                    session_id: $session,
                    action: "denied"
                }')

            append_jsonl "$evidence_file" "$denial_record"

            # Deny the operation
            exit 1
        fi
    done

    # No protected paths found, allow
    exit 0
}

# Execute main function
guard_pre_tool_use "$@"
