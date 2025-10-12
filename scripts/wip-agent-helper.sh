#!/usr/bin/env bash
set -euo pipefail

# WIP Agent Helper Script
# Provides a streamlined interface for agents to work with the WIP queue

usage() {
    cat << 'USAGE_EOF'
Usage: $0 <command> [options]

Commands:
    next                    Get and claim the next WIP task
    status                  Show current WIP queue status
    release                 Release current task lock
    validate [files...]     Validate WIP markers in changed files
    help                    Show this help message

Examples:
    $0 next                 # Get and claim next task
    $0 status               # Show queue status
    $0 release              # Release current lock
    $0 validate file1.rs file2.rs  # Validate specific files
USAGE_EOF
}

get_next_task() {
    echo "🔍 Getting next WIP task..."
    
    # Get the first unlocked task
    local task_json=$(cargo run -p wipqueue --quiet | jq -r '.[0]')
    
    if [[ "$task_json" == "null" || -z "$task_json" ]]; then
        echo "✅ No WIP tasks found - all done!"
        exit 0
    fi
    
    # Extract task details
    local path=$(echo "$task_json" | jq -r '.path')
    local line=$(echo "$task_json" | jq -r '.line')
    local text=$(echo "$task_json" | jq -r '.text')
    local kind=$(echo "$task_json" | jq -r '.kind')
    
    echo "📋 Next task: $path:$line ($kind)"
    echo "   $text"
    
    # Claim the task
    echo "🔒 Claiming task..."
    if ./scripts/wip-lock claim "$path" "$line"; then
        # Create branch
        local branch_name="wip/$(echo "$path" | tr '/' '__')__L$line"
        echo "🌿 Creating branch: $branch_name"
        git switch -c "$branch_name"
        
        # Save task info for later use
        echo "$task_json" > .wip_current_task
        
        echo ""
        echo "✅ Task claimed and branch created!"
        echo "📝 Edit $path at line $line"
        echo "🔄 Run '$0 release' when done"
    else
        echo "❌ Failed to claim task (may be locked)"
        exit 1
    fi
}

show_status() {
    echo "📋 WIP Queue Status"
    echo "=================="
    
    # Show first 5 tasks
    echo "Next 5 tasks:"
    cargo run -p wipqueue --quiet | jq -r '.[0:5] | .[] | "\(.path):\(.line) (\(.kind)) - \(.text)"'
    
    echo ""
    echo "🔒 Active locks:"
    if [[ -d .wiplocks ]]; then
        local lock_count=$(find .wiplocks -mindepth 1 -maxdepth 1 -type d | wc -l)
        if [[ $lock_count -gt 0 ]]; then
            find .wiplocks -mindepth 1 -maxdepth 1 -type d | while read -r lock; do
                if [[ -f "$lock/owner" ]]; then
                    echo "  $(cat "$lock/owner")"
                fi
            done
        else
            echo "  None"
        fi
    else
        echo "  None"
    fi
    
    # Show current task if exists
    if [[ -f .wip_current_task ]]; then
        echo ""
        echo "📌 Current task:"
        local task_json=$(cat .wip_current_task)
        local path=$(echo "$task_json" | jq -r '.path')
        local line=$(echo "$task_json" | jq -r '.line')
        local text=$(echo "$task_json" | jq -r '.text')
        echo "  $path:$line - $text"
    fi
}

release_task() {
    if [[ ! -f .wip_current_task ]]; then
        echo "❌ No current task found. Run '$0 next' first."
        exit 1
    fi
    
    local task_json=$(cat .wip_current_task)
    local path=$(echo "$task_json" | jq -r '.path')
    
    echo "🔓 Releasing task: $path"
    ./scripts/wip-lock release "$path"
    rm -f .wip_current_task
    
    echo "✅ Task released!"
}

validate_files() {
    local files=("$@")
    
    if [[ ${#files[@]} -eq 0 ]]; then
        echo "❌ No files specified for validation"
        exit 1
    fi
    
    echo "🔍 Validating WIP markers in: ${files[*]}"
    ./scripts/validate-wip-markers.sh "${files[@]}"
}

# Main command dispatch
case "${1:-help}" in
    next)
        get_next_task
        ;;
    status)
        show_status
        ;;
    release)
        release_task
        ;;
    validate)
        shift
        validate_files "$@"
        ;;
    help|--help|-h)
        usage
        ;;
    *)
        echo "❌ Unknown command: $1"
        echo ""
        usage
        exit 1
        ;;
esac
