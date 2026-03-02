#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../../" && pwd)"
STATE_DIR="${PROJECT_ROOT}/.claude/autonomous"
STATE_FILE="${STATE_DIR}/workflow-state.json"
STATE_BACKUP_DIR="${STATE_DIR}/backups"

# Phase progression sequence
declare -a PHASE_SEQUENCE=("explore" "plan" "execute" "verify")

# ============================================================================
# Utility Functions
# ============================================================================

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" >&2
}

error() {
    echo "[ERROR] $*" >&2
    exit 1
}

# Validate jq is available
require_jq() {
    if ! command -v jq &> /dev/null; then
        error "jq is required but not installed"
    fi
}

# Create directories if they don't exist
ensure_directories() {
    mkdir -p "${STATE_DIR}" "${STATE_BACKUP_DIR}"
}

# Create a backup of the current state file
backup_state() {
    if [[ -f "${STATE_FILE}" ]]; then
        local backup_file="${STATE_BACKUP_DIR}/workflow-state.$(date +%s).json"
        cp "${STATE_FILE}" "${backup_file}"
        # Keep only last 3 backups
        ls -t "${STATE_BACKUP_DIR}"/workflow-state.*.json 2>/dev/null | tail -n +4 | xargs -r rm
        log "Backed up state to ${backup_file}"
    fi
}

# Load the current workflow state
load_state() {
    if [[ ! -f "${STATE_FILE}" ]]; then
        return 1
    fi
    cat "${STATE_FILE}"
}

# Atomically write state with temporary file
write_state() {
    local new_state="$1"
    local tmp_file="${STATE_FILE}.tmp"

    echo "${new_state}" | jq . > "${tmp_file}" 2>/dev/null || {
        error "Failed to write state: invalid JSON"
    }

    mv "${tmp_file}" "${STATE_FILE}"
    log "State updated and synced"
}

# Initialize empty state if doesn't exist
initialize_state() {
    local workflow_id="workflow-$(date +%Y-%m-%d-%s)"

    local initial_state=$(cat <<EOF
{
  "workflow_id": "${workflow_id}",
  "current_phase": "explore",
  "created_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "phases": {
    "explore": {
      "status": "pending",
      "agents": 0,
      "started_at": null,
      "completed_at": null,
      "progress": {
        "completed": 0,
        "total": 0,
        "in_progress": 0
      },
      "outputs": []
    },
    "plan": {
      "status": "pending",
      "agents": 0,
      "started_at": null,
      "completed_at": null,
      "progress": {
        "completed": 0,
        "total": 0,
        "in_progress": 0
      },
      "outputs": []
    },
    "execute": {
      "status": "pending",
      "agents": 0,
      "started_at": null,
      "completed_at": null,
      "progress": {
        "completed": 0,
        "total": 0,
        "in_progress": 0
      },
      "outputs": []
    },
    "verify": {
      "status": "pending",
      "agents": 0,
      "started_at": null,
      "completed_at": null,
      "progress": {
        "completed": 0,
        "total": 0,
        "in_progress": 0
      },
      "outputs": []
    }
  }
}
EOF
)

    write_state "${initial_state}"
    log "Initialized new workflow with ID: ${workflow_id}"
}

# Get the current phase from state
get_current_phase() {
    local state="$1"
    echo "${state}" | jq -r '.current_phase'
}

# Get the status of a specific phase
get_phase_status() {
    local state="$1"
    local phase="$2"
    echo "${state}" | jq -r ".phases.\"${phase}\".status"
}

# Find the index of a phase in the sequence
get_phase_index() {
    local phase="$1"
    for i in "${!PHASE_SEQUENCE[@]}"; do
        if [[ "${PHASE_SEQUENCE[$i]}" == "${phase}" ]]; then
            echo "$i"
            return 0
        fi
    done
    return 1
}

# Get the next phase in the sequence
get_next_phase() {
    local current_phase="$1"
    local current_index
    current_index=$(get_phase_index "${current_phase}")

    if [[ $((current_index + 1)) -lt ${#PHASE_SEQUENCE[@]} ]]; then
        echo "${PHASE_SEQUENCE[$((current_index + 1))]}"
    else
        echo "completed"
    fi
}

# Check if all phases are completed
is_workflow_complete() {
    local state="$1"

    for phase in "${PHASE_SEQUENCE[@]}"; do
        local status
        status=$(get_phase_status "${state}" "${phase}")
        if [[ "${status}" != "completed" ]]; then
            return 1
        fi
    done
    return 0
}

# Update phase status in state
set_phase_status() {
    local state="$1"
    local phase="$2"
    local status="$3"

    local now
    now=$(date -u +%Y-%m-%dT%H:%M:%SZ)

    # Update status and corresponding timestamp
    state=$(echo "${state}" | jq ".phases.\"${phase}\".status = \"${status}\"")

    case "${status}" in
        "in_progress")
            state=$(echo "${state}" | jq ".phases.\"${phase}\".started_at = \"${now}\"")
            ;;
        "completed")
            state=$(echo "${state}" | jq ".phases.\"${phase}\".completed_at = \"${now}\"")
            ;;
    esac

    echo "${state}"
}

# Update current phase
set_current_phase() {
    local state="$1"
    local phase="$2"
    echo "${state}" | jq ".current_phase = \"${phase}\""
}

# Check if a phase is ready to transition
should_advance_phase() {
    local state="$1"
    local current_phase="$2"
    local current_status

    current_status=$(get_phase_status "${state}" "${current_phase}")

    if [[ "${current_status}" == "completed" ]]; then
        return 0
    fi
    return 1
}

# ============================================================================
# Orchestration Logic
# ============================================================================

check_and_advance_phase() {
    ensure_directories

    local state
    if ! state=$(load_state); then
        log "No workflow state found, initializing..."
        initialize_state
        return 0
    fi

    local current_phase
    current_phase=$(get_current_phase "${state}")

    log "Current phase: ${current_phase}"

    # Check if current phase is complete
    if should_advance_phase "${state}" "${current_phase}"; then
        log "Phase '${current_phase}' is complete"

        if is_workflow_complete "${state}"; then
            log "Entire workflow complete!"
            echo "workflow_complete"
            return 0
        fi

        # Get next phase and advance
        local next_phase
        next_phase=$(get_next_phase "${current_phase}")

        log "Advancing to next phase: ${next_phase}"

        # Backup before transition
        backup_state

        # Update state: mark next phase as pending (will be started externally)
        state=$(set_current_phase "${state}" "${next_phase}")
        write_state "${state}"

        echo "phase_advanced:${next_phase}"
        return 0
    else
        local status
        status=$(get_phase_status "${state}" "${current_phase}")
        log "Current phase '${current_phase}' status: ${status} (not yet complete)"
        echo "phase_in_progress:${current_phase}"
        return 0
    fi
}

# Mark a phase as complete
mark_phase_complete() {
    local phase_name="$1"

    ensure_directories

    local state
    if ! state=$(load_state); then
        error "No workflow state found"
    fi

    log "Marking phase '${phase_name}' as complete"

    backup_state
    state=$(set_phase_status "${state}" "${phase_name}" "completed")
    write_state "${state}"

    log "Phase '${phase_name}' marked as complete"
}

# Mark a phase as in progress
mark_phase_in_progress() {
    local phase_name="$1"

    ensure_directories

    local state
    if ! state=$(load_state); then
        error "No workflow state found"
    fi

    log "Marking phase '${phase_name}' as in_progress"

    backup_state
    state=$(set_phase_status "${state}" "${phase_name}" "in_progress")
    write_state "${state}"

    log "Phase '${phase_name}' marked as in_progress"
}

# Get workflow status
get_status() {
    ensure_directories

    local state
    if ! state=$(load_state); then
        echo "No workflow initialized"
        return 0
    fi

    echo "${state}" | jq '.'
}

# Reset workflow
reset_workflow() {
    log "Resetting workflow..."

    if [[ -f "${STATE_FILE}" ]]; then
        backup_state
        rm -f "${STATE_FILE}"
        log "Workflow state cleared"
    fi

    initialize_state
    log "Workflow reset complete"
}

# ============================================================================
# Main
# ============================================================================

main() {
    require_jq

    local command="${1:-check}"

    case "${command}" in
        check)
            check_and_advance_phase
            ;;
        mark-complete)
            if [[ -z "${2:-}" ]]; then
                error "Usage: $0 mark-complete <phase-name>"
            fi
            mark_phase_complete "$2"
            ;;
        mark-in-progress)
            if [[ -z "${2:-}" ]]; then
                error "Usage: $0 mark-in-progress <phase-name>"
            fi
            mark_phase_in_progress "$2"
            ;;
        status)
            get_status
            ;;
        reset)
            reset_workflow
            ;;
        *)
            echo "Usage: $0 {check|mark-complete|mark-in-progress|status|reset} [phase-name]"
            echo ""
            echo "Commands:"
            echo "  check              Check phase status and auto-advance if complete"
            echo "  mark-complete      Mark a phase as complete"
            echo "  mark-in-progress   Mark a phase as in progress"
            echo "  status             Display current workflow status"
            echo "  reset              Reset workflow to initial state"
            exit 1
            ;;
    esac
}

main "$@"
