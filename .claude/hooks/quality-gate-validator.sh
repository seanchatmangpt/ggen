#!/usr/bin/env bash

##############################################################################
# Quality Gate Validator - ggen v6.0.0
# Reads quality-gates.json, runs all gates, detects failures, saves receipts
# Returns JSON decision: continue or halt (Andon protocol)
##############################################################################

set -o pipefail

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
readonly GATES_CONFIG="${PROJECT_ROOT}/.claude/quality-gates.json"
readonly RECEIPTS_DIR="${PROJECT_ROOT}/.claude/receipts"
readonly TIMESTAMP=$(date -u +"%Y%m%d_%H%M%S")
readonly RECEIPT_FILE="${RECEIPTS_DIR}/quality-gate-${TIMESTAMP}.json"

##############################################################################
# Initialize directories
##############################################################################

mkdir -p "${RECEIPTS_DIR}"

##############################################################################
# Utility functions
##############################################################################

log_info() {
    echo "[INFO] $*" >&2
}

log_error() {
    echo "[ERROR] $*" >&2
}

log_success() {
    echo "[SUCCESS] $*" >&2
}

##############################################################################
# Main validator logic
##############################################################################

run_quality_gates() {
    local gates_json gates_array
    local total_gates=0 passed_gates=0 failed_gates=0
    local critical_failure=false
    local gates_results="[]"

    # Read and parse quality-gates.json
    if [[ ! -f "${GATES_CONFIG}" ]]; then
        log_error "Quality gates config not found: ${GATES_CONFIG}"
        return 1
    fi

    gates_array=$(jq -r '.gates[] | @base64' "${GATES_CONFIG}")

    # Process each gate
    while IFS= read -r gate_b64; do
        [[ -z "${gate_b64}" ]] && continue

        local gate_json
        gate_json=$(echo "${gate_b64}" | base64 -d)

        local gate_name gate_command timeout_seconds is_critical signal description
        gate_name=$(echo "${gate_json}" | jq -r '.name')
        gate_command=$(echo "${gate_json}" | jq -r '.command')
        timeout_seconds=$(echo "${gate_json}" | jq -r '.timeout_seconds')
        is_critical=$(echo "${gate_json}" | jq -r '.critical')
        signal=$(echo "${gate_json}" | jq -r '.signal')
        description=$(echo "${gate_json}" | jq -r '.description')

        ((total_gates++))

        log_info "Running gate: ${gate_name} (${timeout_seconds}s timeout)"

        # Run gate command with timeout
        local exit_code output start_time end_time duration
        start_time=$(date +%s)

        if output=$(timeout "${timeout_seconds}s" bash -c "cd '${PROJECT_ROOT}' && ${gate_command}" 2>&1); then
            exit_code=0
        else
            exit_code=$?
        fi

        end_time=$(date +%s)
        duration=$((end_time - start_time))

        # Determine gate status
        local gate_status gate_emoji
        if [[ ${exit_code} -eq 0 ]]; then
            gate_status="PASS"
            gate_emoji="✅"
            ((passed_gates++))
        elif [[ ${exit_code} -eq 124 ]]; then
            gate_status="TIMEOUT"
            gate_emoji="⏱️"
            ((failed_gates++))
            if [[ "${is_critical}" == "true" ]]; then
                critical_failure=true
            fi
        else
            gate_status="FAIL"
            gate_emoji="❌"
            ((failed_gates++))
            if [[ "${is_critical}" == "true" ]]; then
                critical_failure=true
            fi
        fi

        log_info "${gate_emoji} ${gate_name}: ${gate_status} (${duration}s)"

        # Build result entry
        local result_entry
        result_entry=$(jq -n \
            --arg name "${gate_name}" \
            --arg status "${gate_status}" \
            --arg signal "${signal}" \
            --arg description "${description}" \
            --arg command "${gate_command}" \
            --argjson critical "${is_critical}" \
            --argjson exit_code "${exit_code}" \
            --argjson duration "${duration}" \
            --arg output_snippet "${output:0:500}" \
            '{
                name: $name,
                status: $status,
                signal: $signal,
                description: $description,
                command: $command,
                critical: $critical,
                exit_code: $exit_code,
                duration_seconds: $duration,
                output_snippet: $output_snippet,
                timestamp: now | floor
            }')

        gates_results=$(echo "${gates_results}" | jq ". += [${result_entry}]")

        # Andon protocol: halt on critical failure
        if [[ "${critical_failure}" == "true" ]]; then
            log_error "${signal} CRITICAL GATE FAILED: ${gate_name}"
            log_error "Action: STOP THE LINE - Fix root cause immediately"
            break
        fi
    done <<< "${gates_array}"

    # Generate decision
    local decision continue_allowed halt_reason

    if [[ "${critical_failure}" == "true" ]]; then
        decision="HALT"
        continue_allowed=false
        halt_reason="Andon protocol triggered: critical gate failure"
    else
        decision="CONTINUE"
        continue_allowed=true
        halt_reason=""
    fi

    # Build receipt
    local receipt
    receipt=$(jq -n \
        --arg timestamp "${TIMESTAMP}" \
        --argjson gates "${gates_results}" \
        --argjson total "${total_gates}" \
        --argjson passed "${passed_gates}" \
        --argjson failed "${failed_gates}" \
        --arg decision "${decision}" \
        --argjson continue_allowed "${continue_allowed}" \
        --arg halt_reason "${halt_reason}" \
        '{
            timestamp: $timestamp,
            summary: {
                total_gates: $total,
                passed: $passed,
                failed: $failed,
                pass_rate: (if $total > 0 then ($passed / $total * 100 | floor) else 0 end)
            },
            gates: $gates,
            decision: {
                action: $decision,
                continue_allowed: $continue_allowed,
                halt_reason: $halt_reason
            },
            andon_protocol: {
                active: (if $failed > 0 then true else false end),
                critical_gates_failed: (if $failed > 0 then "true" else "false" end)
            }
        }')

    # Save receipt
    echo "${receipt}" | jq '.' > "${RECEIPT_FILE}"
    log_success "Receipt saved: ${RECEIPT_FILE}"

    # Output decision JSON to stdout
    echo "${receipt}" | jq '{
        decision: .decision,
        summary: .summary,
        andon_protocol: .andon_protocol,
        receipt_path: "'${RECEIPT_FILE}'"
    }'

    # Exit with appropriate code
    if [[ "${critical_failure}" == "true" ]]; then
        return 1
    else
        return 0
    fi
}

##############################################################################
# Entry point
##############################################################################

main() {
    log_info "Quality Gate Validator starting..."
    log_info "Configuration: ${GATES_CONFIG}"
    log_info "Receipts directory: ${RECEIPTS_DIR}"

    run_quality_gates
    local exit_code=$?

    if [[ ${exit_code} -eq 0 ]]; then
        log_success "All gates passed - proceeding"
    else
        log_error "Quality gate validation failed - halting"
    fi

    return ${exit_code}
}

main "$@"
