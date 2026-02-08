#!/bin/bash

##############################################################################
# Agent Skill Registry Module
#
# Manages extensible skill definitions for agent specialization.
# Supports skill loading, registration, validation, and agent-type queries.
#
# Features:
#  - YAML-based skill definitions with schema validation
#  - In-memory registry with multi-index support
#  - Skill discovery by agent type, category, or name
#  - Performance SLO verification
#  - Error handling with detailed validation messages
#
# Version: 1.0.0
##############################################################################

set -euo pipefail

# ============================================================================
# Configuration
# ============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
CONFIG_DIR="${SCRIPT_DIR}/../config"
SKILLS_DIR="${CONFIG_DIR}/agent-skills"
REGISTRY_DB="${SKILLS_DIR}/.registry.json"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Initialize registry if needed
_SKILL_REGISTRY=""  # JSON registry cache

# ============================================================================
# Utility Functions
# ============================================================================

_log_debug() {
    [[ "${DEBUG:-0}" == "1" ]] && echo -e "${BLUE}[DEBUG]${NC} $1" >&2 || true
}

_log_error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
}

_log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1" >&2
}

_log_success() {
    echo -e "${GREEN}[✓]${NC} $1"
}

# Check if command exists
_cmd_exists() {
    command -v "$1" &>/dev/null
}

# Convert YAML to JSON (basic implementation using yq or Python)
_yaml_to_json() {
    local yaml_file="$1"

    if _cmd_exists python3; then
        python3 -c "
import yaml
import json
import sys
try:
    with open('${yaml_file}', 'r') as f:
        data = yaml.safe_load(f)
        print(json.dumps(data))
except Exception as e:
    print(json.dumps({'error': str(e)}), file=sys.stderr)
    sys.exit(1)
"
    elif _cmd_exists yq; then
        # For yq 3.x, convert to JSON manually
        python3 -c "
import yaml
import json
with open('${yaml_file}', 'r') as f:
    data = yaml.safe_load(f)
    print(json.dumps(data))
"
    else
        _log_error "No YAML parser available (install python3)"
        return 1
    fi
}

# Parse JSON value (using jq or grep fallback)
_json_get() {
    local json_str="$1"
    local path="$2"

    if _cmd_exists jq; then
        echo "$json_str" | jq -r "$path" 2>/dev/null || echo ""
    else
        # Basic fallback parsing
        echo "$json_str" | grep -o "\"$path\":[^,}]*" | cut -d':' -f2- | tr -d ' "' || echo ""
    fi
}

# ============================================================================
# Schema Validation Functions
# ============================================================================

_validate_skill_schema() {
    local skill_json="$1"
    local skill_name="$2"

    # Required top-level fields
    local required_fields=("name" "category" "version" "agent_types" "description" "capabilities" "performance_slo")

    for field in "${required_fields[@]}"; do
        if ! echo "$skill_json" | jq -e ".skill.$field" >/dev/null 2>&1; then
            _log_error "Skill '$skill_name': Missing required field 'skill.$field'"
            return 1
        fi
    done

    # Validate category
    local category=$(echo "$skill_json" | jq -r '.skill.category')
    local valid_categories=("rdf" "sparql" "template" "qa" "devops" "utils" "integration")

    if [[ ! " ${valid_categories[@]} " =~ " ${category} " ]]; then
        _log_error "Skill '$skill_name': Invalid category '$category'. Must be one of: ${valid_categories[*]}"
        return 1
    fi

    # Validate agent_types is an array
    if ! echo "$skill_json" | jq -e '.skill.agent_types | type == "array"' >/dev/null 2>&1; then
        _log_error "Skill '$skill_name': 'agent_types' must be an array"
        return 1
    fi

    # Validate capabilities is an array
    if ! echo "$skill_json" | jq -e '.skill.capabilities | type == "array"' >/dev/null 2>&1; then
        _log_error "Skill '$skill_name': 'capabilities' must be an array"
        return 1
    fi

    # Validate performance_slo structure
    if ! echo "$skill_json" | jq -e '.skill.performance_slo | has("max_duration_ms") and has("success_rate")' >/dev/null 2>&1; then
        _log_error "Skill '$skill_name': 'performance_slo' must have 'max_duration_ms' and 'success_rate'"
        return 1
    fi

    # Validate SLO values are reasonable
    local max_duration=$(echo "$skill_json" | jq -r '.skill.performance_slo.max_duration_ms')
    local success_rate=$(echo "$skill_json" | jq -r '.skill.performance_slo.success_rate')

    if (( max_duration < 100 )); then
        _log_error "Skill '$skill_name': max_duration_ms must be >= 100ms, got $max_duration"
        return 1
    fi

    if (( $(echo "$success_rate < 0.80" | bc -l) )); then
        _log_error "Skill '$skill_name': success_rate must be >= 0.80, got $success_rate"
        return 1
    fi

    if (( $(echo "$success_rate > 1.0" | bc -l) )); then
        _log_error "Skill '$skill_name': success_rate must be <= 1.0, got $success_rate"
        return 1
    fi

    # Validate implementation
    if ! echo "$skill_json" | jq -e '.skill.implementation | has("language") and has("entry_point")' >/dev/null 2>&1; then
        _log_error "Skill '$skill_name': 'implementation' must have 'language' and 'entry_point'"
        return 1
    fi

    # Validate error_handling
    if ! echo "$skill_json" | jq -e '.skill.error_handling | has("retry_strategy") and has("max_retries")' >/dev/null 2>&1; then
        _log_error "Skill '$skill_name': 'error_handling' must have 'retry_strategy' and 'max_retries'"
        return 1
    fi

    _log_debug "Schema validation passed for skill '$skill_name'"
    return 0
}

# ============================================================================
# Registry Management Functions
# ============================================================================

_ensure_skills_dir() {
    mkdir -p "$SKILLS_DIR" || {
        _log_error "Failed to create skills directory: $SKILLS_DIR"
        return 1
    }
}

_init_registry() {
    _ensure_skills_dir

    if [[ ! -f "$REGISTRY_DB" ]]; then
        _log_debug "Initializing new registry database"
        echo '{"skills": {}, "by_category": {}, "by_agent_type": {}}' > "$REGISTRY_DB"
    fi

    _SKILL_REGISTRY=$(cat "$REGISTRY_DB")
}

# ============================================================================
# Core API Functions
# ============================================================================

skill_load() {
    local skill_name="$1"
    local skill_file="${SKILLS_DIR}/${skill_name}.yaml"

    if [[ ! -f "$skill_file" ]]; then
        _log_error "Skill file not found: $skill_file"
        return 1
    fi

    _log_debug "Loading skill from $skill_file"

    # Convert YAML to JSON
    local skill_json
    skill_json=$(_yaml_to_json "$skill_file") || {
        _log_error "Failed to parse YAML file: $skill_file"
        return 1
    }

    # Validate schema
    if ! _validate_skill_schema "$skill_json" "$skill_name"; then
        return 1
    fi

    # Output the parsed skill JSON
    echo "$skill_json"
    return 0
}

skill_register() {
    local skill_json="$1"

    _init_registry

    # Extract skill name
    local skill_name
    skill_name=$(echo "$skill_json" | jq -r '.skill.name')

    if [[ -z "$skill_name" ]] || [[ "$skill_name" == "null" ]]; then
        _log_error "Failed to extract skill name from JSON"
        return 1
    fi

    _log_debug "Registering skill: $skill_name"

    # Update registry with new skill
    local updated_registry
    updated_registry=$(echo "$_SKILL_REGISTRY" | jq \
        --arg name "$skill_name" \
        --argjson skill "$skill_json" \
        '.skills[$name] = $skill')

    # Update by_category index
    local category
    category=$(echo "$skill_json" | jq -r '.skill.category')

    updated_registry=$(echo "$updated_registry" | jq \
        --arg category "$category" \
        --arg name "$skill_name" \
        '.by_category[$category] += [$name]' \
        2>/dev/null || \
        echo "$updated_registry" | jq \
        --arg category "$category" \
        --arg name "$skill_name" \
        '.by_category[$category] = [$name]')

    # Update by_agent_type index
    local agent_types
    agent_types=$(echo "$skill_json" | jq -r '.skill.agent_types[]')

    while IFS= read -r agent_type; do
        updated_registry=$(echo "$updated_registry" | jq \
            --arg agent_type "$agent_type" \
            --arg name "$skill_name" \
            '.by_agent_type[$agent_type] += [$name]' \
            2>/dev/null || \
            echo "$updated_registry" | jq \
            --arg agent_type "$agent_type" \
            --arg name "$skill_name" \
            '.by_agent_type[$agent_type] = [$name]')
    done <<< "$agent_types"

    # Save updated registry
    echo "$updated_registry" > "$REGISTRY_DB"
    _SKILL_REGISTRY="$updated_registry"

    _log_success "Skill registered: $skill_name"
    return 0
}

skill_get_by_name() {
    local skill_name="$1"

    _init_registry

    local skill
    skill=$(echo "$_SKILL_REGISTRY" | jq ".skills[\"$skill_name\"]" 2>/dev/null)

    if [[ -z "$skill" ]] || [[ "$skill" == "null" ]]; then
        _log_error "Skill not found: $skill_name"
        return 1
    fi

    echo "$skill"
    return 0
}

skill_get_by_agent() {
    local agent_type="$1"

    _init_registry

    local skills_list
    skills_list=$(echo "$_SKILL_REGISTRY" | jq -r ".by_agent_type[\"$agent_type\"][]?" 2>/dev/null)

    if [[ -z "$skills_list" ]]; then
        _log_warn "No skills found for agent type: $agent_type"
        return 0
    fi

    # Return as JSON array
    local json_array="["
    local first=true
    while IFS= read -r skill_name; do
        if [[ -z "$skill_name" ]]; then
            continue
        fi
        if [[ "$first" == true ]]; then
            first=false
        else
            json_array+=","
        fi
        json_array+="\"$skill_name\""
    done <<< "$skills_list"
    json_array+="]"

    echo "$json_array"
    return 0
}

skill_get_by_category() {
    local category="$1"

    _init_registry

    local skills_list
    skills_list=$(echo "$_SKILL_REGISTRY" | jq ".by_category[\"$category\"]" 2>/dev/null)

    if [[ -z "$skills_list" ]] || [[ "$skills_list" == "null" ]]; then
        _log_warn "No skills found for category: $category"
        return 0
    fi

    echo "$skills_list"
    return 0
}

skill_list_all() {
    _init_registry

    local skill_names
    skill_names=$(echo "$_SKILL_REGISTRY" | jq -r '.skills | keys[]' 2>/dev/null)

    if [[ -z "$skill_names" ]]; then
        _log_warn "No skills registered"
        return 0
    fi

    # Return as JSON array
    echo "$skill_names" | jq -Rs 'split("\n") | map(select(. != "")) | sort'
    return 0
}

skill_unregister() {
    local skill_name="$1"

    _init_registry

    # Check if skill exists
    if ! echo "$_SKILL_REGISTRY" | jq -e ".skills[\"$skill_name\"]" >/dev/null 2>&1; then
        _log_error "Skill not found: $skill_name"
        return 1
    fi

    _log_debug "Unregistering skill: $skill_name"

    # Get skill details for cleanup
    local skill_json
    skill_json=$(echo "$_SKILL_REGISTRY" | jq ".skills[\"$skill_name\"]")

    local category
    category=$(echo "$skill_json" | jq -r '.skill.category')

    local agent_types
    agent_types=$(echo "$skill_json" | jq -r '.skill.agent_types[]')

    # Remove from skills
    local updated_registry
    updated_registry=$(echo "$_SKILL_REGISTRY" | jq "del(.skills[\"$skill_name\"])")

    # Remove from by_category
    updated_registry=$(echo "$updated_registry" | jq \
        --arg category "$category" \
        --arg name "$skill_name" \
        '.by_category[$category] |= map(select(. != $name))')

    # Remove from by_agent_type
    while IFS= read -r agent_type; do
        updated_registry=$(echo "$updated_registry" | jq \
            --arg agent_type "$agent_type" \
            --arg name "$skill_name" \
            '.by_agent_type[$agent_type] |= map(select(. != $name))')
    done <<< "$agent_types"

    # Save updated registry
    echo "$updated_registry" > "$REGISTRY_DB"
    _SKILL_REGISTRY="$updated_registry"

    _log_success "Skill unregistered: $skill_name"
    return 0
}

skill_validate_file() {
    local skill_file="$1"

    if [[ ! -f "$skill_file" ]]; then
        _log_error "File not found: $skill_file"
        return 1
    fi

    _log_debug "Validating skill file: $skill_file"

    # Convert YAML to JSON
    local skill_json
    skill_json=$(_yaml_to_json "$skill_file") || {
        _log_error "Failed to parse YAML file: $skill_file"
        return 1
    }

    # Extract skill name
    local skill_name
    skill_name=$(echo "$skill_json" | jq -r '.skill.name // "unknown"')

    # Validate schema
    if _validate_skill_schema "$skill_json" "$skill_name"; then
        _log_success "Skill validation passed: $skill_name"
        return 0
    else
        return 1
    fi
}

skill_registry_stats() {
    _init_registry

    local total_skills
    total_skills=$(echo "$_SKILL_REGISTRY" | jq '.skills | length')

    local categories
    categories=$(echo "$_SKILL_REGISTRY" | jq '.by_category | keys | length')

    local agent_types_count
    agent_types_count=$(echo "$_SKILL_REGISTRY" | jq '.by_agent_type | keys | length')

    cat <<EOF
{
  "total_skills": $total_skills,
  "total_categories": $categories,
  "total_agent_types": $agent_types_count,
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
}

# ============================================================================
# Export Functions
# ============================================================================

# Export API functions
export -f skill_load
export -f skill_register
export -f skill_get_by_name
export -f skill_get_by_agent
export -f skill_get_by_category
export -f skill_list_all
export -f skill_unregister
export -f skill_validate_file
export -f skill_registry_stats
