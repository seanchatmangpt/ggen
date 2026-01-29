#!/usr/bin/env bash
# Skill Loader and Registration Module
# Manages skill discovery, loading, validation, and execution

set -euo pipefail

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
readonly SKILLS_DIR="${SCRIPT_DIR}/config/agent-skills"
readonly SKILLS_CACHE_DIR="${SCRIPT_DIR}/.skills-cache"
readonly SKILL_SCHEMA="${SKILLS_DIR}/skill-schema.json"

# Colors for output
readonly COLOR_GREEN='\033[0;32m'
readonly COLOR_YELLOW='\033[1;33m'
readonly COLOR_RED='\033[0;31m'
readonly COLOR_NC='\033[0m' # No Color

# Create cache directory if it doesn't exist
mkdir -p "${SKILLS_CACHE_DIR}"

# Log functions
log_info() {
    echo -e "${COLOR_GREEN}[INFO]${COLOR_NC} $*" >&2
}

log_warn() {
    echo -e "${COLOR_YELLOW}[WARN]${COLOR_NC} $*" >&2
}

log_error() {
    echo -e "${COLOR_RED}[ERROR]${COLOR_NC} $*" >&2
}

# Discover all skills in the skills directory
discover_skills() {
    local category="$1"

    if [[ -d "${SKILLS_DIR}/${category}" ]]; then
        find "${SKILLS_DIR}/${category}" -name "*.yaml" -type f | sort
    fi
}

# List all available skills by category
list_all_skills() {
    local categories=("rdf" "sparql" "template" "qa" "devops")

    echo "Available Agent Skills by Category:"
    echo "===================================="
    echo ""

    for category in "${categories[@]}"; do
        local skill_files
        skill_files=$(discover_skills "$category")

        if [[ -n "$skill_files" ]]; then
            echo "Category: $(echo "$category" | tr '[:lower:]' '[:upper:]')"
            echo "---"

            while IFS= read -r skill_file; do
                local skill_name
                skill_name=$(basename "$skill_file" .yaml)
                echo "  - ${skill_name}"
            done <<< "$skill_files"

            echo ""
        fi
    done
}

# Get skill metadata from YAML file
get_skill_metadata() {
    local skill_file="$1"

    if [[ ! -f "$skill_file" ]]; then
        log_error "Skill file not found: $skill_file"
        return 1
    fi

    # Extract metadata using grep and sed (YAML parsing)
    local name
    local category
    local version
    local entry_point

    name=$(grep "^name:" "$skill_file" | cut -d' ' -f2)
    category=$(grep "^category:" "$skill_file" | cut -d' ' -f2)
    version=$(grep "^version:" "$skill_file" | cut -d' ' -f2 | tr -d '"')
    entry_point=$(grep "entry_point:" "$skill_file" | cut -d' ' -f2)

    cat <<EOF
{
  "name": "${name}",
  "category": "${category}",
  "version": ${version},
  "entry_point": "${entry_point}"
}
EOF
}

# Validate skill YAML against schema
validate_skill_yaml() {
    local skill_file="$1"

    if [[ ! -f "$skill_file" ]]; then
        log_error "Skill file not found: $skill_file"
        return 1
    fi

    if [[ ! -f "$SKILL_SCHEMA" ]]; then
        log_warn "Skill schema not found, skipping validation: $SKILL_SCHEMA"
        return 0
    fi

    # Stub: In production, use a proper YAML/JSON schema validator
    # For now, just check for required fields
    local required_fields=("name" "category" "version" "description" "capabilities"
                          "requirements" "performance_slo" "implementation"
                          "error_handling" "testing" "documentation")

    for field in "${required_fields[@]}"; do
        if ! grep -q "^${field}:" "$skill_file"; then
            log_error "Missing required field '${field}' in skill: $skill_file"
            return 1
        fi
    done

    return 0
}

# Load skill functions from module
load_skill_module() {
    local module_name="$1"
    local module_path="${SCRIPT_DIR}/modules/${module_name}.sh"

    if [[ ! -f "$module_path" ]]; then
        log_error "Skill module not found: $module_path"
        return 1
    fi

    # Source the module
    # shellcheck disable=SC1090
    source "$module_path"

    log_info "Loaded skill module: $module_name"
    return 0
}

# Execute a skill by name
execute_skill() {
    local skill_name="$1"
    shift
    local skill_args=("$@")

    # Construct function name from skill name
    local function_name="skill_${skill_name}"

    if ! command -v "$function_name" &> /dev/null; then
        log_error "Skill function not found: $function_name"
        return 1
    fi

    # Execute the skill function with provided arguments
    "$function_name" "${skill_args[@]}"
}

# Register skill for an agent
register_skill_for_agent() {
    local agent_id="$1"
    local skill_name="$2"
    local skill_file

    # Find the skill file
    skill_file=$(find "${SKILLS_DIR}" -name "${skill_name}.yaml" | head -1)

    if [[ -z "$skill_file" ]]; then
        log_error "Skill not found: $skill_name"
        return 1
    fi

    # Validate the skill
    if ! validate_skill_yaml "$skill_file"; then
        log_error "Skill validation failed: $skill_name"
        return 1
    fi

    # Extract module name from skill file path
    local category
    category=$(basename "$(dirname "$skill_file")")

    # Map category to module
    local module_map
    declare -A module_map=(
        ["rdf"]="skills-rdf"
        ["sparql"]="skills-sparql"
        ["template"]="skills-template"
        ["qa"]="skills-qa"
        ["devops"]="skills-devops"
    )

    local module="${module_map[$category]}"

    # Load the module
    if ! load_skill_module "$module"; then
        log_error "Failed to load skill module for: $skill_name"
        return 1
    fi

    # Create registration record
    local registration_file="${SKILLS_CACHE_DIR}/${agent_id}-${skill_name}.registration"
    cat > "$registration_file" <<EOF
{
  "agent_id": "${agent_id}",
  "skill_name": "${skill_name}",
  "registered_at": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "skill_file": "${skill_file}",
  "module": "${module}"
}
EOF

    log_info "Registered skill '${skill_name}' for agent '${agent_id}'"
    return 0
}

# Get registered skills for an agent
get_registered_skills() {
    local agent_id="$1"
    local registrations

    registrations=$(find "${SKILLS_CACHE_DIR}" -name "${agent_id}-*.registration" -type f | sort)

    if [[ -z "$registrations" ]]; then
        return 1
    fi

    echo "Registered skills for agent: $agent_id"
    echo "======================================"

    while IFS= read -r registration_file; do
        local skill_name
        skill_name=$(basename "$registration_file" .registration | sed "s/${agent_id}-//")
        echo "  - $skill_name"
    done <<< "$registrations"
}

# Validate all skill definitions
validate_all_skills() {
    local categories=("rdf" "sparql" "template" "qa" "devops")
    local total_skills=0
    local valid_skills=0
    local failed_skills=0

    echo "Validating all skill definitions..."
    echo "===================================="
    echo ""

    for category in "${categories[@]}"; do
        local skill_files
        skill_files=$(discover_skills "$category")

        while IFS= read -r skill_file; do
            total_skills=$((total_skills + 1))
            local skill_name
            skill_name=$(basename "$skill_file" .yaml)

            if validate_skill_yaml "$skill_file"; then
                valid_skills=$((valid_skills + 1))
                echo "✓ $skill_name"
            else
                failed_skills=$((failed_skills + 1))
                echo "✗ $skill_name (FAILED)"
            fi
        done <<< "$skill_files"
    done

    echo ""
    echo "Validation Summary:"
    echo "==================="
    echo "Total Skills: $total_skills"
    echo "Valid: $valid_skills"
    echo "Failed: $failed_skills"

    if [[ $failed_skills -eq 0 ]]; then
        return 0
    else
        return 1
    fi
}

# Verify all skill functions exist
verify_skill_functions() {
    local module_files
    module_files=$(find "${SCRIPT_DIR}/modules" -name "skills-*.sh" -type f | sort)

    echo "Verifying skill function implementations..."
    echo "============================================"
    echo ""

    local total_functions=0
    local verified_functions=0

    while IFS= read -r module_file; do
        local module_name
        module_name=$(basename "$module_file" .sh)

        # Load the module in a subshell to check functions
        (
            # shellcheck disable=SC1090
            source "$module_file" 2>/dev/null || true

            # Extract all skill functions from the module
            local functions
            functions=$(grep -o "^skill_[a-z_]*() {" "$module_file" | sed 's/() {//')

            if [[ -n "$functions" ]]; then
                while IFS= read -r func; do
                    total_functions=$((total_functions + 1))

                    if declare -f "$func" > /dev/null 2>&1; then
                        verified_functions=$((verified_functions + 1))
                        echo "✓ $func"
                    else
                        echo "✗ $func (NOT FOUND)"
                    fi
                done <<< "$functions"
            fi
        )
    done <<< "$module_files"

    echo ""
    echo "Function Verification Summary:"
    echo "=============================="
    echo "Total Functions: $total_functions"
    echo "Verified: $verified_functions"

    if [[ $verified_functions -eq $total_functions ]]; then
        return 0
    else
        return 1
    fi
}

# Print help information
print_help() {
    cat <<'EOF'
Skill Loader and Registration Module

Usage: skills-loader.sh <command> [options]

Commands:
  list                          List all available skills
  validate                      Validate all skill definitions
  verify                        Verify all skill function implementations
  load <module>                 Load a skill module
  execute <skill_name> <args>   Execute a skill
  register <agent_id> <skill>   Register a skill for an agent
  get-skills <agent_id>         Get registered skills for an agent

Examples:
  ./skills-loader.sh list
  ./skills-loader.sh validate
  ./skills-loader.sh verify
  ./skills-loader.sh load skills-rdf
  ./skills-loader.sh execute skill_turtle_parser_parse ontology.ttl
  ./skills-loader.sh register agent-001 turtle_parser
  ./skills-loader.sh get-skills agent-001
EOF
}

# Main entry point
main() {
    local command="${1:-}"

    case "$command" in
        list)
            list_all_skills
            ;;
        validate)
            validate_all_skills
            ;;
        verify)
            verify_skill_functions
            ;;
        load)
            if [[ -z "${2:-}" ]]; then
                log_error "Module name required"
                return 1
            fi
            load_skill_module "$2"
            ;;
        execute)
            if [[ -z "${2:-}" ]]; then
                log_error "Skill name required"
                return 1
            fi
            execute_skill "$2" "${@:3}"
            ;;
        register)
            if [[ -z "${2:-}" ]] || [[ -z "${3:-}" ]]; then
                log_error "Agent ID and skill name required"
                return 1
            fi
            register_skill_for_agent "$2" "$3"
            ;;
        get-skills)
            if [[ -z "${2:-}" ]]; then
                log_error "Agent ID required"
                return 1
            fi
            get_registered_skills "$2"
            ;;
        -h|--help|help)
            print_help
            ;;
        *)
            log_error "Unknown command: $command"
            print_help
            return 1
            ;;
    esac
}

# Export functions for use by other scripts
export -f discover_skills
export -f list_all_skills
export -f get_skill_metadata
export -f validate_skill_yaml
export -f load_skill_module
export -f execute_skill
export -f register_skill_for_agent
export -f get_registered_skills
export -f validate_all_skills
export -f verify_skill_functions

# Run main function if script is executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
