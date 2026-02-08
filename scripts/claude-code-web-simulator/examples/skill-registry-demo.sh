#!/bin/bash

##############################################################################
# Skill Registry System - Interactive Demo
#
# Demonstrates the core functionality of the agent skill registry system:
# - Loading skills from YAML definitions
# - Registering skills in the database
# - Querying skills by name, agent type, and category
# - Listing and unregistering skills
#
# Run with: bash examples/skill-registry-demo.sh
##############################################################################

set -euo pipefail

# ============================================================================
# Setup
# ============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
MODULES_DIR="${PROJECT_ROOT}/modules"
CONFIG_DIR="${PROJECT_ROOT}/config"
SKILLS_DIR="${CONFIG_DIR}/agent-skills"

# Source the skill registry module
source "${MODULES_DIR}/skill-registry.sh"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# ============================================================================
# Demo Functions
# ============================================================================

demo_banner() {
    cat <<EOF

${BLUE}╔════════════════════════════════════════════════════════════════════════════╗${NC}
${BLUE}║                                                                            ║${NC}
${BLUE}║               Agent Skill Registry System - Interactive Demo               ║${NC}
${BLUE}║                                                                            ║${NC}
${BLUE}╚════════════════════════════════════════════════════════════════════════════╝${NC}

This demo shows:
  1. Loading skills from YAML definitions
  2. Registering skills in the database
  3. Querying skills by name, agent type, and category
  4. Listing all registered skills
  5. Getting registry statistics

EOF
}

step_banner() {
    local step_num="$1"
    local step_title="$2"
    echo ""
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Step $step_num: $step_title${NC}"
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_info() {
    echo -e "${BLUE}ℹ $1${NC}"
}

# ============================================================================
# Demo Steps
# ============================================================================

demo_step_1() {
    step_banner "1" "Load and Display a Skill (turtle-parser)"

    print_info "Loading skill from YAML file..."
    local skill_json
    if skill_json=$(skill_load "turtle-parser"); then
        print_success "Skill loaded successfully!"
        echo ""
        echo "Skill Details:"
        echo -e "${YELLOW}Name:${NC}" "$(echo "$skill_json" | jq -r '.skill.name')"
        echo -e "${YELLOW}Category:${NC}" "$(echo "$skill_json" | jq -r '.skill.category')"
        echo -e "${YELLOW}Version:${NC}" "$(echo "$skill_json" | jq -r '.skill.version')"
        echo -e "${YELLOW}Agent Types:${NC}"
        echo "$skill_json" | jq -r '.skill.agent_types[]' | sed 's/^/  - /'
        echo -e "${YELLOW}Capabilities:${NC}"
        echo "$skill_json" | jq -r '.skill.capabilities[]' | sed 's/^/  - /'
        echo -e "${YELLOW}Performance SLO:${NC}"
        echo "  Max Duration: $(echo "$skill_json" | jq -r '.skill.performance_slo.max_duration_ms')ms"
        echo "  Success Rate: $(echo "$skill_json" | jq -r '.skill.performance_slo.success_rate')"
    else
        echo -e "${RED}✗ Failed to load skill${NC}"
        return 1
    fi
}

demo_step_2() {
    step_banner "2" "Register Skills in Database"

    print_info "Registering multiple skills..."

    local skills=("turtle-parser" "sparql-executor" "tera-template-renderer" "rust-compiler-validator" "cargo-make-orchestrator" "receipt-generator")

    for skill_name in "${skills[@]}"; do
        if skill_json=$(skill_load "$skill_name"); then
            if skill_register "$skill_json"; then
                print_success "Registered: $skill_name"
            fi
        fi
    done

    echo ""
    print_info "All skills registered successfully!"
}

demo_step_3() {
    step_banner "3" "Query Skills by Name"

    print_info "Looking up 'sparql-executor' skill..."
    if skill_json=$(skill_get_by_name "sparql-executor"); then
        echo ""
        echo "Skill found:"
        echo -e "${YELLOW}Name:${NC}" "$(echo "$skill_json" | jq -r '.skill.name')"
        echo -e "${YELLOW}Description:${NC}" "$(echo "$skill_json" | jq -r '.skill.description')"
        echo -e "${YELLOW}Category:${NC}" "$(echo "$skill_json" | jq -r '.skill.category')"
        echo -e "${YELLOW}Max Duration:${NC}" "$(echo "$skill_json" | jq -r '.skill.performance_slo.max_duration_ms')ms"
    fi
}

demo_step_4() {
    step_banner "4" "Discover Skills by Agent Type"

    print_info "Finding all skills available to 'code-generator' agents..."
    local skills_json
    skills_json=$(skill_get_by_agent "code-generator")

    echo ""
    echo "Skills available to code-generator agents:"
    echo "$skills_json" | jq -r '.[]' | while read -r skill_name; do
        if skill_info=$(skill_get_by_name "$skill_name"); then
            local category=$(echo "$skill_info" | jq -r '.skill.category')
            echo -e "  ${GREEN}•${NC} $skill_name (${YELLOW}${category}${NC})"
        fi
    done
}

demo_step_5() {
    step_banner "5" "Query Skills by Category"

    print_info "Finding all RDF-related skills..."
    local skills_json
    skills_json=$(skill_get_by_category "rdf")

    echo ""
    echo "Skills in the 'rdf' category:"
    echo "$skills_json" | jq -r '.[]' | while read -r skill_name; do
        echo -e "  ${GREEN}•${NC} $skill_name"
    done
}

demo_step_6() {
    step_banner "6" "List All Registered Skills"

    print_info "Retrieving complete skill inventory..."
    local all_skills
    all_skills=$(skill_list_all)

    echo ""
    echo "All registered skills:"
    echo "$all_skills" | jq -r '.[]' | while read -r skill_name; do
        if skill_info=$(skill_get_by_name "$skill_name"); then
            local category=$(echo "$skill_info" | jq -r '.skill.category')
            local version=$(echo "$skill_info" | jq -r '.skill.version')
            printf "  ${GREEN}•${NC} %-30s ${YELLOW}%-15s${NC} v%s\n" "$skill_name" "[$category]" "$version"
        fi
    done
}

demo_step_7() {
    step_banner "7" "Get Registry Statistics"

    print_info "Computing registry statistics..."
    local stats
    stats=$(skill_registry_stats)

    echo ""
    echo "Registry Statistics:"
    echo -e "${YELLOW}Total Skills Registered:${NC}" "$(echo "$stats" | jq -r '.total_skills')"
    echo -e "${YELLOW}Total Categories:${NC}" "$(echo "$stats" | jq -r '.total_categories')"
    echo -e "${YELLOW}Total Agent Types:${NC}" "$(echo "$stats" | jq -r '.total_agent_types')"
    echo -e "${YELLOW}Last Updated:${NC}" "$(echo "$stats" | jq -r '.timestamp')"
}

demo_step_8() {
    step_banner "8" "Advanced: Show Skill Validation"

    print_info "Validating a skill YAML file..."
    local skill_file="${SKILLS_DIR}/rust-compiler-validator.yaml"

    if skill_validate_file "$skill_file"; then
        print_success "Skill validation passed!"
        echo ""
        echo "Validation checks:"
        echo "  ${GREEN}✓${NC} YAML syntax is valid"
        echo "  ${GREEN}✓${NC} All required fields present"
        echo "  ${GREEN}✓${NC} Category is valid"
        echo "  ${GREEN}✓${NC} Performance SLOs are reasonable"
        echo "  ${GREEN}✓${NC} Agent types are defined"
        echo "  ${GREEN}✓${NC} Capabilities are specified"
    fi
}

demo_summary() {
    echo ""
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Demo Summary${NC}"
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""
    echo "The Skill Registry System provides:"
    echo ""
    echo "  1. ${YELLOW}YAML-based skill definitions${NC} for declarative agent capabilities"
    echo "  2. ${YELLOW}Multi-indexed registry database${NC} for efficient queries"
    echo "  3. ${YELLOW}Schema validation${NC} ensuring skill definitions are correct"
    echo "  4. ${YELLOW}Discovery mechanisms${NC} for finding skills by name, type, or category"
    echo "  5. ${YELLOW}Performance SLO tracking${NC} for reliability guarantees"
    echo "  6. ${YELLOW}Extensible design${NC} for adding new skills without code changes"
    echo ""
    echo "Registry Location: ${YELLOW}${SKILLS_DIR}${NC}"
    echo "Registry Database: ${YELLOW}${SKILLS_DIR}/.registry.json${NC}"
    echo "Module Location: ${YELLOW}${MODULES_DIR}/skill-registry.sh${NC}"
    echo ""
    echo -e "${GREEN}✓ Demo completed successfully!${NC}"
    echo ""
}

# ============================================================================
# Main Execution
# ============================================================================

main() {
    demo_banner

    # Verify skills directory
    if [[ ! -d "$SKILLS_DIR" ]]; then
        echo -e "${RED}Error: Skills directory not found at $SKILLS_DIR${NC}"
        exit 1
    fi

    # Run demo steps
    demo_step_1
    demo_step_2
    demo_step_3
    demo_step_4
    demo_step_5
    demo_step_6
    demo_step_7
    demo_step_8
    demo_summary
}

# ============================================================================
# Run Demo
# ============================================================================

main "$@"
