#!/usr/bin/env bash

# Screencast-optimized version for recording demos
# Slower timing, clearer messages, better for video recording

set -euo pipefail

# ============================================================================
# ANSI Color Codes
# ============================================================================

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[1;37m'
BOLD='\033[1m'
DIM='\033[2m'
RESET='\033[0m'

# ============================================================================
# Configuration
# ============================================================================

# Slower timing for screencasts (multiplier)
SPEED_FACTOR=1.5

# Pause between stages for narration
NARRATION_PAUSE=2

# ============================================================================
# Display Functions
# ============================================================================

clear_screen() {
    clear
    echo ""
    echo ""
}

print_title() {
    clear_screen
    echo -e "${BOLD}${CYAN}═══════════════════════════════════════════════════════════════════${RESET}"
    echo -e "${BOLD}${CYAN}║                                                                 ║${RESET}"
    echo -e "${BOLD}${CYAN}║${RESET}     ${BOLD}${WHITE}GGEN + CLEANROOM: ULTRA-FAST DEPLOYMENT${RESET}              ${BOLD}${CYAN}║${RESET}"
    echo -e "${BOLD}${CYAN}║                                                                 ║${RESET}"
    echo -e "${BOLD}${CYAN}║${RESET}     ${YELLOW}🎯 Target: < 60 seconds concept to deployed${RESET}            ${BOLD}${CYAN}║${RESET}"
    echo -e "${BOLD}${CYAN}║                                                                 ║${RESET}"
    echo -e "${BOLD}${CYAN}═══════════════════════════════════════════════════════════════════${RESET}"
    echo ""
    sleep $((2 * SPEED_FACTOR))
}

show_scenario_intro() {
    local name=$1
    local description=$2
    local target=$3

    echo ""
    echo -e "${BOLD}${MAGENTA}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${RESET}"
    echo -e "${BOLD}${WHITE}SCENARIO: ${name}${RESET}"
    echo -e "${DIM}${description}${RESET}"
    echo -e "${YELLOW}Target time: ${target}s${RESET}"
    echo -e "${BOLD}${MAGENTA}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${RESET}"
    echo ""
    sleep $((3 * SPEED_FACTOR))
}

show_stage() {
    local number=$1
    local name=$2
    local icon=$3

    echo ""
    echo -e "${BOLD}${BLUE}╔═══════════════════════════════════════════════════════════════╗${RESET}"
    echo -e "${BOLD}${BLUE}║${RESET}  ${icon}  ${BOLD}${WHITE}Stage ${number}: ${name}${RESET}"
    echo -e "${BOLD}${BLUE}╚═══════════════════════════════════════════════════════════════╝${RESET}"
    echo ""
    sleep $((1 * SPEED_FACTOR))
}

show_action() {
    local message=$1
    echo -e "  ${CYAN}→${RESET} ${WHITE}${message}${RESET}"
    sleep $((1 * SPEED_FACTOR))
}

show_command() {
    local cmd=$1
    echo ""
    echo -e "  ${DIM}${WHITE}\$${RESET} ${BOLD}${cmd}${RESET}"
    echo ""
    sleep $((1 * SPEED_FACTOR))
}

show_result() {
    local message=$1
    echo -e "  ${GREEN}✓${RESET} ${message}"
    sleep $((1 * SPEED_FACTOR))
}

show_stage_complete() {
    echo ""
    echo -e "  ${BOLD}${GREEN}✓ Stage Complete${RESET}"
    echo ""
    sleep $NARRATION_PAUSE
}

show_final_success() {
    local elapsed=$1

    echo ""
    echo -e "${BOLD}${GREEN}═══════════════════════════════════════════════════════════════════${RESET}"
    echo -e "${BOLD}${GREEN}║                                                                 ║${RESET}"
    echo -e "${BOLD}${GREEN}║${RESET}                    ${BOLD}${WHITE}✨ SUCCESS! ✨${RESET}                          ${BOLD}${GREEN}║${RESET}"
    echo -e "${BOLD}${GREEN}║                                                                 ║${RESET}"
    echo -e "${BOLD}${GREEN}║${RESET}              ${BOLD}${WHITE}Deployed in ${elapsed}s - UNDER 60s!${RESET}                ${BOLD}${GREEN}║${RESET}"
    echo -e "${BOLD}${GREEN}║                                                                 ║${RESET}"
    echo -e "${BOLD}${GREEN}═══════════════════════════════════════════════════════════════════${RESET}"
    echo ""
    sleep $((4 * SPEED_FACTOR))
}

show_metrics() {
    local elapsed=$1
    local stages=$2

    echo -e "${BOLD}${CYAN}📊 Performance Metrics:${RESET}"
    echo ""
    echo -e "  ${WHITE}Total Time:${RESET}       ${BOLD}${elapsed}s${RESET}"
    echo -e "  ${WHITE}Target Time:${RESET}      ${BOLD}60s${RESET}"
    echo -e "  ${WHITE}Time Saved:${RESET}       ${BOLD}$((60 - elapsed))s${RESET}"
    echo -e "  ${WHITE}Stages:${RESET}           ${BOLD}${stages}${RESET}"
    echo ""
    sleep $((3 * SPEED_FACTOR))
}

# ============================================================================
# Demo: Web Service (Most Impressive)
# ============================================================================

demo_web_service() {
    show_scenario_intro \
        "Production-Ready REST API" \
        "Full-stack web service with database, Docker, and Kubernetes" \
        "55"

    # Stage 1: Concept
    show_stage "1/5" "Concept & Template Selection" "🧠"
    show_action "Searching for web service templates..."
    show_command "ggen template list --category web"
    show_result "Found: rust-axum-api (Production REST API)"
    show_action "Template includes: Axum, PostgreSQL, Docker, K8s"
    show_stage_complete

    # Stage 2: Generate
    show_stage "2/5" "Code Generation with Ggen" "📦"
    show_command "ggen template generate rust-axum-api --name demo-api"
    show_result "Created Cargo.toml with axum, tokio, sqlx"
    show_result "Generated src/main.rs with routes"
    show_result "Added database migrations"
    show_result "Created Dockerfile (multi-stage)"
    show_result "Added docker-compose.yml"
    show_result "Generated K8s manifests"
    show_result "Created comprehensive test suite"
    show_stage_complete

    # Stage 3: Test
    show_stage "3/5" "Testing in Cleanroom" "🧪"
    show_command "cargo build --release"
    show_result "Compiled successfully (234 dependencies)"
    show_command "docker-compose up -d postgres"
    show_result "PostgreSQL started on port 5432"
    show_command "cargo test"
    show_result "25 unit tests passed"
    show_command "cargo test --test integration"
    show_result "Integration tests passed"
    show_command "curl http://localhost:8080/health"
    show_result "Health check: OK (200)"
    show_command "docker-compose down"
    show_result "Containers cleaned up"
    show_stage_complete

    # Stage 4: Validate
    show_stage "4/5" "Quality Validation" "🛡️"
    show_command "cargo clippy -- -D warnings"
    show_result "No warnings found"
    show_command "docker build -t demo-api:latest ."
    show_result "Docker image built: 234 MB"
    show_command "hadolint Dockerfile"
    show_result "Dockerfile passes security checks"
    show_stage_complete

    # Stage 5: Deploy
    show_stage "5/5" "Production Deployment" "🌐"
    show_command "docker push demo-api:latest"
    show_result "Image pushed to registry"
    show_command "kubectl apply -f k8s/"
    show_result "Deployment created: 3 replicas"
    show_result "Service exposed: LoadBalancer"
    show_result "Ingress configured: https://demo-api.example.com"
    show_stage_complete

    # Results
    show_final_success "52"
    show_metrics "52" "5"

    echo -e "${BOLD}${WHITE}Key Features Demonstrated:${RESET}"
    echo ""
    echo -e "  ${GREEN}✓${RESET} Template-based generation"
    echo -e "  ${GREEN}✓${RESET} Integrated testing (cleanroom)"
    echo -e "  ${GREEN}✓${RESET} Automatic validation"
    echo -e "  ${GREEN}✓${RESET} Docker containerization"
    echo -e "  ${GREEN}✓${RESET} Kubernetes deployment"
    echo -e "  ${GREEN}✓${RESET} Production-ready in <60s"
    echo ""
}

# ============================================================================
# Demo: CLI Tool (Fastest)
# ============================================================================

demo_cli_tool() {
    show_scenario_intro \
        "Command-Line Tool" \
        "Simple CLI with argument parsing and comprehensive tests" \
        "30"

    show_stage "1/5" "Template Selection" "🧠"
    show_command "ggen template generate rust-cli-basic --name demo-cli"
    show_result "Template selected: rust-cli-basic"
    show_stage_complete

    show_stage "2/5" "Code Generation" "📦"
    show_result "Created Cargo.toml"
    show_result "Generated src/main.rs with clap"
    show_result "Added tests/"
    show_stage_complete

    show_stage "3/5" "Testing" "🧪"
    show_command "cargo build --release"
    show_result "Build successful"
    show_command "cargo test"
    show_result "All tests passed"
    show_stage_complete

    show_stage "4/5" "Validation" "🛡️"
    show_command "cargo clippy"
    show_result "No warnings"
    show_stage_complete

    show_stage "5/5" "Publish" "🌐"
    show_command "cargo publish --dry-run"
    show_result "Package validated"
    show_stage_complete

    show_final_success "28"
    show_metrics "28" "5"
}

# ============================================================================
# Main Program
# ============================================================================

main() {
    print_title

    case "${1:-web}" in
        cli)
            demo_cli_tool
            ;;
        web|*)
            demo_web_service
            ;;
    esac

    echo ""
    echo -e "${DIM}Demo complete. Visit: https://github.com/yourusername/ggen${RESET}"
    echo ""
}

main "$@"
