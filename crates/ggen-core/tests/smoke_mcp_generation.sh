#!/usr/bin/env bash
# Smoke test for MCP server generation from RDF ontologies.
# Tests the complete μ₁-μ₅ pipeline with real filesystem I/O (Chicago TDD).
#
# Usage: ./smoke_mcp_generation.sh
#
# Stages:
#   1. Prerequisites - Check ggen binary, Rust toolchain, temp directory
#   2. Ontology validation - Verify test TTL exists and is valid Turtle
#   3. Generation - Run generate_mcp_server() with real config
#   4. Output verification - Check generated files exist and have content
#   5. Compilation - Verify generated code compiles with cargo check
#
# Exit codes:
#   0 - All stages passed
#   1 - One or more stages failed
#   2 - Prerequisites not met

set -euo pipefail

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKTREE_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
TEMP_DIR="${WORKTREE_ROOT}/target/smoke_test_mcp"
ONTOLOGY_FILE="${TEMP_DIR}/test_server.ttl"
OUTPUT_DIR="${TEMP_DIR}/generated"
SERVER_NAME="smoke_test_server"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

log_stage() {
    echo -e "${YELLOW}=== Stage $1: $2 ===${NC}"
}

log_pass() {
    echo -e "${GREEN}✓ PASS:${NC} $1"
}

log_fail() {
    echo -e "${RED}✗ FAIL:${NC} $1"
}

log_info() {
    echo -e "  INFO: $1"
}

# Count files in directory
count_files() {
    find "$1" -type f | wc -l | tr -d ' '
}

# Check if file exists and has content > 0
check_file_nonempty() {
    local file="$1"
    local name="$2"

    if [[ ! -f "${file}" ]]; then
        log_fail "${name} does not exist: ${file}"
        return 1
    fi

    if [[ ! -s "${file}" ]]; then
        log_fail "${name} is empty: ${file}"
        return 1
    fi

    log_pass "${name} exists and has content"
    return 0
}

# ---------------------------------------------------------------------------
# Stage 1: Prerequisites
# ---------------------------------------------------------------------------

stage_1_prerequisites() {
    log_stage "1" "Prerequisites"

    local passed=true

    # Check if we're in the worktree
    if [[ ! -d "${WORKTREE_ROOT}/crates/ggen-core" ]]; then
        log_fail "Not in ggen worktree: ${WORKTREE_ROOT}"
        passed=false
    else
        log_pass "Worktree root exists: ${WORKTREE_ROOT}"
    fi

    # Check if ggen binary exists
    if [[ ! -f "${WORKTREE_ROOT}/ggen-binary" ]]; then
        log_fail "ggen-binary not found at ${WORKTREE_ROOT}/ggen-binary"
        log_info "Run: cargo build --release"
        passed=false
    else
        log_pass "ggen-binary exists"
    fi

    # Check if Rust toolchain is available
    if ! command -v cargo &> /dev/null; then
        log_fail "cargo not found in PATH"
        passed=false
    else
        log_pass "cargo is available: $(cargo --version | head -1)"
    fi

    # Create temp directory
    if [[ -d "${TEMP_DIR}" ]]; then
        log_info "Cleaning up existing temp directory: ${TEMP_DIR}"
        rm -rf "${TEMP_DIR}"
    fi

    mkdir -p "${TEMP_DIR}"
    log_pass "Created temp directory: ${TEMP_DIR}"

    if [[ "${passed}" == "true" ]]; then
        log_pass "Stage 1: Prerequisites met"
        return 0
    else
        log_fail "Stage 1: Prerequisites not met"
        return 1
    fi
}

# ---------------------------------------------------------------------------
# Stage 2: Ontology validation
# ---------------------------------------------------------------------------

stage_2_ontology_validation() {
    log_stage "2" "Ontology validation"

    local passed=true

    # Create minimal valid Turtle ontology for MCP generation
    log_info "Creating test ontology: ${ONTOLOGY_FILE}"

    cat > "${ONTOLOGY_FILE}" << 'EOF'
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix mcp: <http://ggen.dev/ns/mcp#> .

mcp:SmokeTestServer a mcp:Server ;
    rdfs:label "Smoke Test MCP Server" ;
    rdfs:comment "Minimal test ontology for smoke testing MCP generation" ;
    mcp:transport "stdio" ;
    mcp:version "0.1.0" .
EOF

    if [[ ! -f "${ONTOLOGY_FILE}" ]]; then
        log_fail "Failed to create ontology file"
        passed=false
    else
        log_pass "Created ontology file: ${ONTOLOGY_FILE}"
    fi

    # Validate Turtle syntax (basic check)
    if grep -q '@prefix' "${ONTOLOGY_FILE}" && \
       grep -q 'mcp:SmokeTestServer' "${ONTOLOGY_FILE}" && \
       grep -q 'mcp:transport' "${ONTOLOGY_FILE}"; then
        log_pass "Ontology has valid Turtle structure"
    else
        log_fail "Ontology is missing required Turtle elements"
        passed=false
    fi

    # Count triples (rough check)
    local triple_count
    triple_count=$(grep -c '\.' "${ONTOLOGY_FILE}" || echo "0")
    log_info "Ontology has ${triple_count} triples (approximate)"

    if [[ "${passed}" == "true" ]]; then
        log_pass "Stage 2: Ontology validation passed"
        return 0
    else
        log_fail "Stage 2: Ontology validation failed"
        return 1
    fi
}

# ---------------------------------------------------------------------------
# Stage 3: Generation
# ---------------------------------------------------------------------------

stage_3_generation() {
    log_stage "3" "Generation (μ₁-μ₅ pipeline)"

    local passed=true

    # Create a simple Rust test program that calls generate_mcp_server()
    local test_program="${TEMP_DIR}/run_generation.rs"

    log_info "Creating test program: ${test_program}"

    cat > "${test_program}" << EOF
use ggen_core::mcp::{McpConfig, McpTransport, generate_mcp_server};
use std::path::PathBuf;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = McpConfig {
        ontology_path: PathBuf::from("${ONTOLOGY_FILE}"),
        output_dir: PathBuf::from("${OUTPUT_DIR}"),
        transport: McpTransport::Stdio,
        server_name: "${SERVER_NAME}".to_string(),
        validate: false,
        dry_run: false,
    };

    println!("Starting MCP server generation...");
    let result = generate_mcp_server(config).await?;

    println!("Generation successful!");
    println!("  Server name: {}", result.server_name);
    println!("  Transport: {}", result.transport);
    println!("  Files generated: {}", result.files_generated.len());
    println!("  Elapsed time: {}ms", result.elapsed_ms);
    println!("  Receipt: {}", result.receipt);

    // Verify receipt is 64 hex characters (SHA-256)
    assert_eq!(result.receipt.len(), 64, "Receipt should be 64 hex chars");
    assert!(result.receipt.chars().all(|c| c.is_ascii_hexdigit()), "Receipt should be hex");

    // Verify expected number of files (7 for stdio transport)
    assert_eq!(result.files_generated.len(), 7, "Expected 7 generated files");

    Ok(())
}
EOF

    # Compile and run the test program
    log_info "Compiling test program..."
    local compile_output
    if compile_output=$(rustc --edition 2021 \
        -o "${TEMP_DIR}/run_generation" \
        --crate-type bin \
        --extern "ggen_core=${WORKTREE_ROOT}/target/debug/libggen_core.rlib" \
        --extern "tokio=${WORKTREE_ROOT}/target/debug/deps/libtokio-*.rlib" \
        "${test_program}" 2>&1); then
        log_pass "Test program compiled successfully"
    else
        log_fail "Failed to compile test program"
        echo "${compile_output}"
        passed=false
    fi

    # Run the generation
    if [[ "${passed}" == "true" ]]; then
        log_info "Running generation..."
        local gen_output
        if gen_output=$("${TEMP_DIR}/run_generation" 2>&1); then
            log_pass "Generation executed successfully"
            echo "${gen_output}"
        else
            log_fail "Generation failed"
            echo "${gen_output}"
            passed=false
        fi
    fi

    if [[ "${passed}" == "true" ]]; then
        log_pass "Stage 3: Generation passed"
        return 0
    else
        log_fail "Stage 3: Generation failed"
        return 1
    fi
}

# ---------------------------------------------------------------------------
# Stage 4: Output verification
# ---------------------------------------------------------------------------

stage_4_output_verification() {
    log_stage "4" "Output verification"

    local passed=true

    # Check output directory exists
    if [[ ! -d "${OUTPUT_DIR}" ]]; then
        log_fail "Output directory not created: ${OUTPUT_DIR}"
        return 1
    fi

    log_pass "Output directory exists: ${OUTPUT_DIR}"

    # Count generated files
    local file_count
    file_count=$(count_files "${OUTPUT_DIR}")
    log_info "Found ${file_count} files in output directory"

    if [[ "${file_count}" -lt 7 ]]; then
        log_fail "Expected at least 7 files, found ${file_count}"
        passed=false
    else
        log_pass "File count is correct: ${file_count} files"
    fi

    # Check each required file exists and has content
    check_file_nonempty "${OUTPUT_DIR}/main.rs" "main.rs" || passed=false
    check_file_nonempty "${OUTPUT_DIR}/server.rs" "server.rs" || passed=false
    check_file_nonempty "${OUTPUT_DIR}/tools.rs" "tools.rs" || passed=false
    check_file_nonempty "${OUTPUT_DIR}/resources.rs" "resources.rs" || passed=false
    check_file_nonempty "${OUTPUT_DIR}/prompts.rs" "prompts.rs" || passed=false
    check_file_nonempty "${OUTPUT_DIR}/stdio_server.rs" "stdio_server.rs" || passed=false
    check_file_nonempty "${OUTPUT_DIR}/Cargo.toml" "Cargo.toml" || passed=false

    # Verify main.rs contains expected content
    if grep -q "${SERVER_NAME}" "${OUTPUT_DIR}/main.rs"; then
        log_pass "main.rs contains server name: ${SERVER_NAME}"
    else
        log_fail "main.rs missing server name"
        passed=false
    fi

    if grep -q "#\[tokio::main\]" "${OUTPUT_DIR}/main.rs"; then
        log_pass "main.rs contains tokio::main macro"
    else
        log_fail "main.rs missing tokio::main macro"
        passed=false
    fi

    # Verify Cargo.toml has correct package name
    local expected_name="smoke-test-server"  # underscores -> dashes
    if grep -q "name = \"${expected_name}\"" "${OUTPUT_DIR}/Cargo.toml"; then
        log_pass "Cargo.toml has correct package name: ${expected_name}"
    else
        log_fail "Cargo.toml has incorrect package name"
        passed=false
    fi

    if grep -q "rmcp" "${OUTPUT_DIR}/Cargo.toml"; then
        log_pass "Cargo.toml includes rmcp dependency"
    else
        log_fail "Cargo.toml missing rmcp dependency"
        passed=false
    fi

    if grep -q "tokio" "${OUTPUT_DIR}/Cargo.toml"; then
        log_pass "Cargo.toml includes tokio dependency"
    else
        log_fail "Cargo.toml missing tokio dependency"
        passed=false
    fi

    # Verify server.rs has PascalCase struct name
    local expected_struct="SmokeTestServer"
    if grep -q "${expected_struct}" "${OUTPUT_DIR}/server.rs"; then
        log_pass "server.rs has PascalCase struct: ${expected_struct}"
    else
        log_fail "server.rs missing expected struct name"
        passed=false
    fi

    if [[ "${passed}" == "true" ]]; then
        log_pass "Stage 4: Output verification passed"
        return 0
    else
        log_fail "Stage 4: Output verification failed"
        return 1
    fi
}

# ---------------------------------------------------------------------------
# Stage 5: Compilation
# ---------------------------------------------------------------------------

stage_5_compilation() {
    log_stage "5" "Compilation"

    local passed=true

    # Change to output directory for cargo check
    local original_dir
    original_dir="$(pwd)"

    cd "${OUTPUT_DIR}"

    # Run cargo check to verify generated code compiles
    log_info "Running cargo check on generated code..."
    local check_output
    if check_output=$(cargo check 2>&1); then
        log_pass "Generated code compiles successfully"
    else
        log_fail "Generated code does not compile"
        echo "${check_output}"
        passed=false
    fi

    # Return to original directory
    cd "${original_dir}"

    if [[ "${passed}" == "true" ]]; then
        log_pass "Stage 5: Compilation passed"
        return 0
    else
        log_fail "Stage 5: Compilation failed"
        return 1
    fi
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

main() {
    local start_time
    start_time=$(date +%s)

    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║  MCP Server Generation Smoke Test                          ║"
    echo "║  Testing μ₁-μ₅ pipeline with real filesystem I/O            ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo ""

    local stages_passed=0
    local stages_failed=0

    # Run all stages
    if stage_1_prerequisites; then
        ((stages_passed++)) || true
    else
        ((stages_failed++)) || true
        echo ""
        echo -e "${RED}=== SMOKE TEST FAILED ===${NC}"
        echo "Stage 1 (Prerequisites) failed. Aborting."
        exit 1
    fi
    echo ""

    if stage_2_ontology_validation; then
        ((stages_passed++)) || true
    else
        ((stages_failed++)) || true
    fi
    echo ""

    if stage_3_generation; then
        ((stages_passed++)) || true
    else
        ((stages_failed++)) || true
    fi
    echo ""

    if stage_4_output_verification; then
        ((stages_passed++)) || true
    else
        ((stages_failed++)) || true
    fi
    echo ""

    if stage_5_compilation; then
        ((stages_passed++)) || true
    else
        ((stages_failed++)) || true
    fi
    echo ""

    # Calculate total runtime
    local end_time
    end_time=$(date +%s)
    local runtime=$((end_time - start_time))

    # Print summary
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║  Smoke Test Summary                                          ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo ""
    echo "Stages passed: ${stages_passed}/5"
    echo "Stages failed: ${stages_failed}/5"
    echo "Total runtime: ${runtime}s"
    echo ""

    if [[ "${stages_failed}" -eq 0 ]]; then
        echo -e "${GREEN}✓ ALL STAGES PASSED${NC}"
        echo ""
        echo "Generated files:"
        ls -1 "${OUTPUT_DIR}/" | sed 's/^/  - /'
        echo ""
        echo "Output directory: ${OUTPUT_DIR}"
        exit 0
    else
        echo -e "${RED}✗ SMOKE TEST FAILED${NC}"
        echo ""
        echo "Failed stages:"
        [[ "${stages_passed}" -lt 2 ]] && echo "  - Stage 2: Ontology validation"
        [[ "${stages_passed}" -lt 3 ]] && echo "  - Stage 3: Generation"
        [[ "${stages_passed}" -lt 4 ]] && echo "  - Stage 4: Output verification"
        [[ "${stages_passed}" -lt 5 ]] && echo "  - Stage 5: Compilation"
        echo ""
        echo "Output directory (for inspection): ${OUTPUT_DIR}"
        exit 1
    fi
}

# Run main
main "$@"
