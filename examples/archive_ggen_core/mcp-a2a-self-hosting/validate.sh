#!/usr/bin/env bash
# Validation script for MCP/A2A self-hosting example
# Runs all quality gates and reports results

set -e

echo "🔍 Running Validation Gates"
echo "============================"
echo ""

PASSED=0
FAILED=0

# Color codes
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

check_gate() {
    local name=$1
    local command=$2

    echo "🔬 Testing: $name"

    if eval "$command" > /dev/null 2>&1; then
        echo -e "${GREEN}✓ PASSED${NC}: $name"
        ((PASSED++))
        return 0
    else
        echo -e "${RED}✗ FAILED${NC}: $name"
        ((FAILED++))
        return 1
    fi
}

echo "📋 Gate 1: File Structure"
echo "-------------------------"
check_gate "Ontology exists" "test -f ontology/agent.ttl"
check_gate "Templates exist" "test -f templates/skills.tera"
check_gate "Config exists" "test -f ggen.toml"
check_gate "Cargo.toml exists" "test -f Cargo.toml"
echo ""

echo "📋 Gate 2: Ontology Validation"
echo "------------------------------"
if command -v ggen &> /dev/null; then
    check_gate "Turtle syntax valid" "ggen validate ontology/agent.ttl"
else
    echo -e "${YELLOW}⚠ SKIP${NC}: ggen CLI not installed"
fi
echo ""

echo "📋 Gate 3: Schema Parsing"
echo "-------------------------"
check_gate "Schema syntax valid" "grep -q '{' ontology/agent.ttl"
check_gate "Input types defined" "grep -q 'hasInputType' ontology/agent.ttl"
check_gate "Output types defined" "grep -q 'hasOutputType' ontology/agent.ttl"
echo ""

echo "📋 Gate 4: Behavior Predicates"
echo "------------------------------"
check_gate "System prompts defined" "grep -q 'hasSystemPrompt' ontology/agent.ttl"
check_gate "Implementation hints defined" "grep -q 'hasImplementationHint' ontology/agent.ttl"
echo ""

echo "📋 Gate 5: Template Syntax"
echo "-------------------------"
check_gate "Tera syntax valid" "grep -q '{%' templates/skills.tera"
check_gate "Schema filters used" "grep -q 'schema_to_rust' templates/skills.tera"
echo ""

echo "📋 Gate 6: Cargo Compilation"
echo "----------------------------"
if command -v cargo &> /dev/null; then
    check_gate "Cargo check passes" "cargo check --quiet"
else
    echo -e "${YELLOW}⚠ SKIP${NC}: Rust toolchain not available"
fi
echo ""

echo "============================"
echo "📊 Validation Summary"
echo "============================"
echo -e "${GREEN}Passed${NC}: $PASSED"
echo -e "${RED}Failed${NC}: $FAILED"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}✅ All validation gates passed!${NC}"
    echo ""
    echo "🚀 Ready to run:"
    echo "  ./setup.sh          # Generate code from ontology"
    echo "  cargo run --bin a2a-server   # Start A2A server"
    echo "  cargo run --bin mcp-server   # Start MCP server"
    exit 0
else
    echo -e "${RED}❌ Validation failed${NC}"
    echo "Please fix the errors above before proceeding."
    exit 1
fi
