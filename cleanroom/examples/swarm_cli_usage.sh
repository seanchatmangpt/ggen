#!/bin/bash
# Cleanroom Swarm CLI Usage Examples
#
# This script demonstrates the comprehensive swarm coordination capabilities
# of the Cleanroom CLI, inspired by the Hive Queen architecture.

set -e

echo "🐝 Cleanroom Swarm CLI - Comprehensive Examples"
echo "================================================"
echo

# Color codes for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Example 1: Basic Swarm Initialization
echo -e "${BLUE}Example 1: Initialize a Mesh Swarm${NC}"
echo "Command: cleanroom swarm init --topology mesh --agents 5 --output json"
echo

cleanroom swarm init --topology mesh --agents 5 --output json

echo
echo -e "${GREEN}✓ Swarm initialized successfully${NC}"
echo
sleep 1

# Example 2: Hierarchical Swarm with Auto-Scaling
echo -e "${BLUE}Example 2: Hierarchical Swarm with Auto-Scaling${NC}"
echo "Command: cleanroom swarm init --topology hierarchical --agents 10 --strategy adaptive --auto-scale"
echo

cleanroom swarm init --topology hierarchical --agents 10 --strategy adaptive --auto-scale --output json

echo
echo -e "${GREEN}✓ Hierarchical swarm with auto-scaling enabled${NC}"
echo
sleep 1

# Example 3: Spawn Individual Agents
echo -e "${BLUE}Example 3: Spawn Specialized Agents${NC}"
echo "Creating a diverse swarm with different agent types..."
echo

echo "Spawning Coder Agent:"
cleanroom swarm spawn \
    --type coder \
    --name "agent-coder-1" \
    --capabilities "code-generation,refactoring,optimization" \
    --output json

echo
echo "Spawning Tester Agent:"
cleanroom swarm spawn \
    --type tester \
    --name "agent-tester-1" \
    --capabilities "testing,coverage-analysis,integration-tests" \
    --output json

echo
echo "Spawning Researcher Agent:"
cleanroom swarm spawn \
    --type researcher \
    --name "agent-researcher-1" \
    --capabilities "research,documentation,best-practices" \
    --output json

echo
echo "Spawning Analyst Agent:"
cleanroom swarm spawn \
    --type analyst \
    --name "agent-analyst-1" \
    --capabilities "performance-analysis,bottleneck-detection,optimization" \
    --output json

echo
echo "Spawning Coordinator Agent:"
cleanroom swarm spawn \
    --type coordinator \
    --name "agent-coordinator-1" \
    --capabilities "task-orchestration,resource-management,load-balancing" \
    --output json

echo
echo -e "${GREEN}✓ All agents spawned successfully${NC}"
echo
sleep 1

# Example 4: Orchestrate Complex Tasks
echo -e "${BLUE}Example 4: Orchestrate Complex Tasks${NC}"
echo

echo "Task 1: Build and Test Project (High Priority, Parallel Execution)"
cleanroom swarm orchestrate \
    --task "Build the Rust project and run all tests with coverage analysis" \
    --priority high \
    --strategy parallel \
    --max-agents 3 \
    --output json

echo
echo "Task 2: Performance Optimization (Medium Priority, Adaptive Execution)"
cleanroom swarm orchestrate \
    --task "Analyze performance bottlenecks and suggest optimizations" \
    --priority medium \
    --strategy adaptive \
    --max-agents 2 \
    --timeout 300 \
    --output json

echo
echo "Task 3: Documentation Generation (Low Priority, Sequential Execution)"
cleanroom swarm orchestrate \
    --task "Generate comprehensive API documentation and usage examples" \
    --priority low \
    --strategy sequential \
    --max-agents 1 \
    --output json

echo
echo -e "${GREEN}✓ Tasks orchestrated successfully${NC}"
echo
sleep 1

# Example 5: Monitor Swarm Status
echo -e "${BLUE}Example 5: Monitor Swarm Status${NC}"
echo

echo "Basic Status:"
cleanroom swarm status --output json

echo
echo "Detailed Status:"
cleanroom swarm status --detailed --output json

echo
echo -e "${GREEN}✓ Swarm status retrieved${NC}"
echo
sleep 1

# Example 6: List All Agents
echo -e "${BLUE}Example 6: List All Agents${NC}"
echo

echo "All Agents:"
cleanroom swarm list --output json

echo
echo "Active Agents Only:"
cleanroom swarm list --filter active --output json

echo
echo "Idle Agents Only:"
cleanroom swarm list --filter idle --output json

echo
echo -e "${GREEN}✓ Agent list retrieved${NC}"
echo
sleep 1

# Example 7: Get Performance Metrics
echo -e "${BLUE}Example 7: Performance Metrics${NC}"
echo

echo "Swarm-Wide Metrics:"
cleanroom swarm metrics --output json

echo
echo "Agent-Specific Metrics:"
cleanroom swarm metrics --agent-id "agent-001" --metric cpu --output json

echo
echo "Memory Metrics:"
cleanroom swarm metrics --metric memory --output json

echo
echo -e "${GREEN}✓ Performance metrics retrieved${NC}"
echo
sleep 1

# Example 8: Run Commands in Cleanroom
echo -e "${BLUE}Example 8: Run Commands with Security Policies${NC}"
echo

echo "Simple Command:"
cleanroom run echo "Hello from Cleanroom!" --output json

echo
echo "Command with Network Isolation:"
cleanroom run curl https://example.com \
    --network-isolation \
    --output json || echo "Expected to fail due to network isolation"

echo
echo "Command with Resource Limits:"
cleanroom run python3 --version \
    --max-memory 256 \
    --max-cpu 50.0 \
    --timeout 30 \
    --output json

echo
echo -e "${GREEN}✓ Commands executed with policies${NC}"
echo
sleep 1

# Example 9: Environment Management
echo -e "${BLUE}Example 9: Environment Management${NC}"
echo

echo "Create Environment:"
cleanroom env create --name "test-env-1" --output json

echo
echo "List Environments:"
cleanroom env list --output json

echo
echo "Show Environment Details:"
cleanroom env show "test-env-1" --output json

echo
echo -e "${GREEN}✓ Environment operations completed${NC}"
echo
sleep 1

# Example 10: Performance Benchmarking
echo -e "${BLUE}Example 10: Performance Benchmarking${NC}"
echo

echo "Container Benchmark:"
cleanroom bench --bench-type container --iterations 10 --output json

echo
echo "Swarm Benchmark:"
cleanroom bench --bench-type swarm --iterations 5 --output json

echo
echo -e "${GREEN}✓ Benchmarks completed${NC}"
echo
sleep 1

# Example 11: System Status
echo -e "${BLUE}Example 11: System Status${NC}"
echo

cleanroom status --output json

echo
echo -e "${GREEN}✓ System status retrieved${NC}"
echo
sleep 1

# Example 12: Cleanup and Stop Swarm
echo -e "${BLUE}Example 12: Cleanup Operations${NC}"
echo

echo "Delete Environment:"
cleanroom env delete "test-env-1" --force --output json

echo
echo "Stop Swarm:"
cleanroom swarm stop --output json

echo
echo -e "${GREEN}✓ Cleanup completed${NC}"
echo

# Example 13: Advanced Workflow
echo -e "${BLUE}Example 13: Complete Development Workflow${NC}"
echo "Demonstrating a full development cycle using swarm coordination..."
echo

# Initialize swarm
echo "Step 1: Initialize Development Swarm"
cleanroom swarm init --topology mesh --agents 8 --strategy balanced --output json

# Spawn specialized agents
echo "Step 2: Spawn Specialized Development Agents"
cleanroom swarm spawn --type coder --name "backend-dev" --capabilities "rust,backend,api"
cleanroom swarm spawn --type coder --name "frontend-dev" --capabilities "typescript,react,ui"
cleanroom swarm spawn --type tester --name "test-engineer" --capabilities "testing,tdd,integration"
cleanroom swarm spawn --type researcher --name "research-analyst" --capabilities "research,patterns,best-practices"
cleanroom swarm spawn --type optimizer --name "perf-optimizer" --capabilities "performance,profiling,optimization"

# Orchestrate development tasks
echo "Step 3: Orchestrate Development Tasks"
cleanroom swarm orchestrate \
    --task "Research and implement REST API with authentication" \
    --priority high \
    --strategy adaptive \
    --max-agents 5

cleanroom swarm orchestrate \
    --task "Create comprehensive test suite with 90% coverage" \
    --priority high \
    --strategy parallel \
    --max-agents 3

cleanroom swarm orchestrate \
    --task "Optimize performance and identify bottlenecks" \
    --priority medium \
    --strategy sequential \
    --max-agents 2

# Monitor progress
echo "Step 4: Monitor Development Progress"
cleanroom swarm status --detailed --output json
cleanroom swarm metrics --output json

# Complete workflow
echo "Step 5: Complete Workflow"
cleanroom swarm stop --output json

echo
echo -e "${GREEN}✓ Complete development workflow executed${NC}"
echo

# Summary
echo
echo -e "${YELLOW}════════════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}Summary: Cleanroom Swarm CLI Features${NC}"
echo -e "${YELLOW}════════════════════════════════════════════════════════════${NC}"
echo
echo "✓ Swarm Initialization (mesh, hierarchical, ring, star)"
echo "✓ Agent Spawning (coder, tester, researcher, analyst, optimizer)"
echo "✓ Task Orchestration (parallel, sequential, adaptive)"
echo "✓ Status Monitoring (basic and detailed)"
echo "✓ Performance Metrics (agent and swarm-wide)"
echo "✓ Environment Management (create, list, delete)"
echo "✓ Security Policies (network isolation, resource limits)"
echo "✓ Performance Benchmarking"
echo "✓ Complete Development Workflows"
echo
echo -e "${GREEN}All examples completed successfully!${NC}"
echo
echo "For more information:"
echo "  cleanroom --help"
echo "  cleanroom swarm --help"
echo "  cleanroom run --help"
echo
