#!/usr/bin/env bash
# Cleanroom CLI - Complete Feature Demonstration
# Showcases noun-verb command structure and cross-language integration

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

CLI="../../target/debug/cleanroom"

# Check if CLI exists
if [[ ! -f "$CLI" ]]; then
    echo -e "${RED}❌ CLI binary not found at $CLI${NC}"
    echo "Please build first: cargo build --bin cleanroom"
    exit 1
fi

echo -e "${CYAN}════════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}        Cleanroom CLI - Complete Feature Demonstration        ${NC}"
echo -e "${CYAN}════════════════════════════════════════════════════════════${NC}"
echo ""

# ============================================================================
# Section 1: Basic Commands
# ============================================================================
echo -e "${BLUE}━━━ Section 1: Basic Commands ━━━${NC}"
echo ""

echo -e "${YELLOW}1.1 Version Information${NC}"
$CLI --version
echo ""

echo -e "${YELLOW}1.2 System Status (JSON)${NC}"
$CLI status --output json | jq '{system, version, health}'
echo ""

echo -e "${YELLOW}1.3 System Status (Text)${NC}"
$CLI status --output text | head -10
echo ""

# ============================================================================
# Section 2: Environment Management
# ============================================================================
echo -e "${BLUE}━━━ Section 2: Environment Management ━━━${NC}"
echo ""

echo -e "${YELLOW}2.1 Create Environment${NC}"
$CLI env create --name "demo-env" --output json | jq '{name, status, backend}'
echo ""

echo -e "${YELLOW}2.2 List Environments${NC}"
$CLI env list --output json | jq '{count, environments}'
echo ""

echo -e "${YELLOW}2.3 Show Environment Details${NC}"
$CLI env show demo-env --output json | jq '{name, status, containers}'
echo ""

# ============================================================================
# Section 3: Swarm Coordination (Hive Queen Inspired)
# ============================================================================
echo -e "${BLUE}━━━ Section 3: Swarm Coordination (Hive Queen Pattern) ━━━${NC}"
echo ""

echo -e "${YELLOW}3.1 Initialize Swarm - Mesh Topology${NC}"
SWARM_MESH=$($CLI swarm init --topology mesh --agents 5 --strategy balanced --output json)
echo "$SWARM_MESH" | jq '{swarm_id, topology, max_agents, strategy, health}'
SWARM_ID=$(echo "$SWARM_MESH" | jq -r '.swarm_id')
echo ""

echo -e "${YELLOW}3.2 Initialize Swarm - Hierarchical Topology${NC}"
$CLI swarm init --topology hierarchical --agents 8 --strategy specialized --output json | \
    jq '{swarm_id, topology, max_agents, strategy}'
echo ""

echo -e "${YELLOW}3.3 Spawn Agents${NC}"
echo "Spawning coder agent..."
$CLI swarm spawn --agent-type coder --name "coder-1" --capabilities "rust,python,testing" --output json | \
    jq '{agent_id, name, type, capabilities, status}'
echo ""

echo "Spawning researcher agent..."
$CLI swarm spawn --agent-type researcher --name "researcher-1" --capabilities "analysis,documentation" --output json | \
    jq '{agent_id, name, type, capabilities}'
echo ""

echo "Spawning tester agent..."
$CLI swarm spawn --agent-type tester --name "tester-1" --capabilities "integration-testing,e2e-testing" --output json | \
    jq '{agent_id, name, type}'
echo ""

echo -e "${YELLOW}3.4 Orchestrate Tasks${NC}"
echo "High priority task..."
$CLI swarm orchestrate --task "Build and test the project" --priority high --max-agents 3 --output json | \
    jq '{task_id, description, priority, status, progress}'
echo ""

echo "Critical task with timeout..."
$CLI swarm orchestrate --task "Deploy to production" --priority critical --timeout 300 --strategy sequential --output json | \
    jq '{task_id, priority, strategy, timeout}'
echo ""

echo -e "${YELLOW}3.5 Swarm Status${NC}"
echo "Basic status..."
$CLI swarm status --output json | jq '{swarm_id, agents, active_tasks, topology, health}'
echo ""

echo "Detailed status..."
$CLI swarm status --detailed --output json | jq '{
    swarm_id,
    topology,
    agents,
    tasks,
    performance: {
        avg_task_duration_ms: .performance.avg_task_duration_ms,
        throughput_per_minute: .performance.throughput_per_minute,
        success_rate: .performance.success_rate
    }
}'
echo ""

echo -e "${YELLOW}3.6 List Agents${NC}"
echo "All agents..."
$CLI swarm list --output json | jq '{count, agents: [.agents[] | {name, type, status}]}'
echo ""

echo "Active agents only..."
$CLI swarm list --filter active --output json | jq '{count, filter, agents: [.agents[] | .name]}'
echo ""

echo -e "${YELLOW}3.7 Swarm Metrics${NC}"
echo "Overall swarm metrics..."
$CLI swarm metrics --output json | jq '.swarm_metrics'
echo ""

# ============================================================================
# Section 4: Cross-Language Integration
# ============================================================================
echo -e "${BLUE}━━━ Section 4: Cross-Language Integration ━━━${NC}"
echo ""

echo -e "${YELLOW}4.1 Python Integration${NC}"
python3 << 'EOF'
import subprocess
import json

result = subprocess.run(
    ['../../target/debug/cleanroom', 'swarm', 'init', '--topology', 'star', '--agents', '10', '--output', 'json'],
    capture_output=True,
    text=True,
    check=True
)

data = json.loads(result.stdout)
print(f"✅ Python: Created {data['topology']} swarm with {data['max_agents']} agents")
print(f"   Swarm ID: {data['swarm_id']}")
print(f"   Health: {data['health']}")
EOF
echo ""

echo -e "${YELLOW}4.2 Node.js Integration${NC}"
node << 'EOF'
const { execSync } = require('child_process');

const result = execSync(
    '../../target/debug/cleanroom swarm metrics --output json',
    { encoding: 'utf8' }
);

const data = JSON.parse(result);
const metrics = data.swarm_metrics;

console.log(`✅ Node.js: Swarm has ${metrics.total_agents} agents`);
console.log(`   Avg CPU: ${metrics.avg_cpu_percent}%`);
console.log(`   Total Tasks: ${metrics.total_tasks}`);
console.log(`   Success Rate: ${(metrics.avg_success_rate * 100).toFixed(1)}%`);
EOF
echo ""

echo -e "${YELLOW}4.3 Bash Integration with jq${NC}"
STATUS=$($CLI status --output json)
echo "✅ Bash: System status parsed with jq"
echo "   System: $(echo "$STATUS" | jq -r '.system')"
echo "   Version: $(echo "$STATUS" | jq -r '.version')"
echo "   Active Swarms: $(echo "$STATUS" | jq -r '.swarms.active')"
echo "   Total Agents: $(echo "$STATUS" | jq -r '.swarms.agents')"
echo "   Health: $(echo "$STATUS" | jq -r '.health')"
echo ""

# ============================================================================
# Section 5: Performance Benchmarking
# ============================================================================
echo -e "${BLUE}━━━ Section 5: Performance Benchmarking ━━━${NC}"
echo ""

echo -e "${YELLOW}5.1 Swarm Benchmark${NC}"
$CLI bench --bench-type swarm --iterations 10 --output json | \
    jq '{benchmark_type, iterations, results, status}'
echo ""

echo -e "${YELLOW}5.2 Container Benchmark${NC}"
$CLI bench --bench-type container --iterations 5 --output json | \
    jq '.results'
echo ""

# ============================================================================
# Section 6: Different Output Formats
# ============================================================================
echo -e "${BLUE}━━━ Section 6: Output Format Demonstrations ━━━${NC}"
echo ""

echo -e "${YELLOW}6.1 JSON Output (Machine Readable)${NC}"
$CLI swarm status --output json | jq -c '{swarm: .swarm_id, agents: .agents}'
echo ""

echo -e "${YELLOW}6.2 Text Output (Human Readable)${NC}"
$CLI swarm status --output text | head -5
echo ""

echo -e "${YELLOW}6.3 Quiet Output (Errors Only)${NC}"
$CLI swarm list --output quiet
echo "✅ Quiet mode: Only errors would be shown"
echo ""

# ============================================================================
# Section 7: Advanced Swarm Features
# ============================================================================
echo -e "${BLUE}━━━ Section 7: Advanced Swarm Features ━━━${NC}"
echo ""

echo -e "${YELLOW}7.1 Ring Topology${NC}"
$CLI swarm init --topology ring --agents 6 --output json | \
    jq '{topology, max_agents, status}'
echo ""

echo -e "${YELLOW}7.2 Auto-Scaling Swarm${NC}"
$CLI swarm init --topology mesh --agents 5 --auto-scale --output json | \
    jq '{topology, max_agents, auto_scale, status}'
echo ""

echo -e "${YELLOW}7.3 Agent with Multiple Capabilities${NC}"
$CLI swarm spawn \
    --agent-type optimizer \
    --name "optimizer-1" \
    --capabilities "performance-tuning,resource-optimization,caching,load-balancing" \
    --output json | jq '{name, type, capabilities}'
echo ""

echo -e "${YELLOW}7.4 Parallel Task Orchestration${NC}"
$CLI swarm orchestrate \
    --task "Run integration tests in parallel" \
    --priority high \
    --strategy parallel \
    --max-agents 5 \
    --output json | jq '{task_id, strategy, max_agents, status}'
echo ""

echo -e "${YELLOW}7.5 Adaptive Task Execution${NC}"
$CLI swarm orchestrate \
    --task "Optimize database queries" \
    --priority medium \
    --strategy adaptive \
    --output json | jq '{task_id, strategy, priority}'
echo ""

# ============================================================================
# Cleanup
# ============================================================================
echo -e "${BLUE}━━━ Section 8: Cleanup ━━━${NC}"
echo ""

echo -e "${YELLOW}8.1 Stop Swarm${NC}"
$CLI swarm stop --output json | jq '{status, agents_terminated}'
echo ""

echo -e "${YELLOW}8.2 Environment Cleanup${NC}"
$CLI env cleanup --output json | jq '{status, environments_removed}'
echo ""

# ============================================================================
# Summary
# ============================================================================
echo -e "${CYAN}════════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}✅ Demonstration Complete!${NC}"
echo -e "${CYAN}════════════════════════════════════════════════════════════${NC}"
echo ""
echo -e "${YELLOW}Key Features Demonstrated:${NC}"
echo "  ✅ Noun-verb CLI structure (kubectl-style)"
echo "  ✅ 3 output formats (JSON, Text, Quiet)"
echo "  ✅ Environment management"
echo "  ✅ Swarm coordination (4 topologies)"
echo "  ✅ Agent spawning and orchestration"
echo "  ✅ Cross-language integration (Python, Node.js, Bash)"
echo "  ✅ Performance benchmarking"
echo "  ✅ Task orchestration (parallel, sequential, adaptive)"
echo "  ✅ Real-time status monitoring"
echo "  ✅ Metrics and performance tracking"
echo ""
echo -e "${CYAN}Usage from any programming language:${NC}"
echo "  Python:  subprocess.run(['cleanroom', 'status', '--output', 'json'])"
echo "  Node.js: execSync('cleanroom status --output json')"
echo "  Bash:    cleanroom status --output json | jq '.health'"
echo "  Go:      exec.Command(\"cleanroom\", \"status\", \"--output\", \"json\")"
echo "  Rust:    Command::new(\"cleanroom\").args(&[\"status\", \"--output\", \"json\"])"
echo ""
echo -e "${GREEN}Cleanroom CLI is production ready! 🚀${NC}"
echo ""
