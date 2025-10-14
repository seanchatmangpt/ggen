#!/usr/bin/env bash
# Cleanroom CLI - Quick Reference Guide
# Fast demonstration of key commands

set -euo pipefail

CLI="../../target/debug/cleanroom"

if [[ ! -f "$CLI" ]]; then
    echo "❌ CLI not found. Build with: cargo build --bin cleanroom"
    exit 1
fi

echo "🧪 Cleanroom CLI - Quick Reference"
echo "====================================="
echo ""

echo "📌 Version & Status"
echo "-------------------"
$CLI --version
$CLI status --output json | jq -c '{system, version, health}'
echo ""

echo "📌 Swarm Commands"
echo "-----------------"
echo "Initialize swarm:"
SWARM=$($CLI swarm init --topology mesh --agents 5 --output json)
echo "$SWARM" | jq -c '{swarm_id, topology, agents: .max_agents}'
echo ""

echo "Spawn agent:"
$CLI swarm spawn --agent-type coder --name quick-agent --output json | \
    jq -c '{agent_id, name, type, status}'
echo ""

echo "List agents:"
$CLI swarm list --output json | jq -c '{count, agents: [.agents[] | .name]}'
echo ""

echo "Get status:"
$CLI swarm status --output json | jq -c '{agents: .agents, health}'
echo ""

echo "📌 Cross-Language Usage"
echo "-----------------------"

echo "Python:"
python3 -c "import subprocess, json; r=subprocess.run(['$CLI','status','--output','json'],capture_output=True,text=True); print('  ✅', json.loads(r.stdout)['system'], json.loads(r.stdout)['version'])"

echo "Node.js:"
node -e "const {execSync}=require('child_process'); const r=execSync('$CLI status --output json',{encoding:'utf8'}); const d=JSON.parse(r); console.log('  ✅', d.system, d.version);"

echo "Bash:"
echo "  ✅ $(  $CLI status --output json | jq -r '"\(.system) \(.version)"')"
echo ""

echo "📌 Output Formats"
echo "-----------------"
echo "JSON:  $CLI status --output json"
echo "Text:  $CLI status --output text"
echo "Quiet: $CLI status --output quiet"
echo ""

echo "✅ All key features demonstrated!"
echo ""
echo "For full documentation, see:"
echo "  - docs/CLEANROOM_CLI_COMPLETE.md"
echo "  - docs/swarm-cli-guide.md"
echo "  - examples/full_demo.sh (comprehensive)"
echo ""
