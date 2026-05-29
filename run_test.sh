#!/bin/bash
export GGEN_PATH="/Users/sac/ggen"

avatars=("ProductManager" "DataScientist" "SecurityAuditor" "FrontendDev" "BackendDev" "DevOpsEngineer" "ComplianceOfficer" "SystemArchitect")
jtbds=(
    "Analyze_Metrics"
    "Audit_Logs"
    "Design_Schema"
    "Implement_Cache"
    "Verify_Compliance"
    "Optimize_Frontend"
    "Deploy_K8s"
    "Setup_Alerts"
)

# Start MCP Server in the background
echo "Starting MCP Server..."
cargo run -p ggen-cli-lib --bin ggen -- mcp server --transport stdio &
MCP_PID=$!
sleep 5

for avatar in "${avatars[@]}"; do
    echo "--- Actuating Avatar: $avatar ---"
    
    # Constructing a batch prompt for the avatar's 8 JTBDs
    prompt="You are acting as the $avatar persona. I need you to create A2A tasks for the following 8 Jobs To Be Done (JTBDs): ${jtbds[*]}. For each JTBD, run the command \`cargo run -p ggen-cli-lib --bin ggen -- a2a create --title \"<Avatar>_<JTBD>\"\`. Then extract the ID from the output and run \`cargo run -p ggen-cli-lib --bin ggen -- a2a status --id \"<ID>\"\` to confirm it. Please run these sequentially in your terminal."
    
    echo "Prompting Gemini CLI..."
    npx -y @google/gemini-cli -y --model gemini-3.1-flash-lite-preview -p "$prompt"
done

echo "Cleaning up..."
kill $MCP_PID
