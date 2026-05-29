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
    
    # We ask the model to just generate the commands for 2 JTBDs per run to lower token usage
    # and use a 10s delay between avatars to avoid hitting rate limits.
    prompt="You are acting as the $avatar persona. I need you to create A2A tasks for the following Jobs To Be Done (JTBDs): ${jtbds[0]}, ${jtbds[1]}, ${jtbds[2]}, ${jtbds[3]}, ${jtbds[4]}, ${jtbds[5]}, ${jtbds[6]}, ${jtbds[7]}. For each JTBD, run the command \`cargo run -p ggen-cli-lib --bin ggen -- a2a create --title \"<Avatar>_<JTBD>\"\`. Then extract the ID from the output and run \`cargo run -p ggen-cli-lib --bin ggen -- a2a status --id \"<ID>\"\` to confirm it. Please run these sequentially in your terminal."
    
    echo "Prompting Gemini CLI with gemini-3.1-flash-lite-preview..."
    npx -y @google/gemini-cli -y --model gemini-3.1-flash-lite-preview -p "$prompt"
    
    echo "Sleeping for 1 seconds to respect rate limits..."
    sleep 1
done

echo "Cleaning up..."
kill $MCP_PID
