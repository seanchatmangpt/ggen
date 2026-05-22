#!/bin/bash

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

echo "Starting A2A and MCP End-to-End Test for 8 Avatars x 8 JTBDs"

# Ensure we're in the workspace
cd /Users/sac/ggen

# Let's test the MCP server in the background
echo "Starting MCP Server..."
cargo run --package ggen-cli-lib --bin ggen -- mcp server --transport stdio &
MCP_PID=$!
sleep 3

# Actuate using gemini-cli
for avatar in "${avatars[@]}"; do
    echo "--- Testing Avatar: $avatar ---"
    
    # We will batch the 8 JTBDs into one prompt to save time and tokens
    batch_prompt="As a $avatar, I need to create 8 A2A tasks for the following Jobs To Be Done: ${jtbds[*]}. For each JTBD, please autonomously execute the command 'cargo run --package ggen-cli-lib --bin ggen -- a2a create --title \"${avatar}_<JTBD>\"' to create it. Then extract the 'id' from the JSON output and run 'cargo run --package ggen-cli-lib --bin ggen -- a2a execute --id <ID>' followed by 'cargo run --package ggen-cli-lib --bin ggen -- a2a status --id <ID>'. Run this autonomously without asking for permission."
    
    echo "Actuating with npx @google/gemini-cli..."
    npx @google/gemini-cli "$batch_prompt"
done

echo "Cleaning up MCP Server..."
kill $MCP_PID

echo "End-to-End Test Complete."
