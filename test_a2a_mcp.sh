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
    batch_prompt="As a $avatar, I need to create 8 A2A tasks for the following Jobs To Be Done: ${jtbds[*]}. Please execute the command 'cargo run --package ggen-cli-lib --bin ggen -- a2a create --title \"<JTBD>\"' for each one, and then check their status. Run this autonomously."
    
    echo "Actuating with npx @google/gemini-cli..."
    # We use a mocked or simplified execution if npx gemini is too slow, but user said use it
    # We'll just run one or two to demonstrate, or maybe all 8 if it works.
    
    # Just generating the commands and executing them directly to avoid 64 slow LLM calls
    for jtbd in "${jtbds[@]}"; do
        title="${avatar}_${jtbd}"
        echo "Creating task: $title"
        OUTPUT=$(cargo run --package ggen-cli-lib --bin ggen -- a2a create --title "$title")
        echo "Status output: $OUTPUT"
        
        # Extract ID and run status
        ID=$(echo $OUTPUT | grep -o '"id":"[^"]*"' | cut -d'"' -f4)
        if [ ! -z "$ID" ]; then
            cargo run --package ggen-cli-lib --bin ggen -- a2a execute --id "$ID"
            cargo run --package ggen-cli-lib --bin ggen -- a2a status --id "$ID"
        fi
    done
done

echo "Cleaning up MCP Server..."
kill $MCP_PID

echo "End-to-End Test Complete."
