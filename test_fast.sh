#!/bin/bash
WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
avatars=("ProductManager" "DataScientist" "SecurityAuditor" "FrontendDev" "BackendDev" "DevOpsEngineer" "ComplianceOfficer" "SystemArchitect")
jtbds=("Analyze_Metrics" "Audit_Logs" "Design_Schema" "Implement_Cache" "Verify_Compliance" "Optimize_Frontend" "Deploy_K8s" "Setup_Alerts")

success_count=0
fail_count=0

for avatar in "${avatars[@]}"; do
    for jtbd in "${jtbds[@]}"; do
        title="${avatar}_${jtbd}"
        OUTPUT=$("$WORKSPACE_ROOT/target/debug/ggen" a2a create --title "$title" 2>/dev/null)
        ID=$(echo "$OUTPUT" | grep -o '"id":"[^"]*"' | cut -d'"' -f4)
        if [ ! -z "$ID" ]; then
            STATUS_OUT=$("$WORKSPACE_ROOT/target/debug/ggen" a2a status --id "$ID" 2>/dev/null)
            if echo "$STATUS_OUT" | grep -q "$ID"; then
                ((success_count++))
            else
                ((fail_count++))
            fi
        else
            ((fail_count++))
        fi
    done
done

echo "Fast E2E Validation Complete."
echo "Total tasks generated and verified: $success_count"
echo "Total failures: $fail_count"
