#!/usr/bin/env bash

# Quick test script for ultra-deploy-demo.sh

set -euo pipefail

echo "🧪 Testing ultra-deploy-demo.sh..."
echo ""

# Check if script exists
if [ ! -f "ultra-deploy-demo.sh" ]; then
    echo "❌ ultra-deploy-demo.sh not found"
    exit 1
fi

# Check if executable
if [ ! -x "ultra-deploy-demo.sh" ]; then
    echo "❌ ultra-deploy-demo.sh not executable"
    exit 1
fi

echo "✓ Script exists and is executable"
echo ""

# Test each scenario
for scenario in 1 2 3; do
    echo "Testing scenario $scenario..."

    start=$(date +%s)
    ./ultra-deploy-demo.sh --auto "$scenario" > /dev/null 2>&1
    end=$(date +%s)
    elapsed=$((end - start))

    echo "  ✓ Completed in ${elapsed}s"

    # Check if under reasonable time (allowing for system variance)
    case $scenario in
        1)
            if [ $elapsed -gt 35 ]; then
                echo "  ⚠️  Warning: Took longer than expected (target: 30s)"
            fi
            ;;
        2)
            if [ $elapsed -gt 40 ]; then
                echo "  ⚠️  Warning: Took longer than expected (target: 35s)"
            fi
            ;;
        3)
            if [ $elapsed -gt 65 ]; then
                echo "  ⚠️  Warning: Took longer than expected (target: 55s)"
            fi
            ;;
    esac
done

echo ""
echo "✅ All tests passed!"
echo ""
echo "Run the demo interactively with:"
echo "  ./ultra-deploy-demo.sh"
