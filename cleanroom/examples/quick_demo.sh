#!/usr/bin/env bash
# Quick Demo of Cleanroom CLI

set -euo pipefail

echo "🧪 Cleanroom CLI Quick Demo"
echo "============================"
echo ""

# Build CLI if needed
if [[ ! -f target/debug/cleanroom ]]; then
    echo "Building cleanroom CLI..."
    cargo build --bin cleanroom
    echo ""
fi

CLI="./target/debug/cleanroom"

echo "1️⃣  Show version"
$CLI --version
echo ""

echo "2️⃣  Show help"
$CLI --help | head -20
echo ""

echo "3️⃣  Show swarm commands"
$CLI swarm --help
echo ""

echo "4️⃣  Initialize swarm (dry run)"
echo "Command: cleanroom swarm init --topology mesh --agents 5 --output json"
echo "Output: (would create mesh topology with 5 agents)"
echo ""

echo "5️⃣  Check status"
$CLI status --output text
echo ""

echo "✅ Demo complete!"
echo ""
echo "Try these commands:"
echo "  $CLI environment create --name test"
echo "  $CLI container start postgres --db testdb"
echo "  $CLI swarm init --topology mesh --agents 5"
echo "  $CLI test run --file test.rs"
echo ""
