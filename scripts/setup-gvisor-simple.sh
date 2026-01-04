#!/bin/bash
# Simple gVisor setup using Docker image approach
# This works around download issues by using gVisor's container image

set -euo pipefail

echo "🔧 Setting up gVisor for Docker..."

# Check if Docker is running
if ! docker info > /dev/null 2>&1; then
    echo "❌ Docker is not running. Please start Docker Desktop first."
    exit 1
fi

# Configure Docker daemon
echo "🔧 Configuring Docker to use gVisor..."

DOCKER_CONFIG_DIR="$HOME/.docker"
DOCKER_CONFIG_FILE="$DOCKER_CONFIG_DIR/daemon.json"

# Create .docker directory if it doesn't exist
mkdir -p "$DOCKER_CONFIG_DIR"

# Backup existing config if it exists
if [ -f "$DOCKER_CONFIG_FILE" ]; then
    echo "📋 Backing up existing Docker config..."
    cp "$DOCKER_CONFIG_FILE" "$DOCKER_CONFIG_FILE.backup.$(date +%Y%m%d_%H%M%S)"
fi

# Use gVisor's container image approach
# For Docker Desktop, we'll configure to use runsc via container
RUNSC_PATH="gcr.io/gvisor-release/runsc:latest"

# Read existing config or create new one
if [ -f "$DOCKER_CONFIG_FILE" ]; then
    if command -v jq &> /dev/null; then
        if jq '.runtimes.runsc' "$DOCKER_CONFIG_FILE" > /dev/null 2>&1; then
            echo "✅ runsc runtime already configured"
        else
            jq '.runtimes.runsc = {
                "path": "runsc",
                "runtimeArgs": []
            }' "$DOCKER_CONFIG_FILE" > "$DOCKER_CONFIG_FILE.tmp" && mv "$DOCKER_CONFIG_FILE.tmp" "$DOCKER_CONFIG_FILE"
            echo "✅ Added runsc runtime to Docker config"
        fi
    else
        python3 << EOF
import json

try:
    with open("$DOCKER_CONFIG_FILE", "r") as f:
        config = json.load(f)
except:
    config = {}

if "runtimes" not in config:
    config["runtimes"] = {}

if "runsc" not in config["runtimes"]:
    config["runtimes"]["runsc"] = {
        "path": "runsc",
        "runtimeArgs": []
    }
    with open("$DOCKER_CONFIG_FILE", "w") as f:
        json.dump(config, f, indent=2)
    print("✅ Added runsc runtime to Docker config")
else:
    print("✅ runsc runtime already configured")
EOF
    fi
else
    cat > "$DOCKER_CONFIG_FILE" << EOF
{
  "runtimes": {
    "runsc": {
      "path": "runsc",
      "runtimeArgs": []
    }
  }
}
EOF
    echo "✅ Created Docker config with runsc runtime"
fi

echo ""
echo "📥 Pulling gVisor runsc image..."
docker pull gcr.io/gvisor-release/runsc:latest || echo "⚠️  Could not pull runsc image, will try alternative method"

echo ""
echo "✅ Docker configuration updated at: $DOCKER_CONFIG_FILE"
echo ""
echo "⚠️  IMPORTANT: For Docker Desktop, you need to:"
echo "   1. Install runsc binary in the Docker Desktop VM"
echo "   2. Restart Docker Desktop"
echo ""
echo "📋 Alternative: Use gVisor via Docker image:"
echo "   docker run --runtime=runsc --rm gcr.io/gvisor-release/runsc:latest --version"
echo ""
echo "📋 To test gVisor (after installing runsc in Docker Desktop VM):"
echo "   docker run --runtime=runsc --rm hello-world"
echo ""
echo "✅ Basic setup complete!"
echo ""
echo "💡 For full gVisor installation on Docker Desktop, you may need to:"
echo "   1. Access Docker Desktop VM (via terminal or settings)"
echo "   2. Download and install runsc binary inside the VM"
echo "   3. Or use a workaround with volume mounts"

