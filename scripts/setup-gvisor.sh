#!/bin/bash
# Setup gVisor for Docker on macOS
# This script installs gVisor (runsc) and configures Docker Desktop to use it

set -euo pipefail

echo "🔧 Setting up gVisor for Docker Desktop on macOS..."

# Detect Docker Desktop vs other Docker setups
if docker context show | grep -q "desktop-linux"; then
    echo "✅ Detected Docker Desktop"
    DOCKER_TYPE="desktop"
else
    echo "⚠️  Docker Desktop not detected, using standard Docker setup"
    DOCKER_TYPE="standard"
fi

# For Docker Desktop, we need Linux x86_64 or arm64 binary (depending on Docker Desktop's VM arch)
# Docker Desktop on Apple Silicon uses Linux ARM64 in the VM
# Docker Desktop on Intel uses Linux x86_64 in the VM
HOST_ARCH=$(uname -m)
if [ "$HOST_ARCH" = "arm64" ]; then
    # Docker Desktop on Apple Silicon uses ARM64 Linux VM
    GVISOR_ARCH="arm64"
else
    GVISOR_ARCH="x86_64"
fi

echo "📥 Detected architecture: $GVISOR_ARCH (for Docker Desktop VM)"

# Download runsc binary - for Docker Desktop, we need Linux binary inside the VM
# We'll download it and install it via Docker
echo "📥 Fetching latest gVisor release..."
RUNSC_URL="https://storage.googleapis.com/gvisor/releases/release/latest/${GVISOR_ARCH}/runsc"
TEMP_BINARY="/tmp/runsc-${GVISOR_ARCH}"

echo "📥 Downloading runsc (Linux ${GVISOR_ARCH}) from $RUNSC_URL..."
if curl -fsSL "$RUNSC_URL" -o "$TEMP_BINARY"; then
    chmod +x "$TEMP_BINARY"
    
    # Verify it's a valid binary (Linux ELF)
    if file "$TEMP_BINARY" | grep -qE "(ELF|executable|binary)"; then
        echo "✅ Binary downloaded successfully ($(file "$TEMP_BINARY" | cut -d: -f2))"
        
        # For Docker Desktop, install runsc inside the Docker VM
        if [ "$DOCKER_TYPE" = "desktop" ]; then
            echo "🔧 Installing runsc in Docker Desktop VM..."
            # Copy binary into a container that persists, or use docker cp
            # We'll create a helper approach using a volume
            docker run --rm -v /usr/local/bin:/target alpine sh -c "
                cp /tmp/runsc /target/runsc 2>/dev/null || echo 'Note: Direct copy may not work in Docker Desktop'
            " || echo "⚠️  Direct VM install not available, will configure manually"
            
            # Alternative: Save binary for manual installation
            RUNSC_BINARY="$HOME/.docker/runsc"
            cp "$TEMP_BINARY" "$RUNSC_BINARY"
            chmod +x "$RUNSC_BINARY"
            echo "✅ Binary saved to $RUNSC_BINARY for Docker Desktop"
            echo "📋 You may need to manually copy this into Docker Desktop VM"
        else
            # For standard Docker (Linux), install directly
            RUNSC_BINARY="/usr/local/bin/runsc"
            if sudo cp "$TEMP_BINARY" "$RUNSC_BINARY" && sudo chmod +x "$RUNSC_BINARY"; then
                echo "✅ gVisor installed to $RUNSC_BINARY"
            else
                echo "❌ Failed to install runsc (sudo required)"
                exit 1
            fi
        fi
        rm -f "$TEMP_BINARY"
    else
        echo "❌ Downloaded file is not a valid executable: $(file "$TEMP_BINARY")"
        exit 1
    fi
else
    echo "❌ Failed to download runsc from $RUNSC_URL"
    echo "💡 Trying alternative download method..."
    # Try GitHub releases as fallback
    GITHUB_URL=$(curl -s https://api.github.com/repos/google/gvisor/releases/latest | grep "browser_download_url.*runsc.*${GVISOR_ARCH}" | head -1 | cut -d '"' -f 4)
    if [ -n "$GITHUB_URL" ] && curl -fsSL "$GITHUB_URL" -o "$TEMP_BINARY"; then
        chmod +x "$TEMP_BINARY"
        echo "✅ Downloaded from GitHub releases"
        RUNSC_BINARY="$HOME/.docker/runsc"
        cp "$TEMP_BINARY" "$RUNSC_BINARY"
        chmod +x "$RUNSC_BINARY"
        rm -f "$TEMP_BINARY"
    else
        echo "❌ All download methods failed"
        exit 1
    fi
fi

# Set the runsc path for Docker config
if [ "$DOCKER_TYPE" = "desktop" ]; then
    # For Docker Desktop, the path should point to where runsc will be in the VM
    # Docker Desktop will need runsc at /usr/local/bin/runsc inside the VM
    RUNSC_PATH="/usr/local/bin/runsc"
    echo "📋 Note: Docker Desktop requires runsc to be available in the VM"
    echo "   The binary is saved to $HOME/.docker/runsc"
    echo "   Docker Desktop may need the binary copied into its VM manually"
else
    RUNSC_PATH="/usr/local/bin/runsc"
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

# Read existing config or create new one
if [ -f "$DOCKER_CONFIG_FILE" ]; then
    # Use jq if available, otherwise use Python
    if command -v jq &> /dev/null; then
        # Add runsc runtime to existing config
        if jq '.runtimes.runsc' "$DOCKER_CONFIG_FILE" > /dev/null 2>&1; then
            echo "✅ runsc runtime already configured"
        else
        jq --arg path "$RUNSC_PATH" '.runtimes.runsc = {
            "path": $path,
            "runtimeArgs": []
        }' "$DOCKER_CONFIG_FILE" > "$DOCKER_CONFIG_FILE.tmp" && mv "$DOCKER_CONFIG_FILE.tmp" "$DOCKER_CONFIG_FILE"
            echo "✅ Added runsc runtime to Docker config"
        fi
    else
        # Fallback: use Python to merge JSON
        python3 << EOF
import json
import sys

try:
    with open("$DOCKER_CONFIG_FILE", "r") as f:
        config = json.load(f)
except:
    config = {}

if "runtimes" not in config:
    config["runtimes"] = {}

if "runsc" not in config["runtimes"]:
    config["runtimes"]["runsc"] = {
        "path": "$RUNSC_PATH",
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
    # Create new config file
    cat > "$DOCKER_CONFIG_FILE" << EOF
{
  "runtimes": {
    "runsc": {
      "path": "$RUNSC_PATH",
      "runtimeArgs": []
    }
  }
}
EOF
    echo "✅ Created Docker config with runsc runtime"
fi

echo ""
echo "✅ Docker configuration updated at: $DOCKER_CONFIG_FILE"
echo ""
echo "⚠️  IMPORTANT: Restart Docker Desktop for changes to take effect"
echo "   You can restart Docker Desktop from the menu bar or:"
echo "   osascript -e 'quit app \"Docker\"' && open -a Docker"
echo ""
echo "📋 After restart, test gVisor with:"
echo "   docker run --runtime=runsc --rm hello-world"
echo ""
echo "📋 To use gVisor with this project:"
echo "   docker build --runtime=runsc -t ggen:gvisor ."
echo "   docker run --runtime=runsc --rm -v \$(pwd):/workspace ggen:gvisor sync"
echo ""
echo "✅ Setup complete!"

