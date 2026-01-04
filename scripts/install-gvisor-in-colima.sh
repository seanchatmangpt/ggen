#!/bin/bash
# Install gVisor in Colima VM - standalone script
# This can be run inside the Colima VM via: colima ssh < this_script.sh

set -euo pipefail

# Detect architecture
ARCH=$(uname -m)
if [ "$ARCH" = "aarch64" ]; then
    GVISOR_ARCH="arm64"
    GVISOR_SUFFIX="arm64"
else
    GVISOR_ARCH="x86_64"
    GVISOR_SUFFIX="amd64"
fi

echo "Installing gVisor for architecture: $GVISOR_ARCH"

# Try multiple download methods
echo "Attempting to download runsc..."

# Method 1: Try GitHub releases API
GITHUB_RELEASE=$(curl -s https://api.github.com/repos/google/gvisor/releases/latest)
TAG_NAME=$(echo "$GITHUB_RELEASE" | grep '"tag_name"' | sed 's/.*"tag_name": "\([^"]*\)".*/\1/')
DOWNLOAD_URL=$(echo "$GITHUB_RELEASE" | grep "browser_download_url.*runsc.*${GVISOR_SUFFIX}" | head -1 | sed 's/.*"browser_download_url": "\([^"]*\)".*/\1/')

if [ -n "$DOWNLOAD_URL" ]; then
    echo "Found release: $TAG_NAME"
    echo "Downloading from: $DOWNLOAD_URL"
    if curl -fsSL "$DOWNLOAD_URL" -o /tmp/runsc; then
        chmod +x /tmp/runsc
        sudo mv /tmp/runsc /usr/local/bin/runsc
        if /usr/local/bin/runsc --version; then
            echo "✅ gVisor installed successfully from GitHub"
            INSTALLED=true
        fi
    fi
fi

# Method 2: Try direct storage URL with known version format
if [ "${INSTALLED:-false}" != "true" ]; then
    echo "Trying alternative download method..."
    # Try with the tag name we found
    if [ -n "$TAG_NAME" ]; then
        STORAGE_URL="https://storage.googleapis.com/gvisor/releases/release/${TAG_NAME}/${GVISOR_ARCH}/runsc"
        echo "Trying: $STORAGE_URL"
        if curl -fsSL "$STORAGE_URL" -o /tmp/runsc; then
            chmod +x /tmp/runsc
            sudo mv /tmp/runsc /usr/local/bin/runsc
            if /usr/local/bin/runsc --version; then
                echo "✅ gVisor installed successfully from storage"
                INSTALLED=true
            fi
        fi
    fi
fi

# Method 3: Try building from source (last resort)
if [ "${INSTALLED:-false}" != "true" ]; then
    echo "⚠️  Direct download failed. You may need to:"
    echo "   1. Manually download runsc from: https://github.com/google/gvisor/releases"
    echo "   2. Copy it into Colima VM: colima ssh 'sudo cp /path/to/runsc /usr/local/bin/runsc'"
    exit 1
fi

# Install runsc as Docker runtime
echo "Installing runsc as Docker runtime..."
sudo /usr/local/bin/runsc install || echo "⚠️  runsc install command failed, will configure manually"

# Configure Docker daemon
echo "Configuring Docker daemon..."
DOCKER_CONFIG="/etc/docker/daemon.json"

# Backup existing config
if [ -f "$DOCKER_CONFIG" ]; then
    sudo cp "$DOCKER_CONFIG" "${DOCKER_CONFIG}.backup.$(date +%Y%m%d_%H%M%S)"
fi

# Create or update Docker config
sudo mkdir -p /etc/docker

# Use Python to merge JSON
python3 << PYEOF
import json
import os

config_file = "$DOCKER_CONFIG"

# Read existing config
try:
    with open(config_file, 'r') as f:
        config = json.load(f)
except:
    config = {}

# Add runsc runtime
if "runtimes" not in config:
    config["runtimes"] = {}

config["runtimes"]["runsc"] = {
    "path": "/usr/local/bin/runsc",
    "runtimeArgs": []
}

# Write config
with open(config_file, 'w') as f:
    json.dump(config, f, indent=2)

print("✅ Docker daemon configured")
PYEOF

# Restart Docker
echo "Restarting Docker daemon..."
sudo systemctl restart docker || sudo service docker restart || echo "⚠️  Please restart Docker manually: sudo systemctl restart docker"

echo "✅ gVisor setup complete!"

