#!/bin/bash
# Setup gVisor with Colima - Much easier than Docker Desktop!
# Colima provides a Linux VM where we can directly install gVisor

set -euo pipefail

echo "🔧 Setting up gVisor with Colima..."

# Check if Colima is installed
if ! command -v colima &> /dev/null; then
    echo "❌ Colima is not installed"
    echo "📥 Install with: brew install colima"
    exit 1
fi

# Check if Colima is running
if ! colima status > /dev/null 2>&1; then
    echo "🚀 Starting Colima..."
    colima start
else
    echo "✅ Colima is running"
fi

# Detect architecture
ARCH=$(uname -m)
if [ "$ARCH" = "arm64" ]; then
    GVISOR_ARCH="arm64"
else
    GVISOR_ARCH="x86_64"
fi

echo "📥 Detected architecture: $GVISOR_ARCH"

# Install gVisor inside Colima VM
echo "📥 Installing gVisor (runsc) in Colima VM..."

# Download and install runsc inside the Colima VM
colima ssh << 'EOF'
# Detect architecture inside VM
ARCH=$(uname -m)
if [ "$ARCH" = "aarch64" ]; then
    GVISOR_ARCH="arm64"
else
    GVISOR_ARCH="x86_64"
fi

echo "Installing gVisor for architecture: $GVISOR_ARCH"

# Download runsc
RUNSC_URL="https://storage.googleapis.com/gvisor/releases/release/latest/${GVISOR_ARCH}/runsc"
echo "Downloading from: $RUNSC_URL"

if curl -fsSL "$RUNSC_URL" -o /tmp/runsc; then
    chmod +x /tmp/runsc
    sudo mv /tmp/runsc /usr/local/bin/runsc
    
    # Verify installation
    if /usr/local/bin/runsc --version; then
        echo "✅ gVisor installed successfully"
    else
        echo "❌ gVisor installation verification failed"
        exit 1
    fi
else
    echo "❌ Failed to download runsc"
    echo "Trying alternative method..."
    
    # Try GitHub releases as fallback
    GITHUB_URL=$(curl -s https://api.github.com/repos/google/gvisor/releases/latest | grep "browser_download_url.*runsc.*${GVISOR_ARCH}" | head -1 | cut -d '"' -f 4)
    if [ -n "$GITHUB_URL" ] && curl -fsSL "$GITHUB_URL" -o /tmp/runsc; then
        chmod +x /tmp/runsc
        sudo mv /tmp/runsc /usr/local/bin/runsc
        /usr/local/bin/runsc --version && echo "✅ gVisor installed from GitHub releases"
    else
        echo "❌ All download methods failed"
        exit 1
    fi
fi

# Install runsc as Docker runtime
echo "🔧 Installing runsc as Docker runtime..."
sudo /usr/local/bin/runsc install || echo "⚠️  runsc install failed, will configure manually"

# Configure Docker daemon
echo "🔧 Configuring Docker daemon..."
DOCKER_CONFIG="/etc/docker/daemon.json"

# Backup existing config
if [ -f "$DOCKER_CONFIG" ]; then
    sudo cp "$DOCKER_CONFIG" "${DOCKER_CONFIG}.backup.$(date +%Y%m%d_%H%M%S)"
fi

# Create or update Docker config
sudo mkdir -p /etc/docker

# Use Python to merge JSON (jq may not be available)
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
echo "🔄 Restarting Docker daemon..."
sudo systemctl restart docker || sudo service docker restart || echo "⚠️  Please restart Docker manually"

echo "✅ gVisor setup complete in Colima VM!"
EOF

# Switch Docker context to Colima
echo "🔧 Switching Docker context to Colima..."
docker context use colima

# Verify gVisor is available
echo "✅ Verifying gVisor installation..."
if docker run --runtime=runsc --rm alpine echo "gVisor test" 2>&1 | grep -q "gVisor test"; then
    echo "✅ gVisor is working!"
else
    echo "⚠️  gVisor test failed, but installation may still be in progress"
    echo "   Try: docker run --runtime=runsc --rm hello-world"
fi

echo ""
echo "✅ Setup complete!"
echo ""
echo "📋 Test gVisor with:"
echo "   docker run --runtime=runsc --rm hello-world"
echo ""
echo "📋 Build and run ggen with gVisor:"
echo "   docker build -f Dockerfile.gvisor -t ggen:gvisor ."
echo "   docker run --runtime=runsc --rm -v \$(pwd):/workspace ggen:gvisor sync"
echo ""
echo "📋 Or use docker-compose:"
echo "   docker-compose -f docker-compose.gvisor.yml up"

