#!/bin/bash
# Configure containerd to use gVisor runsc runtime

set -euo pipefail

echo "🔧 Configuring containerd to use gVisor..."

# Detect environment
if [[ "$OSTYPE" == "darwin"* ]]; then
    USE_COLIMA=true
    CONTAINERD_CONFIG="/etc/containerd/config.toml"
else
    USE_COLIMA=false
    CONTAINERD_CONFIG="/etc/containerd/config.toml"
fi

if [ "$USE_COLIMA" = true ]; then
    echo "📋 Configuring containerd in Colima VM..."
    
    # Check if containerd config exists
    if ! colima ssh "test -f $CONTAINERD_CONFIG"; then
        echo "📝 Creating containerd config..."
        colima ssh "sudo mkdir -p /etc/containerd"
        colima ssh "sudo containerd config default | sudo tee $CONTAINERD_CONFIG"
    fi
    
    # Backup config
    colima ssh "sudo cp $CONTAINERD_CONFIG ${CONTAINERD_CONFIG}.backup.$(date +%Y%m%d_%H%M%S)"
    
    # Add runsc runtime to containerd config
    colima ssh << 'EOF'
CONFIG_FILE="/etc/containerd/config.toml"

# Check if runsc runtime already configured
if grep -q '\[plugins."io.containerd.grpc.v1.cri".containerd.runtimes.runsc\]' "$CONFIG_FILE"; then
    echo "✅ runsc runtime already configured"
else
    # Add runsc runtime configuration
    sudo tee -a "$CONFIG_FILE" > /dev/null << 'CONFIG'

[plugins."io.containerd.grpc.v1.cri".containerd.runtimes.runsc]
  runtime_type = "io.containerd.runsc.v1"
  [plugins."io.containerd.grpc.v1.cri".containerd.runtimes.runsc.options]
    TypeUrl = "io.containerd.runsc.v1.options"
CONFIG
    
    echo "✅ Added runsc runtime to containerd config"
fi
EOF
    
    # Restart containerd
    echo "🔄 Restarting containerd..."
    colima ssh "sudo systemctl restart containerd || sudo service containerd restart || echo 'Please restart containerd manually'"
    
else
    # Linux system configuration
    if [ ! -f "$CONTAINERD_CONFIG" ]; then
        echo "📝 Creating containerd config..."
        sudo mkdir -p /etc/containerd
        sudo containerd config default | sudo tee "$CONTAINERD_CONFIG" > /dev/null
    fi
    
    # Backup config
    sudo cp "$CONTAINERD_CONFIG" "${CONTAINERD_CONFIG}.backup.$(date +%Y%m%d_%H%M%S)"
    
    # Add runsc runtime
    if grep -q '\[plugins."io.containerd.grpc.v1.cri".containerd.runtimes.runsc\]' "$CONTAINERD_CONFIG"; then
        echo "✅ runsc runtime already configured"
    else
        sudo tee -a "$CONTAINERD_CONFIG" > /dev/null << 'CONFIG'

[plugins."io.containerd.grpc.v1.cri".containerd.runtimes.runsc]
  runtime_type = "io.containerd.runsc.v1"
  [plugins."io.containerd.grpc.v1.cri".containerd.runtimes.runsc.options]
    TypeUrl = "io.containerd.runsc.v1.options"
CONFIG
        echo "✅ Added runsc runtime to containerd config"
    fi
    
    # Restart containerd
    echo "🔄 Restarting containerd..."
    sudo systemctl restart containerd || sudo service containerd restart
fi

echo ""
echo "✅ containerd configured to use gVisor"
echo ""
echo "📋 Test with:"
echo "   ctr run --runtime io.containerd.runsc.v1 docker.io/library/alpine:latest test echo 'Hello from gVisor'"

