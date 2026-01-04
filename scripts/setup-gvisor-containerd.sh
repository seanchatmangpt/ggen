#!/bin/bash
# Setup gVisor with containerd (without Docker)
# This allows using gVisor directly without Docker dependency

set -euo pipefail

echo "🔧 Setting up gVisor with containerd (no Docker required)..."

# Check if we're on macOS (Colima) or Linux
if [[ "$OSTYPE" == "darwin"* ]]; then
    echo "📋 macOS detected - using Colima with containerd"
    USE_COLIMA=true
else
    echo "📋 Linux detected - using system containerd"
    USE_COLIMA=false
fi

# Build runsc from vendored source
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "🔨 Building runsc from vendored source..."
if [ -f "$PROJECT_ROOT/scripts/build-gvisor-runsc.sh" ]; then
    "$PROJECT_ROOT/scripts/build-gvisor-runsc.sh"
else
    echo "❌ Build script not found"
    exit 1
fi

if [ ! -f "$PROJECT_ROOT/bin/runsc" ]; then
    echo "❌ runsc binary not found after build"
    exit 1
fi

# Install runsc
echo "📥 Installing runsc..."
RUNSC_BINARY="$PROJECT_ROOT/bin/runsc"

if [ "$USE_COLIMA" = true ]; then
    # Install in Colima VM
    echo "📥 Installing runsc in Colima VM..."
    colima ssh "sudo cp $(pwd)/bin/runsc /usr/local/bin/runsc && sudo chmod +x /usr/local/bin/runsc"
    
    # Install runsc for containerd
    echo "🔧 Installing runsc for containerd..."
    colima ssh "sudo /usr/local/bin/runsc install --runtime=runsc"
    
    # Check if containerd is available
    if colima ssh "command -v containerd" > /dev/null 2>&1; then
        echo "✅ containerd is available in Colima"
    else
        echo "⚠️  containerd not found - Colima may need to be started with containerd"
        echo "   Try: colima start --runtime containerd"
    fi
else
    # Linux system installation
    sudo cp "$RUNSC_BINARY" /usr/local/bin/runsc
    sudo chmod +x /usr/local/bin/runsc
    
    # Install runsc for containerd
    sudo /usr/local/bin/runsc install --runtime=runsc
    
    # Check containerd
    if command -v containerd > /dev/null 2>&1; then
        echo "✅ containerd is available"
    else
        echo "⚠️  containerd not found - install with:"
        echo "   sudo apt-get install containerd"
    fi
fi

echo ""
echo "✅ gVisor (runsc) installed for containerd"
echo ""
echo "📋 Next steps:"
echo "   1. Configure containerd to use runsc (see scripts/configure-containerd-gvisor.sh)"
echo "   2. Build ggen OCI image: ./scripts/build-ggen-oci.sh"
echo "   3. Run ggen with gVisor: ./scripts/run-ggen-gvisor.sh"

