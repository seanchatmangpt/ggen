#!/bin/bash
# Build gVisor runsc from the vendored source
# This builds runsc from the git submodule in vendors/gvisor

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
GVISOR_DIR="$PROJECT_ROOT/vendors/gvisor"

echo "🔧 Building gVisor runsc from vendored source..."

# Check if gVisor submodule exists
if [ ! -d "$GVISOR_DIR" ]; then
    echo "❌ gVisor submodule not found at $GVISOR_DIR"
    echo "📥 Initializing submodules..."
    cd "$PROJECT_ROOT"
    git submodule update --init --recursive
fi

cd "$GVISOR_DIR"

# Check if we're in a git repository
if [ ! -d ".git" ]; then
    echo "⚠️  Not a git repository, checking if submodule needs update..."
    cd "$PROJECT_ROOT"
    git submodule update --init --recursive vendors/gvisor
    cd "$GVISOR_DIR"
fi

echo "📋 Building runsc..."
echo "   Location: $GVISOR_DIR"

# Check if Docker is available (gVisor uses Docker for building)
if ! command -v docker &> /dev/null; then
    echo "❌ Docker is required to build gVisor"
    echo "   Install Docker or use Colima"
    exit 1
fi

# Build using gVisor's Makefile (uses Docker)
if [ -f "Makefile" ]; then
    echo "🔨 Building runsc using Makefile..."
    mkdir -p "$PROJECT_ROOT/bin"
    
    # Build runsc
    make copy TARGETS=runsc DESTINATION="$PROJECT_ROOT/bin/"
    
    if [ -f "$PROJECT_ROOT/bin/runsc" ]; then
        chmod +x "$PROJECT_ROOT/bin/runsc"
        echo "✅ runsc built successfully at: $PROJECT_ROOT/bin/runsc"
        "$PROJECT_ROOT/bin/runsc" --version
    else
        echo "❌ Build failed - runsc binary not found"
        exit 1
    fi
else
    echo "❌ Makefile not found in gVisor directory"
    exit 1
fi

echo ""
echo "✅ Build complete!"
echo "📋 To install system-wide:"
echo "   sudo cp $PROJECT_ROOT/bin/runsc /usr/local/bin/runsc"
echo "   sudo /usr/local/bin/runsc install"

