#!/bin/bash
# Run ggen using containerd + gVisor (no Docker)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "🚀 Running ggen with gVisor (containerd, no Docker)..."

# Check if containerd is available
if [[ "$OSTYPE" == "darwin"* ]]; then
    if ! colima ssh "command -v ctr" > /dev/null 2>&1; then
        echo "❌ ctr (containerd CLI) not found in Colima"
        echo "   Start Colima with containerd: colima start --runtime containerd"
        exit 1
    fi
    CTR_CMD="colima ssh ctr"
else
    if ! command -v ctr &> /dev/null; then
        echo "❌ ctr (containerd CLI) not found"
        echo "   Install: sudo apt-get install containerd"
        exit 1
    fi
    CTR_CMD="sudo ctr"
fi

# Check if ggen image exists
if ! $CTR_CMD images ls | grep -q "ggen:latest"; then
    echo "📦 ggen image not found - building..."
    "$PROJECT_ROOT/scripts/build-ggen-oci.sh"
    
    # Import image to containerd
    echo "📥 Importing image to containerd..."
    if [ -f "$PROJECT_ROOT/ggen.tar" ]; then
        $CTR_CMD images import "$PROJECT_ROOT/ggen.tar"
    else
        echo "❌ Image tar not found"
        exit 1
    fi
fi

# Run ggen with gVisor
CONTAINER_NAME="ggen-$(date +%s)"
WORKSPACE_DIR="${1:-$(pwd)}"

echo "🚀 Running ggen in gVisor sandbox..."
echo "   Workspace: $WORKSPACE_DIR"
echo "   Container: $CONTAINER_NAME"

# Mount workspace
if [[ "$OSTYPE" == "darwin"* ]]; then
    # Colima: need to mount via SSH
    echo "⚠️  Colima volume mounting requires manual setup"
    echo "   Consider using: colima ssh 'ctr run --mount type=bind,src=$WORKSPACE_DIR,dst=/workspace,options=rbind:ro --runtime io.containerd.runsc.v1 ggen:latest $CONTAINER_NAME ggen sync'"
    $CTR_CMD run --rm --runtime io.containerd.runsc.v1 ggen:latest "$CONTAINER_NAME" ggen "${@:2}"
else
    # Linux: direct mount
    $CTR_CMD run --rm \
        --mount "type=bind,src=$WORKSPACE_DIR,dst=/workspace,options=rbind:rw" \
        --runtime io.containerd.runsc.v1 \
        ggen:latest \
        "$CONTAINER_NAME" \
        ggen "${@:2}"
fi

echo ""
echo "✅ ggen completed in gVisor sandbox"

