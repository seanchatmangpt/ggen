#!/bin/bash
# Build ggen as OCI image using buildah/skopeo (no Docker required)
# This creates an OCI image that can be used with containerd + gVisor

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "🔨 Building ggen OCI image (no Docker required)..."

# Check for buildah (OCI image builder)
if ! command -v buildah &> /dev/null; then
    echo "📥 buildah not found - installing..."
    if [[ "$OSTYPE" == "darwin"* ]]; then
        brew install buildah
    else
        echo "Install buildah: sudo apt-get install buildah"
        exit 1
    fi
fi

# Build Rust binary first
echo "🔨 Building ggen binary..."
cd "$PROJECT_ROOT"
cargo build --release --package ggen-cli-lib --bin ggen

if [ ! -f "target/release/ggen" ]; then
    echo "❌ ggen binary not found"
    exit 1
fi

# Create container from scratch
echo "📦 Creating OCI image..."
CONTAINER=$(buildah from scratch)
MOUNTPOINT=$(buildah mount "$CONTAINER")

# Create minimal rootfs
echo "📁 Setting up rootfs..."
mkdir -p "$MOUNTPOINT/usr/local/bin"
mkdir -p "$MOUNTPOINT/workspace"
mkdir -p "$MOUNTPOINT/etc"

# Copy binary
cp target/release/ggen "$MOUNTPOINT/usr/local/bin/ggen"
chmod +x "$MOUNTPOINT/usr/local/bin/ggen"

# Create minimal /etc/passwd and /etc/group for root user
echo "root:x:0:0:root:/root:/bin/sh" > "$MOUNTPOINT/etc/passwd"
echo "root:x:0:" > "$MOUNTPOINT/etc/group"

# Configure container
buildah config --cmd '["ggen", "--help"]' "$CONTAINER"
buildah config --workingdir /workspace "$CONTAINER"
buildah config --entrypoint '["/usr/local/bin/ggen"]' "$CONTAINER"

# Commit image
IMAGE_NAME="ggen:latest"
buildah commit "$CONTAINER" "$IMAGE_NAME"

# Unmount
buildah unmount "$CONTAINER"

echo ""
echo "✅ OCI image created: $IMAGE_NAME"
echo ""
echo "📋 To use with containerd + gVisor:"
echo "   1. Import image: buildah push $IMAGE_NAME oci-archive:ggen.tar"
echo "   2. Import to containerd: ctr images import ggen.tar"
echo "   3. Run with gVisor: ctr run --runtime io.containerd.runsc.v1 ggen:latest ggen-test ggen sync"

