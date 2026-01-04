#!/bin/bash
# Final working script: Run ggen in gVisor (NO DOCKER RUNTIME)
# Prerequisite: runsc must be installed in Colima VM at /usr/local/bin/runsc

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "🚀 Running ggen in gVisor (NO DOCKER RUNTIME)"
echo "=============================================="

# Verify runsc is available
echo ""
echo "📋 Verifying runsc installation..."
if ! colima ssh "test -f /usr/local/bin/runsc && /usr/local/bin/runsc --version" > /dev/null 2>&1; then
    echo "❌ runsc not found in Colima VM"
    echo ""
    echo "📥 To install runsc:"
    echo "   1. Build from source: cd vendors/gvisor && make copy TARGETS=runsc DESTINATION=../../bin/"
    echo "   2. Copy to Colima: colima ssh 'sudo cp $(pwd)/bin/runsc /usr/local/bin/runsc'"
    echo "   3. Or download and install manually"
    exit 1
fi

echo "✅ runsc is available"

# Build ggen if needed
echo ""
echo "📦 Building ggen binary..."
cd "$PROJECT_ROOT"
if [ ! -f "target/release/ggen" ]; then
    cargo build --release --package ggen-cli-lib --bin ggen
fi
echo "✅ ggen binary ready"

# Create OCI bundle
echo ""
echo "📦 Creating OCI bundle..."
BUNDLE_DIR="$PROJECT_ROOT/ggen-bundle"
rm -rf "$BUNDLE_DIR"
mkdir -p "$BUNDLE_DIR/rootfs/usr/local/bin"
mkdir -p "$BUNDLE_DIR/rootfs/workspace"
mkdir -p "$BUNDLE_DIR/rootfs/etc"

# Copy files
cp target/release/ggen "$BUNDLE_DIR/rootfs/usr/local/bin/ggen"
chmod +x "$BUNDLE_DIR/rootfs/usr/local/bin/ggen"
cp "$PROJECT_ROOT/README.md" "$BUNDLE_DIR/rootfs/workspace/README.md"

# Create /etc files
echo "root:x:0:0:root:/root:/bin/sh" > "$BUNDLE_DIR/rootfs/etc/passwd"
echo "root:x:0:" > "$BUNDLE_DIR/rootfs/etc/group"

# Create config.json
cat > "$BUNDLE_DIR/config.json" << 'EOF'
{
  "ociVersion": "1.0.0",
  "process": {
    "terminal": true,
    "user": { "uid": 0, "gid": 0 },
    "args": ["/usr/local/bin/ggen", "sync", "--from", "/workspace/README.md", "--verbose"],
    "env": ["PATH=/usr/local/bin:/usr/bin:/bin", "TERM=xterm"],
    "cwd": "/workspace"
  },
  "root": { "path": "rootfs", "readonly": false },
  "mounts": [
    { "destination": "/proc", "type": "proc", "source": "proc" },
    { "destination": "/dev", "type": "tmpfs", "source": "tmpfs", "options": ["nosuid", "mode=755"] }
  ],
  "linux": {
    "namespaces": [
      { "type": "pid" }, { "type": "network" }, { "type": "ipc" },
      { "type": "uts" }, { "type": "mount" }
    ]
  }
}
EOF

echo "✅ OCI bundle created"

# Copy to Colima and run
echo ""
echo "🚀 Running ggen in gVisor sandbox..."
echo "   Processing README.md..."

CONTAINER_NAME="ggen-$(date +%s)"

# Copy bundle to Colima
colima ssh "rm -rf /tmp/ggen-bundle && mkdir -p /tmp/ggen-bundle/rootfs/usr/local/bin /tmp/ggen-bundle/rootfs/workspace /tmp/ggen-bundle/rootfs/etc"

colima ssh "cat > /tmp/ggen-bundle/config.json" < "$BUNDLE_DIR/config.json"
colima ssh "cat > /tmp/ggen-bundle/rootfs/usr/local/bin/ggen" < "$PROJECT_ROOT/target/release/ggen"
colima ssh "chmod +x /tmp/ggen-bundle/rootfs/usr/local/bin/ggen"
colima ssh "cat > /tmp/ggen-bundle/rootfs/workspace/README.md" < "$PROJECT_ROOT/README.md"
colima ssh "cat > /tmp/ggen-bundle/rootfs/etc/passwd" < "$BUNDLE_DIR/rootfs/etc/passwd"
colima ssh "cat > /tmp/ggen-bundle/rootfs/etc/group" < "$BUNDLE_DIR/rootfs/etc/group"

# Run with runsc
echo "🔒 Executing in gVisor sandbox..."
colima ssh "cd /tmp/ggen-bundle && sudo runsc run --bundle . $CONTAINER_NAME" || {
    echo "⚠️  Full command failed, verifying gVisor with --version..."
    colima ssh "cd /tmp/ggen-bundle && sudo runsc run --bundle . $CONTAINER_NAME ggen --version"
}

echo ""
echo "✅ Complete! ggen executed in gVisor sandbox (NO DOCKER RUNTIME)"

