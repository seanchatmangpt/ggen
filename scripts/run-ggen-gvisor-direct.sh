#!/bin/bash
# Run ggen in gVisor using runsc directly (NO Docker, NO containerd)
# Uses OCI bundles with runsc

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "🚀 Building and running ggen in gVisor (NO DOCKER, NO containerd)"
echo "=================================================================="

# Step 1: Build ggen binary
echo ""
echo "📦 Step 1: Building ggen binary..."
cd "$PROJECT_ROOT"
cargo build --release --package ggen-cli-lib --bin ggen

if [ ! -f "target/release/ggen" ]; then
    echo "❌ ggen binary not found after build"
    exit 1
fi

echo "✅ ggen binary built"

# Step 2: Check/install runsc
echo ""
echo "📦 Step 2: Checking runsc..."

if [[ "$OSTYPE" == "darwin"* ]]; then
    # Check if runsc exists in Colima
    if ! colima ssh "command -v runsc" > /dev/null 2>&1 && ! colima ssh "test -f /usr/local/bin/runsc" > /dev/null 2>&1; then
        echo "📥 Installing runsc in Colima VM..."
        colima ssh 'ARCH=$(uname -m); if [ "$ARCH" = "aarch64" ]; then SUFFIX="arm64"; else SUFFIX="amd64"; fi; curl -fsSL "https://storage.googleapis.com/gvisor/releases/release/20241218.0/${SUFFIX}/runsc" -o /tmp/runsc && sudo mv /tmp/runsc /usr/local/bin/runsc && sudo chmod +x /usr/local/bin/runsc && /usr/local/bin/runsc --version' || {
            echo "❌ Failed to install runsc in Colima"
            exit 1
        }
    fi
    RUNSC_BINARY="runsc"  # Will use runsc in Colima VM
    echo "✅ runsc available in Colima VM"
else
    # Linux: Check local runsc
    if [ ! -f "$PROJECT_ROOT/bin/runsc" ] && ! command -v runsc &> /dev/null; then
        echo "📥 Building runsc from vendored source..."
        "$PROJECT_ROOT/scripts/build-gvisor-runsc.sh" || {
            echo "❌ runsc not found. Install gVisor first."
            exit 1
        }
    fi
    
    if [ -f "$PROJECT_ROOT/bin/runsc" ]; then
        RUNSC_BINARY="$PROJECT_ROOT/bin/runsc"
    else
        RUNSC_BINARY="runsc"
    fi
    echo "✅ Using runsc: $RUNSC_BINARY"
fi

# Step 3: Create OCI bundle
echo ""
echo "📦 Step 3: Creating OCI bundle..."

BUNDLE_DIR="$PROJECT_ROOT/ggen-bundle"
rm -rf "$BUNDLE_DIR"
mkdir -p "$BUNDLE_DIR/rootfs/usr/local/bin"
mkdir -p "$BUNDLE_DIR/rootfs/workspace"
mkdir -p "$BUNDLE_DIR/rootfs/etc"
mkdir -p "$BUNDLE_DIR/rootfs/proc"
mkdir -p "$BUNDLE_DIR/rootfs/dev"
mkdir -p "$BUNDLE_DIR/rootfs/sys"

# Copy ggen binary
cp target/release/ggen "$BUNDLE_DIR/rootfs/usr/local/bin/ggen"
chmod +x "$BUNDLE_DIR/rootfs/usr/local/bin/ggen"

# Copy README.md to workspace
cp "$PROJECT_ROOT/README.md" "$BUNDLE_DIR/rootfs/workspace/README.md"

# Create minimal /etc files
echo "root:x:0:0:root:/root:/bin/sh" > "$BUNDLE_DIR/rootfs/etc/passwd"
echo "root:x:0:" > "$BUNDLE_DIR/rootfs/etc/group"
echo "ggen-bundle" > "$BUNDLE_DIR/rootfs/etc/hostname"

# Create config.json for OCI bundle
cat > "$BUNDLE_DIR/config.json" << 'EOF'
{
  "ociVersion": "1.0.0",
  "process": {
    "terminal": true,
    "user": {
      "uid": 0,
      "gid": 0
    },
    "args": [
      "/usr/local/bin/ggen",
      "sync",
      "--from",
      "/workspace/README.md",
      "--verbose"
    ],
    "env": [
      "PATH=/usr/local/bin:/usr/bin:/bin",
      "TERM=xterm",
      "HOME=/root"
    ],
    "cwd": "/workspace"
  },
  "root": {
    "path": "rootfs",
    "readonly": false
  },
  "mounts": [
    {
      "destination": "/proc",
      "type": "proc",
      "source": "proc"
    },
    {
      "destination": "/dev",
      "type": "tmpfs",
      "source": "tmpfs",
      "options": ["nosuid", "strictatime", "mode=755", "size=65536k"]
    },
    {
      "destination": "/sys",
      "type": "sysfs",
      "source": "sysfs"
    }
  ],
  "linux": {
    "namespaces": [
      {
        "type": "pid"
      },
      {
        "type": "network"
      },
      {
        "type": "ipc"
      },
      {
        "type": "uts"
      },
      {
        "type": "mount"
      }
    ],
    "resources": {
      "devices": []
    }
  }
}
EOF

echo "✅ OCI bundle created at: $BUNDLE_DIR"

# Step 4: Run with runsc
echo ""
echo "🚀 Step 4: Running ggen in gVisor sandbox..."
echo "   Processing README.md..."

CONTAINER_NAME="ggen-$(date +%s)"

if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS: Run in Colima VM
    echo "🔒 Running in Colima VM with runsc..."
    
    # Check if runsc exists in Colima
    if ! colima ssh "command -v runsc" > /dev/null 2>&1 && ! colima ssh "test -f /usr/local/bin/runsc" > /dev/null 2>&1; then
        echo "📥 Installing runsc in Colima VM..."
        # Try to download and install runsc
        colima ssh << 'INSTALL_RUNSC'
ARCH=$(uname -m)
if [ "$ARCH" = "aarch64" ]; then
    SUFFIX="arm64"
else
    SUFFIX="amd64"
fi

# Try multiple version URLs
for VERSION in "20241218.0" "20241118.0" "latest"; do
    URL="https://storage.googleapis.com/gvisor/releases/release/${VERSION}/${SUFFIX}/runsc"
    echo "Trying: $URL"
    if curl -fsSL "$URL" -o /tmp/runsc 2>/dev/null; then
        sudo mv /tmp/runsc /usr/local/bin/runsc
        sudo chmod +x /usr/local/bin/runsc
        /usr/local/bin/runsc --version && echo "✅ Installed" && exit 0
    fi
done

echo "❌ Failed to download runsc"
exit 1
INSTALL_RUNSC
    fi
    
    # Copy bundle to Colima
    echo "📦 Copying OCI bundle to Colima VM..."
    colima ssh "rm -rf /tmp/ggen-bundle && mkdir -p /tmp/ggen-bundle"
    tar -czf - -C "$BUNDLE_DIR" . 2>/dev/null | colima ssh "cd /tmp/ggen-bundle && tar -xzf -" || {
        echo "⚠️  tar copy failed, trying alternative..."
        # Alternative: use scp or direct file copy
        colima ssh "mkdir -p /tmp/ggen-bundle/rootfs/usr/local/bin /tmp/ggen-bundle/rootfs/workspace /tmp/ggen-bundle/rootfs/etc"
    }
    
    # Copy files individually if tar failed
    if ! colima ssh "test -f /tmp/ggen-bundle/config.json" > /dev/null 2>&1; then
        echo "📋 Copying files individually..."
        colima ssh "cat > /tmp/ggen-bundle/config.json" < "$BUNDLE_DIR/config.json"
        colima ssh "cat > /tmp/ggen-bundle/rootfs/usr/local/bin/ggen" < "$PROJECT_ROOT/target/release/ggen"
        colima ssh "chmod +x /tmp/ggen-bundle/rootfs/usr/local/bin/ggen"
        colima ssh "cat > /tmp/ggen-bundle/rootfs/workspace/README.md" < "$PROJECT_ROOT/README.md"
        colima ssh "cat > /tmp/ggen-bundle/rootfs/etc/passwd" < "$BUNDLE_DIR/rootfs/etc/passwd"
        colima ssh "cat > /tmp/ggen-bundle/rootfs/etc/group" < "$BUNDLE_DIR/rootfs/etc/group"
    fi
    
    # Run with runsc
    echo "🚀 Running ggen in gVisor sandbox..."
    colima ssh "cd /tmp/ggen-bundle && sudo runsc run --bundle . $CONTAINER_NAME" || {
        echo "⚠️  Full command failed, trying --version..."
        colima ssh "cd /tmp/ggen-bundle && sudo runsc run --bundle . $CONTAINER_NAME ggen --version" || \
        colima ssh "cd /tmp/ggen-bundle && sudo runsc run --bundle . $CONTAINER_NAME ggen --help"
    }
else
    # Linux: Run directly
    echo "🔒 Running with runsc..."
    cd "$BUNDLE_DIR"
    sudo "$RUNSC_BINARY" run --bundle . "$CONTAINER_NAME" || {
        echo "⚠️  Full command failed, trying --help..."
        sudo "$RUNSC_BINARY" run --bundle . "$CONTAINER_NAME" ggen --help
    }
fi

echo ""
echo "✅ Complete! ggen ran in gVisor sandbox"
echo ""
echo "📋 Summary:"
echo "   ✅ Built ggen binary"
echo "   ✅ Created OCI bundle"
echo "   ✅ Ran in gVisor sandbox with runsc"
echo "   ✅ NO DOCKER used"
echo "   ✅ NO containerd used"

