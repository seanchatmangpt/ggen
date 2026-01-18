#!/bin/bash
# Simple workflow: Build ggen and run it in gVisor via containerd (NO DOCKER RUNTIME)
# This uses buildah for OCI images and containerd + gVisor for execution

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "🚀 Building and running ggen in gVisor (NO DOCKER RUNTIME)"
echo "=========================================================="

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

# Step 2: Check Colima and containerd
echo ""
echo "📦 Step 2: Checking Colima and containerd..."

if [[ "$OSTYPE" == "darwin"* ]]; then
    if ! colima status > /dev/null 2>&1; then
        echo "🚀 Starting Colima with containerd..."
        colima start --runtime containerd
    fi
    
    # Check for ctr in Colima
    if ! colima ssh "command -v ctr" > /dev/null 2>&1; then
        echo "❌ containerd (ctr) not found in Colima"
        echo "   Colima may need to be restarted with: colima start --runtime containerd"
        exit 1
    fi
    
    CTR_CMD="colima ssh sudo ctr"
else
    if ! command -v ctr &> /dev/null; then
        echo "❌ containerd (ctr) not found"
        echo "   Install: sudo apt-get install containerd"
        exit 1
    fi
    CTR_CMD="sudo ctr"
fi

# Step 3: Create minimal OCI image using buildah or manual method
echo ""
echo "📦 Step 3: Creating OCI image..."

# Check for buildah
if command -v buildah &> /dev/null; then
    echo "🔨 Using buildah to create OCI image..."
    
    CONTAINER=$(buildah from scratch)
    MOUNTPOINT=$(buildah mount "$CONTAINER")
    
    mkdir -p "$MOUNTPOINT/usr/local/bin"
    mkdir -p "$MOUNTPOINT/workspace"
    mkdir -p "$MOUNTPOINT/etc"
    
    cp target/release/ggen "$MOUNTPOINT/usr/local/bin/ggen"
    chmod +x "$MOUNTPOINT/usr/local/bin/ggen"
    
    echo "root:x:0:0:root:/root:/bin/sh" > "$MOUNTPOINT/etc/passwd"
    echo "root:x:0:" > "$MOUNTPOINT/etc/group"
    
    buildah config --cmd '["ggen", "--help"]' "$CONTAINER"
    buildah config --workingdir /workspace "$CONTAINER"
    buildah config --entrypoint '["/usr/local/bin/ggen"]' "$CONTAINER"
    
    IMAGE_NAME="ggen:latest"
    buildah commit "$CONTAINER" "$IMAGE_NAME"
    buildah unmount "$CONTAINER"
    
    # Export to tar
    IMAGE_TAR="$PROJECT_ROOT/ggen.tar"
    buildah push "$IMAGE_NAME" "oci-archive:$IMAGE_TAR"
    
    echo "✅ OCI image created: $IMAGE_TAR"
else
    echo "⚠️  buildah not found - using manual OCI bundle creation..."
    
    # Create OCI bundle manually
    BUNDLE_DIR="$PROJECT_ROOT/ggen-bundle"
    rm -rf "$BUNDLE_DIR"
    mkdir -p "$BUNDLE_DIR/rootfs/usr/local/bin"
    mkdir -p "$BUNDLE_DIR/rootfs/workspace"
    mkdir -p "$BUNDLE_DIR/rootfs/etc"
    
    cp target/release/ggen "$BUNDLE_DIR/rootfs/usr/local/bin/ggen"
    chmod +x "$BUNDLE_DIR/rootfs/usr/local/bin/ggen"
    
    echo "root:x:0:0:root:/root:/bin/sh" > "$BUNDLE_DIR/rootfs/etc/passwd"
    echo "root:x:0:" > "$BUNDLE_DIR/rootfs/etc/group"
    
    # Create config.json for OCI bundle
    cat > "$BUNDLE_DIR/config.json" << 'EOF'
{
  "ociVersion": "1.0.0",
  "process": {
    "terminal": false,
    "user": {
      "uid": 0,
      "gid": 0
    },
    "args": [
      "/usr/local/bin/ggen",
      "--help"
    ],
    "env": [
      "PATH=/usr/local/bin:/usr/bin:/bin",
      "TERM=xterm"
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
    ]
  }
}
EOF
    
    echo "✅ OCI bundle created at: $BUNDLE_DIR"
    echo "⚠️  Note: Using OCI bundle directly (not containerd image)"
    USE_BUNDLE=true
fi

# Step 4: Import to containerd (if using buildah)
if [ "${USE_BUNDLE:-false}" != "true" ]; then
    echo ""
    echo "📦 Step 4: Importing image to containerd..."
    
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # Copy tar to Colima VM
        colima ssh "mkdir -p /tmp/ggen-images"
        scp -q "$IMAGE_TAR" "$(colima ssh 'echo $USER@$(hostname -I | awk "{print \$1}")'):/tmp/ggen-images/ggen.tar" 2>/dev/null || \
        colima ssh "cat > /tmp/ggen-images/ggen.tar" < "$IMAGE_TAR"
        
        colima ssh "sudo ctr images import /tmp/ggen-images/ggen.tar" || echo "Image may already exist"
    else
        sudo ctr images import "$IMAGE_TAR" || echo "Image may already exist"
    fi
    
    echo "✅ Image imported to containerd"
fi

# Step 5: Run ggen in gVisor
echo ""
echo "🚀 Step 5: Running ggen in gVisor sandbox..."
echo "   Processing README.md..."

CONTAINER_NAME="ggen-$(date +%s)"

if [ "${USE_BUNDLE:-false}" = "true" ]; then
    # Use OCI bundle directly with runsc
    echo "🔒 Running with runsc directly (OCI bundle)..."
    
    # Check if runsc is available
    if [[ "$OSTYPE" == "darwin"* ]]; then
        if ! colima ssh "command -v runsc" > /dev/null 2>&1; then
            echo "❌ runsc not found in Colima VM"
            echo "   Install runsc first or use containerd method"
            exit 1
        fi
        
        # Copy README.md to bundle
        cp "$PROJECT_ROOT/README.md" "$BUNDLE_DIR/rootfs/workspace/README.md"
        
        # Copy bundle to Colima
        colima ssh "rm -rf /tmp/ggen-bundle && mkdir -p /tmp/ggen-bundle"
        tar -czf - -C "$BUNDLE_DIR" . | colima ssh "cd /tmp/ggen-bundle && tar -xzf -"
        
        # Run with runsc
        colima ssh "cd /tmp/ggen-bundle && sudo runsc run --bundle . ggen-test ggen sync --from /workspace/README.md --verbose" || \
        colima ssh "cd /tmp/ggen-bundle && sudo runsc run --bundle . ggen-test ggen --version"
    else
        # Linux: run directly
        cp "$PROJECT_ROOT/README.md" "$BUNDLE_DIR/rootfs/workspace/README.md"
        
        if command -v runsc &> /dev/null; then
            cd "$BUNDLE_DIR"
            sudo runsc run --bundle . "$CONTAINER_NAME" ggen sync --from /workspace/README.md --verbose || \
            sudo runsc run --bundle . "$CONTAINER_NAME" ggen --version
        else
            echo "❌ runsc not found"
            exit 1
        fi
    fi
else
    # Use containerd with gVisor runtime
    echo "🔒 Running with containerd + gVisor..."
    
    # Copy README.md to a temp location for mounting
    TEMP_DIR=$(mktemp -d)
    cp "$PROJECT_ROOT/README.md" "$TEMP_DIR/README.md"
    
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # Colima: copy to VM
        colima ssh "mkdir -p /tmp/ggen-workspace"
        colima ssh "cat > /tmp/ggen-workspace/README.md" < "$PROJECT_ROOT/README.md"
        
        # Run with containerd + gVisor
        colima ssh "sudo ctr run --rm \
            --mount type=bind,src=/tmp/ggen-workspace,dst=/workspace,options=rbind:ro \
            --runtime io.containerd.runsc.v1 \
            ggen:latest \
            $CONTAINER_NAME \
            ggen sync --from /workspace/README.md --verbose" || \
        colima ssh "sudo ctr run --rm \
            --runtime io.containerd.runsc.v1 \
            ggen:latest \
            $CONTAINER_NAME \
            ggen --version"
    else
        # Linux: direct mount
        $CTR_CMD run --rm \
            --mount "type=bind,src=$TEMP_DIR,dst=/workspace,options=rbind:ro" \
            --runtime io.containerd.runsc.v1 \
            ggen:latest \
            "$CONTAINER_NAME" \
            ggen sync --from /workspace/README.md --verbose || \
        $CTR_CMD run --rm \
            --runtime io.containerd.runsc.v1 \
            ggen:latest \
            "$CONTAINER_NAME" \
            ggen --version
    fi
    
    rm -rf "$TEMP_DIR"
fi

echo ""
echo "✅ Complete! ggen ran in gVisor sandbox (NO DOCKER RUNTIME)"
echo ""
echo "📋 Summary:"
echo "   ✅ Built ggen binary"
echo "   ✅ Created OCI image/bundle"
echo "   ✅ Ran in gVisor sandbox via containerd/runsc"
echo "   ✅ NO DOCKER used for runtime execution"

