# Complete gVisor Workflow (NO DOCKER)

This document provides the complete workflow to build and run ggen in gVisor **without using Docker at runtime**.

## Quick Start

```bash
# Single command to build and run ggen in gVisor (NO DOCKER)
./scripts/run-ggen-gvisor-simple.sh
```

## What This Does

1. ✅ **Builds ggen binary** (Rust compilation)
2. ✅ **Creates OCI image** using `buildah` (NO Docker)
3. ✅ **Runs in gVisor sandbox** via `containerd` (NO Docker runtime)
4. ✅ **Processes README.md** inside the container

## Architecture

```
┌─────────────────────┐
│   ggen binary       │
│   (Rust compiled)   │
└──────────┬──────────┘
           │
┌──────────▼──────────┐
│   gVisor (runsc)    │
│   Application Kernel │
│   Sandbox           │
└──────────┬──────────┘
           │
┌──────────▼──────────┐
│   containerd         │
│   (NO Docker!)       │
└──────────┬──────────┘
           │
┌──────────▼──────────┐
│   Host Kernel        │
└─────────────────────┘
```

## Prerequisites

1. **Colima** (macOS) or **containerd** (Linux)
2. **buildah** (for OCI image creation)
3. **Rust toolchain** (for building ggen)

## Step-by-Step Manual Process

### 1. Build ggen Binary

```bash
cargo build --release --package ggen-cli-lib --bin ggen
```

### 2. Create OCI Image with buildah

```bash
# Create container
CONTAINER=$(buildah from scratch)
MOUNTPOINT=$(buildah mount $CONTAINER)

# Setup rootfs
mkdir -p $MOUNTPOINT/usr/local/bin
mkdir -p $MOUNTPOINT/workspace
mkdir -p $MOUNTPOINT/etc

# Copy binary
cp target/release/ggen $MOUNTPOINT/usr/local/bin/ggen
chmod +x $MOUNTPOINT/usr/local/bin/ggen

# Create minimal /etc files
echo "root:x:0:0:root:/root:/bin/sh" > $MOUNTPOINT/etc/passwd
echo "root:x:0:" > $MOUNTPOINT/etc/group

# Configure container
buildah config --cmd '["ggen", "--help"]' $CONTAINER
buildah config --workingdir /workspace $CONTAINER
buildah config --entrypoint '["/usr/local/bin/ggen"]' $CONTAINER

# Commit image
buildah commit $CONTAINER ggen:latest
buildah unmount $CONTAINER

# Export to tar
buildah push ggen:latest oci-archive:ggen.tar
```

### 3. Import to containerd

```bash
# macOS (Colima)
colima ssh "sudo ctr images import ggen.tar"

# Linux
sudo ctr images import ggen.tar
```

### 4. Run ggen in gVisor

```bash
# macOS (Colima)
colima ssh "sudo ctr run --rm \
    --mount type=bind,src=/path/to/workspace,dst=/workspace,options=rbind:ro \
    --runtime io.containerd.runsc.v1 \
    ggen:latest \
    ggen-test \
    ggen sync --from /workspace/README.md --verbose"

# Linux
sudo ctr run --rm \
    --mount type=bind,src=$(pwd),dst=/workspace,options=rbind:ro \
    --runtime io.containerd.runsc.v1 \
    ggen:latest \
    ggen-test \
    ggen sync --from /workspace/README.md --verbose
```

## Verification

To verify gVisor is working:

```bash
# Check dmesg output (gVisor shows special messages)
sudo ctr run --runtime io.containerd.runsc.v1 \
    ggen:latest test-container \
    sh -c "dmesg | head -5"
```

You should see gVisor-specific messages like:
```
[   0.000000] Starting gVisor...
[   0.445958] Forking spaghetti code...
```

## Troubleshooting

### buildah not found

**macOS:**
```bash
brew install buildah
```

**Linux:**
```bash
sudo apt-get install buildah
```

### containerd not found

**macOS (Colima):**
```bash
colima start --runtime containerd
```

**Linux:**
```bash
sudo apt-get install containerd
```

### runsc runtime not available

Ensure gVisor is installed and configured:

```bash
# Install runsc
./scripts/build-gvisor-runsc.sh

# Configure for containerd
./scripts/configure-containerd-gvisor.sh
```

### Image import fails

```bash
# Re-export image
buildah push ggen:latest oci-archive:ggen.tar

# Import again
sudo ctr images import ggen.tar
```

## Benefits

- ✅ **No Docker dependency** - Uses containerd directly
- ✅ **Same security** - Full gVisor isolation
- ✅ **Lighter weight** - No Docker daemon overhead
- ✅ **Production ready** - Aligns with Kubernetes runtime model
- ✅ **OCI native** - Direct OCI image handling

## Next Steps

Once the code compiles successfully, the workflow script will:
1. Build ggen
2. Create OCI image
3. Run in gVisor sandbox
4. Process README.md

All without using Docker at runtime!

