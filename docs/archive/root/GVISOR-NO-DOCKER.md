# Using gVisor Without Docker

This guide explains how to build and run ggen using gVisor directly with containerd, **without requiring Docker**.

## Overview

gVisor's `runsc` is an OCI-compliant runtime that can work with containerd directly, eliminating the need for Docker. This provides:
- ✅ No Docker dependency
- ✅ Direct containerd integration
- ✅ Same security isolation (gVisor sandbox)
- ✅ Lighter weight (no Docker daemon)

## Architecture

```
┌─────────────────────────────────────┐
│         Your Application            │
│            (ggen)                   │
└──────────────┬──────────────────────┘
               │
┌──────────────▼──────────────────────┐
│         gVisor (runsc)              │
│    Application Kernel Sandbox       │
└──────────────┬──────────────────────┘
               │
┌──────────────▼──────────────────────┐
│         containerd                  │
│    Container Runtime                │
└──────────────┬──────────────────────┘
               │
┌──────────────▼──────────────────────┐
│         Host Linux Kernel           │
└─────────────────────────────────────┘
```

## Prerequisites

1. **containerd** - Container runtime (can use Colima on macOS)
2. **buildah** - OCI image builder (alternative to Docker build)
3. **gVisor runsc** - Built from vendored source

## Setup Steps

### 1. Build runsc from Source

```bash
./scripts/build-gvisor-runsc.sh
```

This builds `runsc` from `vendors/gvisor` and places it in `bin/runsc`.

### 2. Install and Configure gVisor with containerd

```bash
# Install runsc and configure for containerd
./scripts/setup-gvisor-containerd.sh

# Configure containerd to use runsc
./scripts/configure-containerd-gvisor.sh
```

### 3. Build ggen as OCI Image

```bash
# Build ggen binary and create OCI image (no Docker)
./scripts/build-ggen-oci.sh
```

This creates an OCI image using `buildah` (no Docker required).

### 4. Run ggen with gVisor

```bash
# Run ggen in gVisor sandbox via containerd
./scripts/run-ggen-gvisor.sh /path/to/workspace sync
```

## Manual Steps (Alternative)

### macOS with Colima

```bash
# Start Colima with containerd
colima start --runtime containerd

# Install runsc in Colima VM
colima ssh "sudo cp $(pwd)/bin/runsc /usr/local/bin/runsc"
colima ssh "sudo /usr/local/bin/runsc install --runtime=runsc"

# Configure containerd
colima ssh "sudo containerd config default | sudo tee /etc/containerd/config.toml"
# Edit config to add runsc runtime (see configure-containerd-gvisor.sh)

# Build and import image
./scripts/build-ggen-oci.sh
buildah push ggen:latest oci-archive:ggen.tar
colima ssh "sudo ctr images import ggen.tar"

# Run with gVisor
colima ssh "sudo ctr run --runtime io.containerd.runsc.v1 ggen:latest ggen-test ggen sync"
```

### Linux (Native)

```bash
# Install containerd
sudo apt-get install containerd

# Install runsc
sudo cp bin/runsc /usr/local/bin/runsc
sudo /usr/local/bin/runsc install --runtime=runsc

# Configure containerd
sudo containerd config default | sudo tee /etc/containerd/config.toml
# Add runsc runtime to config

# Build and import image
./scripts/build-ggen-oci.sh
sudo ctr images import ggen.tar

# Run with gVisor
sudo ctr run --runtime io.containerd.runsc.v1 \
  --mount type=bind,src=$(pwd),dst=/workspace,options=rbind:rw \
  ggen:latest ggen-test ggen sync
```

## Comparison: Docker vs containerd + gVisor

| Feature | Docker + gVisor | containerd + gVisor |
|---------|----------------|---------------------|
| Docker dependency | ✅ Required | ❌ Not required |
| Image building | `docker build` | `buildah` |
| Runtime | Docker daemon | containerd daemon |
| gVisor isolation | ✅ Yes | ✅ Yes |
| OCI compliance | ✅ Yes | ✅ Yes |
| Weight | Heavier | Lighter |

## Troubleshooting

### containerd not found

**macOS (Colima):**
```bash
colima start --runtime containerd
```

**Linux:**
```bash
sudo apt-get install containerd
sudo systemctl start containerd
```

### runsc not recognized by containerd

1. Verify runsc is installed: `runsc --version`
2. Install for containerd: `sudo runsc install --runtime=runsc`
3. Check containerd config includes runsc runtime
4. Restart containerd: `sudo systemctl restart containerd`

### Image import fails

```bash
# Check image exists
buildah images

# Re-export if needed
buildah push ggen:latest oci-archive:ggen.tar

# Import to containerd
sudo ctr images import ggen.tar
```

### Permission errors

Use `sudo` for containerd operations on Linux:
```bash
sudo ctr images ls
sudo ctr run ...
```

## Benefits of No-Docker Approach

1. **Lighter Weight**: No Docker daemon overhead
2. **Direct Integration**: containerd is what Kubernetes uses
3. **OCI Native**: Direct OCI image handling
4. **Same Security**: Full gVisor isolation
5. **Production Ready**: Aligns with Kubernetes runtime model

## Next Steps

- Build ggen images for different architectures
- Set up CI/CD with containerd + gVisor
- Integrate with Kubernetes (uses containerd by default)

## Resources

- [gVisor containerd Quick Start](https://gvisor.dev/docs/user_guide/quick_start/containerd/)
- [containerd Documentation](https://containerd.io/docs/)
- [buildah Documentation](https://buildah.io/)

