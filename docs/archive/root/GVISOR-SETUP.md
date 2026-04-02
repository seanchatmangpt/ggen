<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [gVisor Setup Guide for ggen](#gvisor-setup-guide-for-ggen)
  - [Overview](#overview)
  - [Prerequisites](#prerequisites)
  - [Quick Start](#quick-start)
    - [1. Initialize Submodules](#1-initialize-submodules)
    - [2. Build runsc from Source](#2-build-runsc-from-source)
    - [3. Install runsc (Colima)](#3-install-runsc-colima)
    - [4. Verify Installation](#4-verify-installation)
  - [Using gVisor with ggen](#using-gvisor-with-ggen)
    - [Build ggen Image with gVisor Support](#build-ggen-image-with-gvisor-support)
    - [Run ggen with gVisor](#run-ggen-with-gvisor)
    - [Build and Test with gVisor](#build-and-test-with-gvisor)
  - [Docker Compose with gVisor](#docker-compose-with-gvisor)
  - [Troubleshooting](#troubleshooting)
    - [runsc not found](#runsc-not-found)
    - [Build failures](#build-failures)
    - [Permission issues](#permission-issues)
  - [Alternative: Using Pre-built runsc](#alternative-using-pre-built-runsc)
  - [Resources](#resources)
  - [Project Integration](#project-integration)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# gVisor Setup Guide for ggen

This guide explains how to set up and use gVisor with the ggen project. gVisor provides additional security isolation for containers by implementing a userspace kernel.

## Overview

gVisor is included in this project as a git submodule at `vendors/gvisor`. This allows us to build `runsc` (the gVisor runtime) from source and integrate it with Docker/Colima.

## Prerequisites

- Docker or Colima running
- Git submodules initialized
- Build tools (Docker for building gVisor)

## Quick Start

### 1. Initialize Submodules

If you haven't already, initialize the gVisor submodule:

```bash
git submodule update --init --recursive vendors/gvisor
```

### 2. Build runsc from Source

Build the `runsc` binary from the vendored gVisor source:

```bash
./scripts/build-gvisor-runsc.sh
```

This will:
- Build `runsc` using gVisor's Makefile (which uses Docker)
- Place the binary in `bin/runsc`
- Verify the build with `runsc --version`

### 3. Install runsc (Colima)

For Colima, install runsc in the VM:

```bash
# Copy runsc into Colima VM
colima ssh "sudo cp $(pwd)/bin/runsc /usr/local/bin/runsc && sudo chmod +x /usr/local/bin/runsc"

# Install as Docker runtime
colima ssh "sudo /usr/local/bin/runsc install"

# Configure Docker daemon
colima ssh 'sudo mkdir -p /etc/docker && echo "{\"runtimes\":{\"runsc\":{\"path\":\"/usr/local/bin/runsc\",\"runtimeArgs\":[]}}}" | sudo tee /etc/docker/daemon.json'

# Restart Docker
colima ssh "sudo systemctl restart docker || sudo service docker restart"
```

### 4. Verify Installation

Test that gVisor is working:

```bash
# Switch to Colima context
docker context use colima

# Test gVisor
docker run --runtime=runsc --rm alpine echo "✅ gVisor is working!"
```

## Using gVisor with ggen

### Build ggen Image with gVisor Support

Build the Docker image that includes gVisor:

```bash
docker build -f Dockerfile.gvisor -t ggen:gvisor .
```

### Run ggen with gVisor

```bash
# Using docker run
docker run --runtime=runsc --rm -v $(pwd):/workspace ggen:gvisor sync

# Using docker-compose
docker-compose -f docker-compose.gvisor.yml up
```

### Build and Test with gVisor

```bash
# Build and test in gVisor container
docker-compose -f docker-compose.gvisor.yml run --rm ggen-build
```

## Docker Compose with gVisor

The `docker-compose.gvisor.yml` file is configured to use gVisor:

```yaml
services:
  ggen:
    runtime: runsc
    # ... other config
```

## Troubleshooting

### runsc not found

If you get "unknown or invalid runtime name: runsc":

1. Verify runsc is installed: `colima ssh "which runsc"`
2. Check Docker daemon config: `colima ssh "cat /etc/docker/daemon.json"`
3. Restart Docker: `colima ssh "sudo systemctl restart docker"`

### Build failures

If building runsc fails:

1. Ensure Docker is running: `docker info`
2. Check gVisor submodule is initialized: `git submodule status`
3. Try updating submodule: `git submodule update --remote vendors/gvisor`

### Permission issues

If you encounter permission issues:

```bash
# Make runsc executable
chmod +x bin/runsc

# In Colima VM
colima ssh "sudo chmod +x /usr/local/bin/runsc"
```

## Alternative: Using Pre-built runsc

If building from source fails, you can download a pre-built binary:

```bash
# For ARM64 (Apple Silicon)
ARCH="arm64"
VERSION="20241218.0"  # Use latest version
curl -fsSL "https://storage.googleapis.com/gvisor/releases/release/${VERSION}/${ARCH}/runsc" -o bin/runsc
chmod +x bin/runsc
```

Note: The download URLs may change. Check [gVisor releases](https://github.com/google/gvisor/releases) for the latest version.

## Resources

- [gVisor Documentation](https://gvisor.dev)
- [gVisor GitHub](https://github.com/google/gvisor)
- [gVisor Quick Start](https://gvisor.dev/docs/user_guide/quick_start/docker/)
- [Colima Documentation](https://github.com/abiosoft/colima)

## Project Integration

The gVisor submodule is located at:
- **Path**: `vendors/gvisor`
- **URL**: `https://github.com/seanchatmangpt/gvisor.git`
- **Build Script**: `scripts/build-gvisor-runsc.sh`
- **Dockerfile**: `Dockerfile.gvisor`
- **Docker Compose**: `docker-compose.gvisor.yml`

