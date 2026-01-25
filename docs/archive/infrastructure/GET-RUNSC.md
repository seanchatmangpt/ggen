<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How to Get runsc for gVisor](#how-to-get-runsc-for-gvisor)
  - [Method 1: Build from Vendored Source (Recommended)](#method-1-build-from-vendored-source-recommended)
  - [Method 2: Manual Download](#method-2-manual-download)
  - [Method 3: Use Homebrew (if available)](#method-3-use-homebrew-if-available)
  - [Once runsc is installed](#once-runsc-is-installed)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How to Get runsc for gVisor

Since automatic downloads are failing, here are working methods to get runsc:

## Method 1: Build from Vendored Source (Recommended)

The vendored gVisor source is at `vendors/gvisor`. Build runsc:

```bash
cd vendors/gvisor
make copy TARGETS=runsc DESTINATION=../../bin/
```

**Note**: This uses Docker for the build process (one-time), but Docker is NOT used at runtime.

If the build fails with container errors, try:

```bash
# Clean up any existing containers
docker rm -f $(docker ps -a --format "{{.Names}}" | grep gvisor) 2>/dev/null || true

# Try building again
cd vendors/gvisor
make copy TARGETS=runsc DESTINATION=../../bin/
```

## Method 2: Manual Download

1. Visit: https://github.com/google/gvisor/releases
2. Find the latest release
3. Download `runsc` for your architecture (arm64 for Apple Silicon, amd64 for Intel)
4. Copy to Colima:
   ```bash
   colima ssh 'sudo cp /path/to/downloaded/runsc /usr/local/bin/runsc'
   colima ssh 'sudo chmod +x /usr/local/bin/runsc'
   ```

## Method 3: Use Homebrew (if available)

```bash
brew install gvisor
```

Then copy to Colima if needed.

## Once runsc is installed

Verify it works:

```bash
colima ssh 'runsc --version'
```

Then run ggen in gVisor:

```bash
./scripts/run-ggen-gvisor-final.sh
```

This will process README.md in the gVisor sandbox with **NO DOCKER RUNTIME**.

