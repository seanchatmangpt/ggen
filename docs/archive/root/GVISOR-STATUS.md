<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [gVisor Setup Status](#gvisor-setup-status)
  - [✅ Completed](#-completed)
  - [🔄 In Progress](#-in-progress)
  - [📋 Current Options](#-current-options)
    - [Option 1: Build runsc from vendored source (uses Docker for build only)](#option-1-build-runsc-from-vendored-source-uses-docker-for-build-only)
    - [Option 2: Use pre-built runsc (if downloads work)](#option-2-use-pre-built-runsc-if-downloads-work)
    - [Option 3: Manual runsc installation](#option-3-manual-runsc-installation)
  - [🚀 Once runsc is installed](#-once-runsc-is-installed)
  - [Architecture (Runtime - NO DOCKER)](#architecture-runtime---no-docker)
  - [Next Steps](#next-steps)
  - [Verification](#verification)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# gVisor Setup Status

## ✅ Completed

1. **Fixed compilation error** - Added missing `when` field to `GenerationRule::default()`
2. **ggen binary builds successfully** - `target/release/ggen` is ready
3. **OCI bundle creation script** - `scripts/run-ggen-gvisor-direct.sh` creates OCI bundles
4. **gVisor submodule** - `vendors/gvisor` is available for building runsc

## 🔄 In Progress

**runsc installation** - The gVisor build system requires Docker for building runsc from source. This is a one-time build step (Docker is NOT used at runtime).

## 📋 Current Options

### Option 1: Build runsc from vendored source (uses Docker for build only)

```bash
cd vendors/gvisor
make copy TARGETS=runsc DESTINATION=../../bin/
```

This will:
- Use Docker to build runsc (build-time only, not runtime)
- Place runsc in `bin/runsc`
- Then you can copy it to Colima: `colima ssh 'sudo cp $(pwd)/bin/runsc /usr/local/bin/runsc'`

### Option 2: Use pre-built runsc (if downloads work)

The script `scripts/setup-and-run-ggen-gvisor.sh` attempts to download runsc automatically, but the download URLs are currently failing.

### Option 3: Manual runsc installation

1. Download runsc from a working source
2. Copy to Colima: `colima ssh 'sudo cp /path/to/runsc /usr/local/bin/runsc'`
3. Run: `./scripts/setup-and-run-ggen-gvisor.sh`

## 🚀 Once runsc is installed

Run the complete workflow:

```bash
./scripts/setup-and-run-ggen-gvisor.sh
```

This will:
1. ✅ Build ggen (already done)
2. ✅ Install/verify runsc in Colima
3. ✅ Create OCI bundle with ggen + README.md
4. ✅ Run ggen in gVisor sandbox processing README.md
5. ✅ **NO DOCKER used at runtime**

## Architecture (Runtime - NO DOCKER)

```
ggen binary
    ↓
gVisor (runsc) - Application Kernel Sandbox
    ↓
Host Linux Kernel (via Colima VM)
```

No Docker daemon, no containerd - just runsc directly with OCI bundles.

## Next Steps

1. **Build runsc** (one-time, uses Docker for build):
   ```bash
   cd vendors/gvisor
   make copy TARGETS=runsc DESTINATION=../../bin/
   ```

2. **Install in Colima**:
   ```bash
   colima ssh 'sudo cp $(pwd)/bin/runsc /usr/local/bin/runsc && sudo chmod +x /usr/local/bin/runsc'
   ```

3. **Run ggen in gVisor**:
   ```bash
   ./scripts/setup-and-run-ggen-gvisor.sh
   ```

## Verification

Once runsc is installed, verify it works:

```bash
colima ssh 'runsc --version'
```

Then run the complete workflow to process README.md in the gVisor sandbox.

