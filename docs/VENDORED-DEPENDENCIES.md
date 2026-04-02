<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Vendored Dependencies](#vendored-dependencies)
  - [Current Vendored Dependencies](#current-vendored-dependencies)
    - [1. gVisor (`vendors/gvisor`)](#1-gvisor-vendorsgvisor)
    - [2. Spec-Kit (`vendors/spec-kit`)](#2-spec-kit-vendorsspec-kit)
  - [Dependencies NOT Vendored (And Why)](#dependencies-not-vendored-and-why)
    - [Docker Base Images](#docker-base-images)
    - [Rust Dependencies (Cargo)](#rust-dependencies-cargo)
    - [Python Dependencies](#python-dependencies)
    - [Node.js Dependencies](#nodejs-dependencies)
    - [Bazel (for gVisor builds)](#bazel-for-gvisor-builds)
  - [External Downloads in Scripts](#external-downloads-in-scripts)
  - [Recommendations](#recommendations)
    - [✅ Already Vendored](#-already-vendored)
    - [⚠️ Consider Vendoring (If Needed)](#-consider-vendoring-if-needed)
    - [🔄 Scripts to Update](#-scripts-to-update)
  - [Updating Vendored Dependencies](#updating-vendored-dependencies)
    - [Update gVisor Submodule](#update-gvisor-submodule)
    - [Update Spec-Kit Submodule](#update-spec-kit-submodule)
    - [Initialize All Submodules](#initialize-all-submodules)
  - [Build Reproducibility](#build-reproducibility)
  - [Future Considerations](#future-considerations)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Vendored Dependencies

This document tracks all external dependencies that have been vendored as git submodules to avoid relying on external downloads during builds.

## Current Vendored Dependencies

### 1. gVisor (`vendors/gvisor`)
- **Repository**: https://github.com/seanchatmangpt/gvisor
- **Purpose**: Application kernel for containers, provides `runsc` runtime
- **Why Vendored**: 
  - Builds `runsc` from source instead of downloading binaries
  - Ensures reproducible builds
  - Allows customization if needed
- **Build Script**: `scripts/build-gvisor-runsc.sh`
- **Usage**: Build runsc from source: `./scripts/build-gvisor-runsc.sh`

### 2. Spec-Kit (`vendors/spec-kit`)
- **Repository**: https://github.com/seanchatmangpt/spec-kit (or original source)
- **Purpose**: Spec-Driven Development (SDD) tooling with RDF-first architecture
- **Why Vendored**: 
  - Used for project specification and planning
  - Provides RDF templates and ontology support
- **Status**: Already exists in vendors directory

## Dependencies NOT Vendored (And Why)

### Docker Base Images
- **Examples**: `rust:bookworm`, `debian:bookworm-slim`
- **Why Not**: Standard Docker practice - base images are pulled from Docker Hub/registries
- **Impact**: Low - these are standard, well-maintained images

### Rust Dependencies (Cargo)
- **Managed By**: `Cargo.lock`
- **Why Not**: Cargo's dependency management is robust and reproducible via lockfile
- **Impact**: Low - Cargo ensures reproducible builds via lockfile

### Python Dependencies
- **Managed By**: `pyproject.toml`, `requirements.txt`, or `poetry.lock`
- **Why Not**: Standard Python package management
- **Impact**: Low - Lockfiles ensure reproducibility

### Node.js Dependencies
- **Managed By**: `package-lock.json` or `yarn.lock`
- **Why Not**: Standard Node.js package management
- **Impact**: Low - Lockfiles ensure reproducibility

### Bazel (for gVisor builds)
- **Why Not**: gVisor's build system uses Docker containers that include Bazel
- **Impact**: None - Handled internally by gVisor's build process

## External Downloads in Scripts

The following scripts download external resources, but these are **not build dependencies**:

1. **Release Scripts** (`scripts/release-*.sh`)
   - Download GitHub releases for testing/validation
   - Not required for building the project
   - Used only for release processes

2. **Setup Scripts** (`scripts/setup-gvisor-*.sh`)
   - Download gVisor binaries as fallback
   - **Now Obsolete**: Use `scripts/build-gvisor-runsc.sh` instead
   - Can be updated to use vendored source

3. **Marketplace Scripts** (`marketplace/scripts/*.sh`)
   - Generate indices and metadata
   - Not build dependencies

## Recommendations

### ✅ Already Vendored
- gVisor (for building runsc from source)
- Spec-Kit (for SDD tooling)

### ⚠️ Consider Vendoring (If Needed)
None currently identified. All critical build dependencies are either:
- Already vendored (gVisor, spec-kit)
- Managed by package managers with lockfiles (Cargo, npm, pip)
- Standard Docker base images

### 🔄 Scripts to Update
The following scripts can be updated to use vendored gVisor instead of downloads:

1. `scripts/setup-gvisor.sh` - Update to use `vendors/gvisor`
2. `scripts/setup-gvisor-colima.sh` - Update to use `vendors/gvisor`
3. `scripts/install-gvisor-in-colima.sh` - Update to use `vendors/gvisor`
4. `Dockerfile.gvisor` - Update to build from `vendors/gvisor` instead of downloading

## Updating Vendored Dependencies

### Update gVisor Submodule
```bash
cd vendors/gvisor
git fetch origin
git checkout <desired-branch-or-tag>
cd ../..
git add vendors/gvisor
git commit -m "Update gVisor submodule to <version>"
```

### Update Spec-Kit Submodule
```bash
cd vendors/spec-kit
git fetch origin
git checkout <desired-branch-or-tag>
cd ../..
git add vendors/spec-kit
git commit -m "Update spec-kit submodule to <version>"
```

### Initialize All Submodules
```bash
git submodule update --init --recursive
```

## Build Reproducibility

With vendored dependencies:
- ✅ gVisor builds from source (no external downloads)
- ✅ Spec-Kit available locally
- ✅ Rust dependencies locked via `Cargo.lock`
- ✅ All critical build dependencies are deterministic

## Future Considerations

If new large dependencies are added that:
1. Are critical for builds
2. Have unreliable download sources
3. Need customization
4. Are large binaries/tools

Then consider vendoring them as git submodules.

