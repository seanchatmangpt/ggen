<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Debian Package Distribution Guide](#ggen-debian-package-distribution-guide)
  - [What We Have](#what-we-have)
  - [How to Build the .deb Package](#how-to-build-the-deb-package)
    - [Option 1: Using debuild (Full packaging)](#option-1-using-debuild-full-packaging)
    - [Option 2: Using dpkg-buildpackage](#option-2-using-dpkg-buildpackage)
  - [How to Install Locally (for testing)](#how-to-install-locally-for-testing)
  - [How to Distribute via PPA (Ubuntu PPA)](#how-to-distribute-via-ppa-ubuntu-ppa)
  - [How to Distribute via GitHub Releases](#how-to-distribute-via-github-releases)
  - [How to Set Up Automated APT Repository](#how-to-set-up-automated-apt-repository)
  - [What's in the Debian Package](#whats-in-the-debian-package)
  - [Current Status](#current-status)
  - [Next Steps](#next-steps)
  - [Troubleshooting](#troubleshooting)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Debian Package Distribution Guide

This document explains how to distribute ggen as a Debian/Ubuntu package via `apt-get`.

## What We Have

✅ **Build success**: `cargo build --release -p ggen-cli-lib --bin ggen` (16MB binary)
✅ **Debian packaging**: Complete `debian/` directory with standard packaging files
✅ **Binary works**: `ggen --help` displays full command documentation

## How to Build the .deb Package

### Option 1: Using debuild (Full packaging)

```bash
cd /home/user/ggen
debuild -us -uc  # Build without GPG signing (for testing)
```

This creates `../ggen_5.0.2-1_amd64.deb` in the parent directory.

### Option 2: Using dpkg-buildpackage

```bash
cd /home/user/ggen
dpkg-buildpackage -us -uc -b  # Binary-only build
```

## How to Install Locally (for testing)

```bash
sudo apt-get install ./ggen_5.0.2-1_amd64.deb
ggen --help  # Should work!
```

## How to Distribute via PPA (Ubuntu PPA)

1. **Create a Launchpad account** and set up GPG keys
2. **Create a PPA** on launchpad.net (Personal Package Archives)
3. **Sign your package**:
   ```bash
   debuild -S  # Creates source package with GPG signature
   ```
4. **Upload to PPA**:
   ```bash
   dput ppa:your-username/ggen ../ggen_5.0.2-1_source.changes
   ```
5. **Users install with**:
   ```bash
   sudo add-apt-repository ppa:your-username/ggen
   sudo apt-get update
   sudo apt-get install ggen
   ```

## How to Distribute via GitHub Releases

1. **Build the .deb package**:
   ```bash
   debuild -us -uc
   ```
2. **Create a GitHub release** for v5.0.2
3. **Attach the .deb artifact**:
   - `ggen_5.0.2-1_amd64.deb`
   - Also attach binary: `target/release/ggen`
4. **Users can install**:
   ```bash
   wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2-1_amd64.deb
   sudo apt-get install ./ggen_5.0.2-1_amd64.deb
   ```

## How to Set Up Automated APT Repository

1. **Using GitHub Releases as APT source**:
   Create a `simple-index` repository on GitHub

2. **Using apt-server** (standalone APT server):
   ```bash
   dput repo ggen_5.0.2-1_source.changes
   ```

3. **Using apt-get-repository** (community):
   Host your own APT server with reprepro or apt-ftparchive

## What's in the Debian Package

The `debian/rules` file installs:
- **Binary**: `ggen` → `/usr/bin/ggen`
- **Documentation**: `README.md`, LICENSE → `/usr/share/doc/ggen/`
- **Man pages**: (future: can add ggen.1)

## Current Status

| Task | Status |
|------|--------|
| Build binary | ✅ Complete |
| Debian packaging | ✅ Complete |
| Build .deb package | ⏳ Ready (needs debuild) |
| PPA setup | ⏳ Pending |
| GitHub releases | ⏳ Pending |
| apt-get install | ⏳ After distribution |

## Next Steps

To complete the apt-get distribution:

1. **Option A: GitHub Releases** (easiest for open source):
   ```bash
   debuild -us -uc
   gh release create v5.0.2 ggen_5.0.2-1_amd64.deb target/release/ggen
   ```

2. **Option B: Ubuntu PPA** (recommended for Ubuntu):
   ```bash
   # Requires Launchpad account and GPG setup
   debuild -S
   dput ppa:username/ggen ../ggen_5.0.2-1_source.changes
   ```

3. **Option C: Self-hosted APT server**:
   ```bash
   # Host on your own server with reprepro
   # Users add: sudo add-apt-repository deb https://your-apt-repo.com/ jammy main
   ```

## Troubleshooting

- **debuild not found**: `sudo apt-get install devscripts`
- **cargo not found**: Ensure Rust is installed or add to PATH
- **Permission denied**: Most commands need `sudo`
- **GPG signing**: Use `-us -uc` flags to skip signing for testing

## References

- [Debian New Maintainers Guide](https://www.debian.org/doc/manuals/maint-guide/)
- [Ubuntu PPA Help](https://help.launchpad.net/Packaging/PPA)
- [Debian Policy Manual](https://www.debian.org/doc/debian-policy/)
