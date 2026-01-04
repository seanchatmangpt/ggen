# ggen GitHub Release Workflow (Fail-Proof Edition)

This document describes the complete, idempotent, poka-yoke fail-proof process for releasing ggen as a Debian package via GitHub Releases.

## Architecture: Why GitHub Releases?

**Chosen Distribution Method**: GitHub Releases (most reliable for open-source)

**Why not alternatives?**
- ❌ **PPA**: Requires Launchpad account + GPG keys (external dependency failure points)
- ❌ **Self-hosted APT**: Requires maintaining server infrastructure (operational burden)
- ✅ **GitHub Releases**: Leverages existing GitHub infrastructure, built-in CDN, no external dependencies

**Availability**: 99.95% uptime, distributed globally via GitHub's CDN

---

## Release Process (Automated + Manual Verification)

### Phase 1: Pre-Release Validation (No Manual Steps Required)

Run the build script **from the ggen repository root**:

```bash
cd /home/user/ggen
bash scripts/build-debian-package.sh
```

**What it does** (12 poka-yoke gates):

| Gate | Check | Poka-Yoke | Output |
|------|-------|-----------|--------|
| 1 | Required tools (cargo, debuild, sha256sum) | FAIL if missing | ❌ Exit with error |
| 2 | Project structure (ggen.toml, Cargo.toml, debian/) | FAIL if invalid | ❌ Exit with error |
| 3 | Clean old artifacts (preserve cargo target) | Skip cargo clean | Selective cleanup |
| 4 | Build release binary | FAIL if missing | ❌ Exit if binary not found |
| 5 | Test binary (verify `ggen --help` works) | FAIL if doesn't work | ❌ Exit if test fails |
| 6 | Validate binary size (1MB - 100MB) | FAIL if outside range | ❌ Exit if suspicious |
| 7 | Create binary checksums (SHA256) | Always produces checksums | ggen.sha256 |
| 8 | Build .deb via debuild | FAIL if dpkg-deb fails | ❌ Exit with error |
| 9 | Verify .deb exists and is valid | FAIL if missing/small | ❌ Exit if invalid |
| 10 | Create .deb checksums (SHA256) | Always produces checksums | ggen_5.0.2-1_amd64.deb.sha256 |
| 11 | List distribution artifacts | Display results | Console output |
| 12 | Display next steps | Echo instructions | Release commands |

**Output locations**:
- `.deb package`: `../ggen_5.0.2-1_amd64.deb` (4.1 MB)
- `.deb checksum`: `../ggen_5.0.2-1_amd64.deb.sha256`
- Binary checksum`: `./ggen.sha256`

**Exit codes**:
- `0` = Success (all gates passed, artifacts ready)
- `1` = Failure (any gate failed, error reported with color-coded ✗)

---

### Phase 2: Create GitHub Release

Once the build script completes successfully, create the release:

```bash
# Verify artifacts exist
ls -lh ../ggen_5.0.2-1_amd64.deb*
ls -lh ggen.sha256

# Create GitHub release (requires 'gh' CLI installed)
gh release create v5.0.2 \
  ../ggen_5.0.2-1_amd64.deb \
  ../ggen_5.0.2-1_amd64.deb.sha256 \
  --title "ggen v5.0.2: SPARQL Inference + Fail-Proof Distribution" \
  --notes "
## Release Highlights

✅ **SPARQL Inference**: Full semantic enrichment via CONSTRUCT queries
✅ **Tera Templates**: Type-safe code generation with template variables
✅ **SHACL Validation**: Specifications validated before code generation
✅ **Fail-Proof Distribution**: Debian package with poka-yoke error gates

## Installation

### Easy Install (Recommended)

\`\`\`bash
# Download and verify
wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2-1_amd64.deb
wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2-1_amd64.deb.sha256

# Verify checksum
sha256sum -c ggen_5.0.2-1_amd64.deb.sha256

# Install
sudo apt install ./ggen_5.0.2-1_amd64.deb

# Verify installation
ggen --help
\`\`\`

### Using Install Script (Recommended for Automation)

\`\`\`bash
curl -fSL https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2-1_amd64.deb -o /tmp/ggen.deb
bash scripts/install-ggen.sh \"\" /tmp/ggen.deb
\`\`\`

## What's New

- Full SPARQL CONSTRUCT support for graph enrichment
- Type-safe Tera template rendering with SPARQL variables
- Comprehensive SHACL shape validation before generation
- Fail-proof .deb building with 12 poka-yoke gates
- Idempotent installation script with rollback capability
- SHA256 checksum verification on all artifacts
"
```

**Expected output**:
```
✓ Created release v5.0.2
  3 assets uploaded: ggen_5.0.2-1_amd64.deb, ggen_5.0.2-1_amd64.deb.sha256, ggen.sha256
```

---

### Phase 3: User Installation (Poka-Yoke Protected)

Users can install using one of three methods:

#### Method 1: Direct apt install (Simplest)

```bash
# Download
wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2-1_amd64.deb

# Verify checksum (optional but recommended)
wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2-1_amd64.deb.sha256
sha256sum -c ggen_5.0.2-1_amd64.deb.sha256

# Install
sudo apt install ./ggen_5.0.2-1_amd64.deb

# Verify
ggen --help
```

#### Method 2: Using install script (Recommended for CI/CD)

```bash
# Download installer
wget https://github.com/seanchatmangpt/ggen/raw/main/scripts/install-ggen.sh

# Install (all poka-yoke gates run automatically)
bash install-ggen.sh "" /path/to/ggen_5.0.2-1_amd64.deb

# Or install from URL
bash install-ggen.sh https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2-1_amd64.deb
```

#### Method 3: From source (Development)

```bash
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen
cargo build --release -p ggen-cli-lib --bin ggen
sudo install target/release/ggen /usr/local/bin/
```

---

## Safety & Verification

### Checksum Verification (Always Recommended)

**Why checksums matter**: Detect corrupted downloads or tampering

```bash
# Download both files
wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2-1_amd64.deb
wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2-1_amd64.deb.sha256

# Verify
sha256sum -c ggen_5.0.2-1_amd64.deb.sha256

# Expected output
# ggen_5.0.2-1_amd64.deb: OK
```

**If verification fails**:
- ❌ Do NOT install
- Download fresh copies
- Check network for interference
- Report issue on GitHub

### Installation Script Gates (Automated Safety)

The `install-ggen.sh` script includes 12 built-in poka-yoke gates:

| Gate | Protection |
|------|-----------|
| 1 | Permission check (requires sudo or root) |
| 2 | System requirements (Linux + dpkg) |
| 3 | Input validation (.deb file exists) |
| 4 | Existing installation detection |
| 5 | Download verification (if URL) |
| 6 | Checksum verification (SHA256) |
| 7 | Package format validation (dpkg -I) |
| 8 | Existing binary backup |
| 9 | Installation via dpkg |
| 10 | Installation verification |
| 11 | Functionality test (ggen --help) |
| 12 | Cleanup and rollback on error |

**Rollback capability**: If any gate fails, script automatically restores previous version

---

## Troubleshooting

### Build Script Failures

**If `bash scripts/build-debian-package.sh` fails:**

| Error | Solution |
|-------|----------|
| `cargo not found` | Install Rust: `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \| sh` |
| `debuild not found` | Install: `sudo apt-get install devscripts` |
| `sha256sum not found` | Install: `sudo apt-get install coreutils` |
| `ggen.toml not found` | Ensure running from repository root: `cd /home/user/ggen` |
| `debian/ not found` | Clone full repository, don't use sparse checkout |
| `Binary suspiciously small` | Rebuild: `cargo clean && cargo build --release -p ggen-cli-lib --bin ggen` |
| `debuild fails silently` | Check `/tmp/debuild.log` for details |

### Installation Script Failures

**If `bash scripts/install-ggen.sh` fails:**

| Error | Solution |
|-------|----------|
| `Permission denied` | Use sudo or run as root: `sudo bash scripts/install-ggen.sh ...` |
| `dpkg not found` | Use on Debian/Ubuntu only: `uname -s` should print "Linux" |
| `.deb checksum verification failed` | Download fresh copy from GitHub Release |
| `.deb package is invalid` | Verify download completed fully: `ls -lh ggen.sha256` |
| `Installation failed` | Check `/tmp/ggen_*` for logs, review `/var/log/apt/` |
| `Rollback failed` | Manual recovery: `sudo /usr/bin/ggen.backup.[timestamp]` → `/usr/bin/ggen` |

### Verification After Install

```bash
# Check installation path
which ggen  # Should print /usr/bin/ggen

# Check version
ggen --version  # Should print version info

# Test functionality
ggen help | head -5  # Should show help text

# Full test
ggen sync --dry-run --validate-only  # Should complete without error
```

---

## Maintenance & Future Releases

### Releasing v5.0.3 (or any future version)

1. **Update version in files**:
   ```bash
   # Update crates/ggen-cli-lib/Cargo.toml
   # Update debian/changelog
   # Update scripts/ (if needed)
   ```

2. **Commit and tag**:
   ```bash
   git add -A
   git commit -m "chore: Bump version to 5.0.3"
   git tag v5.0.3
   git push origin main
   git push origin v5.0.3
   ```

3. **Run build script**:
   ```bash
   bash scripts/build-debian-package.sh
   ```

4. **Create release**:
   ```bash
   gh release create v5.0.3 ../ggen_5.0.3-1_amd64.deb --notes "Release notes here"
   ```

### Automating with GitHub Actions (Recommended)

See [GitHub Actions CI/CD](#github-actions-ci-cd) section below.

---

## GitHub Actions CI/CD (Optional, Recommended for Automation)

To automate build + release on every push to `main`:

**File**: `.github/workflows/release-debian.yml`

```yaml
name: Build and Release Debian Package

on:
  push:
    tags:
      - 'v*'

jobs:
  build:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Install Rust
        uses: dtolnay/rust-toolchain@stable

      - name: Install build dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y devscripts debhelper

      - name: Build Debian package
        run: |
          bash scripts/build-debian-package.sh

      - name: Upload release assets
        uses: softprops/action-gh-release@v1
        with:
          files: |
            ../ggen_*.deb*
            ggen.sha256
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

**Usage**:
```bash
git tag v5.0.3
git push origin v5.0.3
# GitHub Actions automatically builds and creates release
```

---

## Release Checklist

Before publishing a release, verify:

- [ ] `bash scripts/build-debian-package.sh` exits with code 0
- [ ] All 12 gates show ✓ (green checkmarks)
- [ ] `.deb` file exists: `ls -lh ../ggen_5.0.2-1_amd64.deb`
- [ ] Checksums exist: `ls -lh ../ggen_5.0.2-1_amd64.deb.sha256 ggen.sha256`
- [ ] Test installation: `bash scripts/install-ggen.sh "" ../ggen_5.0.2-1_amd64.deb`
- [ ] Verify installed binary: `ggen --help` works
- [ ] Checksum verification passes: `sha256sum -c ggen_5.0.2-1_amd64.deb.sha256`

---

## Philosophy: Poka-Yoke Fail-Proof Design

All scripts implement Toyota's **Poka-Yoke** (error-proofing) principles:

1. **Detect Errors Early**: Every gate validates before proceeding
2. **Fail Fast**: Exit immediately on any error (no cascading failures)
3. **Clear Feedback**: Color-coded output (✓ success, ✗ failure, ⚠ warning)
4. **No Recovery Burden**: Scripts automatically rollback or provide instructions
5. **Deterministic**: Same inputs always produce same outputs (reproducible)
6. **Idempotent**: Can re-run safely without side effects

**Design principle**: _"Make it impossible to fail silently"_

---

## References

- [Debian Maintainers Guide](https://www.debian.org/doc/manuals/maint-guide/)
- [GitHub Releases Documentation](https://docs.github.com/en/repositories/releasing-projects-on-github/managing-releases-in-a-repository)
- [Debian Policy Manual](https://www.debian.org/doc/debian-policy/)
- [Poka-Yoke (Error-Proofing) Wikipedia](https://en.wikipedia.org/wiki/Poka-yoke)

