<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Erlang Release Process](#erlang-release-process)
  - [Table of Contents](#table-of-contents)
  - [Release Workflow](#release-workflow)
    - [Automatic CI/CD Pipeline](#automatic-cicd-pipeline)
  - [Versioning Strategy](#versioning-strategy)
    - [Semantic Versioning](#semantic-versioning)
    - [Examples](#examples)
  - [Creating a Release](#creating-a-release)
    - [Step 1: Prepare the Release](#step-1-prepare-the-release)
    - [Step 2: Tag the Release](#step-2-tag-the-release)
    - [Step 3: Monitor CI/CD Pipeline](#step-3-monitor-cicd-pipeline)
    - [Step 4: Verify Release](#step-4-verify-release)
  - [Verification Steps](#verification-steps)
    - [Pre-Release Verification](#pre-release-verification)
    - [Post-Release Verification](#post-release-verification)
  - [Artifact Management](#artifact-management)
    - [Artifact Naming Convention](#artifact-naming-convention)
    - [Artifact Retention](#artifact-retention)
    - [Artifact Contents](#artifact-contents)
  - [Release Receipt](#release-receipt)
  - [Rollback Procedures](#rollback-procedures)
    - [If Release Has Issues](#if-release-has-issues)
      - [Option 1: Delete the GitHub Release (Recommended for Critical Issues)](#option-1-delete-the-github-release-recommended-for-critical-issues)
      - [Option 2: Mark as Pre-Release (For Minor Issues)](#option-2-mark-as-pre-release-for-minor-issues)
      - [Option 3: Create a Patch Release (For Bug Fixes)](#option-3-create-a-patch-release-for-bug-fixes)
    - [Rollback in Production](#rollback-in-production)
  - [Troubleshooting](#troubleshooting)
    - [Build Failures](#build-failures)
    - [Test Failures](#test-failures)
    - [Docker Push Failures](#docker-push-failures)
    - [Checksum Verification Fails](#checksum-verification-fails)
  - [Branch Protection Rules](#branch-protection-rules)
  - [FAQ](#faq)
  - [Contact & Support](#contact--support)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Erlang Release Process

This document describes the release process for Erlang components (TPS Jidoka, TAI Erlang Autonomics) in the ggen project using GitHub Actions CI/CD.

## Table of Contents

1. [Release Workflow](#release-workflow)
2. [Versioning Strategy](#versioning-strategy)
3. [Creating a Release](#creating-a-release)
4. [Verification Steps](#verification-steps)
5. [Artifact Management](#artifact-management)
6. [Rollback Procedures](#rollback-procedures)

## Release Workflow

### Automatic CI/CD Pipeline

The release process is fully automated using GitHub Actions:

```
┌─────────────────────────────────────────────────────────────┐
│ 1. Validate Release                                          │
│    - Extract version from git tag                            │
│    - Validate semantic versioning (X.Y.Z)                    │
│    - Check CHANGELOG entries                                 │
└──────────────────┬──────────────────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────────────────┐
│ 2. Build Release (Matrix Strategy)                           │
│    - OTP 24, 25, 26, 27                                      │
│    - Ubuntu Linux + macOS                                    │
│    - Run full test suite (EUnit + Common Test)               │
│    - Generate release artifacts (.tar.gz)                    │
│    - Create SHA-256 checksums                                │
└──────────────────┬──────────────────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────────────────┐
│ 3. Create GitHub Release                                     │
│    - Upload all artifacts                                    │
│    - Generate release notes                                  │
│    - Create verification instructions                        │
│    - Publish to GitHub Releases                              │
└──────────────────┬──────────────────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────────────────┐
│ 4. Optional: Publish Docker Image                            │
│    - Build and push to Docker Hub                            │
│    - Tag with version and 'latest'                           │
│    - Cache build layers (2.8-4.4x speedup)                   │
└──────────────────┬──────────────────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────────────────┐
│ 5. Post-Release Tasks                                        │
│    - Emit cryptographic release receipt                      │
│    - Update version files                                    │
│    - Generate RELEASE_RECEIPT.md                             │
└─────────────────────────────────────────────────────────────┘
```

## Versioning Strategy

### Semantic Versioning

All releases follow **Semantic Versioning 2.0.0**:

```
MAJOR.MINOR.PATCH
```

- **MAJOR**: Breaking API changes, incompatible protocol changes
- **MINOR**: New features, backward compatible
- **PATCH**: Bug fixes, internal improvements

### Examples

```
tps-jidoka-v1.0.0         # Initial release
tps-jidoka-v1.1.0         # New features (backward compatible)
tps-jidoka-v1.1.1         # Bug fix
tps-jidoka-v2.0.0         # Major breaking changes

tai-erlang-v0.5.0         # Pre-release development
tai-erlang-v0.5.0-alpha.1 # Alpha release (prerelease)
tai-erlang-v0.5.0-beta.1  # Beta release (prerelease)
```

## Creating a Release

### Step 1: Prepare the Release

Before creating a release, ensure:

1. **All tests pass**:
   ```bash
   cd crates/tps-jidoka
   rebar3 eunit          # EUnit tests
   rebar3 ct             # Common Tests
   rebar3 dialyzer       # Static analysis
   ```

2. **Code is formatted**:
   ```bash
   rebar3 format         # Format code
   rebar3 format --check # Verify formatting
   ```

3. **CHANGELOG is updated**:
   ```markdown
   ## [1.2.3] - 2026-01-26

   ### Added
   - New circuit breaker implementation
   - Support for OTP 27

   ### Fixed
   - Memory leak in pool cleanup
   - Race condition in shutdown sequence

   ### Changed
   - Improved error messages

   ### Deprecated
   - Legacy API endpoints (use v2 endpoints instead)

   [1.2.3]: https://github.com/seanchatmangpt/ggen/releases/tag/tps-jidoka-v1.2.3
   ```

### Step 2: Tag the Release

Create a git tag following the naming convention:

```bash
# For TPS Jidoka
git tag -a tps-jidoka-v1.2.3 -m "TPS Jidoka release v1.2.3"
git push origin tps-jidoka-v1.2.3

# For TAI Erlang Autonomics
git tag -a tai-erlang-v1.2.3 -m "TAI Erlang Autonomics release v1.2.3"
git push origin tai-erlang-v1.2.3

# For comprehensive Erlang release
git tag -a erlang-v1.2.3 -m "Erlang subsystem release v1.2.3"
git push origin erlang-v1.2.3
```

### Step 3: Monitor CI/CD Pipeline

The CI/CD pipeline will automatically:

1. **Validate Release**:
   - Extract version from tag
   - Validate semantic versioning format
   - Check CHANGELOG entries

2. **Build Release**:
   - Compile for OTP 24, 25, 26, 27
   - Build on Ubuntu Linux and macOS
   - Run full test suite
   - Generate artifacts

3. **Create GitHub Release**:
   - Upload all artifacts
   - Generate release notes
   - Publish to GitHub Releases page

4. **Publish Docker Image** (if configured):
   - Build Docker image
   - Push to Docker Hub
   - Tag with version

### Step 4: Verify Release

Once the CI/CD pipeline completes:

```bash
# Visit the GitHub Releases page
https://github.com/seanchatmangpt/ggen/releases

# Download artifacts
wget https://github.com/seanchatmangpt/ggen/releases/download/tps-jidoka-v1.2.3/tps_jidoka-1.2.3-otp26-Linux.tar.gz
wget https://github.com/seanchatmangpt/ggen/releases/download/tps-jidoka-v1.2.3/tps_jidoka-1.2.3-otp26-Linux.tar.gz.sha256

# Verify checksum
sha256sum -c tps_jidoka-1.2.3-otp26-Linux.tar.gz.sha256
# Expected output: tps_jidoka-1.2.3-otp26-Linux.tar.gz: OK
```

## Verification Steps

### Pre-Release Verification

**Checklist before creating the tag:**

- [ ] All branches merged to master
- [ ] All CI checks passing
- [ ] CHANGELOG.md updated with new version
- [ ] Version number updated in `rebar.config`
- [ ] Git tag created with proper format
- [ ] All test suites passing locally

### Post-Release Verification

**Checklist after GitHub release is created:**

```bash
# 1. Verify artifact checksums
cd /tmp
wget https://github.com/seanchatmangpt/ggen/releases/download/tps-jidoka-v1.2.3/tps_jidoka-1.2.3-otp26-Linux.tar.gz{,.sha256}
sha256sum -c tps_jidoka-1.2.3-otp26-Linux.tar.gz.sha256

# 2. Extract and verify artifact
tar -tzf tps_jidoka-1.2.3-otp26-Linux.tar.gz | head -20

# 3. Test release startup (optional)
tar -xzf tps_jidoka-1.2.3-otp26-Linux.tar.gz
cd tps_jidoka
./bin/release_handler start

# 4. Verify version in running system
./bin/release_handler version
```

## Artifact Management

### Artifact Naming Convention

Artifacts follow this naming pattern:

```
{component}-{version}-otp{otp_version}-{os}.tar.gz
{component}-{version}-otp{otp_version}-{os}.tar.gz.sha256
```

Examples:
```
tps_jidoka-1.2.3-otp26-Linux.tar.gz
tps_jidoka-1.2.3-otp26-macOS.tar.gz
tps_jidoka-1.2.3-otp27-Linux.tar.gz
tps_jidoka-1.2.3-otp27-macOS.tar.gz
```

### Artifact Retention

- **GitHub Releases**: Permanent (90-day artifact retention in Actions)
- **Docker Hub**: Latest + version tag
- **Local Build Artifacts**: Deleted after release (CI/CD cleanup)

### Artifact Contents

Each `.tar.gz` contains:

```
tps_jidoka/
├── bin/
│   ├── release_handler     # Main executable
│   ├── ctl                 # Control script
│   └── erl                 # Erlang shell launcher
├── lib/
│   ├── tps_jidoka-1.2.3/   # Application BEAM files
│   ├── poolboy-1.5.2/      # Dependencies
│   ├── lager-3.9.2/
│   └── ...
├── config/
│   ├── sys.config          # System configuration
│   └── vm.args             # VM arguments
├── releases/
│   └── 1.2.3/              # Release metadata
└── RELEASE_NOTES.md        # This release's notes
```

## Release Receipt

After successful release, GitHub Actions generates a `RELEASE_RECEIPT.md`:

```markdown
# Release Receipt

**Component**: tps-jidoka
**Version**: 1.2.3
**Tag**: tps-jidoka-v1.2.3
**Timestamp**: 2026-01-26T15:30:45Z

## Build Matrix
- [x] OTP 26 - Linux (Ubuntu)
- [x] OTP 26 - macOS
- [x] OTP 27 - Linux (Ubuntu)
- [x] OTP 27 - macOS

## Artifacts Generated
- tps_jidoka-1.2.3-otp26-Linux.tar.gz
- tps_jidoka-1.2.3-otp26-macOS.tar.gz
- tps_jidoka-1.2.3-otp27-Linux.tar.gz
- tps_jidoka-1.2.3-otp27-macOS.tar.gz
- [checksums for each]

## Verification
All artifacts passed CI/CD validation:
- [x] Build compilation
- [x] EUnit tests (all OTP versions)
- [x] Common Tests (all OTP versions)
- [x] Code quality checks
- [x] SHA-256 checksum generation

Release is production-ready.
```

## Rollback Procedures

### If Release Has Issues

#### Option 1: Delete the GitHub Release (Recommended for Critical Issues)

```bash
# Delete the git tag locally
git tag -d tps-jidoka-v1.2.3

# Delete the remote tag
git push origin --delete tps-jidoka-v1.2.3

# Delete the GitHub Release via web UI or:
gh release delete tps-jidoka-v1.2.3 --yes
```

#### Option 2: Mark as Pre-Release (For Minor Issues)

1. Visit GitHub Releases page
2. Edit the release
3. Check "Set as a pre-release" checkbox
4. Update release notes with issue description

#### Option 3: Create a Patch Release (For Bug Fixes)

```bash
# Create a new release with bug fix
git tag -a tps-jidoka-v1.2.4 -m "TPS Jidoka release v1.2.4 - fixes critical bug in v1.2.3"
git push origin tps-jidoka-v1.2.4

# Update CHANGELOG.md
# Commit and push changes
```

### Rollback in Production

If deployed release has critical issues:

```bash
# Stop current release
./bin/release_handler stop

# Restore previous version
tar -xzf tps_jidoka-1.2.2-otp26-Linux.tar.gz
cd tps_jidoka
./bin/release_handler start

# Verify
./bin/release_handler version
```

## Troubleshooting

### Build Failures

**Issue**: CI build fails for specific OTP version

**Solution**:
1. Check OTP compatibility in `rebar.config`
2. Verify dependencies support OTP version
3. Update `rebar.lock` with compatible versions
4. Retry release after fixing issues

### Test Failures

**Issue**: Tests fail during CI release

**Solution**:
1. Fix tests locally: `rebar3 eunit`
2. Ensure all tests pass: `rebar3 ct`
3. Create new tag after fixes
4. Push to trigger new CI run

### Docker Push Failures

**Issue**: Docker image push fails

**Solution**:
1. Verify Docker Hub credentials in GitHub Secrets
2. Check Docker build logs in Actions output
3. Manually push if automatic push fails:
   ```bash
   docker login
   docker build -t myusername/tps-jidoka:1.2.3 .
   docker push myusername/tps-jidoka:1.2.3
   ```

### Checksum Verification Fails

**Issue**: SHA-256 checksum mismatch

**Solution**:
1. Re-download artifact
2. Check file not corrupted: `ls -lh *.tar.gz*`
3. Verify using original checksum: `sha256sum -c *.sha256`
4. If still fails, contact maintainers

## Branch Protection Rules

The repository enforces these branch protection rules:

- ✅ Require CI to pass before merge
- ✅ Require 1 approval on pull requests
- ✅ Dismiss stale reviews on new commits
- ✅ Require signed commits (optional)
- ✅ Allow force pushes to release branches only

## FAQ

**Q: How often should we release?**
A: Release when features are ready or bugs are fixed. Typically weekly for active development, monthly for stable releases.

**Q: Can I release multiple components at once?**
A: Yes. Create separate tags for each component (e.g., `tps-jidoka-v1.2.3` and `tai-erlang-v1.2.3`). They'll be released independently.

**Q: What if I need to release multiple patches quickly?**
A: Create release tags in sequence. Each tag triggers a new CI run. No need to wait between releases.

**Q: How do I handle pre-releases (alpha, beta)?**
A: Use semantic versioning: `tps-jidoka-v1.2.3-alpha.1`, `tps-jidoka-v1.2.3-beta.1`. The GitHub Actions workflow will mark these as pre-releases.

**Q: Can I automate Docker image building?**
A: Yes. Set `DOCKER_USERNAME` and `DOCKER_PASSWORD` secrets in GitHub, and push to Docker Hub will be automatic.

## Contact & Support

- **Issues**: [GitHub Issues](https://github.com/seanchatmangpt/ggen/issues)
- **Discussions**: [GitHub Discussions](https://github.com/seanchatmangpt/ggen/discussions)
- **Maintainer**: Sean Chat Management Team
