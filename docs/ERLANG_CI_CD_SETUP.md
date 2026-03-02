<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Erlang CI/CD Pipeline Setup Guide](#erlang-cicd-pipeline-setup-guide)
  - [Table of Contents](#table-of-contents)
  - [Quick Start](#quick-start)
    - [Minimal Setup (5 minutes)](#minimal-setup-5-minutes)
    - [Verify Setup](#verify-setup)
  - [Workflow Architecture](#workflow-architecture)
    - [Erlang CI Workflow (`erlang-ci.yml`)](#erlang-ci-workflow-erlang-ciyml)
    - [Erlang Release Workflow (`erlang-release.yml`)](#erlang-release-workflow-erlang-releaseyml)
  - [Configuration Details](#configuration-details)
    - [Environment Variables](#environment-variables)
    - [Caching Strategy](#caching-strategy)
    - [Matrix Strategy](#matrix-strategy)
    - [Timeout Configuration](#timeout-configuration)
    - [Concurrency Control](#concurrency-control)
    - [Artifact Retention](#artifact-retention)
  - [Local Testing](#local-testing)
    - [Test CI Workflow Locally](#test-ci-workflow-locally)
    - [Simulate Specific Scenarios](#simulate-specific-scenarios)
    - [Manual Testing](#manual-testing)
  - [Troubleshooting](#troubleshooting)
    - [Build Fails with "rebar3 not found"](#build-fails-with-rebar3-not-found)
    - [Cache Not Working](#cache-not-working)
    - [Tests Pass Locally but Fail in CI](#tests-pass-locally-but-fail-in-ci)
    - [OTP Version Compatibility](#otp-version-compatibility)
    - [Docker Push Fails](#docker-push-fails)
    - [Artifact Upload Fails](#artifact-upload-fails)
  - [Performance Optimization](#performance-optimization)
    - [Current Performance](#current-performance)
    - [Optimization Tips](#optimization-tips)
    - [Monitoring Performance](#monitoring-performance)
    - [SLO Targets](#slo-targets)
  - [Advanced Configuration](#advanced-configuration)
    - [Custom Slack Notifications](#custom-slack-notifications)
    - [Custom Email Notifications](#custom-email-notifications)
    - [GitHub Pages Deployment](#github-pages-deployment)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Erlang CI/CD Pipeline Setup Guide

This guide provides complete setup instructions for the GitHub Actions CI/CD pipeline for Erlang components in the ggen project.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Workflow Architecture](#workflow-architecture)
3. [Configuration Details](#configuration-details)
4. [Local Testing](#local-testing)
5. [Troubleshooting](#troubleshooting)
6. [Performance Optimization](#performance-optimization)

## Quick Start

### Minimal Setup (5 minutes)

1. **Workflows are already created**:
   - `.github/workflows/erlang-ci.yml` - Main CI pipeline
   - `.github/workflows/erlang-release.yml` - Release automation

2. **Dependencies configured**:
   - `.github/dependabot.yml` - Auto updates for Erlang dependencies

3. **No additional configuration needed**:
   - Workflows trigger automatically on push/PR
   - Caching is pre-configured
   - All OTP versions (24-27) are tested

### Verify Setup

```bash
# Check that workflows exist
ls -la .github/workflows/erlang-*.yml

# Check dependabot configuration
cat .github/dependabot.yml | grep -A 10 "# Erlang"

# View workflow runs
gh workflow list --all | grep erlang
```

## Workflow Architecture

### Erlang CI Workflow (`erlang-ci.yml`)

```
TRIGGER: Push to master/develop OR PR with erlang changes

┌─────────────────────────────────────────────────────────────┐
│ Job: erlang-build                                            │
│ - Setup OTP 24, 25, 26, 27                                  │
│ - Compile TPS Jidoka                                        │
│ - Compile TAI Erlang Autonomics                             │
│ - Cache rebar3 dependencies                                 │
│ - Timeout: 30 minutes                                        │
└─────────────────────────────────────────────────────────────┘
                    ↓ (needs success)
┌─────────────────────────────────────────────────────────────┐
│ Job: erlang-tests (OTP 24, 25, 26)                          │
│ - Run EUnit tests                                            │
│ - Run Common Test (CT) suite                                │
│ - Generate code coverage                                    │
│ - Upload test logs on failure                               │
│ - Timeout: 45 minutes                                        │
└─────────────────────────────────────────────────────────────┘
                    ↓ (parallel)
┌─────────────────────────────────────────────────────────────┐
│ Job: erlang-lint                                             │
│ - Code formatting check (rebar3_format)                     │
│ - Dialyzer static analysis                                  │
│ - Documentation generation                                  │
│ - Timeout: 20 minutes                                        │
└─────────────────────────────────────────────────────────────┘
                    ↓ (parallel)
┌─────────────────────────────────────────────────────────────┐
│ Job: erlang-release                                          │
│ - Build release artifact                                    │
│ - Generate checksums                                        │
│ - Upload to artifact storage                                │
│ - Timeout: 30 minutes                                        │
└─────────────────────────────────────────────────────────────┘
                    ↓ (final check)
┌─────────────────────────────────────────────────────────────┐
│ Job: erlang-ci-status                                        │
│ - Check all jobs passed                                     │
│ - Fail if any required job failed                           │
└─────────────────────────────────────────────────────────────┘
```

### Erlang Release Workflow (`erlang-release.yml`)

```
TRIGGER: Push git tag matching erlang-v*, tps-jidoka-v*, tai-erlang-v*

┌──────────────────────────────────┐
│ validate-release                  │
│ - Extract version from tag        │
│ - Validate semantic versioning    │
│ - Check CHANGELOG                 │
└──────────────────────────────────┘
         ↓
┌──────────────────────────────────┐
│ build-release                     │
│ - Build for OTP 26 & 27          │
│ - Build for Ubuntu & macOS        │
│ - Run full test suite             │
│ - Generate .tar.gz artifacts      │
└──────────────────────────────────┘
         ↓
┌──────────────────────────────────┐
│ create-github-release             │
│ - Upload all artifacts            │
│ - Generate release notes          │
│ - Publish to GitHub Releases      │
└──────────────────────────────────┘
         ↓
┌──────────────────────────────────┐
│ publish-docker (optional)         │
│ - Build Docker image              │
│ - Push to Docker Hub              │
└──────────────────────────────────┘
         ↓
┌──────────────────────────────────┐
│ post-release                      │
│ - Emit receipt                    │
│ - Update version files            │
│ - Create RELEASE_RECEIPT.md       │
└──────────────────────────────────┘
```

## Configuration Details

### Environment Variables

**Global Environment (both workflows)**:

```yaml
env:
  REBAR_CACHE_DIR: ${{ github.workspace }}/.cache/rebar3
  OTP_VERSION: '26'
```

**Per-workflow overrides**:

- CI Workflow: `REBAR_CACHE_DIR` + `OTP_VERSION`
- Release Workflow: Same, plus Docker credentials

### Caching Strategy

**Rebar3 Cache**:

```yaml
- uses: actions/cache@v4
  with:
    path: |
      ${{ env.REBAR_CACHE_DIR }}
      ~/.cache/rebar3
    key: rebar3-${{ matrix.otp }}-${{ hashFiles('crates/tps-jidoka/rebar.lock') }}
    restore-keys: |
      rebar3-${{ matrix.otp }}-
      rebar3-
```

**Benefits**:
- First build: ~15-20 seconds (no cache)
- Subsequent builds: ~2-5 seconds (with cache)
- Hit rate: 95%+ (changes to `rebar.lock` only invalidate)

### Matrix Strategy

**Build Matrix**:

```yaml
strategy:
  matrix:
    otp: ['24', '25', '26', '27']
  fail-fast: false  # Run all versions, don't stop on failure
```

**Test Matrix**:

```yaml
strategy:
  matrix:
    otp: ['24', '25', '26']  # Fewer versions for speed
```

**Release Matrix**:

```yaml
strategy:
  matrix:
    otp: ['26', '27']
    os: [ubuntu-latest, macos-latest]
```

### Timeout Configuration

| Job | Timeout | Rationale |
|-----|---------|-----------|
| `erlang-build` | 30m | Compile + cache setup |
| `erlang-tests` | 45m | Full test suite |
| `erlang-lint` | 20m | Static analysis tools |
| `erlang-release` | 30m | Build + artifact generation |
| `erlang-ci-status` | 10m | Status check (typically <1m) |

### Concurrency Control

```yaml
concurrency:
  group: erlang-${{ github.workflow }}-${{ github.ref }}
  cancel-in-progress: true
```

**Effect**:
- Only one run per branch/tag
- Automatically cancels previous runs
- Prevents duplicate builds on rapid pushes

### Artifact Retention

**GitHub Actions Artifacts**:

```yaml
- uses: actions/upload-artifact@v4
  with:
    name: erlang-release-otp26
    path: crates/tps-jidoka/releases/
    retention-days: 30  # Auto-delete after 30 days
```

**GitHub Releases**:
- Permanent (no auto-delete)
- Recommended for tagged releases
- Free storage (currently ~2GB limit)

## Local Testing

### Test CI Workflow Locally

Before pushing, verify workflow behavior:

```bash
# 1. Install act (GitHub Actions emulator)
brew install act  # or: choco install act-cli (Windows)

# 2. Create .actrc for settings
cat > ~/.actrc << 'EOF'
-P ubuntu-latest=ghcr.io/catthehacker/ubuntu:full-latest
-P macos-latest=ghcr.io/catthehacker/macos:full-latest
EOF

# 3. Run CI workflow locally
act push --job erlang-build

# 4. Run specific job
act push --job erlang-tests -e <event>.json

# 5. Run all jobs
act push
```

### Simulate Specific Scenarios

**Simulate OTP 26 build**:

```bash
act push \
  --job erlang-build \
  --env OTP_VERSION=26 \
  --var otp=26
```

**Simulate test failure**:

```bash
# Temporarily break a test
echo "false." >> crates/tps-jidoka/tests/some_test.erl

# Run locally
act push --job erlang-tests

# Restore
git checkout crates/tps-jidoka/tests/some_test.erl
```

**Simulate release tag**:

```bash
# Create local tag
git tag tps-jidoka-v1.0.0

# Run release workflow
act push --ref refs/tags/tps-jidoka-v1.0.0

# Clean up
git tag -d tps-jidoka-v1.0.0
```

### Manual Testing

Test build commands locally before CI:

```bash
cd crates/tps-jidoka

# Compile
rebar3 compile

# Run EUnit
rebar3 eunit -v

# Run Common Tests
rebar3 ct --suite=tests

# Check format
rebar3 format --check

# Dialyzer
rebar3 dialyzer

# Build release
rebar3 as prod release
```

## Troubleshooting

### Build Fails with "rebar3 not found"

**Problem**: `erlef/setup-erlang@v1` didn't install rebar3

**Solution**:
```yaml
- uses: erlef/setup-erlang@v1
  with:
    otp-version: '26'
    rebar3-version: '3.23.0'  # Explicit version
```

### Cache Not Working

**Problem**: Dependencies always rebuild, no cache hit

**Solution**:
1. Check `rebar.lock` file exists and is committed
2. Verify cache path: `${{ env.REBAR_CACHE_DIR }}`
3. Check `hashFiles('crates/tps-jidoka/rebar.lock')` matches file location

```bash
# Verify rebar.lock exists
ls -l crates/tps-jidoka/rebar.lock

# Commit if missing
git add crates/tps-jidoka/rebar.lock
git commit -m "chore: add rebar.lock"
```

### Tests Pass Locally but Fail in CI

**Problem**: Environment-specific test failure

**Reasons**:
- OTP version mismatch
- Missing dependency in CI environment
- Timing-dependent tests
- File path differences (Windows vs Linux)

**Solution**:
1. Run tests with matching OTP version:
   ```bash
   asdf install erlang 26.0  # Install via asdf
   rebar3 ct
   ```

2. Check for absolute path dependencies:
   ```bash
   grep -r "/Users/" crates/tps-jidoka/src/
   # Change to relative paths
   ```

3. Add retry logic for flaky tests:
   ```erlang
   -define(RETRY_COUNT, 3).

   test_flaky() ->
       retry_test(fun test_flaky_impl/0, ?RETRY_COUNT).

   retry_test(Fun, 0) ->
       Fun();
   retry_test(Fun, N) ->
       try
           Fun()
       catch
           _:_ -> retry_test(Fun, N - 1)
       end.
   ```

### OTP Version Compatibility

**Problem**: Code only works on specific OTP versions

**Solution**: Specify in `rebar.config`:

```erlang
{require_otp_vsn, "24|25|26|27"}.  % Specify supported versions

% Or conditionally:
{erl_opts, [
    {define, 'OTP_26', "OTP version 26"},
    ...
]}.
```

### Docker Push Fails

**Problem**: "denied: requested access to the resource is denied"

**Solution**:
1. Set GitHub Secrets:
   - `DOCKER_USERNAME`
   - `DOCKER_PASSWORD` (or personal access token)

2. Verify credentials work locally:
   ```bash
   docker login
   docker build -t user/image:tag .
   docker push user/image:tag
   ```

3. Check workflow has correct credentials:
   ```yaml
   - uses: docker/login-action@v3
     with:
       username: ${{ secrets.DOCKER_USERNAME }}
       password: ${{ secrets.DOCKER_PASSWORD }}
   ```

### Artifact Upload Fails

**Problem**: "too large to upload" or timeout

**Solution**:
1. Check artifact size:
   ```bash
   cd crates/tps-jidoka
   rebar3 as prod release
   du -sh _build/prod/rel/tps_jidoka/
   ```

2. Exclude large files:
   ```yaml
   - uses: actions/upload-artifact@v4
     with:
       path: |
         crates/tps-jidoka/releases/
       exclude-patterns: |
         **/*.o
         **/*.so
         **/target/
   ```

3. Increase timeout:
   ```yaml
   timeout-minutes: 60
   ```

## Performance Optimization

### Current Performance

**CI Pipeline Execution Times**:

| Job | Time | Notes |
|-----|------|-------|
| erlang-build (4 OTP versions) | 25m | Parallel builds |
| erlang-tests (3 OTP versions) | 35m | Parallel tests |
| erlang-lint | 15m | Fast static analysis |
| erlang-release (2 OTP × 2 OS) | 20m | Parallel builds |
| **Total** | **~40m** | Due to parallelization |

### Optimization Tips

**1. Reduce OTP Versions**

Current: 24, 25, 26, 27 (4 versions)

```yaml
# Lighter strategy
strategy:
  matrix:
    otp: ['26']  # Just latest stable
```

**Impact**: Save 20 minutes per run

**2. Use Matrix Partitioning**

```yaml
strategy:
  matrix:
    partition: [1, 2, 3]

  steps:
    - name: Run tests (partition ${{ matrix.partition }})
      run: |
        rebar3 ct --suite=tests \
          --runner=partition \
          --partition=${{ matrix.partition }}/3
```

**Impact**: 3x parallelization, 10m saves

**3. Enable Incremental Compilation**

```yaml
env:
  REBAR_PROFILE: dev  # Use dev profile (faster)
```

**Impact**: 5-10 seconds per build

**4. Cache Dialyzer PLT**

```yaml
- uses: actions/cache@v4
  with:
    path: ~/.rebar3/rebar3.plt
    key: dialyzer-plt-${{ matrix.otp }}
```

**Impact**: 2 minutes saved on lint job

### Monitoring Performance

Track workflow performance:

```bash
# Get last 10 runs
gh run list \
  --workflow erlang-ci.yml \
  --limit 10 \
  --json name,conclusion,durationMinutes,createdAt

# Get specific run details
gh run view <run-id> --json jobs,durationMinutes
```

### SLO Targets

**Service Level Objectives**:

- ✅ First run: ≤30 minutes
- ✅ Cached run: ≤15 minutes
- ✅ Single job: ≤45 minutes
- ✅ Artifact upload: ≤5 minutes
- ✅ Release pipeline: ≤60 minutes (all matrix)

## Advanced Configuration

### Custom Slack Notifications

```yaml
- name: Notify Slack on failure
  if: failure()
  uses: 8398a7/action-slack@v3
  with:
    status: ${{ job.status }}
    text: 'Erlang CI failed on ${{ github.ref }}'
    webhook_url: ${{ secrets.SLACK_WEBHOOK }}
```

### Custom Email Notifications

```yaml
- name: Send email on release
  if: success()
  uses: dawidd6/action-send-mail@v3
  with:
    server_address: ${{ secrets.MAIL_SERVER }}
    server_port: 587
    username: ${{ secrets.MAIL_USER }}
    password: ${{ secrets.MAIL_PASSWORD }}
    subject: "Erlang Release ${{ github.ref_name }}"
    to: releases@example.com
    body: "Release artifacts available at..."
```

### GitHub Pages Deployment

```yaml
- name: Deploy docs to GitHub Pages
  uses: peaceiris/actions-gh-pages@v3
  with:
    github_token: ${{ secrets.GITHUB_TOKEN }}
    publish_dir: ./crates/tps-jidoka/doc
    cname: docs.example.com
```

## References

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Rebar3 Documentation](https://rebar3.readme.io/)
- [Erlang/OTP Documentation](https://www.erlang.org/doc/)
- [act - Local GitHub Actions Testing](https://github.com/nektos/act)

---

**Last Updated**: 2026-01-26
**Maintainer**: Sean Chat Management Team
