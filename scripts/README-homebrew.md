# Homebrew Formula Update Scripts

## Overview

**Script-based only** - No GitHub Actions dependency. All updates and validation happen locally via scripts.

The scripts provide an end-to-end solution for updating the Homebrew formula to match the crate version. They ensure:

- ✅ **Version Matching**: Homebrew formula version always matches crate version
- ✅ **Static Binaries Only**: Uses pre-built static binaries (no compilation required)
- ✅ **Local Testing**: Tests formula syntax and installation locally (on macOS)
- ✅ **Submodule Integration**: Uses `vendors/homebrew-ggen` submodule for version tracking

## Prerequisites

1. **Initialize the submodule** (first time only):
   ```bash
   git submodule update --init --recursive vendors/homebrew-ggen
   ```

2. **GitHub Release**: The release must exist with static binaries for all platforms:
   - `ggen-aarch64-apple-darwin.tar.gz` (macOS ARM64)
   - `ggen-x86_64-apple-darwin.tar.gz` (macOS x86_64)
   - `ggen-aarch64-unknown-linux-gnu.tar.gz` (Linux ARM64)
   - `ggen-x86_64-unknown-linux-gnu.tar.gz` (Linux x86_64)
   - SHA256 checksum files for each binary

3. **Git Access**: Write access to `seanchatmangpt/homebrew-ggen` repository

## Quick Start

**One command to update and validate:**

```bash
./scripts/update-and-validate-homebrew.sh
```

This will:
1. Initialize the submodule if needed
2. Update the Homebrew formula to match crate version
3. Validate the formula locally

## Usage

### Basic Usage

Update Homebrew formula to match current crate version:

```bash
./scripts/update-homebrew-formula-e2e.sh
```

### Specify Version

Update to a specific version:

```bash
./scripts/update-homebrew-formula-e2e.sh --version 5.1.0
```

### Dry Run

Preview changes without making them:

```bash
./scripts/update-homebrew-formula-e2e.sh --dry-run
```

### Skip Testing

Skip local formula testing (useful in CI):

```bash
./scripts/update-homebrew-formula-e2e.sh --skip-test
```

### Skip Push

Make changes locally without pushing to remote:

```bash
./scripts/update-homebrew-formula-e2e.sh --skip-push
```

### Combined Options

```bash
./scripts/update-homebrew-formula-e2e.sh --version 5.1.0 --dry-run
```

## What the Script Does

1. **Verifies Submodule**: Checks that `vendors/homebrew-ggen` exists and initializes if needed
2. **Version Matching**: Ensures requested version matches `Cargo.toml` version
3. **Release Verification**: Checks that all required release assets exist
4. **SHA256 Checksums**: Downloads SHA256 checksums for all platform binaries
5. **Formula Generation**: Generates Homebrew formula with static binary URLs
6. **Local Testing**: Tests formula syntax and installation (macOS only)
7. **Commit Changes**: Commits formula update to submodule
8. **Push to Remote**: Pushes changes to `homebrew-ggen` repository
9. **Update Submodule Reference**: Updates main repo's submodule reference

## Formula Structure

The generated formula:

- Uses **static binaries only** (no compilation)
- Supports macOS (ARM64 and x86_64) and Linux (ARM64 and x86_64)
- Includes shell completion generation
- Includes basic version test

Example formula structure:

```ruby
class Ggen < Formula
  desc "Language-agnostic, graph-aware generator for reproducible projections"
  homepage "https://github.com/seanchatmangpt/ggen"
  version "5.1.0"
  license "MIT"
  
  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/seanchatmangpt/ggen/releases/download/v5.1.0/ggen-aarch64-apple-darwin.tar.gz"
      sha256 "..."
    else
      url "https://github.com/seanchatmangpt/ggen/releases/download/v5.1.0/ggen-x86_64-apple-darwin.tar.gz"
      sha256 "..."
    end
  end
  
  # ... Linux support ...
  
  def install
    bin.install "ggen"
    generate_completions_from_executable(bin/"ggen", "completion")
  end
  
  test do
    assert_match "ggen", shell_output("#{bin}/ggen --version")
  end
end
```

## Installation After Update

After the formula is updated and pushed, users can install with:

```bash
brew tap seanchatmangpt/ggen
brew install ggen
```

### Validate Formula Locally

After updating, validate the formula:

```bash
./scripts/validate-homebrew-formula.sh
```

This tests:
- Formula syntax
- Installation dry-run
- Version matching
- Static binary verification
- Platform support
- SHA256 checksums

## CI/CD Integration (Optional)

The script can be integrated into GitHub Actions workflows, but it's designed to work standalone. The workflow at `.github/workflows/homebrew-release.yml` can use this script, but it's not required.

## Troubleshooting

### Submodule Not Found

If you see "Homebrew submodule not found", initialize it:

```bash
git submodule update --init --recursive vendors/homebrew-ggen
```

### Version Mismatch

If you see a version mismatch error, ensure:
- The version in `Cargo.toml` matches the requested version
- You're running the script from the repository root

### Push Failed

If push fails, check:
- You have write access to `seanchatmangpt/homebrew-ggen`
- Git credentials are configured (or `GITHUB_TOKEN` is set)
- Remote branch is up to date

### Release Assets Missing

If release assets are missing:
- Ensure the GitHub release exists
- Verify all binary files are uploaded
- Check that SHA256 checksum files are present

## Related Files

- `.gitmodules`: Submodule configuration
- `vendors/homebrew-ggen/Formula/ggen.rb`: Homebrew formula file
- `.github/workflows/homebrew-release.yml`: CI/CD workflow
- `scripts/update-homebrew-formula.sh`: Legacy script (uses homebrew-tap)

