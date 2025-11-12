# Release v2.5.1 - Homebrew Binary Distribution Test

## Tag Pushed ✅

Tag `v2.5.1` has been pushed to GitHub. The release workflow will:

1. **Build binaries** for all 4 platforms:
   - macOS ARM64 (aarch64-apple-darwin)
   - macOS x86_64 (x86_64-apple-darwin)
   - Linux ARM64 (aarch64-unknown-linux-gnu)
   - Linux x86_64 (x86_64-unknown-linux-gnu)

2. **Create release archives** (.tar.gz) with SHA256 checksums

3. **Upload to GitHub Releases** automatically

## Monitor Progress

**GitHub Actions**: https://github.com/seanchatmangpt/ggen/actions

**Release Page**: https://github.com/seanchatmangpt/ggen/releases/tag/v2.5.1

## Verify Release (after ~10-15 minutes)

Run the test script:
```bash
./scripts/test-release-process.sh
```

Or check manually:
```bash
# Check if release exists
curl -s "https://api.github.com/repos/seanchatmangpt/ggen/releases/tags/v2.5.1" | grep tag_name

# Check if binaries are ready
curl -I "https://github.com/seanchatmangpt/ggen/releases/download/v2.5.1/ggen-aarch64-apple-darwin.tar.gz"
```

## Update Homebrew Formula

Once binaries are ready, update the Homebrew formula:

```bash
./scripts/update-homebrew-formula.sh 2.5.1
```

This will:
1. Download SHA256 checksums from GitHub release
2. Update the formula in `homebrew-tap` repository
3. Push the changes

## Test Homebrew Installation

After the formula is updated, users can install with:

```bash
brew install seanchatmangpt/tap/ggen
```

This will:
- ✅ Download prebuilt binary (no Rust required!)
- ✅ Install directly to `/opt/homebrew/bin/ggen` (or `/usr/local/bin/ggen`)
- ✅ Generate shell completions

## Expected Timeline

- **Build time**: ~10-15 minutes (4 parallel builds)
- **Release creation**: Automatic after builds complete
- **Homebrew update**: Manual (run script after release is ready)

## Verification Checklist

- [ ] All 4 binaries built successfully
- [ ] Release created on GitHub
- [ ] SHA256 checksums available
- [ ] Homebrew formula updated
- [ ] Test installation: `brew install seanchatmangpt/tap/ggen`
- [ ] Verify binary works: `ggen --version`



