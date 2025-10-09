# Release Process

This document describes the automated release process for ggen, including Homebrew formula updates.

## Quick Start

### Automated Release (Recommended)

```bash
# 1. Update version in Cargo.toml
# 2. Run the complete release workflow
cargo make release-brew
```

This command will:
1. Run CI checks (format, lint, test, docs)
2. Create and push git tag
3. Wait for GitHub Actions to build binaries
4. Automatically update Homebrew formula with SHA256 checksums
5. Push updated formula to homebrew-tap repository

### Manual Release

If you prefer more control over the process:

```bash
# 1. Update version in Cargo.toml

# 2. Run CI checks
cargo make ci

# 3. Create and push tag
VERSION=$(grep '^version = ' Cargo.toml | head -1 | sed 's/version = "\(.*\)"/\1/')
git tag "v${VERSION}"
git push origin "v${VERSION}"

# 4. Wait for GitHub Actions to complete (~5-10 minutes)
# Monitor at: https://github.com/seanchatmangpt/ggen/actions

# 5. Check if release is ready
cargo make release-check

# 6. Update Homebrew formula
cargo make brew-update-formula
```

## Available Commands

### `cargo make release-brew`
Complete automated release workflow:
- Runs CI checks
- Creates git tag
- Waits for builds
- Updates Homebrew formula

### `cargo make release-check [VERSION]`
Check if release artifacts are ready:
```bash
cargo make release-check          # Check current Cargo.toml version
cargo make release-check 0.2.1    # Check specific version
```

### `cargo make brew-update-formula [VERSION]`
Update Homebrew formula with SHA256 checksums:
```bash
cargo make brew-update-formula          # Use current Cargo.toml version
cargo make brew-update-formula 0.2.1    # Update specific version
```

Alias: `cargo make brew-update`

### Manual Script

You can also run the update script directly:
```bash
./scripts/update-homebrew-formula.sh [VERSION]
```

## GitHub Actions Workflow

The release workflow (`.github/workflows/release.yml`) triggers on version tags:

1. **Create Release**: Creates GitHub release
2. **Build Binaries**: Builds for 4 platforms in parallel:
   - macOS ARM64 (Apple Silicon)
   - macOS x86_64 (Intel)
   - Linux ARM64
   - Linux x86_64
3. **Upload Assets**: Uploads binaries and SHA256 checksums

### Platforms Built

| Platform | Target Triple | Binary Name |
|----------|---------------|-------------|
| macOS ARM64 | aarch64-apple-darwin | ggen-aarch64-apple-darwin.tar.gz |
| macOS x86_64 | x86_64-apple-darwin | ggen-x86_64-apple-darwin.tar.gz |
| Linux ARM64 | aarch64-unknown-linux-gnu | ggen-aarch64-unknown-linux-gnu.tar.gz |
| Linux x86_64 | x86_64-unknown-linux-gnu | ggen-x86_64-unknown-linux-gnu.tar.gz |

## Homebrew Formula Update

The formula update process:

1. Downloads SHA256 checksums from GitHub release
2. Updates `Formula/ggen.rb` with:
   - New version number
   - SHA256 for each platform
   - Download URLs
3. Commits and pushes to `homebrew-tap` repository

### Formula Location

The Homebrew formula is maintained in a separate repository:
- **Repository**: https://github.com/seanchatmangpt/homebrew-tap
- **Formula File**: `Formula/ggen.rb`

Users install with:
```bash
brew install seanchatmangpt/tap/ggen
```

## Troubleshooting

### Release artifacts not ready

```bash
cargo make release-check
```

If artifacts are missing, check:
- GitHub Actions workflow status
- Build logs for errors
- Network connectivity

### Homebrew formula update fails

Common issues:

**1. Release not found (404)**
```
Error: Release v0.2.1 not found
```
Solution: Ensure GitHub release exists and binaries are uploaded

**2. Git push fails**
```
error: failed to push some refs
```
Solution: You may need to authenticate with GitHub. The script uses HTTPS, so ensure you have credentials configured.

**3. Invalid SHA256**
```
Failed to download one or more SHA256 checksums
```
Solution: Wait for GitHub Actions to complete uploading all assets

### Build failures

**Linux ARM64 build fails**
- Ensure `g++-aarch64-linux-gnu` is installed in CI
- Check cross-compilation environment variables

**macOS builds fail**
- Verify Rust target is installed: `rustup target add aarch64-apple-darwin`
- Check Xcode tools are available

## Version Management

### Updating Version

Update version in:
1. `Cargo.toml` (root)
2. `utils/Cargo.toml`
3. `cli/Cargo.toml`
4. `ggen-core/Cargo.toml`

Or use a version bump script:
```bash
# TODO: Add version bump automation
```

### Tag Format

Tags must follow the format: `v<VERSION>`

Examples:
- `v0.2.0`
- `v0.2.1`
- `v1.0.0`

## Testing Locally

Before releasing, test locally:

```bash
# 1. Build release binary
cargo make build-release

# 2. Test binary
./target/release/ggen --version

# 3. Run tests
cargo make test

# 4. Build docs
cargo make docs-build
```

## Post-Release

After a successful release:

1. ✅ Verify release on GitHub: https://github.com/seanchatmangpt/ggen/releases
2. ✅ Test Homebrew installation:
   ```bash
   brew uninstall ggen
   brew untap seanchatmangpt/tap
   brew tap seanchatmangpt/tap
   brew install seanchatmangpt/tap/ggen
   ggen --version
   ```
3. ✅ Update CHANGELOG.md (if applicable)
4. ✅ Announce release (if applicable)

## CI/CD Architecture

```
Push Tag (v*) → GitHub Actions → Build Platforms → Upload Assets
                                                        ↓
                                              Release Complete
                                                        ↓
                                    cargo make brew-update-formula
                                                        ↓
                                        Update homebrew-tap repo
                                                        ↓
                                           brew install ggen
```

## Security Notes

- SHA256 checksums ensure binary integrity
- All builds happen in GitHub Actions (trusted environment)
- No secrets are required for the release process
- Homebrew formula updates require git push access to homebrew-tap repo

## Future Improvements

- [ ] Automated version bumping
- [ ] Changelog generation
- [ ] Release notes automation
- [ ] Verification of Homebrew formula before push
- [ ] Rollback mechanism
- [ ] Multi-platform testing automation
