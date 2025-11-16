<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How to Install ggen](#how-to-install-ggen)
  - [Prerequisites](#prerequisites)
  - [Installation Methods](#installation-methods)
    - [Homebrew (Recommended for macOS/Linux)](#homebrew-recommended-for-macoslinux)
    - [Cargo (Rust Package Manager)](#cargo-rust-package-manager)
    - [From Source (Latest Development Version)](#from-source-latest-development-version)
  - [Post-Installation Setup](#post-installation-setup)
    - [Shell Completions (Optional)](#shell-completions-optional)
    - [Environment Variables (Optional)](#environment-variables-optional)
    - [Verify AI Features (Optional)](#verify-ai-features-optional)
  - [Troubleshooting Installation](#troubleshooting-installation)
    - [Command Not Found](#command-not-found)
    - [Marketplace Connection Issues](#marketplace-connection-issues)
    - [Cargo Installation Fails](#cargo-installation-fails)
    - [Permission Denied](#permission-denied)
  - [Uninstallation](#uninstallation)
    - [Homebrew](#homebrew)
    - [Cargo](#cargo)
    - [Cleanup Cache](#cleanup-cache)
  - [Updates](#updates)
    - [Check for Updates](#check-for-updates)
    - [Version Management](#version-management)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How to Install ggen

Complete installation guide for all platforms and methods.

## Prerequisites

- **Rust 1.70+** (for Cargo installation or building from source)
- **macOS/Linux** (Windows via WSL)
- **Internet connection** (for marketplace access)

## Installation Methods

### Homebrew (Recommended for macOS/Linux)

```bash
brew tap seanchatmangpt/tap
brew install ggen
```

**Verification:**
```bash
ggen --version
# Output: ggen 2.7.0
```

### Cargo (Rust Package Manager)

Install from crates.io:

```bash
cargo install ggen
```

**Note:** This compiles from source and may take 3-5 minutes.

### From Source (Latest Development Version)

For the absolute latest features:

```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo install --path crates/ggen-cli --bin ggen --force
```

**Build time:** 3-5 minutes for first compilation.

## Post-Installation Setup

### Shell Completions (Optional)

Add tab-completion for your shell:

```bash
# Bash
ggen completion bash > ~/.bash_completion.d/ggen
source ~/.bashrc

# Zsh
ggen completion zsh > ~/.zsh/completions/_ggen
source ~/.zshrc

# Fish
ggen completion fish > ~/.config/fish/completions/ggen.fish
```

### Environment Variables (Optional)

Configure ggen behavior via environment variables:

```bash
# Custom cache directory
export GGEN_CACHE_DIR="$HOME/.cache/ggen"

# Custom marketplace registry
export GGEN_REGISTRY_URL="https://registry.ggen.io"

# AI provider configuration
export ANTHROPIC_API_KEY="sk-ant-..."  # For ggen ai commands
```

Add to your `~/.bashrc` or `~/.zshrc` to persist.

### Verify AI Features (Optional)

If you plan to use AI-powered ontology generation:

```bash
# Set API key
export ANTHROPIC_API_KEY="sk-ant-..."

# Test AI commands
ggen ai generate-ontology --prompt "User, Post" --output test.ttl
```

## Troubleshooting Installation

### Command Not Found

**Problem:** `ggen: command not found`

**Solution:**
```bash
# Check if ggen is in PATH
which ggen

# If not found, add cargo bin to PATH
export PATH="$HOME/.cargo/bin:$PATH"

# Make permanent (bash)
echo 'export PATH="$HOME/.cargo/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc

# Make permanent (zsh)
echo 'export PATH="$HOME/.cargo/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

### Marketplace Connection Issues

**Problem:** `Failed to connect to marketplace`

**Solution:**
```bash
# Test network connectivity
ping registry.ggen.io

# Check DNS resolution
nslookup registry.ggen.io

# Try manual registry access
curl -I https://registry.ggen.io/health

# If behind corporate firewall, configure proxy
export HTTPS_PROXY="http://proxy.company.com:8080"
```

### Cargo Installation Fails

**Problem:** Compilation errors during `cargo install ggen`

**Solution:**
```bash
# Update Rust toolchain
rustup update stable

# Verify Rust version
rustc --version  # Should be 1.70 or higher

# Clear cargo cache and retry
cargo clean
cargo install ggen --force

# If still fails, try nightly
rustup install nightly
cargo +nightly install ggen
```

### Permission Denied

**Problem:** Permission errors when running ggen

**Solution:**
```bash
# Fix binary permissions
chmod +x $(which ggen)

# If installed via Homebrew, verify
brew doctor

# For source installation, use correct prefix
cargo install --path crates/ggen-cli --root ~/.local
export PATH="$HOME/.local/bin:$PATH"
```

## Uninstallation

### Homebrew
```bash
brew uninstall ggen
```

### Cargo
```bash
cargo uninstall ggen
```

### Cleanup Cache
```bash
# Remove project-level cache
rm -rf .ggen/

# Remove global cache
rm -rf ~/.cache/ggen/

# Remove shell completions
rm ~/.bash_completion.d/ggen
rm ~/.zsh/completions/_ggen
rm ~/.config/fish/completions/ggen.fish
```

## Updates

### Check for Updates

```bash
# Homebrew
brew upgrade ggen

# Cargo
cargo install ggen --force

# From source
cd ggen && git pull
cargo install --path crates/ggen-cli --force
```

### Version Management

```bash
# Check current version
ggen --version

# View changelog
cat CHANGELOG.md

# Rollback to previous version (Cargo)
cargo install ggen --version 2.6.0
```

## Next Steps

After installation:

1. **Try the Quick Start:** Follow the [Getting Started Tutorial](../tutorials/getting-started.md)
2. **Explore Templates:** Learn about [Creating Templates](create-templates.md)
3. **Browse Marketplace:** Discover pre-built templates in the [Marketplace Explanation](../explanations/marketplace.md)

