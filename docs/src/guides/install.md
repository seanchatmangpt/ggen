# Installation

Get ggen running in under 2 minutes. Choose your preferred installation method below.

## Prerequisites

- **Rust 1.70+** (for Cargo installation or building from source)
- **macOS/Linux** (Windows via WSL)
- **Internet connection** (for marketplace access)

## Installation Methods

### Homebrew (Recommended for macOS/Linux)

```bash
brew install seanchatmangpt/ggen/ggen
```

**Verification:**
```bash
ggen --version
# Output: ggen 2.5.0 (or later)
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
cargo install --path crates/ggen-cli
```

**Build time:** 3-5 minutes for first compilation.

### Verification

Check that ggen is installed correctly:

```bash
# Check version
ggen --version

# Test basic command
ggen help

# Verify marketplace connectivity
ggen marketplace list | head -5
```

Expected output:
```
ggen 2.5.0
Commands available: ai, project, template, graph, hook, marketplace
Marketplace: Connected to registry.ggen.io
```

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

## Troubleshooting

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

## Next Steps

After installation:

1. **Try the Quick Start**: Follow the [Quick Start Guide](quickstart.md) to generate your first code in 5 minutes
2. **Explore Templates**: Learn about [Templates](templates.md) and the ontology-driven workflow
3. **Browse Marketplace**: Discover pre-built templates in the [Marketplace Guide](marketplace.md)
4. **Read CLI Reference**: Master all commands in the [CLI Reference](../reference/cli.md)

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
ggen changelog

# Rollback to previous version (Cargo)
cargo install ggen --version 2.4.0
```

---

**Installation complete!** Head to the [Quick Start Guide](quickstart.md) to generate your first ontology-driven code.
