# ggen Installation Guide

**ggen v0.2.0** - Ontology-driven code generation

Choose your installation method based on your environment:

## Quick Comparison

| Method | Platform | Speed | Size | Requirements |
|--------|----------|-------|------|--------------|
| **cargo install** | Any | ⚡ Fast | ~200MB | Rust 1.75+ |
| **Pre-built binary** | Linux/macOS/Windows | ⚡⚡ Fastest | ~50MB | No dependencies |
| **Debian package** | Ubuntu/Debian | 🟢 Easy | ~50MB | dpkg |
| **Docker** | Any | 🟢 Isolated | ~500MB | Docker |
| **From source** | Any | ⏱️ Slow (~120s) | Build | Rust + dependencies |

---

## 1. Claude Code Web (Recommended)

**This is the fastest way to use ggen in Claude Code Web environments.**

### Method A: cargo install (Recommended)

Works instantly in Claude Code Web with the pre-installed Rust toolchain:

```bash
# Install latest stable release
cargo install ggen

# Install from main branch
cargo install --git https://github.com/seanchatmangpt/ggen

# Install with all features
cargo install ggen --features otel

# Verify installation
ggen --version
```

**Time**: ~30-60 seconds
**Disk usage**: ~200MB in .cargo directory
**Benefit**: Automatic updates via `cargo install --force ggen`

### Method B: Pre-built Binary

If you want faster installation without Rust recompilation:

```bash
# Download and extract binary
curl -L https://github.com/seanchatmangpt/ggen/releases/download/v0.2.0/ggen-x86_64-unknown-linux-gnu.tar.gz | tar xz

# Make executable
chmod +x ggen

# Add to PATH
export PATH="$PWD:$PATH"

# Verify
ggen --version
```

**Time**: ~10-15 seconds
**Disk usage**: ~50MB
**Benefit**: Instant execution, no compilation

---

## 2. macOS

### Using Homebrew

```bash
# Coming soon - currently use cargo install
cargo install ggen
```

### Using cargo

```bash
cargo install ggen
```

### Manual Binary

```bash
curl -L https://github.com/seanchatmangpt/ggen/releases/download/v0.2.0/ggen-x86_64-apple-darwin.tar.gz | tar xz
sudo mv ggen /usr/local/bin/
ggen --version
```

---

## 3. Linux (Ubuntu/Debian)

### Method A: cargo install (Fastest)

```bash
cargo install ggen
```

### Method B: Debian Package

```bash
# Download .deb file
wget https://github.com/seanchatmangpt/ggen/releases/download/v0.2.0/ggen_0.2.0_amd64.deb

# Install
sudo dpkg -i ggen_0.2.0_amd64.deb

# Or using apt
sudo apt install ./ggen_0.2.0_amd64.deb
```

### Method C: Pre-built Binary

```bash
curl -L https://github.com/seanchatmangpt/ggen/releases/download/v0.2.0/ggen-x86_64-unknown-linux-gnu.tar.gz | tar xz
sudo mv ggen /usr/local/bin/
ggen --version
```

### Build from Source

```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen

# Build with cargo make (recommended)
cargo make build-release

# Or with cargo directly
cargo build --release -p ggen-cli

# Binary at target/release/ggen
sudo cp target/release/ggen /usr/local/bin/
```

---

## 4. Windows

### Pre-built Binary

```powershell
# Download and extract
$ProgressPreference = 'SilentlyContinue'
Invoke-WebRequest -Uri "https://github.com/seanchatmangpt/ggen/releases/download/v0.2.0/ggen-x86_64-pc-windows-gnu.zip" -OutFile "ggen.zip"
Expand-Archive -Path "ggen.zip" -DestinationPath "C:\Program Files\ggen\"

# Add to PATH (requires admin)
$env:Path += ";C:\Program Files\ggen"

# Verify
ggen --version
```

### Using Rust

```powershell
cargo install ggen
```

---

## 5. Docker

### Quick Start

```bash
# Using official image (once published to GHCR)
docker run -v $(pwd):/workspace ghcr.io/seanchatmangpt/ggen:latest sync

# Using Dockerfile
cat > Dockerfile <<EOF
FROM rust:latest
RUN cargo install ggen
ENTRYPOINT ["ggen"]
CMD ["--help"]
EOF

docker build -t ggen .
docker run -v $(pwd):/workspace ggen sync
```

---

## 6. Alpine Linux

```bash
# Install Rust dependencies
apk add --no-cache rustup curl

# Install ggen
cargo install ggen
```

---

## 7. Arch Linux (AUR)

```bash
# Build from AUR
git clone https://aur.archlinux.org/ggen.git
cd ggen
makepkg -si
```

---

## Verification

After installation, verify ggen works:

```bash
# Check version
ggen --version

# Show help
ggen --help

# Create a quick test
mkdir -p /tmp/ggen-test && cd /tmp/ggen-test

# Initialize a project
ggen init

# Run generation (dry-run)
ggen sync --dry_run true
```

---

## Post-Installation Setup

### 1. Shell Completion (Optional)

Generate shell completions for faster CLI usage:

```bash
# Bash
ggen --generate-bash-completion >> ~/.bashrc

# Zsh
ggen --generate-zsh-completion >> ~/.zshrc

# Fish
ggen --generate-fish-completion > ~/.config/fish/completions/ggen.fish

# Reload shell
source ~/.bashrc  # or your shell config
```

### 2. Update .claude/settings.json

Configure ggen for Claude Code Web sessions:

```json
{
  "environment": {
    "GGEN_HOME": ".ggen",
    "GGEN_LOG_LEVEL": "info"
  },
  "hooks": {
    "SessionStart": [
      {
        "matcher": "startup",
        "hooks": [
          {
            "type": "command",
            "command": "cargo install ggen --locked"
          }
        ]
      }
    ]
  }
}
```

### 3. Configure Your First Project

See [Getting Started Tutorial](../tutorials/01-getting-started.md) for the next steps.

---

## Troubleshooting

### "ggen: command not found"

**Solution**: Add `~/.cargo/bin` to your PATH:

```bash
export PATH="$HOME/.cargo/bin:$PATH"
```

Add to `~/.bashrc` or `~/.zshrc` for permanent change.

### "error: linker `cc` not found"

**Solution**: Install build essentials:

```bash
# Ubuntu/Debian
sudo apt-get install build-essential

# Fedora
sudo dnf install gcc

# macOS
xcode-select --install
```

### "error: cannot find openssl"

**Solution**: Install OpenSSL headers:

```bash
# Ubuntu/Debian
sudo apt-get install libssl-dev

# Fedora
sudo dnf install openssl-devel

# macOS
brew install openssl
```

### "cargo: command not found"

**Solution**: Install Rust:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env
```

### Performance in Claude Code Web

**Issue**: Installation slow in Claude Code Web (network limited)

**Solution**: Use pre-built binary or enable faster network:

```bash
# Pre-built (fastest)
curl -L https://github.com/seanchatmangpt/ggen/releases/download/v0.2.0/ggen-x86_64-unknown-linux-gnu.tar.gz | tar xz

# Or: cache cargo registry locally in .claude/cache/
```

---

## Version Management

### Update to Latest

```bash
# Update via cargo
cargo install ggen --force

# Update from source
git clone https://github.com/seanchatmangpt/ggen
cd ggen && git pull
cargo install --path crates/ggen-cli
```

### Pin to Specific Version

```bash
# Install v0.2.0 specifically
cargo install ggen --version 0.2.0

# Or from source
git checkout v0.2.0
cargo install --path crates/ggen-cli
```

---

## Next Steps

- 📖 [Getting Started Tutorial](../tutorials/01-getting-started.md)
- 🎯 [Quick Start: Your First Project](../tutorials/02-first-project.md)
- 📚 [Common Tasks How-To Guide](../how-to/01-common-tasks.md)
- 🔧 [Command Reference](../reference/01-commands.md)

---

## System Requirements

| Component | Minimum | Recommended |
|-----------|---------|-------------|
| CPU | 1 core | 2+ cores |
| RAM | 512 MB | 2 GB+ |
| Disk | 100 MB | 500 MB+ |
| Rust | 1.75+ | Latest stable |
| OS | Ubuntu 20.04+ / macOS 10.15+ / Windows 10+ | Latest LTS |

---

## Getting Help

- 📖 [Complete Documentation](../README.md)
- 🐛 [Report Issues](https://github.com/seanchatmangpt/ggen/issues)
- 💬 [Discussions](https://github.com/seanchatmangpt/ggen/discussions)
- 📧 [Email Support](mailto:sean@chatmangpt.com)

---

## License

ggen is released under the MIT License. See [LICENSE](../../LICENSE) for details.
