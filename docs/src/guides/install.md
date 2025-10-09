# Install

## Homebrew
```bash
brew tap rgen-dev/tap
brew install rgen
```

## Cargo

```bash
cargo install rgen
```

## Verify Installation

```bash
rgen --version
```

## Post-Installation Setup

### Marketplace Access

The marketplace is available immediately after installation. No additional configuration is required.

### First Rpack Installation

```bash
# Search for available rpacks
rgen search rust cli

# Install your first rpack
rgen add io.ggen.rust.cli-subcommand

# Verify installation
rgen packs
```

### Rpack Cache Location

Rpacks are cached locally in your project directory:

```bash
# View cache location
ls -la .rgen/rpacks/

# Cache structure:
# .rgen/
# ├── rpacks/
# │   └── io.ggen.rust.cli-subcommand/
# │       └── 0.2.1/
# │           ├── templates/
# │           ├── macros/
# │           └── graphs/
# └── rgen.lock
```

### Registry Configuration

By default, rgen uses the official registry. No configuration is needed for most users.

```bash
# Check registry status
rgen search --help

# Verify connectivity
rgen categories
```

### Shell Completions (Optional)

```bash
# Generate completions for your shell
rgen completion bash > ~/.bash_completion.d/rgen
rgen completion zsh > ~/.zsh/completions/_rgen
rgen completion fish > ~/.config/fish/completions/rgen.fish

# Reload shell
source ~/.bashrc  # or ~/.zshrc
```

## Troubleshooting Installation

### Command Not Found

```bash
# Check if rgen is in PATH
which rgen

# If not found, add to PATH
export PATH="$HOME/.cargo/bin:$PATH"

# Or reinstall with explicit path
cargo install rgen --root ~/.local
export PATH="$HOME/.local/bin:$PATH"
```

### Marketplace Access Issues

```bash
# Test marketplace connectivity
rgen search rust

# Check network connectivity
ping registry.rgen.io

# Verify DNS resolution
nslookup registry.rgen.io
```

### Permission Issues

```bash
# Fix cache directory permissions
chmod -R 755 .rgen/

# Or use different cache location
export RGEN_CACHE_DIR="$HOME/.cache/rgen"
```

## Next Steps

After installation:

1. **Try the quickstart**: Follow the [quickstart guide](quickstart.md)
2. **Explore rpacks**: Browse the [marketplace](marketplace.md)
3. **Learn templates**: Read the [templates guide](templates.md)
4. **Generate code**: Use the [CLI reference](cli.md)

## Uninstallation

### Homebrew
```bash
brew uninstall rgen
brew untap rgen-dev/tap
```

### Cargo
```bash
cargo uninstall rgen
```

### Cleanup
```bash
# Remove cache directories
rm -rf .rgen/
rm -rf ~/.cache/rgen/

# Remove completions
rm ~/.bash_completion.d/rgen
rm ~/.zsh/completions/_rgen
rm ~/.config/fish/completions/rgen.fish
```
