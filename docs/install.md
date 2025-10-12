<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Install](#install)
  - [Homebrew](#homebrew)
  - [Cargo](#cargo)
  - [Verify Installation](#verify-installation)
  - [Post-Installation Setup](#post-installation-setup)
    - [🚀 AI-Powered Features (v1.0.0)](#-ai-powered-features-v100)
      - [Option 1: Local AI (Recommended for Privacy)](#option-1-local-ai-recommended-for-privacy)
      - [Option 2: Cloud AI (OpenAI/Anthropic)](#option-2-cloud-ai-openaianthropic)
    - [Marketplace Access](#marketplace-access)
    - [First Gpack Installation](#first-gpack-installation)
    - [Gpack Cache Location](#gpack-cache-location)
    - [Registry Configuration](#registry-configuration)
    - [Shell Completions (Optional)](#shell-completions-optional)
  - [Troubleshooting Installation](#troubleshooting-installation)
    - [Command Not Found](#command-not-found)
    - [Marketplace Access Issues](#marketplace-access-issues)
    - [Permission Issues](#permission-issues)
  - [Next Steps](#next-steps)
  - [Uninstallation](#uninstallation)
    - [Homebrew](#homebrew-1)
    - [Cargo](#cargo-1)
    - [Cleanup](#cleanup)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Install

## Homebrew
```bash
brew tap seanchatmangpt/tap
brew install ggen
```

## Cargo

```bash
cargo install ggen
```

## Verify Installation

```bash
ggen --version
```

## Post-Installation Setup

### 🚀 AI-Powered Features (v1.2.0)

**ggen v1.2.0** includes powerful AI capabilities. To unlock them:

#### Option 1: Local AI (Recommended for Privacy)

```bash
# Install Ollama for local AI
curl -fsSL https://ollama.ai/install.sh | sh

# Pull the recommended model (no API keys needed!)
ollama pull qwen3-coder:30b

# Test AI generation
ggen ai generate -d "Hello world function in Rust" -o hello.rs
```

#### Option 2: Cloud AI (OpenAI/Anthropic)

```bash
# Create .env file for API keys
echo "OPENAI_API_KEY=sk-your-key-here" >> ~/.config/ggen/.env
echo "ANTHROPIC_API_KEY=sk-ant-your-key-here" >> ~/.config/ggen/.env

# Test with OpenAI
ggen ai generate -d "REST API endpoint" -o api.rs

# Test with Anthropic
ANTHROPIC_API_KEY=sk-ant-your-key ggen ai generate -d "Database model" -o model.rs
```

**AI Features Available:**
- 🤖 **Template Generation** - Create templates from natural language
- 🏗️ **Project Scaffolding** - Generate complete projects with AI
- 🔍 **Natural Language Search** - Find templates conversationally
- 📋 **Smart Frontmatter** - AI-generated metadata

### Marketplace Access

The marketplace is available immediately after installation. No additional configuration is required.

### First Gpack Installation

```bash
# Search for available gpacks
ggen search rust cli

# Install your first gpack
ggen add io.ggen.rust.cli-subcommand

# Verify installation
ggen packs
```

### Gpack Cache Location

Gpacks are cached locally in your project directory:

```bash
# View cache location
ls -la .ggen/gpacks/

# Cache structure:
# .ggen/
# ├── gpacks/
# │   └── io.ggen.rust.cli-subcommand/
# │       └── 0.2.1/
# │           ├── templates/
# │           ├── macros/
# │           └── graphs/
# └── ggen.lock
```

### Registry Configuration

By default, ggen uses the official registry. No configuration is needed for most users.

```bash
# Check registry status
ggen search --help

# Verify connectivity
ggen categories
```

### Shell Completions (Optional)

```bash
# Generate completions for your shell
ggen completion bash > ~/.bash_completion.d/ggen
ggen completion zsh > ~/.zsh/completions/_ggen
ggen completion fish > ~/.config/fish/completions/ggen.fish

# Reload shell
source ~/.bashrc  # or ~/.zshrc
```

## Troubleshooting Installation

### Command Not Found

```bash
# Check if ggen is in PATH
which ggen

# If not found, add to PATH
export PATH="$HOME/.cargo/bin:$PATH"

# Or reinstall with explicit path
cargo install ggen --root ~/.local
export PATH="$HOME/.local/bin:$PATH"
```

### Marketplace Access Issues

```bash
# Test marketplace connectivity
ggen search rust

# Check network connectivity
ping registry.ggen.io

# Verify DNS resolution
nslookup registry.ggen.io
```

### Permission Issues

```bash
# Fix cache directory permissions
chmod -R 755 .ggen/

# Or use different cache location
export GGEN_CACHE_DIR="$HOME/.cache/ggen"
```

## Next Steps

After installation:

1. **Try the quickstart**: Follow the [quickstart guide](quickstart.md)
2. **Explore gpacks**: Browse the [marketplace](marketplace.md)
3. **Learn templates**: Read the [templates guide](templates.md)
4. **Generate code**: Use the [CLI reference](cli.md)

## Uninstallation

### Homebrew
```bash
brew uninstall ggen
brew untap seanchatmangpt/tap
```

### Cargo
```bash
cargo uninstall ggen
```

### Cleanup
```bash
# Remove cache directories
rm -rf .ggen/
rm -rf ~/.cache/ggen/

# Remove completions
rm ~/.bash_completion.d/ggen
rm ~/.zsh/completions/_ggen
rm ~/.config/fish/completions/ggen.fish
```
