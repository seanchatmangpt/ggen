# ggen v0.2.0 Installation Guide

Complete installation and setup instructions for ggen v0.2.0.

## Table of Contents

- [System Requirements](#system-requirements)
- [Installation Methods](#installation-methods)
- [Verification](#verification)
- [Configuration](#configuration)
- [First Use](#first-use)
- [Troubleshooting](#troubleshooting)

## System Requirements

### Minimum Requirements
- **Rust**: 1.78 or later
- **Memory**: 512MB RAM minimum (2GB recommended)
- **Disk Space**: 500MB for installation and dependencies
- **OS**: Linux, macOS, or Windows

### Recommended Requirements
- **Rust**: Latest stable (1.84+)
- **Memory**: 4GB+ for large project generation
- **Disk Space**: 2GB+ for workspace and caches
- **OS**: Linux or macOS for best compatibility

### Development Requirements (for contributing)
- **Rust**: Latest stable + nightly for some features
- **Cargo**: Latest version with workspace support
- **Node.js**: 20+ (for marketplace integration)
- **Docker**: Optional, for testcontainers e2e tests

## Installation Methods

### Method 1: From Crates.io (Recommended)

```bash
# Install latest stable release
cargo install ggen

# Or install specific version
cargo install ggen@0.2.0

# Verify installation
ggen --version
# Expected output: ggen 0.2.0
```

**Pros:**
- Simple and fast
- Precompiled binaries for most platforms
- No build required

**Cons:**
- Longer wait for new releases on crates.io
- Limited customization

### Method 2: From Source

```bash
# Clone repository
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen

# Checkout v0.2.0 tag
git checkout v0.2.0

# Build and install
cargo install --path . --release

# Verify
ggen --version
```

**Pros:**
- Full control over build
- Access to latest development features
- Can customize build parameters

**Cons:**
- Requires Rust toolchain
- Longer compilation time
- Need to build for releases

### Method 3: Docker

```bash
# Build Docker image
docker build -t ggen:0.2.0 .

# Run in container
docker run --rm ggen:0.2.0 ggen --version

# Or use pre-built image (if available)
docker pull seanchatmangpt/ggen:0.2.0
```

**Pros:**
- Isolated environment
- Consistent across platforms
- No local Rust installation needed

**Cons:**
- Container overhead
- Requires Docker installation
- Less direct IDE integration

## Verification

### 1. Check Version

```bash
ggen --version
# Output: ggen 0.2.0
```

### 2. Verify Installation

```bash
ggen --help
# Output: Shows main command help
```

### 3. Test Basic Functionality

```bash
# Create test directory
mkdir ggen-test
cd ggen-test

# Initialize new project
ggen init --name test-project --type rust

# Verify generated files
ls -la test-project/
```

### 4. Verify Optional Features

```bash
# Check AI integration (if genai configured)
ggen ai --help

# Check marketplace (if configured)
ggen marketplace search

# Check ontology support
ggen ontology --version
```

## Configuration

### Environment Variables

```bash
# Log level (trace, debug, info, warn, error)
export RUST_LOG=ggen=debug

# Cache directory
export GGEN_CACHE_DIR=$HOME/.ggen/cache

# Config file location
export GGEN_CONFIG=$HOME/.config/ggen/config.toml

# Marketplace endpoint
export GGEN_MARKETPLACE_URL=https://marketplace.ggen.dev

# AI provider configuration
export GGEN_AI_PROVIDER=ollama
export GGEN_AI_BASE_URL=http://localhost:11434
export GGEN_AI_MODEL=qwen:7b-code
```

### Configuration File

Create `~/.config/ggen/config.toml`:

```toml
# Basic configuration
[general]
cache_dir = "$HOME/.ggen/cache"
log_level = "info"
timeout_seconds = 300

# Code generation
[generation]
deterministic = true
output_format = "json"
parallel_threads = "auto"

# Marketplace
[marketplace]
enabled = true
endpoint = "https://marketplace.ggen.dev"
cache_duration_hours = 24

# AI integration
[ai]
enabled = true
provider = "ollama"
base_url = "http://localhost:11434"
model = "qwen:7b-code"
timeout_seconds = 60
streaming = true

# RDF/Ontology
[ontology]
enabled = true
validation = "strict"
formats = ["ttl", "rdf", "jsonld"]

# Performance
[performance]
incremental_cache = true
parallel_processing = true
memory_limit_mb = 1024
```

### Platform-Specific Paths

**Linux:**
```bash
~/.config/ggen/config.toml
~/.ggen/cache/
```

**macOS:**
```bash
~/Library/Application Support/ggen/config.toml
~/Library/Caches/ggen/
```

**Windows:**
```cmd
%APPDATA%\ggen\config.toml
%LOCALAPPDATA%\ggen\cache\
```

## First Use

### 1. Initialize Project

```bash
ggen init --name my-project --type rust --template basic
cd my-project
```

### 2. Configure Project

Edit `ggen.toml`:

```toml
[package]
name = "my-project"
version = "0.1.0"
author = "Your Name"

[generation]
output_dir = "./src/generated"
deterministic = true

[ontology]
rdf_sources = ["./data/ontology.ttl"]
```

### 3. Generate Code

```bash
ggen generate
```

### 4. Verify Generated Code

```bash
ls -la src/generated/
cat src/generated/main.rs
```

### 5. Build Generated Code

```bash
# If Rust project
cargo build

# If TypeScript project
npm install && npm run build
```

## Feature Setup

### AI Integration

#### With Ollama (Recommended for Local Development)

```bash
# Install Ollama
# https://ollama.ai

# Start Ollama server
ollama serve

# In another terminal, configure ggen
export GGEN_AI_PROVIDER=ollama
export GGEN_AI_BASE_URL=http://localhost:11434
export GGEN_AI_MODEL=qwen:7b-code

# Test connection
ggen ai --test
```

#### With OpenAI

```bash
export GGEN_AI_PROVIDER=openai
export GGEN_AI_API_KEY=sk-...
export GGEN_AI_MODEL=gpt-4

# Test connection
ggen ai --test
```

#### With Anthropic

```bash
export GGEN_AI_PROVIDER=anthropic
export GGEN_AI_API_KEY=sk-ant-...
export GGEN_AI_MODEL=claude-3-sonnet

# Test connection
ggen ai --test
```

### Marketplace Integration

```bash
# Login to marketplace
ggen marketplace login

# Search packages
ggen marketplace search --query "typescript-rest"

# Install package
ggen marketplace install typescript-rest@latest

# List installed packages
ggen marketplace list
```

### Ontology Support

```bash
# Validate ontology file
ggen ontology validate ./data/schema.ttl

# Query ontology with SPARQL
ggen ontology query --file ./data/schema.ttl \
  "SELECT ?class WHERE { ?class a rdfs:Class }"

# Generate from ontology
ggen generate-from-ontology ./data/schema.ttl
```

## Updating

### From v0.1.0 to v0.2.0

```bash
# Backup current installation
cp -r ~/.ggen ~/.ggen.backup

# Update installation
cargo install ggen@0.2.0 --force

# Run migration
ggen migrate --from 0.1.0 --to 0.2.0

# Verify new version
ggen --version
```

### Uninstall

```bash
# Remove installation
cargo uninstall ggen

# Remove configuration (optional)
rm -rf ~/.ggen
rm -rf ~/.config/ggen
```

## Troubleshooting

### Installation Issues

**Problem: "cargo: command not found"**
```bash
# Solution: Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env
```

**Problem: "ggen: command not found" after installation**
```bash
# Solution: Add Cargo bin to PATH
export PATH="$HOME/.cargo/bin:$PATH"
# Add to shell config file (~/.bashrc, ~/.zshrc, etc.)
```

**Problem: Installation hangs or times out**
```bash
# Solution: Set longer timeout
cargo install ggen --verbose --lock-wait-timeout 300
```

### Runtime Issues

**Problem: "Command not found: ggen"**
```bash
# Verify installation
which ggen
# If not found, check PATH
echo $PATH

# Reinstall
cargo install ggen --force
```

**Problem: Memory error during generation**
```bash
# Increase memory limit
export GGEN_MEMORY_LIMIT=2048

# Or use parallel_threads = 1 in config.toml
```

**Problem: Configuration not loading**
```bash
# Check configuration validity
ggen config validate

# View current configuration
ggen config show

# Reset to defaults
ggen config reset
```

### Feature Issues

**Problem: AI features not working**
```bash
# Check AI provider connection
ggen ai --diagnose

# Verify environment variables
echo $GGEN_AI_PROVIDER
echo $GGEN_AI_BASE_URL

# Test with verbose logging
RUST_LOG=ggen::ai=debug ggen ai --test
```

**Problem: Marketplace connection failed**
```bash
# Test marketplace connectivity
ggen marketplace --test

# Check endpoint
echo $GGEN_MARKETPLACE_URL

# Try alternate endpoint
export GGEN_MARKETPLACE_URL=https://mirror.marketplace.ggen.dev
```

**Problem: Ontology validation failing**
```bash
# Check ontology file
file ./data/schema.ttl

# Validate format
ggen ontology validate --verbose ./data/schema.ttl

# Check for syntax errors
head -20 ./data/schema.ttl
```

## Getting Help

- **Documentation**: https://github.com/seanchatmangpt/ggen/docs
- **Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Discussions**: https://github.com/seanchatmangpt/ggen/discussions
- **Community**: Discord (if available)

## Next Steps

1. Read [Getting Started Guide](../../GETTING_STARTED.md)
2. Explore [Example Projects](../../examples/)
3. Review [Architecture Documentation](../../ARCHITECTURE.md)
4. Check [API Reference](../../API_REFERENCE.md)

---

For migration from earlier versions, see [MIGRATION-GUIDE.md](MIGRATION-GUIDE.md)
