# How the ggen Marketplace Works & Creating CLI Projects

## Marketplace Status: Infrastructure Complete, Commands Placeholder

### What Exists ✅

**1. Full Infrastructure**
```
marketplace/
├── registry/
│   └── packages.toml          # Package catalog (TOML format)
└── packages/
    └── rig-mcp/               # First marketplace package
        ├── Cargo.toml         # Standalone Rust workspace
        ├── README.md          # Documentation
        ├── config.toml.example
        └── src/               # Reusable library code
```

**2. Registry System** (`marketplace/registry/packages.toml`)
```toml
version = "1.0.0"

[[package]]
name = "rig-mcp"
full_name = "rig-mcp-integration"
version = "0.1.0"
description = "Production-ready Rig LLM framework + MCP protocol integration"
category = "ai"
author = "ggen-team"
repository = "https://github.com/seanchatmangpt/ggen"
path = "marketplace/packages/rig-mcp"
license = "MIT"
dependencies = ["rig-core = 0.15.1", "rmcp = 0.8", "tokio = 1.0"]
features = [
    "Multi-provider LLM support (20+ providers)",
    "Dynamic MCP tool loading with vector-based selection",
    "Multi-transport MCP support (stdio, SSE, HTTP)",
    # ... more features
]
tags = ["llm", "mcp", "rig", "agent", "ai"]
keywords = ["llm", "agent-framework", "mcp-protocol"]
```

**3. Full Command Suite**
```bash
# Marketplace commands (CLI exists, but shows placeholders)
ggen market search "query"      # Search packages
ggen market add <name>          # Install package
ggen market list                # List installed
ggen market show <name>         # Package details
ggen market update              # Update packages
ggen market remove <name>       # Uninstall
ggen market categories          # Browse categories
ggen market natural "query"     # AI-powered search
ggen market recommend           # Get recommendations
ggen market sync                # Sync with remote
ggen market cache               # Manage cache
ggen market publish             # Publish package
ggen market unpublish           # Remove package
```

### What's Placeholder 🚧

**Current Status**: Commands show `🚧 Placeholder` messages:
```bash
$ ggen market search "CLI"
🚧 Placeholder: market search
  Query: CLI
  Category: None
  Detailed: false
  JSON: false
```

**Why?** The infrastructure and commands exist, but the implementations are stubs waiting to be connected to the registry system.

## How the Marketplace Is Designed to Work

### 1. Package Discovery
```rust
// cli/src/cmds/market/registry.rs (WORKING CODE)
impl Registry {
    pub async fn load() -> Result<Self> {
        let registry_path = Self::default_path()?;
        // Loads from marketplace/registry/packages.toml
        let content = tokio::fs::read_to_string(path).await?;
        toml::from_str(&content)
    }

    pub fn search(&self, query: &str, limit: usize) -> Vec<&Package> {
        // Scores packages by:
        // - Exact name match = 100 points
        // - Name contains = 50 points
        // - Description = 20 points
        // - Tags = 30 points
        // - Keywords = 25 points
        // - Features = 10 points
        // Returns top matches sorted by score
    }
}
```

### 2. Package Installation (Planned Flow)
```bash
$ ggen market add rust-cli-template

1. Search registry for "rust-cli-template"
2. Find package entry in packages.toml
3. Copy from marketplace/packages/rust-cli-template/ to .ggen/packages/
4. Make templates available for `ggen project gen`
5. Track in .ggen/installed.toml
```

### 3. Template Usage (Planned)
```bash
$ ggen project gen rust-cli-template --output my-cli-tool

1. Look up "rust-cli-template" in .ggen/installed.toml
2. Find template files in .ggen/packages/rust-cli-template/
3. Parse frontmatter (YAML + Tera templates)
4. Generate code to output directory
5. Run any post-generation hooks
```

## Can You Create a CLI Project Like ggen From Marketplace?

### Current Answer: Not Yet (Commands Are Placeholders)

But you can create CLI projects using **3 other working methods**:

### **Option 1: AI-Powered Generation** ✅ WORKS NOW

```bash
# Generate a CLI template using AI
ggen ai generate \
  "Create a Rust CLI tool with clap for argument parsing, \
   subcommands, configuration file support, and comprehensive error handling" \
  --output templates/rust-cli.md

# Then use it to generate projects
ggen project gen rust-cli --name my-cli-tool --output ./my-cli
```

**What you get:**
- Complete Cargo.toml with clap dependency
- Main.rs with argument parsing
- Subcommand structure
- Error handling
- Configuration file loading
- CLI help text

### **Option 2: Extract From Source** ✅ WORKS NOW

```bash
# Extract a template from ggen's own CLI code
ggen ai from-source cli/src/main.rs \
  --template-name "CLI project from ggen structure" \
  --output templates/cli-from-ggen.md

# Generate new projects from it
ggen project gen cli-from-ggen --output my-new-cli
```

**Benefit**: Creates a template based on ggen's proven architecture!

### **Option 3: Use Examples** ✅ WORKS NOW

```bash
# Use the complete project generation example
cd examples/complete-project-generation
./generate-project.sh generate

# Or the MCP + Rig integration example (full AI agent)
cd examples/mcp-rig-integration
./generate-project.sh my-agent
```

## When Marketplace Commands Will Work

The infrastructure exists. To make commands functional, we need to:

### Step 1: Connect Commands to Registry

**File**: `cli/src/cmds/market/search.rs`
```rust
// Change from placeholder:
pub async fn run(args: &SearchArgs) -> Result<()> {
    println!("🚧 Placeholder: market search");  // ❌ Current
}

// To registry integration:
pub async fn run(args: &SearchArgs) -> Result<()> {
    let registry = Registry::load().await?;      // ✅ Load registry
    let results = registry.search(&args.query, args.limit);  // ✅ Search

    // Display results
    for pkg in results {
        println!("📦 {} v{}", pkg.name, pkg.version);
        println!("   {}", pkg.description);
        // ...
    }
}
```

**Status**: The `Registry::load()` and `registry.search()` code exists and works! Just needs to be called.

### Step 2: Implement Package Installation

**Concept**:
```rust
// cli/src/cmds/market/add.rs
pub async fn run(args: &AddArgs) -> Result<()> {
    let registry = Registry::load().await?;
    let pkg = registry.get_package(&args.name)
        .ok_or("Package not found")?;

    // Copy package to .ggen/packages/
    let source = Path::new(&pkg.path);
    let dest = Path::new(".ggen/packages").join(&pkg.name);
    fs_extra::dir::copy(source, dest, &CopyOptions::new())?;

    // Track installation
    update_installed_toml(&pkg)?;

    println!("✅ Installed {} v{}", pkg.name, pkg.version);
}
```

### Step 3: Add CLI Template Package

**Create**: `marketplace/packages/rust-cli/`
```
marketplace/packages/rust-cli/
├── gpack.toml                 # Package manifest
├── templates/
│   ├── main.md                # Main.rs template
│   ├── lib.md                 # Lib.rs template
│   └── subcommand.md          # Subcommand template
└── examples/
    └── basic-usage.md         # Example generated code
```

**Registry Entry**: `marketplace/registry/packages.toml`
```toml
[[package]]
name = "rust-cli"
full_name = "@ggen/rust-cli"
version = "1.0.0"
description = "Production-ready Rust CLI template with clap, subcommands, and config"
category = "tools"
author = "ggen-team"
path = "marketplace/packages/rust-cli"
license = "MIT"
dependencies = ["clap = 4.0", "serde = 1.0", "anyhow = 1.0"]
features = [
    "Argument parsing with clap",
    "Subcommand support",
    "Configuration file loading",
    "Comprehensive error handling",
    "Colored terminal output",
    "Interactive prompts"
]
tags = ["rust", "cli", "clap", "command-line", "terminal"]
keywords = ["cli", "tool", "command", "terminal", "rust"]
```

**Then users could**:
```bash
# Search for it
ggen market search "CLI"
# Found 1 package matching "CLI"
# 📦 rust-cli v1.0.0
#    Production-ready Rust CLI template with clap, subcommands, and config

# Install it
ggen market add rust-cli
# ✅ Installed rust-cli v1.0.0

# Use it
ggen project gen rust-cli --name my-awesome-cli --output ./my-cli
# ✨ Generated project at ./my-cli
```

## Current Marketplace Package: rig-mcp

**Purpose**: AI agent framework integration (not a CLI template)

**What it is**:
- Full Rust library for building AI agents
- Supports 20+ LLM providers (OpenAI, Anthropic, Cohere, DeepSeek, Gemini, Ollama, etc.)
- Dynamic MCP tool loading with vector-based selection
- Multi-transport support (stdio, SSE, HTTP)
- Production-ready code from official MCP Rust SDK

**Example Usage**:
```rust
use rig_mcp_integration::prelude::*;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let config = Config::from_file("config.toml")?;
    let mut client = RigMcpClient::new(config).await?;
    let agent = client.agent("gpt-4").await?.build();
    let response = agent.prompt("Hello!").await?;
    println!("{}", response);
    Ok(())
}
```

**Not suitable for CLI generation**, but proves marketplace infrastructure works!

## Summary

### Marketplace Status Matrix

| Component | Status | Details |
|-----------|--------|---------|
| **Infrastructure** | ✅ Complete | `marketplace/` directory structure |
| **Registry File** | ✅ Exists | `marketplace/registry/packages.toml` |
| **Registry Code** | ✅ Working | Can load and search packages |
| **CLI Commands** | 🚧 Placeholder | Exist but show placeholder messages |
| **Packages** | 1 package | `rig-mcp` (AI agent framework) |
| **CLI Templates** | ❌ None yet | No CLI project templates |

### To Create CLI Projects Today

**Best Options**:
1. **AI Generate**: `ggen ai generate "CLI project description"`
2. **Extract Pattern**: `ggen ai from-source cli/src/main.rs`
3. **Use Examples**: `examples/complete-project-generation/`

### To Get Full Marketplace Working

**What's Needed**:
1. ✅ Infrastructure (DONE)
2. ✅ Registry system (DONE)
3. 🚧 Connect commands to registry (code exists, needs wiring)
4. ❌ Create CLI template packages
5. ❌ Add package management (install/remove/update logic)

### Recommendation

**For immediate CLI project creation**: Use AI generation or examples (they work great!)

**For marketplace-based workflow**: The infrastructure is solid and ready. Adding 2-3 CLI template packages and wiring up the commands would make it fully functional.

Want me to:
1. Create a `rust-cli` marketplace package?
2. Wire up the search command to actually use the registry?
3. Implement the `add` command for package installation?
