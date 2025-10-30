# Ggen Examples

This directory contains working examples demonstrating ggen's capabilities.

**Status**: All examples compile and run successfully (verified 2025-10-11)

## 🚀 Quick Start

**Brand new to ggen?** Start here to get oriented:

### 🏥 Step 1: Check Your Environment
```bash
# First time? Check if your environment is ready
ggen doctor

# ✅ Rust toolchain (rustc 1.90.0)
# ✅ Cargo (cargo 1.90.0)
# ✅ Git (git 2.39.0)
# ✅ Environment variables configured
# ✅ Templates directory exists
```

### 📚 Step 2: Get Personalized Help
```bash
# Get help adapted to your experience level
ggen help-me

# 📚 Your level: newcomer
# 🚀 Welcome to ggen!
# Here are the essential commands to get started:
#   ggen doctor - Check if your environment is ready
#   ggen template list - See available templates
#   ggen help-me --tips - Get personalized tips
```

### 🎯 Step 3: Try Your First Example
**New to ggen?** Start with these comprehensive tutorials:

1. **[basic-template-generation/](basic-template-generation/)** - Learn template fundamentals (15-30 min)
2. **[ai-template-creation/](ai-template-creation/)** - AI-powered template creation (30-45 min)
3. **[complete-project-generation/](complete-project-generation/)** - Generate full projects (45-60 min)

**Want ultra-fast deployment?** Check out the **[cleanroom/examples/](../cleanroom/examples/)** directory for <60 second concept-to-deploy workflows.

## 🎯 User-Friendly Features (NEW!)

### 🏥 Environment Health Check
Before diving into examples, ensure your environment is ready:

```bash
ggen doctor
```

**What it checks**:
- ✅ Rust toolchain and Cargo
- ✅ Git installation
- ✅ Environment variables
- ✅ Templates directory
- ✅ Configuration files

**If something is missing**, ggen doctor provides platform-specific fix instructions!

### 📚 Progressive Help System
Get help that adapts to your experience level:

```bash
# General help (adapts based on your usage)
ggen help-me

# Get personalized tips
ggen help-me --tips

# Command-specific help
ggen help-me template
```

**Your help evolves as you use ggen more:**
- **Newcomer** (0-5 commands): Essential commands and quickstart
- **Intermediate** (6-20 commands): Common workflows and best practices
- **Advanced** (21-50 commands): Advanced features and optimization
- **Expert** (50+ commands): Power user tips and customization

### 🔧 Enhanced Error Messages
When something goes wrong, ggen now provides actionable help:

```bash
$ ggen template generate rust-servce
❌ Error: Template 'rust-servce' not found

📝 Context: The specified template does not exist in the registry

💡 Did you mean:
   • rust-service
   • rust-server
   • rust-svc

🔧 How to fix:
   1. Run 'ggen template list' to see available templates
   2. Use 'ggen template search rust' to find Rust templates

📚 Documentation: https://seanchatmangpt.github.io/ggen/templates
```

**Error messages now include:**
- Context explaining what went wrong
- "Did you mean?" suggestions for typos
- Step-by-step fix instructions
- Platform-specific commands (macOS/Linux/Windows)
- Links to relevant documentation

---

## 📚 Available Examples

### 🎓 Comprehensive Tutorial Examples (NEW!)

#### 1. **basic-template-generation/**
Complete tutorial for learning ggen template fundamentals.

**What You'll Learn**:
- Template anatomy (YAML frontmatter + Tera templating)
- Variable substitution and filters
- Conditional rendering and loops
- Generating production-quality Rust code

**Quick Start**:
```bash
# First, check your environment
ggen doctor

# Then run the tutorial
cd basic-template-generation
./run-example.sh        # Interactive tutorial
./run-example.sh show   # View all templates

# Get help if you need it
ggen help-me template
```

**Time**: 15-30 minutes
**Prerequisites**: Just ggen

---

#### 2. **ai-template-creation/**
AI-powered template creation using ggen's AI commands.

**What You'll Learn**:
- `ggen ai generate` - Create templates from descriptions
- `ggen ai validate` - Template validation
- Iterative improvement workflow
- Mock mode for cost-free testing

**Quick Start**:
```bash
cd ai-template-creation
./run-ai-workflow.sh    # Complete AI workflow demo
cat prompts.txt         # Example prompts library
```

**Time**: 30-45 minutes
**Prerequisites**: ggen (mock mode) or Ollama for real AI

---

#### 3. **complete-project-generation/**
Generate complete, production-ready Rust web services.

**What You'll Learn**:
- Multi-file project generation
- REST API with 6 endpoints
- Integration tests
- Complete build-test-deploy workflow

**Quick Start**:
```bash
cd complete-project-generation
./generate-project.sh validate    # Validate templates
./generate-project.sh generate    # Generate project
cd output/my-web-service && cargo run
```

**Time**: 45-60 minutes
**Prerequisites**: ggen, Rust toolchain

---

#### 4. **source-code-analysis/**
Analyze existing code and extract reusable templates.

**What You'll Learn**:
- `ggen ai from-source` - Pattern extraction
- Analyzing code to create templates
- Reverse engineering workflows
- Code regeneration with customization

**Quick Start**:
```bash
cd source-code-analysis
./analyze-and-generate.sh    # Complete analysis workflow
```

**Time**: 30-45 minutes
**Prerequisites**: ggen (mock mode works)

---

#### 5. **mcp-integration/**
Integrate ggen with Claude Desktop and Cline via MCP.

**What You'll Learn**:
- Model Context Protocol (MCP) integration
- 25+ MCP tools for conversational code generation
- Real AI assistant workflows
- Production integration patterns

**Quick Start**:
```bash
cd mcp-integration
./install-ggen-mcp.sh     # Install MCP server
./test-mcp-tools.sh       # Test integration
```

**Time**: 45-60 minutes
**Prerequisites**: ggen-mcp, Claude Desktop or Cline

---

#### 6. **mcp-rig-integration/** (NEW!)
Generate MCP + Rig AI agent projects with dynamic tool selection.

**What You'll Learn**:
- MCP Rust SDK integration with Rig framework
- Multi-provider LLM support (DeepSeek, Cohere, OpenAI)
- Dynamic tool discovery using RAG and embeddings
- Streaming chat with real-time tool calls
- Multi-transport MCP servers (stdio, SSE, streamable)

**Quick Start**:
```bash
cd mcp-rig-integration
./generate-project.sh my-agent    # Generate project
cd my-agent
# Configure API keys in config.toml
cargo run                          # Run AI agent
```

**Time**: 60-90 minutes
**Prerequisites**: Rust, ggen, MCP servers

**Features**:
- Vector-based dynamic tool selection
- Concurrent multi-server startup
- Beautiful CLI with colored streaming output
- Template-driven customization
- Production-ready architecture

---

#### 7. **Ultra-Fast Deployment** (NEW!) ⚡
Deploy production services in <60 seconds using ggen + cleanroom synergy.

**What You'll Learn**:
- <60s concept-to-deployment workflow
- ggen marketplace → cleanroom testing → lifecycle deployment
- Hermetic test environments with testcontainers
- Production-grade error handling patterns
- Deployment validation and verification

**Quick Start**:
```bash
cd ../cleanroom/examples
./full_demo.sh                    # Complete ultra-fast demo
./quick_demo.sh                   # Quick 30s demo
cat quick_reference.sh            # Command reference
```

**Time**: 5-10 minutes for demo, 30-60 minutes for deep dive
**Prerequisites**: ggen, cleanroom (built from ../cleanroom)

**Features**:
- ⚡ <60s total deployment time
- 🧪 100% tested with cleanroom
- 🔒 Production-safe (zero `.expect()` calls)
- 🎯 Deterministic and reproducible
- 📦 Includes templates, tests, Docker, CI/CD

**Documentation**:
- [User Guide](../docs/ULTRA_FAST_DEPLOY.md) - Getting started, examples, workflows
- [Technical Reference](../docs/ULTRA_FAST_REFERENCE.md) - Architecture, API, performance
- [Cleanroom Examples](../cleanroom/examples/README.md) - Hermetic testing

---

### Frontmatter CLI Examples

### Frontmatter CLI Examples

#### 1. **frontmatter-cli.rs**
Full-featured frontmatter template processor with RDF/SPARQL support.

**Usage**:
```bash
cargo run --example frontmatter-cli
```

**Features**:
- RDF ontology parsing
- SPARQL query execution
- Template variable extraction
- YAML frontmatter parsing

#### 2. **simple-frontmatter-cli.rs**
Minimal frontmatter processor for quick template generation.

**Usage**:
```bash
cargo run --example simple-frontmatter-cli
```

**Features**:
- Basic YAML frontmatter parsing
- Template rendering
- Variable substitution

#### 3. **standalone-frontmatter-cli.rs**
Self-contained frontmatter processor with no external dependencies.

**Usage**:
```bash
cargo run --example standalone-frontmatter-cli
```

**Features**:
- Zero-dependency parsing
- Embedded templates
- Minimal footprint

### GenAI Examples

#### 4. **genai_ollama_loop.rs**
Interactive chat loop with Ollama's qwen3-coder:30b model.

**Prerequisites**:
```bash
# 1. Install Ollama: https://ollama.ai
# 2. Start Ollama server
ollama serve

# 3. Pull the model
ollama pull qwen3-coder:30b
```

**Usage**:
```bash
cargo run --example genai_ollama_loop
```

**Features**:
- Interactive coding assistant
- Conversation history
- Token usage tracking
- Commands: 'clear', 'quit', 'exit'

#### 5. **genai_multi_provider_compare.rs**
Compare responses from multiple LLM providers for the same coding question.

**Prerequisites**:
```bash
# Optional - set API keys for providers you want to test
export OPENAI_API_KEY="your-key"
export ANTHROPIC_API_KEY="your-key"
export GEMINI_API_KEY="your-key"

# Ollama is used by default (no API key needed)
```

**Usage**:
```bash
cargo run --example genai_multi_provider_compare
```

**Features**:
- Multi-provider comparison
- Performance benchmarking
- Token usage comparison
- Works with: Ollama, OpenAI, Anthropic, Google Gemini

### Utility Examples

#### 6. **json-to-yaml-frontmatter.rs**
Convert JSON frontmatter to YAML format.

**Usage**:
```bash
cargo run --example json-to-yaml-frontmatter
```

**Features**:
- JSON to YAML conversion
- Frontmatter generation
- RDF/SPARQL support

## Workspace Examples

The following examples are full workspace members with their own Cargo.toml:

- **examples/frontmatter-cli/** - Production-ready frontmatter CLI
- **examples/natural-market-search/** - Natural language market search
- **examples/ai-template-project/** - AI-powered template generation

## Running Examples

```bash
# Run specific example
cargo run --example <example-name>

# Build all examples
cargo build --examples

# List available examples
cargo run --example
```

## Requirements

- Rust 1.70 or later
- For GenAI examples: genai = "0.4"
- For Ollama examples: Ollama server running locally

## Troubleshooting

### 🏥 First Step: Run Doctor
**Always start troubleshooting with:**
```bash
ggen doctor
```

This checks your environment and provides platform-specific fix instructions for any issues.

### 📚 Get Contextual Help
```bash
# Get tips based on your usage patterns
ggen help-me --tips

# Get help for a specific command
ggen help-me <command>
```

### Enhanced Error Messages
Ggen now provides detailed error messages with:
- Context explaining what went wrong
- "Did you mean?" suggestions for typos
- Step-by-step fix instructions
- Platform-specific commands

**Example of a helpful error:**
```bash
$ ggen template generate non-existent
❌ Error: Template 'non-existent' not found

💡 Did you mean:
   • rust-cli-template
   • basic-template

🔧 How to fix:
   1. Run 'ggen template list' to see available templates
   2. Use 'ggen template search <query>' to find templates
```

### Ollama Examples

If genai examples fail:
1. **Run `ggen doctor`** to check prerequisites
2. Check Ollama is running: `curl http://localhost:11434/api/tags`
3. Verify model is installed: `ollama list`
4. Pull model if needed: `ollama pull qwen3-coder:30b`

### Frontmatter Examples

If frontmatter examples fail:
1. **Run `ggen doctor`** to verify environment
2. Check template file exists
3. Verify YAML syntax is valid
4. Ensure RDF/SPARQL syntax is correct

## Contributing

When adding new examples:
1. Add file to `examples/` directory
2. Test compilation: `cargo build --example <name>`
3. Add usage documentation to this README
4. Ensure example is self-contained and working

## 📊 Example Comparison Matrix

| Example | Complexity | Time | Prerequisites | What You Learn |
|---------|-----------|------|---------------|----------------|
| **basic-template-generation** | ⭐ Beginner | 15-30m | ggen only | Template fundamentals |
| **ai-template-creation** | ⭐⭐ Intermediate | 30-45m | ggen (mock) | AI-powered generation |
| **source-code-analysis** | ⭐⭐ Intermediate | 30-45m | ggen (mock) | Pattern extraction |
| **complete-project-generation** | ⭐⭐⭐ Advanced | 45-60m | ggen + Rust | Full project generation |
| **mcp-integration** | ⭐⭐⭐ Advanced | 45-60m | ggen-mcp + AI | MCP protocol integration |
| **mcp-rig-integration** | ⭐⭐⭐⭐ Expert | 60-90m | Rust + MCP + Rig | AI agents with dynamic tools |
| **ultra-fast-deployment** ⚡ | ⭐⭐⭐ Advanced | 5-60m | ggen + cleanroom | <60s production deployment |

## 🎯 Learning Paths

### Path 1: Template Author
1. basic-template-generation (fundamentals)
2. ai-template-creation (AI assistance)
3. source-code-analysis (pattern extraction)

### Path 2: Project Generator
1. basic-template-generation (fundamentals)
2. complete-project-generation (full projects)
3. ai-template-creation (automation)

### Path 3: Integration Developer
1. basic-template-generation (fundamentals)
2. mcp-integration (MCP protocol)
3. complete-project-generation (real workflows)

### Path 4: DevOps Engineer ⚡ (NEW!)
1. basic-template-generation (fundamentals)
2. complete-project-generation (full projects)
3. **ultra-fast-deployment** (<60s production deploys)

**Path 4 is ideal for:**
- Teams needing rapid deployment cycles
- Developers wanting production-ready templates
- Organizations requiring hermetic testing
- Projects needing <60s concept-to-deploy workflows

## 📖 Documentation

Each comprehensive example includes:
- ✅ Complete README with step-by-step instructions
- ✅ Runnable scripts for hands-on learning
- ✅ Expected outputs and success criteria
- ✅ Troubleshooting guides
- ✅ Best practices and tips

## Maintenance

Last audit: 2025-01-13
- Total examples: 6 standalone + 3 workspace + 7 comprehensive tutorials = 16 total
- Compilation status: 100% passing
- New comprehensive examples: 7 (60+ files, ~15,000 lines)
- Latest additions:
  - MCP + Rig integration example (expert level)
  - Ultra-fast deployment workflow (<60s concept-to-deploy)
- All scripts executable and tested
- Dependencies: All up-to-date
- New documentation:
  - [ULTRA_FAST_DEPLOY.md](../docs/ULTRA_FAST_DEPLOY.md) - User guide
  - [ULTRA_FAST_REFERENCE.md](../docs/ULTRA_FAST_REFERENCE.md) - Technical reference
