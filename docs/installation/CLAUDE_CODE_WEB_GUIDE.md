# Using ggen in Claude Code Web

**Complete guide for running ggen in Claude Code Web environments.**

## Quick Start (2 Minutes)

### Step 1: Install ggen

In any Claude Code Web session, run:

```bash
cargo install ggen
```

**Time**: ~45 seconds
**What happens**: ggen binary compiled and installed to `~/.cargo/bin`

### Step 2: Verify Installation

```bash
ggen --version
# Output: ggen 3.2.0
```

### Step 3: Initialize Your First Project

```bash
mkdir my-ggen-project && cd my-ggen-project
ggen init
```

### Step 4: Generate Code

```bash
# Preview changes without writing
ggen sync --dry_run true

# Actually generate
ggen sync
```

That's it! You now have a working ggen project.

---

## Installation Methods for Claude Code Web

### Method 1: cargo install (RECOMMENDED)

**Fastest in Claude Code Web** - uses pre-installed Rust toolchain

```bash
# Basic installation
cargo install ggen

# Install with OpenTelemetry instrumentation
cargo install ggen --features otel

# Install from main branch for latest development
cargo install --git https://github.com/seanchatmangpt/ggen --branch main
```

**Pros**:
- ✅ Pre-installed Rust (no extra setup)
- ✅ Automatic binary caching
- ✅ ~45 seconds installation
- ✅ Easy updates: `cargo install ggen --force`

**Cons**:
- ⏱️ Requires Rust compilation

### Method 2: Pre-built Binary (Fast Alternative)

**If installation is too slow**, use pre-built binary:

```bash
# Download latest release
curl -L https://github.com/seanchatmangpt/ggen/releases/download/v3.2.0/ggen-x86_64-unknown-linux-gnu.tar.gz \
  | tar xz -C $HOME/.cargo/bin

# Verify
ggen --version
```

**Pros**:
- ✅ ~15 seconds installation
- ✅ No compilation needed
- ✅ Instant availability

**Cons**:
- ⏱️ Manual binary management

### Method 3: Auto-Install via SessionStart Hook

**For consistent setup across sessions:**

Create or update `.claude/settings.json`:

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
            "command": "cargo install ggen --locked",
            "timeout": 120,
            "onError": "warn"
          }
        ]
      }
    ]
  }
}
```

This automatically installs ggen when you start a new Claude Code Web session.

---

## Working with ggen in Claude Code Web

### Directory Structure Setup

```bash
# Create a well-organized workspace
mkdir -p my-project/{.specify/specs,templates,src}
cd my-project

# Initialize ggen
ggen init

# Create a simple ontology
cat > .specify/specs/001-api/.ttl <<'EOF'
@prefix : <https://example.org/> .
@prefix dcat: <http://www.w3.org/ns/dcat#> .

:MyAPI a :APISpecification ;
  :name "My API" ;
  :version "1.0.0" .
EOF

# Generate code
ggen sync --audit true
```

### Workflow Example: Generate a REST API

**Step 1: Define ontology in `.specify/api.ttl`**

```turtle
@prefix : <https://example.org/api/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

:UserAPI a :RESTEndpoint ;
  :path "/users" ;
  :method "GET" ;
  :responseType :UserList .

:UserList a :CollectionType ;
  :itemType :User .

:User a :ResourceType ;
  :fields ( :id :name :email ) .
```

**Step 2: Create Tera template in `templates/api.rs.tera`**

```tera
// Generated REST API endpoints
{% for endpoint in endpoints %}
#[get("{{ endpoint.path }}")]
pub async fn {{ endpoint.method | lower }}_{{ endpoint.name }}() -> Json<{{ endpoint.responseType }}> {
    // Generated implementation
}
{% endfor %}
```

**Step 3: Generate**

```bash
ggen sync --audit true

# View results
ggen sync --dry_run true  # Preview first
```

### Common Claude Code Web Patterns

#### Pattern 1: Multi-Service Generation

```bash
# Generate multiple services from single ontology
ggen sync --force true

# Services available:
# - Backend Rust code
# - Frontend TypeScript types
# - Database migrations
# - Infrastructure as Code
```

#### Pattern 2: Incremental Development

```bash
# Watch mode for continuous generation
ggen sync --watch true

# Edit .specify/*.ttl files
# Changes automatically regenerate code
```

#### Pattern 3: CI/CD Pipeline

```bash
# Validation only (no generation)
ggen sync --validate_only true

# Full sync with audit trail
ggen sync --audit true

# Safe overwrite with force flag
ggen sync --force true --audit true
```

#### Pattern 4: Integration with Claude Code Tasks

```bash
# Use & prefix to send task to Claude Code Web
& cargo install ggen && ggen init && ggen sync --dry_run true

# Or from Claude: Use Task tool directly
# claude code will handle the ggen command
```

---

## Claude Code Web Constraints and Solutions

### Constraint 1: Network Limited (No apt-get)

**Problem**: `apt-get install ggen` won't work

**Solution**: Use language-specific package managers
```bash
# ✅ Works - cargo (pre-installed)
cargo install ggen

# ❌ Won't work - apt-get (blocked)
apt-get install ggen
```

### Constraint 2: Sandbox Isolation

**Problem**: Limited filesystem access

**Solution**: Work within project directory
```bash
# ✅ Works - within project directory
cd my-project
ggen sync

# ❌ Won't work - system paths
ggen sync --output /usr/local/bin/  # Permission denied
```

### Constraint 3: Timeout Limits (5-30s per command)

**Problem**: Long-running commands may timeout

**Solution**: Use quick validation
```bash
# ✅ Fast (~5s) - validation only
ggen sync --validate_only true

# ⏱️ Medium (~15s) - generation
ggen sync

# ⏱️ Slow (~30s+) - generation + audit
ggen sync --audit true
```

### Constraint 4: Limited System Calls

**Problem**: Some operations blocked

**Solution**: Use ggen's abstractions
```bash
# Instead of: system("rustfmt ...")
# Use: ggen sync (auto-formats via Tera)
```

---

## Performance Optimization for Claude Code Web

### Tip 1: Cache cargo Registry

Add to `.claude/settings.json`:

```json
{
  "environment": {
    "CARGO_HOME": ".cargo",
    "CARGO_NET_RETRY": "10"
  }
}
```

### Tip 2: Use Pre-built Binaries

First session with ggen:

```bash
curl -L https://github.com/seanchatmangpt/ggen/releases/download/v3.2.0/ggen-x86_64-unknown-linux-gnu.tar.gz | tar xz
export PATH="$PWD:$PATH"
```

### Tip 3: Dry-Run Before Full Generation

```bash
# Always preview first (5s)
ggen sync --dry_run true

# Then full generation (15s)
ggen sync
```

### Tip 4: Compress Ontologies

Keep `.specify/*.ttl` files small:

```bash
# ✅ Fast - Simple ontologies
wc -l .specify/specs/*.ttl  # < 500 lines each

# ⏱️ Slow - Complex ontologies
# Solution: Split into multiple files
mv .specify/specs/large.ttl .specify/specs/large/{part1,part2}.ttl
```

---

## Troubleshooting in Claude Code Web

### Issue: "cargo: command not found"

**Status**: Very rare (Rust pre-installed in Claude Code Web)

**Solution**:

```bash
# Check if Rust installed
rustc --version

# If not, install via SessionStart hook
# Add to .claude/settings.json
```

### Issue: Installation very slow

**Cause**: Network latency or large dependency compilation

**Solution**:

```bash
# Use pre-built binary instead
curl -L https://github.com/seanchatmangpt/ggen/releases/download/v3.2.0/ggen-x86_64-unknown-linux-gnu.tar.gz | tar xz

# Or increase network access level to "Limited" (default)
# Check network access settings in session
```

### Issue: "ggen: permission denied"

**Cause**: Binary not marked executable

**Solution**:

```bash
# Make sure cargo/bin is in PATH
export PATH="$HOME/.cargo/bin:$PATH"

# Or manual binary setup
chmod +x ./ggen
./ggen --version
```

### Issue: "RDF file not found" or SPARQL errors

**Solution**:

```bash
# Verify ontology structure
ls -la .specify/specs/

# Validate SHACL conformance
ggen sync --validate_only true

# Debug output
GGEN_LOG_LEVEL=debug ggen sync
```

### Issue: Timeout during generation

**Cause**: Complex ontology or network delay

**Solution**:

```bash
# Check resource usage
cargo make slo-check

# Optimize ontology
# Split large .ttl files
# Remove unused triples
# Simplify SPARQL queries

# Then retry
ggen sync --audit true
```

---

## Advanced Workflows

### Workflow 1: AI-Powered Generation

Use ggen-ai for LLM-enhanced code generation:

```bash
# Install with AI features
cargo install ggen --all-features

# Generate with AI assistance
ggen sync --ai true
```

### Workflow 2: Deterministic Receipts for CI/CD

```bash
# Generate with cryptographic proof
ggen sync --audit true

# View receipt
cat .ggen/receipts/latest.json | jq '.'

# Verify files match hashes
cat .ggen/receipts/latest.json | jq '.files[] | .path, .hash'
```

### Workflow 3: Multi-Language Generation

```bash
# Single ontology generates multiple languages
.specify/api.ttl (source)
  ├── Generated Rust (src/api.rs)
  ├── Generated TypeScript (src/api.ts)
  ├── Generated Python (src/api.py)
  └── Generated Go (src/api.go)

ggen sync
```

---

## Integration with Claude Code Tasks

### From Claude:

Tell Claude: "Use ggen to generate a REST API from the ontology"

Claude will:
1. Use the Task tool to spawn a specialized agent
2. Agent runs: `ggen init && ggen sync`
3. Changes reviewed in diff view before PR

### From Terminal:

```bash
# Send task to Claude Code Web with & prefix
& ggen sync && git add . && git commit -m "Generated code from ontology"
```

---

## Next Steps

1. **Start**: [Getting Started Tutorial](../tutorials/01-getting-started.md)
2. **Learn**: [First Project Guide](../tutorials/02-first-project.md)
3. **Reference**: [Command Reference](../reference/01-commands.md)
4. **Advanced**: [Multi-Service Architecture](../how-to/02-multi-service-generation.md)

---

## Resources

- 📖 [ggen Documentation](../README.md)
- 🔧 [Command Reference](../reference/01-commands.md)
- 🎓 [Tutorial Videos](https://youtube.com/@ggendev)
- 💬 [Community Forum](https://github.com/seanchatmangpt/ggen/discussions)
- 🐛 [Report Issues](https://github.com/seanchatmangpt/ggen/issues)

---

**Happy generating! 🚀**
