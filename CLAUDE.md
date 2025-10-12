# Ggen Development Environment - Production CLI Tool

## 🚨 CORE WORKFLOW: Marketplace → Lifecycle → Deploy

**80/20 Rule Applied**: Focus on the 20% of features that provide 80% of development value.

### ⚡ Essential Commands (High Value)
```bash
# 1. Search for proven patterns
ggen market search "rust web service"

# 2. Install required packages
ggen market add "rust-axum-service"
ggen market add "postgresql-database"

# 3. Initialize project structure
ggen lifecycle run init

# 4. Generate using marketplace templates
ggen template generate rust-axum-service:user-service.tmpl

# 5. Test and deploy safely
ggen lifecycle run test
ggen lifecycle validate --env production
ggen lifecycle run deploy --env production
```

### 🎯 Production Readiness (Critical)
```bash
# Check if ready for production
ggen lifecycle readiness

# Validate deployment requirements
ggen lifecycle validate --env production

# Update requirement status
ggen lifecycle readiness-update auth-basic complete
```

## 📋 Core Commands Reference

| Command | Purpose | Example |
|---------|---------|---------|
| `ggen market search <query>` | Find packages | `ggen market search "rust web"` |
| `ggen market add <package>` | Install package | `ggen market add "rust-axum"` |
| `ggen lifecycle run init` | Initialize project | `ggen lifecycle run init` |
| `ggen template generate <template>` | Generate code | `ggen template generate service.tmpl` |
| `ggen lifecycle readiness` | Check production status | `ggen lifecycle readiness` |
| `ggen lifecycle validate` | Validate deployment | `ggen lifecycle validate --env prod` |
| `ggen lifecycle run deploy` | Deploy to environment | `ggen lifecycle run deploy --env prod` |

## 🚨 Production Code Rules

**NEVER use `.expect()` or `.unwrap()` in production:**
```rust
// ❌ BAD - Crashes in production
let result = some_operation().expect("This will crash");

// ✅ GOOD - Handle errors gracefully
let result = some_operation().map_err(|e| anyhow::anyhow!("Context: {}", e))?;
```

## 📦 Marketplace Workflow

```bash
# Search for packages (discover what exists)
ggen market search "rust web service"
ggen market categories

# Install required packages
ggen market add "rust-axum-service"
ggen market add "postgresql-database"
ggen market add "docker-compose"

# Generate using installed templates
ggen template generate rust-axum-service:user-service.tmpl
```

**📚 See:** `docs/marketplace.md` for complete package reference

# Run comprehensive tests
ggen lifecycle run test

# Deploy to staging
ggen lifecycle run deploy --env staging
```

**KEY**: Ggen provides the complete development toolchain - marketplace, lifecycle, templates, AI, and deployment.

## 🚀 Quick Setup

### Install Ggen CLI
```bash
# Install ggen CLI (requires Rust)
cargo install ggen

# Or build from source
cargo build --release
cargo install --path .
```

### Verify Installation
```bash
# Verify ggen installation
ggen --version
ggen market list
ggen lifecycle list
```

## 🎯 Development Workflow (80/20 Rule)

**Focus on high-value activities:**

1. **Search Marketplace** → `ggen market search "your need"`
2. **Install Packages** → `ggen market add "package-name"`
3. **Initialize Project** → `ggen lifecycle run init`
4. **Generate Code** → `ggen template generate template.tmpl`
5. **Validate & Deploy** → `ggen lifecycle validate && ggen lifecycle run deploy`

**📚 See:** `docs/development-workflow.md` for detailed workflows

## 📚 Examples & References

**📖 See:** `examples/microservices-architecture/` - Complete microservices example
**📖 See:** `examples/ai-code-generation/` - AI + marketplace workflow
**📖 See:** `docs/marketplace.md` - Complete package reference

## 📋 Workflow Summary

**✅ DO:** `market search` → `market add` → `lifecycle init` → `template generate` → `lifecycle deploy`
**❌ DON'T:** `cargo init` → manual coding → custom deployment scripts

**📚 See:** `docs/complete-workflow.md` for detailed examples

## 📋 Essential Commands Summary

| Command | Purpose | Example |
|---------|---------|---------|
| `ggen market search <query>` | Find packages | `ggen market search "rust web"` |
| `ggen market add <package>` | Install package | `ggen market add "rust-axum"` |
| `ggen lifecycle run init` | Initialize project | `ggen lifecycle run init` |
| `ggen template generate <template>` | Generate code | `ggen template generate service.tmpl` |
| `ggen lifecycle readiness` | Check production status | `ggen lifecycle readiness` |
| `ggen lifecycle validate` | Validate deployment | `ggen lifecycle validate --env prod` |
| `ggen lifecycle run deploy` | Deploy to environment | `ggen lifecycle run deploy --env prod` |

## 📚 Documentation References

**📖 Core Docs:**
- `README.md` - Project overview and getting started
- `docs/cli.md` - Complete command reference
- `docs/marketplace.md` - Package management guide
- `docs/lifecycle.md` - Project workflow guide

**📖 Examples:**
- `examples/microservices-architecture/` - Production microservices
- `examples/ai-code-generation/` - AI-powered development
- `examples/advanced-rust-project/` - Advanced patterns

**📖 Development:**
- `docs/development-workflow.md` - Complete workflows
- `docs/production-readiness.md` - Production deployment guide

## 📚 Documentation References

**📖 Core Docs:**
- `README.md` - Project overview and getting started
- `docs/cli.md` - Complete command reference
- `docs/marketplace.md` - Package management guide
- `docs/lifecycle.md` - Project workflow guide

**📖 Examples:**
- `examples/microservices-architecture/` - Production microservices
- `examples/ai-code-generation/` - AI-powered development
- `examples/advanced-rust-project/` - Advanced patterns

**📖 Development:**
- `docs/development-workflow.md` - Complete workflows
- `docs/production-readiness.md` - Production deployment guide

## 🎯 80/20 Rule Applied

**Focus on:**
- **Marketplace discovery** (search before building)
- **Production readiness** (validate before deploying)
- **Essential commands** (workflow over features)

**Minimize:**
- Detailed configuration options (for advanced users)
- Internal implementation details (for contributors)
- Advanced edge cases (for power users)

---

**Ggen provides the complete development toolchain - marketplace, lifecycle, templates, AI, and deployment!**
