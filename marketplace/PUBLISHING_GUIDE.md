# Package Publishing Guide

Complete guide to creating and publishing packages to the ggen marketplace.

## 🚀 Quick Start (5 Minutes)

```bash
# 1. Create package directory
mkdir -p marketplace/packages/my-package

# 2. Create package files
cd marketplace/packages/my-package
cat > package.toml <<EOF
[package]
name = "my-package"
version = "0.1.0"
description = "My awesome package"
author = "yourusername"
license = "MIT"
EOF

# 3. Add to registry (edit marketplace/registry/packages.toml)
# 4. Test locally
ggen market install "my-package" --dry-run

# 5. Publish (creates PR)
ggen market publish --dry-run
```

## 📋 Prerequisites

Before publishing:
- ✅ GitHub account
- ✅ Forked ggen repository
- ✅ Package tested locally
- ✅ Documentation written
- ✅ License chosen

## 🏗️ Package Structure

### Minimal Package

```
marketplace/packages/my-package/
├── package.toml          # Package metadata (required)
├── README.md             # Documentation (required)
└── src/                  # Source code (required)
    └── lib.rs
```

### Full Package

```
marketplace/packages/my-package/
├── package.toml          # Package metadata
├── README.md             # User documentation
├── make.toml             # Lifecycle management
├── src/                  # Source code
│   ├── lib.rs
│   ├── main.rs
│   └── utils/
├── templates/            # Code generation templates (optional)
│   ├── api.hbs
│   └── config.hbs
├── data/                 # SPARQL/RDF specs (optional)
│   └── schema.ttl
├── tests/                # Test suite
│   ├── integration/
│   └── unit/
├── examples/             # Usage examples (optional)
└── docs/                 # Additional docs (optional)
```

## 📝 Package Metadata (package.toml)

### Minimal Configuration

```toml
[package]
name = "my-package"
version = "0.1.0"
description = "Brief description of your package"
author = "your-github-username"
license = "MIT"
```

### Complete Configuration

```toml
[package]
name = "my-package"
full_name = "username/my-package"
version = "0.1.0"
description = "Comprehensive description of what your package does"
category = "templates"  # templates, utilities, ai, frameworks
author = "your-github-username"
repository = "https://github.com/seanchatmangpt/ggen"
homepage = "https://your-package-site.com"
documentation = "https://docs.your-package.com"
license = "MIT"

# Features list (shown in search results)
features = [
    "Production-ready code generation",
    "Comprehensive error handling",
    "Built-in testing utilities",
    "Docker containerization"
]

# Tags for categorization
tags = ["rust", "api", "rest", "production"]

# Search keywords
keywords = ["api", "rest", "axum", "template"]

# Dependencies (other marketplace packages)
dependencies = []

# Optional metadata
[package.metadata]
min_ggen_version = "2.5.0"
stability = "stable"  # alpha, beta, stable
```

## 📚 Documentation Requirements

### README.md Structure

```markdown
# Package Name

Brief one-line description.

## Features

- Feature 1
- Feature 2
- Feature 3

## Installation

```bash
ggen market install "my-package"
```

## Quick Start

```bash
# Basic usage example
ggen template generate my-package/template.hbs
```

## Usage

### Basic Example

```rust
// Code example here
```

### Advanced Example

```rust
// More complex example
```

## Configuration

Explain configuration options.

## API Reference

Document public APIs if applicable.

## Examples

Link to examples/ directory.

## Troubleshooting

Common issues and solutions.

## Contributing

How users can contribute.

## License

MIT
```

## 🧪 Testing Before Publishing

### 1. Local Installation Test

```bash
# Test installation process
ggen market install "my-package" --dry-run

# Check for errors
ggen market info "my-package"
```

### 2. Template Testing

```bash
# Test all templates
cd marketplace/packages/my-package
ggen template generate templates/*.hbs --output test-output/

# Verify generated code
cargo check --manifest-path test-output/Cargo.toml
```

### 3. Lifecycle Testing

```bash
# Test make.toml phases
ggen lifecycle init
ggen lifecycle run --phases build,test

# Verify all phases work
ggen lifecycle run
```

### 4. Documentation Review

```bash
# Check README renders correctly
markdown-toc README.md

# Validate links
markdown-link-check README.md

# Check spelling
aspell check README.md
```

## 📦 Publishing Process

### Method 1: CLI Publishing (Recommended)

```bash
# 1. Navigate to package
cd marketplace/packages/my-package

# 2. Dry run first
ggen market publish --dry-run

# 3. Review output, then publish
ggen market publish --tag stable

# 4. This creates a PR automatically
```

### Method 2: Manual Publishing

```bash
# 1. Fork repository
gh repo fork seanchatmangpt/ggen

# 2. Create branch
git checkout -b add-my-package

# 3. Add package files
mkdir -p marketplace/packages/my-package
cp -r my-package/* marketplace/packages/my-package/

# 4. Register package
cat >> marketplace/registry/packages.toml <<EOF
[[package]]
name = "my-package"
full_name = "username/my-package"
version = "0.1.0"
description = "My awesome package"
category = "templates"
author = "username"
repository = "https://github.com/seanchatmangpt/ggen"
download_url = "https://github.com/seanchatmangpt/ggen/archive/refs/heads/master.zip"
path = "marketplace/packages/my-package"
license = "MIT"
dependencies = []
features = ["Feature 1", "Feature 2"]
tags = ["tag1", "tag2"]
keywords = ["keyword1", "keyword2"]
EOF

# 5. Commit and push
git add marketplace/
git commit -m "Add my-package to marketplace"
git push origin add-my-package

# 6. Create PR
gh pr create --title "Add my-package to marketplace" \
  --body "New package for [purpose]"
```

## 🔄 Versioning Strategy

### Semantic Versioning (SemVer)

```
MAJOR.MINOR.PATCH

1.0.0 - Initial stable release
1.1.0 - New features, backward compatible
1.1.1 - Bug fixes
2.0.0 - Breaking changes
```

### Version Lifecycle

```toml
# Alpha - Early development
version = "0.1.0-alpha"

# Beta - Feature complete, testing
version = "0.1.0-beta"

# Release Candidate
version = "0.1.0-rc.1"

# Stable Release
version = "0.1.0"

# Updates
version = "0.1.1"  # Bug fix
version = "0.2.0"  # New feature
version = "1.0.0"  # Stable API
```

### Publishing Updates

```bash
# 1. Update version in package.toml
sed -i 's/version = "0.1.0"/version = "0.2.0"/' package.toml

# 2. Update registry
sed -i 's/version = "0.1.0"/version = "0.2.0"/' \
  ../../registry/packages.toml

# 3. Publish update
ggen market publish --tag v0.2.0
```

## ✅ Quality Checklist

Before submitting PR:

### Code Quality
- [ ] All code compiles without warnings
- [ ] Clippy passes: `cargo clippy -- -D warnings`
- [ ] Formatted: `cargo fmt --check`
- [ ] Tests pass: `cargo test`
- [ ] No unused dependencies

### Documentation
- [ ] README.md is complete and clear
- [ ] Examples are tested and work
- [ ] API documentation is accurate
- [ ] CHANGELOG.md updated (for updates)

### Package Structure
- [ ] package.toml has all required fields
- [ ] LICENSE file included
- [ ] .gitignore configured
- [ ] No secrets or credentials committed

### Marketplace Integration
- [ ] Registry entry is correct
- [ ] Package installs cleanly
- [ ] Dependencies resolve
- [ ] Dry-run succeeds

## 🎯 Best Practices

### Package Naming

**Good:**
- `rust-api-template`
- `ai-code-generator`
- `graphql-server-starter`

**Avoid:**
- `my-package` (too generic)
- `test123` (not descriptive)
- `pkg` (unclear purpose)

### Description Writing

**Good:**
```
Production-ready REST API template with authentication,
database integration, and comprehensive testing
```

**Avoid:**
```
A package that does stuff
```

### Feature Lists

**Good:**
```toml
features = [
    "JWT authentication with bcrypt",
    "PostgreSQL with SQLx migrations",
    "OpenAPI/Swagger documentation",
    "Docker containerization ready"
]
```

**Avoid:**
```toml
features = ["Cool stuff", "Works great", "Easy to use"]
```

### Tags & Keywords

**Good:**
```toml
tags = ["rust", "api", "rest", "production", "axum"]
keywords = ["api", "rest", "authentication", "database"]
```

**Avoid:**
```toml
tags = ["awesome", "best", "new"]
keywords = ["package", "code"]
```

## 🔒 Security Guidelines

### What NOT to Include

- API keys or secrets
- Hardcoded passwords
- Private credentials
- Personal information
- Proprietary code

### Safe Practices

```rust
// ✅ Good - Use environment variables
let api_key = env::var("API_KEY")?;

// ❌ Bad - Hardcoded secret
let api_key = "sk_live_abc123";
```

### Security Checklist

- [ ] No secrets in code
- [ ] No secrets in tests
- [ ] No secrets in examples
- [ ] .env files in .gitignore
- [ ] Credentials documented in README

## 🚀 Post-Publishing

### Monitor Your Package

```bash
# Check installation stats (future)
ggen market stats "my-package"

# View download count
curl https://seanchatmangpt.github.io/ggen/marketplace/stats/my-package.json
```

### Maintain Your Package

```bash
# Respond to issues
gh issue list --repo seanchatmangpt/ggen --label "package:my-package"

# Update documentation
# Update examples
# Fix bugs
# Add features
```

### Promote Your Package

- Share on social media
- Write blog posts
- Create tutorials
- Present at meetups

## 🆘 Troubleshooting

### Common Issues

**Package not found after publishing**
```bash
# Check registry file
curl https://seanchatmangpt.github.io/ggen/marketplace/registry/packages.toml
# Wait 2-3 minutes for GitHub Pages deployment
```

**Installation fails**
```bash
# Verify package structure
ls -la marketplace/packages/my-package/
# Check package.toml syntax
toml-cli get package.toml package.name
```

**Templates don't generate**
```bash
# Test template syntax
ggen template validate templates/my-template.hbs
# Check variable names match
```

## 📞 Getting Help

- **Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Discussions**: https://github.com/seanchatmangpt/ggen/discussions
- **Documentation**: https://seanchatmangpt.github.io/ggen/

## 📚 Examples

See published packages for reference:
- `marketplace/packages/advanced-rust-api-8020/`
- `marketplace/packages/comprehensive-rust-showcase/`
- `marketplace/packages/ai-microservice/`

---

**Ready to publish?** Follow the checklist, test thoroughly, and submit your PR!
