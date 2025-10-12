# ggen Marketplace

**Production-ready package registry hosted on GitHub Pages**

## 🌐 Live Marketplace

- **Registry URL**: `https://seanchatmangpt.github.io/ggen/marketplace/registry/packages.toml`
- **Repository**: `https://github.com/seanchatmangpt/ggen`
- **Documentation**: `https://seanchatmangpt.github.io/ggen/`

## 📦 Available Packages

Browse and install packages using:

```bash
# Search packages
ggen market search "rust api"

# View package details
ggen market info "advanced-rust-api-8020"

# Install package
ggen market add "advanced-rust-api-8020"

# List installed packages
ggen market list
```

## 🚀 Publishing Packages

### Prerequisites

1. GitHub account
2. Fork the ggen repository
3. Package must follow ggen structure

### Publishing Steps

1. **Create your package** in `marketplace/packages/your-package-name/`
2. **Add to registry** in `marketplace/registry/packages.toml`:

```toml
[[package]]
name = "your-package-name"
full_name = "yourusername/your-package-name"
version = "0.1.0"
description = "Your package description"
category = "templates"  # or "utilities", "ai", etc.
author = "your-github-username"
repository = "https://github.com/seanchatmangpt/ggen"
download_url = "https://github.com/seanchatmangpt/ggen/archive/refs/heads/master.zip"
path = "marketplace/packages/your-package-name"
license = "MIT"
dependencies = []
features = ["Feature 1", "Feature 2"]
tags = ["tag1", "tag2"]
keywords = ["keyword1", "keyword2"]
```

3. **Submit Pull Request** to main repository
4. **CI/CD Deployment**: Once merged, package is automatically deployed to GitHub Pages

### Using CLI to Publish

```bash
# From your package directory
cd marketplace/packages/your-package-name

# Publish to marketplace (creates PR automatically)
ggen market publish --tag stable

# Publish with dry-run to test
ggen market publish --dry-run
```

## 🏗️ Package Structure

```
marketplace/packages/your-package-name/
├── make.toml              # Lifecycle configuration
├── README.md              # Package documentation
├── src/                   # Source code
├── templates/             # Code generation templates (optional)
├── data/                  # SPARQL/RDF specifications (optional)
└── tests/                 # Test files
```

## 🎯 80/20 Production Deployment

### Critical (Production)

- ✅ **GitHub Pages Hosting**: Registry served via HTTPS
- ✅ **Git-based Distribution**: Packages via GitHub releases/archives
- ✅ **Automatic CI/CD**: Deploy on merge to master
- ✅ **Versioning**: Semantic versioning support
- ✅ **Search & Discovery**: Full-text search in registry

### Important (Nice-to-have)

- 🚧 **Package Verification**: GPG signatures (future)
- 🚧 **Download Statistics**: Track package downloads
- 🚧 **Dependency Resolution**: Automatic dep installation
- 🚧 **Package Ratings**: Community ratings/reviews

### Future

- ⏳ **Private Registries**: Organization-specific registries
- ⏳ **Package Mirrors**: CDN distribution
- ⏳ **Build Artifacts**: Pre-compiled binaries

## 📚 GitHub Pages Setup

### Enable GitHub Pages

1. Go to repository **Settings** → **Pages**
2. Source: **Deploy from a branch**
3. Branch: **master** (or **main**)
4. Folder: **/ (root)**
5. Click **Save**

### Access Registry

After deployment (2-3 minutes):
- Registry: `https://seanchatmangpt.github.io/ggen/marketplace/registry/packages.toml`
- Packages: `https://seanchatmangpt.github.io/ggen/marketplace/packages/`

### CI/CD Workflow

The marketplace automatically deploys when:
1. PR is merged to master
2. Registry file is updated
3. New packages are added

```yaml
# .github/workflows/marketplace.yml
name: Deploy Marketplace
on:
  push:
    branches: [master]
    paths:
      - 'marketplace/**'
jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Deploy to GitHub Pages
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./marketplace
```

## 🔐 Security

### Package Verification

1. All packages reviewed before merge
2. Source code visible in GitHub repo
3. SHA256 checksums for verification
4. License compliance checked

### Safe Installation

```bash
# Dry-run to preview changes
ggen market add "package-name" --dry-run

# Install from specific version
ggen market add "package-name@1.0.0"

# Install with verification
ggen market add "package-name" --verify
```

## 🤝 Contributing

See [CONTRIBUTING.md](../CONTRIBUTING.md) for guidelines on:
- Package quality standards
- Code review process
- Testing requirements
- Documentation standards

## 📞 Support

- **Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Discussions**: https://github.com/seanchatmangpt/ggen/discussions
- **Documentation**: https://seanchatmangpt.github.io/ggen/

---

**Production Readiness**: 80% (Critical features complete, ready for public use)
