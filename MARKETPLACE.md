# ggen Marketplace - Production Deployment Guide

## 🎯 80/20 Production Architecture

The ggen marketplace uses a **simple, production-ready architecture** hosted on GitHub Pages.

### Critical Components (20% effort, 80% value)

✅ **GitHub Pages Hosting**
- Static file hosting (free, reliable)
- HTTPS by default
- Global CDN distribution
- No server maintenance

✅ **Git-based Distribution**
- Packages stored in GitHub repository
- Version control built-in
- Download via GitHub releases/archives
- Transparent source code

✅ **TOML Registry**
- Simple, human-readable format
- Easy to edit and review
- Git-based versioning
- No database required

✅ **Automatic CI/CD**
- Deploy on git push
- No manual deployment steps
- Validates registry on merge
- Zero downtime updates

## 🚀 How It Works

### For Users (Installing Packages)

```bash
# 1. Search marketplace (reads from GitHub Pages)
ggen market search "rust api"
# → Fetches: https://seanchatmangpt.github.io/ggen/marketplace/registry/packages.toml

# 2. View package details
ggen market info "advanced-rust-api-8020"
# → Shows package info from registry

# 3. Install package (downloads from GitHub)
ggen market add "advanced-rust-api-8020"
# → Downloads: https://github.com/seanchatmangpt/ggen/archive/master.zip
# → Extracts: examples/advanced-rust-api-8020/
```

### For Publishers (Publishing Packages)

```bash
# 1. Create package in marketplace/packages/
mkdir -p marketplace/packages/my-package
# Add make.toml, src/, README.md, etc.

# 2. Add to registry
# Edit marketplace/registry/packages.toml

# 3. Submit PR to main repo
git add marketplace/
git commit -m "Add my-package to marketplace"
git push origin add-my-package

# 4. Automatic deployment after merge
# → CI/CD validates and deploys to GitHub Pages
# → Package available within 2-3 minutes
```

## 📁 Repository Structure

```
ggen/
├── .github/workflows/
│   └── marketplace.yml          # Auto-deploy to GitHub Pages
├── marketplace/
│   ├── README.md                # Publishing guide
│   ├── index.html               # Web interface (generated)
│   ├── registry/
│   │   └── packages.toml        # Package registry (THE SOURCE OF TRUTH)
│   └── packages/
│       ├── rust-cli-template/   # Example package 1
│       ├── graphql-api-rust/    # Example package 2
│       └── ...                  # More packages
└── examples/
    └── advanced-rust-api-8020/  # Production example (also a package)
```

## 🌐 Production URLs

### GitHub Pages Deployment

- **Marketplace Home**: `https://seanchatmangpt.github.io/ggen/marketplace/`
- **Registry File**: `https://seanchatmangpt.github.io/ggen/marketplace/registry/packages.toml`
- **Package Downloads**: `https://github.com/seanchatmangpt/ggen/archive/refs/heads/master.zip`

### Repository

- **Main Repo**: `https://github.com/seanchatmangpt/ggen`
- **Issues**: `https://github.com/seanchatmangpt/ggen/issues`
- **Pull Requests**: `https://github.com/seanchatmangpt/ggen/pulls`

## 🔧 CLI Implementation

### Registry Loading (80/20 Approach)

```rust
// Load registry from GitHub Pages OR local file
pub async fn load() -> Result<Self> {
    // Try production URL first
    if let Ok(registry) = Self::load_from_url(
        "https://seanchatmangpt.github.io/ggen/marketplace/registry/packages.toml"
    ).await {
        return Ok(registry);
    }

    // Fallback to local file (for development)
    let local_path = Self::default_path_sync()?;
    Self::load_from_path(&local_path).await
}
```

### Package Installation (Git-based)

```rust
// Download package from GitHub
pub async fn install(package_name: &str) -> Result<()> {
    // 1. Get package info from registry
    let registry = Registry::load().await?;
    let package = registry.get_package(package_name)?;

    // 2. Download from GitHub
    let url = package.download_url; // GitHub archive URL
    download_and_extract(url, package.path).await?;

    // 3. Update local lockfile
    update_lockfile(package)?;

    Ok(())
}
```

## 🎯 Why This Approach (80/20)

### ✅ What We DID Implement (Critical 20%)

1. **GitHub Pages hosting** - Free, reliable, HTTPS, CDN
2. **TOML registry** - Simple, git-versioned, human-readable
3. **Git-based distribution** - Transparent, versioned, auditable
4. **Automatic CI/CD** - Deploy on merge, zero-downtime
5. **Full-text search** - In CLI, no external search service needed

### ❌ What We DIDN'T Implement (80% complexity)

1. ~~Custom package server~~ - GitHub Pages is sufficient
2. ~~Database backend~~ - TOML file is enough
3. ~~CDN setup~~ - GitHub provides CDN
4. ~~Authentication system~~ - GitHub accounts handle auth
5. ~~Package upload API~~ - Git workflow is standard
6. ~~Download analytics~~ - Not critical for MVP
7. ~~Package mirrors~~ - Single source is simpler

### 💡 Benefits

- **Zero server costs** - GitHub Pages is free
- **Zero maintenance** - No servers to manage
- **Built-in versioning** - Git handles all versions
- **Transparent** - All code visible in GitHub
- **Standard workflow** - Developers know Git/PR workflow
- **Automatic backups** - Git provides history
- **Global distribution** - GitHub's CDN is worldwide

## 🚢 Deployment Checklist

### Enable GitHub Pages

- [ ] Go to repository Settings → Pages
- [ ] Source: Deploy from a branch
- [ ] Branch: master (or main)
- [ ] Folder: / (root)
- [ ] Save

### Verify Deployment

```bash
# Test registry is accessible
curl https://seanchatmangpt.github.io/ggen/marketplace/registry/packages.toml

# Should return TOML registry content
```

### Test CLI

```bash
# Search should work without local files
ggen market search "rust"

# Should show packages from GitHub Pages registry
```

## 📊 Success Metrics

### For Users

- ✅ Can search packages without setup
- ✅ Can install packages from anywhere
- ✅ Package downloads work globally
- ✅ Registry always up-to-date

### For Publishers

- ✅ Simple PR workflow to publish
- ✅ Automatic validation on merge
- ✅ Package live within 2-3 minutes
- ✅ No manual deployment needed

### For Maintainers

- ✅ Zero hosting costs
- ✅ Zero server maintenance
- ✅ Automatic backups (Git)
- ✅ Full audit trail (Git history)

## 🔐 Security

### Package Verification

- All packages reviewed in PR
- Source code visible in GitHub
- Git history shows all changes
- Community can audit any package

### Installation Safety

```bash
# View what will be installed
ggen market info "package-name"

# Dry-run installation
ggen market add "package-name" --dry-run

# Install specific version
ggen market add "package-name@1.0.0"
```

## 🤝 Contributing Packages

See [marketplace/README.md](marketplace/README.md) for detailed publishing guide.

Quick steps:
1. Create package in `marketplace/packages/your-package/`
2. Add entry to `marketplace/registry/packages.toml`
3. Submit PR
4. Automatic deploy after merge

---

**Production Status**: ✅ **Ready for public use** (80% complete, all critical features working)
