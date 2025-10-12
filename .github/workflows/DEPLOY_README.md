# GitHub Pages Deployment - Automatic Setup

## ✅ Fully Automatic Deployment

The marketplace deploys **automatically** to GitHub Pages with **zero manual configuration required**.

## 🔧 How It Works

### Option 1: Simple Deployment (Recommended) ✅

**Workflow**: `.github/workflows/pages-simple.yml`

- **Fully automatic** - No GitHub settings needed
- Uses `peaceiris/actions-gh-pages@v3`
- Creates and manages `gh-pages` branch automatically
- Deploys on every push to `master`/`main`

**What happens automatically:**
1. Push to master/main → Workflow triggers
2. Validates `marketplace/registry/packages.toml`
3. Generates `marketplace/index.html`
4. Deploys to `gh-pages` branch
5. GitHub Pages serves content from `gh-pages`

**Live in 2-3 minutes!**

### Option 2: GitHub Pages API Deployment

**Workflow**: `.github/workflows/marketplace.yml`

- Uses newer GitHub Pages API
- Requires one-time manual setup:
  1. Go to Settings → Pages
  2. Source: Deploy from a branch
  3. Branch: gh-pages (will be auto-created)
  4. Save

## 🚀 Testing Deployment

### Check Workflow Status

```bash
# View in GitHub
https://github.com/seanchatmangpt/ggen/actions
```

### Verify Deployment

```bash
# Check if gh-pages branch exists (created automatically)
git fetch origin
git branch -r | grep gh-pages

# Test marketplace URL (after deployment)
curl https://seanchatmangpt.github.io/ggen/marketplace/

# Test registry URL
curl https://seanchatmangpt.github.io/ggen/marketplace/registry/packages.toml
```

### Manual Trigger (if needed)

```bash
# Go to Actions tab in GitHub
# Select "Deploy Marketplace (Simple)"
# Click "Run workflow"
```

## 📁 What Gets Deployed

```
gh-pages branch (auto-created):
├── index.html                    # Generated marketplace homepage
├── README.md                     # Publishing guide
├── VERIFICATION.md               # Verification report
├── registry/
│   └── packages.toml            # Package registry
└── packages/
    └── (package directories)
```

## 🔍 Troubleshooting

### Issue: Workflow not running

**Solution**: Push a change to `marketplace/**`:
```bash
echo "# Test" >> marketplace/README.md
git add marketplace/README.md
git commit -m "Trigger deployment"
git push origin master
```

### Issue: 404 on marketplace URL

**Check:**
1. Workflow completed successfully (green checkmark in Actions)
2. `gh-pages` branch exists: `git branch -r | grep gh-pages`
3. GitHub Pages is serving from `gh-pages` (auto-configured by workflow)

**Manual fix** (rarely needed):
1. Settings → Pages
2. Source: Deploy from a branch
3. Branch: gh-pages
4. Folder: / (root)
5. Save

### Issue: Registry not loading

**Check:**
```bash
# Verify registry file exists in gh-pages
git checkout gh-pages
ls -la registry/packages.toml
git checkout master
```

## ✅ Verification Commands

```bash
# 1. Check marketplace homepage
curl -I https://seanchatmangpt.github.io/ggen/marketplace/

# 2. Check registry
curl https://seanchatmangpt.github.io/ggen/marketplace/registry/packages.toml

# 3. Test CLI integration
ggen market search "rust"
# Should fetch from GitHub Pages URL
```

## 🎯 Expected Behavior

### After First Push to Master:

1. **Immediate**: GitHub Actions workflow starts
2. **1-2 minutes**: Workflow completes, creates `gh-pages` branch
3. **2-3 minutes**: GitHub Pages detects `gh-pages` branch and deploys
4. **3-5 minutes**: Marketplace live at URL

### After Subsequent Pushes:

1. **Immediate**: Workflow starts
2. **1-2 minutes**: Updated content deployed
3. **Live immediately**: Changes visible (CDN may cache for ~1 minute)

## 📊 Status Check

**Deployment is working if:**
- ✅ Workflow shows green checkmark in Actions tab
- ✅ `gh-pages` branch exists in repository
- ✅ `https://seanchatmangpt.github.io/ggen/marketplace/` returns HTML
- ✅ Registry URL returns TOML content
- ✅ `ggen market search` works

## 🚦 Current Status

Run this to verify everything is ready:

```bash
cd /Users/sac/ggen

# Check workflows exist
ls -la .github/workflows/*pages*.yml .github/workflows/marketplace.yml

# Check marketplace files
ls -la marketplace/registry/packages.toml

# Test locally
cargo run --bin ggen -- market search "rust"
```

---

**Summary**: Both workflows are configured for **automatic deployment**. The simple workflow (`pages-simple.yml`) requires **zero manual configuration** and will deploy automatically on the first push to master/main.
