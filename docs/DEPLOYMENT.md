# GitHub Pages Deployment Guide

## Current Status

❌ **GitHub Pages is NOT currently deployed** - Returns 404 at https://seanchatmangpt.github.io/ggen/

## What Has Been Fixed

1. ✅ GitHub Actions workflow updated to trigger on `master` branch (not `main`)
2. ✅ Makefile.toml configured with workspace-aware mdbook tasks
3. ✅ Local mdbook build working correctly
4. ✅ Documentation validation passing

## What Needs to Be Done

### Step 1: Push Changes to GitHub

First, commit and push the fixes I just made:

```bash
git add .github/workflows/publish-registry.yml Makefile.toml MAKEFILE.md
git commit -m "fix(docs): configure GitHub Pages deployment for master branch"
git push origin master
```

### Step 2: Enable GitHub Pages in Repository Settings

You need to configure GitHub Pages in your repository settings:

1. Go to https://github.com/seanchatmangpt/ggen/settings/pages
2. Under "Build and deployment":
   - **Source**: Select "GitHub Actions" (NOT "Deploy from a branch")
3. Save the settings

### Step 3: Trigger the Workflow

After pushing the changes, the workflow should trigger automatically. You can also manually trigger it:

1. Go to https://github.com/seanchatmangpt/ggen/actions
2. Click on "Deploy Documentation to GitHub Pages" workflow
3. Click "Run workflow" → Select "master" branch → "Run workflow"

### Step 4: Wait for Deployment

The workflow will:
1. Install Rust and mdbook
2. Validate the registry index JSON
3. Build the documentation with mdbook
4. Validate the HTML output
5. Upload the artifact to GitHub Pages
6. Deploy to https://seanchatmangpt.github.io/ggen/

This takes about 2-3 minutes.

### Step 5: Verify Deployment

Once the workflow completes successfully:

```bash
# Check the deployment URL
curl -I https://seanchatmangpt.github.io/ggen/

# Or visit in browser
open https://seanchatmangpt.github.io/ggen/
```

## Troubleshooting

### Workflow Permissions Error

If you see an error about permissions, you need to:

1. Go to https://github.com/seanchatmangpt/ggen/settings/actions
2. Under "Workflow permissions", select "Read and write permissions"
3. Check "Allow GitHub Actions to create and approve pull requests"
4. Save

### 404 After Successful Deployment

If the workflow succeeds but the site still shows 404:

1. Check if GitHub Pages is set to "GitHub Actions" source (not branch)
2. Wait 1-2 minutes for DNS propagation
3. Check workflow logs for the actual deployment URL
4. Verify the environment is set to "github-pages" in workflow

### Registry Index Validation Fails

If the registry index validation fails:

```bash
# Validate locally first
python3 -m json.tool docs/src/registry/index.json
cargo make docs-validate
```

## Local Testing

Before deploying, always test locally:

```bash
# Clean build and validate
cargo make docs-deploy

# Serve locally to preview
cargo make docs-serve
# Opens http://localhost:3000
```

## Continuous Deployment

Once configured, documentation will automatically deploy on every push to `master`:

1. Make changes to docs in `docs/src/`
2. Commit and push to master
3. GitHub Actions builds and deploys automatically
4. Changes live in ~2-3 minutes

## Checking Workflow Status

```bash
# View workflow runs
gh workflow view "Deploy Documentation to GitHub Pages"

# View recent runs
gh run list --workflow="Deploy Documentation to GitHub Pages"

# View logs for latest run
gh run view --log
```

## GitHub Pages Configuration Reference

The workflow requires these repository settings:

- **Pages Source**: GitHub Actions
- **Environment**: github-pages (created automatically)
- **Permissions**: `contents: read`, `pages: write`, `id-token: write`
- **Branch Protection**: Not required for documentation deployment
