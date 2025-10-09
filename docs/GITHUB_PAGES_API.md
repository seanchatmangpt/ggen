# GitHub Pages API Scripts Reference

This document describes the cargo-make scripts for diagnosing and managing GitHub Pages deployment via GitHub's API.

## Quick Start

**Check everything at once:**
```bash
cargo make gh-pages-setup-check
```

This validates:
- ✅ Local documentation build
- ✅ Workflow file configuration
- ✅ mdbook installation
- ✅ GitHub CLI authentication
- ✅ Remote site accessibility

## Available Scripts

### 1. `gh-pages-setup-check` - Comprehensive Validation

**Purpose:** Validates the entire GitHub Pages setup from local build to remote deployment.

```bash
cargo make gh-pages-setup-check
```

**Checks:**
- Local docs build exists (`docs/book/index.html`)
- Workflow configured for correct branch (master)
- mdbook installed locally
- GitHub CLI installed and authenticated
- Remote site returns 200 (not 404)

**Output Example:**
```
🔍 GitHub Pages Setup Validation
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

1️⃣  Checking local build...
   ✅ Local docs built successfully
2️⃣  Checking workflow file...
   ✅ Workflow configured for master branch
3️⃣  Checking mdbook installation...
   ✅ mdbook v0.4.52
4️⃣  Checking GitHub CLI...
   ✅ GitHub CLI authenticated
5️⃣  Checking remote site...
   ❌ Site returns 404 - GitHub Pages may not be configured
      Visit: https://github.com/seanchatmangpt/ggen/settings/pages

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📊 Summary:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Errors:   1
Warnings: 0
```

**Exit Codes:**
- `0` - All checks passed
- `1` - Errors found (see output for details)

---

### 2. `gh-pages-status` - API Status Check

**Purpose:** Query GitHub's API for Pages configuration and deployment status.

```bash
cargo make gh-pages-status
```

**Features:**
- Works with or without GitHub CLI (limited info without)
- Shows Pages configuration (build source, custom domain, etc.)
- Lists recent deployments
- Shows workflow run history
- Tests site accessibility with HTTP request

**Output Sections:**
1. 📄 GitHub Pages Configuration (via API)
2. 🚀 Latest Pages Deployment
3. ⚙️ Recent Workflow Runs
4. 🌐 Site Accessibility Test
5. 📋 Quick Setup Checklist

**Example Output:**
```
🔍 Checking GitHub Pages status for seanchatmangpt/ggen...

📊 Using GitHub CLI for authenticated requests
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📄 GitHub Pages Configuration:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
{
  "url": "https://seanchatmangpt.github.io/ggen/",
  "status": "built",
  "source": {
    "branch": "gh-pages",
    "path": "/"
  }
}

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
🌐 Testing Site Accessibility:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
❌ Site returns 404: https://seanchatmangpt.github.io/ggen/
   HTTP Status: 404

💡 Possible issues:
   1. GitHub Pages not enabled in repository settings
   2. No successful deployment yet
   3. Source not set to 'GitHub Actions'
```

---

### 3. `gh-workflow-status` - Workflow Run History

**Purpose:** View GitHub Actions workflow runs for Pages deployment.

**Requirements:** GitHub CLI (`gh`) must be installed and authenticated.

```bash
cargo make gh-workflow-status
```

**Shows:**
- Recent workflow runs (last 10)
- Latest run details (status, timing, conclusion)
- Helpful commands for viewing logs and rerunning

**Example Output:**
```
⚙️  Checking workflow status for seanchatmangpt/ggen...

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📊 Recent Workflow Runs:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
STATUS      TITLE                              BRANCH  EVENT  ID
✓ completed Deploy Documentation to GitHub P... master  push   12345678
✗ failed    Deploy Documentation to GitHub P... master  push   12345677

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
🔍 Latest Run Details:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✓ master Deploy Documentation to GitHub Pages · 12345678
Triggered via push about 1 hour ago

💡 To view logs: gh run view 12345678 --log
💡 To rerun: gh run rerun 12345678
```

---

### 4. `gh-pages-trigger` - Manual Workflow Trigger

**Purpose:** Manually trigger the Pages deployment workflow.

**Requirements:** GitHub CLI (`gh`) must be installed and authenticated.

```bash
cargo make gh-pages-trigger
```

**Use Cases:**
- Retry failed deployment
- Deploy without pushing commits
- Test workflow after configuration changes

**Example Output:**
```
🚀 Triggering GitHub Pages deployment workflow...

✅ Workflow triggered successfully

💡 Check status with: cargo make gh-workflow-status
💡 Or visit: https://github.com/seanchatmangpt/ggen/actions
```

---

### 5. `gh-pages-logs` - View Deployment Logs

**Purpose:** View detailed logs from the latest deployment workflow run.

**Requirements:** GitHub CLI (`gh`) must be installed and authenticated.

```bash
cargo make gh-pages-logs
```

**Shows:**
- Full logs from all workflow steps
- Build output from mdbook
- Deployment status
- Error messages (if any)

---

### 6. `gh-pages-compare` - Local vs Remote Comparison

**Purpose:** Compare local documentation build with deployed version.

**Requirements:**
- `wget` installed
- Remote site must be accessible (200 status)
- Local build must exist

```bash
cargo make gh-pages-compare
```

**Features:**
- Downloads deployed version
- Compares file counts
- Checks key files (index.html, marketplace.html, searchindex.js)
- Reports size differences

**Example Output:**
```
🔄 Comparing local build with deployed version...

📥 Downloading deployed version...

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📊 File Comparison:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Local files:  156
Remote files: 156

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
🔍 Key File Comparison:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✅ index.html: Same size (16400 bytes)
⚠️  marketplace.html: Different sizes (Local: 14800, Remote: 14614)
✅ searchindex.js: Same size (646228 bytes)

💡 To see detailed diff, check: /var/folders/.../tmp.ABC123
```

---

## Common Workflows

### Initial Setup and Deployment

```bash
# 1. Validate everything is configured correctly
cargo make gh-pages-setup-check

# 2. Fix any errors shown, then build locally
cargo make docs-deploy

# 3. Push changes to trigger deployment
git add .github/workflows/publish-registry.yml Makefile.toml
git commit -m "fix(docs): configure GitHub Pages deployment"
git push origin master

# 4. Check deployment status
cargo make gh-pages-status

# 5. Wait for workflow to complete
cargo make gh-workflow-status

# 6. Verify site is live
curl -I https://seanchatmangpt.github.io/ggen/
```

### Debugging Failed Deployment

```bash
# 1. Check what failed
cargo make gh-workflow-status

# 2. View detailed logs
cargo make gh-pages-logs

# 3. Fix issues locally and test
cargo make docs-deploy

# 4. Retry deployment
git commit --amend --no-edit
git push --force-with-lease origin master

# Or trigger manually without new commit:
cargo make gh-pages-trigger
```

### Verifying Deployment Matches Local Build

```bash
# 1. Build locally
cargo make docs-build

# 2. Compare with deployed version
cargo make gh-pages-compare

# 3. If different, check what changed
cargo make gh-workflow-status
cargo make gh-pages-logs
```

---

## GitHub API Endpoints Used

These scripts query the following GitHub API endpoints:

1. **Pages Configuration:**
   ```
   GET /repos/{owner}/{repo}/pages
   ```
   Returns: build source, custom domain, HTTPS enforcement, etc.

2. **Pages Deployments:**
   ```
   GET /repos/{owner}/{repo}/pages/deployments
   ```
   Returns: deployment history, status URLs, timestamps

3. **Workflow Runs:**
   ```
   GET /repos/{owner}/{repo}/actions/workflows/{workflow_id}/runs
   ```
   Returns: run status, conclusions, timing, artifacts

## Authentication

**Without GitHub CLI:**
- `gh-pages-status` works with public API (limited info)
- Other scripts will fail or show errors

**With GitHub CLI:**
- All scripts work fully
- Authenticated requests have higher rate limits
- Can trigger workflows and view private data

**Setup GitHub CLI:**
```bash
# Install
brew install gh

# Authenticate
gh auth login

# Verify
gh auth status
```

## Environment Variables

The scripts use these environment variables (set in Makefile.toml):

```toml
[env]
GITHUB_REPO = "seanchatmangpt/ggen"
GITHUB_API = "https://api.github.com"
GITHUB_PAGES_URL = "https://seanchatmangpt.github.io/ggen"
```

To override for a different repository:
```bash
GITHUB_REPO=myuser/myrepo cargo make gh-pages-status
```

## Troubleshooting

### "GitHub Pages not configured or not accessible"

**Solution:**
1. Go to https://github.com/seanchatmangpt/ggen/settings/pages
2. Under "Build and deployment", set Source to "GitHub Actions"
3. Save settings
4. Push a commit to trigger deployment

### "No workflow runs found"

**Possible causes:**
- Workflow never ran (push to wrong branch)
- Workflow file has syntax errors
- Workflow name doesn't match

**Solution:**
```bash
# Check workflow file
cat .github/workflows/publish-registry.yml

# Manually trigger
cargo make gh-pages-trigger
```

### "gh CLI not available"

**Solution:**
```bash
brew install gh
gh auth login
```

### "Site returns 404"

**Possible causes:**
1. GitHub Pages not enabled
2. Workflow failed
3. Wrong source configured
4. DNS not propagated yet

**Solution:**
```bash
# Check all possible issues
cargo make gh-pages-setup-check

# View workflow logs
cargo make gh-pages-logs
```

## See Also

- [DEPLOYMENT.md](./DEPLOYMENT.md) - Complete deployment guide
- [MAKEFILE.md](../MAKEFILE.md) - All cargo-make tasks
- [GitHub Pages Documentation](https://docs.github.com/en/pages)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
