# TOC Workflow Migration Guide

## Problem: Auto-Commit Causing Merge Conflicts

The previous TOC Generator workflow automatically committed changes to master/main branch whenever documentation was updated. This caused merge conflicts for developers working on branches.

**Previous behavior:**
1. Developer pushes to master
2. TOC Generator runs automatically
3. Bot commits TOC updates to master
4. Other developers get merge conflicts

## Solution: Validation-Only Approach

Changed to **PR validation** pattern following core team best practices:

### Changes Made

#### 1. GitHub Action (`.github/workflows/toc.yml`)

**Before:**
- Triggered on every push to master/main
- Automatically committed TOC updates
- Caused surprise commits and merge conflicts

**After:**
- Only runs on pull requests that modify markdown files
- Validates TOC is up to date (CHECK_ONLY mode)
- **Fails CI if TOC is outdated** - no auto-commit
- Developers must update TOC locally before merging

#### 2. Pre-Commit Hook (`scripts/git-hooks/pre-commit-toc`)

Created local git hook to help developers:
- Automatically updates TOC before committing
- Only processes staged markdown files
- Gracefully handles missing dependencies
- Optional installation (developer choice)

### Developer Workflow

#### Setup (Optional but Recommended)

Install the pre-commit hook to auto-update TOC:

```bash
ln -s ../../scripts/git-hooks/pre-commit-toc .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit
```

#### When Modifying Markdown Files

1. Edit your markdown files (README.md, docs/*.md)
2. Commit your changes:
   - **With hook**: TOC updates automatically
   - **Without hook**: Manually run `npx markdown-toc -i README.md`
3. Push to your branch
4. Create PR
5. CI validates TOC is current
   - ✅ Pass: TOC is up to date
   - ❌ Fail: TOC needs updating (update locally and push)

### Benefits

✅ **No Surprise Commits**: Only developer commits appear in history
✅ **No Merge Conflicts**: No bot pushing to master after your changes
✅ **PR Validation**: TOC issues caught during review, not after merge
✅ **Developer Control**: You own your commits completely
✅ **Clean History**: Git log shows only intentional changes
✅ **Fail Fast**: Problems caught early in PR review

### Core Team Best Practices Applied

1. **CI Validates, Doesn't Modify**: GitHub Actions check code, don't change it
2. **Developer Responsibility**: Developers own their changes end-to-end
3. **No Magic**: No hidden automation changing code unexpectedly
4. **Predictable Workflow**: Same process for all contributors
5. **Local Tools**: Pre-commit hooks help developers before pushing

### Troubleshooting

#### "TOC validation failed in PR"

Your TOC is out of date. Fix it:

```bash
# Update TOC in README
npx markdown-toc -i README.md

# Update TOC in all docs
find docs -name "*.md" -exec npx markdown-toc -i {} \;

# Commit and push
git add .
git commit -m "chore: update TOC"
git push
```

#### "Want to skip pre-commit hook"

```bash
git commit --no-verify
```

But remember: PR CI will still validate TOC.

### Migration Notes

- Old `TOC Generator` workflow replaced with `TOC Validation`
- No action required for existing branches
- Next PR will validate TOC (may fail if outdated)
- Install pre-commit hook to avoid manual TOC updates

### Technical Details

**Workflow Changes:**
- Trigger: `push` → `pull_request` with path filters
- Job name: `generateTOC` → `validate-toc`
- Added: `CHECK_ONLY: 'true'` flag
- Behavior: Generate + commit → Validate only

**Pre-Commit Hook:**
- Location: `scripts/git-hooks/pre-commit-toc`
- Language: POSIX shell (maximum compatibility)
- Dependencies: npx, markdown-toc (optional, graceful fallback)
- Scope: Only staged markdown files with TOC markers

---

**Issue Reference**: GitHub Action was committing to repo causing merge conflicts
**Solution**: Core team best practice - validate in PR, never auto-commit to main
**Status**: ✅ Implemented and tested
