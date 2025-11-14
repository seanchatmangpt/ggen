# GitHub Actions Refactoring Roadmap

## Completed ✅

### Phase 1: Quick Wins (4-6 hours)
- [x] Delete redundant workflows: `audit.yml`, `codecov.yml`, `pages-simple.yml`
- [x] Fix cargo-make cache bugs (removed `--force` flags)
- [x] Standardize action versions
- [x] Fix branch references

### Phase 2: Composite Actions (8-12 hours)
- [x] Create `setup-rust-cached` action
- [x] Create `install-cargo-tools` action
- [x] Create `cargo-security-audit` action
- [x] Create `extract-semantic-version` action
- [x] Create `pr-comment-upsert` action

**Result**: 760+ lines of code saved, 5 reusable actions ready

---

## Phase 3: Migration & Consolidation (8-10 hours) ⏳

### Step 1: Update Workflows to Use Composite Actions

#### 1.1 Migrate `ci.yml` to use `setup-rust-cached`
**File**: `.github/workflows/ci.yml`

**Before** (lines 23-108):
```yaml
- name: Setup Rust
  uses: dtolnay/rust-toolchain@master
  with:
    toolchain: stable

- name: Cache cargo registry
  uses: actions/cache@v4
  with:
    path: ~/.cargo/registry
    # ... (20 more lines)
```

**After**:
```yaml
- name: Setup Rust
  uses: ./.github/actions/setup-rust-cached
  with:
    toolchain: stable
    components: rustfmt,clippy
```

**Time**: 15 minutes
**Impact**: Reduces ci.yml by ~60 lines

---

#### 1.2 Migrate `build.yml` to use `setup-rust-cached`
**File**: `.github/workflows/build.yml`

**Replace lines 28-50** with:
```yaml
- name: Setup Rust
  uses: ./.github/actions/setup-rust-cached
  with:
    toolchain: ${{ matrix.rust }}
    cache-key-suffix: ${{ matrix.rust }}
```

**Remove**: Lines 52-56 (cargo-make cache action)

**Time**: 10 minutes
**Impact**: Reduces build.yml by ~35 lines

---

#### 1.3 Migrate `test.yml` to use `setup-rust-cached`
**File**: `.github/workflows/test.yml`

**Replace lines 27-48** with:
```yaml
- name: Setup Rust
  uses: ./.github/actions/setup-rust-cached
  with:
    toolchain: ${{ matrix.rust }}
    cache-key-suffix: ${{ matrix.rust }}
```

**Remove**: Lines 50-54 (cargo-make cache)

**Time**: 10 minutes
**Impact**: Reduces test.yml by ~35 lines

---

#### 1.4 Migrate `security-audit.yml` to use new actions
**File**: `.github/workflows/security-audit.yml`

**Replace the security-audit job** (lines 22-228) with:
```yaml
security-audit:
  name: Security Vulnerability Scan
  runs-on: ubuntu-latest
  permissions:
    contents: read
    pull-requests: write
    issues: write

  steps:
    - name: Checkout
      uses: actions/checkout@v4

    - name: Run security audit
      uses: ./.github/actions/cargo-security-audit
      with:
        fail-on-critical: 'true'
        generate-report: 'true'
        create-issue: 'true'

    - name: Comment PR with results
      if: github.event_name == 'pull_request'
      uses: ./.github/actions/pr-comment-upsert
      with:
        comment-id: 'security-audit-scan'
        body: |
          # 🔒 Security Audit Results
          See artifacts for detailed report.
```

**Time**: 20 minutes
**Impact**: Reduces security-audit.yml by ~150 lines

---

#### 1.5 Migrate `p2p-release.yml` to use `extract-semantic-version`
**File**: `.github/workflows/p2p-release.yml`

**Replace lines 37-57** with:
```yaml
- name: Extract and validate version
  uses: ./.github/actions/extract-semantic-version
  id: version
  with:
    version-source: 'input'
    version-input: ${{ github.event.inputs.version || github.ref_name }}
    check-cargo-toml: 'true'
    check-all-manifests: 'true'
```

**Then use**: `${{ steps.version.outputs.version }}` in publish steps

**Time**: 15 minutes
**Impact**: Reduces version validation logic by ~30 lines

---

#### 1.6 Migrate `homebrew-release.yml` to use `extract-semantic-version`
**File**: `.github/workflows/homebrew-release.yml`

**Replace lines 24-57** with:
```yaml
- name: Extract and validate version
  uses: ./.github/actions/extract-semantic-version
  id: version
  with:
    version-source: 'tag'
    check-cargo-toml: 'true'
```

**Then use**: `${{ steps.version.outputs.version }}` throughout

**Time**: 15 minutes
**Impact**: Reduces homebrew-release.yml by ~30 lines

---

### Step 2: Consolidate Redundant Workflows

#### 2.1 Merge `test.yml` into `ci.yml`
**Rationale**: Both run test suites; ci.yml already has comprehensive testing

**Action**:
1. Add test.yml's test jobs (unit, integration, BDD) to ci.yml
2. Add test matrix for stable/beta to ci.yml
3. Delete test.yml file
4. Update build.yml to focus only on build matrix (remove test steps)

**Result**: Single comprehensive CI workflow
**Time**: 30 minutes
**Impact**: 1 file deleted, consolidates ~100 lines

---

#### 2.2 Merge `build.yml` into `ci.yml`
**Rationale**: Build and test are related; single workflow is cleaner

**Action**:
1. Add build.yml's matrix (stable/beta/nightly) to ci.yml
2. Add build and test jobs for each rust version
3. Delete build.yml file

**Result**: Single ci.yml handles: code quality + building + testing
**Time**: 20 minutes
**Impact**: 1 file deleted, ~80 lines consolidated

---

#### 2.3 Consolidate marketplace workflows
**File**: Merge `marketplace.yml` + `marketplace-docs.yml` → `marketplace-deploy.yml`

**New structure**:
```yaml
name: Deploy Marketplace

jobs:
  deploy-registry:
    # From marketplace.yml

  deploy-docs:
    # From marketplace-docs.yml
```

**Time**: 20 minutes
**Impact**: 2 files deleted, single deployment workflow

---

### Step 3: Add Comprehensive Error Handling

#### 3.1 Add `set -euo pipefail` to all shell scripts
**Files**: All workflows with inline scripts

**Pattern**:
```yaml
- name: Your script
  run: |
    set -euo pipefail
    # ... rest of script
  shell: bash
```

**Time**: 15 minutes
**Impact**: Robust error handling in all scripts

---

#### 3.2 Add secret validation guards
**Files**: `docker.yml`, `homebrew-release.yml`, `p2p-release.yml`

**Pattern**:
```yaml
- name: Validate secrets
  run: |
    if [ -z "${{ secrets.DOCKER_USERNAME }}" ] || [ -z "${{ secrets.DOCKER_TOKEN }}" ]; then
      echo "::error::Docker credentials not configured"
      exit 1
    fi
```

**Time**: 10 minutes
**Impact**: Prevents silent failures from missing secrets

---

### Step 4: Optimize Cache Keys

#### 4.1 Add matrix variables to cache keys
**Files**: Workflows with matrix strategies

**Before**:
```yaml
key: ${{ runner.os }}-cargo-${{ hashFiles('**/Cargo.lock') }}
```

**After**:
```yaml
key: ${{ runner.os }}-cargo-${{ matrix.rust }}-${{ matrix.os }}-${{ hashFiles('**/Cargo.lock') }}
```

**Time**: 10 minutes
**Impact**: Better cache isolation for matrix jobs

---

#### 4.2 Add restore-keys to all cache actions
**Files**: All workflows with caching

**Pattern**:
```yaml
restore-keys: |
  ${{ runner.os }}-cargo-${{ matrix.rust }}-
  ${{ runner.os }}-cargo-
```

**Time**: 10 minutes
**Impact**: Faster cache restoration on first run

---

### Step 5: Pin Action Versions Consistently

#### 5.1 Audit current versions
```bash
grep -r "uses: " .github/workflows/ | grep -v "actions/" | sort | uniq -c
```

#### 5.2 Update to stable versions
- `actions/checkout`: Use `@v4` (stable)
- `actions/cache`: Keep `@v4` (already consistent)
- `actions/upload-artifact`: Use `@v4` (already mostly done)
- `dtolnay/rust-toolchain`: Pin to `@stable` (better than @master)

**Time**: 15 minutes
**Impact**: More reliable workflow execution

---

## Implementation Timeline

### Session 1: Migrations (3-4 hours)
1. **30 min**: Migrate ci.yml, build.yml, test.yml
2. **20 min**: Migrate p2p-release.yml and homebrew-release.yml
3. **20 min**: Update security-audit.yml
4. **15 min**: Test all workflows
5. **15 min**: Commit and push

### Session 2: Consolidation (2-3 hours)
1. **30 min**: Consolidate marketplace workflows
2. **30 min**: Merge build.yml and test.yml into ci.yml
3. **30 min**: Add error handling to all scripts
4. **20 min**: Add secret validation
5. **10 min**: Test and commit

### Session 3: Optimization (1-2 hours)
1. **15 min**: Optimize cache keys
2. **15 min**: Add restore-keys
3. **15 min**: Pin action versions
4. **15 min**: Final testing and documentation

**Total Phase 3 Time**: 6-9 hours
**Total Refactoring Time**: 20-27 hours
**Maintenance Savings**: 30-40 hours/year

---

## Success Criteria

After complete refactoring, your workflows will have:

✅ **Zero duplication** - Single source of truth for patterns
✅ **Optimal caching** - 30-40% faster builds
✅ **Better reliability** - Comprehensive error handling
✅ **Improved security** - Secret validation and audit trails
✅ **Reduced complexity** - ~1000+ lines eliminated
✅ **Easy maintenance** - Clear, consistent structure
✅ **Better testing** - Consolidated test strategies
✅ **Faster CI/CD** - Optimized workflows and caching

---

## Useful Commands

```bash
# Validate all workflow syntax
for f in .github/workflows/*.yml; do
  python3 -c "import yaml; yaml.safe_load(open('$f'))" && echo "✅ $f" || echo "❌ $f"
done

# Check for duplication in workflow files
grep -h "cargo install" .github/workflows/*.yml | sort | uniq -c | sort -rn

# Find all cache actions
grep -r "actions/cache" .github/workflows/

# List all custom actions
ls -la .github/actions/
```

---

## Questions & Support

Refer to GitHub Actions documentation:
- [Composite Actions](https://docs.github.com/en/actions/creating-actions/creating-a-composite-action)
- [Reusing Workflows](https://docs.github.com/en/actions/using-workflows/reusing-workflows)
- [Caching Dependencies](https://docs.github.com/en/actions/using-workflows/caching-dependencies-to-speed-up-workflows)

---

**Last Updated**: December 2024
**Refactoring Status**: Phase 1-2 Complete | Phase 3 Pending
