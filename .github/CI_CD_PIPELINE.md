# CI/CD Pipeline Configuration Guide

## Overview

The ggen CI/CD pipeline is built with GitHub Actions and split into two tiers:

**PR-gating CI (4 workflows).** Every pull request runs exactly four workflows —
`ci.yml`, `quality.yml`, `docs.yml`, and `example-tpot2.yml`. Only `ci.yml`'s checks
are required to merge; the rest are advisory. See
[`CI_ARCHITECTURE.md`](CI_ARCHITECTURE.md) for the design rationale and
[`CI_CONVENTIONS.md`](CI_CONVENTIONS.md) for the honesty/convention rules.

**Operational CD (push/tag/schedule/manual).** The remaining workflows are
deployment/release machinery and are **not** PR gates:

- **Semantic Versioning**: Automatic version bumping based on conventional commits
- **Multi-Registry Deployment**: Push images to GCP Artifact Registry and GitHub Container Registry
- **Security Scanning**: dependency checks (`cargo audit`), container image scanning (Trivy)
- **Artifact Provenance**: Image signing with cosign, SBOM generation with syft
- **Release automation**: GitHub releases, Debian packages, Homebrew formula, registry publish
- **Automated Rollback**: Health-check-triggered rollbacks with automatic revert

> **Note:** Earlier revisions of this guide documented Helm-chart validation and
> Flux GitOps workflows. Those workflows were **removed** in the CI refactor — this
> repository has no `helm/` or `infra/flux/` directory, so the workflows could never
> run. They have been deleted from this guide; see `CI_ARCHITECTURE.md` for the
> teardown record.

---

## Workflows

### 1. PR-Gating CI (ci.yml + quality.yml + docs.yml + example-tpot2.yml)

The merge gate is four workflows. Only `ci.yml`'s jobs are **required**; the rest are
**advisory** (they run for signal and do not block merge). All cargo jobs first run
the composite action `./.github/actions/setup-ggen-build`, which clones the four
sibling repos (`lsp-max`, `lsp-types-max`, `wasm4pm`, `wasm4pm-compat`) and installs
the pinned `nightly-2026-04-15` toolchain — the only toolchain that compiles this
workspace. The required gate is **deliverable-scoped**: it excludes the `ggen-lsp`,
`ggen-lsp-mcp`, and `ggen-lsp-a2a` leaf crates (see `CI_ARCHITECTURE.md` §4).

#### `ci.yml` — the required gate

**Triggers:** `pull_request`, `push` to `main`, and `merge_group` (so required checks
also run in the merge queue).

**Jobs:**

| Job (`name:`) | Command | Required? |
|---------------|---------|-----------|
| `Check` | `cargo check --workspace --exclude ggen-lsp --exclude ggen-lsp-mcp --exclude ggen-lsp-a2a` | ✅ required |
| `Build` | `cargo build --workspace --exclude …` | ✅ required |
| `Test` | `cargo test --workspace --lib --exclude …` (lib-only, fast PR gate) | ✅ required |
| `Doctest` | `cargo test --doc --workspace --exclude …` | ✅ required |
| `lsp crates (advisory)` | `cargo check -p ggen-lsp -p ggen-lsp-mcp -p ggen-lsp-a2a` | ❌ advisory — surfaces the trio's true (currently red) status; **not** in `ci-status.needs` |
| `CI Status` | aggregate, `needs: [check, build, test, doctest]` | ✅ required (the stable aggregate) |

> **Scope note:** `Test` is intentionally lib-only for a fast PR gate. Integration,
> BDD, and E2E suites are a documented follow-up and are **not** currently gated (see
> `CI_ARCHITECTURE.md` §5).

#### `quality.yml` — advisory quality gates

**Triggers:** `pull_request`, `push` to `main`, `merge_group`.

| Job (`name:`) | Command | Blocking? |
|---------------|---------|-----------|
| `fmt` | `just fmt-check` | blocking **within** quality.yml (fails honestly), but not a required branch-protection check |
| `clippy (advisory)` | `cargo clippy --workspace --all-targets --exclude ggen-lsp --exclude ggen-lsp-mcp --exclude ggen-lsp-a2a` (deliberately **not** `-D warnings`) | advisory |
| `audit (advisory)` | `cargo audit` (warns on advisories) | advisory |
| `slo (advisory)` | `just slo-check` (warns) | advisory |

#### `docs.yml` — advisory docs check

**Triggers:** `pull_request` / `push` filtered to `**/*.md` and `docs/**`. One job,
`links` (a no-cargo relative-link check). Advisory — there is a large pre-existing
backlog of broken links, so it must not block merge.

#### `example-tpot2.yml` — advisory example check

**Triggers:** `pull_request` / `push` filtered to `examples/tpot2-wasm4pm-autoconfig/**`.
One job, `verify` — runs the example's Python 3.11 verify scripts. **No cargo**, so no
sibling provisioning. Advisory and path-filtered.

**Usage:**

The four workflows run automatically on every PR. To view results:

```bash
# List recent runs of the required gate
gh run list --workflow=ci.yml

# List runs of the advisory quality gate
gh run list --workflow=quality.yml

# View a specific run
gh run view <run_id>

# Re-run a failed run (or just the failed jobs)
gh run rerun <run_id>
gh run rerun <run_id> --failed
```

---

### 2. Semantic Release (semantic-release.yml)

Automatically analyzes commits using conventional commits specification and creates releases with proper semantic versioning.

**Triggers:**
- Push to `main` (automatic)
- Manual workflow dispatch

**Commit Types:**
- `feat:` → Minor version bump (e.g., 0.2.0 → 0.3.0)
- `fix:` → Patch version bump (e.g., 0.2.0 → 0.2.1)
- `feat!:` or `breaking:` → Major version bump (e.g., 0.2.0 → 1.0.0)

**Stages:**

1. **Analyze Commits**
   - Examines commits since last tag
   - Categorizes by conventional commit type
   - Calculates next semantic version
   - Generates changelog from commits

2. **Create Release**
   - Updates `Cargo.toml` version
   - Updates `helm/*/Chart.yaml` versions
   - Creates git tag
   - Creates GitHub release with changelog

**Example:**

Commits since last release (v0.2.0):
```
feat(core): Add RDF processing with SPARQL support  → Features
fix(cli): Fix argument parsing bug                  → Bugfixes
chore(deps): Update dependencies                    → Chores
```

Result: → Creates v0.3.0 (minor version bump)

**Configuration:**

Edit the version analysis logic to customize release type detection:

```yaml
# Look for this section in semantic-release.yml
HAS_BREAKING=$(echo "$COMMITS" | grep -E "^[a-f0-9]+ (feat|fix|refactor)(\(.+\))?!:" | wc -l)
HAS_FEATURE=$(echo "$COMMITS" | grep -E "^[a-f0-9]+ feat(\(.+\))?:" | wc -l)
HAS_BUGFIX=$(echo "$COMMITS" | grep -E "^[a-f0-9]+ fix(\(.+\))?:" | wc -l)
```

**Usage:**

```bash
# Create release manually
gh workflow run semantic-release.yml --ref main

# View recent releases
gh release list

# Create release from specific commit
git commit --allow-empty -m "chore(release): trigger new release"
git push origin main
```

---

### 3. Docker Build & Push (docker-build-push.yml)

Builds Docker images, scans for vulnerabilities, signs with cosign, and generates SBOMs.

**Triggers:**
- Push to `main`, `develop`
- Tag pushes (`v*`)
- Manual workflow dispatch

**Stages:**

1. **Build Controller Image**
   - Multi-stage Dockerfile build
   - Docker Buildx for caching
   - Push to GCP Artifact Registry
   - Push to GitHub Container Registry

2. **Build CLI Image**
   - Builds Rust binary
   - Creates minimal Docker image
   - Push to both registries

3. **Scan Images** (Trivy)
   - Container vulnerability scanning
   - SARIF format reports
   - Upload to GitHub Security tab

4. **Sign & Generate SBOM**
   - Sign images with cosign (Sigstore)
   - Generate SPDX and CycloneDX SBOMs
   - Attach attestations to images
   - Upload SBOMs as artifacts

**Output Images:**

- `artifacts.example.com/catalog-controller:latest`
- `ghcr.io/seanchatmangpt/catalog-controller:latest`
- `artifacts.example.com/ggen-cli:latest`
- `ghcr.io/seanchatmangpt/ggen-cli:latest`

**Configuration:**

Update registries and repository names:

```yaml
env:
  REGISTRY_GCP: artifacts.example.com
  REGISTRY_GHCR: ghcr.io
  IMAGE_NAME_CONTROLLER: catalog-controller
  IMAGE_NAME_CLI: ggen-cli
```

**Secrets Required:**

```
GCP_ARTIFACT_REGISTRY_KEY    - JSON key for GCP Artifact Registry
GITHUB_TOKEN                 - GitHub token for GHCR (auto-provided)
```

**Usage:**

```bash
# Build and push images manually
gh workflow run docker-build-push.yml --ref main

# View image tags
docker image ls | grep -E "(catalog-controller|ggen-cli)"

# Verify image signature
cosign verify ghcr.io/seanchatmangpt/ggen-cli:latest

# Check SBOM
cosign find attestations ghcr.io/seanchatmangpt/ggen-cli:latest | jq .
```

---

### 4. Generate Release Notes (generate-release-notes.yml)

Automatically generates release notes from commit history using conventional commits.

**Triggers:**
- GitHub release creation
- Manual workflow dispatch with tag input

**Stages:**

1. **Analyze Commits** (2 min)
   - Gets commits between releases
   - Categorizes by type:
     - 🚨 Breaking Changes
     - ✨ Features
     - 🐛 Bug Fixes
     - ⚡ Performance
     - 🔧 Chores

2. **Generate Changelog** (1 min)
   - Creates formatted markdown
   - Includes commit hashes
   - Counts commits and contributors

3. **Update Release Notes** (1 min)
   - Updates GitHub release body
   - Appends to CHANGELOG.md
   - Commits changelog to repository

**Example Output:**

```markdown
## Release v0.3.0 - 2026-01-25

### 🚨 Breaking Changes
- **BREAKING**: Changed RDF triple format (abc1234)

### ✨ Features
- Add SPARQL query builder (def5678)
- Implement template rendering (ghi9012)

### 🐛 Bug Fixes
- Fix memory leak in parser (jkl3456)

### 📊 Summary
- **Commits**: 8
- **Contributors**: 3
```

**Configuration:**

Update comparison logic to customize changelog generation:

```bash
# Look for this pattern
HAS_BREAKING=$(echo "$COMMITS" | grep -E "^[a-f0-9]+ (feat|fix)!:" | wc -l)
```

**Usage:**

```bash
# Trigger release notes generation
gh release create v0.3.0 --title "Version 0.3.0"

# View release notes
gh release view v0.3.0

# Update existing release notes
gh release edit v0.3.0 --notes "Updated notes"
```

---

### 5. Automated Rollback (automated-rollback.yml)

Health-check-triggered automatic rollbacks with manual override capability.

**Triggers:**
- Scheduled: Every 5 minutes (health checks)
- Manual workflow dispatch with environment, release, and reason

**Stages:**

1. **Health Check** (5 min, scheduled only)
   - Monitors all deployments
   - Checks pod status
   - Verifies service endpoints
   - Outputs health status
   - Triggers auto-rollback if unhealthy

2. **Auto-rollback Trigger** (immediate)
   - Triggered by health check failure
   - Calls workflow dispatch with auto-detected release

3. **Prepare Rollback** (2 min)
   - Determines current and previous releases
   - Validates target release exists
   - Gathers rollback metadata

4. **Execute Rollback** (5 min)
   - Helm rollback to previous version
   - Waits for deployment readiness
   - Verifies rollback success
   - Creates incident record

5. **Post-Rollback Verification** (5 min)
   - Checks cluster state
   - Verifies deployment health
   - Reports summary

**Rollback Methods:**

```bash
# Manual rollback to specific version
gh workflow run automated-rollback.yml \
  -f environment=production \
  -f release=v0.2.0 \
  -f reason="Critical bug discovered in v0.3.0"

# Auto-rollback to previous version
gh workflow run automated-rollback.yml \
  -f environment=production \
  -f release=previous \
  -f reason="Automatic rollback: health check failure"
```

**Helm Rollback:**

```bash
# Manual helm rollback
helm rollback tai --namespace production

# View rollback history
helm history tai --namespace production

# Rollback to specific revision
helm rollback tai 3 --namespace production
```

**Incident Recording:**

All rollbacks are recorded in `.rollback-history/`:

```json
{
  "timestamp": "2026-01-25T10:30:00Z",
  "environment": "production",
  "from_release": "v0.3.0",
  "to_release": "v0.2.0",
  "reason": "Critical bug discovered",
  "triggered_by": "github-actions[bot]",
  "workflow_run": "https://github.com/.../runs/12345"
}
```

---

### 6. Other Operational Workflows (by reference)

These workflows exist in `.github/workflows/` and run on push/tag/schedule/manual
triggers — none is a PR gate. They are listed here for completeness; inspect each
file (or `gh workflow view <name>.yml`) for its exact triggers and jobs.

| Workflow | Purpose |
|----------|---------|
| `release.yml` | Build and publish GitHub releases on tag push (`v*`). |
| `release-debian.yml` | Build `.deb` packages for releases. |
| `homebrew-release.yml` | Update the Homebrew formula on release. |
| `publish-registry.yml` | Publish ggen packages to the package registry. |
| `deploy-docs.yml` | Build and deploy the documentation site. |
| `docker.yml` | Auxiliary Docker build (alongside `docker-build-push.yml`). |
| `secrets-sync.yml` | Synchronize repository/organization secrets. |
| `marketplace.yml` | Marketplace top-level orchestration. |
| `marketplace-deploy.yml` | Deploy the marketplace. |
| `marketplace-docs.yml` | Build/publish marketplace docs. |
| `marketplace-test.yml` | Marketplace tests (operational, not a PR gate). |
| `marketplace-validate.yml` | Marketplace pack validation (operational, not a PR gate). |
| `erlang-ci.yml` | Erlang component CI. |
| `erlang-release.yml` | Erlang component release. |

> Use `gh run list --workflow=<name>.yml` to inspect recent runs of any of these.

---

## Environment Configuration

### Secrets Setup

Configure these secrets in **Settings → Secrets and variables → Actions**:

| Secret | Description | Example |
|--------|-------------|---------|
| `GCP_ARTIFACT_REGISTRY_KEY` | JSON key for GCP Artifact Registry | `{"type": "service_account", ...}` |
| `KUBECONFIG_STAGING` | base64-encoded kubeconfig for staging | `LS0tLS1CRUdJTiBDRVJUSUZJQ0FURS...` |
| `KUBECONFIG_PROD` | base64-encoded kubeconfig for production | `LS0tLS1CRUdJTiBDRVJUSUZJQ0FURS...` |
| `PROD_CLUSTER_URL` | Production cluster URL | `https://gke-prod.gcp.io` |

### Variables Setup

Configure these variables in **Settings → Secrets and variables → Variables**:

| Variable | Description | Example |
|----------|-------------|---------|
| `REGISTRY_GCP` | GCP Artifact Registry URL | `artifacts.example.com` |
| `IMAGE_NAME_CONTROLLER` | Controller image name | `catalog-controller` |
| `IMAGE_NAME_CLI` | CLI image name | `ggen-cli` |

### Branch Protection

Configure branch protection in **Settings → Branches → main**. The required
status checks are the deliverable-scoped gate from `ci.yml` (see
`.github/CI_ARCHITECTURE.md` §4 for the authoritative list and rationale —
operational workflows like `docker-build-push` / `semantic-release` are NOT PR
gates):

```
✅ Require status checks to pass (all from ci.yml):
   - Check
   - Build
   - Test
   - Doctest
   - CI Status        # aggregate of the four above

# NON-required (advisory; run for signal, do not block):
#   clippy / audit / slo (quality.yml), example-tpot2, docs (links),
#   lsp-crates (ci.yml advisory — promote once ggen-lsp tracks a pinned lsp-max)

✅ Require code reviews before merging
✅ Require approval from code owners
✅ Require branches to be up to date
✅ Require merge queue (gates also run on merge_group)

✅ Require deploy reviews before merging
   - Environment: production
```

---

## Customization Guide

### Add a New PR Check

Add a required check by adding a job to `ci.yml` and wiring it into the `ci-status`
aggregate (so branch protection keeps tracking a single stable check). Add an
advisory check by adding a job to `quality.yml` instead. Cargo jobs must call the
`setup-ggen-build` action first and use the deliverable scope
(`--exclude ggen-lsp --exclude ggen-lsp-mcp --exclude ggen-lsp-a2a`):

```yaml
# In ci.yml — a new REQUIRED gate
my-check:
  name: My Check
  runs-on: ubuntu-latest
  timeout-minutes: 30
  steps:
    - uses: actions/checkout@<sha> # vX.Y.Z
    - uses: ./.github/actions/setup-ggen-build
    - run: cargo <command> --workspace --exclude ggen-lsp --exclude ggen-lsp-mcp --exclude ggen-lsp-a2a

# Then add it to the aggregate so it becomes part of the `CI Status` required check:
ci-status:
  needs: [check, build, test, doctest, my-check]
```

No `continue-on-error` — checks must fail honestly (see `CI_CONVENTIONS.md`).

### Change Release Type Logic

Edit `semantic-release.yml` to customize version bumping:

```yaml
# Change trigger for major version
if [[ "$MSG" =~ ^breaking: ]]; then
  # Custom breaking change detection
fi
```

### Add New Build Target

Add to `docker-build-push.yml`:

```yaml
build-new-image:
  name: Build New Image
  runs-on: ubuntu-latest
  steps:
    # Build steps here
```

---

## Monitoring & Debugging

### View Workflow Runs

```bash
# List all workflow runs
gh run list

# List runs for specific workflow
gh run list --workflow=ci.yml

# List runs for specific branch
gh run list --branch=main

# List failed runs
gh run list --status=failure
```

### View Run Details

```bash
# Get summary
gh run view <run-id>

# Get full logs
gh run view <run-id> --log

# Watch run in real-time
gh run watch <run-id>
```

### Re-run Workflows

```bash
# Re-run specific workflow
gh run rerun <run-id>

# Re-run failed jobs
gh run rerun <run-id> --failed
```

### View Job Artifacts

```bash
# List artifacts
gh run list --json artifacts

# Download specific artifact
gh run download <run-id> -n artifact-name
```

---

## Common Issues & Solutions

### Issue: `ci.yml` Check/Build/Test failed — compilation error

**Solution:**

The required gate (`ci.yml`) builds with the pinned `nightly-2026-04-15` toolchain and
the four provisioned siblings. Reproduce locally with the same scope:

```bash
# Compile the shippable deliverable (same exclusions as the required gate)
cargo check --workspace --exclude ggen-lsp --exclude ggen-lsp-mcp --exclude ggen-lsp-a2a

# Or use the project entry point
just check && just test-lib
```

Then fix the errors and push. Note: failures in the advisory `lsp crates` job
(`ggen-lsp` / `ggen-lsp-mcp` / `ggen-lsp-a2a`) do **not** block merge — that job is
intentionally non-required until `ggen-lsp` compiles against a pinned `lsp-max`.

### Issue: "Docker image build timeout"

**Solution:**

1. Check Docker build output: `gh run view <run-id> --log`
2. Clear Docker cache: `docker image prune -a`
3. Increase timeout in workflow:
   ```yaml
   timeout-minutes: 40  # Increase from 30
   ```

### Issue: "Health check failed - rollback triggered"

**Solution:**

1. Check cluster state: `kubectl get pods -n production`
2. View pod logs: `kubectl logs <pod-name> -n production`
3. Check deployment status: `kubectl rollout status deployment/tai-controller -n production`
4. Manual verification: `kubectl describe pod <pod-name> -n production`

---

## Performance Optimization

### Cache Optimization

The CI pipeline uses GitHub Actions caching for:
- Rust build artifacts: `Swatinem/rust-cache` (commit-pinned inside the shared
  `./.github/actions/setup-ggen-build` composite action, so every cargo job caches
  consistently)
- Docker layers: `docker/build-push-action` with `cache-from: type=gha`

To clear caches:

```bash
# Using GitHub CLI
gh api repos/seanchatmangpt/ggen/actions/cache -X GET | jq '.actions_caches[].id' | \
  while read id; do
    gh api repos/seanchatmangpt/ggen/actions/cache/$id -X DELETE
  done
```

### Parallel Job Optimization

Jobs run in parallel when possible:
- In `ci.yml`, the `check`, `build`, `test`, `doctest`, and advisory `lsp-crates`
  jobs all run in parallel; `ci-status` fans in last (`needs: [check, build, test,
  doctest]`).
- In `quality.yml`, the `fmt`, `clippy`, `audit`, and `slo` jobs all run in parallel
  (no inter-job `needs:`).

Adjust concurrency with:

```yaml
concurrency:
  group: ${{ github.workflow }}-${{ github.ref }}
  cancel-in-progress: true
```

---

## Security Best Practices

1. **Secret Management**
   - Use GitHub Secrets for sensitive data
   - Rotate credentials regularly
   - Never commit secrets to git

2. **Image Signing**
   - All images signed with cosign
   - Verify signatures before deployment
   - Store attestations in registry

3. **Access Control**
   - Require approval for production deployments
   - Use GitHub environments
   - Audit all deployments

4. **Dependency Management**
   - Regular `cargo audit` runs
   - `cargo deny` for license compliance
   - Dependabot for automated updates

---

## Related Documentation

- [`CI_ARCHITECTURE.md`](CI_ARCHITECTURE.md) — PR-gating CI design and rationale
- [`CI_CONVENTIONS.md`](CI_CONVENTIONS.md) — honesty/convention rules for workflows
- [`CI_TEARDOWN.md`](CI_TEARDOWN.md) — per-workflow teardown decision record
- [Conventional Commits](https://www.conventionalcommits.org/)
- [Semantic Versioning](https://semver.org/)
- [cosign Documentation](https://docs.sigstore.dev/cosign/overview/)
- [syft Documentation](https://github.com/anchore/syft)

