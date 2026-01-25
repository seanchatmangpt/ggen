# CI/CD Pipeline Configuration Guide

## Overview

The ggen CI/CD pipeline is a comprehensive, production-ready system built with GitHub Actions that implements:

- **Andon Signals**: Real-time quality gates that stop the line on errors
- **Semantic Versioning**: Automatic version bumping based on conventional commits
- **Multi-Registry Deployment**: Push images to GCP Artifact Registry and GitHub Container Registry
- **Helm Chart Validation**: Lint, template, kubeval, schema validation
- **Security Scanning**: SAST, dependency checks, container image scanning
- **Artifact Provenance**: Image signing with cosign, SBOM generation with syft
- **GitOps with Flux**: Declarative infrastructure, automatic reconciliation
- **Automated Rollback**: Health-check-triggered rollbacks with automatic revert

---

## Workflows

### 1. CI Complete Pipeline (ci-complete.yml)

Main continuous integration pipeline triggered on every push and pull request.

**Triggers:**
- Push to `main`, `develop`, `feature/**`
- Pull requests to `main`, `develop`

**Stages:**

1. **Quick Checks** (2-3 min)
   - Formatting check (`cargo fmt`)
   - Clippy linting (`cargo clippy -D warnings`)
   - Compilation check (`cargo check`)
   - **Andon Signal**: Stops on any failure

2. **Unit Tests** (5-10 min)
   - Chicago TDD pattern tests
   - State-based test verification
   - `--test-threads=1` for deterministic execution
   - **Andon Signal**: All tests must pass

3. **Integration Tests** (10-15 min)
   - Database tests (PostgreSQL)
   - Cache tests (Redis)
   - Full end-to-end workflows
   - **Andon Signal**: All tests must pass

4. **Security Scanning** (5-10 min)
   - `cargo audit`: Dependency vulnerabilities
   - `cargo deny`: License and source verification
   - `cargo-geiger`: Unsafe usage detection
   - **Andon Signal**: No critical vulnerabilities allowed

5. **Specification Validation** (2 min)
   - TTL/RDF syntax validation
   - Markdown documentation checks
   - SHACL conformance (if applicable)

6. **Test Report** (1 min)
   - Summary of all checks
   - Status indicators for each stage

7. **Andon Signal Verification** (Final gate)
   - Verifies all signals are green (✅)
   - Blocks any downstream workflows if signals are red (🔴)

**Configuration:**

```yaml
env:
  CARGO_TERM_COLOR: always
  RUST_BACKTRACE: 1
  REGISTRY: artifacts.example.com
```

**Usage:**

The CI pipeline runs automatically on every push. To view results:

```bash
# View workflow runs
gh run list --workflow=ci-complete.yml

# View specific run
gh run view <run_id>

# Re-run failed workflow
gh run rerun <run_id>
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

### 4. Helm Chart Validation (helm-validation.yml)

Comprehensive Helm chart validation including linting, templating, Kubernetes manifest validation, and integration testing.

**Triggers:**
- Push to `main`, `develop` (with `helm/` path changes)
- Pull requests (with `helm/` path changes)

**Stages:**

1. **Helm Lint** (2 min)
   - Validates chart structure
   - Checks for common errors
   - Strict mode enabled

2. **Template Validation** (2 min)
   - Renders templates
   - Validates YAML syntax
   - Checks for template errors

3. **Kubeval** (3 min)
   - Validates generated manifests against Kubernetes schemas
   - Version: 1.28.0
   - Reports schema violations

4. **Schema Validation** (2 min)
   - Validates `values.yaml` against `values.schema.json`
   - Uses JSON Schema validation
   - Ensures config compliance

5. **Version Consistency** (1 min)
   - Checks Chart.yaml version
   - Compares with Cargo.toml version
   - Reports mismatches

6. **K8s Integration Test** (10 min)
   - Creates Kind cluster
   - Installs chart
   - Verifies rollout
   - Tests with different value profiles
   - Cleans up resources

7. **Validation Summary** (1 min)
   - Reports all check statuses
   - Final pass/fail indication

**Configuration:**

Update Kind cluster config to customize test environment:

```yaml
kind: Cluster
apiVersion: kind.x-k8s.io/v1alpha4
nodes:
- role: control-plane
  extraPortMappings:
  - containerPort: 80
    hostPort: 80
  - containerPort: 443
    hostPort: 443
```

**Usage:**

```bash
# Run Helm validation
gh workflow run helm-validation.yml --ref main

# Test Helm chart locally
helm lint helm/tai-chart
helm template tai-chart helm/tai-chart
helm install tai-test helm/tai-chart --dry-run

# Validate with kubeval
helm template tai-chart helm/tai-chart | kubeval
```

---

### 5. Generate Release Notes (generate-release-notes.yml)

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

### 6. GitOps with Flux (gitops-sync-flux.yml)

Declarative infrastructure with automatic synchronization using Flux CD.

**Triggers:**
- Push to `main`, `develop` (with `infra/flux/` path changes)
- Manual workflow dispatch

**Stages:**

1. **Validate Flux Configuration** (3 min)
   - Checks Flux sources
   - Validates kustomizations
   - Verifies directory structure

2. **Validate Kustomize** (3 min)
   - Builds all kustomizations
   - Detects errors early

3. **Validate Manifests** (3 min)
   - kubeval validation
   - Kubernetes schema compliance
   - Version: 1.28.0

4. **Dry-run on Staging** (5 min, `develop` branch only)
   - Connects to staging cluster
   - Performs dry-run apply
   - Shows predicted drift

5. **Apply to Production** (10 min, `main` branch only)
   - Requires GitHub environment approval
   - Applies to production cluster
   - Waits for Flux reconciliation (max 5 min)
   - Verifies deployment

6. **Verify Deployment** (5 min)
   - Checks rollout status
   - Verifies service endpoints
   - Performs health checks

**Directory Structure:**

```
infra/flux/
├── sources/                    # Git/Helm/Other sources
│   └── ggen-source.yaml
├── kustomizations/             # Kustomize overlays
│   ├── base/
│   │   ├── kustomization.yaml
│   │   ├── deployment.yaml
│   │   └── service.yaml
│   ├── staging/
│   │   └── kustomization.yaml
│   └── production/
│       └── kustomization.yaml
└── gotk-components.yaml        # Flux system components
```

**Configuration:**

Update kubeconfig secrets:

```bash
# Encode and store kubeconfig
cat ~/.kube/config | base64 | xclip -selection clipboard
# Paste into: Settings → Secrets and variables → Actions → New repository secret
# Names: KUBECONFIG_STAGING, KUBECONFIG_PROD
```

**Secrets Required:**

```
KUBECONFIG_STAGING         - base64-encoded kubeconfig for staging
KUBECONFIG_PROD            - base64-encoded kubeconfig for production
PROD_CLUSTER_URL           - URL of production cluster
```

**Usage:**

```bash
# Create Flux sources
flux create source git ggen \
  --url=https://github.com/seanchatmangpt/ggen \
  --branch=main \
  --interval=5m \
  --export | kubectl apply -f -

# Create kustomizations
flux create kustomization ggen \
  --source=GitRepository/ggen \
  --path="./infra/flux/kustomizations" \
  --prune=true \
  --interval=5m \
  --export | kubectl apply -f -

# Monitor reconciliation
flux get kustomizations --watch
flux get sources all --watch

# Force reconciliation
flux reconcile source git ggen
flux reconcile kustomization ggen

# View Flux logs
kubectl logs -n flux-system deployment/source-controller
kubectl logs -n flux-system deployment/kustomize-controller
```

---

### 7. Automated Rollback (automated-rollback.yml)

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

Configure branch protection in **Settings → Branches → main**:

```
✅ Require status checks to pass:
   - ci-complete
   - helm-validation
   - docker-build-push

✅ Require code reviews before merging
✅ Require approval from code owners
✅ Require branches to be up to date

✅ Require deploy reviews before merging
   - Environment: production
```

---

## Customization Guide

### Add Custom Andon Signals

Edit `ci-complete.yml` to add custom checks:

```yaml
- name: Custom andon signal
  run: |
    # Your check here
    if [ condition ]; then
      echo "❌ ANDON SIGNAL TRIGGERED"
      exit 1
    fi
    echo "✅ Check passed"
```

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

### Customize Helm Validation

Edit `helm-validation.yml` to add validators:

```yaml
- name: Custom Helm validator
  run: |
    # Custom validation logic
```

### Change GitOps Source

Edit `gitops-sync-flux.yml` to use different Git source:

```yaml
flux create source git ggen \
  --url=${{ secrets.GIT_REPO_URL }} \
  --branch=${{ secrets.GIT_BRANCH }}
```

---

## Monitoring & Debugging

### View Workflow Runs

```bash
# List all workflow runs
gh run list

# List runs for specific workflow
gh run list --workflow=ci-complete.yml

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

### Issue: "Andon Signal Failed - Compilation Error"

**Solution:**

1. Run locally: `cargo check`
2. Fix errors: `cargo clippy --fix`
3. Commit and push: `git push origin feature-branch`

### Issue: "Helm Lint Failed"

**Solution:**

```bash
# Lint chart locally
helm lint helm/tai-chart

# Check for missing dependencies
helm dependency list helm/tai-chart
helm dependency update helm/tai-chart

# Validate YAML
helm template tai-chart helm/tai-chart | kubeval
```

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

### Issue: "Kubeconfig authentication failed"

**Solution:**

```bash
# Verify kubeconfig is valid
kubectl config view

# Encode kubeconfig correctly
cat ~/.kube/config | base64 -w 0

# Update secret:
echo "NEW_BASE64_KUBECONFIG" | gh secret set KUBECONFIG_PROD
```

---

## Performance Optimization

### Cache Optimization

The CI pipeline uses GitHub Actions caching for:
- Rust build artifacts: `Swatinem/rust-cache@v2`
- Docker layers: `docker/build-push-action@v5` with `cache-from: type=gha`

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
- `quick-checks` → `unit-tests`, `integration-tests`, `security-scan` (parallel)
- `validate-flux` → `validate-kustomize` → `validate-manifests` (sequential)

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

- [Conventional Commits](https://www.conventionalcommits.org/)
- [Semantic Versioning](https://semver.org/)
- [Flux CD Documentation](https://fluxcd.io/)
- [cosign Documentation](https://docs.sigstore.dev/cosign/overview/)
- [syft Documentation](https://github.com/anchore/syft)
- [Helm Best Practices](https://helm.sh/docs/chart_best_practices/)

