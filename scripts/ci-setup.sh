#!/usr/bin/bin/env bash
#
# CI/CD Pipeline Setup Script
#
# Sets up GitHub secrets, variables, and branch protection for the CI/CD pipeline
# Usage: ./scripts/ci-setup.sh <repository> <environment>

set -euo pipefail

REPO="${1:-}"
ENVIRONMENT="${2:-development}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() { echo -e "${BLUE}ℹ️  $1${NC}"; }
log_success() { echo -e "${GREEN}✅ $1${NC}"; }
log_warning() { echo -e "${YELLOW}⚠️  $1${NC}"; }
log_error() { echo -e "${RED}❌ $1${NC}"; }

# Verify prerequisites
check_prerequisites() {
  log_info "Checking prerequisites..."

  if ! command -v gh &> /dev/null; then
    log_error "GitHub CLI (gh) not found. Install from https://cli.github.com/"
    exit 1
  fi

  if ! gh auth status &>/dev/null; then
    log_error "Not authenticated with GitHub. Run 'gh auth login'"
    exit 1
  fi

  log_success "Prerequisites verified"
}

# Validate repository
validate_repository() {
  if [ -z "$REPO" ]; then
    log_error "Repository not specified. Usage: ./scripts/ci-setup.sh <org/repo>"
    exit 1
  fi

  if ! gh repo view "$REPO" &>/dev/null; then
    log_error "Repository $REPO not found or not accessible"
    exit 1
  fi

  log_success "Repository verified: $REPO"
}

# Setup secrets
setup_secrets() {
  log_info "Setting up GitHub Secrets..."

  # GCP Artifact Registry Key
  read -p "Enter base64-encoded GCP Artifact Registry key (or press Enter to skip): " gcp_key
  if [ -n "$gcp_key" ]; then
    echo "$gcp_key" | gh secret set GCP_ARTIFACT_REGISTRY_KEY --repo "$REPO"
    log_success "GCP_ARTIFACT_REGISTRY_KEY set"
  else
    log_warning "GCP_ARTIFACT_REGISTRY_KEY not set"
  fi

  # Staging kubeconfig
  read -p "Enter path to staging kubeconfig (or press Enter to skip): " staging_kubeconfig
  if [ -n "$staging_kubeconfig" ] && [ -f "$staging_kubeconfig" ]; then
    KUBECONFIG_STAGING=$(cat "$staging_kubeconfig" | base64 -w 0)
    echo "$KUBECONFIG_STAGING" | gh secret set KUBECONFIG_STAGING --repo "$REPO"
    log_success "KUBECONFIG_STAGING set"
  else
    log_warning "KUBECONFIG_STAGING not set"
  fi

  # Production kubeconfig
  read -p "Enter path to production kubeconfig (or press Enter to skip): " prod_kubeconfig
  if [ -n "$prod_kubeconfig" ] && [ -f "$prod_kubeconfig" ]; then
    KUBECONFIG_PROD=$(cat "$prod_kubeconfig" | base64 -w 0)
    echo "$KUBECONFIG_PROD" | gh secret set KUBECONFIG_PROD --repo "$REPO"
    log_success "KUBECONFIG_PROD set"
  else
    log_warning "KUBECONFIG_PROD not set"
  fi

  # Production cluster URL
  read -p "Enter production cluster URL (or press Enter to skip): " prod_url
  if [ -n "$prod_url" ]; then
    echo "$prod_url" | gh secret set PROD_CLUSTER_URL --repo "$REPO"
    log_success "PROD_CLUSTER_URL set"
  else
    log_warning "PROD_CLUSTER_URL not set"
  fi
}

# Setup variables
setup_variables() {
  log_info "Setting up GitHub Variables..."

  read -p "Enter GCP Artifact Registry URL (default: artifacts.example.com): " registry_gcp
  registry_gcp="${registry_gcp:-artifacts.example.com}"
  echo "$registry_gcp" | gh variable set REGISTRY_GCP --repo "$REPO"
  log_success "REGISTRY_GCP set to $registry_gcp"

  read -p "Enter controller image name (default: catalog-controller): " controller_image
  controller_image="${controller_image:-catalog-controller}"
  echo "$controller_image" | gh variable set IMAGE_NAME_CONTROLLER --repo "$REPO"
  log_success "IMAGE_NAME_CONTROLLER set to $controller_image"

  read -p "Enter CLI image name (default: ggen-cli): " cli_image
  cli_image="${cli_image:-ggen-cli}"
  echo "$cli_image" | gh variable set IMAGE_NAME_CLI --repo "$REPO"
  log_success "IMAGE_NAME_CLI set to $cli_image"
}

# Setup branch protection
setup_branch_protection() {
  log_info "Setting up branch protection for 'main'..."

  # Note: Branch protection requires Admin access and gh extension
  # This is a guide for manual setup or requires additional permissions

  log_warning "Branch protection setup requires GitHub CLI extension"
  log_info "Manual setup required at: https://github.com/$REPO/settings/branch_protection_rules/new"

  cat << 'EOF'

Branch Protection Settings for 'main':

✅ Require status checks to pass before merging:
   - ci-complete
   - helm-validation
   - docker-build-push

✅ Require branches to be up to date before merging

✅ Require code review approval before merging
   - Required approving reviews: 1

✅ Require CODEOWNERS review

✅ Require status checks to pass for administrators too

✅ Restrict who can push to matching branches
   - Allow force pushes: No
   - Allow deletions: No

EOF

  log_warning "Visit https://github.com/$REPO/settings/branch_protection_rules/new to set these manually"
}

# Setup GitHub environments
setup_environments() {
  log_info "Setting up GitHub Environments..."

  # Production environment
  log_info "Configuring 'production' environment..."

  log_warning "GitHub environments require manual setup via GitHub UI"
  log_info "Visit: https://github.com/$REPO/settings/environments"

  cat << 'EOF'

Environment Configuration:

Production Environment:
- Name: production
- Deployment branches: Only selected branches
  - main
- Required reviewers: At least 1 person
  - Add codeowners as required reviewers
- Prevent administrators from bypassing these protections: ✅

Staging Environment:
- Name: staging
- Deployment branches: Only selected branches
  - develop
- Required reviewers: None (automatic)

EOF

  log_warning "Please configure environments manually at https://github.com/$REPO/settings/environments"
}

# Setup required status checks
setup_status_checks() {
  log_info "Configured status checks:"

  echo ""
  echo "The following workflows should be marked as required status checks:"
  echo ""
  echo "1. ci-complete.yml"
  echo "   - Ensures compilation, linting, and tests pass"
  echo "   - Andon signal: Stops on any failure"
  echo ""
  echo "2. helm-validation.yml (if Helm charts exist)"
  echo "   - Validates Helm charts"
  echo "   - Andon signal: Blocks invalid charts"
  echo ""
  echo "3. docker-build-push.yml (if pushing images)"
  echo "   - Builds and pushes Docker images"
  echo "   - Andon signal: Blocks on build failures"
  echo ""

  log_warning "Required status checks must be enabled at:"
  log_info "https://github.com/$REPO/settings/branch_protection_rules"
}

# Verify setup
verify_setup() {
  log_info "Verifying setup..."

  # Check secrets
  SECRETS=$(gh secret list --repo "$REPO" | wc -l)
  log_success "Found $SECRETS GitHub Secrets"

  # Check variables
  VARIABLES=$(gh variable list --repo "$REPO" | wc -l)
  log_success "Found $VARIABLES GitHub Variables"

  # Check workflows
  log_info "Checking GitHub Actions workflows..."

  local workflows=(
    "ci-complete.yml"
    "semantic-release.yml"
    "docker-build-push.yml"
    "helm-validation.yml"
    "gitops-sync-flux.yml"
    "automated-rollback.yml"
  )

  for workflow in "${workflows[@]}"; do
    if gh workflow list --repo "$REPO" | grep -q "$workflow"; then
      log_success "Workflow found: $workflow"
    else
      log_warning "Workflow not found: $workflow"
    fi
  done
}

# Print summary
print_summary() {
  cat << EOF

${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}
${GREEN}✅ CI/CD Pipeline Setup Complete!${NC}
${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}

${BLUE}Repository:${NC} $REPO
${BLUE}Environment:${NC} $ENVIRONMENT

${YELLOW}Next Steps:${NC}

1. ${BLUE}Complete GitHub Setup${NC}
   - Visit: https://github.com/$REPO/settings/branches
   - Enable branch protection for 'main'
   - Configure required status checks

2. ${BLUE}Configure Kubernetes Access${NC}
   - Ensure staging and production kubeconfigs are set
   - Test Flux CD connectivity: ${BLUE}flux check --kubeconfig <path>${NC}

3. ${BLUE}Configure GCP Artifact Registry${NC}
   - Set up GCP project and service account
   - Create Artifact Registry repositories:
     - ${BLUE}catalog-controller${NC}
     - ${BLUE}ggen-cli${NC}

4. ${BLUE}Configure GitHub Environments${NC}
   - Visit: https://github.com/$REPO/settings/environments
   - Create 'production' environment with required reviewers
   - Create 'staging' environment for automatic deployments

5. ${BLUE}Monitor Workflows${NC}
   - Visit: https://github.com/$REPO/actions
   - Watch workflows run on your next push to 'main'

${YELLOW}Useful Commands:${NC}

# View workflow runs
gh run list --repo $REPO

# Trigger a workflow
gh workflow run ci-complete.yml --repo $REPO --ref main

# View workflow logs
gh run view <run-id> --repo $REPO --log

# Re-run a workflow
gh run rerun <run-id> --repo $REPO

${YELLOW}Documentation:${NC}
- Full guide: .github/CI_CD_PIPELINE.md
- Workflow details: .github/workflows/
- GitHub Actions: https://docs.github.com/en/actions

EOF
}

# Main execution
main() {
  log_info "Starting CI/CD Pipeline Setup"
  echo ""

  check_prerequisites
  validate_repository
  echo ""

  setup_secrets
  echo ""

  setup_variables
  echo ""

  setup_environments
  echo ""

  setup_status_checks
  echo ""

  verify_setup
  echo ""

  print_summary
}

# Run main
main "$@"
