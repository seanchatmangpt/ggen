# GitHub Organization Secrets Configuration

## Overview

This document defines the organization-level secrets required for CI/CD workflows in the seanchatmangpt organization.

**Access**: Only organization owners can manage these secrets at:
https://github.com/organizations/seanchatmangpt/settings/secrets/actions

## Required Organization Secrets

### 1. GCP_PROJECT_ID

**Purpose**: GCP project identifier for all infrastructure operations

**Location**: GCP Console > Project Settings

**Value Format**: `taiea-v1`

**Usage**:
- Referenced in: `.github/workflows/*.yml`
- Used in: Terraform, Cloud Build, Deployment scripts

**Setup**:
```bash
# Verify project ID
gcloud projects describe taiea-v1 --format='value(projectId)'
```

**Expiration**: Never expires (static configuration)

---

### 2. GCP_SERVICE_ACCOUNT_EMAIL

**Purpose**: Service account email for GitHub Actions authentication

**Location**: GCP Console > IAM & Admin > Service Accounts

**Value Format**: `tai-autonomics-sa@taiea-v1.iam.gserviceaccount.com`

**Usage**:
- Authenticate GitHub Actions to GCP
- Authorize Cloud Run deployments
- Access GCP resources

**Setup**:
```bash
# List service accounts
gcloud iam service-accounts list --project=taiea-v1

# Get specific account email
gcloud iam service-accounts describe tai-autonomics-sa \
  --project=taiea-v1 \
  --format='value(email)'
```

**Expiration**: Never expires (static configuration)

---

### 3. GCP_WORKLOAD_IDENTITY_PROVIDER

**Purpose**: Federated identity provider for GitHub Actions → GCP authentication

**Location**: GCP Console > IAM & Admin > Workload Identity Federation

**Value Format**: `projects/PROJECT_NUMBER/locations/global/workloadIdentityPools/github/providers/github`

**Example**: `projects/123456789/locations/global/workloadIdentityPools/github/providers/github`

**Usage**:
- Enables keyless authentication from GitHub Actions
- Replaces service account key files
- More secure than storing credentials

**Setup**:
```bash
# Get project number
PROJECT_NUMBER=$(gcloud projects describe taiea-v1 --format='value(projectNumber)')

# Create workload identity pool (if needed)
gcloud iam workload-identity-pools create github \
  --project=taiea-v1 \
  --location=global \
  --display-name=GitHub

# Create provider
gcloud iam workload-identity-pools providers create-oidc github \
  --project=taiea-v1 \
  --location=global \
  --workload-identity-pool=github \
  --display-name=GitHub \
  --attribute-mapping="google.subject=assertion.sub,assertion.aud=assertion.aud" \
  --issuer-uri=https://token.actions.githubusercontent.com

# Get full provider path
echo "projects/${PROJECT_NUMBER}/locations/global/workloadIdentityPools/github/providers/github"
```

**Expiration**: Never expires (provider configuration)

---

### 4. GCP_SERVICE_ACCOUNT_KEY

**Purpose**: Alternative authentication method (legacy, less secure)

**Location**: GCP Console > IAM & Admin > Service Accounts > Keys

**Value Format**: Base64-encoded JSON key file

**SECURITY WARNING**:
- Prefer Workload Identity Federation (GCP_WORKLOAD_IDENTITY_PROVIDER)
- Only use if Workload Identity is not available
- Rotate every 60 days minimum
- Store encrypted if possible

**Usage**:
- Fallback authentication for GCP services
- Some legacy tools require key-based auth

**Setup**:
```bash
# Create key
gcloud iam service-accounts keys create key.json \
  --iam-account=tai-autonomics-sa@taiea-v1.iam.gserviceaccount.com \
  --project=taiea-v1

# Encode to base64
cat key.json | base64 | tr -d '\n'

# Clean up
rm key.json

# Copy base64 output to GitHub secret
```

**Expiration**:
- Rotate every 60 days
- Revoke old keys: `gcloud iam service-accounts keys delete KEY_ID`

---

### 5. SLACK_WEBHOOK_URL

**Purpose**: Send notifications to Slack for deployments and alerts

**Location**: Slack App Configuration > Incoming Webhooks

**Value Format**: `https://hooks.slack.com/services/T00000000/B00000000/XXXXXXXXXXXXXXXXXXXX`

**SECURITY WARNING**:
- Restrict to specific channels only
- Rotate every 90 days
- Don't share via email

**Usage**:
- Deployment notifications
- Alert notifications
- Incident communications

**Setup**:
```bash
# 1. Go to: https://api.slack.com/apps
# 2. Select workspace
# 3. Create "New App" > "From scratch"
#    Name: "TAI Autonomics"
#    Workspace: Your Slack workspace
#
# 4. Enable "Incoming Webhooks"
# 5. Click "Add New Webhook to Workspace"
# 6. Select channel: #tai-deployments
# 7. Copy webhook URL

# 8. Add to GCP Secret Manager
echo -n "https://hooks.slack.com/services/..." | \
  gcloud secrets versions add slack-webhook-url \
    --data-file=- \
    --project=taiea-v1
```

**Expiration**: Rotate every 90 days

---

### 6. DOCKER_REGISTRY_TOKEN

**Purpose**: Authenticate to container registry for push/pull operations

**Location**: GCP Console > Artifact Registry or Docker Hub

**Value Format**: For Artifact Registry: `_json_key` username with base64 key
For Docker Hub: Personal access token

**Usage**:
- Build and push container images
- Pull images in Cloud Run
- CI/CD pipeline container operations

**Setup** (Artifact Registry):
```bash
# Use GCP service account key as Docker credentials
gcloud auth configure-docker us-central1-docker.pkg.dev

# Or create explicit token
TOKEN=$(gcloud auth application-default print-access-token)

# For GitHub Actions, use service account:
gcloud iam service-accounts keys create key.json \
  --iam-account=tai-autonomics-sa@taiea-v1.iam.gserviceaccount.com

cat key.json | base64
```

**Setup** (Docker Hub - if needed):
```bash
# 1. Go to: https://hub.docker.com/settings/security
# 2. Create Personal Access Token
# 3. Format: base64(username:token)
echo -n "username:token" | base64
```

**Expiration**: Rotate every 90 days

---

### 7. GitHub_TOKEN (Repository-Level)

**Note**: GitHub automatically provides `GITHUB_TOKEN` for each workflow run.
This is different from `GCP_PROJECT_ID` and other org-level secrets.

**Auto-provided value**: Scoped to current workflow run
**Duration**: Expires at end of workflow

**Usage**:
- Clone private repositories
- Create/update pull requests
- Post workflow results

**No manual setup required** - GitHub provides automatically.

---

## Organization Secret Matrix

| Secret Name | Type | Value | Rotation | Accessed By |
|-------------|------|-------|----------|------------|
| GCP_PROJECT_ID | Configuration | taiea-v1 | Never | All workflows |
| GCP_SERVICE_ACCOUNT_EMAIL | Configuration | tai-autonomics-sa@... | Never | All workflows |
| GCP_WORKLOAD_IDENTITY_PROVIDER | Configuration | projects/123.../pools/github/... | Never | CI/CD workflows |
| GCP_SERVICE_ACCOUNT_KEY | Secret | base64(key.json) | 60 days | Legacy workflows |
| SLACK_WEBHOOK_URL | Secret | https://hooks.slack.com/... | 90 days | Notification workflows |
| DOCKER_REGISTRY_TOKEN | Secret | base64(creds) | 90 days | Build/push workflows |

## Setting Up Organization Secrets

### Step 1: Authenticate to GitHub

```bash
gh auth login
# Select: GitHub.com
# Select: SSH
# Authorize CLI with browser
```

### Step 2: Create Secrets via CLI

```bash
# Install GitHub CLI (if needed)
# https://cli.github.com/

# Create secret
gh secret set GCP_PROJECT_ID \
  --org seanchatmangpt \
  --body "taiea-v1"

gh secret set GCP_SERVICE_ACCOUNT_EMAIL \
  --org seanchatmangpt \
  --body "tai-autonomics-sa@taiea-v1.iam.gserviceaccount.com"

# For sensitive values (prompt for input)
gh secret set SLACK_WEBHOOK_URL \
  --org seanchatmangpt \
  --prompt
```

### Step 3: Verify Secrets

```bash
# List all org secrets (names only, values hidden)
gh secret list --org seanchatmangpt

# Verify secret exists
gh secret view GCP_PROJECT_ID --org seanchatmangpt
```

## Using Secrets in Workflows

### Example: Using Organization Secrets

```yaml
name: Deploy

on: [push]

jobs:
  deploy:
    runs-on: ubuntu-latest
    permissions:
      contents: read
      id-token: write

    steps:
      # Authenticate to GCP
      - uses: google-github-actions/auth@v1
        with:
          workload_identity_provider: ${{ secrets.GCP_WORKLOAD_IDENTITY_PROVIDER }}
          service_account: ${{ secrets.GCP_SERVICE_ACCOUNT_EMAIL }}

      # Use GCP_PROJECT_ID in commands
      - name: Deploy to Cloud Run
        run: |
          gcloud run deploy tai-autonomics \
            --region us-central1 \
            --project ${{ secrets.GCP_PROJECT_ID }}

      # Send Slack notification
      - name: Notify Slack
        uses: slackapi/slack-github-action@v1
        with:
          webhook-url: ${{ secrets.SLACK_WEBHOOK_URL }}
          payload: |
            text: "Deployment successful"
```

## Secret Rotation Schedule

### Daily
- Review access logs for unusual activity
- Check for failed authentications

### Weekly
- Verify all secrets are accessible
- Test secret injection in test workflows

### Monthly
- Audit who accessed which secrets
- Review rotation schedule compliance

### Quarterly (90 days)
- Rotate: SLACK_WEBHOOK_URL
- Rotate: DOCKER_REGISTRY_TOKEN
- Review: GCP service account permissions

### Biannually (60 days)
- Rotate: GCP_SERVICE_ACCOUNT_KEY (if used)
- Regenerate: GitHub tokens

## Rotation Procedure

### Example: Rotating SLACK_WEBHOOK_URL

```bash
# 1. Generate new webhook in Slack
# https://api.slack.com/apps → Select app → Incoming Webhooks

# 2. Copy new webhook URL

# 3. Update GitHub secret
gh secret set SLACK_WEBHOOK_URL \
  --org seanchatmangpt \
  --body "https://hooks.slack.com/services/..."

# 4. Verify new secret works
# - Trigger a test workflow that uses SLACK_WEBHOOK_URL
# - Confirm Slack receives notification

# 5. Remove old webhook from Slack
# https://api.slack.com/apps → Select app → Remove Webhook
```

## Incident Response

### If Secret Is Exposed

```bash
# 1. Immediately revoke the secret

# For service account key:
gcloud iam service-accounts keys delete KEY_ID \
  --iam-account=tai-autonomics-sa@taiea-v1.iam.gserviceaccount.com

# For Slack webhook:
# Delete in https://api.slack.com/apps

# 2. Create new secret
# Follow rotation procedure above

# 3. Update GitHub secret
gh secret set SECRET_NAME --org seanchatmangpt

# 4. Document incident
# Create issue: https://github.com/seanchatmangpt/ggen/issues
```

## Troubleshooting

### Secret Not Available in Workflow

```yaml
# Verify secret is set
- name: Check Secret
  run: |
    if [ -z "${{ secrets.MY_SECRET }}" ]; then
      echo "❌ Secret MY_SECRET is not set"
      exit 1
    else
      echo "✓ Secret MY_SECRET is available"
    fi
```

### Workflow Failing with Authentication Error

1. Verify secret value is correct (typo-free)
2. Check secret expiration date
3. Verify service account has required permissions
4. Test authentication locally:
   ```bash
   gcloud auth activate-service-account --key-file=key.json
   ```

### Secret Appears in Logs (BREACH!)

1. Revoke secret immediately
2. Create new secret
3. Update GitHub secret
4. Clear workflow logs

## Related Documentation

- [GitHub Secrets Documentation](https://docs.github.com/en/actions/security-guides/encrypted-secrets)
- [GCP IAM Best Practices](https://cloud.google.com/iam/docs/best-practices)
- [Workload Identity Federation](https://cloud.google.com/iam/docs/workload-identity-federation)

## Support

- Create issue: https://github.com/seanchatmangpt/ggen/issues
- Contact: security@tai-autonomics.com
- On-call: Check incident response runbook
