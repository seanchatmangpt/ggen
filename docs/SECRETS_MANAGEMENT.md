# Secrets Management Guide

## Overview

This document describes how to manage sensitive credentials and secrets for the TAI Autonomics project using Google Cloud Secret Manager and GitHub.

**Key Principles:**
1. Never commit secrets to git repositories
2. All secrets are stored in GCP Secret Manager with automatic replication
3. Secrets are injected into CI/CD pipelines via GitHub organization secrets
4. Rotation policies ensure credentials are regularly updated
5. Audit logging tracks all secret access

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ GCP Secret Manager                                          │
│ ┌──────────────────────────────────────────────────────┐   │
│ │ github-token                 (VCS)                  │   │
│ │ gcp-service-account-key      (IAM)                  │   │
│ │ firestore-url                (Database)             │   │
│ │ docker-registry-token        (Registry)             │   │
│ │ slack-webhook-url            (Notifications)        │   │
│ │ erlang-cookie                (Application)          │   │
│ │ database-connection-string   (Database)             │   │
│ │ internal-api-key             (Authentication)       │   │
│ │ tls-certificate              (Security)             │   │
│ │ tls-private-key              (Security)             │   │
│ └──────────────────────────────────────────────────────┘   │
└──────────────────┬──────────────────────────────────────────┘
                   │
         ┌─────────┴──────────┐
         │                    │
    ┌────▼────┐         ┌────▼──────┐
    │ GitHub  │         │ Cloud Run  │
    │ Actions │         │ Services   │
    │ (CI/CD) │         │            │
    └─────────┘         └────────────┘
```

## File Structure

```
.env.example                    # Template (NO ACTUAL SECRETS)
.gitignore                      # Prevents .env commits
infra/gcp/secrets.tf            # Terraform: Secret Manager resources
infra/gcp/secrets-variables.tf  # Terraform: Variables
.github/workflows/secrets-sync.yml  # Automation: Sync & verify
docs/SECRETS_MANAGEMENT.md      # This file
```

## Getting Started

### 1. Initial Setup

#### Create .env File (Local Development Only)

```bash
# Copy template
cp .env.example .env

# Fill in values from GCP Secret Manager
gcloud secrets versions access latest --secret="github-token" --project=taiea-v1
gcloud secrets versions access latest --secret="gcp-service-account-key" --project=taiea-v1
```

**NEVER commit .env file!**

#### Initialize GCP Secret Manager

```bash
# Initialize Terraform
cd infra/gcp
terraform init

# Review planned changes
terraform plan

# Apply configuration
terraform apply
```

### 2. Adding Secrets to GCP Secret Manager

#### Using gcloud CLI

```bash
# Add a new secret
gcloud secrets create my-secret \
  --replication-policy="automatic" \
  --project=taiea-v1

# Set the secret value
gcloud secrets versions add my-secret \
  --data-file=-
# Then paste secret value and press Ctrl+D

# Or from file
gcloud secrets versions add my-secret \
  --data-file=/path/to/secret.txt
```

#### Creating GitHub Token

```bash
# 1. Go to https://github.com/settings/tokens
# 2. Click "Generate new token"
# 3. Select scopes:
#    - repo (full control)
#    - workflow (manage workflows)
#    - admin:org_hook (manage hooks)
# 4. Copy token

# 5. Add to Secret Manager
gcloud secrets create github-token \
  --replication-policy="automatic" \
  --project=taiea-v1

gcloud secrets versions add github-token \
  --data-file=-
# Paste token and press Ctrl+D
```

#### Creating GCP Service Account Key

```bash
# 1. Create service account
gcloud iam service-accounts create tai-autonomics-ci \
  --display-name="TAI Autonomics CI/CD" \
  --project=taiea-v1

# 2. Grant necessary roles
gcloud projects add-iam-policy-binding taiea-v1 \
  --member="serviceAccount:tai-autonomics-ci@taiea-v1.iam.gserviceaccount.com" \
  --role="roles/run.developer"

gcloud projects add-iam-policy-binding taiea-v1 \
  --member="serviceAccount:tai-autonomics-ci@taiea-v1.iam.gserviceaccount.com" \
  --role="roles/artifactregistry.admin"

# 3. Create and encode key
gcloud iam service-accounts keys create key.json \
  --iam-account=tai-autonomics-ci@taiea-v1.iam.gserviceaccount.com

# 4. Encode to base64
cat key.json | base64 > key.json.b64

# 5. Add to Secret Manager
gcloud secrets create gcp-service-account-key \
  --replication-policy="automatic" \
  --project=taiea-v1

gcloud secrets versions add gcp-service-account-key \
  --data-file=key.json.b64

# 6. Cleanup
rm key.json key.json.b64
```

#### Creating Erlang Cookie

```bash
# Generate secure random cookie
ERLANG_COOKIE=$(openssl rand -base64 32)

# Add to Secret Manager
gcloud secrets create erlang-cookie \
  --replication-policy="automatic" \
  --project=taiea-v1

echo -n "$ERLANG_COOKIE" | gcloud secrets versions add erlang-cookie \
  --data-file=-
```

#### Creating TLS Certificate & Key

```bash
# Generate self-signed certificate (development only)
openssl req -x509 -newkey rsa:4096 -nodes \
  -out tls.crt -keyout tls.key -days 365

# For production, use acme-sh or certbot

# Add certificate to Secret Manager
gcloud secrets create tls-certificate \
  --replication-policy="automatic" \
  --project=taiea-v1

gcloud secrets versions add tls-certificate \
  --data-file=tls.crt

# Add private key to Secret Manager
gcloud secrets create tls-private-key \
  --replication-policy="automatic" \
  --project=taiea-v1

gcloud secrets versions add tls-private-key \
  --data-file=tls.key

# Cleanup
rm tls.crt tls.key
```

### 3. Configuring GitHub Organization Secrets

**Only organization owners can configure organization secrets.**

```bash
# Go to: https://github.com/organizations/seanchatmangpt/settings/secrets/actions

# Add these organization secrets:

# 1. GCP_PROJECT_ID
#    Value: taiea-v1

# 2. GCP_SERVICE_ACCOUNT_EMAIL
#    Value: tai-autonomics-sa@taiea-v1.iam.gserviceaccount.com

# 3. GCP_WORKLOAD_IDENTITY_PROVIDER
#    Value: projects/PROJECT_NUMBER/locations/global/workloadIdentityPools/github/providers/github
#    Get PROJECT_NUMBER: gcloud projects describe taiea-v1 --format='value(projectNumber)'

# 4. SLACK_WEBHOOK_URL
#    From: https://api.slack.com/apps
#    Set in: GCP Secret Manager > slack-webhook-url

# 5. DOCKER_REGISTRY_TOKEN
#    From: gcloud secrets versions access latest --secret="docker-registry-token"
```

## Using Secrets in CI/CD

### GitHub Actions

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
      - uses: actions/checkout@v4

      # Authenticate to GCP using Workload Identity Federation
      - uses: google-github-actions/auth@v1
        with:
          workload_identity_provider: ${{ secrets.GCP_WORKLOAD_IDENTITY_PROVIDER }}
          service_account: ${{ secrets.GCP_SERVICE_ACCOUNT_EMAIL }}

      # Retrieve secrets from GCP Secret Manager
      - name: Get GitHub Token
        run: |
          gcloud secrets versions access latest \
            --secret="github-token" \
            --project="${{ secrets.GCP_PROJECT_ID }}" \
            > /tmp/github-token

      # Use secret in your workflow
      - name: Deploy
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        run: |
          # Your deployment script
          echo "Deploying..."
```

### Cloud Run Environment Variables

Secrets are injected into Cloud Run service via Terraform:

```hcl
# In terraform configuration
resource "google_cloud_run_service" "app" {
  template {
    spec {
      containers {
        env {
          name = "DATABASE_URL"
          value_from {
            secret_key_ref {
              name = "firestore-url"
              key  = "latest"
            }
          }
        }
      }
    }
  }
}
```

## Secret Rotation

### Rotation Schedule

- **Critical Secrets** (GitHub token, service account keys): Every 60 days
- **Application Secrets** (Erlang cookie, API keys): Every 90 days
- **TLS Certificates**: Every 365 days
- **Database Credentials**: Every 120 days

### Rotating a Secret

```bash
# 1. Generate new secret value
NEW_VALUE=$(openssl rand -base64 32)

# 2. Create new version in Secret Manager
echo -n "$NEW_VALUE" | gcloud secrets versions add github-token \
  --data-file=-

# 3. Update applications to use new version
# Most services automatically use the latest version

# 4. Verify all services are using new version
gcloud logging read \
  "resource.type=secretmanager.googleapis.com/Secret AND \
   protoPayload.request.name=~/github-token" \
  --project=taiea-v1 \
  --limit=10

# 5. (Optional) Destroy old version after verification
gcloud secrets versions destroy VERSION_NUMBER \
  --secret=github-token \
  --project=taiea-v1
```

### Automated Rotation with Cloud Scheduler

```bash
# Create Cloud Scheduler job
gcloud scheduler jobs create http rotate-secrets \
  --location=us-central1 \
  --schedule="0 2 1 * *" \
  --uri="https://YOUR_ROTATION_FUNCTION_URL" \
  --http-method=POST \
  --project=taiea-v1
```

## Emergency Access Procedure

**For Production Incidents Only**

### Step 1: Authenticate

```bash
gcloud auth login
gcloud config set project taiea-v1
```

### Step 2: Retrieve Secret

```bash
# Only individuals with secretmanager.secretAccessor role
gcloud secrets versions access latest --secret=SECRE_NAME
```

### Step 3: Document Access

```bash
# All access is logged in Cloud Audit Logs
gcloud logging read \
  "resource.type=secretmanager.googleapis.com/Secret AND \
   protoPayload.methodName=~google.cloud.secretmanager.v1.SecretManagerService.AccessSecretVersion" \
  --project=taiea-v1 \
  --limit=20
```

### Step 4: Communicate

1. Notify security team: security@tai-autonomics.com
2. Create incident ticket in JIRA
3. Determine if secret rotation needed
4. Document root cause

## Compliance & Security

### Principle of Least Privilege

Each service has minimum required permissions:

```bash
# Cloud Run service account - secret access only
gcloud projects add-iam-policy-binding taiea-v1 \
  --member="serviceAccount:tai-autonomics-sa@taiea-v1.iam.gserviceaccount.com" \
  --role="roles/secretmanager.secretAccessor" \
  --condition="resource.name=startsWith('projects/_/secrets/erlang-')"

# CI/CD service account - secret access + artifact writing
gcloud projects add-iam-policy-binding taiea-v1 \
  --member="serviceAccount:tai-autonomics-ci@taiea-v1.iam.gserviceaccount.com" \
  --role="roles/secretmanager.secretAccessor"
```

### Audit Logging

All secret access is logged:

```bash
# View all secret access
gcloud logging read \
  'resource.type="secretmanager.googleapis.com/Secret"' \
  --project=taiea-v1 \
  --format="table(timestamp, protoPayload.authenticationInfo.principalEmail, protoPayload.methodName)"

# Filter by secret
gcloud logging read \
  'resource.type="secretmanager.googleapis.com/Secret" AND \
   protoPayload.request.name=~"github-token"' \
  --project=taiea-v1

# Export audit logs
gcloud logging read \
  'resource.type="secretmanager.googleapis.com/Secret"' \
  --project=taiea-v1 \
  --format=json > secret-audit.json
```

### Encryption

All secrets are encrypted:
- **At Rest**: Google-managed encryption keys (default)
- **In Transit**: TLS 1.2+
- **Customer-managed Keys (Optional)**: Use Cloud KMS for CMEK

```bash
# Enable CMEK
gcloud secrets create high-security-secret \
  --replication-policy="automatic" \
  --kms-key=projects/taiea-v1/locations/us-central1/keyRings/secrets/cryptoKeys/secret-key \
  --project=taiea-v1
```

## Troubleshooting

### Secret Not Found

```bash
# List all secrets
gcloud secrets list --project=taiea-v1

# Describe specific secret
gcloud secrets describe github-token --project=taiea-v1

# Check for typos in secret name
```

### Permission Denied

```bash
# Check your role
gcloud projects get-iam-policy taiea-v1 \
  --flatten="bindings[].members" \
  --filter="bindings.members:$(gcloud config get-value account)"

# Request access (contact: ops@tai-autonomics.com)
# Or add permissions:
gcloud projects add-iam-policy-binding taiea-v1 \
  --member="user:your-email@tai-autonomics.com" \
  --role="roles/secretmanager.secretAccessor"
```

### Secret Version Issues

```bash
# List all versions of a secret
gcloud secrets versions list github-token --project=taiea-v1

# Get specific version
gcloud secrets versions access VERSION_ID --secret=github-token --project=taiea-v1

# Disable old version
gcloud secrets versions disable VERSION_ID --secret=github-token --project=taiea-v1

# Destroy version (irreversible)
gcloud secrets versions destroy VERSION_ID --secret=github-token --project=taiea-v1
```

## Best Practices

### Do's ✓

- **Store in Secret Manager**: All credentials, API keys, tokens
- **Use environment variables**: Inject via CI/CD or Cloud Run
- **Rotate regularly**: Quarterly minimum
- **Audit access**: Review logs monthly
- **Use service accounts**: Prefer to user credentials
- **Enable CMEK**: For sensitive environments
- **Document rotation**: Keep rotation calendar updated
- **Test rotation**: Verify services handle new versions

### Don'ts ✗

- **Never commit secrets**: Use .gitignore
- **Never hardcode values**: Use environment variables
- **Never log secrets**: Redact from logs
- **Never share via email**: Use Secret Manager links
- **Never use master passwords**: Use temporary access tokens
- **Never skip rotation**: Schedule quarterly reviews
- **Never ignore audit logs**: Set up alerts

## Related Documentation

- [GCP Secret Manager Docs](https://cloud.google.com/secret-manager/docs)
- [GitHub Actions Secrets](https://docs.github.com/en/actions/security-guides/encrypted-secrets)
- [Cloud IAM Best Practices](https://cloud.google.com/iam/docs/best-practices)
- [TLS Certificate Management](docs/TLS_CERTIFICATE_MANAGEMENT.md)

## Support

For questions or issues:
- Contact: security@tai-autonomics.com
- Create issue: https://github.com/seanchatmangpt/ggen/issues
- On-call: Check incident response runbook
