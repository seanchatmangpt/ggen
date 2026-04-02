# Flux CD Configuration (GitOps)

This directory contains the GitOps configuration for managing ggen deployments across Kubernetes clusters using Flux CD v2.

## Overview

Flux CD provides:
- **Declarative Infrastructure**: Kubernetes manifests stored in git as source of truth
- **Automatic Reconciliation**: Cluster state synced to git every 5 minutes
- **Health Monitoring**: Automated drift detection and alerts
- **Multi-cluster Support**: Deploy to staging and production simultaneously

## Directory Structure

```
infra/flux/
├── sources/                      # Git/Helm/OCI sources
│   ├── git-sources.yaml          # Git repository configurations
│   └── helm-sources.yaml         # Helm repository configurations
├── kustomizations/               # Kustomize overlays
│   ├── base/                     # Base configuration (shared)
│   │   ├── kustomization.yaml
│   │   ├── namespace.yaml
│   │   ├── deployment.yaml
│   │   ├── service.yaml
│   │   ├── configmap.yaml
│   │   └── ingress.yaml
│   ├── staging/                  # Staging overlay
│   │   ├── kustomization.yaml    # Patches for staging
│   │   ├── configmap-patch.yaml
│   │   └── replicas-patch.yaml
│   └── production/               # Production overlay
│       ├── kustomization.yaml    # Patches for production
│       ├── configmap-patch.yaml
│       ├── replicas-patch.yaml
│       └── resources-patch.yaml
├── gotk-components.yaml          # Flux system components
├── gotk-sync.yaml               # Flux sync configuration
└── README.md                     # This file

```

## Quick Start

### 1. Install Flux

```bash
# Install Flux CLI
curl -s https://fluxcd.io/install.sh | bash

# Bootstrap Flux in cluster
flux bootstrap github \
  --owner=seanchatmangpt \
  --repo=ggen \
  --branch=main \
  --path=infra/flux \
  --personal
```

### 2. Create Git Source

```bash
# Create git source pointing to this repository
flux create source git ggen \
  --url=https://github.com/seanchatmangpt/ggen \
  --branch=main \
  --interval=5m \
  --export | kubectl apply -f -
```

### 3. Create Kustomizations

```bash
# Create kustomization for staging
flux create kustomization tai-staging \
  --source=GitRepository/ggen \
  --path="./infra/flux/kustomizations/staging" \
  --prune=true \
  --interval=5m \
  --namespace=staging \
  --export | kubectl apply -f -

# Create kustomization for production
flux create kustomization tai-production \
  --source=GitRepository/ggen \
  --path="./infra/flux/kustomizations/production" \
  --prune=true \
  --interval=5m \
  --namespace=production \
  --export | kubectl apply -f -
```

### 4. Monitor Reconciliation

```bash
# Watch sources
flux get sources all --watch

# Watch kustomizations
flux get kustomizations --watch

# View reconciliation status
flux describe kustomization tai-staging
flux describe kustomization tai-production
```

## Configuration Details

### Sources (infra/flux/sources/)

Git and Helm sources define where Flux pulls configuration from.

**git-sources.yaml**: Git repository source
```yaml
apiVersion: source.toolkit.fluxcd.io/v1
kind: GitRepository
metadata:
  name: ggen
  namespace: flux-system
spec:
  interval: 5m
  url: https://github.com/seanchatmangpt/ggen
  ref:
    branch: main
```

**helm-sources.yaml**: Helm chart repository
```yaml
apiVersion: source.toolkit.fluxcd.io/v1beta1
kind: HelmRepository
metadata:
  name: bitnami
  namespace: flux-system
spec:
  interval: 1h
  url: https://charts.bitnami.com/bitnami
```

### Kustomizations (infra/flux/kustomizations/)

Kustomizations define which manifests to apply and how to patch them for different environments.

**Base** (`kustomizations/base/kustomization.yaml`):
- Contains shared configuration
- Applied to all environments
- Includes: namespace, deployment, service, configmap, ingress

**Staging** (`kustomizations/staging/kustomization.yaml`):
- Patches base with staging-specific values
- Reduced replicas, resource limits
- Development-grade configuration

**Production** (`kustomizations/production/kustomization.yaml`):
- Patches base with production-specific values
- Full replicas, resource guarantees
- Production-grade configuration

### Example Kustomization

```yaml
# kustomizations/staging/kustomization.yaml
apiVersion: kustomize.config.k8s.io/v1beta1
kind: Kustomization
namespace: staging

resources:
  - ../base

patches:
  - target:
      kind: Deployment
      name: tai-controller
    patch: |-
      - op: replace
        path: /spec/replicas
        value: 2
  - target:
      kind: ConfigMap
      name: tai-config
    patch: |-
      - op: add
        path: /data/LOG_LEVEL
        value: "debug"
```

## Deployment Workflows

### Staging Deployment

```bash
# Git push triggers this automatically:
# 1. GitHub Actions runs ci-complete.yml
# 2. If tests pass, runs docker-build-push.yml
# 3. Updates infra/flux/kustomizations/staging/
# 4. Flux detects changes in 5 minutes
# 5. Applies to staging cluster
```

### Production Deployment

```bash
# Manual trigger required:
# 1. PR approved by reviewers
# 2. Merged to 'main' branch
# 3. GitHub Actions runs ci-complete.yml
# 4. If tests pass, runs docker-build-push.yml
# 5. Creates GitHub release (semantic versioning)
# 6. Updates infra/flux/kustomizations/production/
# 7. Flux detects changes in 5 minutes (or manual trigger)
# 8. Applies to production cluster
```

## Common Operations

### Monitor Flux Health

```bash
# Check flux system
flux check

# Verify all sources reconciled
flux get sources all

# Verify all kustomizations reconciled
flux get kustomizations all

# View detailed status
flux describe kustomization tai-production
```

### Force Reconciliation

```bash
# Reconcile specific source
flux reconcile source git ggen

# Reconcile specific kustomization
flux reconcile kustomization tai-production

# Watch reconciliation
flux reconcile kustomization tai-production --with-source
```

### View Reconciliation Logs

```bash
# Source controller logs
kubectl logs -n flux-system deployment/source-controller

# Kustomize controller logs
kubectl logs -n flux-system deployment/kustomize-controller

# Notification controller logs
kubectl logs -n flux-system deployment/notification-controller
```

### Suspend/Resume Reconciliation

```bash
# Suspend a source
flux suspend source git ggen

# Resume a source
flux resume source git ggen

# Suspend a kustomization
flux suspend kustomization tai-production

# Resume a kustomization
flux resume kustomization tai-production
```

### Manual Deployment

```bash
# Temporarily override with manual manifest
kubectl apply -f manifest.yaml

# Flux will automatically revert to git state
# To prevent, suspend the kustomization:
flux suspend kustomization tai-production

# Then apply and suspend reconciliation
kubectl apply -f manifest.yaml

# Resume when ready
flux resume kustomization tai-production
```

## Troubleshooting

### Kustomization Not Reconciling

```bash
# Check status
flux get kustomizations

# Describe for details
flux describe kustomization tai-production

# View events
kubectl describe kustomization tai-production -n flux-system

# Check logs
kubectl logs -n flux-system deployment/kustomize-controller | tail -100
```

### Source Not Updating

```bash
# Check source status
flux get sources all

# Force refresh
flux reconcile source git ggen

# Check source logs
kubectl logs -n flux-system deployment/source-controller | tail -100
```

### Wrong Version Deployed

```bash
# Check current deployment image
kubectl get deployment tai-controller -n production -o jsonpath='{.spec.template.spec.containers[0].image}'

# Check what git has
git show HEAD:infra/flux/kustomizations/production/kustomization.yaml

# Manual fix: update kustomization.yaml and commit
git add infra/flux/kustomizations/production/
git commit -m "fix: Correct deployment image version"
git push origin main

# Flux will reconcile in 5 minutes
```

## Security Best Practices

### Git Repository Access

```bash
# Use SSH key (preferred)
flux create source git ggen \
  --url=ssh://git@github.com/seanchatmangpt/ggen \
  --ssh-key-algorithm=rsa \
  --ssh-key-bits=4096

# Or HTTPS with token
flux create source git ggen \
  --url=https://github.com/seanchatmangpt/ggen \
  --username=<user> \
  --password=<token>
```

### Image Pull Secrets

```bash
# Create secret for private registries
kubectl create secret docker-registry acr-secret \
  --docker-server=artifacts.example.com \
  --docker-username=_json_key \
  --docker-password="$(cat key.json)"

# Reference in kustomization
resources:
  - deployment.yaml
secretGenerator:
  - name: acr-secret
    files:
      - ./key.json
```

### RBAC

```bash
# Flux automatically creates least-privilege service accounts
kubectl get serviceaccount -n flux-system
kubectl get clusterrole | grep flux
kubectl get clusterrolebinding | grep flux
```

## Integration with CI/CD

### Automatic Updates from CI

1. Docker image pushed to registry with new tag
2. Kustomization updated in git by CI workflow
3. Flux detects change in 5 minutes
4. New image deployed automatically

### Manual Approval Gates

```bash
# Suspend production kustomization to require approval
flux suspend kustomization tai-production

# After manual approval:
flux resume kustomization tai-production
```

### Rollback via Git

```bash
# Revert last change
git revert HEAD

# Push revert
git push origin main

# Flux automatically applies revert
```

## Alerts and Notifications

### GitHub Notifications

```bash
# Enable notifications on reconciliation
flux create alert github \
  --event-severity=error \
  --provider-ref=github-token \
  --export | kubectl apply -f -
```

### Slack Notifications

```bash
# Enable Slack alerts
flux create alert slack \
  --provider-ref=slack-token \
  --channel=deployments \
  --export | kubectl apply -f -
```

## Advanced Configuration

### Multi-cluster GitOps

```bash
# Separate overlays per cluster
kustomizations/
├── us-east-prod/
├── us-west-prod/
├── eu-prod/
└── staging/

# Each with own kustomization.yaml pointing to cluster-specific overlays
```

### Progressive Delivery

```bash
# Canary deployment
flux create kustomization tai-canary \
  --source=GitRepository/ggen \
  --path="./infra/flux/kustomizations/canary" \
  --depends-on=tai-staging
```

### External Secrets

```bash
# Use External Secrets Operator for secrets management
flux create helmrelease external-secrets \
  --source=HelmRepository/external-secrets \
  --chart=external-secrets
```

## Useful Links

- [Flux CD Official Docs](https://fluxcd.io/docs/)
- [Flux CLI Commands](https://fluxcd.io/docs/cmd/)
- [Kustomize Documentation](https://kustomize.io/)
- [GitOps Best Practices](https://opengitops.dev/)
- [Flux GitHub Repository](https://github.com/fluxcd/flux2)

---

**Last Updated**: 2026-01-25 | **Flux Version**: v2.1.0 | **Kustomize Version**: v5.0+
