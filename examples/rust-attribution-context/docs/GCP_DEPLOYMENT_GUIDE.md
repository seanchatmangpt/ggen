# GCP Deployment Guide - FactoryPaaS Attribution Service

## Overview

This guide covers deploying the production-ready Attribution Service on Google Cloud Platform (GCP) using serverless Cloud Run, Cloud SQL PostgreSQL, Cloud Storage with CDN, and comprehensive monitoring.

**Architecture**: Serverless + Event Sourcing + CQRS
**Estimated Cost**: $497-$850 USD/month
**High Availability**: Regional Cloud SQL + Read Replica + Auto-scaling

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Architecture Overview](#architecture-overview)
3. [Infrastructure Components](#infrastructure-components)
4. [Deployment Steps](#deployment-steps)
5. [Configuration](#configuration)
6. [Monitoring & Observability](#monitoring--observability)
7. [Security](#security)
8. [Cost Optimization](#cost-optimization)
9. [Disaster Recovery](#disaster-recovery)
10. [Troubleshooting](#troubleshooting)

---

## Prerequisites

### Required Tools

```bash
# Install gcloud CLI
curl https://sdk.cloud.google.com | bash
exec -l $SHELL

# Install Terraform
wget https://releases.hashicorp.com/terraform/1.6.0/terraform_1.6.0_linux_amd64.zip
unzip terraform_1.6.0_linux_amd64.zip
sudo mv terraform /usr/local/bin/

# Install Docker
curl -fsSL https://get.docker.com | sh

# Verify installations
gcloud --version
terraform --version
docker --version
```

### GCP Project Setup

```bash
# Authenticate with GCP
gcloud auth login
gcloud auth application-default login

# Set project
export PROJECT_ID="your-gcp-project-id"
gcloud config set project $PROJECT_ID

# Enable billing
gcloud beta billing accounts list
gcloud beta billing projects link $PROJECT_ID --billing-account=BILLING_ACCOUNT_ID
```

---

## Architecture Overview

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Internet Traffic                        │
└────────────────────┬──────────────────┬─────────────────────┘
                     │                  │
                     ▼                  ▼
            ┌────────────────┐  ┌────────────────┐
            │  Cloud Armor   │  │  Cloud CDN     │
            │  (DDoS/WAF)    │  │  (Static)      │
            └────────┬───────┘  └────────┬───────┘
                     │                   │
                     ▼                   ▼
            ┌────────────────┐  ┌────────────────┐
            │  Cloud Run     │  │ Cloud Storage  │
            │  (API Service) │  │ (Bucket)       │
            └────────┬───────┘  └────────────────┘
                     │
                     │ VPC Connector
                     │
            ┌────────▼───────┐
            │  Private VPC   │
            │  10.0.0.0/16   │
            └────────┬───────┘
                     │
            ┌────────▼───────────────┐
            │  Cloud SQL PostgreSQL  │
            │  (Event Store)         │
            │  Primary + Replica     │
            └────────────────────────┘
```

### Components

1. **Cloud Run**: Serverless container platform for API service
2. **Cloud SQL**: Managed PostgreSQL for event sourcing
3. **Cloud Storage**: Static content hosting with CDN
4. **VPC**: Private networking with Cloud Armor protection
5. **Cloud Monitoring**: Observability and alerting
6. **Secret Manager**: Secure credential storage
7. **Artifact Registry**: Container image storage
8. **Cloud KMS**: Encryption key management

---

## Infrastructure Components

### Cloud Run Service

- **Compute**: 2 vCPU, 2GB RAM per instance
- **Auto-scaling**: 1-10 instances based on CPU/requests
- **Timeout**: 300 seconds
- **Concurrency**: 80 requests per instance
- **Health Checks**: Startup, liveness, readiness probes

### Cloud SQL PostgreSQL

- **Tier**: db-custom-2-7680 (2 vCPU, 7.5GB RAM)
- **Availability**: Regional (high availability)
- **Storage**: 100GB SSD with auto-resize
- **Backups**: Daily automated backups, 30-day retention
- **PITR**: Point-in-time recovery enabled
- **Replica**: Read replica in us-east1 for analytics

### Cloud Storage

- **Static Bucket**: Multi-region US, versioning enabled
- **Backup Bucket**: Encrypted with KMS, lifecycle policies
- **Log Archive**: 365-day retention, tiered storage
- **CDN**: Global Cloud CDN with cache optimization

### Networking

- **VPC**: Private network (10.0.0.0/16)
- **Subnets**: Public (10.0.1.0/24), Private (10.0.2.0/24)
- **VPC Connector**: Serverless VPC Access (10.8.0.0/28)
- **Cloud NAT**: Outbound connectivity for private resources
- **Cloud Armor**: DDoS protection, rate limiting, WAF

---

## Deployment Steps

### 1. Generate Terraform Configuration

```bash
# Navigate to example directory
cd examples/rust-attribution-context

# Generate Terraform files from ontology
ggen sync --audit true

# Verify generated files
ls -la generated/terraform/gcp_*.tf
```

### 2. Configure Variables

Create `terraform.tfvars`:

```hcl
# Required variables
project_id  = "your-gcp-project-id"
region      = "us-central1"
environment = "prod"

# Monitoring
alert_email       = "alerts@example.com"
slack_channel     = "#production-alerts"
slack_webhook_url = "https://hooks.slack.com/services/YOUR/WEBHOOK/URL"

# Custom domain
domain_name = "attribution.example.com"

# Feature flags
enable_cdn                 = true
enable_high_availability   = true
enable_cloudsql_replica    = true
enable_binary_authorization = true
enable_vpc_flow_logs       = true

# Scaling
min_cloud_run_instances = 1
max_cloud_run_instances = 10

# Tags
tags = {
  environment = "production"
  team        = "platform"
  cost_center = "engineering"
}
```

### 3. Initialize Terraform

```bash
cd generated/terraform

# Initialize Terraform
terraform init

# Validate configuration
terraform validate

# Preview changes
terraform plan -out=tfplan
```

### 4. Deploy Infrastructure

```bash
# Apply infrastructure
terraform apply tfplan

# Wait for deployment (10-15 minutes)
# Save outputs
terraform output -json > outputs.json
```

### 5. Build and Deploy Container

```bash
# Get Artifact Registry URL
REGISTRY_URL=$(terraform output -raw artifact_registry_repository_url)

# Build container
docker build -t $REGISTRY_URL/attribution-service:latest .

# Authenticate Docker
gcloud auth configure-docker us-central1-docker.pkg.dev

# Push image
docker push $REGISTRY_URL/attribution-service:latest

# Deploy to Cloud Run (already configured via Terraform)
# Container will auto-deploy on push if using Cloud Build triggers
```

### 6. Run Database Migrations

```bash
# Get database connection from Secret Manager
gcloud secrets versions access latest --secret="db-connection-string" > db-url.txt

# Run migrations (using your preferred migration tool)
export DATABASE_URL=$(cat db-url.txt)
cargo run --bin migrate-db

# Or use Cloud Run Jobs for migrations
gcloud run jobs create db-migrate \
  --image=$REGISTRY_URL/attribution-service:latest \
  --service-account=db-migrator@$PROJECT_ID.iam.gserviceaccount.com \
  --region=us-central1 \
  --args="migrate"
```

### 7. Configure DNS

```bash
# Get IP addresses
API_IP=$(terraform output -raw cloud_run_ip_address)
CDN_IP=$(terraform output -raw cdn_ip_address)

# Add DNS A records (in your DNS provider):
# api.attribution.example.com  → $API_IP
# static.attribution.example.com → $CDN_IP

# Wait for DNS propagation (1-24 hours)
dig api.attribution.example.com
```

### 8. Verify Deployment

```bash
# Test Cloud Run service
SERVICE_URL=$(terraform output -raw cloud_run_service_url)
curl -i $SERVICE_URL/health/ready

# Expected response:
# HTTP/2 200
# {"status": "healthy", "database": "connected", "version": "1.0.0"}

# Test custom domain (after DNS propagation)
curl -i https://api.attribution.example.com/health/ready
```

---

## Configuration

### Environment Variables

Cloud Run service automatically receives:

- `DATABASE_URL`: PostgreSQL connection string (from Secret Manager)
- `RUST_LOG`: Logging configuration (info,attribution_context=debug)
- `OTEL_EXPORTER_OTLP_ENDPOINT`: OpenTelemetry endpoint (Cloud Trace)

### Secrets Management

All sensitive data stored in Secret Manager:

```bash
# List secrets
gcloud secrets list

# View secret metadata
gcloud secrets describe db-connection-string

# Access secret (requires permissions)
gcloud secrets versions access latest --secret="db-connection-string"
```

### Scaling Configuration

Auto-scaling based on:

1. **CPU Utilization**: Scale at 70% CPU
2. **Request Rate**: Scale at 100 requests/instance
3. **Min Instances**: 1 (always warm)
4. **Max Instances**: 10 (prevent runaway costs)

Adjust in `terraform.tfvars`:

```hcl
min_cloud_run_instances = 2  # Higher for guaranteed capacity
max_cloud_run_instances = 50 # Higher for traffic spikes
```

---

## Monitoring & Observability

### Cloud Monitoring Dashboard

Access dashboard:

```bash
DASHBOARD_ID=$(terraform output -raw monitoring_dashboard_id)
echo "https://console.cloud.google.com/monitoring/dashboards/custom/$DASHBOARD_ID?project=$PROJECT_ID"
```

Dashboard includes:

- Request rate (requests/sec)
- Response latency (P50, P95, P99)
- Error rate by status code
- Database connection count
- CPU and memory utilization

### Uptime Checks

Global uptime monitoring from 3 regions:

- USA
- Europe
- Asia-Pacific

Check every 60 seconds, alert on failure.

### Alerts

Configured alerts:

1. **Service Availability**: Alert if uptime check fails (critical)
2. **Error Rate**: Alert if 5xx rate > 5% (high)
3. **Latency**: Alert if P95 > 1000ms (warning)
4. **Database Connections**: Alert if connections > 150 (warning)
5. **Disk Utilization**: Alert if disk > 80% (high)

Notifications sent to:

- Email: `alert_email` variable
- Slack: `slack_webhook_url` variable

### Logs

Logs automatically exported to:

1. **Cloud Logging**: Real-time log viewing
2. **Log Archive Bucket**: Long-term storage (365 days)
3. **Tiered Storage**: NEARLINE after 30 days, delete after 365 days

View logs:

```bash
# View Cloud Run logs
gcloud logging read "resource.type=cloud_run_revision AND resource.labels.service_name=attribution-service" --limit 50

# View Cloud SQL logs
gcloud logging read "resource.type=cloudsql_database" --limit 50
```

### Tracing

OpenTelemetry integration with Cloud Trace:

```bash
# View traces
gcloud trace list

# Open trace in console
echo "https://console.cloud.google.com/traces/list?project=$PROJECT_ID"
```

---

## Security

### Network Security

1. **Private Cloud SQL**: No public IP, VPC peering only
2. **Cloud Armor**: DDoS protection, rate limiting (100 req/min/IP)
3. **WAF Rules**: Block SQL injection, XSS, LFI attempts
4. **VPC Firewall**: Allow only necessary traffic
5. **Cloud NAT**: Outbound internet for private resources

### Encryption

1. **In Transit**: TLS 1.2+ for all connections
2. **At Rest**: Cloud SQL encrypted by default
3. **Backups**: Encrypted with Cloud KMS
4. **Secrets**: Secret Manager with auto-rotation

### IAM & Access Control

Least privilege service accounts:

- **Cloud Run SA**: cloudsql.client, cloudtrace.agent, monitoring.metricWriter, secretmanager.secretAccessor
- **CI/CD SA**: run.admin, storage.admin, artifactregistry.writer
- **DB Migrator SA**: cloudsql.client, secretmanager.secretAccessor

### Binary Authorization

Container images must be attested before deployment:

```bash
# Sign image
gcloud container binauthz attestations sign-and-create \
  --artifact-url=$REGISTRY_URL/attribution-service:latest \
  --attestor=attribution-attestor \
  --keyversion=1
```

### Audit Logging

Comprehensive audit logs for all services:

- Admin read
- Data read
- Data write

Retention: 365 days in Cloud Storage.

---

## Cost Optimization

### Estimated Monthly Costs

| Component               | Estimated Cost  |
|-------------------------|-----------------|
| Cloud Run               | $50-100         |
| Cloud SQL (primary)     | $150-200        |
| Cloud SQL (replica)     | $150-200        |
| Cloud Storage           | $5-10           |
| Cloud CDN               | $80-120         |
| Monitoring/Logging      | $20-50          |
| VPC/Networking          | $40-60          |
| Secret Manager/KMS      | $2-10           |
| **Total**               | **$497-850**    |

### Cost Reduction Strategies

1. **Disable Read Replica** (saves $150-200/month):
   ```hcl
   enable_cloudsql_replica = false
   ```

2. **Reduce Cloud Run Min Instances** (saves ~$25/month):
   ```hcl
   min_cloud_run_instances = 0  # Cold starts, but cheaper
   ```

3. **Use Zonal Cloud SQL** (saves ~$50/month):
   ```hcl
   enable_high_availability = false  # No multi-zone
   ```

4. **Reduce Backup Retention** (saves ~$5/month):
   ```hcl
   cloudsql_backup_retention_days = 7
   ```

5. **Optimize CDN Caching**:
   - Increase cache TTL (reduce origin requests)
   - Enable compression
   - Use cache invalidation sparingly

### Budget Alerts

Budget alerts configured at:

- 50% of budget ($425)
- 75% of budget ($638)
- 90% of budget ($765)
- 100% of budget ($850)

---

## Disaster Recovery

### Backup Strategy

**Cloud SQL Backups**:

- **Automated Backups**: Daily at 03:00 UTC
- **Retention**: 30 days
- **Point-in-Time Recovery**: 7 days
- **Storage**: Encrypted with KMS

**Manual Backups**:

```bash
# Create on-demand backup
gcloud sql backups create --instance=attribution-event-store

# List backups
gcloud sql backups list --instance=attribution-event-store

# Restore from backup
gcloud sql backups restore BACKUP_ID --backup-instance=attribution-event-store --backup-instance=attribution-event-store
```

### Disaster Recovery Procedures

**Scenario 1: Cloud Run Service Failure**

```bash
# Auto-recovers via Cloud Run health checks
# Manual force restart:
gcloud run services update attribution-service --region=us-central1
```

**Scenario 2: Cloud SQL Primary Failure**

```bash
# Regional Cloud SQL auto-fails over to standby
# Monitor failover:
gcloud sql operations list --instance=attribution-event-store

# Promote replica to primary (if needed):
gcloud sql instances promote-replica attribution-event-store-replica
```

**Scenario 3: Complete Region Failure**

1. Promote read replica in us-east1 to primary
2. Deploy Cloud Run service to us-east1
3. Update DNS to point to us-east1
4. Verify service health

**Scenario 4: Data Corruption**

```bash
# Point-in-time recovery
gcloud sql instances restore-backup attribution-event-store \
  --backup-id=BACKUP_ID

# Or restore to specific timestamp (within 7 days)
gcloud sql instances clone attribution-event-store attribution-event-store-clone \
  --point-in-time='2024-01-24T12:00:00Z'
```

### RTO & RPO

- **RTO (Recovery Time Objective)**: < 15 minutes (regional failover)
- **RPO (Recovery Point Objective)**: < 5 minutes (PITR with transaction logs)

---

## Troubleshooting

### Common Issues

**1. Cloud Run 503 Service Unavailable**

```bash
# Check logs
gcloud logging read "resource.type=cloud_run_revision AND severity>=ERROR" --limit 50

# Check service status
gcloud run services describe attribution-service --region=us-central1

# Check database connectivity
gcloud sql instances describe attribution-event-store

# Verify VPC connector
gcloud compute networks vpc-access connectors describe attribution-vpc-connector --region=us-central1
```

**2. Database Connection Errors**

```bash
# Verify Cloud SQL instance is running
gcloud sql instances list

# Check private IP connectivity
gcloud compute networks peerings list --network=attribution-vpc

# Test connection from Cloud Shell
gcloud sql connect attribution-event-store --user=attribution_app

# Check database user permissions
gcloud sql users list --instance=attribution-event-store
```

**3. High Latency**

```bash
# Check Cloud SQL performance insights
gcloud sql instances describe attribution-event-store --format="get(settings.insightsConfig)"

# Review slow queries
gcloud logging read "resource.type=cloudsql_database AND jsonPayload.duration > 1000" --limit 50

# Check Cloud Run cold starts
gcloud run services describe attribution-service --region=us-central1 --format="get(spec.template.spec.containers[0].startupProbe)"
```

**4. Cost Overruns**

```bash
# Check current spend
gcloud billing accounts list
gcloud billing budgets list --billing-account=BILLING_ACCOUNT_ID

# Analyze costs by service
echo "https://console.cloud.google.com/billing/reports?project=$PROJECT_ID"

# Identify top cost drivers
gcloud logging read "protoPayload.methodName=~'compute|sql|storage'" --format=json | jq '.[] | .protoPayload.resourceName'
```

**5. Authentication Errors**

```bash
# Verify service account permissions
gcloud projects get-iam-policy $PROJECT_ID --flatten="bindings[].members" --filter="bindings.members:serviceAccount:attribution-service-sa@*"

# Check Secret Manager access
gcloud secrets get-iam-policy db-connection-string

# Verify Cloud SQL IAM bindings
gcloud sql instances describe attribution-event-store --format="get(settings.ipConfiguration.authorizedNetworks)"
```

### Debug Mode

Enable verbose logging:

```bash
# Update Cloud Run service with debug logging
gcloud run services update attribution-service \
  --update-env-vars RUST_LOG=debug,attribution_context=trace \
  --region=us-central1

# View real-time logs
gcloud logging tail "resource.type=cloud_run_revision"
```

### Health Check Endpoints

```bash
# Startup probe: /health/startup
curl https://api.attribution.example.com/health/startup

# Liveness probe: /health/live
curl https://api.attribution.example.com/health/live

# Readiness probe: /health/ready
curl https://api.attribution.example.com/health/ready
```

---

## Production Checklist

Before going live:

- [ ] DNS records configured and propagated
- [ ] SSL certificates verified
- [ ] Database migrations applied
- [ ] Monitoring dashboard reviewed
- [ ] Alert policies tested
- [ ] Budget alerts configured
- [ ] Security scans passed (Binary Authorization)
- [ ] Load testing completed
- [ ] Backup restoration tested
- [ ] Disaster recovery plan documented
- [ ] Runbook created for on-call team
- [ ] Cost optimization reviewed

---

## Support & Resources

- **Documentation**: [ggen Repository](https://github.com/seanchatmangpt/ggen)
- **GCP Console**: https://console.cloud.google.com
- **Cloud Run Docs**: https://cloud.google.com/run/docs
- **Cloud SQL Docs**: https://cloud.google.com/sql/docs
- **Terraform GCP Provider**: https://registry.terraform.io/providers/hashicorp/google

For issues, open a ticket: https://github.com/seanchatmangpt/ggen/issues

---

**Generated by ggen v6.0.0** - Production-ready infrastructure from RDF ontology.
