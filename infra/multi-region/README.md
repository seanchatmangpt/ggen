# Multi-Region Deployment Architecture (ggen Marketplace)

**Status**: Production-Ready | **Version**: 1.0.0 | **Last Updated**: 2026-01-25

## Overview

This directory contains the complete multi-region deployment strategy for ggen Marketplace, deployed across three GCP regions:

- **Primary**: us-central1 (Iowa) - Primary writes, orchestration
- **Secondary**: us-east1 (South Carolina) - Read replicas, failover
- **Secondary**: europe-west1 (Belgium) - EU data residency, read replicas

**Topology**: Active-Active with Intelligent Failover
**RTO (Recovery Time Objective)**: 5 minutes
**RPO (Recovery Point Objective)**: 1 minute
**Availability Target**: 99.95% uptime (multi-region SLA)

## Architecture Decision: Active-Active

### Rationale

| Aspect | Active-Active | Active-Passive |
|--------|---------------|-----------------|
| **Availability** | 99.95% (better) | 99.9% (good) |
| **Failover Time** | <1 min (fast) | 5-10 min (slower) |
| **Cost** | Higher (3x resources) | Lower (standby overhead) |
| **Complexity** | Higher (eventual consistency) | Lower (simpler replication) |
| **Recovery** | Automatic | Manual intervention |
| **Use Case** | Global SaaS, high availability | Cost-sensitive, lower availability needs |

**Selection**: Active-Active chosen because:
1. **SaaS marketplace** requires high availability
2. **Global users** need low-latency access
3. **Automatic failover** without manual intervention
4. **Data distribution** across regions minimizes latency
5. **Cost justified** by reduced operational overhead and faster recovery

### Topology Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                      Global Load Balancer                       │
│           (Google Cloud Load Balancer + Cloud Armor)            │
└────────────────┬─────────────────────┬─────────────────────────┘
                 │                     │
        ┌────────▼────────┐    ┌──────▼───────────┐
        │   us-central1   │    │   europe-west1   │
        │   (Primary)     │    │   (Secondary)    │
        │                 │    │                  │
        │ ┌─────────────┐ │    │ ┌──────────────┐ │
        │ │ Cloud Run   │ │    │ │  Cloud Run   │ │
        │ │ Services    │ │    │ │  Services    │ │
        │ │ (3 replicas)│ │    │ │ (2 replicas) │ │
        │ └─────────────┘ │    │ └──────────────┘ │
        │ ┌─────────────┐ │    │ ┌──────────────┐ │
        │ │ Firestore   │ │◄─────┤ Firestore    │ │
        │ │ Multi-region│◄─────►│ Replica      │ │
        │ │ Database    │ │    │ └──────────────┘ │
        │ └─────────────┘ │    │                  │
        │ ┌─────────────┐ │    │ ┌──────────────┐ │
        │ │ Redis Cluster│ │   │ │ Redis Cluster│ │
        │ │ Sentinel    │ │    │ │ Sentinel     │ │
        │ └─────────────┘ │    │ └──────────────┘ │
        │ ┌─────────────┐ │    │ ┌──────────────┐ │
        │ │ Cloud KMS   │ │    │ │ Cloud KMS    │ │
        │ │ (Replicated)│ │    │ │ (Replicated) │ │
        │ └─────────────┘ │    │ └──────────────┘ │
        └─────────────────┘    └──────────────────┘
                 │                     │
        ┌────────▼─────────────────────▼────────┐
        │         us-east1 (Secondary)          │
        │                                       │
        │ ┌─────────────────────────────────┐  │
        │ │  Cloud Run Services (1 replica) │  │
        │ │  Read-only Firestore Replica    │  │
        │ │  Redis Replica (read-only)      │  │
        │ │  Cloud KMS Keys (replicated)    │  │
        │ └─────────────────────────────────┘  │
        └─────────────────────────────────────┘
```

## Directory Structure

```
infra/multi-region/
├── README.md                          # This file
├── ARCHITECTURE.md                    # Detailed technical architecture
├── DEPLOYMENT_GUIDE.md                # Step-by-step deployment procedures
├── RUNBOOK_FAILOVER.md                # Emergency failover procedures
├── RUNBOOK_RESTORE.md                 # Data recovery procedures
├── RUNBOOK_DR_TESTING.md              # Disaster recovery testing schedule
├── COST_ANALYSIS.md                   # Regional pricing and optimization
│
├── terraform/
│   ├── main.tf                        # Root module (multi-region orchestration)
│   ├── variables.tf                   # Root variables
│   ├── outputs.tf                     # Root outputs
│   ├── provider.tf                    # Provider configuration
│   ├── terraform.tfvars.example       # Example variables file
│   │
│   └── modules/
│       ├── firestore-regional/        # Firestore setup per region
│       │   ├── main.tf
│       │   ├── variables.tf
│       │   └── outputs.tf
│       │
│       ├── firestore-replication/     # Cross-region replication pipelines
│       │   ├── main.tf
│       │   ├── variables.tf
│       │   └── outputs.tf
│       │
│       ├── redis-regional/            # Redis Sentinel per region
│       │   ├── main.tf
│       │   ├── variables.tf
│       │   └── outputs.tf
│       │
│       ├── redis-replication/         # Redis cross-region sync
│       │   ├── main.tf
│       │   ├── variables.tf
│       │   └── outputs.tf
│       │
│       ├── load-balancer/             # Global Load Balancer
│       │   ├── main.tf
│       │   ├── variables.tf
│       │   └── outputs.tf
│       │
│       ├── health-checks/             # Regional health checks
│       │   ├── main.tf
│       │   ├── variables.tf
│       │   └── outputs.tf
│       │
│       ├── kms-regional/              # Cloud KMS per region
│       │   ├── main.tf
│       │   ├── variables.tf
│       │   └── outputs.tf
│       │
│       ├── backup-restore/            # Backup/restore pipelines
│       │   ├── main.tf
│       │   ├── variables.tf
│       │   └── outputs.tf
│       │
│       ├── monitoring/                # Cross-region monitoring
│       │   ├── main.tf
│       │   ├── variables.tf
│       │   └── outputs.tf
│       │
│       └── istio-traffic/             # Istio service mesh traffic mgmt
│           ├── main.tf
│           ├── variables.tf
│           └── outputs.tf
│
├── helm/
│   ├── values-us-central1.yaml        # Helm values per region
│   ├── values-us-east1.yaml
│   ├── values-europe-west1.yaml
│   ├── keda-hpa.yaml                  # KEDA autoscaling (cross-region)
│   └── istio-config.yaml              # Istio traffic management
│
└── scripts/
    ├── deploy-all-regions.sh          # Deploy all regions
    ├── failover-region.sh             # Emergency region failover
    ├── restore-from-backup.sh         # Data restoration
    ├── dr-test.sh                     # DR testing automation
    └── cost-report.sh                 # Regional cost analysis
```

## Key Features

### 1. Firestore Multi-Region Replication
- **Primary**: us-central1 (transactional writes)
- **Replicas**: us-east1, europe-west1 (eventual consistency)
- **Replication Lag**: < 5 seconds (99th percentile)
- **Conflict Resolution**: Vector Clocks + Last-Write-Wins with timestamps
- **Consistency Model**: Eventual consistency with causal ordering

### 2. Redis Cross-Region Failover
- **Primary**: us-central1 Redis Sentinel + Cluster
- **Replicas**: us-east1, europe-west1 (read-only initially)
- **Failover Trigger**: Health check (30s interval, 3 consecutive failures = failover)
- **Cache Invalidation**: Event-driven via Pub/Sub
- **Automatic Promotion**: Secondary to primary on health check failure

### 3. Traffic Management
- **Global Load Balancer**: Health-based routing with Cloud Armor
- **Istio Service Mesh**: Latency-based routing, circuit breakers, retries
- **KEDA Autoscaling**: CPU/memory/request-based scaling per region
- **Connection Draining**: Graceful shutdown on region degradation

### 4. Health Checks
- **Endpoint Checks**: Service readiness (every 30s)
- **Data Consistency Checks**: Replication lag monitoring (every 60s)
- **Failover Automation**: Automatic region switchover on health failure
- **Manual Override**: CLI tool for manual region management

### 5. Disaster Recovery
- **RTO**: 5 minutes (automated failover)
- **RPO**: 1 minute (point-in-time recovery)
- **Monthly DR Drills**: Simulated region failures
- **Backup Retention**: 30 days (daily snapshots)
- **Restore Testing**: Monthly verification of restore procedures

### 6. Secrets Management
- **Cloud KMS**: Per-region key rings with cross-region replication
- **HashiCorp Vault**: Optional HA cluster for secret management
- **Key Rotation**: Automated quarterly rotation
- **Audit Logging**: All secret access logged to Cloud Logging

### 7. Cost Optimization
- **Commitment Discounts**: CUDs for compute, storage, and networking
- **Network Optimization**: Private VPC peering to minimize egress
- **Regional Pricing**: Leverage cheaper regions for read replicas
- **Cost Tracking**: Automatic BigQuery exports for BI analysis

## Quick Start

### Deploy All Regions
```bash
cd infra/multi-region/terraform

# Initialize Terraform
terraform init -var-file=terraform.tfvars

# Plan deployment
terraform plan -out=tfplan

# Apply configuration (30-40 minutes)
terraform apply tfplan

# Verify deployment
terraform output
```

### Configure Helm Deployments
```bash
# Deploy to all regions
for region in us-central1 us-east1 europe-west1; do
  helm upgrade --install ggen-marketplace ./helm \
    -f helm/values-${region}.yaml \
    --namespace ggen \
    --kubeconfig=$HOME/.kube/${region}-config
done
```

### Test Failover
```bash
./scripts/dr-test.sh \
  --primary us-central1 \
  --secondary us-east1 \
  --test-type region-failure
```

## Monitoring & Alerting

### Key Metrics
- **Replication Lag**: us-central1 → us-east1 (target: <5s, alert >10s)
- **Health Check Success Rate**: (target: 99.9%, alert <98%)
- **Cross-Region Latency**: p95 latency (target: <200ms, alert >300ms)
- **Failover Count**: Number of automatic failovers (target: 0/month)
- **Data Consistency**: Documents synced across regions (target: 99.99%)

### Alerting Policies
All alerts configured in GCP Cloud Monitoring:
- Replication lag > 10 seconds → Page on-call engineer
- Health check failures in 2+ regions → Page incident commander
- Cross-region latency > 500ms → Alert to #incidents Slack channel
- Backup restoration failure → Page on-call DBA

## Documentation

- **ARCHITECTURE.md**: Detailed technical architecture and design decisions
- **DEPLOYMENT_GUIDE.md**: Step-by-step deployment procedures for each region
- **RUNBOOK_FAILOVER.md**: Emergency failover procedures with examples
- **RUNBOOK_RESTORE.md**: Data recovery and restore procedures
- **RUNBOOK_DR_TESTING.md**: Monthly DR testing schedule and procedures
- **COST_ANALYSIS.md**: Regional pricing, CUD strategies, optimization recommendations

## Support & Troubleshooting

See **RUNBOOK_*.md** files for:
- Emergency failover procedures
- Data corruption recovery
- Replication lag troubleshooting
- Cost anomaly detection
- Performance optimization

## Production Readiness Checklist

- [x] Multi-region Terraform modules (production-ready)
- [x] Health check automation (99.95% reliability)
- [x] Automatic failover logic (no manual intervention required)
- [x] Cross-region data replication (eventual consistency model)
- [x] Disaster recovery procedures (monthly testing schedule)
- [x] Backup and restore procedures (30-day retention)
- [x] Secrets replication and rotation (automated)
- [x] Cost tracking and optimization (monthly reports)
- [x] Comprehensive monitoring and alerting (per metric)
- [x] Complete runbook documentation (5+ procedures)

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2026-01-25 | Initial production release |
| | | - Multi-region Firestore with eventual consistency |
| | | - Redis cross-region failover with Sentinel |
| | | - Active-active topology with intelligent routing |
| | | - Automated health checks and region failover |
| | | - DR testing procedures (monthly schedule) |
| | | - Cost optimization and tracking |

---

**Maintainers**: Platform Engineering Team
**Last Reviewed**: 2026-01-25
**Next Review**: 2026-02-25
