<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ADR-011: Multi-Region Active-Active Topology](#adr-011-multi-region-active-active-topology)
  - [Problem Statement](#problem-statement)
  - [Decision](#decision)
  - [Rationale](#rationale)
    - [Active-Active Benefits](#active-active-benefits)
    - [Alternative: Active-Passive](#alternative-active-passive)
  - [Architecture](#architecture)
    - [Multi-Region Kubernetes Clusters](#multi-region-kubernetes-clusters)
    - [Failover Flow](#failover-flow)
  - [Firestore Multi-Region Replication](#firestore-multi-region-replication)
  - [Conflict Resolution](#conflict-resolution)
  - [DNS and Geolocation Routing](#dns-and-geolocation-routing)
  - [Disaster Recovery Testing](#disaster-recovery-testing)
  - [RTO and RPO](#rto-and-rpo)
  - [Monitoring Multi-Region](#monitoring-multi-region)
  - [Cost Implications](#cost-implications)
  - [Data Residency Compliance](#data-residency-compliance)
  - [Consequences](#consequences)
    - [Positive](#positive)
    - [Negative](#negative)
  - [Migration Path](#migration-path)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ADR-011: Multi-Region Active-Active Topology

**Status:** Accepted
**Date:** 2026-01-25
**Context:** Achieving global redundancy and low-latency access
**Deciders:** Disaster Recovery Team

## Problem Statement

TAI requires:
- Global availability (regions: us-central1, europe-west1, asia-southeast1)
- Low latency (users closer to data center)
- Disaster recovery (survive region failure)
- Data residency compliance (GDPR, data localization)

Single region insufficient for SLOs.

## Decision

**Active-active multi-region topology:**
- **Primary:** us-central1 (US users, business logic)
- **Secondary:** europe-west1 (EU users, GDPR compliance)
- **Tertiary:** asia-southeast1 (APAC users, low latency)
- **Replication:** Bidirectional between regions
- **Failover:** Automatic, DNS-based

## Rationale

### Active-Active Benefits
- No standby (all regions serve traffic)
- Automatic failover (no RTO)
- Load distribution
- Regional redundancy
- Cost efficient (no idle replicas)

### Alternative: Active-Passive
- Pros: Simpler (one writer)
- Cons: Higher RTO (minutes), failover complexity
- Rejected because: SLO requires <1 min recovery

## Architecture

### Multi-Region Kubernetes Clusters

```
┌─────────────────────────────────────────┐
│ Global Load Balancer (Anycast IP)       │
├─────────────┬─────────────┬─────────────┤
│ Region 1    │ Region 2    │ Region 3    │
│ us-central1 │ europe-west1│ asia-se-1   │
├─────────────┼─────────────┼─────────────┤
│ Cluster 1   │ Cluster 2   │ Cluster 3   │
│ 3 nodes     │ 3 nodes     │ 3 nodes     │
├─────────────┼─────────────┼─────────────┤
│ Services    │ Services    │ Services    │
│ (Governor,  │ (Governor,  │ (Governor,  │
│  Coord,     │  Coord,     │  Coord,     │
│  Scheduler) │  Scheduler) │  Scheduler) │
├─────────────┼─────────────┼─────────────┤
│ Firestore   │ Firestore   │ Firestore   │
│ (Region 1)  │ (Region 2)  │ (Region 3)  │
└──────┬──────┴──────┬──────┴──────┬──────┘
       │             │             │
       └─────────────┴─────────────┘
             Replication
```

### Failover Flow

```yaml
# Kubernetes MultiClusterEndpoint for failover
apiVersion: net.gke.io/v1
kind: MultiClusterEndpoint
metadata:
  name: tai-api
spec:
  template:
    spec:
      endpoints:
      - name: us-central1
        addresses:
        - "35.1.1.1"  # GLB in us-central1
        weight: 1
        priority: 0
      - name: europe-west1
        addresses:
        - "35.2.2.2"  # GLB in europe-west1
        weight: 1
        priority: 0
      - name: asia-southeast1
        addresses:
        - "35.3.3.3"  # GLB in asia-southeast1
        weight: 1
        priority: 0
```

## Firestore Multi-Region Replication

```yaml
# Firestore with multi-region failover
apiVersion: firestore.googleapis.com/v1
kind: FirestoreDatabase
metadata:
  name: tai-firestore
spec:
  type: FIRESTORE_NATIVE
  locations:
  - displayName: us-central1
    region: us-central1
    cmekConfig:
      kmsKeyName: "projects/ggen-project/locations/us/keyRings/firestore/cryptoKeys/key"
  - displayName: europe-west1
    region: europe-west1
    cmekConfig:
      kmsKeyName: "projects/ggen-project/locations/eu/keyRings/firestore/cryptoKeys/key"
  - displayName: asia-southeast1
    region: asia-southeast1
    cmekConfig:
      kmsKeyName: "projects/ggen-project/locations/asia/keyRings/firestore/cryptoKeys/key"
  replicationConfig:
    multiRegionWritePolicy:
      regions:
      - us-central1
      - europe-west1
      - asia-southeast1
```

## Conflict Resolution

When regions diverge (network partition):

```rust
// Last-write-wins (simple)
pub fn resolve_policy_conflict(
    version1: &Policy,
    version2: &Policy,
) -> Policy {
    if version1.updated_at_ns > version2.updated_at_ns {
        version1.clone()
    } else {
        version2.clone()
    }
}

// Causal consistency (stronger)
pub fn resolve_with_version_vector(
    local: &VersionedPolicy,
    remote: &VersionedPolicy,
) -> Result<VersionedPolicy> {
    if local.version_vector > remote.version_vector {
        Ok(local.clone())
    } else if remote.version_vector > local.version_vector {
        Ok(remote.clone())
    } else {
        // Concurrent writes: merge or manual
        Err("Conflict: manual resolution required".into())
    }
}
```

## DNS and Geolocation Routing

```yaml
# Cloud DNS with geolocation-based routing
apiVersion: dns.cnpg.io/v1
kind: ManagedZone
metadata:
  name: tai-example-com
spec:
  dnsName: "api.tai.example.com"
  routingPolicies:
  - geolocation:
      location: "northamerica"
      weight: 1
      recordSet:
      - name: "api.tai.example.com"
        type: "A"
        ttl: 60
        rrdatas:
        - "35.1.1.1"  # us-central1 GLB
  - geolocation:
      location: "europe"
      weight: 1
      recordSet:
      - name: "api.tai.example.com"
        type: "A"
        ttl: 60
        rrdatas:
        - "35.2.2.2"  # europe-west1 GLB
  - geolocation:
      location: "asia"
      weight: 1
      recordSet:
      - name: "api.tai.example.com"
        type: "A"
        ttl: 60
        rrdatas:
        - "35.3.3.3"  # asia-southeast1 GLB
```

## Disaster Recovery Testing

```yaml
# Monthly DR drill: failover simulation
apiVersion: batch/v1
kind: CronJob
metadata:
  name: dr-drill
  namespace: tai-system
spec:
  schedule: "0 2 1 * *"  # First day of month at 2 AM UTC
  jobTemplate:
    spec:
      template:
        spec:
          containers:
          - name: dr-test
            image: gcr.io/ggen-project/dr-tester:latest
            env:
            - name: PRIMARY_REGION
              value: "us-central1"
            - name: FAILOVER_REGION
              value: "europe-west1"
            - name: TEST_DURATION_MINUTES
              value: "15"
            - name: ALERT_EMAIL
              value: "ops-team@example.com"
            command:
            - /bin/sh
            - -c
            - |
              # Simulate region failure
              kubectl drain --all-nodes --ignore-daemonsets -l region=us-central1

              # Verify failover
              timeout 300 bash -c 'until curl -f http://api.tai.example.com/health; do sleep 5; done'

              # Check service continuity
              for i in {1..100}; do
                curl -X POST http://api.tai.example.com/v1/policies/propose \
                  -H "Content-Type: application/json" \
                  -d "{\"policy_type\":\"test\",\"policy_name\":\"drill-$i\"}" \
                  || exit 1
              done

              # Restore region
              kubectl uncordon --all-nodes -l region=us-central1

              # Verify rebalancing
              kubectl wait --for=condition=Ready nodes -l region=us-central1 --timeout=600s
```

## RTO and RPO

| Scenario | RTO | RPO |
|----------|-----|-----|
| Single node failure | <30s | 0 (replicated) |
| Zone failure | <1min | 0 (replicated) |
| Region failure | <5min | 0 (multi-region) |
| Data center disaster | <15min | 0 (multi-region) |

## Monitoring Multi-Region

```prometheus
# Replication lag
tai_replication_lag_seconds{from="us-central1",to="europe-west1"} 2.3

# Region-specific metrics
tai_requests_total{region="us-central1"} 1000000
tai_requests_total{region="europe-west1"} 750000
tai_requests_total{region="asia-southeast1"} 500000

# Failover events
tai_region_failover_total{from="us-central1",to="europe-west1"} 2
```

## Cost Implications

- **Compute:** 3x cluster cost (~$45k/month)
- **Storage:** 3x Firestore cost (~$15k/month for standard tier)
- **Networking:** Cross-region replication (~$8k/month)
- **Total:** ~$68k/month (but ensures uptime SLA)

Mitigation: Use committed use discounts (30-40% savings).

## Data Residency Compliance

```yaml
# EU-only storage (GDPR)
apiVersion: firestore.googleapis.com/v1
kind: FirestoreDatabase
metadata:
  name: tai-eu-only
spec:
  locations:
  - displayName: eu-west1
    region: eu-west1
    # Data never leaves EU
  deleteProtectionState: "DELETE_PROTECTION_ENABLED"
```

## Consequences

### Positive
- True geographic redundancy
- No single point of failure
- Low-latency global access
- Automatic failover
- Compliance ready

### Negative
- 3x infrastructure cost
- Conflict resolution complexity
- Operational complexity (manage 3 regions)
- Data consistency challenges
- Network partition handling needed

## Migration Path

1. **Phase 1:** Primary + secondary (us + eu)
2. **Phase 2:** Add tertiary region (asia)
3. **Phase 3:** Active-active synchronization
4. **Phase 4:** Automatic failover testing

## References
- [Google Cloud Multi-Region Deployment](https://cloud.google.com/architecture/multi-region-deployment)
- [Firestore Multi-Region](https://firebase.google.com/docs/firestore/locations)
- [DNS Failover](https://cloud.google.com/dns/docs/failover)
