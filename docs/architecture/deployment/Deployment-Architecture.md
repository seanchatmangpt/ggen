# TAI Deployment Architecture

Complete deployment topology for TAI system across GCP regions.

## Overview

TAI deploys to Google Cloud Platform (GCP) with 3-region active-active setup:
- **Primary:** us-central1 (North America)
- **Secondary:** europe-west1 (Europe)
- **Tertiary:** asia-southeast1 (Asia)

## Kubernetes Cluster Configuration

### Cluster Specs (Per Region)

```yaml
apiVersion: container.cnpg.io/v1
kind: Cluster
metadata:
  name: tai-cluster
  region: us-central1
spec:
  initialNodeCount: 3
  nodeConfig:
    machineType: n1-standard-4      # 4 vCPU, 15GB RAM
    diskSizeGb: 100
    oauthScopes:
      - https://www.googleapis.com/auth/cloud-platform
      - https://www.googleapis.com/auth/logging.write
      - https://www.googleapis.com/auth/monitoring

  addonsConfig:
    httpLoadBalancing:
      disabled: false
    horizontalPodAutoscaling:
      disabled: false
    networkPolicyConfig:
      disabled: false

  addonsConfig:
    cloudLoggingConfig:
      enabled: true
    cloudMonitoringConfig:
      enabled: true

  loggingService: logging.googleapis.com/kubernetes
  monitoringService: monitoring.googleapis.com/kubernetes
  networkPolicy:
    enabled: true
    provider: CALICO
```

### Node Configuration

```yaml
apiVersion: v1
kind: Node
metadata:
  labels:
    workload-type: general
    region: us-central1
    tier: compute
spec:
  capacity:
    cpu: 4
    memory: 15Gi
    pods: 110
```

### Node Pools (Workload Separation)

```yaml
# General workload pool
apiVersion: container.cnpg.io/v1
kind: NodePool
metadata:
  name: general-pool
spec:
  config:
    machineType: n1-standard-4
    diskSizeGb: 100
    nodeCount: 3
    autoScaling:
      minNodeCount: 3
      maxNodeCount: 10

---
# Memory-intensive workload pool (Coordinator)
apiVersion: container.cnpg.io/v1
kind: NodePool
metadata:
  name: memory-pool
spec:
  config:
    machineType: n1-highmem-8       # 8 vCPU, 52GB RAM
    diskSizeGb: 100
    nodeCount: 2
    autoScaling:
      minNodeCount: 2
      maxNodeCount: 5
  nodeSelector:
    workload-type: memory-intensive

---
# Spot instances (cost optimization)
apiVersion: container.cnpg.io/v1
kind: NodePool
metadata:
  name: spot-pool
spec:
  config:
    machineType: n1-standard-4
    preemptible: true              # Spot instances
    diskSizeGb: 100
    nodeCount: 2
    autoScaling:
      minNodeCount: 2
      maxNodeCount: 20
  nodeSelector:
    spot: "true"
```

## Namespace Organization

```
Kubernetes Namespaces
├── default
│   └── kube-system (standard K8s services)
│
├── istio-system
│   ├── istio-ingressgateway
│   ├── istio-egressgateway
│   └── istiod (control plane)
│
├── monitoring
│   ├── prometheus
│   ├── grafana
│   ├── alertmanager
│   └── kube-state-metrics
│
├── logging
│   ├── fluent-bit
│   ├── elasticsearch (optional)
│   └── kibana (optional)
│
├── tai-system (APPLICATION)
│   ├── governor (deployment)
│   ├── coordinator (deployment)
│   ├── scheduler (deployment)
│   ├── redis (statefulset)
│   └── configmaps/secrets
│
└── vault
    ├── vault (statefulset)
    ├── vault-agent-injector
    └── unseal key shards
```

## Service Deployments

### Governor Service Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: governor
  namespace: tai-system
spec:
  replicas: 3
  selector:
    matchLabels:
      app: governor
  template:
    metadata:
      labels:
        app: governor
        version: v1
    spec:
      serviceAccountName: governor
      affinity:
        # Spread across nodes
        podAntiAffinity:
          preferredDuringSchedulingIgnoredDuringExecution:
          - weight: 100
            podAffinityTerm:
              labelSelector:
                matchExpressions:
                - key: app
                  operator: In
                  values:
                  - governor
              topologyKey: kubernetes.io/hostname

      containers:
      - name: governor
        image: gcr.io/ggen-project/governor:v1.0.0
        imagePullPolicy: IfNotPresent
        ports:
        - name: grpc
          containerPort: 50051
        - name: metrics
          containerPort: 8080

        env:
        - name: RUST_LOG
          value: "info"
        - name: FIRESTORE_DATABASE
          value: "tai-firestore"
        - name: REDIS_URL
          value: "redis://redis:6379"
        - name: VAULT_ADDR
          value: "http://vault:8200"
        - name: VAULT_ROLE
          value: "tai-app"

        resources:
          requests:
            cpu: 250m
            memory: 256Mi
          limits:
            cpu: 1000m
            memory: 1Gi

        livenessProbe:
          grpc:
            port: 50051
            service: tai.Governor/HealthCheck
          initialDelaySeconds: 30
          periodSeconds: 10
          timeoutSeconds: 5
          failureThreshold: 3

        readinessProbe:
          grpc:
            port: 50051
            service: tai.Governor/HealthCheck
          initialDelaySeconds: 5
          periodSeconds: 5
          timeoutSeconds: 3
          failureThreshold: 2

        securityContext:
          runAsNonRoot: true
          runAsUser: 1000
          allowPrivilegeEscalation: false
          capabilities:
            drop:
            - ALL

---
apiVersion: v1
kind: Service
metadata:
  name: governor
  namespace: tai-system
  labels:
    app: governor
spec:
  type: ClusterIP
  ports:
  - name: grpc
    port: 50051
    targetPort: 50051
  - name: metrics
    port: 8080
    targetPort: 8080
  selector:
    app: governor
```

### StatefulSet for Redis

```yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: redis
  namespace: tai-system
spec:
  serviceName: redis
  replicas: 3
  selector:
    matchLabels:
      app: redis
  template:
    metadata:
      labels:
        app: redis
    spec:
      containers:
      - name: redis
        image: redis:7-alpine
        command:
          - redis-server
          - "--cluster-enabled"
          - "yes"
          - "--cluster-config-file"
          - "/data/nodes.conf"
          - "--cluster-node-timeout"
          - "5000"
          - "--appendonly"
          - "yes"
        ports:
        - containerPort: 6379
          name: client
        - containerPort: 16379
          name: gossip

        resources:
          requests:
            cpu: 100m
            memory: 128Mi
          limits:
            cpu: 500m
            memory: 512Mi

        volumeMounts:
        - name: data
          mountPath: /data

  volumeClaimTemplates:
  - metadata:
      name: data
    spec:
      accessModes: [ "ReadWriteOnce" ]
      storageClassName: standard
      resources:
        requests:
          storage: 10Gi
```

## Ingress Configuration

```yaml
# Istio Ingress Gateway
apiVersion: networking.istio.io/v1beta1
kind: Gateway
metadata:
  name: tai-gateway
  namespace: istio-system
spec:
  selector:
    istio: ingressgateway
  servers:
  - port:
      number: 443
      name: https
      protocol: HTTPS
    tls:
      mode: SIMPLE
      credentialName: tai-tls-cert
    hosts:
    - "api.tai.example.com"
    - "*.tai.example.com"

---
# Route requests to services
apiVersion: networking.istio.io/v1beta1
kind: VirtualService
metadata:
  name: tai-api
  namespace: tai-system
spec:
  hosts:
  - "api.tai.example.com"
  gateways:
  - istio-system/tai-gateway
  http:
  - match:
    - uri:
        prefix: "/tai.Governor"
    route:
    - destination:
        host: governor
        port:
          number: 50051
  - match:
    - uri:
        prefix: "/tai.Coordinator"
    route:
    - destination:
        host: coordinator
        port:
          number: 50052
  - match:
    - uri:
        prefix: "/tai.Scheduler"
    route:
    - destination:
        host: scheduler
        port:
          number: 50053
```

## Global Load Balancer

```yaml
apiVersion: compute.cnpg.io/v1
kind: GlobalForwardingRule
metadata:
  name: tai-global-lb
spec:
  IPProtocol: TCP
  ports:
  - 443
  target: tai-backend-service
  address: 35.1.2.3               # Anycast IP
  loadBalancingScheme: EXTERNAL

---
# Health check for backends
apiVersion: compute.cnpg.io/v1
kind: HealthCheck
metadata:
  name: tai-health-check
spec:
  type: TCP
  tcpHealthCheck:
    port: 50051
  checkIntervalSec: 10
  timeoutSec: 5
  healthyThreshold: 2
  unhealthyThreshold: 3
```

## DNS Configuration (Geolocation-based Routing)

```yaml
apiVersion: dns.cnpg.io/v1
kind: ManagedZone
metadata:
  name: tai-example-com
spec:
  dnsName: tai.example.com
  recordSets:
  # North America
  - name: api.tai.example.com
    type: A
    ttl: 60
    routingPolicy:
      geo:
        location: northamerica
      weight: 1
    rrdatas:
    - 35.1.1.1    # us-central1 GLB IP

  # Europe
  - name: api.tai.example.com
    type: A
    ttl: 60
    routingPolicy:
      geo:
        location: europe
      weight: 1
    rrdatas:
    - 35.2.2.2    # europe-west1 GLB IP

  # Asia
  - name: api.tai.example.com
    type: A
    ttl: 60
    routingPolicy:
      geo:
        location: asia
      weight: 1
    rrdatas:
    - 35.3.3.3    # asia-southeast1 GLB IP
```

## Helm Chart Structure

```
helm/tai-chart/
├── Chart.yaml
├── values.yaml              # Default values
├── values-us-central1.yaml
├── values-europe-west1.yaml
├── values-asia-southeast1.yaml
└── templates/
    ├── governor-deployment.yaml
    ├── coordinator-deployment.yaml
    ├── scheduler-deployment.yaml
    ├── redis-statefulset.yaml
    ├── service.yaml
    ├── configmap.yaml
    ├── secret.yaml
    └── serviceaccount.yaml
```

## Deployment Steps

```bash
# 1. Create GKE clusters (per region)
gcloud container clusters create tai-cluster \
  --region us-central1 \
  --num-nodes 3

# 2. Install Istio
istioctl install --set profile=production -y

# 3. Install monitoring stack
helm install prometheus prometheus-community/kube-prometheus-stack \
  -n monitoring --create-namespace

# 4. Deploy Vault
helm install vault hashicorp/vault \
  -n vault --create-namespace

# 5. Deploy TAI services
helm install tai ./helm/tai-chart \
  -n tai-system --create-namespace \
  -f helm/tai-chart/values-us-central1.yaml

# 6. Configure DNS failover
gcloud dns managed-zones create tai-example-com \
  --dns-name=tai.example.com

# 7. Deploy autoscaling
kubectl apply -f k8s/hpa.yaml
kubectl apply -f k8s/keda-triggers.yaml
```

## Rollout Strategy

```bash
# Blue-green deployment
kubectl set image deployment/governor \
  governor=gcr.io/ggen-project/governor:v2.0.0 \
  --record

# Monitor rollout
kubectl rollout status deployment/governor -w

# Rollback if needed
kubectl rollout undo deployment/governor
```

## References
- Kubernetes Documentation: https://kubernetes.io/docs/
- GKE Best Practices: https://cloud.google.com/kubernetes-engine/docs/best-practices
- Istio Documentation: https://istio.io/latest/docs/
