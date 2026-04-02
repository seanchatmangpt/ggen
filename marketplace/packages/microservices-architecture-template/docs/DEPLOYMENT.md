# Deployment Guide

## Overview

This guide covers deploying the microservices architecture to various environments: local development, Docker Compose, Kubernetes, and cloud platforms.

## Prerequisites

- Docker 24.0+
- Docker Compose 2.20+
- Kubernetes 1.28+ (for K8s deployment)
- kubectl configured
- Helm 3.12+ (optional, for Helm charts)

## Local Development

### Environment Setup

```bash
# Clone the generated project
cd my-microservices

# Install dependencies for each service
cd templates/api-gateway && cargo build
cd ../user-service && npm install
cd ../product-service && pip install -r requirements.txt
cd ../order-service && cargo build
```

### Run Services Individually

**PostgreSQL:**
```bash
docker run -d \
  --name postgres \
  -e POSTGRES_PASSWORD=postgres \
  -p 5432:5432 \
  postgres:15-alpine
```

**MongoDB:**
```bash
docker run -d \
  --name mongodb \
  -p 27017:27017 \
  mongo:7-alpine
```

**RabbitMQ:**
```bash
docker run -d \
  --name rabbitmq \
  -p 5672:5672 \
  -p 15672:15672 \
  rabbitmq:3.12-management-alpine
```

**Jaeger:**
```bash
docker run -d \
  --name jaeger \
  -e COLLECTOR_OTLP_ENABLED=true \
  -p 16686:16686 \
  -p 4318:4318 \
  jaegertracing/all-in-one:1.52
```

**User Service:**
```bash
cd templates/user-service
export DATABASE_URL=postgresql://postgres:postgres@localhost:5432/users
export JAEGER_AGENT_HOST=localhost
npm run dev
```

**Product Service:**
```bash
cd templates/product-service
export MONGO_URL=mongodb://localhost:27017/products
export JAEGER_AGENT_HOST=localhost
uvicorn main:app --reload --port 8002
```

**Order Service:**
```bash
cd templates/order-service
export DATABASE_URL=postgresql://postgres:postgres@localhost:5432/orders
export RABBITMQ_URL=amqp://guest:guest@localhost:5672
export JAEGER_AGENT_HOST=localhost
cargo run
```

**API Gateway:**
```bash
cd templates/api-gateway
export USER_SERVICE_URL=http://localhost:8001
export PRODUCT_SERVICE_URL=http://localhost:8002
export ORDER_SERVICE_URL=http://localhost:8003
export JAEGER_AGENT_HOST=localhost
cargo run
```

## Docker Compose Deployment

### Quick Start

```bash
# Build all services
docker-compose build

# Start all services
docker-compose up -d

# View logs
docker-compose logs -f

# Check service health
docker-compose ps
```

### Service-Specific Commands

```bash
# Start only infrastructure
docker-compose up -d postgres mongodb rabbitmq jaeger

# Start specific service
docker-compose up -d user-service

# Restart service
docker-compose restart api-gateway

# View service logs
docker-compose logs -f order-service

# Execute command in container
docker-compose exec user-service npm run migrate

# Scale service (if configured)
docker-compose up -d --scale user-service=3
```

### Configuration

Create `.env` file:

```bash
# Database
POSTGRES_USER=postgres
POSTGRES_PASSWORD=changeme123
POSTGRES_DB=microservices

# MongoDB
MONGO_INITDB_DATABASE=products

# RabbitMQ
RABBITMQ_DEFAULT_USER=admin
RABBITMQ_DEFAULT_PASS=changeme123

# Jaeger
JAEGER_AGENT_HOST=jaeger

# Services
USER_SERVICE_PORT=8001
PRODUCT_SERVICE_PORT=8002
ORDER_SERVICE_PORT=8003
API_GATEWAY_PORT=8000
```

### Healthcheck Validation

```bash
# Check all services are healthy
curl http://localhost:8000/health/live  # API Gateway
curl http://localhost:8001/health/live  # User Service
curl http://localhost:8002/health/live  # Product Service
curl http://localhost:8003/health/live  # Order Service

# Check readiness
curl http://localhost:8000/health/ready
```

### Troubleshooting Docker Compose

**Services not starting:**
```bash
# Check logs for errors
docker-compose logs

# Check specific service
docker-compose logs user-service

# Restart problematic service
docker-compose restart user-service
```

**Database connection issues:**
```bash
# Verify database is running
docker-compose ps postgres

# Test connection
docker-compose exec postgres psql -U postgres -c "SELECT 1"

# Check environment variables
docker-compose exec user-service env | grep DATABASE_URL
```

**Network connectivity:**
```bash
# Test service-to-service connectivity
docker-compose exec api-gateway ping user-service

# Check network
docker network ls
docker network inspect microservices_default
```

## Kubernetes Deployment

### Namespace Setup

```bash
# Create namespace
kubectl create namespace microservices

# Set as default
kubectl config set-context --current --namespace=microservices
```

### Deploy Infrastructure

```bash
# PostgreSQL
kubectl apply -f infrastructure/kubernetes/postgres-statefulset.yaml
kubectl apply -f infrastructure/kubernetes/postgres-service.yaml

# MongoDB
kubectl apply -f infrastructure/kubernetes/mongodb-statefulset.yaml
kubectl apply -f infrastructure/kubernetes/mongodb-service.yaml

# RabbitMQ
kubectl apply -f infrastructure/kubernetes/rabbitmq-statefulset.yaml
kubectl apply -f infrastructure/kubernetes/rabbitmq-service.yaml

# Jaeger
kubectl apply -f infrastructure/kubernetes/jaeger-deployment.yaml
kubectl apply -f infrastructure/kubernetes/jaeger-service.yaml
```

### Deploy Services

```bash
# ConfigMaps and Secrets
kubectl apply -f infrastructure/kubernetes/configmap.yaml
kubectl apply -f infrastructure/kubernetes/secrets.yaml

# Services
kubectl apply -f infrastructure/kubernetes/user-service-deployment.yaml
kubectl apply -f infrastructure/kubernetes/product-service-deployment.yaml
kubectl apply -f infrastructure/kubernetes/order-service-deployment.yaml
kubectl apply -f infrastructure/kubernetes/api-gateway-deployment.yaml

# Ingress
kubectl apply -f infrastructure/kubernetes/ingress.yaml
```

### Verify Deployment

```bash
# Check pods
kubectl get pods

# Check services
kubectl get svc

# Check deployments
kubectl get deployments

# Describe specific resource
kubectl describe pod api-gateway-xxxxx

# View logs
kubectl logs -f api-gateway-xxxxx

# Check events
kubectl get events --sort-by='.lastTimestamp'
```

### Scaling

```bash
# Manual scaling
kubectl scale deployment user-service --replicas=5

# Autoscaling
kubectl autoscale deployment user-service \
  --min=2 --max=10 \
  --cpu-percent=70

# Check HPA
kubectl get hpa
```

### Rolling Updates

```bash
# Update image
kubectl set image deployment/user-service \
  user-service=user-service:v2.0.0

# Check rollout status
kubectl rollout status deployment/user-service

# Rollback if needed
kubectl rollout undo deployment/user-service

# View rollout history
kubectl rollout history deployment/user-service
```

### Access Services

```bash
# Port forward for local access
kubectl port-forward service/api-gateway 8000:80

# Exec into pod
kubectl exec -it api-gateway-xxxxx -- /bin/sh

# Copy files from pod
kubectl cp api-gateway-xxxxx:/app/logs/app.log ./app.log
```

## Helm Deployment

### Install Helm Chart

```bash
# Add repository (if using external chart)
helm repo add microservices https://charts.example.com

# Install chart
helm install my-microservices ./helm/microservices \
  --namespace microservices \
  --create-namespace \
  --values values-production.yaml

# Verify installation
helm list -n microservices
kubectl get pods -n microservices
```

### Custom Values

Create `values-production.yaml`:

```yaml
global:
  environment: production
  imageRegistry: registry.example.com

apiGateway:
  replicas: 3
  resources:
    limits:
      cpu: 1000m
      memory: 1Gi
    requests:
      cpu: 500m
      memory: 512Mi
  autoscaling:
    enabled: true
    minReplicas: 3
    maxReplicas: 10
    targetCPUUtilizationPercentage: 70

userService:
  replicas: 5
  database:
    host: postgres-cluster.default.svc.cluster.local
    name: users_production

productService:
  replicas: 5
  database:
    host: mongodb-cluster.default.svc.cluster.local

orderService:
  replicas: 3
  messageQueue:
    host: rabbitmq-cluster.default.svc.cluster.local

ingress:
  enabled: true
  className: nginx
  annotations:
    cert-manager.io/cluster-issuer: letsencrypt-prod
  hosts:
    - host: api.example.com
      paths:
        - path: /
          pathType: Prefix
  tls:
    - secretName: api-tls
      hosts:
        - api.example.com
```

### Helm Operations

```bash
# Upgrade deployment
helm upgrade my-microservices ./helm/microservices \
  --values values-production.yaml

# Rollback
helm rollback my-microservices 1

# Uninstall
helm uninstall my-microservices -n microservices

# Dry run
helm install my-microservices ./helm/microservices \
  --dry-run --debug
```

## Cloud Platform Deployment

### AWS EKS

```bash
# Create EKS cluster
eksctl create cluster \
  --name microservices-cluster \
  --region us-east-1 \
  --nodegroup-name standard-workers \
  --node-type t3.medium \
  --nodes 3 \
  --nodes-min 1 \
  --nodes-max 10

# Configure kubectl
aws eks update-kubeconfig \
  --region us-east-1 \
  --name microservices-cluster

# Deploy services
kubectl apply -f infrastructure/kubernetes/
```

**AWS-specific resources:**
- RDS for PostgreSQL
- DocumentDB for MongoDB
- Amazon MQ for RabbitMQ
- X-Ray for distributed tracing
- ALB Ingress Controller

### Google GKE

```bash
# Create GKE cluster
gcloud container clusters create microservices-cluster \
  --zone us-central1-a \
  --num-nodes 3 \
  --machine-type n1-standard-2 \
  --enable-autoscaling \
  --min-nodes 1 \
  --max-nodes 10

# Get credentials
gcloud container clusters get-credentials \
  microservices-cluster \
  --zone us-central1-a

# Deploy services
kubectl apply -f infrastructure/kubernetes/
```

**GCP-specific resources:**
- Cloud SQL for PostgreSQL
- MongoDB Atlas
- Cloud Pub/Sub (alternative to RabbitMQ)
- Cloud Trace for distributed tracing

### Azure AKS

```bash
# Create resource group
az group create \
  --name microservices-rg \
  --location eastus

# Create AKS cluster
az aks create \
  --resource-group microservices-rg \
  --name microservices-cluster \
  --node-count 3 \
  --node-vm-size Standard_D2s_v3 \
  --enable-cluster-autoscaler \
  --min-count 1 \
  --max-count 10

# Get credentials
az aks get-credentials \
  --resource-group microservices-rg \
  --name microservices-cluster

# Deploy services
kubectl apply -f infrastructure/kubernetes/
```

**Azure-specific resources:**
- Azure Database for PostgreSQL
- Cosmos DB (MongoDB API)
- Azure Service Bus
- Application Insights

## Production Best Practices

### 1. Secrets Management

**Kubernetes Secrets:**
```bash
kubectl create secret generic database-credentials \
  --from-literal=username=postgres \
  --from-literal=password=changeme123

kubectl create secret generic api-keys \
  --from-literal=jwt-secret=your-secret-key
```

**External Secrets (HashiCorp Vault):**
```yaml
apiVersion: external-secrets.io/v1beta1
kind: SecretStore
metadata:
  name: vault-backend
spec:
  provider:
    vault:
      server: "https://vault.example.com"
      path: "secret"
      version: "v2"
```

### 2. Resource Limits

```yaml
resources:
  limits:
    cpu: 1000m
    memory: 1Gi
  requests:
    cpu: 500m
    memory: 512Mi
```

### 3. Health Checks

```yaml
livenessProbe:
  httpGet:
    path: /health/live
    port: 8000
  initialDelaySeconds: 30
  periodSeconds: 10
  timeoutSeconds: 5
  failureThreshold: 3

readinessProbe:
  httpGet:
    path: /health/ready
    port: 8000
  initialDelaySeconds: 10
  periodSeconds: 5
  timeoutSeconds: 3
  failureThreshold: 2
```

### 4. Persistent Storage

```yaml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: postgres-pvc
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 100Gi
  storageClassName: fast-ssd
```

### 5. Network Policies

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: api-gateway-policy
spec:
  podSelector:
    matchLabels:
      app: api-gateway
  policyTypes:
    - Ingress
    - Egress
  ingress:
    - from:
        - podSelector:
            matchLabels:
              app: ingress-nginx
  egress:
    - to:
        - podSelector:
            matchLabels:
              app: user-service
```

### 6. Monitoring Setup

```yaml
apiVersion: v1
kind: ServiceMonitor
metadata:
  name: api-gateway-monitor
spec:
  selector:
    matchLabels:
      app: api-gateway
  endpoints:
    - port: metrics
      interval: 30s
      path: /metrics
```

## Backup and Disaster Recovery

### Database Backups

**PostgreSQL:**
```bash
# Automated backup with CronJob
kubectl apply -f infrastructure/kubernetes/postgres-backup-cronjob.yaml

# Manual backup
kubectl exec -it postgres-0 -- pg_dumpall -U postgres > backup.sql

# Restore
kubectl exec -i postgres-0 -- psql -U postgres < backup.sql
```

**MongoDB:**
```bash
# Backup
kubectl exec -it mongodb-0 -- mongodump --out /backup

# Restore
kubectl exec -it mongodb-0 -- mongorestore /backup
```

### Disaster Recovery Plan

1. **Regular Backups**: Daily automated backups
2. **Multi-Region Deployment**: Active-passive setup
3. **RTO Target**: 1 hour
4. **RPO Target**: 15 minutes

## Monitoring Deployment Health

```bash
# Check pod status
kubectl get pods --watch

# Monitor resource usage
kubectl top pods
kubectl top nodes

# Check events
kubectl get events --sort-by='.lastTimestamp' | tail -20

# Validate services
for svc in api-gateway user-service product-service order-service; do
  kubectl get svc $svc
done
```

## Cleanup

**Docker Compose:**
```bash
docker-compose down -v  # Remove volumes
docker-compose down --rmi all  # Remove images
```

**Kubernetes:**
```bash
kubectl delete namespace microservices
helm uninstall my-microservices -n microservices
```

## Conclusion

This deployment guide covers the full lifecycle from local development to production Kubernetes deployment with cloud platform integration.
