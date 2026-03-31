# Deployment Guide

Complete deployment guide for the onboarding platform to production environments.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Local Development](#local-development)
3. [Docker Deployment](#docker-deployment)
4. [Kubernetes Deployment](#kubernetes-deployment)
5. [GCP Cloud Run](#gcp-cloud-run)
6. [Database Migration](#database-migration)
7. [Monitoring & Logging](#monitoring--logging)
8. [Scaling & Performance](#scaling--performance)
9. [Security Hardening](#security-hardening)
10. [Troubleshooting](#troubleshooting)

## Prerequisites

### Required Tools
- **Node.js 18+** - Runtime environment
- **Docker 20.10+** - Container platform
- **Docker Compose 2.0+** - Multi-container orchestration
- **Kubernetes 1.24+** - Container orchestration (for production)
- **kubectl 1.24+** - Kubernetes CLI
- **PostgreSQL 14+** - Database
- **Redis 7+** - Cache layer
- **Terraform 1.0+** - Infrastructure as code (optional)

### Cloud Credentials
- **GCP Project** with APIs enabled:
  - Cloud Run
  - Cloud SQL
  - Cloud Storage
  - Artifact Registry
  - Cloud Monitoring
- **AWS Credentials** (if using AWS):
  - S3 for backups
  - RDS for database
  - ElastiCache for Redis

### Network Requirements
- Domain name with DNS configured
- SSL/TLS certificate (Let's Encrypt or custom)
- Firewall rules allowing inbound HTTP/HTTPS

## Local Development

### Setup

```bash
# Clone repository
git clone https://github.com/your-org/onboarding.git
cd onboarding

# Install dependencies
npm install

# Copy environment template
cp .env.example .env

# Start development stack
docker-compose up -d

# Run database migrations
npm run db:migrate

# Seed initial data (optional)
npm run db:seed

# Start dev servers
npm run dev
```

### Access Services

- **Frontend**: http://localhost:3000
- **Backend API**: http://localhost:3001
- **PostgreSQL**: localhost:5432
- **Redis**: localhost:6379
- **Prometheus**: http://localhost:9090
- **Grafana**: http://localhost:3005 (admin/admin)
- **Jaeger**: http://localhost:16686

### Development Workflow

```bash
# Watch mode for backend
npm run backend:dev

# Watch mode for frontend
npm run frontend:dev

# Run tests
npm test

# Run tests in watch mode
npm run test:watch

# Linting
npm run lint

# Type checking
npm run typecheck

# Build
npm run build
```

## Docker Deployment

### Build Images

```bash
# Build backend
docker build -f backend/Dockerfile -t onboarding-backend:latest backend/

# Build frontend
docker build -f frontend/Dockerfile -t onboarding-frontend:latest frontend/

# Tag for registry
docker tag onboarding-backend:latest gcr.io/your-project/onboarding-backend:latest
docker tag onboarding-frontend:latest gcr.io/your-project/onboarding-frontend:latest
```

### Push to Registry

```bash
# Authenticate with GCR
gcloud auth configure-docker gcr.io

# Push images
docker push gcr.io/your-project/onboarding-backend:latest
docker push gcr.io/your-project/onboarding-frontend:latest
```

### Run with Docker Compose

```bash
# Start all services
docker-compose up -d

# View logs
docker-compose logs -f

# Stop services
docker-compose down

# Remove data
docker-compose down -v
```

## Kubernetes Deployment

### Create Namespace

```bash
kubectl create namespace onboarding
```

### Create Secrets

```bash
# Create secrets from YAML (edit values first!)
kubectl apply -f k8s/secrets.yaml

# Or create individually
kubectl create secret generic onboarding-secrets \
  -n onboarding \
  --from-literal=db_user=postgres \
  --from-literal=db_password=secure_password \
  --from-literal=jwt_secret=your_secret
```

### Deploy Application

```bash
# Apply ConfigMap
kubectl apply -f k8s/configmap.yaml

# Apply Deployments and Services
kubectl apply -f k8s/deployment.yaml

# Apply Ingress
kubectl apply -f k8s/ingress.yaml

# Verify deployment
kubectl get pods -n onboarding
kubectl get services -n onboarding
```

### Run Database Migrations

```bash
# Port forward to backend
kubectl port-forward svc/onboarding-backend 3001:3001 -n onboarding

# In another terminal, run migrations
npm run db:migrate -- --connection 'postgresql://postgres:password@localhost:5432/onboarding'
```

### Scale Deployments

```bash
# Scale backend to 5 replicas
kubectl scale deployment onboarding-backend -n onboarding --replicas=5

# Scale frontend to 3 replicas
kubectl scale deployment onboarding-frontend -n onboarding --replicas=3

# Auto-scaling with HPA
kubectl apply -f k8s/hpa.yaml
```

## GCP Cloud Run

### Prerequisites

```bash
# Set project
export PROJECT_ID=your-project-id
gcloud config set project $PROJECT_ID

# Enable APIs
gcloud services enable run.googleapis.com cloudsql.googleapis.com artifactregistry.googleapis.com
```

### Deploy Backend

```bash
# Build and push image
gcloud builds submit --tag gcr.io/$PROJECT_ID/onboarding-backend

# Deploy to Cloud Run
gcloud run deploy onboarding-backend \
  --image gcr.io/$PROJECT_ID/onboarding-backend \
  --platform managed \
  --region us-central1 \
  --allow-unauthenticated \
  --set-env-vars "DB_HOST=cloudsql-proxy,REDIS_HOST=redis" \
  --max-instances 100 \
  --memory 512Mi \
  --cpu 2
```

### Deploy Frontend

```bash
# Build and push image
gcloud builds submit -f frontend/Dockerfile --tag gcr.io/$PROJECT_ID/onboarding-frontend frontend/

# Deploy to Cloud Run
gcloud run deploy onboarding-frontend \
  --image gcr.io/$PROJECT_ID/onboarding-frontend \
  --platform managed \
  --region us-central1 \
  --allow-unauthenticated \
  --max-instances 50 \
  --memory 256Mi \
  --cpu 1
```

### Cloud SQL

```bash
# Create instance
gcloud sql instances create onboarding \
  --database-version POSTGRES_14 \
  --tier db-f1-micro \
  --region us-central1

# Create database
gcloud sql databases create onboarding \
  --instance onboarding

# Create user
gcloud sql users create onboarding \
  --instance onboarding \
  --password=secure_password

# Connect from Cloud Run
gcloud run services update onboarding-backend \
  --add-cloudsql-instances $PROJECT_ID:us-central1:onboarding \
  --set-env-vars "CLOUDSQL_CONNECTION_NAME=$PROJECT_ID:us-central1:onboarding"
```

### Cloud Memorystore (Redis)

```bash
# Create Redis instance
gcloud memorystore instances create onboarding-redis \
  --size 1 \
  --region us-central1

# Get Redis host
gcloud memorystore instances describe onboarding-redis \
  --region us-central1 \
  --format="value(host)"
```

## Database Migration

### Create Migration

```bash
# Create new migration file
npm run db:create-migration -- create_customers_table

# Edit the migration file in migrations/
```

### Run Migrations

```bash
# Run all pending migrations
npm run db:migrate

# Run specific migration
npm run db:migrate -- --target 001_initial_schema.sql

# Rollback last migration
npm run db:rollback
```

### Backup & Restore

```bash
# Backup database
pg_dump -U postgres -d onboarding -h localhost > backup.sql

# Restore database
psql -U postgres -d onboarding -h localhost < backup.sql

# Automated backups with Cloud SQL
gcloud sql backups create \
  --instance onboarding \
  --description "Manual backup"
```

## Monitoring & Logging

### Prometheus Metrics

```bash
# Access Prometheus
curl http://localhost:9090

# Query metrics
curl 'http://localhost:9090/api/v1/query?query=http_requests_total'
```

### Grafana Dashboards

```bash
# Access Grafana
open http://localhost:3005
# Login: admin/admin

# Import dashboards
# - Dashboard ID 3662 (Prometheus)
# - Dashboard ID 6417 (PostgreSQL)
# - Dashboard ID 11074 (Node Exporter)
```

### Cloud Logging (GCP)

```bash
# View logs
gcloud logging read "resource.type=cloud_run_revision AND resource.labels.service_name=onboarding-backend" \
  --limit 50 \
  --format json

# Stream logs
gcloud logging read --stream \
  "resource.type=cloud_run_revision AND resource.labels.service_name=onboarding-backend"
```

### Structured Logging

All services should log structured JSON:

```json
{
  "timestamp": "2024-01-25T10:00:00Z",
  "level": "INFO",
  "service": "onboarding-backend",
  "method": "GET",
  "path": "/api/v1/customers/123",
  "status": 200,
  "duration_ms": 45,
  "customerId": "550e8400-e29b-41d4-a716-446655440000",
  "userId": "user-123",
  "requestId": "req-abc123"
}
```

## Scaling & Performance

### Horizontal Scaling

```bash
# Kubernetes auto-scaling
kubectl apply -f k8s/hpa.yaml

# View HPA status
kubectl get hpa -n onboarding

# Manual scaling
kubectl scale deployment onboarding-backend -n onboarding --replicas=10
```

### Vertical Scaling

```bash
# Update resource requests/limits in deployment.yaml
kubectl apply -f k8s/deployment.yaml

# Check resource usage
kubectl top nodes
kubectl top pods -n onboarding
```

### Database Optimization

```sql
-- Analyze query performance
EXPLAIN ANALYZE
SELECT * FROM customers WHERE status = 'LIVE';

-- Vacuum and analyze
VACUUM ANALYZE customers;

-- Create indexes
CREATE INDEX idx_customers_status ON customers(status);
CREATE INDEX idx_value_calculations_timestamp ON value_calculations(timestamp);
```

### Caching Strategy

- Redis for session storage
- HTTP cache headers (ETag, Cache-Control)
- Query result caching for dashboard queries
- Frontend component caching with React Query

## Security Hardening

### Network Security

```bash
# Apply network policies
kubectl apply -f k8s/network-policy.yaml

# Verify policies
kubectl get networkpolicies -n onboarding
```

### Secrets Management

```bash
# Use Google Secret Manager
gcloud secrets create onboarding-db-password --data-file password.txt

# Grant service account access
gcloud secrets add-iam-policy-binding onboarding-db-password \
  --member serviceAccount:onboarding@$PROJECT_ID.iam.gserviceaccount.com \
  --role roles/secretmanager.secretAccessor
```

### SSL/TLS

```bash
# Create self-signed certificate (development)
openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -days 365 -nodes

# Use cert-manager for Let's Encrypt
kubectl apply -f https://github.com/jetstack/cert-manager/releases/download/v1.12.0/cert-manager.yaml
```

### Pod Security Policy

```bash
# Apply pod security standards
kubectl label namespace onboarding pod-security.kubernetes.io/enforce=restricted
```

## Troubleshooting

### Common Issues

```bash
# Pod not starting
kubectl describe pod onboarding-backend-xyz -n onboarding

# Check logs
kubectl logs onboarding-backend-xyz -n onboarding

# Database connection issues
kubectl run -it --rm debug --image=postgres:14 --restart=Never -- \
  psql -h postgres.onboarding -U postgres -d onboarding

# Redis connection issues
kubectl run -it --rm debug --image=redis:7 --restart=Never -- \
  redis-cli -h redis.onboarding ping
```

### Performance Issues

```bash
# Check slow queries
SELECT query, calls, mean_time FROM pg_stat_statements
ORDER BY mean_time DESC LIMIT 10;

# Monitor resource usage
kubectl top pods -n onboarding --containers

# Check network latency
kubectl run -it --rm debug --image=nicolaka/netshoot --restart=Never -- \
  ping redis.onboarding
```

### Debugging

```bash
# Enable debug logging
kubectl set env deployment onboarding-backend LOG_LEVEL=debug -n onboarding

# Port forward for local debugging
kubectl port-forward svc/onboarding-backend 3001:3001 -n onboarding
kubectl port-forward svc/onboarding-frontend 3000:3000 -n onboarding

# SSH into pod
kubectl exec -it onboarding-backend-xyz -n onboarding -- /bin/bash
```

---

**Last Updated**: 2024-01-25
**Version**: 1.0.0
