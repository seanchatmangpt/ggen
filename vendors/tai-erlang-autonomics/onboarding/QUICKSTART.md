# Quick Start Guide

Get the onboarding platform running in 5 minutes.

## Option 1: Docker Compose (Recommended for Development)

### 1. Prerequisites

```bash
# Check Docker installation
docker --version
docker-compose --version

# Should output: Docker version 20.10+ and docker-compose 2.0+
```

### 2. Clone and Setup

```bash
# Clone repository (replace with your repo)
git clone https://github.com/your-org/onboarding.git
cd onboarding

# Copy environment file
cp .env.example .env

# Start services
docker-compose up -d
```

### 3. Verify Setup

```bash
# Wait 30 seconds for services to start
sleep 30

# Check services are running
docker-compose ps

# Should show all services as "Up"
```

### 4. Access Application

```bash
# Frontend
open http://localhost:3000

# Backend API
curl http://localhost:3001/health

# Prometheus Metrics
open http://localhost:9090

# Grafana Dashboard
open http://localhost:3005 (login: admin/admin)

# Jaeger Tracing
open http://localhost:16686
```

### 5. Create Test Customer

```bash
# Get JWT token
TOKEN=$(curl -X POST http://localhost:3001/api/v1/auth/login \
  -H "Content-Type: application/json" \
  -d '{"email":"test@example.com","password":"test"}' \
  | jq -r '.data.token')

# Create customer
curl -X POST http://localhost:3001/api/v1/customers \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "Test Company",
    "email": "company@example.com"
  }'
```

## Option 2: Kubernetes (Production-Ready)

### 1. Prerequisites

```bash
# Install kubectl
kubectl version --client

# Access to Kubernetes cluster (local or cloud)
kubectl cluster-info
```

### 2. Deploy

```bash
# Create namespace
kubectl create namespace onboarding

# Create secrets (edit values first!)
kubectl apply -f k8s/secrets.yaml

# Create config
kubectl apply -f k8s/configmap.yaml

# Deploy application
kubectl apply -f k8s/deployment.yaml

# Deploy ingress
kubectl apply -f k8s/ingress.yaml
```

### 3. Verify

```bash
# Check deployments
kubectl get deployments -n onboarding

# Check pods
kubectl get pods -n onboarding

# Check services
kubectl get services -n onboarding

# View logs
kubectl logs -f deployment/onboarding-backend -n onboarding
```

### 4. Access

```bash
# Port forward to frontend
kubectl port-forward svc/onboarding-frontend 3000:3000 -n onboarding &

# Port forward to backend
kubectl port-forward svc/onboarding-backend 3001:3001 -n onboarding &

# Access
open http://localhost:3000
```

## Option 3: GCP Cloud Run (Easiest Cloud Deployment)

### 1. Prerequisites

```bash
# Install gcloud CLI
curl https://sdk.cloud.google.com | bash

# Authenticate
gcloud auth login
gcloud auth application-default login

# Set project
export PROJECT_ID=your-project-id
gcloud config set project $PROJECT_ID

# Enable APIs
gcloud services enable run.googleapis.com cloudsql.googleapis.com artifactregistry.googleapis.com
```

### 2. Deploy

```bash
# Deploy backend
gcloud run deploy onboarding-backend \
  --source backend \
  --region us-central1 \
  --allow-unauthenticated \
  --memory 512M \
  --cpu 2

# Deploy frontend
gcloud run deploy onboarding-frontend \
  --source frontend \
  --region us-central1 \
  --allow-unauthenticated \
  --memory 256M

# Get URLs
BACKEND_URL=$(gcloud run services describe onboarding-backend --region us-central1 --format 'value(status.url)')
FRONTEND_URL=$(gcloud run services describe onboarding-frontend --region us-central1 --format 'value(status.url)')

echo "Backend: $BACKEND_URL"
echo "Frontend: $FRONTEND_URL"
```

### 3. Setup Database

```bash
# Create Cloud SQL instance
gcloud sql instances create onboarding \
  --database-version POSTGRES_14 \
  --tier db-f1-micro \
  --region us-central1

# Create database
gcloud sql databases create onboarding --instance onboarding

# Create user
gcloud sql users create onboarding \
  --instance onboarding \
  --password=secure_password

# Connect and run migrations
gcloud sql connect onboarding --user=postgres
# At psql prompt: psql -U postgres -d onboarding < migrations/001_initial_schema.sql
```

## Testing the Setup

### 1. API Health Check

```bash
curl http://localhost:3001/health

# Expected response:
# {"success":true,"data":{"status":"healthy"},"timestamp":"2024-01-25T10:00:00Z"}
```

### 2. Create Test Customer

```bash
# Login
curl -X POST http://localhost:3001/api/v1/auth/login \
  -H "Content-Type: application/json" \
  -d '{
    "email": "test@example.com",
    "password": "testpass"
  }'

# Create customer
curl -X POST http://localhost:3001/api/v1/customers \
  -H "Authorization: Bearer <token_from_above>" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "Test Company",
    "email": "test@company.com"
  }'
```

### 3. View Dashboard

```bash
# Open browser
open http://localhost:3000

# Login with credentials
# Email: admin@example.com
# Password: admin123

# Navigate to Dashboard to see onboarding progress
```

## Running Tests

```bash
# Unit tests
npm test

# Integration tests
npm run test:e2e

# Load tests
npm run test:load

# View coverage
npm run test:coverage
```

## Useful Commands

```bash
# View logs
docker-compose logs -f backend
docker-compose logs -f frontend

# Rebuild images
docker-compose build

# Reset database
docker-compose down -v
docker-compose up -d

# Access database
docker-compose exec postgres psql -U postgres -d onboarding

# Run shell in container
docker-compose exec backend bash
docker-compose exec frontend bash

# Monitor performance
docker stats

# View metrics
curl http://localhost:9090/api/v1/query?query=http_requests_total
```

## Troubleshooting

### Port Already in Use

```bash
# Find process using port
lsof -i :3000
lsof -i :3001
lsof -i :5432

# Kill process
kill -9 <pid>

# Or use different ports in docker-compose.yml
```

### Database Connection Error

```bash
# Check PostgreSQL is running
docker-compose ps postgres

# Check logs
docker-compose logs postgres

# Recreate database
docker-compose down -v
docker-compose up -d
```

### Frontend Can't Connect to API

```bash
# Check backend is running
curl http://localhost:3001/health

# Check CORS configuration in backend
# Update CORS_ORIGIN in .env if needed

# Check browser console for errors
```

### Memory Issues

```bash
# Increase Docker memory allocation
# In Docker Desktop: Settings → Resources → Memory

# Reduce replica count in docker-compose.yml
# Or remove some optional services
```

## Next Steps

1. **Read Documentation**
   - [Architecture Guide](./docs/ARCHITECTURE.md)
   - [API Reference](./docs/API.md)
   - [Deployment Guide](./docs/DEPLOYMENT.md)

2. **Explore Features**
   - Setup Wizard: http://localhost:3000/setup
   - Value Definitions: http://localhost:3000/value-definitions
   - Dashboard: http://localhost:3000/dashboard
   - Support: http://localhost:3000/support

3. **Customize**
   - Update environment variables in `.env`
   - Modify configuration in `k8s/configmap.yaml`
   - Customize UI components in `frontend/src/components`

4. **Deploy**
   - Follow [Deployment Guide](./docs/DEPLOYMENT.md)
   - Use provided Terraform modules for infrastructure
   - Set up CI/CD pipeline (GitHub Actions included)

## Support

For issues or questions:

1. Check [Troubleshooting](./docs/DEPLOYMENT.md#troubleshooting) section
2. Review API documentation for endpoint details
3. Check logs: `docker-compose logs -f`
4. Create GitHub issue for bug reports
5. Email support@onboarding.io for urgent issues

---

**Happy Onboarding!** 🚀
