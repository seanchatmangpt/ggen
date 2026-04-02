# Multi-Region Deployment Guide

**Duration**: 2-3 hours | **Complexity**: High | **Team Size**: 2-3 engineers

## Prerequisites Checklist

- [ ] GCP project created with billing enabled
- [ ] `gcloud` CLI installed and authenticated
- [ ] `terraform` >= 1.0 installed
- [ ] `kubectl` configured for 3 GKE clusters
- [ ] Helm >= 3.0 installed
- [ ] On-call team briefed on deployment
- [ ] Change management approval obtained
- [ ] Backup of existing infrastructure taken
- [ ] Network diagram reviewed by security team
- [ ] 2-3 hour maintenance window scheduled

## Step 1: Initialize GCP Project

### 1.1 Set Project Variables

```bash
export PROJECT_ID="ggen-marketplace-prod"
export PRIMARY_REGION="us-central1"
export SECONDARY_REGION="us-east1"
export TERTIARY_REGION="europe-west1"
export TERRAFORM_BUCKET="gs://${PROJECT_ID}-terraform-state"
export TERRAFORM_PREFIX="multi-region"

# Verify project
gcloud config set project $PROJECT_ID
gcloud auth application-default login
```

### 1.2 Create Terraform State Backend

```bash
# Create GCS bucket for Terraform state
gsutil mb -p $PROJECT_ID -l us-central1 $TERRAFORM_BUCKET

# Enable versioning
gsutil versioning set on $TERRAFORM_BUCKET

# Create subdirectories for each region
for region in $PRIMARY_REGION $SECONDARY_REGION $TERTIARY_REGION; do
  echo "Creating state backend for $region"
done

# Create terraform.tfstate bucket user with minimal permissions
gcloud iam service-accounts create terraform-sa \
  --display-name="Terraform Service Account"

# Grant permissions
gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:terraform-sa@${PROJECT_ID}.iam.gserviceaccount.com" \
  --role="roles/compute.admin"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:terraform-sa@${PROJECT_ID}.iam.gserviceaccount.com" \
  --role="roles/firestore.admin"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:terraform-sa@${PROJECT_ID}.iam.gserviceaccount.com" \
  --role="roles/container.admin"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:terraform-sa@${PROJECT_ID}.iam.gserviceaccount.com" \
  --role="roles/cloudfunctions.admin"
```

### 1.3 Enable Required APIs

```bash
# Enable all necessary GCP APIs
gcloud services enable \
  compute.googleapis.com \
  container.googleapis.com \
  firestore.googleapis.com \
  redis.googleapis.com \
  cloudkms.googleapis.com \
  cloudfunctions.googleapis.com \
  cloudscheduler.googleapis.com \
  monitoring.googleapis.com \
  logging.googleapis.com \
  artifactregistry.googleapis.com \
  containerregistry.googleapis.com \
  run.googleapis.com

echo "✓ All APIs enabled"
```

## Step 2: Deploy Core Networking

### 2.1 Create VPC Networks (Optional: If not using default VPC)

```bash
# Create custom VPC for multi-region setup
gcloud compute networks create ggen-multi-region \
  --subnet-mode=custom \
  --bgp-routing-mode=regional

# Create subnets per region
gcloud compute networks subnets create ggen-us-central1 \
  --network=ggen-multi-region \
  --region=$PRIMARY_REGION \
  --range=10.0.0.0/20

gcloud compute networks subnets create ggen-us-east1 \
  --network=ggen-multi-region \
  --region=$SECONDARY_REGION \
  --range=10.1.0.0/20

gcloud compute networks subnets create ggen-europe-west1 \
  --network=ggen-multi-region \
  --region=$TERTIARY_REGION \
  --range=10.2.0.0/20

echo "✓ VPC and subnets created"
```

### 2.2 Configure Firewall Rules

```bash
# Allow inter-region traffic
gcloud compute firewall-rules create allow-inter-region \
  --network=ggen-multi-region \
  --allow=tcp,udp \
  --source-ranges=10.0.0.0/8 \
  --target-tags=ggen-api

# Allow health checks
gcloud compute firewall-rules create allow-health-checks \
  --network=ggen-multi-region \
  --allow=tcp \
  --source-ranges=35.191.0.0/16,130.211.0.0/22 \
  --target-tags=ggen-api

echo "✓ Firewall rules created"
```

## Step 3: Deploy Firestore Multi-Region

### 3.1 Create Primary Firestore Database

```bash
# Create multi-region Firestore database
gcloud firestore databases create \
  --database=default \
  --location=us-central1 \
  --type=firestore-native \
  --enable-multi-region-writes

# Enable PITR for backup/recovery
gcloud firestore databases update default \
  --enable-point-in-time-recovery \
  --point-in-time-recovery-retention-days=7

# List databases to verify
gcloud firestore databases list

echo "✓ Firestore primary database created"
```

### 3.2 Deploy Firestore Replicas

```bash
# Firestore multi-region replication is automatic
# Verify replica status
gcloud firestore databases get-metadata default \
  --format="table(name,databaseType,locationConfig)"

# Monitor replication lag
for region in $SECONDARY_REGION $TERTIARY_REGION; do
  gcloud firestore databases get-metadata default \
    --location=$region \
    --format="table(name,replicationLag)"
done

echo "✓ Firestore replicas deployed"
```

### 3.3 Create Collections & Indexes

```bash
# Create initial collections (must have >= 1 document)
for collection in marketplace_products marketplace_orders marketplace_users; do
  gcloud firestore databases documents create \
    --database=default \
    --collection=$collection \
    --document=_init \
    --data='{"type":"init","createdAt":"NOW()"}'
done

# Create composite indexes for queries
gcloud firestore indexes composite create \
  --database=default \
  --collection=marketplace_orders \
  --field-config="customerId=Ascending,createdAt=Descending" \
  --async

echo "✓ Collections and indexes created"
```

## Step 4: Deploy Cloud Load Balancer

### 4.1 Create Health Checks

```bash
# Create health check for each region
for region in $PRIMARY_REGION $SECONDARY_REGION $TERTIARY_REGION; do
  gcloud compute health-checks create https \
    ggen-marketplace-healthcheck-$region \
    --port=443 \
    --request-path=/health \
    --check-interval=30s \
    --timeout=5s \
    --healthy-threshold=2 \
    --unhealthy-threshold=3
done

echo "✓ Health checks created"
```

### 4.2 Create Backend Services

```bash
# Create global backend service
gcloud compute backend-services create ggen-marketplace-backend \
  --global \
  --protocol=HTTPS \
  --health-checks=ggen-marketplace-healthcheck-$PRIMARY_REGION \
  --enable-cdn \
  --cache-mode=CACHE_ALL_STATIC \
  --default-ttl=3600 \
  --max-ttl=86400 \
  --client-ttl=3600 \
  --negative-caching \
  --negative-caching-policy=404=120,410=120

echo "✓ Backend services created"
```

### 4.3 Add Backends per Region

```bash
# Create instance groups per region
for region in $PRIMARY_REGION $SECONDARY_REGION $TERTIARY_REGION; do
  gcloud compute instance-groups managed create \
    ggen-marketplace-ig-$region \
    --base-instance-name=ggen-api \
    --region=$region \
    --template=ggen-api-template \
    --size=3 \
    --autoscaling-mode=on \
    --min-num-replicas=2 \
    --max-num-replicas=10 \
    --target-cpu-utilization=0.7
done

# Add backends to backend service
for region in $PRIMARY_REGION $SECONDARY_REGION $TERTIARY_REGION; do
  gcloud compute backend-services add-backend ggen-marketplace-backend \
    --global \
    --instance-group=ggen-marketplace-ig-$region \
    --instance-group-region=$region \
    --balancing-mode=RATE \
    --max-rate-per-instance=1000
done

echo "✓ Backends added to load balancer"
```

### 4.4 Create URL Maps & Forwarding Rules

```bash
# Create URL map
gcloud compute url-maps create ggen-marketplace-url-map \
  --default-service=ggen-marketplace-backend

# Create SSL certificate
gcloud compute ssl-certificates create ggen-marketplace-cert \
  --certificate=/path/to/cert.pem \
  --private-key=/path/to/key.pem \
  --global

# Create target HTTPS proxy
gcloud compute target-https-proxies create ggen-marketplace-proxy \
  --url-map=ggen-marketplace-url-map \
  --ssl-certificates=ggen-marketplace-cert

# Create global forwarding rule
gcloud compute forwarding-rules create ggen-marketplace-https \
  --global \
  --target-https-proxy=ggen-marketplace-proxy \
  --address=ggen-marketplace-ip \
  --ports=443

echo "✓ URL map and forwarding rules created"
```

## Step 5: Deploy Redis Sentinel Clusters

### 5.1 Create Redis Instances per Region

```bash
# Create Redis instance in each region
for region in $PRIMARY_REGION $SECONDARY_REGION $TERTIARY_REGION; do
  gcloud redis instances create \
    ggen-redis-$region \
    --region=$region \
    --tier=standard \
    --size=5 \
    --redis-version=7.2 \
    --enable-auth \
    --auth-string="$(openssl rand -base64 32)"
done

# Wait for instances to be ready
for region in $PRIMARY_REGION $SECONDARY_REGION $TERTIARY_REGION; do
  gcloud redis operations wait \
    --region=$region ggen-redis-$region-create-operation
done

echo "✓ Redis instances created"
```

### 5.2 Configure Redis Replication

```bash
# Get primary Redis endpoint
PRIMARY_REDIS_IP=$(gcloud redis instances describe ggen-redis-$PRIMARY_REGION \
  --region=$PRIMARY_REGION \
  --format="value(host)")

# Configure replicas to sync from primary
redis-cli -h ${SECONDARY_REDIS_IP} replicaof $PRIMARY_REDIS_IP 6379
redis-cli -h ${TERTIARY_REDIS_IP} replicaof $PRIMARY_REDIS_IP 6379

echo "✓ Redis replication configured"
```

## Step 6: Deploy Kubernetes Clusters & Services

### 6.1 Create GKE Clusters per Region

```bash
# Create cluster in each region
for region in $PRIMARY_REGION $SECONDARY_REGION $TERTIARY_REGION; do
  gcloud container clusters create ggen-marketplace-$region \
    --region=$region \
    --num-nodes=3 \
    --machine-type=n1-standard-2 \
    --enable-autoscaling \
    --min-nodes=2 \
    --max-nodes=10 \
    --enable-autorepair \
    --enable-autoupgrade \
    --enable-vertical-pod-autoscaling \
    --network=ggen-multi-region \
    --subnetwork=ggen-$region \
    --enable-stackdriver-kubernetes \
    --addons=HorizontalPodAutoscaling,HttpLoadBalancing,GcePersistentDiskCsiDriver
done

echo "✓ GKE clusters created"
```

### 6.2 Get Cluster Credentials

```bash
# Get credentials for each cluster
for region in $PRIMARY_REGION $SECONDARY_REGION $TERTIARY_REGION; do
  gcloud container clusters get-credentials ggen-marketplace-$region \
    --region=$region \
    --project=$PROJECT_ID
done

echo "✓ Cluster credentials configured"
```

### 6.3 Deploy Services to Each Cluster

```bash
# Deploy to each cluster
for region in $PRIMARY_REGION $SECONDARY_REGION $TERTIARY_REGION; do
  kubectl --context=gke_${PROJECT_ID}_${region}_ggen-marketplace-${region} \
    create namespace ggen

  # Deploy Helm chart per region
  helm upgrade --install ggen-marketplace ./helm \
    --namespace=ggen \
    --values=./helm/values-${region}.yaml \
    --context=gke_${PROJECT_ID}_${region}_ggen-marketplace-${region}
done

echo "✓ Services deployed to all clusters"
```

## Step 7: Configure Cloud KMS Keys

### 7.1 Create Key Rings & Keys

```bash
# Create key ring per region
for region in $PRIMARY_REGION $SECONDARY_REGION $TERTIARY_REGION; do
  gcloud kms keyrings create ggen-keyring-$region \
    --location=$region

  # Create keys for different purposes
  gcloud kms keys create ggen-encryption-key \
    --location=$region \
    --keyring=ggen-keyring-$region \
    --purpose=encryption

  gcloud kms keys create ggen-signing-key \
    --location=$region \
    --keyring=ggen-keyring-$region \
    --purpose=asymmetric-signing
done

echo "✓ KMS keys created"
```

### 7.2 Configure Key Access

```bash
# Grant service accounts access to keys
for region in $PRIMARY_REGION $SECONDARY_REGION $TERTIARY_REGION; do
  gcloud kms keys add-iam-policy-binding ggen-encryption-key \
    --location=$region \
    --keyring=ggen-keyring-$region \
    --member="serviceAccount:ggen-api-sa@${PROJECT_ID}.iam.gserviceaccount.com" \
    --role="roles/cloudkms.cryptoKeyEncrypterDecrypter"
done

echo "✓ KMS access configured"
```

## Step 8: Deploy with Terraform

### 8.1 Initialize Terraform

```bash
cd infra/multi-region/terraform

# Initialize Terraform with remote backend
terraform init \
  -backend-config="bucket=$TERRAFORM_BUCKET" \
  -backend-config="prefix=$TERRAFORM_PREFIX"

# Verify initialization
terraform version
```

### 8.2 Plan Deployment

```bash
# Create terraform.tfvars file
cat > terraform.tfvars << EOF
project_id     = "$PROJECT_ID"
primary_region = "$PRIMARY_REGION"
secondary_regions = ["$SECONDARY_REGION", "$TERTIARY_REGION"]

# Firestore config
firestore_retention_days = 7
enable_pitr = true

# Redis config
redis_tier = "standard"
redis_size = 5

# Load balancer
enable_cdn = true
cdn_ttl = 3600

# KMS
kms_locations = ["$PRIMARY_REGION", "$SECONDARY_REGION", "$TERTIARY_REGION"]
EOF

# Plan deployment
terraform plan -out=tfplan

# Review plan
terraform show tfplan
```

### 8.3 Apply Configuration

```bash
# Apply configuration (30-40 minutes)
terraform apply tfplan

# Capture outputs
terraform output -json > deployment_outputs.json

echo "✓ Infrastructure deployed via Terraform"
```

## Step 9: Post-Deployment Verification

### 9.1 Test Health Checks

```bash
# Test primary region endpoint
curl -v https://api-${PRIMARY_REGION}.ggen-marketplace.example.com/health

# Test secondary regions
for region in $SECONDARY_REGION $TERTIARY_REGION; do
  curl -v https://api-${region}.ggen-marketplace.example.com/health
done
```

### 9.2 Verify Data Replication

```bash
# Create test document in primary
TEST_DOC=$(gcloud firestore databases documents create \
  --database=default \
  --collection=test_replication \
  --document="" \
  --data='{"test":true,"timestamp":"NOW()"}' \
  --format="value(name)")

sleep 5

# Verify replication to other regions
for region in $SECONDARY_REGION $TERTIARY_REGION; do
  DOC_EXISTS=$(gcloud firestore databases documents get \
    --database=default \
    --document-path="$TEST_DOC" \
    --location=$region \
    2>/dev/null | jq '.name' || echo "NOT_FOUND")

  echo "$region: Document replicated = $DOC_EXISTS"
done

# Clean up
gcloud firestore databases documents delete "$TEST_DOC" --database=default
```

### 9.3 Run Full Integration Test

```bash
# Run integration test suite
kubectl --context=gke_${PROJECT_ID}_${PRIMARY_REGION}_ggen-marketplace-${PRIMARY_REGION} \
  run integration-tests \
  --image=ggen-marketplace-tests:latest \
  --rm=true \
  -it \
  --restart=Never \
  --env="FIRESTORE_DATABASE=default" \
  --env="REDIS_HOST=$(gcloud redis instances describe ggen-redis-$PRIMARY_REGION \
    --region=$PRIMARY_REGION \
    --format='value(host)')"
```

## Step 10: Monitor & Alert Setup

### 10.1 Create Monitoring Dashboard

```bash
# Create custom dashboard with key metrics
gcloud monitoring dashboards create \
  --config-from-file=monitoring/dashboard-config.json
```

### 10.2 Configure Alerting Policies

```bash
# Create alert for replication lag
gcloud monitoring alert-policies create \
  --notification-channels=$NOTIFICATION_CHANNEL_ID \
  --display-name="Replication Lag Alert" \
  --condition-display-name="Lag > 10s" \
  --condition-threshold-value="10000" \
  --condition-threshold-duration="60s" \
  --condition-metric="custom.googleapis.com/firestore/replication_lag"

echo "✓ Monitoring and alerting configured"
```

## Post-Deployment Checklist

- [ ] All 3 regions deployed and healthy
- [ ] Health checks passing in all regions
- [ ] Firestore replication lag < 5 seconds
- [ ] Redis replication working
- [ ] Load balancer routing traffic correctly
- [ ] API endpoints responding (all regions)
- [ ] Database writes persisting and replicating
- [ ] Monitoring dashboard created
- [ ] Alert policies active
- [ ] Backups scheduled and running
- [ ] Team trained on runbooks
- [ ] Documentation updated
- [ ] Incident response plan communicated
- [ ] Status page updated

## Rollback Plan (If Needed)

If deployment fails:

```bash
# Destroy all resources (WARNING: Data loss!)
cd infra/multi-region/terraform
terraform destroy -auto-approve

# Restore from backup
gcloud firestore databases restore \
  --database=default \
  --backup=projects/$PROJECT_ID/locations/us/backups/BACKUP_ID

# Verify restoration
gcloud firestore databases get-metadata default
```

---

**Last Updated**: 2026-01-25
**Estimated Deployment Time**: 2-3 hours
**Post-Deployment Support**: 24/7 on-call rotation
