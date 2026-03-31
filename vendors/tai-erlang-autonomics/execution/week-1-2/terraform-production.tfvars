# TAI Erlang Autonomics - Production Terraform Variables
# Week 1-2 Deployment Configuration

project_id            = "tai-autonomics-prod"
region                = "us-central1"
environment           = "production"
image_tag             = "v1.0.0"

# Cloud Run Configuration
container_concurrency = 80
cpu_limit             = "2"
memory_limit          = "2Gi"
min_instances         = 1          # Keep at least 1 instance warm
max_instances         = 10         # Auto-scale up to 10
timeout_seconds       = 300        # 5-minute timeout for long operations
execution_environment = "gen2"      # Latest generation with better performance

# Firestore Configuration
firestore_location    = "us-central"  # Multi-region for high availability
firestore_enabled     = true

# Receipt Ledger Backend
receipt_ledger_backend = "firestore"  # Use Firestore for distributed ledger

# Observability
tracing_enabled       = true
gcp_zone             = "us-central1-a"

# Network Configuration
enable_public_access         = true   # Allow public access (secured by Cloud Armor)
enable_authenticated_access = false   # Disable if only public access needed

# Alert Notification Channels (will be created after terraform apply)
# Update this after creating notification channels in GCP
alert_notification_channels = []  # ["projects/[PROJECT_ID]/notificationChannels/[CHANNEL_ID]"]
