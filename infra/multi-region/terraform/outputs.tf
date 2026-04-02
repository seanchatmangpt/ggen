###############################################################################
# Load Balancer & Networking Outputs
###############################################################################

output "global_load_balancer_ip" {
  value       = google_compute_global_address.marketplace.address
  description = "Global Load Balancer external IP address"
}

output "load_balancer_dns_name" {
  value       = "api.ggen-marketplace.example.com"
  description = "DNS name for global load balancer"
}

output "load_balancer_backend_service" {
  value       = google_compute_backend_service.marketplace.name
  description = "Global backend service name"
}

output "ssl_certificate_subject" {
  value       = google_compute_ssl_certificate.marketplace.certificate
  description = "SSL certificate subject"
  sensitive   = true
}

###############################################################################
# Firestore Outputs
###############################################################################

output "firestore_database_name" {
  value       = google_firestore_database.primary.name
  description = "Firestore database name"
}

output "firestore_database_location" {
  value       = google_firestore_database.primary.location_id
  description = "Firestore database location"
}

output "firestore_multi_region_config" {
  value       = google_firestore_database.primary.multi_region_config
  description = "Firestore multi-region configuration"
}

output "firestore_collections" {
  value       = var.firestore_collections
  description = "Firestore collections being replicated"
}

###############################################################################
# Redis Outputs
###############################################################################

output "redis_primary_instance" {
  value = {
    name = google_redis_instance.cache[var.primary_region].name
    host = google_redis_instance.cache[var.primary_region].host
    port = google_redis_instance.cache[var.primary_region].port
    region = var.primary_region
  }
  description = "Primary Redis instance details"
}

output "redis_secondary_instances" {
  value = {
    for region in var.secondary_regions :
    region => {
      name = google_redis_instance.cache[region].name
      host = google_redis_instance.cache[region].host
      port = google_redis_instance.cache[region].port
    }
  }
  description = "Secondary Redis instance details"
}

output "redis_all_instances" {
  value = {
    for region in concat([var.primary_region], var.secondary_regions) :
    region => {
      name = google_redis_instance.cache[region].name
      host = google_redis_instance.cache[region].host
      port = google_redis_instance.cache[region].port
      region = region
      tier = google_redis_instance.cache[region].tier
      memory_size_gb = google_redis_instance.cache[region].memory_size_gb
    }
  }
  description = "All Redis instances by region"
}

###############################################################################
# Compute Outputs
###############################################################################

output "instance_groups" {
  value = {
    for region in concat([var.primary_region], var.secondary_regions) :
    region => {
      name = google_compute_instance_group_manager.primary[region].name
      instance_group = google_compute_instance_group_manager.primary[region].instance_group
    }
  }
  description = "Instance groups by region"
}

output "instance_template" {
  value       = google_compute_instance_template.marketplace.name
  description = "Instance template name"
}

output "autoscalers" {
  value = {
    for region in concat([var.primary_region], var.secondary_regions) :
    region => {
      name = google_compute_autoscaler.marketplace[region].name
      min_replicas = google_compute_autoscaler.marketplace[region].autoscaling_policy[0].min_replicas
      max_replicas = google_compute_autoscaler.marketplace[region].autoscaling_policy[0].max_replicas
    }
  }
  description = "Autoscaler configuration by region"
}

###############################################################################
# KMS & Security Outputs
###############################################################################

output "kms_key_rings" {
  value = {
    for region in concat([var.primary_region], var.secondary_regions) :
    region => {
      name = google_kms_key_ring.firestore[region].name
      location = google_kms_key_ring.firestore[region].location
    }
  }
  description = "KMS key rings by region"
}

output "kms_encryption_keys" {
  value = {
    for region in concat([var.primary_region], var.secondary_regions) :
    region => {
      name = google_kms_crypto_key.firestore[region].name
      key_ring = google_kms_crypto_key.firestore[region].key_ring
    }
  }
  description = "KMS encryption keys by region"
  sensitive   = true
}

###############################################################################
# Service Account Outputs
###############################################################################

output "api_service_account_email" {
  value       = google_service_account.api.email
  description = "API service account email"
}

output "api_service_account_id" {
  value       = google_service_account.api.name
  description = "API service account resource name"
}

###############################################################################
# Monitoring Outputs
###############################################################################

output "health_check_name" {
  value       = google_compute_health_check.api.name
  description = "Health check resource name"
}

output "health_check_id" {
  value       = google_compute_health_check.api.id
  description = "Health check resource ID"
}

output "alert_policies" {
  value = {
    replication_lag = google_monitoring_alert_policy.replication_lag.display_name
    health_check_failure = google_monitoring_alert_policy.health_check_failure.display_name
  }
  description = "Alert policies configured"
}

output "notification_channel" {
  value       = google_monitoring_notification_channel.slack.display_name
  description = "Primary notification channel"
}

###############################################################################
# Deployment Summary
###############################################################################

output "deployment_summary" {
  value = {
    primary_region = var.primary_region
    secondary_regions = var.secondary_regions
    total_regions = length(concat([var.primary_region], var.secondary_regions))
    load_balancer_ip = google_compute_global_address.marketplace.address
    firestore_database = google_firestore_database.primary.name
    redis_instances = length(google_redis_instance.cache)
    instance_groups = length(google_compute_instance_group_manager.primary)
    kms_locations = length(google_kms_key_ring.firestore)
    replication_enabled = google_firestore_database.primary.multi_region_config != null
    cdn_enabled = var.enable_cdn
    multi_region_writes_enabled = true
  }
  description = "Summary of deployed infrastructure"
}

###############################################################################
# Connection Strings
###############################################################################

output "firestore_connection_string" {
  value       = "projects/${data.google_project.project.project_id}/databases/${google_firestore_database.primary.name}"
  description = "Firestore database connection string"
}

output "redis_connection_strings" {
  value = {
    for region in concat([var.primary_region], var.secondary_regions) :
    region => "redis://${google_redis_instance.cache[region].host}:${google_redis_instance.cache[region].port}"
  }
  description = "Redis connection strings by region"
  sensitive   = true
}

output "api_base_urls" {
  value = {
    global = "https://api.ggen-marketplace.example.com"
    primary = "https://api-${var.primary_region}.ggen-marketplace.example.com"
    secondary = {
      for region in var.secondary_regions :
      region => "https://api-${region}.ggen-marketplace.example.com"
    }
  }
  description = "API base URLs for all regions"
}

###############################################################################
# Next Steps / Instructions
###############################################################################

output "next_steps" {
  value = <<-EOT
Terraform deployment complete!

Next steps:
1. Update DNS records to point to: ${google_compute_global_address.marketplace.address}
   - A record: api.ggen-marketplace.example.com -> ${google_compute_global_address.marketplace.address}

2. Deploy application to Kubernetes clusters:
   for region in us-central1 us-east1 europe-west1; do
     gcloud container clusters get-credentials ggen-marketplace-$region --region=$region
     helm upgrade --install ggen-marketplace ./helm -f helm/values-$region.yaml
   done

3. Verify multi-region deployment:
   - Primary (us-central1): curl https://api-us-central1.ggen-marketplace.example.com/health
   - Secondary (us-east1): curl https://api-us-east1.ggen-marketplace.example.com/health
   - Tertiary (europe-west1): curl https://api-europe-west1.ggen-marketplace.example.com/health

4. Configure Firestore backups:
   gcloud firestore databases backup create --database=default --backup-id=daily-$(date +%Y%m%d)

5. Set up monitoring dashboard:
   - Open Cloud Console > Monitoring > Dashboards > Create Dashboard
   - Add panels for: replication lag, error rates, latency, CPU/memory

6. Configure alerting:
   - Alert thresholds configured in terraform
   - Update Slack webhook URL in variables.tf for notifications

7. Test failover procedures:
   - Run: ./scripts/dr-test.sh --test-type=region-failure
   - Estimated duration: 45 minutes

Documentation:
   - Architecture: ./ARCHITECTURE.md
   - Deployment: ./DEPLOYMENT_GUIDE.md
   - Runbooks: ./RUNBOOK_*.md
   - Cost Analysis: ./COST_ANALYSIS.md
EOT
  description = "Next steps after Terraform deployment"
}
