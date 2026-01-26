output "cloud_run_service_url" {
  description = "URL of the Cloud Run service"
  value       = google_cloud_run_service.tai_autonomics.status[0].url
}

output "cloud_run_service_name" {
  description = "Name of the Cloud Run service"
  value       = google_cloud_run_service.tai_autonomics.name
}

output "service_account_email" {
  description = "Email of the service account"
  value       = google_service_account.tai_autonomics.email
}

output "pubsub_topic_name" {
  description = "Name of the Pub/Sub topic"
  value       = google_pubsub_topic.signals.name
}

output "pubsub_subscription_name" {
  description = "Name of the Pub/Sub subscription"
  value       = google_pubsub_subscription.signals.name
}

output "artifact_registry_repository" {
  description = "Artifact Registry repository URL"
  value       = google_artifact_registry_repository.tai_autonomics.name
}

output "firestore_database_id" {
  description = "Firestore database ID"
  value       = google_firestore_database.tai_autonomics.id
}
