# IAM Configuration
# Purpose: Service account creation, IAM bindings, and access control

# Service Account for Cloud Run (TAIEA)
resource "google_service_account" "taiea" {
  account_id   = "taiea-sa"
  display_name = "TAI Autonomics Engine Service Account"
  description  = "Service account for TAIEA Cloud Run service with Firestore access"
  project      = var.project_id
}

# Service Account Key (for local development if needed)
resource "google_service_account_key" "taiea_key" {
  count              = var.create_service_account_key ? 1 : 0
  service_account_id = google_service_account.taiea.name
  public_key_type    = "TYPE_X509_PEM_FILE"

  depends_on = [
    google_service_account.taiea
  ]
}

# Save the service account key to local file (for export)
resource "local_file" "taiea_key_file" {
  count    = var.create_service_account_key ? 1 : 0
  filename = "${path.root}/taiea-service-account-key.json"
  content = base64decode(
    google_service_account_key.taiea_key[0].private_key
  )
  file_permission = "0600"

  depends_on = [
    google_service_account_key.taiea_key
  ]
}

# ==============================
# Firestore IAM Bindings
# ==============================

# Allow Cloud Run service account to read/write to Firestore
resource "google_project_iam_member" "taiea_firestore_user" {
  project = var.project_id
  role    = "roles/datastore.user"
  member  = "serviceAccount:${google_service_account.taiea.email}"

  depends_on = [
    google_service_account.taiea
  ]
}

# Allow Cloud Run service account to import/export Firestore data
resource "google_project_iam_member" "taiea_firestore_import_export" {
  project = var.project_id
  role    = "roles/datastore.importExportAdmin"
  member  = "serviceAccount:${google_service_account.taiea.email}"

  depends_on = [
    google_service_account.taiea
  ]
}

# ==============================
# Logging & Monitoring IAM Bindings
# ==============================

# Allow Cloud Run service account to write logs
resource "google_project_iam_member" "taiea_logging_log_writer" {
  project = var.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.taiea.email}"

  depends_on = [
    google_service_account.taiea
  ]
}

# Allow Cloud Run service account to write metrics
resource "google_project_iam_member" "taiea_monitoring_metric_writer" {
  project = var.project_id
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.taiea.email}"

  depends_on = [
    google_service_account.taiea
  ]
}

# ==============================
# Cloud Trace IAM Bindings
# ==============================

# Allow Cloud Run service account to write trace data
resource "google_project_iam_member" "taiea_cloud_trace_agent" {
  project = var.project_id
  role    = "roles/cloudtrace.agent"
  member  = "serviceAccount:${google_service_account.taiea.email}"

  depends_on = [
    google_service_account.taiea
  ]
}

# ==============================
# Pub/Sub IAM Bindings (if enabled)
# ==============================

# Allow Cloud Run service account to subscribe to Pub/Sub
resource "google_project_iam_member" "taiea_pubsub_subscriber" {
  count   = var.enable_pubsub ? 1 : 0
  project = var.project_id
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.taiea.email}"

  depends_on = [
    google_service_account.taiea
  ]
}

# Allow Cloud Run service account to publish to Pub/Sub
resource "google_project_iam_member" "taiea_pubsub_publisher" {
  count   = var.enable_pubsub ? 1 : 0
  project = var.project_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.taiea.email}"

  depends_on = [
    google_service_account.taiea
  ]
}

# ==============================
# Cloud Storage IAM Bindings (if backups to GCS)
# ==============================

# Allow Cloud Run service account to read/write to Cloud Storage
resource "google_project_iam_member" "taiea_storage_object_admin" {
  count   = var.enable_gcs_backups ? 1 : 0
  project = var.project_id
  role    = "roles/storage.objectAdmin"
  member  = "serviceAccount:${google_service_account.taiea.email}"

  depends_on = [
    google_service_account.taiea
  ]
}

# ==============================
# Secret Manager IAM Bindings (if using secrets)
# ==============================

# Allow Cloud Run service account to read secrets
resource "google_project_iam_member" "taiea_secret_accessor" {
  count   = var.enable_secrets ? 1 : 0
  project = var.project_id
  role    = "roles/secretmanager.secretAccessor"
  member  = "serviceAccount:${google_service_account.taiea.email}"

  depends_on = [
    google_service_account.taiea
  ]
}

# ==============================
# Cloud Run Invoker IAM Bindings
# ==============================

# Allow public access to Cloud Run (if enabled)
resource "google_cloud_run_service_iam_member" "taiea_public_invoker" {
  count    = var.enable_public_access ? 1 : 0
  service  = google_cloud_run_service.taiea.name
  location = google_cloud_run_service.taiea.location
  role     = "roles/run.invoker"
  member   = "allUsers"

  depends_on = [
    google_cloud_run_service.taiea
  ]
}

# Allow authenticated users to invoke Cloud Run
resource "google_cloud_run_service_iam_member" "taiea_authenticated_invoker" {
  count    = var.enable_authenticated_access ? 1 : 0
  service  = google_cloud_run_service.taiea.name
  location = google_cloud_run_service.taiea.location
  role     = "roles/run.invoker"
  member   = "allAuthenticatedUsers"

  depends_on = [
    google_cloud_run_service.taiea
  ]
}

# Allow specific service accounts to invoke Cloud Run (for inter-service communication)
resource "google_cloud_run_service_iam_member" "taiea_service_invoker" {
  for_each = toset(var.allowed_invoker_service_accounts)

  service  = google_cloud_run_service.taiea.name
  location = google_cloud_run_service.taiea.location
  role     = "roles/run.invoker"
  member   = "serviceAccount:${each.value}"

  depends_on = [
    google_cloud_run_service.taiea
  ]
}

# ==============================
# Outputs
# ==============================

output "taiea_service_account_email" {
  description = "Email address of the TAIEA service account"
  value       = google_service_account.taiea.email
}

output "taiea_service_account_id" {
  description = "ID of the TAIEA service account"
  value       = google_service_account.taiea.unique_id
}

output "taiea_service_account_key_path" {
  description = "Path to service account key file (if created)"
  value       = var.create_service_account_key ? local_file.taiea_key_file[0].filename : null
  sensitive   = true
}
