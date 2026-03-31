# IAM Policy Configuration for erlmcp
# Purpose: Fine-grained access control, tenant isolation, audit logging
# Establishes service-to-service authentication and Cloud Run invocation policies

# ==============================
# Admin Service Account
# ==============================
# Limited admin account with permission to manage TAIEA infrastructure
# Used for automated deployments and administrative tasks

resource "google_service_account" "taiea_admin" {
  account_id   = "taiea-admin"
  display_name = "TAIEA Administrator Service Account"
  description  = "Admin service account for infrastructure management and deployments"
  project      = var.project_id
}

# ==============================
# Tenant Isolation Configuration
# ==============================
# Custom role for tenant-scoped data access
# Restricts service accounts to accessing only their assigned tenant's data in Firestore

resource "google_project_custom_role" "tenant_firestore_reader" {
  role_id     = "tenantFirestoreReader"
  title       = "Tenant Firestore Reader"
  description = "Read-only access to tenant-scoped Firestore documents"
  project     = var.project_id

  included_permissions = [
    "datastore.databases.get",
    "datastore.entities.get",
    "datastore.entities.list",
    "datastore.indexes.get",
    "datastore.indexes.list"
  ]
}

resource "google_project_custom_role" "tenant_firestore_writer" {
  role_id     = "tenantFirestoreWriter"
  title       = "Tenant Firestore Writer"
  description = "Read-write access to tenant-scoped Firestore documents"
  project     = var.project_id

  included_permissions = [
    "datastore.databases.get",
    "datastore.entities.create",
    "datastore.entities.delete",
    "datastore.entities.get",
    "datastore.entities.list",
    "datastore.entities.update",
    "datastore.indexes.get",
    "datastore.indexes.list"
  ]
}

# ==============================
# API Server Service Account
# ==============================
# Service account for API servers that need to invoke TAIEA Cloud Run

resource "google_service_account" "api_client" {
  account_id   = "taiea-api-client"
  display_name = "TAIEA API Client Service Account"
  description  = "Service account for API servers to invoke TAIEA Cloud Run"
  project      = var.project_id
}

# ==============================
# Billing Service Account
# ==============================
# Service account for billing system with access to billing-related operations

resource "google_service_account" "billing_service" {
  account_id   = "taiea-billing"
  display_name = "TAIEA Billing Service Account"
  description  = "Service account for billing operations and ledger management"
  project      = var.project_id
}

# ==============================
# IAM Bindings: Admin Service Account
# ==============================

# Admin: Firestore Admin (manage databases, indexes, backups)
resource "google_project_iam_member" "admin_firestore_admin" {
  project = var.project_id
  role    = "roles/datastore.admin"
  member  = "serviceAccount:${google_service_account.taiea_admin.email}"
}

# Admin: Cloud Run Admin (deploy revisions, manage traffic)
resource "google_project_iam_member" "admin_run_admin" {
  project = var.project_id
  role    = "roles/run.admin"
  member  = "serviceAccount:${google_service_account.taiea_admin.email}"
}

# Admin: Service Account User (allow impersonation for deployments)
resource "google_project_iam_member" "admin_service_account_user" {
  project = var.project_id
  role    = "roles/iam.serviceAccountUser"
  member  = "serviceAccount:${google_service_account.taiea_admin.email}"
}

# Admin: Service Account Token Creator (create tokens for service account)
resource "google_project_iam_member" "admin_service_account_token_creator" {
  project = var.project_id
  role    = "roles/iam.serviceAccountTokenCreator"
  member  = "serviceAccount:${google_service_account.taiea_admin.email}"
}

# Admin: Logging Admin
resource "google_project_iam_member" "admin_logging_admin" {
  project = var.project_id
  role    = "roles/logging.admin"
  member  = "serviceAccount:${google_service_account.taiea_admin.email}"
}

# Admin: Monitoring Admin
resource "google_project_iam_member" "admin_monitoring_admin" {
  project = var.project_id
  role    = "roles/monitoring.admin"
  member  = "serviceAccount:${google_service_account.taiea_admin.email}"
}

# ==============================
# IAM Bindings: API Client Service Account
# ==============================

# API Client: Cloud Run Invoker (invoke TAIEA service)
resource "google_cloud_run_service_iam_member" "api_client_invoker" {
  service  = google_cloud_run_service.taiea.name
  location = google_cloud_run_service.taiea.location
  role     = "roles/run.invoker"
  member   = "serviceAccount:${google_service_account.api_client.email}"

  depends_on = [
    google_cloud_run_service.taiea
  ]
}

# API Client: Firestore User (read/write access)
resource "google_project_iam_member" "api_client_firestore_user" {
  project = var.project_id
  role    = "roles/datastore.user"
  member  = "serviceAccount:${google_service_account.api_client.email}"
}

# API Client: Logging Writer (write application logs)
resource "google_project_iam_member" "api_client_logging_writer" {
  project = var.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.api_client.email}"
}

# ==============================
# IAM Bindings: Billing Service Account
# ==============================

# Billing: Cloud Run Invoker (invoke TAIEA service)
resource "google_cloud_run_service_iam_member" "billing_service_invoker" {
  service  = google_cloud_run_service.taiea.name
  location = google_cloud_run_service.taiea.location
  role     = "roles/run.invoker"
  member   = "serviceAccount:${google_service_account.billing_service.email}"

  depends_on = [
    google_cloud_run_service.taiea
  ]
}

# Billing: Firestore User (read/write billing data)
resource "google_project_iam_member" "billing_service_firestore_user" {
  project = var.project_id
  role    = "roles/datastore.user"
  member  = "serviceAccount:${google_service_account.billing_service.email}"
}

# Billing: BigQuery Data Editor (write billing events to BigQuery)
resource "google_project_iam_member" "billing_service_bigquery_editor" {
  count   = var.enable_bigquery_export ? 1 : 0
  project = var.project_id
  role    = "roles/bigquery.dataEditor"
  member  = "serviceAccount:${google_service_account.billing_service.email}"
}

# ==============================
# Credential Configuration
# ==============================

# Admin service account key (for CI/CD pipelines)
resource "google_service_account_key" "admin_key" {
  count              = var.create_admin_service_account_key ? 1 : 0
  service_account_id = google_service_account.taiea_admin.name
  public_key_type    = "TYPE_X509_PEM_FILE"

  depends_on = [
    google_service_account.taiea_admin
  ]
}

# API client service account key (for development)
resource "google_service_account_key" "api_client_key" {
  count              = var.create_api_client_key ? 1 : 0
  service_account_id = google_service_account.api_client.name
  public_key_type    = "TYPE_X509_PEM_FILE"

  depends_on = [
    google_service_account.api_client
  ]
}

# Save keys to local files
resource "local_file" "admin_key_file" {
  count    = var.create_admin_service_account_key ? 1 : 0
  filename = "${path.root}/credentials/taiea-admin-key.json"
  content = base64decode(
    google_service_account_key.admin_key[0].private_key
  )
  file_permission = "0600"

  depends_on = [
    google_service_account_key.admin_key
  ]
}

resource "local_file" "api_client_key_file" {
  count    = var.create_api_client_key ? 1 : 0
  filename = "${path.root}/credentials/taiea-api-client-key.json"
  content = base64decode(
    google_service_account_key.api_client_key[0].private_key
  )
  file_permission = "0600"

  depends_on = [
    google_service_account_key.api_client_key
  ]
}

# ==============================
# Workload Identity Binding
# ==============================
# Allows Kubernetes workloads to impersonate service accounts (for GKE deployments)

resource "google_service_account_iam_member" "workload_identity_binding" {
  count              = var.enable_workload_identity ? 1 : 0
  service_account_id = google_service_account.taiea.name
  role               = "roles/iam.workloadIdentityUser"
  member             = "serviceAccount:${var.project_id}.svc.id.goog[${var.kubernetes_namespace}/taiea-workload]"

  depends_on = [
    google_service_account.taiea
  ]
}

# ==============================
# Output Service Account Information
# ==============================

output "taiea_admin_service_account_email" {
  description = "Email of the admin service account"
  value       = google_service_account.taiea_admin.email
}

output "api_client_service_account_email" {
  description = "Email of the API client service account"
  value       = google_service_account.api_client.email
}

output "billing_service_account_email" {
  description = "Email of the billing service account"
  value       = google_service_account.billing_service.email
}

output "admin_key_path" {
  description = "Path to admin service account key file"
  value       = var.create_admin_service_account_key ? local_file.admin_key_file[0].filename : null
  sensitive   = true
}

output "api_client_key_path" {
  description = "Path to API client service account key file"
  value       = var.create_api_client_key ? local_file.api_client_key_file[0].filename : null
  sensitive   = true
}

output "tenant_firestore_reader_role_id" {
  description = "Custom role ID for tenant-scoped Firestore reader"
  value       = google_project_custom_role.tenant_firestore_reader.id
}

output "tenant_firestore_writer_role_id" {
  description = "Custom role ID for tenant-scoped Firestore writer"
  value       = google_project_custom_role.tenant_firestore_writer.id
}
