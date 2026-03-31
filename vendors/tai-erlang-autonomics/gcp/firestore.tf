# Firestore Database Configuration
# Purpose: Persistent receipt ledger and operational state storage
# Mode: Native (recommended) with optimistic concurrency control

# Enable Firestore API
resource "google_app_engine_application" "firestore_app" {
  count             = var.firestore_enabled ? 1 : 0
  project           = var.project_id
  location_id       = var.firestore_location
  database_type     = "CLOUD_FIRESTORE"
  deletion_policy   = "DELETE"

  depends_on = [
    google_project_service.required_apis["firestore.googleapis.com"],
    google_project_service.required_apis["appengine.googleapis.com"]
  ]
}

# Firestore Database (Native Mode)
resource "google_firestore_database" "receipts" {
  project                 = var.project_id
  name                    = "taiea-receipts"
  location_id             = var.firestore_location
  type                    = "FIRESTORE_NATIVE"
  concurrency_mode        = "OPTIMISTIC"
  app_engine_integration_mode = "DISABLED"
  delete_protection_state = "DELETE_PROTECTION_DISABLED"
  point_in_time_recovery_enablement = var.enable_pitr ? "ENABLED" : "DISABLED"

  labels = {
    environment = var.environment
    service     = "taiea"
    managed-by  = "terraform"
  }

  depends_on = [
    google_project_service.required_apis["firestore.googleapis.com"]
  ]
}

# Firestore Database (Operations/Metadata)
resource "google_firestore_database" "operations" {
  project                 = var.project_id
  name                    = "taiea-operations"
  location_id             = var.firestore_location
  type                    = "FIRESTORE_NATIVE"
  concurrency_mode        = "OPTIMISTIC"
  app_engine_integration_mode = "DISABLED"
  delete_protection_state = "DELETE_PROTECTION_DISABLED"

  labels = {
    environment = var.environment
    service     = "taiea"
    database    = "operations"
    managed-by  = "terraform"
  }

  depends_on = [
    google_project_service.required_apis["firestore.googleapis.com"]
  ]
}

# Firestore Index for Receipt Lookup by Status
resource "google_firestore_index" "receipt_status_index" {
  collection = "receipts"
  database   = google_firestore_database.receipts.name
  query_scope = "COLLECTION"

  fields {
    field_path = "status"
    order      = "ASCENDING"
  }

  fields {
    field_path = "created_at"
    order      = "DESCENDING"
  }

  depends_on = [
    google_firestore_database.receipts
  ]
}

# Firestore Index for Receipt Lookup by Timestamp
resource "google_firestore_index" "receipt_timestamp_index" {
  collection = "receipts"
  database   = google_firestore_database.receipts.name
  query_scope = "COLLECTION"

  fields {
    field_path = "created_at"
    order      = "DESCENDING"
  }

  fields {
    field_path = "__name__"
    order      = "DESCENDING"
  }

  depends_on = [
    google_firestore_database.receipts
  ]
}

# Firestore Index for Receipt Lookup by User
resource "google_firestore_index" "receipt_user_index" {
  collection = "receipts"
  database   = google_firestore_database.receipts.name
  query_scope = "COLLECTION"

  fields {
    field_path = "user_id"
    order      = "ASCENDING"
  }

  fields {
    field_path = "created_at"
    order      = "DESCENDING"
  }

  depends_on = [
    google_firestore_database.receipts
  ]
}

# Firestore Index for Operations Metadata
resource "google_firestore_index" "operations_timestamp_index" {
  collection = "operations"
  database   = google_firestore_database.operations.name
  query_scope = "COLLECTION"

  fields {
    field_path = "timestamp"
    order      = "DESCENDING"
  }

  fields {
    field_path = "operation_type"
    order      = "ASCENDING"
  }

  depends_on = [
    google_firestore_database.operations
  ]
}

# Firestore Backup Policy (weekly automated backups)
resource "google_firestore_backup_schedule" "weekly_receipts" {
  count       = var.enable_backups ? 1 : 0
  parent      = "projects/${var.project_id}/databases/${google_firestore_database.receipts.name}"
  retention   = "604800s" # 7 days
  display_name = "Weekly Receipts Backup"

  weekly_recurrence {
    day = "MONDAY"
  }

  depends_on = [
    google_firestore_database.receipts
  ]
}

# Firestore Backup Policy (weekly automated backups for operations)
resource "google_firestore_backup_schedule" "weekly_operations" {
  count       = var.enable_backups ? 1 : 0
  parent      = "projects/${var.project_id}/databases/${google_firestore_database.operations.name}"
  retention   = "2592000s" # 30 days
  display_name = "Weekly Operations Backup"

  weekly_recurrence {
    day = "MONDAY"
  }

  depends_on = [
    google_firestore_database.operations
  ]
}

# Outputs
output "firestore_receipts_database_id" {
  description = "Firestore receipts database ID"
  value       = google_firestore_database.receipts.uid
}

output "firestore_receipts_database_name" {
  description = "Firestore receipts database name"
  value       = google_firestore_database.receipts.name
}

output "firestore_operations_database_id" {
  description = "Firestore operations database ID"
  value       = google_firestore_database.operations.uid
}

output "firestore_operations_database_name" {
  description = "Firestore operations database name"
  value       = google_firestore_database.operations.name
}
