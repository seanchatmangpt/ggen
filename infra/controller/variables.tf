variable "project_id" {
  type        = string
  description = "GCP project ID"
}

variable "region" {
  type        = string
  description = "GCP region for Cloud Run Job + Cloud Scheduler"
  default     = "us-central1"
}

variable "controller_image_uri" {
  type        = string
  description = "Full image URI for the controller (e.g., us-central1-docker.pkg.dev/PROJECT/REPO/catalog-controller:TAG)"
}

variable "tf_state_bucket" {
  type        = string
  description = "GCS bucket name for Terraform state"
}

variable "catalog_repo_url" {
  type        = string
  description = "Git repo URL containing ggen.toml, ontology/, templates/, infra/catalog/"
}

variable "catalog_branch" {
  type        = string
  description = "Branch/tag to check out"
  default     = "main"
}

variable "tf_workdir" {
  type        = string
  description = "Terraform working directory relative to repo root"
  default     = "infra/catalog"
}
