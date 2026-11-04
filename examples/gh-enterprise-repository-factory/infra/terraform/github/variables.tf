variable "github_owner" {
  description = "GitHub org/user that owns the managed repository."
  type        = string
  default     = "seanchatmangpt"
}

variable "repository" {
  description = "Repository name to manage."
  type        = string
  default     = "ggen"
}

variable "ggen_signing_key_value" {
  description = "Value for the GGEN_SIGNING_KEY actions secret. Supplied at apply time only; never stored in RDF or state committed to the repo."
  type        = string
  sensitive   = true
  default     = ""
}
