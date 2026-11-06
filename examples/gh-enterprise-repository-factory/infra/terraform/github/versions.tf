terraform {
  required_providers {
    github = {
      source  = "integrations/github"
      version = "5.41.0"
    }
  }
}

provider "github" {
  owner = var.github_owner
}

