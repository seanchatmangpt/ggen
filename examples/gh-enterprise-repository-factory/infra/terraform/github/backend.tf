terraform {
  backend "s3" {
    bucket         = "acme-corp-terraform-state"
    key            = "org-management/github/terraform.tfstate"
    region         = "us-east-1"
    dynamodb_table = "acme-corp-terraform-locks"
    encrypt        = true
    kms_key_id     = "arn:aws:kms:us-east-1:123456789012:key/1234abcd-12ab-34cd-56ef-1234567890ab"
  }
}
