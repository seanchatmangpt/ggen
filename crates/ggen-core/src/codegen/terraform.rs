//! Terraform IaC code generation from OntologySchema
//!
//! This module generates Terraform configurations for AWS infrastructure setup,
//! including VPC, security groups, RDS databases, and service endpoints.

use std::fmt::Write;

/// Terraform Infrastructure-as-Code generator
pub struct TerraformGenerator;

impl TerraformGenerator {
    /// Generate Terraform provider block with AWS configuration
    ///
    /// Returns:
    /// - terraform { required_providers { aws = { source = "hashicorp/aws", version = "~> 5.0" } } }
    /// - provider "aws" { region = var.aws_region }
    pub fn generate_provider_block() -> Result<String, String> {
        let mut output = String::new();

        writeln!(
            output,
            "/**\n * Auto-generated Terraform configuration\n * Provider: AWS\n */\n"
        )
        .map_err(|e| e.to_string())?;

        writeln!(
            output,
            "terraform {{\n  required_providers {{\n    aws = {{\n      source  = \"hashicorp/aws\"\n      version = \"~> 5.0\"\n    }}\n  }}\n}}\n"
        )
        .map_err(|e| e.to_string())?;

        writeln!(
            output,
            "provider \"aws\" {{\n  region = var.aws_region\n}}\n"
        )
        .map_err(|e| e.to_string())?;

        Ok(output)
    }

    /// Generate VPC and security group resources with configurable ingress ports
    ///
    /// # Arguments
    /// * `ports` - Slice of port numbers to open in security group ingress rules
    ///
    /// Returns Terraform configuration with:
    /// - aws_vpc with cidr_block = "10.0.0.0/16"
    /// - aws_security_group with ingress rules for each port
    pub fn generate_vpc_and_security_group(ports: &[u16]) -> Result<String, String> {
        let mut output = String::new();

        // VPC resource
        writeln!(
            output,
            "resource \"aws_vpc\" \"main\" {{\n  cidr_block = \"10.0.0.0/16\"\n\n  tags = {{\n    Name = \"main-vpc\"\n  }}\n}}\n"
        )
        .map_err(|e| e.to_string())?;

        // Security Group resource
        writeln!(output, "resource \"aws_security_group\" \"main\" {{").map_err(|e| e.to_string())?;
        writeln!(output, "  name = \"main-sg\"").map_err(|e| e.to_string())?;
        writeln!(output, "  description = \"Main security group\"")
            .map_err(|e| e.to_string())?;
        writeln!(output, "  vpc_id = aws_vpc.main.id\n").map_err(|e| e.to_string())?;

        // Generate ingress rules for each port
        for port in ports {
            writeln!(output, "  ingress {{").map_err(|e| e.to_string())?;
            writeln!(output, "    from_port   = {}", port).map_err(|e| e.to_string())?;
            writeln!(output, "    to_port     = {}", port).map_err(|e| e.to_string())?;
            writeln!(output, "    protocol    = \"tcp\"").map_err(|e| e.to_string())?;
            writeln!(output, "    cidr_blocks = [\"0.0.0.0/0\"]")
                .map_err(|e| e.to_string())?;
            writeln!(output, "  }}\n").map_err(|e| e.to_string())?;
        }

        // Egress rule (allow all outbound)
        writeln!(output, "  egress {{").map_err(|e| e.to_string())?;
        writeln!(output, "    from_port   = 0").map_err(|e| e.to_string())?;
        writeln!(output, "    to_port     = 0").map_err(|e| e.to_string())?;
        writeln!(output, "    protocol    = \"-1\"").map_err(|e| e.to_string())?;
        writeln!(output, "    cidr_blocks = [\"0.0.0.0/0\"]")
            .map_err(|e| e.to_string())?;
        writeln!(output, "  }}\n").map_err(|e| e.to_string())?;

        writeln!(output, "  tags = {{\n    Name = \"main-sg\"\n  }}\n}}\n")
            .map_err(|e| e.to_string())?;

        Ok(output)
    }

    /// Generate RDS database resource configuration
    ///
    /// # Arguments
    /// * `db_type` - Database engine type (e.g., "postgres", "mysql")
    /// * `db_name` - Database name to create
    ///
    /// Returns Terraform configuration with:
    /// - aws_db_instance with engine, allocated_storage, instance_class
    /// - Master username, password (from variable), skip final snapshot
    pub fn generate_rds_database(db_type: &str, db_name: &str) -> Result<String, String> {
        let mut output = String::new();

        writeln!(output, "resource \"aws_db_instance\" \"main\" {{")
            .map_err(|e| e.to_string())?;
        writeln!(output, "  allocated_storage = 20").map_err(|e| e.to_string())?;
        writeln!(output, "  storage_type = \"gp2\"").map_err(|e| e.to_string())?;
        writeln!(output, "  engine = \"{}\"", db_type).map_err(|e| e.to_string())?;
        writeln!(output, "  engine_version = \"14.7\"").map_err(|e| e.to_string())?;
        writeln!(output, "  instance_class = \"db.t3.micro\"")
            .map_err(|e| e.to_string())?;
        writeln!(output, "  db_name = \"{}\"", db_name).map_err(|e| e.to_string())?;
        writeln!(output, "  username = \"admin\"").map_err(|e| e.to_string())?;
        writeln!(output, "  password = var.db_password")
            .map_err(|e| e.to_string())?;
        writeln!(output, "  parameter_group_name = \"default.{}14\"", db_type)
            .map_err(|e| e.to_string())?;
        writeln!(output, "  skip_final_snapshot = true").map_err(|e| e.to_string())?;
        writeln!(output, "  multi_az = false\n").map_err(|e| e.to_string())?;
        writeln!(output, "  tags = {{\n    Name = \"main-db\"\n  }}\n}}\n")
            .map_err(|e| e.to_string())?;

        Ok(output)
    }

    /// Generate Terraform variables and outputs for service endpoints
    ///
    /// Returns Terraform configuration with:
    /// - variable "aws_region" with default = "us-east-1"
    /// - variable "db_password" (sensitive, no default)
    /// - output "service_endpoints" with order_api and other service DNS names
    pub fn generate_variables_and_outputs() -> Result<String, String> {
        let mut output = String::new();

        // aws_region variable
        writeln!(
            output,
            "variable \"aws_region\" {{\n  description = \"AWS region\"\n  type = string\n  default = \"us-east-1\"\n}}\n"
        )
        .map_err(|e| e.to_string())?;

        // db_password variable (sensitive)
        writeln!(
            output,
            "variable \"db_password\" {{\n  description = \"RDS master password\"\n  type = string\n  sensitive = true\n}}\n"
        )
        .map_err(|e| e.to_string())?;

        // service_endpoints output
        writeln!(
            output,
            "output \"service_endpoints\" {{\n  description = \"DNS names for service endpoints\"\n  value = {{\n"
        )
        .map_err(|e| e.to_string())?;

        writeln!(
            output,
            "    order_api = \"api-order.example.com\"\n    user_api  = \"api-user.example.com\"\n    db_endpoint = aws_db_instance.main.endpoint\n"
        )
        .map_err(|e| e.to_string())?;

        writeln!(output, "  }}\n}}\n").map_err(|e| e.to_string())?;

        // vpc_id output
        writeln!(
            output,
            "output \"vpc_id\" {{\n  description = \"VPC ID\"\n  value       = aws_vpc.main.id\n}}\n"
        )
        .map_err(|e| e.to_string())?;

        // security_group_id output
        writeln!(
            output,
            "output \"security_group_id\" {{\n  description = \"Security group ID\"\n  value       = aws_security_group.main.id\n}}\n"
        )
        .map_err(|e| e.to_string())?;

        Ok(output)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_provider_block() {
        let result = TerraformGenerator::generate_provider_block();
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("terraform"));
        assert!(code.contains("required_providers"));
        assert!(code.contains("hashicorp/aws"));
        assert!(code.contains("~> 5.0"));
        assert!(code.contains("provider \"aws\""));
        assert!(code.contains("var.aws_region"));
    }

    #[test]
    fn test_generate_vpc_and_security_group_no_ports() {
        let ports: &[u16] = &[];
        let result = TerraformGenerator::generate_vpc_and_security_group(ports);
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("aws_vpc"));
        assert!(code.contains("10.0.0.0/16"));
        assert!(code.contains("aws_security_group"));
        assert!(code.contains("main-sg"));
    }

    #[test]
    fn test_generate_vpc_and_security_group_with_ports() {
        let ports = vec![8089, 9089, 5432];
        let result = TerraformGenerator::generate_vpc_and_security_group(&ports);
        assert!(result.is_ok());
        let code = result.unwrap();

        // Verify VPC
        assert!(code.contains("aws_vpc"));
        assert!(code.contains("10.0.0.0/16"));

        // Verify security group
        assert!(code.contains("aws_security_group"));
        assert!(code.contains("main-sg"));

        // Verify all ports are included
        for port in ports {
            assert!(code.contains(&port.to_string()));
        }

        // Verify ingress rules
        assert!(code.contains("ingress"));
        assert!(code.contains("protocol    = \"tcp\""));
        assert!(code.contains("cidr_blocks = [\"0.0.0.0/0\"]"));

        // Verify egress rule
        assert!(code.contains("egress"));
    }

    #[test]
    fn test_generate_rds_database_postgres() {
        let result = TerraformGenerator::generate_rds_database("postgres", "myapp_db");
        assert!(result.is_ok());
        let code = result.unwrap();

        assert!(code.contains("aws_db_instance"));
        assert!(code.contains("allocated_storage = 20"));
        assert!(code.contains("engine = \"postgres\""));
        assert!(code.contains("instance_class = \"db.t3.micro\""));
        assert!(code.contains("db_name = \"myapp_db\""));
        assert!(code.contains("skip_final_snapshot = true"));
        assert!(code.contains("var.db_password"));
    }

    #[test]
    fn test_generate_rds_database_mysql() {
        let result = TerraformGenerator::generate_rds_database("mysql", "test_db");
        assert!(result.is_ok());
        let code = result.unwrap();

        assert!(code.contains("engine = \"mysql\""));
        assert!(code.contains("db_name = \"test_db\""));
        assert!(code.contains("parameter_group_name = \"default.mysql14\""));
    }

    #[test]
    fn test_generate_variables_and_outputs() {
        let result = TerraformGenerator::generate_variables_and_outputs();
        assert!(result.is_ok());
        let code = result.unwrap();

        // Verify variables
        assert!(code.contains("variable \"aws_region\""));
        assert!(code.contains("default = \"us-east-1\""));
        assert!(code.contains("variable \"db_password\""));
        assert!(code.contains("sensitive = true"));

        // Verify outputs
        assert!(code.contains("output \"service_endpoints\""));
        assert!(code.contains("output \"vpc_id\""));
        assert!(code.contains("output \"security_group_id\""));

        // Verify endpoint values
        assert!(code.contains("order_api"));
        assert!(code.contains("user_api"));
    }

    #[test]
    fn test_provider_block_contains_version_constraint() {
        let result = TerraformGenerator::generate_provider_block();
        let code = result.unwrap();
        assert!(code.contains("version = \"~> 5.0\""));
    }

    #[test]
    fn test_vpc_security_group_integration() {
        let ports = vec![80, 443, 3306];
        let result = TerraformGenerator::generate_vpc_and_security_group(&ports);
        assert!(result.is_ok());
        let code = result.unwrap();

        // Verify both resources in single output.
        // HCL format: resource "aws_vpc" "main" — test substring without leading resource keyword.
        assert!(code.contains("\"aws_vpc\" \"main\""));
        assert!(code.contains("\"aws_security_group\" \"main\""));
        assert!(code.contains("vpc_id = aws_vpc.main.id"));
    }

    #[test]
    fn test_database_with_custom_names() {
        let db_type = "mariadb";
        let db_name = "prod_database";
        let result = TerraformGenerator::generate_rds_database(db_type, db_name);
        assert!(result.is_ok());
        let code = result.unwrap();

        assert!(code.contains(&format!("engine = \"{}\"", db_type)));
        assert!(code.contains(&format!("db_name = \"{}\"", db_name)));
    }
}
