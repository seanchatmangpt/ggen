//! Marketplace Sector Bundles
//!
//! Bundles are curated collections of packages forming complete vertical stacks.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Sector bundle definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SectorBundle {
    pub id: String,
    pub version: String,
    pub description: String,
    pub domain: String,
    pub minimum_score: f64,
    pub packages: Vec<String>,
    pub features: Vec<String>,
}

/// Bundle registry
pub struct BundleRegistry;

impl BundleRegistry {
    /// Get all available bundles
    pub fn list_bundles() -> Vec<SectorBundle> {
        vec![
            Self::academic_papers(),
            Self::enterprise_saas(),
            Self::data_pipelines(),
            Self::healthcare(),
            Self::fintech(),
        ]
    }

    /// Get a bundle by ID
    pub fn get_bundle(id: &str) -> Option<SectorBundle> {
        Self::list_bundles()
            .into_iter()
            .find(|b| b.id == id)
    }

    /// Academic Papers Bundle
    fn academic_papers() -> SectorBundle {
        SectorBundle {
            id: "sector-academic-papers".to_string(),
            version: "1.0.0".to_string(),
            description: "Complete lifecycle for academic paper development, peer review, and publishing with ontological metadata".to_string(),
            domain: "academic".to_string(),
            minimum_score: 80.0,
            packages: vec![
                "academic-paper-lifecycle".to_string(),
                "academic-bibliography-manager".to_string(),
                "academic-peer-review-workflow".to_string(),
            ],
            features: vec![
                "Paper template generation".to_string(),
                "Bibliography management with BibTeX".to_string(),
                "Peer review workflow".to_string(),
                "Citation tracking".to_string(),
                "Publication metadata".to_string(),
            ],
        }
    }

    /// Enterprise SaaS Bundle
    fn enterprise_saas() -> SectorBundle {
        SectorBundle {
            id: "sector-enterprise-saas".to_string(),
            version: "1.0.0".to_string(),
            description: "Multi-tenant SaaS architectures with CRM, ERP, and enterprise features".to_string(),
            domain: "enterprise".to_string(),
            minimum_score: 80.0,
            packages: vec![
                "multi-tenant-saas".to_string(),
                "crm-customer-management".to_string(),
                "enterprise-erp-core".to_string(),
                "human-resources-management".to_string(),
            ],
            features: vec![
                "Multi-tenant architecture".to_string(),
                "Customer relationship management".to_string(),
                "Enterprise resource planning".to_string(),
                "User management and roles".to_string(),
                "Audit logging".to_string(),
            ],
        }
    }

    /// Data Pipelines Bundle
    fn data_pipelines() -> SectorBundle {
        SectorBundle {
            id: "sector-data-pipelines".to_string(),
            version: "1.0.0".to_string(),
            description: "Data engineering stack for ETL, transformation, schema management, and analytics".to_string(),
            domain: "data".to_string(),
            minimum_score: 80.0,
            packages: vec![
                "data-pipeline-cli".to_string(),
                "database-schema-generator".to_string(),
                "business-intelligence-reporting".to_string(),
            ],
            features: vec![
                "ETL pipeline management".to_string(),
                "Schema generation and validation".to_string(),
                "Data transformation".to_string(),
                "Analytics and reporting".to_string(),
                "Data quality monitoring".to_string(),
            ],
        }
    }

    /// Healthcare Systems Bundle
    fn healthcare() -> SectorBundle {
        SectorBundle {
            id: "sector-healthcare".to_string(),
            version: "1.0.0".to_string(),
            description: "Healthcare system architectures including EHR, laboratory management, and clinical data".to_string(),
            domain: "healthcare".to_string(),
            minimum_score: 85.0,
            packages: vec![
                "healthcare-analytics".to_string(),
                "laboratory-information-system".to_string(),
                "medical-billing".to_string(),
                "ehr-integration".to_string(),
            ],
            features: vec![
                "Electronic Health Records (EHR)".to_string(),
                "Laboratory information systems".to_string(),
                "Medical billing and claims".to_string(),
                "Clinical analytics".to_string(),
                "HIPAA-compliant data handling".to_string(),
                "Patient management".to_string(),
            ],
        }
    }

    /// Financial Services Bundle
    fn fintech() -> SectorBundle {
        SectorBundle {
            id: "sector-fintech".to_string(),
            version: "1.0.0".to_string(),
            description: "Financial services stack including banking, payments, KYC/AML, and trading systems".to_string(),
            domain: "finance".to_string(),
            minimum_score: 90.0,
            packages: vec![
                "banking-core".to_string(),
                "iso-20022-payments".to_string(),
                "kyc-aml-compliance".to_string(),
                "cryptocurrency-exchange".to_string(),
            ],
            features: vec![
                "Core banking infrastructure".to_string(),
                "Payment processing (ISO 20022)".to_string(),
                "KYC/AML compliance".to_string(),
                "Cryptocurrency support".to_string(),
                "Regulatory reporting".to_string(),
                "Transaction auditing".to_string(),
            ],
        }
    }
}

/// Bundle installation manifest
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BundleInstallManifest {
    pub bundle_id: String,
    pub installed_at: String,
    pub packages_installed: Vec<String>,
    pub packages_failed: Vec<(String, String)>,
    pub total_packages: usize,
    pub successful_installs: usize,
}

impl BundleInstallManifest {
    pub fn new(bundle_id: String) -> Self {
        Self {
            bundle_id,
            installed_at: chrono::Utc::now().to_rfc3339(),
            packages_installed: Vec::new(),
            packages_failed: Vec::new(),
            total_packages: 0,
            successful_installs: 0,
        }
    }
}

/// Generate bundle documentation
pub fn generate_bundle_docs(bundle: &SectorBundle) -> String {
    let mut doc = format!(
        r#"# {} Sector Bundle

**Version**: {}
**Domain**: {}
**Minimum Package Score**: {}%

## Description
{}

## Included Packages

{}

## Features

{}

## Installation

```bash
ggen marketplace install-bundle {}
```

## Bundle Validation

This bundle is validated as a complete vertical stack. When installed, all packages are:
1. Checked for compatibility
2. Validated against minimum score thresholds
3. Installed in dependency order

## Documentation

For more information on each package, run:
```bash
ggen marketplace info <package-name>
```
"#,
        bundle.id,
        bundle.version,
        bundle.domain,
        bundle.minimum_score as u32,
        bundle.description,
        bundle.packages
            .iter()
            .map(|p| format!("- `{}`", p))
            .collect::<Vec<_>>()
            .join("\n"),
        bundle.features
            .iter()
            .map(|f| format!("- {}", f))
            .collect::<Vec<_>>()
            .join("\n"),
        bundle.id
    );

    doc
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bundle_registry() {
        let bundles = BundleRegistry::list_bundles();
        assert!(bundles.len() >= 5);
        assert!(BundleRegistry::get_bundle("sector-academic-papers").is_some());
    }

    #[test]
    fn test_bundle_docs_generation() {
        let bundle = BundleRegistry::academic_papers();
        let docs = generate_bundle_docs(&bundle);
        assert!(docs.contains("Academic Papers"));
        assert!(docs.contains("sector-academic-papers"));
    }
}
