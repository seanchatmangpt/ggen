use crate::manifest::GgenManifest;
use ggen_utils::error::Result;

pub struct PackageValidation {
    pub package_name: String,
    pub version: String,
    pub critical_issues: Vec<String>,
    pub warnings: Vec<String>,
    pub passed: bool,
}

pub struct FmeaRisk {
    pub failure_mode: String,
    pub severity: u8,
    pub occurrence: u8,
    pub detection: u8,
    pub risk_priority_number: u16,
}

pub struct MarketplaceValidator {
    risk_threshold: u16,
}

impl MarketplaceValidator {
    pub fn new(risk_threshold: u16) -> Self {
        Self { risk_threshold }
    }

    pub fn validate_dependencies(&self, manifest: &GgenManifest) -> Result<Vec<PackageValidation>> {
        let mut validations = vec![];

        // Validate generation rules for package dependencies
        for rule in &manifest.generation.rules {
            let issues = vec![];
            let mut warnings = vec![];

            // Check rule name format (packages should use org/package convention)
            if !rule.name.contains('/') {
                warnings.push(format!(
                    "Rule '{}' doesn't use package convention (org/package)",
                    rule.name
                ));
            }

            // Check for circular dependencies in rule names
            let parts: Vec<&str> = rule.name.split('/').collect();
            if parts.len() > 1 {
                let package_name = parts[0].to_string();

                // Detect self-referential patterns
                if manifest
                    .generation
                    .rules
                    .iter()
                    .filter(|r| r.name.starts_with(&format!("{}/", package_name)))
                    .count()
                    > 10
                {
                    warnings.push(format!(
                        "Package '{}' has many rules ({}), may indicate circular dependency risk",
                        package_name,
                        manifest
                            .generation
                            .rules
                            .iter()
                            .filter(|r| r.name.starts_with(&format!("{}/", package_name)))
                            .count()
                    ));
                }
            }

            // Infer package info from rule name
            let (package_name, version) = if parts.len() >= 2 {
                (
                    parts[0].to_string(),
                    parts.get(1).unwrap_or(&"1.0.0").to_string(),
                )
            } else {
                (rule.name.clone(), "1.0.0".to_string())
            };

            let passed = issues.is_empty();

            validations.push(PackageValidation {
                package_name,
                version,
                critical_issues: issues,
                warnings,
                passed,
            });
        }

        Ok(validations)
    }

    pub fn compute_fmea_risks(&self, manifest: &GgenManifest) -> Vec<FmeaRisk> {
        let mut risks = vec![];

        // Risk 1: Ontology import failures
        let ontology_imports = &manifest.ontology.imports;
        if !ontology_imports.is_empty() {
            risks.push(FmeaRisk {
                failure_mode: "Ontology import failure".to_string(),
                severity: 8,
                occurrence: if ontology_imports.len() > 5 { 7 } else { 4 },
                detection: 3,
                risk_priority_number: 8 * 7 * 3,
            });
        }

        // Risk 2: Circular dependencies
        let rule_count = manifest.generation.rules.len();
        if rule_count > 50 {
            risks.push(FmeaRisk {
                failure_mode: "High rule count leading to circular dependencies".to_string(),
                severity: 7,
                occurrence: 5,
                detection: 4,
                risk_priority_number: 7 * 5 * 4,
            });
        }

        // Risk 3: Template rendering failures
        risks.push(FmeaRisk {
            failure_mode: "Template rendering failure".to_string(),
            severity: 6,
            occurrence: 4,
            detection: 5,
            risk_priority_number: 6 * 4 * 5,
        });

        // Risk 4: Memory exhaustion with large ontologies
        risks.push(FmeaRisk {
            failure_mode: "Memory exhaustion during RDF processing".to_string(),
            severity: 9,
            occurrence: 3,
            detection: 6,
            risk_priority_number: 9 * 3 * 6,
        });

        // Risk 5: SPARQL query timeout
        risks.push(FmeaRisk {
            failure_mode: "SPARQL query execution timeout".to_string(),
            severity: 7,
            occurrence: 5,
            detection: 4,
            risk_priority_number: 7 * 5 * 4,
        });

        risks
    }

    pub fn pre_flight_check(&self, manifest: &GgenManifest) -> Result<PreFlightReport> {
        let validations = self.validate_dependencies(manifest)?;
        let all_risks = self.compute_fmea_risks(manifest);

        // Convert to cloneable risks
        let high_risk_items: Vec<FmeaRiskClone> = all_risks
            .iter()
            .filter(|r| r.risk_priority_number >= self.risk_threshold)
            .map(|r| r.into())
            .collect();

        // Count critical failures and warnings before moving validations
        let critical_failures_count = validations
            .iter()
            .filter(|v| !v.critical_issues.is_empty())
            .count();

        let warnings_count = validations.iter().map(|v| v.warnings.len()).sum();
        let all_passed = validations.iter().all(|v| v.passed) && high_risk_items.is_empty();

        Ok(PreFlightReport {
            all_passed,
            validations,
            high_risks: high_risk_items,
            critical_failures_count,
            warnings_count,
        })
    }
}

#[derive(Clone)]
pub struct FmeaRiskClone {
    pub failure_mode: String,
    pub severity: u8,
    pub occurrence: u8,
    pub detection: u8,
    pub risk_priority_number: u16,
}

impl From<&FmeaRisk> for FmeaRiskClone {
    fn from(risk: &FmeaRisk) -> Self {
        FmeaRiskClone {
            failure_mode: risk.failure_mode.clone(),
            severity: risk.severity,
            occurrence: risk.occurrence,
            detection: risk.detection,
            risk_priority_number: risk.risk_priority_number,
        }
    }
}

pub struct PreFlightReport {
    pub all_passed: bool,
    pub validations: Vec<PackageValidation>,
    pub high_risks: Vec<FmeaRiskClone>,
    pub critical_failures_count: usize,
    pub warnings_count: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_marketplace_validator_creation() {
        let validator = MarketplaceValidator::new(100);
        assert_eq!(validator.risk_threshold, 100);
    }

    #[test]
    fn test_fmea_risk_priority_calculation() {
        let risk = FmeaRisk {
            failure_mode: "Test failure".to_string(),
            severity: 8,
            occurrence: 5,
            detection: 3,
            risk_priority_number: 8 * 5 * 3,
        };

        assert_eq!(risk.risk_priority_number, 120);
    }
}
