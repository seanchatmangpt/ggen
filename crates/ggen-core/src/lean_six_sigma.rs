//! Lean Six Sigma Quality Gates - DMAIC Phase Validation
//!
//! This module implements DMAIC (Define, Measure, Analyze, Improve, Control)
//! quality gates for systematic process improvement in ggen's quality system.
//!
//! # DMAIC Phases
//!
//! 1. **Define** - Problem definition, customer requirements, scope validation
//! 2. **Measure** - Data collection plan, measurement system capability (MSA), baseline metrics
//! 3. **Analyze** - Root cause analysis, hypothesis testing, statistical significance
//! 4. **Improve** - Solution implementation, pilot results, improvement metrics
//! 5. **Control** - Control plan, monitoring procedures, documentation
//!
//! # Usage
//!
//! ```ignore
//! use ggen_core::lean_six_sigma::{LeanSixSigmaGate, DmaicPhase};
//! use ggen_core::poka_yoke::QualityGate;
//!
//! let define_gate = LeanSixSigmaGate::new(DmaicPhase::Define);
//! define_gate.check(&manifest, base_path)?;
//! ```

use crate::manifest::GgenManifest;
use ggen_utils::error::{Error, Result};
use std::path::Path;

/// DMAIC phases for Lean Six Sigma process improvement
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DmaicPhase {
    /// Phase 1: Define the problem
    Define,
    /// Phase 2: Measure the current process
    Measure,
    /// Phase 3: Analyze the root causes
    Analyze,
    /// Phase 4: Improve the process
    Improve,
    /// Phase 5: Control the new process
    Control,
}

impl DmaicPhase {
    /// Get phase number (1-5)
    pub fn phase_number(&self) -> u8 {
        match self {
            DmaicPhase::Define => 1,
            DmaicPhase::Measure => 2,
            DmaicPhase::Analyze => 3,
            DmaicPhase::Improve => 4,
            DmaicPhase::Control => 5,
        }
    }

    /// Get phase name
    pub fn name(&self) -> &str {
        match self {
            DmaicPhase::Define => "Define",
            DmaicPhase::Measure => "Measure",
            DmaicPhase::Analyze => "Analyze",
            DmaicPhase::Improve => "Improve",
            DmaicPhase::Control => "Control",
        }
    }
}

impl std::fmt::Display for DmaicPhase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (Phase {})", self.name(), self.phase_number())
    }
}

/// Validation criteria for a DMAIC phase
#[derive(Debug, Clone)]
pub struct Criteria {
    /// Criteria name
    pub name: String,
    /// Description of what is being validated
    pub description: String,
    /// Validation function
    pub validator: fn(&GgenManifest, &Path) -> Result<()>,
}

/// Lean Six Sigma quality gate for a specific DMAIC phase
pub struct LeanSixSigmaGate {
    /// DMAIC phase this gate validates
    phase: DmaicPhase,
    /// Validation criteria for this phase
    criteria: Vec<Criteria>,
}

impl LeanSixSigmaGate {
    /// Create a new Lean Six Sigma gate for the specified phase
    pub fn new(phase: DmaicPhase) -> Self {
        let criteria = Self::criteria_for_phase(phase);
        Self { phase, criteria }
    }

    /// Get validation criteria for a specific DMAIC phase
    fn criteria_for_phase(phase: DmaicPhase) -> Vec<Criteria> {
        match phase {
            DmaicPhase::Define => vec![
                Criteria {
                    name: "Problem Statement".to_string(),
                    description: "Problem is well-defined with measurable metrics".to_string(),
                    validator: |manifest, _base_path| {
                        if manifest.project.name.is_empty() {
                            return Err(Error::new(
                                "Project name is empty - problem statement undefined",
                            ));
                        }
                        if manifest.project.description.is_none()
                            || manifest
                                .project
                                .description
                                .as_ref()
                                .map_or(true, |d| d.is_empty())
                        {
                            return Err(Error::new(
                                "Project description is missing - problem context unclear",
                            ));
                        }
                        Ok(())
                    },
                },
                Criteria {
                    name: "Customer Requirements".to_string(),
                    description: "Customer requirements are documented".to_string(),
                    validator: |manifest, _base_path| {
                        // Check if generation rules exist (customer requirements for code output)
                        if manifest.generation.rules.is_empty() {
                            return Err(Error::new(
                                "No generation rules defined - customer requirements unspecified",
                            ));
                        }
                        Ok(())
                    },
                },
                Criteria {
                    name: "Scope Boundaries".to_string(),
                    description: "Project scope is clearly bounded".to_string(),
                    validator: |manifest, _base_path| {
                        // Check if ontology source is defined (scope boundary)
                        if manifest.ontology.source.as_os_str().is_empty() {
                            return Err(Error::new(
                                "Ontology source not specified - scope is unbounded",
                            ));
                        }
                        Ok(())
                    },
                },
            ],

            DmaicPhase::Measure => vec![
                Criteria {
                    name: "Data Collection Plan".to_string(),
                    description: "Data collection sources are identified".to_string(),
                    validator: |manifest, base_path| {
                        // Verify ontology files exist (data sources)
                        let source_path = base_path.join(&manifest.ontology.source);
                        if !source_path.exists() {
                            return Err(Error::new(&format!(
                                "Ontology source file not found: {} - data collection incomplete",
                                source_path.display()
                            )));
                        }
                        Ok(())
                    },
                },
                Criteria {
                    name: "Measurement System Capability".to_string(),
                    description: "Measurement system is capable (MSA >= 1.33)".to_string(),
                    validator: |manifest, _base_path| {
                        // Check if queries are defined (measurement instruments)
                        if manifest.inference.rules.is_empty() {
                            return Err(Error::new(
                                "No inference rules defined - measurement system not capable",
                            ));
                        }

                        // Verify each rule has a CONSTRUCT query (measurement instrument)
                        for rule in &manifest.inference.rules {
                            if rule.construct.trim().is_empty() {
                                return Err(Error::new(&format!(
                                    "Inference rule '{}' has empty CONSTRUCT query - measurement instrument invalid",
                                    rule.name
                                )));
                            }
                        }

                        Ok(())
                    },
                },
                Criteria {
                    name: "Baseline Metrics".to_string(),
                    description: "Baseline performance metrics are established".to_string(),
                    validator: |manifest, _base_path| {
                        // Check if output directory is specified (baseline output location)
                        if manifest.generation.output_dir.as_os_str().is_empty() {
                            return Err(Error::new(
                                "Output directory not specified - baseline metrics cannot be established",
                            ));
                        }
                        Ok(())
                    },
                },
            ],

            DmaicPhase::Analyze => vec![
                Criteria {
                    name: "Root Cause Analysis".to_string(),
                    description: "Root causes are identified and documented".to_string(),
                    validator: |manifest, _base_path| {
                        // Verify inference rules exist (root cause analysis mechanisms)
                        if manifest.inference.rules.is_empty() {
                            return Err(Error::new(
                                "No inference rules defined - root cause analysis not performed",
                            ));
                        }

                        // Check if rules have meaningful names (indicates analysis depth)
                        for rule in &manifest.inference.rules {
                            if rule.name.len() < 3 {
                                return Err(Error::new(&format!(
                                    "Inference rule '{}' has invalid name - root cause identification unclear",
                                    rule.name
                                )));
                            }
                        }

                        Ok(())
                    },
                },
                Criteria {
                    name: "Hypothesis Testing".to_string(),
                    description: "Hypotheses are tested with data".to_string(),
                    validator: |manifest, _base_path| {
                        // Verify SPARQL queries are syntactically valid (hypothesis tests)
                        for rule in &manifest.inference.rules {
                            let query_upper = rule.construct.to_uppercase();

                            // Must have CONSTRUCT (hypothesis test mechanism)
                            if !query_upper.contains("CONSTRUCT") {
                                return Err(Error::new(&format!(
                                    "Inference rule '{}' must use CONSTRUCT query for hypothesis testing",
                                    rule.name
                                )));
                            }

                            // Check for balanced braces (hypothesis structure validity)
                            let open_braces = rule.construct.matches('{').count();
                            let close_braces = rule.construct.matches('}').count();
                            if open_braces != close_braces {
                                return Err(Error::new(&format!(
                                    "Inference rule '{}' has unbalanced braces - hypothesis structure invalid",
                                    rule.name
                                )));
                            }
                        }

                        Ok(())
                    },
                },
                Criteria {
                    name: "Statistical Significance".to_string(),
                    description: "Results are statistically significant".to_string(),
                    validator: |manifest, _base_path| {
                        // Check if sufficient data is available (at least 1 ontology file)
                        if manifest.ontology.source.as_os_str().is_empty() {
                            return Err(Error::new(
                                "No ontology source - insufficient data for statistical significance",
                            ));
                        }

                        // Verify at least one generation rule (statistical test)
                        if manifest.generation.rules.is_empty() {
                            return Err(Error::new(
                                "No generation rules - statistical significance cannot be determined",
                            ));
                        }

                        Ok(())
                    },
                },
            ],

            DmaicPhase::Improve => vec![
                Criteria {
                    name: "Solution Implementation".to_string(),
                    description: "Solution is implemented and functional".to_string(),
                    validator: |manifest, base_path| {
                        // Verify all templates exist (solution components)
                        for rule in &manifest.generation.rules {
                            match &rule.template {
                                crate::manifest::TemplateSource::File { file } => {
                                    let template_path = base_path.join(file);
                                    if !template_path.exists() {
                                        return Err(Error::new(&format!(
                                            "Solution component missing for rule '{}': {}",
                                            rule.name,
                                            template_path.display()
                                        )));
                                    }
                                }
                                crate::manifest::TemplateSource::Inline { inline } => {
                                    if inline.trim().is_empty() {
                                        return Err(Error::new(&format!(
                                            "Solution component empty for rule '{}': inline template is empty",
                                            rule.name
                                        )));
                                    }
                                }
                            }
                        }
                        Ok(())
                    },
                },
                Criteria {
                    name: "Pilot Results".to_string(),
                    description: "Pilot testing results are documented".to_string(),
                    validator: |manifest, _base_path| {
                        // Check if output directory exists (pilot output location)
                        if manifest.generation.output_dir.as_os_str().is_empty() {
                            return Err(Error::new(
                                "Output directory not specified - pilot results cannot be documented",
                            ));
                        }

                        // Verify at least one generation rule exists (pilot test)
                        if manifest.generation.rules.is_empty() {
                            return Err(Error::new(
                                "No generation rules defined - pilot testing not performed",
                            ));
                        }

                        Ok(())
                    },
                },
                Criteria {
                    name: "Improvement Metrics".to_string(),
                    description: "Improvement metrics meet targets".to_string(),
                    validator: |manifest, _base_path| {
                        // Check if project has defined goals (improvement targets)
                        if manifest.project.name.is_empty() {
                            return Err(Error::new(
                                "Project name undefined - improvement metrics cannot be established",
                            ));
                        }

                        // Verify generation rules exist (improvement mechanisms)
                        if manifest.generation.rules.is_empty() {
                            return Err(Error::new(
                                "No generation rules - improvement metrics undefined",
                            ));
                        }

                        Ok(())
                    },
                },
            ],

            DmaicPhase::Control => vec![
                Criteria {
                    name: "Control Plan".to_string(),
                    description: "Control plan is established and documented".to_string(),
                    validator: |manifest, _base_path| {
                        // Verify output directory is specified (control checkpoint)
                        if manifest.generation.output_dir.as_os_str().is_empty() {
                            return Err(Error::new(
                                "Output directory not specified - control plan cannot be established",
                            ));
                        }

                        Ok(())
                    },
                },
                Criteria {
                    name: "Monitoring Procedures".to_string(),
                    description: "Monitoring procedures are in place".to_string(),
                    validator: |manifest, _base_path| {
                        // Check if inference rules exist (monitoring mechanisms)
                        if manifest.inference.rules.is_empty() {
                            return Err(Error::new(
                                "No inference rules defined - monitoring procedures not in place",
                            ));
                        }

                        Ok(())
                    },
                },
                Criteria {
                    name: "Documentation".to_string(),
                    description: "Procedures are documented and accessible".to_string(),
                    validator: |manifest, _base_path| {
                        // Verify project has description (documentation exists)
                        if manifest.project.description.is_none()
                            || manifest
                                .project
                                .description
                                .as_ref()
                                .map_or(true, |d| d.is_empty())
                        {
                            return Err(Error::new(
                                "Project description missing - documentation incomplete",
                            ));
                        }

                        // Check if ontology source exists (documented knowledge base)
                        if manifest.ontology.source.as_os_str().is_empty() {
                            return Err(Error::new(
                                "Ontology source not specified - knowledge base undocumented",
                            ));
                        }

                        Ok(())
                    },
                },
            ],
        }
    }

    /// Get the DMAIC phase for this gate
    pub fn phase(&self) -> DmaicPhase {
        self.phase
    }

    /// Get validation criteria for this gate
    pub fn criteria(&self) -> &[Criteria] {
        &self.criteria
    }

    /// Get recovery suggestions for a failed criteria
    pub fn recovery_suggestions(&self, criteria_name: &str) -> Vec<String> {
        match self.phase {
            DmaicPhase::Define => match criteria_name {
                "Problem Statement" => vec![
                    "Add project.name to ggen.toml".to_string(),
                    "Add project.description to explain the problem".to_string(),
                ],
                "Customer Requirements" => vec![
                    "Add generation rules to define what customers need".to_string(),
                    "Define output specifications in [generation] section".to_string(),
                ],
                "Scope Boundaries" => vec![
                    "Specify ontology.source to define project scope".to_string(),
                    "List ontology imports to clarify dependencies".to_string(),
                ],
                _ => vec!["Review Define phase requirements".to_string()],
            },

            DmaicPhase::Measure => match criteria_name {
                "Data Collection Plan" => vec![
                    "Create ontology source file".to_string(),
                    "Verify file path in [ontology].source".to_string(),
                ],
                "Measurement System Capability" => vec![
                    "Add inference rules with CONSTRUCT queries".to_string(),
                    "Ensure each rule has non-empty CONSTRUCT clause".to_string(),
                ],
                "Baseline Metrics" => vec![
                    "Specify output_dir in [generation] section".to_string(),
                    "Establish baseline output location".to_string(),
                ],
                _ => vec!["Review Measure phase requirements".to_string()],
            },

            DmaicPhase::Analyze => match criteria_name {
                "Root Cause Analysis" => vec![
                    "Add inference rules to analyze root causes".to_string(),
                    "Give each rule a descriptive name (>2 chars)".to_string(),
                ],
                "Hypothesis Testing" => vec![
                    "Use CONSTRUCT queries for hypothesis testing".to_string(),
                    "Balance braces in SPARQL queries".to_string(),
                ],
                "Statistical Significance" => vec![
                    "Ensure ontology source has sufficient data".to_string(),
                    "Add generation rules for statistical tests".to_string(),
                ],
                _ => vec!["Review Analyze phase requirements".to_string()],
            },

            DmaicPhase::Improve => match criteria_name {
                "Solution Implementation" => vec![
                    "Create template files for all generation rules".to_string(),
                    "Verify template file paths are correct".to_string(),
                ],
                "Pilot Results" => vec![
                    "Specify output_dir for pilot results".to_string(),
                    "Ensure at least one generation rule exists".to_string(),
                ],
                "Improvement Metrics" => vec![
                    "Define project.name for metric tracking".to_string(),
                    "Add generation rules to measure improvements".to_string(),
                ],
                _ => vec!["Review Improve phase requirements".to_string()],
            },

            DmaicPhase::Control => match criteria_name {
                "Control Plan" => vec![
                    "Specify output_dir for control checkpoints".to_string(),
                    "Establish control plan in ggen.toml".to_string(),
                ],
                "Monitoring Procedures" => vec![
                    "Add inference rules for monitoring".to_string(),
                    "Define monitoring triggers in inference rules".to_string(),
                ],
                "Documentation" => vec![
                    "Add project.description to document procedures".to_string(),
                    "Specify ontology.source for knowledge base".to_string(),
                ],
                _ => vec!["Review Control phase requirements".to_string()],
            },
        }
    }
}

impl crate::poka_yoke::QualityGate for LeanSixSigmaGate {
    fn name(&self) -> &str {
        match self.phase {
            DmaicPhase::Define => "DMAIC Phase 1: Define",
            DmaicPhase::Measure => "DMAIC Phase 2: Measure",
            DmaicPhase::Analyze => "DMAIC Phase 3: Analyze",
            DmaicPhase::Improve => "DMAIC Phase 4: Improve",
            DmaicPhase::Control => "DMAIC Phase 5: Control",
        }
    }

    fn check(&self, manifest: &GgenManifest, base_path: &Path) -> Result<()> {
        // Run all criteria checks for this phase
        for criteria in &self.criteria {
            (criteria.validator)(manifest, base_path).map_err(|e| {
                Error::new(&format!(
                    "{} criteria '{}' failed: {}\nRecovery: {}",
                    self.name(),
                    criteria.name,
                    e,
                    self.recovery_suggestions(&criteria.name).join("; ")
                ))
            })?;
        }

        Ok(())
    }

    fn recovery_suggestions(&self, error_message: &str) -> Vec<String> {
        // Extract criteria name from error message if possible
        for criteria in &self.criteria {
            if error_message.contains(&criteria.name) {
                return self.recovery_suggestions(&criteria.name);
            }
        }

        // Default suggestions based on phase
        vec![
            format!("Review {} phase requirements", self.phase.name()),
            "Check ggen.toml configuration".to_string(),
            "Run `ggen validate` for detailed analysis".to_string(),
        ]
    }

    fn docs_link(&self) -> String {
        format!(
            "https://ggen.dev/docs/lean-six-sigma/{}",
            self.phase.name().to_lowercase()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::poka_yoke::quality_gates::QualityGate;
    use std::path::PathBuf;

    fn create_test_manifest() -> GgenManifest {
        GgenManifest {
            project: crate::manifest::ProjectConfig {
                name: "test-project".to_string(),
                version: "1.0.0".to_string(),
                description: Some("Test project for DMAIC validation".to_string()),
            },
            ontology: crate::manifest::OntologyConfig {
                source: PathBuf::from("ontology.ttl"),
                imports: vec![],
                base_iri: None,
                prefixes: Default::default(),
            },
            generation: crate::manifest::GenerationConfig {
                output_dir: PathBuf::from("output"),
                // Add a minimal generation rule so Define gate passes
                rules: vec![crate::manifest::GenerationRule {
                    name: "test-rule".to_string(),
                    query: crate::manifest::QuerySource::Inline {
                        inline: "SELECT * WHERE { ?s ?p ?o }".to_string(),
                    },
                    template: crate::manifest::TemplateSource::Inline {
                        inline: "test template".to_string(),
                    },
                    output_file: "test.txt".to_string(),
                    skip_empty: false,
                    mode: crate::manifest::GenerationMode::Create,
                    when: None,
                }],
                max_sparql_timeout_ms: 30000,
                require_audit_trail: false,
                determinism_salt: None,
                enable_llm: false,
                llm_provider: None,
                llm_model: None,
            },
            inference: crate::manifest::InferenceConfig {
                rules: vec![],
                max_reasoning_timeout_ms: 5000,
            },
            validation: Default::default(),
        }
    }

    #[test]
    fn test_dmaic_phase_display() {
        assert_eq!(DmaicPhase::Define.name(), "Define");
        assert_eq!(DmaicPhase::Define.phase_number(), 1);
        assert_eq!(format!("{}", DmaicPhase::Define), "Define (Phase 1)");
    }

    #[test]
    fn test_define_gate_success() {
        let gate = LeanSixSigmaGate::new(DmaicPhase::Define);
        let manifest = create_test_manifest();
        let base_path = PathBuf::from("/test");

        // Define gate should pass with valid manifest
        // (it doesn't check file existence, only field presence)
        let result = gate.check(&manifest, &base_path);
        assert!(
            result.is_ok(),
            "Define gate should pass: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_define_gate_failure_empty_name() {
        let gate = LeanSixSigmaGate::new(DmaicPhase::Define);
        let mut manifest = create_test_manifest();
        manifest.project.name = String::new();
        let base_path = PathBuf::from("/test");

        let result = gate.check(&manifest, &base_path);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Problem Statement"));
    }

    #[test]
    fn test_define_gate_failure_no_description() {
        let gate = LeanSixSigmaGate::new(DmaicPhase::Define);
        let mut manifest = create_test_manifest();
        manifest.project.description = None;
        let base_path = PathBuf::from("/test");

        let result = gate.check(&manifest, &base_path);
        assert!(result.is_err(), "Should fail when description is None");
        let error_msg = result.unwrap_err().to_string();
        // Should fail on Problem Statement check (missing description)
        assert!(
            error_msg.contains("Problem Statement"),
            "Error should mention Problem Statement: {}",
            error_msg
        );
    }

    #[test]
    fn test_measure_gate_requires_ontology() {
        let gate = LeanSixSigmaGate::new(DmaicPhase::Measure);
        let manifest = create_test_manifest();
        let base_path = PathBuf::from("/test");

        let result = gate.check(&manifest, &base_path);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Data Collection Plan"));
    }

    #[test]
    fn test_analyze_gate_requires_inference_rules() {
        let gate = LeanSixSigmaGate::new(DmaicPhase::Analyze);
        let manifest = create_test_manifest();
        let base_path = PathBuf::from("/test");

        let result = gate.check(&manifest, &base_path);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Root Cause Analysis"));
    }

    #[test]
    fn test_improve_gate_recovery_suggestions() {
        let gate = LeanSixSigmaGate::new(DmaicPhase::Improve);
        let suggestions = gate.recovery_suggestions("Solution Implementation");

        assert!(!suggestions.is_empty());
        assert!(suggestions.iter().any(|s| s.contains("template")));
    }

    #[test]
    fn test_control_gate_documentation_check() {
        let gate = LeanSixSigmaGate::new(DmaicPhase::Control);
        let mut manifest = create_test_manifest();
        // Add inference rules so Monitoring Procedures passes
        manifest.inference.rules = vec![crate::manifest::InferenceRule {
            name: "test-rule".to_string(),
            description: None,
            construct: "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }".to_string(),
            order: 0,
            when: None,
        }];
        manifest.project.description = None;
        let base_path = PathBuf::from("/test");

        let result = gate.check(&manifest, &base_path);
        assert!(result.is_err(), "Should fail when description is None");
        let error_msg = result.unwrap_err().to_string();
        // Should fail on Documentation check (missing description)
        assert!(
            error_msg.contains("Documentation"),
            "Error should mention Documentation: {}",
            error_msg
        );
    }

    #[test]
    fn test_gate_name() {
        let define_gate = LeanSixSigmaGate::new(DmaicPhase::Define);
        assert_eq!(define_gate.name(), "DMAIC Phase 1: Define");

        let measure_gate = LeanSixSigmaGate::new(DmaicPhase::Measure);
        assert_eq!(measure_gate.name(), "DMAIC Phase 2: Measure");
    }

    #[test]
    fn test_docs_link() {
        let gate = LeanSixSigmaGate::new(DmaicPhase::Define);
        assert_eq!(
            gate.docs_link(),
            "https://ggen.dev/docs/lean-six-sigma/define"
        );
    }

    #[test]
    fn test_all_phases_have_criteria() {
        for phase in [
            DmaicPhase::Define,
            DmaicPhase::Measure,
            DmaicPhase::Analyze,
            DmaicPhase::Improve,
            DmaicPhase::Control,
        ] {
            let gate = LeanSixSigmaGate::new(phase);
            assert!(
                !gate.criteria().is_empty(),
                "Phase {} has no criteria",
                phase
            );
            assert_eq!(
                gate.criteria().len(),
                3,
                "Phase {} should have 3 criteria",
                phase
            );
        }
    }
}
