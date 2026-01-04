use crate::manifest::GgenManifest;
use ggen_utils::error::Result;
use std::collections::{HashMap, HashSet};
use std::path::Path;

pub struct DependencyValidator;

pub struct DependencyCheck {
    pub name: String,
    pub version: String,
    pub passed: bool,
    pub details: String,
}

pub struct DependencyValidationReport {
    pub manifest_path: String,
    pub total_checks: usize,
    pub passed_checks: usize,
    pub failed_checks: usize,
    pub checks: Vec<DependencyCheck>,
    pub has_cycles: bool,
    pub cycle_nodes: Vec<String>,
}

impl DependencyValidator {
    pub fn validate_manifest(manifest: &GgenManifest, base_path: &Path) -> Result<DependencyValidationReport> {
        let mut checks = vec![];
        let mut failed_count = 0;

        // Check 1: Ontology exists and is readable
        let ontology_path = base_path.join(&manifest.ontology.source);
        let ontology_check = if ontology_path.exists() {
            DependencyCheck {
                name: "ontology_exists".to_string(),
                version: "1.0.0".to_string(),
                passed: true,
                details: format!("Ontology found at {}", ontology_path.display()),
            }
        } else {
            failed_count += 1;
            DependencyCheck {
                name: "ontology_exists".to_string(),
                version: "1.0.0".to_string(),
                passed: false,
                details: format!("Ontology not found at {}", ontology_path.display()),
            }
        };
        checks.push(ontology_check);

        // Check 2: All ontology imports exist
        for import in &manifest.ontology.imports {
            let import_path = base_path.join(import);
            let import_name = import.display().to_string();
            let import_check = if import_path.exists() {
                DependencyCheck {
                    name: format!("import_{}", import_name),
                    version: "1.0.0".to_string(),
                    passed: true,
                    details: format!("Import found at {}", import_path.display()),
                }
            } else {
                failed_count += 1;
                DependencyCheck {
                    name: format!("import_{}", import_name),
                    version: "1.0.0".to_string(),
                    passed: false,
                    details: format!("Import not found at {}", import_path.display()),
                }
            };
            checks.push(import_check);
        }

        // Check 3: Inference rule ordering (topological sort for dependencies)
        let (has_cycles, cycle_nodes) = Self::detect_inference_cycles(&manifest.inference.rules);
        if has_cycles {
            failed_count += 1;
            checks.push(DependencyCheck {
                name: "inference_cycles".to_string(),
                version: "1.0.0".to_string(),
                passed: false,
                details: format!("Circular dependency detected: {:?}", cycle_nodes),
            });
        } else {
            checks.push(DependencyCheck {
                name: "inference_cycles".to_string(),
                version: "1.0.0".to_string(),
                passed: true,
                details: "No circular dependencies detected".to_string(),
            });
        }

        // Check 4: Generation rule templates exist (if file-based)
        for rule in &manifest.generation.rules {
            if let crate::manifest::TemplateSource::File { file } = &rule.template {
                let template_path = base_path.join(file);
                let template_check = if template_path.exists() {
                    DependencyCheck {
                        name: format!("template_{}", rule.name),
                        version: "1.0.0".to_string(),
                        passed: true,
                        details: format!("Template found at {}", template_path.display()),
                    }
                } else {
                    failed_count += 1;
                    DependencyCheck {
                        name: format!("template_{}", rule.name),
                        version: "1.0.0".to_string(),
                        passed: false,
                        details: format!("Template not found at {}", template_path.display()),
                    }
                };
                checks.push(template_check);
            }
        }

        // Check 5: Query files exist (if file-based)
        for rule in &manifest.generation.rules {
            if let crate::manifest::QuerySource::File { file } = &rule.query {
                let query_path = base_path.join(file);
                let query_check = if query_path.exists() {
                    DependencyCheck {
                        name: format!("query_{}", rule.name),
                        version: "1.0.0".to_string(),
                        passed: true,
                        details: format!("Query found at {}", query_path.display()),
                    }
                } else {
                    failed_count += 1;
                    DependencyCheck {
                        name: format!("query_{}", rule.name),
                        version: "1.0.0".to_string(),
                        passed: false,
                        details: format!("Query not found at {}", query_path.display()),
                    }
                };
                checks.push(query_check);
            }
        }

        let total_checks = checks.len();
        let passed_checks = total_checks - failed_count;

        Ok(DependencyValidationReport {
            manifest_path: manifest.project.name.clone(),
            total_checks,
            passed_checks,
            failed_checks: failed_count,
            checks,
            has_cycles,
            cycle_nodes,
        })
    }

    fn detect_inference_cycles(
        rules: &[crate::manifest::InferenceRule],
    ) -> (bool, Vec<String>) {
        let mut graph: HashMap<String, HashSet<String>> = HashMap::new();
        let mut visited: HashSet<String> = HashSet::new();
        let mut rec_stack: HashSet<String> = HashSet::new();
        let mut cycles = vec![];

        for rule in rules {
            graph.insert(rule.name.clone(), HashSet::new());
        }

        // Build dependency graph based on rule names
        for rule in rules {
            if let Some(when) = &rule.when {
                for other_rule in rules {
                    if when.contains(&other_rule.name) && other_rule.name != rule.name {
                        if let Some(deps) = graph.get_mut(&rule.name) {
                            deps.insert(other_rule.name.clone());
                        }
                    }
                }
            }
        }

        for node in graph.keys() {
            if !visited.contains(node) {
                let has_cycle = Self::dfs_cycle(&graph, node, &mut visited, &mut rec_stack, &mut cycles);
                if has_cycle {
                    return (true, cycles);
                }
            }
        }

        (false, vec![])
    }

    fn dfs_cycle(
        graph: &HashMap<String, HashSet<String>>,
        node: &str,
        visited: &mut HashSet<String>,
        rec_stack: &mut HashSet<String>,
        cycles: &mut Vec<String>,
    ) -> bool {
        visited.insert(node.to_string());
        rec_stack.insert(node.to_string());

        if let Some(neighbors) = graph.get(node) {
            for neighbor in neighbors {
                if !visited.contains(neighbor) {
                    if Self::dfs_cycle(graph, neighbor, visited, rec_stack, cycles) {
                        return true;
                    }
                } else if rec_stack.contains(neighbor) {
                    cycles.push(format!("{} -> {}", node, neighbor));
                    return true;
                }
            }
        }

        rec_stack.remove(node);
        false
    }
}

impl Default for crate::manifest::GenerationRule {
    fn default() -> Self {
        Self {
            name: String::new(),
            query: crate::manifest::QuerySource::Inline { inline: String::new() },
            template: crate::manifest::TemplateSource::Inline { inline: String::new() },
            output_file: String::new(),
            skip_empty: false,
            mode: crate::manifest::GenerationMode::Overwrite,
            when: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cycle_detection() {
        let rules = vec![
            crate::manifest::InferenceRule {
                name: "rule1".to_string(),
                construct: "".to_string(),
                order: 0,
                description: None,
                when: Some("rule2".to_string()),
            },
            crate::manifest::InferenceRule {
                name: "rule2".to_string(),
                construct: "".to_string(),
                order: 1,
                description: None,
                when: Some("rule1".to_string()),
            },
        ];

        let (has_cycle, cycles) = DependencyValidator::detect_inference_cycles(&rules);
        assert!(has_cycle);
        assert!(!cycles.is_empty());
    }
}
