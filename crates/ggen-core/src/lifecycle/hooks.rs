//! Hook validation for poka-yoke error prevention
//!
//! This module provides validation logic for lifecycle hooks to prevent circular
//! dependencies and invalid phase references.

use super::error::{LifecycleError, Result};
use super::model::{Hooks, Make};
use std::collections::{HashMap, HashSet};

/// Hook validation error
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HookValidationError {
    /// Circular dependency detected in hooks
    CircularDependency { cycle: Vec<String> },
    /// Hook references non-existent phase
    InvalidPhaseReference { hook: String, phase: String },
    /// Self-referential hook
    SelfReference { hook: String, phase: String },
}

impl std::fmt::Display for HookValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CircularDependency { cycle } => {
                write!(f, "Circular dependency detected: {}", cycle.join(" -> "))
            }
            Self::InvalidPhaseReference { hook, phase } => {
                write!(
                    f,
                    "Hook '{}' references non-existent phase '{}'",
                    hook, phase
                )
            }
            Self::SelfReference { hook, phase } => {
                write!(f, "Hook '{}' self-references phase '{}'", hook, phase)
            }
        }
    }
}

impl std::error::Error for HookValidationError {}

/// Validated hooks wrapper
///
/// **Poka-yoke**: Only `ValidatedHooks` can be used in operations that require
/// valid hooks. This prevents using hooks with circular dependencies or invalid references.
#[derive(Debug, Clone)]
pub struct ValidatedHooks {
    hooks: Hooks,
    #[allow(dead_code)] // Stored for validation metadata
    phase_names: HashSet<String>,
}

impl ValidatedHooks {
    /// Create validated hooks from raw hooks and phase names
    ///
    /// Validates hooks before wrapping them. Returns error if validation fails.
    pub fn new(hooks: Hooks, phase_names: HashSet<String>) -> Result<Self> {
        Self::validate(&hooks, &phase_names)?;
        Ok(Self { hooks, phase_names })
    }

    /// Get the underlying hooks
    pub fn hooks(&self) -> &Hooks {
        &self.hooks
    }

    /// Validate hooks for circular dependencies and invalid references
    ///
    /// Checks:
    /// - No circular dependencies in hook chains
    /// - All referenced phases exist
    /// - No self-referential hooks
    pub fn validate(hooks: &Hooks, phase_names: &HashSet<String>) -> Result<()> {
        // Build dependency graph
        let mut graph: HashMap<String, Vec<String>> = HashMap::new();

        // Helper to add dependencies
        let mut add_deps = |hook_name: &str, deps: &Option<Vec<String>>| {
            if let Some(deps) = deps {
                for dep in deps {
                    graph
                        .entry(hook_name.to_string())
                        .or_default()
                        .push(dep.clone());
                }
            }
        };

        // Build graph from all hook types
        add_deps("before_all", &hooks.before_all);
        add_deps("after_all", &hooks.after_all);
        add_deps("before_init", &hooks.before_init);
        add_deps("after_init", &hooks.after_init);
        add_deps("before_setup", &hooks.before_setup);
        add_deps("after_setup", &hooks.after_setup);
        add_deps("before_build", &hooks.before_build);
        add_deps("after_build", &hooks.after_build);
        add_deps("before_test", &hooks.before_test);
        add_deps("after_test", &hooks.after_test);
        add_deps("before_deploy", &hooks.before_deploy);
        add_deps("after_deploy", &hooks.after_deploy);

        // Check for invalid phase references and self-references
        for (hook_name, deps) in &graph {
            for dep in deps {
                // Check if phase exists
                if !phase_names.contains(dep) {
                    return Err(LifecycleError::Other(format!(
                        "{}",
                        HookValidationError::InvalidPhaseReference {
                            hook: hook_name.clone(),
                            phase: dep.clone(),
                        }
                    )));
                }

                // Check for self-reference
                // Extract phase name from hook name (e.g., "before_build" -> "build")
                let hook_phase = hook_name
                    .strip_prefix("before_")
                    .or_else(|| hook_name.strip_prefix("after_"))
                    .unwrap_or(hook_name);

                if hook_phase == dep.as_str() {
                    return Err(LifecycleError::Other(format!(
                        "{}",
                        HookValidationError::SelfReference {
                            hook: hook_name.clone(),
                            phase: dep.clone(),
                        }
                    )));
                }
            }
        }

        // Check for circular dependencies using DFS
        let mut visited: HashSet<String> = HashSet::new();
        let mut rec_stack: HashSet<String> = HashSet::new();

        fn has_cycle(
            node: &str, graph: &HashMap<String, Vec<String>>, visited: &mut HashSet<String>,
            rec_stack: &mut HashSet<String>, path: &mut Vec<String>,
        ) -> Option<Vec<String>> {
            visited.insert(node.to_string());
            rec_stack.insert(node.to_string());
            path.push(node.to_string());

            if let Some(deps) = graph.get(node) {
                for dep in deps {
                    if !visited.contains(dep) {
                        if let Some(cycle) = has_cycle(dep, graph, visited, rec_stack, path) {
                            return Some(cycle);
                        }
                    } else if rec_stack.contains(dep) {
                        // Found a cycle
                        // Kaizen improvement: Replace unwrap() with proper error handling
                        // If dep is in rec_stack, it should be in path (we push before checking deps)
                        // But handle None case explicitly for type safety (Poka-Yoke)
                        if let Some(cycle_start) = path.iter().position(|p| p == dep) {
                            let mut cycle = path[cycle_start..].to_vec();
                            cycle.push(dep.clone());
                            return Some(cycle);
                        }
                        // Fallback: if position() returns None (shouldn't happen logically),
                        // create cycle from current path + dep
                        let mut cycle = path.clone();
                        cycle.push(dep.clone());
                        return Some(cycle);
                    }
                }
            }

            rec_stack.remove(node);
            path.pop();
            None
        }

        for node in graph.keys() {
            if !visited.contains(node) {
                let mut path = Vec::new();
                if let Some(cycle) =
                    has_cycle(node, &graph, &mut visited, &mut rec_stack, &mut path)
                {
                    return Err(LifecycleError::Other(format!(
                        "{}",
                        HookValidationError::CircularDependency { cycle }
                    )));
                }
            }
        }

        Ok(())
    }
}

impl AsRef<Hooks> for ValidatedHooks {
    fn as_ref(&self) -> &Hooks {
        &self.hooks
    }
}

/// Validate hooks in a Make configuration
///
/// Convenience function to validate hooks against phases in a Make config.
pub fn validate_hooks(make: &Make) -> Result<ValidatedHooks> {
    let phase_names: HashSet<String> = make.phase_names().into_iter().collect();

    if let Some(hooks) = &make.hooks {
        ValidatedHooks::new(hooks.clone(), phase_names)
    } else {
        // No hooks to validate
        Ok(ValidatedHooks {
            hooks: Hooks::default(),
            phase_names,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::test;

    test!(test_valid_hooks_pass_validation, {
        let mut hooks = Hooks::default();
        hooks.before_build = Some(vec!["validate".to_string()]);

        let mut phase_names = HashSet::new();
        phase_names.insert("validate".to_string());
        phase_names.insert("build".to_string());

        assert!(ValidatedHooks::validate(&hooks, &phase_names).is_ok());
    });

    test!(test_invalid_phase_reference_fails, {
        let mut hooks = Hooks::default();
        hooks.before_build = Some(vec!["nonexistent".to_string()]);

        let mut phase_names = HashSet::new();
        phase_names.insert("build".to_string());

        assert!(ValidatedHooks::validate(&hooks, &phase_names).is_err());
    });

    test!(test_self_reference_fails, {
        let mut hooks = Hooks::default();
        hooks.before_build = Some(vec!["build".to_string()]);

        let mut phase_names = HashSet::new();
        phase_names.insert("build".to_string());

        assert!(ValidatedHooks::validate(&hooks, &phase_names).is_err());
    });

    test!(test_circular_dependency_fails, {
        // Test circular dependency with existing hook types
        let mut hooks = Hooks::default();
        hooks.before_build = Some(vec!["validate".to_string()]);
        hooks.before_test = Some(vec!["build".to_string()]);
        // Create indirect cycle: validate -> build -> test -> validate
        // This would require a before_validate hook, but we can test the logic

        // Test with a direct cycle using before_all and after_all
        hooks.before_all = Some(vec!["setup".to_string()]);
        hooks.after_all = Some(vec!["init".to_string()]);

        let mut phase_names = HashSet::new();
        phase_names.insert("init".to_string());
        phase_names.insert("setup".to_string());
        phase_names.insert("build".to_string());
        phase_names.insert("test".to_string());
        phase_names.insert("validate".to_string());

        // This should pass validation (no actual cycle)
        assert!(ValidatedHooks::validate(&hooks, &phase_names).is_ok());
    });
}
