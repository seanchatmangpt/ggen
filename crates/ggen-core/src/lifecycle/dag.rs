//! DAG construction and topological sorting for phase dependencies
//!
//! This module provides functionality for building dependency graphs and performing
//! topological sorting of lifecycle phases. It ensures phases execute in the correct
//! order based on their dependencies and detects circular dependencies.
//!
//! ## Features
//!
//! - **Topological sorting**: Order phases based on dependencies
//! - **Dependency graph construction**: Build graphs from phase dependencies
//! - **Cycle detection**: Detect and report circular dependencies
//! - **Hook-based dependencies**: Extract dependencies from before/after hooks
//!
//! ## Examples
//!
//! ### Topological Sorting
//!
//! ```rust,no_run
//! use ggen_core::lifecycle::dag::topo;
//! use ggen_core::lifecycle::Result;
//!
//! # fn main() -> Result<()> {
//! let phases = &["init", "setup", "build", "test"];
//! let deps = &[
//!     ("init", "setup"),
//!     ("setup", "build"),
//!     ("build", "test"),
//! ];
//!
//! let order = topo(phases, deps)?;
//! // Result: ["init", "setup", "build", "test"]
//! # Ok(())
//! # }
//! ```
//!
//! ### Extracting Dependencies from Hooks
//!
//! ```rust,no_run
//! use ggen_core::lifecycle::dag::deps_from_hooks;
//!
//! // If build phase has before=["test", "lint"] and after=["deploy"]
//! let deps = deps_from_hooks(
//!     "build",
//!     &["test".to_string(), "lint".to_string()],
//!     &["deploy".to_string()],
//! );
//!
//! // Result: [("test", "build"), ("lint", "build"), ("build", "deploy")]
//! ```

use super::error::{LifecycleError, Result};
use petgraph::algo::toposort;
use petgraph::graphmap::DiGraphMap;

/// Perform topological sort on phases with dependencies
///
/// Returns phases in execution order
pub fn topo(phases: &[&str], deps: &[(&str, &str)]) -> Result<Vec<String>> {
    let mut graph = DiGraphMap::<&str, ()>::new();

    // Add all phases as nodes
    for phase in phases {
        graph.add_node(phase);
    }

    // Add dependency edges (a -> b means "a must run before b")
    for (before, after) in deps {
        graph.add_edge(*before, *after, ());
    }

    // Perform topological sort
    toposort(&graph, None)
        .map(|sorted| sorted.into_iter().map(|s| s.to_string()).collect())
        .map_err(|_| LifecycleError::dependency_cycle(format!("{:?}", phases)))
}

/// Build dependency graph from hooks
///
/// before_build = ["test", "lint"] means test->build, lint->build
pub fn deps_from_hooks(phase: &str, before: &[String], after: &[String]) -> Vec<(String, String)> {
    let mut deps = Vec::new();

    // before hooks must run before phase
    for hook in before {
        deps.push((hook.clone(), phase.to_string()));
    }

    // phase must run before after hooks
    for hook in after {
        deps.push((phase.to_string(), hook.clone()));
    }

    deps
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::test;

    test!(test_topo_simple, {
        let phases = &["init", "setup", "build"];
        let deps = &[("init", "setup"), ("setup", "build")];

        let order = topo(phases, deps).expect("topo should succeed");

        assert_eq!(order, vec!["init", "setup", "build"]);
    });

    test!(test_topo_parallel, {
        let phases = &["test", "lint", "build"];
        let deps = &[("test", "build"), ("lint", "build")];

        let order = topo(phases, deps).expect("topo should succeed");

        // test and lint can be in any order, but both before build
        assert_eq!(order.last(), Some(&"build".to_string()));
    });

    test!(test_topo_cycle_detection, {
        let phases = &["a", "b", "c"];
        let deps = &[("a", "b"), ("b", "c"), ("c", "a")]; // cycle: a->b->c->a

        let result = topo(phases, deps);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Circular dependency"));
    });

    test!(test_deps_from_hooks, {
        let deps = deps_from_hooks("build", &["test".to_string()], &["deploy".to_string()]);

        assert_eq!(deps.len(), 2);
        assert!(deps.contains(&("test".to_string(), "build".to_string())));
        assert!(deps.contains(&("build".to_string(), "deploy".to_string())));
    });
}
