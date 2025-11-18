//! Γ-Checker: Validate consistency against Q-invariants
//!
//! Ensures all components obey constraints and detect drift

use crate::models::*;
use crate::ontology;
use crate::Result;
use std::collections::{HashMap, HashSet};

/// Validate thesis against Q-invariants
pub fn check_thesis(shards: Vec<DeltaShard>) -> Result<GammaCheckResult> {
    let mut passed = Vec::new();
    let mut failed = Vec::new();
    let mut drift = Vec::new();
    let mut recommendations = Vec::new();

    // Check AllFamiliesCovered
    let families_in_thesis: HashSet<_> = shards.iter().map(|s| s.family).collect();
    let all_families: HashSet<_> = ontology::all_families().into_iter().collect();

    if families_in_thesis.len() == all_families.len() {
        passed.push("AllFamiliesCovered".to_string());
    } else {
        failed.push("AllFamiliesCovered".to_string());
        let missing: Vec<_> = all_families
            .difference(&families_in_thesis)
            .map(|f| format!("{:?}", f))
            .collect();
        recommendations.push(format!("Missing families: {}", missing.join(", ")));
    }

    // Check NoCyclicDependencies
    if !has_cycle(&shards) {
        passed.push("NoCyclicDependencies".to_string());
    } else {
        failed.push("NoCyclicDependencies".to_string());
        recommendations
            .push("Circular dependencies detected. Review shard dependencies.".to_string());
        drift.push("Cyclic dependency detected - structural integrity compromised".to_string());
    }

    // Check ContentNotEmpty
    let empty_shards: Vec<_> = shards
        .iter()
        .filter(|s| s.content.trim().is_empty())
        .map(|s| s.id.clone())
        .collect();

    if empty_shards.is_empty() {
        passed.push("ContentNotEmpty".to_string());
    } else {
        failed.push("ContentNotEmpty".to_string());
        recommendations.push(format!("Empty shards: {}", empty_shards.join(", ")));
    }

    // Check StatusConsistent
    let status_ok = check_status_consistency(&shards);
    if status_ok {
        passed.push("StatusConsistent".to_string());
    } else {
        failed.push("StatusConsistent".to_string());
        recommendations.push("Some shards have inconsistent status transitions.".to_string());
    }

    // Check TotalOrderPreserved (via dependency validation)
    let order_ok = check_order_preservation(&shards);
    if order_ok {
        passed.push("TotalOrderPreserved".to_string());
    } else {
        failed.push("TotalOrderPreserved".to_string());
        recommendations.push("Dependencies violate Λ-ordering constraints.".to_string());
    }

    Ok(GammaCheckResult {
        is_valid: failed.is_empty(),
        invariants_passed: passed,
        invariants_failed: failed,
        drift_detected: drift,
        recommendations,
    })
}

/// Detect cycles in dependency graph
fn has_cycle(shards: &[DeltaShard]) -> bool {
    let mut visited = HashSet::new();
    let mut rec_stack = HashSet::new();

    let shard_map: HashMap<String, &DeltaShard> =
        shards.iter().map(|s| (s.id.clone(), s)).collect();

    for shard in shards {
        if !visited.contains(&shard.id) {
            if dfs_cycle_check(&shard.id, &shard_map, &mut visited, &mut rec_stack) {
                return true;
            }
        }
    }
    false
}

fn dfs_cycle_check(
    node: &str, graph: &HashMap<String, &DeltaShard>, visited: &mut HashSet<String>,
    rec_stack: &mut HashSet<String>,
) -> bool {
    visited.insert(node.to_string());
    rec_stack.insert(node.to_string());

    if let Some(shard) = graph.get(node) {
        for dep in &shard.dependencies {
            if !visited.contains(dep) {
                if dfs_cycle_check(dep, graph, visited, rec_stack) {
                    return true;
                }
            } else if rec_stack.contains(dep) {
                return true;
            }
        }
    }

    rec_stack.remove(node);
    false
}

/// Check status transitions are valid
fn check_status_consistency(shards: &[DeltaShard]) -> bool {
    // All shards should not have contradictory statuses
    // e.g., Final shards shouldn't depend on Draft shards
    for shard in shards {
        if shard.status == ShardStatus::Final {
            // Check that dependencies are also final or review
            for dep_id in &shard.dependencies {
                if let Some(dep) = shards.iter().find(|s| &s.id == dep_id) {
                    if dep.status == ShardStatus::Draft {
                        return false;
                    }
                }
            }
        }
    }
    true
}

/// Check Λ-ordering is preserved
fn check_order_preservation(shards: &[DeltaShard]) -> bool {
    let lambda_order = ontology::lambda_order();
    let family_index: HashMap<ShardFamily, usize> = lambda_order
        .iter()
        .enumerate()
        .map(|(i, f)| (*f, i))
        .collect();

    for shard in shards {
        let shard_order = family_index.get(&shard.family).copied().unwrap_or(1000);
        for dep_id in &shard.dependencies {
            if let Some(dep) = shards.iter().find(|s| &s.id == dep_id) {
                let dep_order = family_index.get(&dep.family).copied().unwrap_or(1000);
                // Dependencies should come before dependents in order
                if dep_order >= shard_order && dep.family != shard.family {
                    return false;
                }
            }
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_check_no_cycle() {
        let shards = vec![
            DeltaShard {
                id: "a".to_string(),
                name: "A".to_string(),
                family: ShardFamily::Intro,
                content: "Content A".to_string(),
                status: ShardStatus::Draft,
                dependencies: vec![],
            },
            DeltaShard {
                id: "b".to_string(),
                name: "B".to_string(),
                family: ShardFamily::Method,
                content: "Content B".to_string(),
                status: ShardStatus::Draft,
                dependencies: vec!["a".to_string()],
            },
        ];

        assert!(!has_cycle(&shards));
    }

    #[test]
    fn test_check_cycle_detected() {
        let shards = vec![
            DeltaShard {
                id: "a".to_string(),
                name: "A".to_string(),
                family: ShardFamily::Intro,
                content: "Content A".to_string(),
                status: ShardStatus::Draft,
                dependencies: vec!["b".to_string()],
            },
            DeltaShard {
                id: "b".to_string(),
                name: "B".to_string(),
                family: ShardFamily::Method,
                content: "Content B".to_string(),
                status: ShardStatus::Draft,
                dependencies: vec!["a".to_string()],
            },
        ];

        assert!(has_cycle(&shards));
    }
}
