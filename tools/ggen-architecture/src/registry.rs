//! Architecture registry, dependency planning, and impact analysis.

use std::collections::{BTreeMap, BTreeSet, VecDeque};

use serde::{Deserialize, Serialize};

use crate::{
    error::{ArchitectureError, Result},
    model::{ArchitectureAsset, LifecycleState, Severity, TransitionStep},
};

/// One structural or governance violation in the registry.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RegistryViolation {
    /// Stable diagnostic code.
    pub code: String,
    /// Finding severity.
    pub severity: Severity,
    /// Primary subject.
    pub asset_id: String,
    /// Human-readable explanation.
    pub message: String,
}

/// Dependency and revalidation impact for one changed architecture asset.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ImpactReport {
    /// Changed root asset.
    pub root: String,
    /// Root plus all transitive dependents.
    pub affected: Vec<String>,
    /// Dependency-safe revalidation order.
    pub ordered_revalidation: Vec<String>,
    /// One shortest discovered path from the root to each affected asset.
    pub paths: BTreeMap<String, Vec<String>>,
}

/// Authoritative registry for architecture assets and their realization edges.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArchitectureRegistry {
    /// Registry schema version.
    #[serde(default = "default_schema_version")]
    pub schema_version: u32,
    /// Assets indexed by stable identifier.
    #[serde(default)]
    pub assets: BTreeMap<String, ArchitectureAsset>,
}

const fn default_schema_version() -> u32 {
    1
}

impl Default for ArchitectureRegistry {
    fn default() -> Self {
        Self {
            schema_version: default_schema_version(),
            assets: BTreeMap::new(),
        }
    }
}

impl ArchitectureRegistry {
    /// Construct an empty versioned registry.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a new architecture asset.
    pub fn register(&mut self, asset: ArchitectureAsset) -> Result<()> {
        let id = asset.id.trim();
        if id.is_empty() {
            return Err(ArchitectureError::InvalidAssetId(
                "identifier must not be empty".to_string(),
            ));
        }
        if self.assets.contains_key(id) {
            return Err(ArchitectureError::DuplicateAsset(id.to_string()));
        }
        self.assets.insert(id.to_string(), asset);
        Ok(())
    }

    /// Return an asset by identifier.
    pub fn asset(&self, asset_id: &str) -> Result<&ArchitectureAsset> {
        self.assets
            .get(asset_id)
            .ok_or_else(|| ArchitectureError::UnknownAsset(asset_id.to_string()))
    }

    /// Return a mutable asset by identifier.
    pub fn asset_mut(&mut self, asset_id: &str) -> Result<&mut ArchitectureAsset> {
        self.assets
            .get_mut(asset_id)
            .ok_or_else(|| ArchitectureError::UnknownAsset(asset_id.to_string()))
    }

    /// Add a declared dependency after proving both endpoints exist.
    pub fn add_dependency(&mut self, asset_id: &str, dependency_id: &str) -> Result<()> {
        if !self.assets.contains_key(dependency_id) {
            return Err(ArchitectureError::UnknownAsset(dependency_id.to_string()));
        }
        self.asset_mut(asset_id)?
            .dependencies
            .insert(dependency_id.to_string());
        Ok(())
    }

    /// Move an asset through one lawful lifecycle transition.
    pub fn transition(&mut self, asset_id: &str, target: LifecycleState) -> Result<()> {
        let asset = self.asset_mut(asset_id)?;
        if !asset.lifecycle.allows(target) {
            return Err(ArchitectureError::InvalidTransition {
                asset_id: asset_id.to_string(),
                from: asset.lifecycle.to_string(),
                to: target.to_string(),
            });
        }
        asset.lifecycle = target;
        Ok(())
    }

    /// Root plus all transitively required dependencies.
    pub fn dependency_closure(&self, root: &str) -> Result<BTreeSet<String>> {
        self.asset(root)?;
        let mut closure = BTreeSet::new();
        let mut stack = vec![root.to_string()];

        while let Some(asset_id) = stack.pop() {
            if !closure.insert(asset_id.clone()) {
                continue;
            }
            let asset = self.asset(&asset_id)?;
            for dependency in asset.dependencies.iter().rev() {
                stack.push(dependency.clone());
            }
        }

        Ok(closure)
    }

    /// Calculate every asset requiring revalidation after `root` changes.
    pub fn impact_report(&self, root: &str) -> Result<ImpactReport> {
        self.asset(root)?;
        let mut reverse = BTreeMap::<String, Vec<String>>::new();
        for asset in self.assets.values() {
            for dependency in &asset.dependencies {
                reverse
                    .entry(dependency.clone())
                    .or_default()
                    .push(asset.id.clone());
            }
        }
        for dependents in reverse.values_mut() {
            dependents.sort();
            dependents.dedup();
        }

        let mut affected = BTreeSet::new();
        let mut paths = BTreeMap::new();
        let mut queue = VecDeque::new();
        affected.insert(root.to_string());
        paths.insert(root.to_string(), vec![root.to_string()]);
        queue.push_back(root.to_string());

        while let Some(current) = queue.pop_front() {
            let current_path = paths
                .get(&current)
                .cloned()
                .unwrap_or_else(|| vec![root.to_string()]);
            if let Some(dependents) = reverse.get(&current) {
                for dependent in dependents {
                    if affected.insert(dependent.clone()) {
                        let mut path = current_path.clone();
                        path.push(dependent.clone());
                        paths.insert(dependent.clone(), path);
                        queue.push_back(dependent.clone());
                    }
                }
            }
        }

        let ordered_revalidation = self.topological_order(&affected)?;
        Ok(ImpactReport {
            root: root.to_string(),
            affected: affected.into_iter().collect(),
            ordered_revalidation,
            paths,
        })
    }

    /// Produce dependency-safe lifecycle work for a bounded target set.
    pub fn plan_transitions(
        &self, targets: &BTreeMap<String, LifecycleState>,
    ) -> Result<Vec<TransitionStep>> {
        let selected: BTreeSet<String> = targets.keys().cloned().collect();
        let order = self.topological_order(&selected)?;
        let mut steps = Vec::with_capacity(order.len());

        for asset_id in order {
            let asset = self.asset(&asset_id)?;
            let target = targets
                .get(&asset_id)
                .copied()
                .ok_or_else(|| ArchitectureError::UnknownAsset(asset_id.clone()))?;
            if !asset.lifecycle.allows(target) {
                return Err(ArchitectureError::InvalidTransition {
                    asset_id: asset_id.clone(),
                    from: asset.lifecycle.to_string(),
                    to: target.to_string(),
                });
            }
            steps.push(TransitionStep {
                asset_id,
                from: asset.lifecycle,
                to: target,
                prerequisites: asset.dependencies.iter().cloned().collect(),
            });
        }

        Ok(steps)
    }

    /// Validate graph closure, lifecycle use, and replacement references.
    #[must_use]
    pub fn validate(&self) -> Vec<RegistryViolation> {
        let mut violations = Vec::new();

        for (key, asset) in &self.assets {
            if key != &asset.id {
                violations.push(RegistryViolation {
                    code: "EA-REG-001".to_string(),
                    severity: Severity::Error,
                    asset_id: key.clone(),
                    message: format!(
                        "registry key `{key}` does not match embedded identifier `{}`",
                        asset.id
                    ),
                });
            }

            for dependency_id in &asset.dependencies {
                match self.assets.get(dependency_id) {
                    None => violations.push(RegistryViolation {
                        code: "EA-REG-002".to_string(),
                        severity: Severity::Error,
                        asset_id: asset.id.clone(),
                        message: format!("dependency `{dependency_id}` is not registered"),
                    }),
                    Some(dependency)
                        if asset.lifecycle == LifecycleState::Active
                            && matches!(
                                dependency.lifecycle,
                                LifecycleState::Retired | LifecycleState::Archived
                            ) =>
                    {
                        violations.push(RegistryViolation {
                            code: "EA-REG-003".to_string(),
                            severity: Severity::Error,
                            asset_id: asset.id.clone(),
                            message: format!(
                                "active asset depends on {} dependency `{dependency_id}`",
                                dependency.lifecycle
                            ),
                        });
                    }
                    Some(_) => {}
                }
            }

            for replacement_id in &asset.replaces {
                if !self.assets.contains_key(replacement_id) {
                    violations.push(RegistryViolation {
                        code: "EA-REG-004".to_string(),
                        severity: Severity::Warning,
                        asset_id: asset.id.clone(),
                        message: format!("replacement target `{replacement_id}` is not registered"),
                    });
                }
            }
        }

        let all: BTreeSet<String> = self.assets.keys().cloned().collect();
        if let Err(ArchitectureError::DependencyCycle(cycle)) = self.topological_order(&all) {
            violations.push(RegistryViolation {
                code: "EA-REG-005".to_string(),
                severity: Severity::Error,
                asset_id: "registry".to_string(),
                message: format!("dependency cycle detected among: {cycle}"),
            });
        }

        violations.sort_by(|left, right| {
            (&left.severity, &left.code, &left.asset_id).cmp(&(
                &right.severity,
                &right.code,
                &right.asset_id,
            ))
        });
        violations
    }

    fn topological_order(&self, selected: &BTreeSet<String>) -> Result<Vec<String>> {
        for asset_id in selected {
            self.asset(asset_id)?;
        }

        let mut indegree: BTreeMap<String, usize> =
            selected.iter().cloned().map(|id| (id, 0)).collect();
        let mut reverse = BTreeMap::<String, Vec<String>>::new();

        for asset_id in selected {
            let asset = self.asset(asset_id)?;
            for dependency_id in &asset.dependencies {
                if !selected.contains(dependency_id) {
                    continue;
                }
                if let Some(value) = indegree.get_mut(asset_id) {
                    *value = value.saturating_add(1);
                }
                reverse
                    .entry(dependency_id.clone())
                    .or_default()
                    .push(asset_id.clone());
            }
        }

        for dependents in reverse.values_mut() {
            dependents.sort();
        }

        let mut ready: BTreeSet<String> = indegree
            .iter()
            .filter_map(|(asset_id, degree)| (*degree == 0).then_some(asset_id.clone()))
            .collect();
        let mut order = Vec::with_capacity(selected.len());

        while let Some(next) = ready.pop_first() {
            order.push(next.clone());
            if let Some(dependents) = reverse.get(&next) {
                for dependent in dependents {
                    if let Some(value) = indegree.get_mut(dependent) {
                        *value = value.saturating_sub(1);
                        if *value == 0 {
                            ready.insert(dependent.clone());
                        }
                    }
                }
            }
        }

        if order.len() != selected.len() {
            let remaining = indegree
                .into_iter()
                .filter_map(|(asset_id, degree)| (degree > 0).then_some(asset_id))
                .collect::<Vec<_>>()
                .join(", ");
            return Err(ArchitectureError::DependencyCycle(remaining));
        }

        Ok(order)
    }
}
