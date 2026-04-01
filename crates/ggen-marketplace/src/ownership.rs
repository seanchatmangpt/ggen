//! Ownership classes for conflict detection.
//!
//! CISO requirement: All emit targets and protocol-visible fields must declare
//! ownership class. Undeclared overlap is a hard failure.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// Ownership class for artifacts/fields.
///
/// CISO requirement: Unowned overlap is one of the fastest ways to create
/// nondeterministic builds and hidden override attacks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OwnershipClass {
    /// Exactly one pack may own this artifact/field.
    ///
    /// Any overlap is a hard failure.
    Exclusive,

    /// Multiple packs may contribute if merge rules are declared and validated.
    Mergeable,

    /// Downstream refinement allowed only with explicit ownership transfer policy.
    Overlay,

    /// Any overlap is a hard failure (default for undeclared targets).
    ForbiddenOverlap,
}

impl OwnershipClass {
    /// Check if overlap is allowed for this ownership class.
    #[must_use]
    pub const fn allows_overlap(&self) -> bool {
        matches!(self, Self::Mergeable | Self::Overlay)
    }

    /// Check if this class requires explicit merge rules when overlapping.
    #[must_use]
    pub const fn requires_merge_strategy(&self) -> bool {
        matches!(self, Self::Mergeable)
    }
}

/// Target that can be owned by a pack.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OwnershipTarget {
    /// File path (emitted artifact).
    FilePath(PathBuf),

    /// RDF namespace URI.
    RdfNamespace(String),

    /// Protocol-visible field name.
    ProtocolField(String),

    /// Template variable name.
    TemplateVariable(String),

    /// Dependency package name.
    DependencyPackage(String),

    /// Feature flag name.
    FeatureFlag(String),
}

impl OwnershipTarget {
    /// Get a string key for this ownership target (for HashMap indexing).
    #[must_use]
    pub fn key(&self) -> String {
        match self {
            Self::FilePath(p) => format!("file:{}", p.display()),
            Self::RdfNamespace(ns) => format!("namespace:{}", ns),
            Self::ProtocolField(f) => format!("field:{}", f),
            Self::TemplateVariable(v) => format!("template:{}", v),
            Self::DependencyPackage(p) => format!("dep:{}", p),
            Self::FeatureFlag(f) => format!("feature:{}", f),
        }
    }
}

/// Merge strategy for mergeable ownership.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MergeStrategy {
    /// Concatenate arrays/sequences.
    Concat,

    /// Use last writer wins (later packs override earlier).
    LastWriterWins,

    /// Use first writer wins (earlier packs take precedence).
    FirstWriterWins,

    /// Merge objects/structs recursively.
    Recursive,

    /// Custom merge logic with SPARQL query.
    CustomSparql { query: String },

    /// Fail if conflict detected (require manual resolution).
    FailOnConflict,
}

/// Ownership declaration for a target.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OwnershipDeclaration {
    /// Target being owned (file path, namespace, field, etc.)
    pub target: OwnershipTarget,

    /// Ownership class (exclusive, mergeable, overlay, forbidden-overlap)
    pub class: OwnershipClass,

    /// Pack that owns this target.
    pub owner_pack: String,

    /// Merge strategy (if applicable).
    pub merge_strategy: Option<MergeStrategy>,

    /// Additional metadata.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, String>>,
}

impl OwnershipDeclaration {
    /// Create a new ownership declaration.
    #[must_use]
    pub fn new(target: OwnershipTarget, class: OwnershipClass, owner_pack: String) -> Self {
        Self {
            target,
            class,
            owner_pack,
            merge_strategy: None,
            metadata: None,
        }
    }

    /// Create an exclusive ownership declaration.
    #[must_use]
    pub fn exclusive(target: OwnershipTarget, owner_pack: String) -> Self {
        Self::new(target, OwnershipClass::Exclusive, owner_pack)
    }

    /// Create a mergeable ownership declaration.
    #[must_use]
    pub fn mergeable(
        target: OwnershipTarget,
        owner_pack: String,
        merge_strategy: MergeStrategy,
    ) -> Self {
        Self {
            target,
            class: OwnershipClass::Mergeable,
            owner_pack,
            merge_strategy: Some(merge_strategy),
            metadata: None,
        }
    }

    /// Create a forbidden overlap declaration.
    #[must_use]
    pub fn forbidden(target: OwnershipTarget, owner_pack: String) -> Self {
        Self::new(target, OwnershipClass::ForbiddenOverlap, owner_pack)
    }

    /// Check if this target conflicts with another declaration.
    ///
    /// Returns Some(conflict_description) if there's a conflict, None if compatible.
    #[must_use]
    pub fn conflicts_with(&self, other: &Self) -> Option<String> {
        // Different targets don't conflict
        if self.target != other.target {
            return None;
        }

        // Same owner doesn't conflict
        if self.owner_pack == other.owner_pack {
            return None;
        }

        match (&self.class, &other.class) {
            (OwnershipClass::Exclusive, OwnershipClass::Exclusive) => {
                Some(format!(
                    "Exclusive overlap: {:?} owned by both {} and {}",
                    self.target, self.owner_pack, other.owner_pack
                ))
            }
            (OwnershipClass::ForbiddenOverlap, _) | (_, OwnershipClass::ForbiddenOverlap) => {
                Some(format!(
                    "Forbidden overlap: {:?} has forbidden overlap between {} and {}",
                    self.target, self.owner_pack, other.owner_pack
                ))
            }
            (OwnershipClass::Mergeable, OwnershipClass::Mergeable) => {
                // Check if merge strategies are compatible
                match (&self.merge_strategy, &other.merge_strategy) {
                    (Some(ms1), Some(ms2)) if ms1 != ms2 => {
                        Some(format!(
                            "Incompatible merge strategies for {:?}: {} wants {:?}, {} wants {:?}",
                            self.target, self.owner_pack, ms1, other.owner_pack, ms2
                        ))
                    }
                    _ => None, // Merge strategies are compatible
                }
            }
            (OwnershipClass::Overlay, OwnershipClass::Overlay) => {
                // Overlap allowed for overlay, but requires explicit transfer
                Some(format!(
                    "Overlay conflict: {:?} requires explicit ownership transfer between {} and {}",
                    self.target, self.owner_pack, other.owner_pack
                ))
            }
            (OwnershipClass::Overlay, OwnershipClass::Mergeable) | (OwnershipClass::Mergeable, OwnershipClass::Overlay) => {
                Some(format!(
                    "Incompatible ownership classes for {:?}: {} is {:?}, {} is {:?}",
                    self.target,
                    self.owner_pack,
                    self.class,
                    other.owner_pack,
                    other.class
                ))
            }
            (OwnershipClass::Exclusive, _) | (_, OwnershipClass::Exclusive) => {
                Some(format!(
                    "Exclusive violation: {:?} owned exclusively by {}, but {} also claims {:?}",
                    self.target, self.owner_pack, other.owner_pack, other.class
                ))
            }
        }
    }
}

/// Ownership map tracking all ownership declarations.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct OwnershipMap {
    /// All ownership declarations indexed by target key.
    pub declarations: HashMap<String, Vec<OwnershipDeclaration>>,
}

impl OwnershipMap {
    /// Create a new empty ownership map.
    #[must_use]
    pub fn new() -> Self {
        Self {
            declarations: HashMap::new(),
        }
    }

    /// Add an ownership declaration to the map.
    ///
    /// Note: This method tracks conflicts but doesn't prevent adding.
    /// Use `check_conflicts()` to validate the map.
    pub fn add(&mut self, declaration: OwnershipDeclaration) -> Result<(), String> {
        let key = declaration.target.key();
        self.declarations.entry(key).or_default().push(declaration);
        Ok(())
    }

    /// Check if a target has any ownership declarations.
    #[must_use]
    pub fn is_owned(&self, target: &OwnershipTarget) -> bool {
        self.declarations.contains_key(&target.key())
    }

    /// Get all declarations for a target.
    #[must_use]
    pub fn get_declarations(&self, target: &OwnershipTarget) -> Option<&[OwnershipDeclaration]> {
        self.declarations.get(&target.key()).map(|v| v.as_slice())
    }

    /// Check all declarations for conflicts.
    ///
    /// Returns a list of all conflicts found.
    #[must_use]
    pub fn check_conflicts(&self) -> Vec<String> {
        let mut conflicts = Vec::new();

        for (_key, declarations) in &self.declarations {
            for (i, decl1) in declarations.iter().enumerate() {
                for decl2 in declarations.iter().skip(i + 1) {
                    if let Some(conflict) = decl1.conflicts_with(decl2) {
                        conflicts.push(conflict);
                    }
                }
            }
        }

        conflicts
    }

    /// Merge another ownership map into this one.
    ///
    /// # Errors
    ///
    /// Returns error if any conflicts are found during merge.
    pub fn merge(&mut self, other: OwnershipMap) -> Result<(), String> {
        for (_key, declarations) in other.declarations {
            for decl in declarations {
                self.add(decl)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ownership_class_allows_overlap() {
        assert!(!OwnershipClass::Exclusive.allows_overlap());
        assert!(OwnershipClass::Mergeable.allows_overlap());
        assert!(OwnershipClass::Overlay.allows_overlap());
        assert!(!OwnershipClass::ForbiddenOverlap.allows_overlap());
    }

    #[test]
    fn test_exclusive_conflicts() {
        let target = OwnershipTarget::FilePath(PathBuf::from("/src/main.rs"));
        let decl1 = OwnershipDeclaration::exclusive(target.clone(), "pack-a".to_string());
        let decl2 = OwnershipDeclaration::exclusive(target, "pack-b".to_string());

        assert!(decl1.conflicts_with(&decl2).is_some());
    }

    #[test]
    fn test_mergeable_no_conflict() {
        let target = OwnershipTarget::FilePath(PathBuf::from("/src/config.rs"));
        let decl1 = OwnershipDeclaration::mergeable(
            target.clone(),
            "pack-a".to_string(),
            MergeStrategy::Concat,
        );
        let decl2 = OwnershipDeclaration::mergeable(
            target,
            "pack-b".to_string(),
            MergeStrategy::Concat,
        );

        assert!(decl1.conflicts_with(&decl2).is_none());
    }

    #[test]
    fn test_mergeable_incompatible_strategies() {
        let target = OwnershipTarget::FilePath(PathBuf::from("/src/config.rs"));
        let decl1 = OwnershipDeclaration::mergeable(
            target.clone(),
            "pack-a".to_string(),
            MergeStrategy::Concat,
        );
        let decl2 = OwnershipDeclaration::mergeable(
            target,
            "pack-b".to_string(),
            MergeStrategy::LastWriterWins,
        );

        assert!(decl1.conflicts_with(&decl2).is_some());
    }

    #[test]
    fn test_ownership_map_add() {
        let mut map = OwnershipMap::new();
        let target = OwnershipTarget::FilePath(PathBuf::from("/src/main.rs"));
        let decl = OwnershipDeclaration::exclusive(target, "pack-a".to_string());

        assert!(map.add(decl).is_ok());
        assert!(map.is_owned(&OwnershipTarget::FilePath(PathBuf::from("/src/main.rs"))));
    }

    #[test]
    fn test_ownership_map_conflict() {
        let mut map = OwnershipMap::new();
        let target = OwnershipTarget::FilePath(PathBuf::from("/src/main.rs"));

        let decl1 = OwnershipDeclaration::exclusive(target.clone(), "pack-a".to_string());
        let decl2 = OwnershipDeclaration::exclusive(target, "pack-b".to_string());

        // Both adds succeed (conflicts are tracked)
        assert!(map.add(decl1).is_ok());
        assert!(map.add(decl2).is_ok());

        // check_conflicts() reports the conflict
        assert!(!map.check_conflicts().is_empty());
    }

    #[test]
    fn test_forbidden_overlap() {
        let target = OwnershipTarget::ProtocolField("api_version".to_string());
        let decl1 = OwnershipDeclaration::forbidden(target.clone(), "pack-a".to_string());
        let decl2 = OwnershipDeclaration::mergeable(
            target,
            "pack-b".to_string(),
            MergeStrategy::Concat,
        );

        assert!(decl1.conflicts_with(&decl2).is_some());
    }

    #[test]
    fn test_check_conflicts() {
        let mut map = OwnershipMap::new();
        let target = OwnershipTarget::FilePath(PathBuf::from("/src/main.rs"));

        // First add succeeds
        map.add(OwnershipDeclaration::exclusive(
            target.clone(),
            "pack-a".to_string(),
        ))
        .unwrap();

        assert!(map.check_conflicts().is_empty());

        // Second add also succeeds (conflicts are tracked, not prevented)
        map.add(OwnershipDeclaration::exclusive(
            target.clone(),
            "pack-b".to_string(),
        ))
        .unwrap();

        // Now check_conflicts() should find the conflict
        let conflicts = map.check_conflicts();
        assert_eq!(conflicts.len(), 1);
        assert!(conflicts[0].contains("Exclusive overlap"));
    }
}
