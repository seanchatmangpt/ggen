//! Ownership classes for conflict detection.
//!
//! CISO requirement: All emit targets and protocol-visible fields must declare
//! ownership class. Undeclared overlap is a hard failure.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// Result of a merge strategy execution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MergeResult {
    /// The merged content.
    pub content: String,
    /// Whether the merge was a conflict that required resolution.
    pub had_conflict: bool,
    /// Human-readable description of what happened during the merge.
    pub description: String,
}

/// Error returned when a merge strategy cannot be applied.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MergeError {
    /// Semver conflict between versions.
    SemVerConflict(String),
    /// Invalid SPARQL query.
    InvalidSparqlQuery(String),
    /// Conflict detected between contents.
    ConflictDetected(String),
    /// Content parse error.
    ParseError(String),
}

impl std::fmt::Display for MergeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SemVerConflict(msg) => write!(f, "semver conflict: {msg}"),
            Self::InvalidSparqlQuery(msg) => write!(f, "invalid SPARQL query: {msg}"),
            Self::ConflictDetected(msg) => write!(f, "conflict detected: {msg}"),
            Self::ParseError(msg) => write!(f, "parse error: {msg}"),
        }
    }
}

impl std::error::Error for MergeError {}

/// Ownership class for artifacts/fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OwnershipClass {
    Exclusive,
    Mergeable,
    Overlay,
    ForbiddenOverlap,
}

impl OwnershipClass {
    #[must_use]
    pub const fn allows_overlap(&self) -> bool {
        matches!(self, Self::Mergeable | Self::Overlay)
    }

    #[must_use]
    pub const fn requires_merge_strategy(&self) -> bool {
        matches!(self, Self::Mergeable)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OwnershipTarget {
    FilePath(PathBuf),
    RdfNamespace(String),
    ProtocolField(String),
    TemplateVariable(String),
    DependencyPackage(String),
    FeatureFlag(String),
}

impl OwnershipTarget {
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MergeStrategy {
    Concat,
    LastWriterWins,
    FirstWriterWins,
    Recursive,
    CustomSparql { query: String },
    FailOnConflict,
}

pub fn execute_merge_strategy(
    content_a: &str,
    content_b: &str,
    strategy: &MergeStrategy,
) -> Result<MergeResult, MergeError> {
    match strategy {
        MergeStrategy::Concat => merge_concat(content_a, content_b),
        MergeStrategy::LastWriterWins => merge_last_writer_wins(content_a, content_b),
        MergeStrategy::FirstWriterWins => merge_first_writer_wins(content_a, content_b),
        MergeStrategy::Recursive => merge_recursive(content_a, content_b),
        MergeStrategy::CustomSparql { query } => merge_custom_sparql(content_a, content_b, query),
        MergeStrategy::FailOnConflict => merge_fail_on_conflict(content_a, content_b),
    }
}

fn merge_concat(content_a: &str, content_b: &str) -> Result<MergeResult, MergeError> {
    if content_a.is_empty() {
        return Ok(MergeResult { content: content_b.to_string(), had_conflict: false, description: "concat: content_a was empty, returned content_b".into() });
    }
    if content_b.is_empty() {
        return Ok(MergeResult { content: content_a.to_string(), had_conflict: false, description: "concat: content_b was empty, returned content_a".into() });
    }
    let merged = format!("{content_a}\n{content_b}");
    Ok(MergeResult { content: merged, had_conflict: false, description: format!("concat: joined {} bytes + {} bytes", content_a.len(), content_b.len()) })
}

fn merge_last_writer_wins(content_a: &str, content_b: &str) -> Result<MergeResult, MergeError> {
    let had_conflict = content_a != content_b;
    Ok(MergeResult { content: content_b.to_string(), had_conflict, description: if had_conflict { "last_writer_wins: content_b overrode content_a".into() } else { "last_writer_wins: contents identical, no override".into() } })
}

fn merge_first_writer_wins(content_a: &str, content_b: &str) -> Result<MergeResult, MergeError> {
    let had_conflict = content_a != content_b;
    Ok(MergeResult { content: content_a.to_string(), had_conflict, description: if had_conflict { "first_writer_wins: content_a preserved, content_b ignored".into() } else { "first_writer_wins: contents identical, no conflict".into() } })
}

fn merge_recursive(content_a: &str, content_b: &str) -> Result<MergeResult, MergeError> {
    let a_trimmed = content_a.trim();
    let b_trimmed = content_b.trim();
    if let (Some(ver_a), Some(ver_b)) = (semver::Version::parse(a_trimmed).ok(), semver::Version::parse(b_trimmed).ok()) {
        return merge_semver(&ver_a, &ver_b);
    }
    let lines_a: Vec<&str> = content_a.lines().collect();
    let lines_b: Vec<&str> = content_b.lines().collect();
    let mut merged_lines = Vec::with_capacity(lines_a.len() + lines_b.len());
    let mut conflicts = 0;
    let mut seen: HashMap<&str, bool> = HashMap::new();
    for line in &lines_a { if !seen.contains_key(line) { merged_lines.push(*line); seen.insert(line, true); } }
    for line in &lines_b { if seen.contains_key(line) { conflicts += 1; } else { merged_lines.push(*line); seen.insert(line, true); } }
    let content = merged_lines.join("\n");
    let had_conflict = conflicts > 0;
    Ok(MergeResult { content, had_conflict, description: if had_conflict { format!("recursive: merged with {} duplicate line(s) deduplicated", conflicts) } else { "recursive: merged without conflicts".into() } })
}

fn merge_semver(ver_a: &semver::Version, ver_b: &semver::Version) -> Result<MergeResult, MergeError> {
    let chosen = if ver_a > ver_b { ver_a } else if ver_b > ver_a { ver_b } else {
        return Ok(MergeResult { content: ver_a.to_string(), had_conflict: false, description: "semver: versions identical".into() });
    };
    let had_conflict = ver_a != ver_b;
    Ok(MergeResult { content: chosen.to_string(), had_conflict, description: if had_conflict { format!("semver: selected {chosen} (higher of {ver_a} and {ver_b})") } else { "semver: versions identical".into() } })
}

fn merge_custom_sparql(content_a: &str, content_b: &str, query: &str) -> Result<MergeResult, MergeError> {
    let query_upper = query.to_uppercase();
    if !query_upper.contains("SELECT") && !query_upper.contains("CONSTRUCT") && !query_upper.contains("ASK") && !query_upper.contains("DESCRIBE") {
        return Err(MergeError::InvalidSparqlQuery(format!("query must contain a SPARQL keyword (SELECT, CONSTRUCT, ASK, DESCRIBE): {query}")));
    }
    if !query_upper.contains("WHERE") && !query.contains('{') {
        return Err(MergeError::InvalidSparqlQuery("query must contain WHERE clause or graph pattern".into()));
    }
    let had_conflict = content_a != content_b;
    Ok(MergeResult { content: if had_conflict { format!("{content_a}\n{content_b}") } else { content_a.to_string() }, had_conflict, description: format!("custom_sparql: query validated, merged {} bytes", if had_conflict { content_a.len() + content_b.len() + 1 } else { content_a.len() }) })
}

fn merge_fail_on_conflict(content_a: &str, content_b: &str) -> Result<MergeResult, MergeError> {
    if content_a == content_b {
        return Ok(MergeResult { content: content_a.to_string(), had_conflict: false, description: "fail_on_conflict: contents identical, no conflict".into() });
    }
    Err(MergeError::ConflictDetected(format!("contents differ:\n  content_a ({} bytes): {}\n  content_b ({} bytes): {}", content_a.len(), truncate_preview(content_a, 80), content_b.len(), truncate_preview(content_b, 80))))
}

fn truncate_preview(s: &str, max_len: usize) -> String {
    if s.len() <= max_len { s.to_string() } else { format!("{}...", &s[..max_len]) }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OwnershipDeclaration {
    pub target: OwnershipTarget,
    pub class: OwnershipClass,
    pub owner_pack: String,
    pub merge_strategy: Option<MergeStrategy>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, String>>,
}

impl OwnershipDeclaration {
    #[must_use]
    pub fn new(target: OwnershipTarget, class: OwnershipClass, owner_pack: String) -> Self { Self { target, class, owner_pack, merge_strategy: None, metadata: None } }
    #[must_use]
    pub fn exclusive(target: OwnershipTarget, owner_pack: String) -> Self { Self::new(target, OwnershipClass::Exclusive, owner_pack) }
    #[must_use]
    pub fn mergeable(target: OwnershipTarget, owner_pack: String, merge_strategy: MergeStrategy) -> Self { Self { target, class: OwnershipClass::Mergeable, owner_pack, merge_strategy: Some(merge_strategy), metadata: None } }
    #[must_use]
    pub fn forbidden(target: OwnershipTarget, owner_pack: String) -> Self { Self::new(target, OwnershipClass::ForbiddenOverlap, owner_pack) }

    #[must_use]
    pub fn conflicts_with(&self, other: &Self) -> Option<String> {
        if self.target != other.target { return None; }
        if self.owner_pack == other.owner_pack { return None; }
        match (&self.class, &other.class) {
            (OwnershipClass::Exclusive, OwnershipClass::Exclusive) => Some(format!("Exclusive overlap: {:?} owned by both {} and {}", self.target, self.owner_pack, other.owner_pack)),
            (OwnershipClass::ForbiddenOverlap, _) | (_, OwnershipClass::ForbiddenOverlap) => Some(format!("Forbidden overlap: {:?} between {} and {}", self.target, self.owner_pack, other.owner_pack)),
            (OwnershipClass::Mergeable, OwnershipClass::Mergeable) => match (&self.merge_strategy, &other.merge_strategy) { (Some(a), Some(b)) if a != b => Some(format!("Incompatible merge strategies for {:?}: {} wants {:?}, {} wants {:?}", self.target, self.owner_pack, a, other.owner_pack, b)), _ => None },
            (OwnershipClass::Overlay, OwnershipClass::Overlay) => Some(format!("Overlay conflict: {:?} requires transfer between {} and {}", self.target, self.owner_pack, other.owner_pack)),
            (OwnershipClass::Overlay, OwnershipClass::Mergeable) | (OwnershipClass::Mergeable, OwnershipClass::Overlay) => Some(format!("Incompatible ownership classes for {:?}: {} is {:?}, {} is {:?}", self.target, self.owner_pack, self.class, other.owner_pack, other.class)),
            (OwnershipClass::Exclusive, _) | (_, OwnershipClass::Exclusive) => Some(format!("Exclusive violation: {:?} owned by {}, {} claims {:?}", self.target, self.owner_pack, other.owner_pack, other.class)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct OwnershipMap { pub declarations: HashMap<String, Vec<OwnershipDeclaration>> }

impl OwnershipMap {
    #[must_use]
    pub fn new() -> Self { Self { declarations: HashMap::new() } }
    pub fn add(&mut self, declaration: OwnershipDeclaration) -> Result<(), String> { let key = declaration.target.key(); self.declarations.entry(key).or_default().push(declaration); Ok(()) }
    #[must_use]
    pub fn is_owned(&self, target: &OwnershipTarget) -> bool { self.declarations.contains_key(&target.key()) }
    #[must_use]
    pub fn get_declarations(&self, target: &OwnershipTarget) -> Option<&[OwnershipDeclaration]> { self.declarations.get(&target.key()).map(|v| v.as_slice()) }
    #[must_use]
    pub fn check_conflicts(&self) -> Vec<String> { let mut conflicts = Vec::new(); for (_key, declarations) in &self.declarations { for (i, decl1) in declarations.iter().enumerate() { for decl2 in declarations.iter().skip(i + 1) { if let Some(conflict) = decl1.conflicts_with(decl2) { conflicts.push(conflict); } } } } conflicts }
    pub fn merge(&mut self, other: OwnershipMap) -> Result<(), String> { for (_key, declarations) in other.declarations { for decl in declarations { self.add(decl)?; } } Ok(()) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_ownership_class_allows_overlap() { assert!(!OwnershipClass::Exclusive.allows_overlap()); assert!(OwnershipClass::Mergeable.allows_overlap()); assert!(OwnershipClass::Overlay.allows_overlap()); assert!(!OwnershipClass::ForbiddenOverlap.allows_overlap()); }
    #[test] fn test_ownership_class_requires_merge_strategy() { assert!(OwnershipClass::Mergeable.requires_merge_strategy()); assert!(!OwnershipClass::Exclusive.requires_merge_strategy()); }

    #[test] fn test_concat_basic() { let r = execute_merge_strategy("line1", "line2", &MergeStrategy::Concat).unwrap(); assert_eq!(r.content, "line1\nline2"); assert!(!r.had_conflict); }
    #[test] fn test_concat_empty_a() { let r = execute_merge_strategy("", "content", &MergeStrategy::Concat).unwrap(); assert_eq!(r.content, "content"); }
    #[test] fn test_concat_empty_b() { let r = execute_merge_strategy("content", "", &MergeStrategy::Concat).unwrap(); assert_eq!(r.content, "content"); }
    #[test] fn test_concat_both_empty() { let r = execute_merge_strategy("", "", &MergeStrategy::Concat).unwrap(); assert_eq!(r.content, ""); }

    #[test] fn test_last_writer_wins_basic() { let r = execute_merge_strategy("old", "new", &MergeStrategy::LastWriterWins).unwrap(); assert_eq!(r.content, "new"); assert!(r.had_conflict); }
    #[test] fn test_last_writer_wins_identical() { let r = execute_merge_strategy("same", "same", &MergeStrategy::LastWriterWins).unwrap(); assert!(!r.had_conflict); }

    #[test] fn test_first_writer_wins_basic() { let r = execute_merge_strategy("first", "second", &MergeStrategy::FirstWriterWins).unwrap(); assert_eq!(r.content, "first"); assert!(r.had_conflict); }
    #[test] fn test_first_writer_wins_identical() { let r = execute_merge_strategy("same", "same", &MergeStrategy::FirstWriterWins).unwrap(); assert!(!r.had_conflict); }

    #[test] fn test_recursive_semver_higher_b() { let r = execute_merge_strategy("1.0.0", "2.0.0", &MergeStrategy::Recursive).unwrap(); assert_eq!(r.content, "2.0.0"); assert!(r.had_conflict); }
    #[test] fn test_recursive_semver_identical() { let r = execute_merge_strategy("1.2.3", "1.2.3", &MergeStrategy::Recursive).unwrap(); assert_eq!(r.content, "1.2.3"); assert!(!r.had_conflict); }
    #[test] fn test_recursive_semver_prerelease() { let r = execute_merge_strategy("1.0.0-alpha", "1.0.0", &MergeStrategy::Recursive).unwrap(); assert_eq!(r.content, "1.0.0"); }
    #[test] fn test_recursive_line_by_line() { let r = execute_merge_strategy("a\nb", "c\nd", &MergeStrategy::Recursive).unwrap(); assert_eq!(r.content, "a\nb\nc\nd"); assert!(!r.had_conflict); }
    #[test] fn test_recursive_deduplication() { let r = execute_merge_strategy("a\nb", "b\nc", &MergeStrategy::Recursive).unwrap(); assert_eq!(r.content, "a\nb\nc"); assert!(r.had_conflict); }

    #[test] fn test_custom_sparql_valid() { let r = execute_merge_strategy("a", "b", &MergeStrategy::CustomSparql { query: "SELECT ?x WHERE { ?x a :Thing }".into() }).unwrap(); assert!(r.had_conflict); }
    #[test] fn test_custom_sparql_invalid() { let r = execute_merge_strategy("a", "b", &MergeStrategy::CustomSparql { query: "GARBAGE".into() }); assert!(matches!(r.unwrap_err(), MergeError::InvalidSparqlQuery(_))); }

    #[test] fn test_fail_on_conflict_identical() { let r = execute_merge_strategy("same", "same", &MergeStrategy::FailOnConflict).unwrap(); assert!(!r.had_conflict); }
    #[test] fn test_fail_on_conflict_differs() { let r = execute_merge_strategy("a", "b", &MergeStrategy::FailOnConflict); assert!(matches!(r.unwrap_err(), MergeError::ConflictDetected(_))); }

    #[test] fn test_merge_error_display() { assert!(MergeError::ConflictDetected("x".into()).to_string().contains("conflict detected")); }
    #[test] fn test_merge_error_is_error() { let _: Box<dyn std::error::Error> = Box::new(MergeError::ConflictDetected("test".into())); }
    #[test] fn test_merge_result_serialization() { let r = MergeResult { content: "m".into(), had_conflict: true, description: "d".into() }; let j = serde_json::to_string(&r).unwrap(); assert_eq!(serde_json::from_str::<MergeResult>(&j).unwrap(), r); }
    #[test] fn test_merge_strategy_serialization() { for s in [MergeStrategy::Concat, MergeStrategy::LastWriterWins, MergeStrategy::FirstWriterWins, MergeStrategy::Recursive, MergeStrategy::FailOnConflict] { let j = serde_json::to_string(&s).unwrap(); assert_eq!(serde_json::from_str::<MergeStrategy>(&j).unwrap(), s); } }
    #[test] fn test_truncate_preview() { assert_eq!(truncate_preview("hello", 10), "hello"); assert!(truncate_preview(&"a".repeat(100), 80).ends_with("...")); }

    #[test] fn test_exclusive_conflicts() { let t = OwnershipTarget::FilePath(PathBuf::from("/x")); let d1 = OwnershipDeclaration::exclusive(t.clone(), "a".into()); let d2 = OwnershipDeclaration::exclusive(t, "b".into()); assert!(d1.conflicts_with(&d2).is_some()); }
    #[test] fn test_ownership_map_add() { let mut m = OwnershipMap::new(); m.add(OwnershipDeclaration::exclusive(OwnershipTarget::FilePath(PathBuf::from("/x")), "a".into())).unwrap(); assert!(m.is_owned(&OwnershipTarget::FilePath(PathBuf::from("/x")))); }
    #[test] fn test_ownership_map_conflict_detection() { let mut m = OwnershipMap::new(); let t = OwnershipTarget::FilePath(PathBuf::from("/x")); m.add(OwnershipDeclaration::exclusive(t.clone(), "a".into())).unwrap(); m.add(OwnershipDeclaration::exclusive(t, "b".into())).unwrap(); assert_eq!(m.check_conflicts().len(), 1); }
}
