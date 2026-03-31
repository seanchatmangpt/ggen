//! Cycle detection and fixing for RDF ontology imports
//!
//! This module provides utilities to detect circular dependencies in ontology
//! import graphs and apply automated fix strategies.
//!
//! # Fix Strategies
//!
//! - `remove_import`: Remove problematic import statements
//! - `merge_files`: Merge cyclic files into a single ontology
//! - `create_interface`: Extract shared definitions into interface file
//!
//! # Examples
//!
//! ```ignore
//! use ggen_core::graph::cycle_fixer::{CycleFixer, FixStrategy, FixReport};
//!
//! let fixer = CycleFixer::new("/path/to/ontology");
//! let report = fixer.detect_and_fix(FixStrategy::RemoveImport, false)?;
//!
//! println!("Cycles found: {}", report.cycles_found);
//! println!("Fixes applied: {}", report.fixes_applied);
//! ```

use crate::graph::cycle_detection::detect_cycles;
use ggen_utils::error::{Error, Result};
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::str::FromStr;

/// Strategy for fixing circular dependencies
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum FixStrategy {
    /// Remove the problematic import statement
    RemoveImport,

    /// Merge all files in the cycle into a single ontology
    MergeFiles,

    /// Extract shared definitions into a separate interface file
    CreateInterface,
}

impl FromStr for FixStrategy {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "remove_import" => Ok(FixStrategy::RemoveImport),
            "merge_files" => Ok(FixStrategy::MergeFiles),
            "create_interface" => Ok(FixStrategy::CreateInterface),
            _ => Err(format!("Invalid fix strategy: {}", s)),
        }
    }
}

/// Report of cycle detection and fixing operations
#[derive(Debug, Clone, Serialize)]
pub struct FixReport {
    /// Number of cycles detected
    pub cycles_found: usize,

    /// Number of fixes applied
    pub fixes_applied: usize,

    /// Files that were modified
    pub files_modified: Vec<String>,

    /// Path to backup directory (if backups were created)
    pub backup_path: Option<String>,

    /// Detailed cycle information
    pub cycles: Vec<CycleInfo>,
}

/// Information about a detected cycle
#[derive(Debug, Clone, Serialize)]
pub struct CycleInfo {
    /// Files involved in the cycle (in order)
    pub files: Vec<String>,

    /// Strategy used to fix this cycle
    pub fix_strategy: Option<FixStrategy>,

    /// Whether the fix was successful
    pub fixed: bool,
}

/// Cycle detection and fixing engine
pub struct CycleFixer {
    /// Base directory for ontologies
    base_dir: PathBuf,

    /// Backup directory path
    backup_dir: PathBuf,
}

impl CycleFixer {
    /// Create a new cycle fixer
    pub fn new(base_dir: impl AsRef<Path>) -> Self {
        let base_dir = base_dir.as_ref();
        let backup_dir = base_dir.join(".ggen").join("backups");

        Self {
            base_dir: base_dir.to_path_buf(),
            backup_dir,
        }
    }

    /// Detect cycles and optionally fix them
    pub fn detect_and_fix(&self, strategy: FixStrategy, dry_run: bool) -> Result<FixReport> {
        // Step 1: Build import graph from TTL files
        let import_graph = self.build_import_graph()?;

        // Step 2: Detect cycles
        let cycles = detect_cycles(&import_graph);
        let cycles_info: Vec<CycleInfo> = cycles
            .iter()
            .map(|cycle| CycleInfo {
                files: cycle.clone(),
                fix_strategy: None,
                fixed: false,
            })
            .collect();

        if cycles.is_empty() {
            return Ok(FixReport {
                cycles_found: 0,
                fixes_applied: 0,
                files_modified: vec![],
                backup_path: None,
                cycles: cycles_info,
            });
        }

        if dry_run {
            return Ok(FixReport {
                cycles_found: cycles.len(),
                fixes_applied: 0,
                files_modified: vec![],
                backup_path: None,
                cycles: cycles_info,
            });
        }

        // Step 3: Create backup
        let backup_path = self.create_backup()?;

        // Step 4: Apply fixes
        let mut files_modified = HashSet::new();
        let mut fixed_cycles = Vec::new();

        for cycle in &cycles {
            match self.fix_cycle(cycle, strategy, &mut files_modified) {
                Ok(fixed) => {
                    fixed_cycles.push(CycleInfo {
                        files: cycle.clone(),
                        fix_strategy: Some(strategy),
                        fixed,
                    });
                }
                Err(e) => {
                    fixed_cycles.push(CycleInfo {
                        files: cycle.clone(),
                        fix_strategy: Some(strategy),
                        fixed: false,
                    });
                    return Err(Error::new(&format!(
                        "Failed to fix cycle {:?}: {}",
                        cycle, e
                    )));
                }
            }
        }

        let fixes_applied = fixed_cycles.iter().filter(|c| c.fixed).count();

        Ok(FixReport {
            cycles_found: cycles.len(),
            fixes_applied,
            files_modified: files_modified.into_iter().collect(),
            backup_path: Some(backup_path),
            cycles: fixed_cycles,
        })
    }

    /// Build import graph by parsing TTL files for owl:imports
    fn build_import_graph(&self) -> Result<HashMap<String, Vec<String>>> {
        let mut graph = HashMap::new();

        // Find all .ttl files in base directory
        let ttl_files = self.find_ttl_files()?;

        for ttl_file in &ttl_files {
            let file_name = ttl_file
                .strip_prefix(&self.base_dir)
                .unwrap_or(ttl_file)
                .to_string_lossy()
                .to_string();

            // Parse file for owl:imports statements
            let imports = self.extract_imports(ttl_file)?;
            graph.insert(file_name, imports);
        }

        Ok(graph)
    }

    /// Find all .ttl files in the base directory
    fn find_ttl_files(&self) -> Result<Vec<PathBuf>> {
        let mut ttl_files = Vec::new();

        if !self.base_dir.exists() {
            return Ok(ttl_files);
        }

        self.visit_dir(&self.base_dir, &mut ttl_files)?;

        Ok(ttl_files)
    }

    /// Recursively visit directory to find .ttl files
    fn visit_dir(&self, dir: &Path, ttl_files: &mut Vec<PathBuf>) -> Result<()> {
        let entries = fs::read_dir(dir).map_err(|e| {
            Error::new(&format!(
                "Failed to read directory {}: {}",
                dir.display(),
                e
            ))
        })?;

        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                self.visit_dir(&path, ttl_files)?;
            } else if path.extension().and_then(|e| e.to_str()) == Some("ttl") {
                ttl_files.push(path);
            }
        }

        Ok(())
    }

    /// Extract owl:imports statements from a TTL file
    fn extract_imports(&self, ttl_file: &Path) -> Result<Vec<String>> {
        let content = fs::read_to_string(ttl_file)
            .map_err(|e| Error::new(&format!("Failed to read {}: {}", ttl_file.display(), e)))?;

        let mut imports = Vec::new();

        // Parse owl:imports statements
        // Format: <file> owl:imports <otherfile> .
        // or: :ontology owl:imports <otherfile> .
        for line in content.lines() {
            let line = line.trim();

            // Skip comments
            if line.starts_with('#') {
                continue;
            }

            // Look for owl:imports pattern and extract URI after it
            if let Some(imports_pos) = line.find("owl:imports") {
                let after_imports = &line[imports_pos + "owl:imports".len()..];
                // Find the opening < after owl:imports
                if let Some(uri_start) = after_imports.find('<') {
                    if let Some(uri_end) = after_imports[uri_start..].find('>') {
                        let import_path = &after_imports[uri_start + 1..uri_start + uri_end];
                        // Remove any trailing whitespace
                        let clean_path = import_path.trim();
                        imports.push(clean_path.to_string());
                    }
                }
            }
        }

        Ok(imports)
    }

    /// Fix a specific cycle using the given strategy
    fn fix_cycle(
        &self, cycle: &[String], strategy: FixStrategy, files_modified: &mut HashSet<String>,
    ) -> Result<bool> {
        match strategy {
            FixStrategy::RemoveImport => self.fix_by_removing_import(cycle, files_modified),
            FixStrategy::MergeFiles => self.fix_by_merging(cycle, files_modified),
            FixStrategy::CreateInterface => self.fix_by_interface(cycle, files_modified),
        }
    }

    /// Fix cycle by removing the last import in the cycle
    fn fix_by_removing_import(
        &self, cycle: &[String], files_modified: &mut HashSet<String>,
    ) -> Result<bool> {
        if cycle.len() < 2 {
            return Ok(false);
        }

        // The cycle format is [A, B, C, A] where A->B->C->A
        // We need to remove the import from the second-to-last file (C) to the last file (A)
        // This breaks the cycle C->A
        let source_file = &cycle[cycle.len() - 2];
        let target_import = &cycle[cycle.len() - 1];

        let source_path = self.base_dir.join(source_file);
        let content = fs::read_to_string(&source_path)
            .map_err(|e| Error::new(&format!("Failed to read {}: {}", source_path.display(), e)))?;

        // Remove the import line
        let new_content = self.remove_import_line(&content, target_import)?;

        fs::write(&source_path, new_content).map_err(|e| {
            Error::new(&format!("Failed to write {}: {}", source_path.display(), e))
        })?;

        files_modified.insert(source_file.clone());
        Ok(true)
    }

    /// Remove an import line from TTL content
    fn remove_import_line(&self, content: &str, import: &str) -> Result<String> {
        let mut new_lines = Vec::new();
        let mut removed = false;

        for line in content.lines() {
            let trimmed = line.trim();

            // Check if this is the import line to remove
            if trimmed.contains("owl:imports") && trimmed.contains(&format!("<{}>", import)) {
                removed = true;
                // Skip this line (add comment instead)
                new_lines.push(format!("# Import removed by cycle fixer: {}", trimmed));
            } else {
                new_lines.push(line.to_string());
            }
        }

        if !removed {
            return Err(Error::new(&format!("Import {} not found in file", import)));
        }

        Ok(new_lines.join("\n"))
    }

    /// Fix cycle by merging all files in the cycle
    fn fix_by_merging(
        &self, cycle: &[String], files_modified: &mut HashSet<String>,
    ) -> Result<bool> {
        if cycle.is_empty() {
            return Ok(false);
        }

        // Create merged file with name based on first file
        let first_file = &cycle[0];
        let merged_path = self.base_dir.join(format!(
            "{}_merged.ttl",
            first_file.trim_end_matches(".ttl")
        ));

        let mut merged_content = String::new();
        merged_content.push_str("# Merged ontology - cycle fix\n\n");

        // Collect all prefixes and base declarations
        let mut prefixes = Vec::new();
        let mut base_declarations = Vec::new();
        let mut statements = Vec::new();

        for file in cycle {
            let file_path = self.base_dir.join(file);
            let content = fs::read_to_string(&file_path).map_err(|e| {
                Error::new(&format!("Failed to read {}: {}", file_path.display(), e))
            })?;

            // Parse content into sections - collect owned strings
            for line in content.lines().map(|l| l.to_string()).collect::<Vec<_>>() {
                let trimmed = line.trim();

                if trimmed.starts_with("@prefix") {
                    prefixes.push(trimmed.to_string());
                } else if trimmed.starts_with("@base") {
                    base_declarations.push(trimmed.to_string());
                } else if !trimmed.is_empty() && !trimmed.starts_with('#') {
                    statements.push(format!("# From: {}\n{}", file, line));
                }
            }
        }

        // Deduplicate and write prefixes
        let mut seen_prefixes = HashSet::new();
        for prefix in &prefixes {
            if seen_prefixes.insert(prefix) {
                merged_content.push_str(prefix);
                merged_content.push('\n');
            }
        }

        // Write base declarations (first one wins)
        if let Some(base) = base_declarations.first() {
            merged_content.push_str(base);
            merged_content.push('\n');
        }

        merged_content.push('\n');

        // Write all statements
        for stmt in &statements {
            merged_content.push_str(stmt);
            merged_content.push('\n');
        }

        // Write merged file
        fs::write(&merged_path, merged_content).map_err(|e| {
            Error::new(&format!("Failed to write {}: {}", merged_path.display(), e))
        })?;

        // Mark all original files for deletion/replacement
        for file in cycle {
            files_modified.insert(file.clone());
        }

        files_modified.insert(merged_path.to_string_lossy().to_string());
        Ok(true)
    }

    /// Fix cycle by creating interface file with shared definitions
    fn fix_by_interface(
        &self, cycle: &[String], files_modified: &mut HashSet<String>,
    ) -> Result<bool> {
        if cycle.is_empty() {
            return Ok(false);
        }

        // Create interface file
        let interface_path = self.base_dir.join("shared_definitions.ttl");

        // Extract common patterns/statements from all files
        let mut common_patterns = HashSet::new();
        let mut file_contents: HashMap<String, String> = HashMap::new();

        for file in cycle {
            let file_path = self.base_dir.join(file);
            let content = fs::read_to_string(&file_path).map_err(|e| {
                Error::new(&format!("Failed to read {}: {}", file_path.display(), e))
            })?;

            // Extract class/property declarations before moving content
            for line in content.lines() {
                let trimmed = line.trim();
                if trimmed.contains("a owl:Class") || trimmed.contains("a rdf:Property") {
                    // Extract the subject
                    if let Some(subject) = trimmed.split_whitespace().next() {
                        common_patterns.insert(subject.to_string());
                    }
                }
            }

            file_contents.insert(file.clone(), content);
        }

        // Create interface file with common definitions
        let mut interface_content = String::new();
        interface_content.push_str("# Shared definitions - cycle fix\n\n");

        // Copy prefixes from first file
        if let Some(first_content) = file_contents.get(&cycle[0]) {
            for line in first_content.lines() {
                if line.trim().starts_with("@prefix") || line.trim().starts_with("@base") {
                    interface_content.push_str(line);
                    interface_content.push('\n');
                }
            }
        }

        interface_content.push('\n');

        // Add common definitions
        for pattern in &common_patterns {
            interface_content.push_str("# Common definition\n");
            // Find and copy the full definition from any file
            for file in cycle {
                if let Some(content) = file_contents.get(file) {
                    for line in content.lines() {
                        if line.trim().starts_with(pattern) {
                            interface_content.push_str(line);
                            interface_content.push('\n');
                            break;
                        }
                    }
                }
            }
        }

        // Write interface file
        fs::write(&interface_path, interface_content).map_err(|e| {
            Error::new(&format!(
                "Failed to write {}: {}",
                interface_path.display(),
                e
            ))
        })?;

        // Update all files to import interface instead of each other
        for file in cycle {
            let file_path = self.base_dir.join(file);
            if let Some(content) = file_contents.get(file) {
                let new_content =
                    self.replace_cycle_imports(content, cycle, "shared_definitions.ttl")?;

                fs::write(&file_path, new_content).map_err(|e| {
                    Error::new(&format!("Failed to write {}: {}", file_path.display(), e))
                })?;

                files_modified.insert(file.clone());
            }
        }

        files_modified.insert(interface_path.to_string_lossy().to_string());
        Ok(true)
    }

    /// Replace cycle imports with interface import
    fn replace_cycle_imports(
        &self, content: &str, cycle: &[String], interface: &str,
    ) -> Result<String> {
        let mut new_lines = Vec::new();
        let mut added_interface_import = false;

        for line in content.lines() {
            let trimmed = line.trim();

            // Check if this is a cycle import
            let is_cycle_import = cycle
                .iter()
                .any(|file| trimmed.contains(&format!("<{}>", file)));

            if is_cycle_import {
                // Replace with interface import (only once)
                if !added_interface_import {
                    new_lines.push(format!("<> owl:imports <{}> .", interface));
                    added_interface_import = true;
                }
                // Comment out the original
                new_lines.push(format!("# Replaced by interface import: {}", line));
            } else {
                new_lines.push(line.to_string());
            }
        }

        Ok(new_lines.join("\n"))
    }

    /// Create backup of all TTL files
    fn create_backup(&self) -> Result<String> {
        let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S");
        let backup_path = self
            .backup_dir
            .join(format!("cycle_fix_backup_{}", timestamp));

        fs::create_dir_all(&backup_path)
            .map_err(|e| Error::new(&format!("Failed to create backup directory: {}", e)))?;

        // Copy all TTL files to backup
        let ttl_files = self.find_ttl_files()?;
        for ttl_file in &ttl_files {
            let file_name = ttl_file.strip_prefix(&self.base_dir).unwrap_or(ttl_file);
            let backup_file = backup_path.join(file_name);

            if let Some(parent) = backup_file.parent() {
                fs::create_dir_all(parent).map_err(|e| {
                    Error::new(&format!("Failed to create backup directory: {}", e))
                })?;
            }

            fs::copy(ttl_file, &backup_file).map_err(|e| {
                Error::new(&format!("Failed to backup {}: {}", ttl_file.display(), e))
            })?;
        }

        Ok(backup_path.to_string_lossy().to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn create_test_ontology(dir: &Path, name: &str, imports: &[&str]) -> Result<()> {
        let path = dir.join(name);
        let mut content = String::new();
        content.push_str("@prefix owl: <http://www.w3.org/2002/07/owl#> .\n");
        content.push_str("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n\n");

        for import in imports {
            content.push_str(&format!("<> owl:imports <{}> .\n", import));
        }

        content.push_str(&format!(
            "\n<{}> a owl:Ontology .\n",
            name.trim_end_matches(".ttl")
        ));

        fs::write(&path, content)
            .map_err(|e| Error::new(&format!("Failed to write test file: {}", e)))?;

        Ok(())
    }

    #[test]
    fn test_extract_imports() {
        let temp_dir = TempDir::new().unwrap();
        let ttl_file = temp_dir.path().join("test.ttl");

        let content = r#"
@prefix owl: <http://www.w3.org/2002/07/owl#> .
<> owl:imports <common.ttl> .
<> owl:imports <base.ttl> .
"#;

        fs::write(&ttl_file, content).unwrap();

        let fixer = CycleFixer::new(temp_dir.path());
        let imports = fixer.extract_imports(&ttl_file).unwrap();

        assert_eq!(imports.len(), 2);
        assert!(imports.contains(&"common.ttl".to_string()));
        assert!(imports.contains(&"base.ttl".to_string()));
    }

    #[test]
    fn test_detect_cycles() {
        let temp_dir = TempDir::new().unwrap();

        // Create cycle: A -> B -> C -> A
        create_test_ontology(temp_dir.path(), "A.ttl", &["B.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "B.ttl", &["C.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "C.ttl", &["A.ttl"]).unwrap();

        let fixer = CycleFixer::new(temp_dir.path());
        let report = fixer
            .detect_and_fix(FixStrategy::RemoveImport, true)
            .unwrap();

        assert_eq!(report.cycles_found, 1);
        assert_eq!(report.fixes_applied, 0);
        assert!(report.backup_path.is_none());
    }

    #[test]
    fn test_fix_by_removing_import() {
        let temp_dir = TempDir::new().unwrap();

        // Create cycle: A -> B -> C -> A
        create_test_ontology(temp_dir.path(), "A.ttl", &["B.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "B.ttl", &["C.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "C.ttl", &["A.ttl"]).unwrap();

        let fixer = CycleFixer::new(temp_dir.path());
        let report = fixer
            .detect_and_fix(FixStrategy::RemoveImport, false)
            .unwrap();

        assert_eq!(report.cycles_found, 1);
        assert_eq!(report.fixes_applied, 1);
        assert_eq!(report.files_modified.len(), 1);
        assert!(report.backup_path.is_some());

        // Verify fix worked by re-detecting
        let report2 = fixer
            .detect_and_fix(FixStrategy::RemoveImport, true)
            .unwrap();
        assert_eq!(report2.cycles_found, 0);
    }

    #[test]
    fn test_no_cycles() {
        let temp_dir = TempDir::new().unwrap();

        // Create DAG: A -> B, A -> C, B -> D, C -> D
        create_test_ontology(temp_dir.path(), "A.ttl", &["B.ttl", "C.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "B.ttl", &["D.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "C.ttl", &["D.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "D.ttl", &[]).unwrap();

        let fixer = CycleFixer::new(temp_dir.path());
        let report = fixer
            .detect_and_fix(FixStrategy::RemoveImport, true)
            .unwrap();

        assert_eq!(report.cycles_found, 0);
        assert_eq!(report.fixes_applied, 0);
    }

    #[test]
    fn test_strategy_from_str() {
        assert_eq!(
            FixStrategy::from_str("remove_import"),
            Ok(FixStrategy::RemoveImport)
        );
        assert_eq!(
            FixStrategy::from_str("merge_files"),
            Ok(FixStrategy::MergeFiles)
        );
        assert_eq!(
            FixStrategy::from_str("create_interface"),
            Ok(FixStrategy::CreateInterface)
        );
        assert!(FixStrategy::from_str("invalid").is_err());
    }
}
