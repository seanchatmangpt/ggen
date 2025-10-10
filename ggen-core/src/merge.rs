//! Three-way merge for delta-driven projection
//!
//! This module provides functionality to:
//! - Merge generated code with manual edits
//! - Preserve manual customizations during regeneration
//! - Handle conflicts between generated and manual content

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::path::{Path, PathBuf};

use crate::snapshot::{FileSnapshot, Region, RegionType};

/// Represents a merge conflict between generated and manual content
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MergeConflict {
    /// Path to the file with conflict
    pub file_path: PathBuf,
    /// Type of conflict
    pub conflict_type: ConflictType,
    /// Description of the conflict
    pub description: String,
    /// Generated content that conflicts
    pub generated: String,
    /// Manual content that conflicts
    pub manual: String,
    /// Baseline content for context
    pub baseline: String,
}

impl MergeConflict {
    /// Create a new conflict
    pub fn new(
        file_path: PathBuf, conflict_type: ConflictType, description: String, generated: String,
        manual: String, baseline: String,
    ) -> Self {
        Self {
            file_path,
            conflict_type,
            description,
            generated,
            manual,
            baseline,
        }
    }
}

/// Types of merge conflicts
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConflictType {
    /// Manual edit overlaps with generated content
    OverlappingEdit,
    /// Manual edit in a region that was regenerated
    RegionConflict,
    /// Structural conflict (e.g., manual edit changes structure)
    StructuralConflict,
}

impl fmt::Display for ConflictType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConflictType::OverlappingEdit => write!(f, "overlapping edit"),
            ConflictType::RegionConflict => write!(f, "region conflict"),
            ConflictType::StructuralConflict => write!(f, "structural conflict"),
        }
    }
}

/// Result of a three-way merge operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MergeResult {
    /// Merged content
    pub content: String,
    /// Whether there were conflicts
    pub has_conflicts: bool,
    /// Conflicts that were detected
    pub conflicts: Vec<MergeConflict>,
    /// Strategy used for resolution
    pub strategy: MergeStrategy,
}

impl MergeResult {
    /// Create a successful merge result
    pub fn success(content: String, strategy: MergeStrategy) -> Self {
        Self {
            content,
            has_conflicts: false,
            conflicts: Vec::new(),
            strategy,
        }
    }

    /// Create a merge result with conflicts
    pub fn with_conflicts(
        content: String, conflicts: Vec<MergeConflict>, strategy: MergeStrategy,
    ) -> Self {
        Self {
            content,
            has_conflicts: true,
            conflicts,
            strategy,
        }
    }
}

/// Strategy for resolving merge conflicts
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum MergeStrategy {
    /// Automatically resolve using generated content (overwrite manual)
    GeneratedWins,
    /// Automatically resolve using manual content (preserve manual)
    ManualWins,
    /// Interactive resolution (ask user)
    Interactive,
    /// Fail on conflicts
    FailOnConflict,
}

impl fmt::Display for MergeStrategy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MergeStrategy::GeneratedWins => write!(f, "generated wins"),
            MergeStrategy::ManualWins => write!(f, "manual wins"),
            MergeStrategy::Interactive => write!(f, "interactive"),
            MergeStrategy::FailOnConflict => write!(f, "fail on conflict"),
        }
    }
}

/// Three-way merger for files with mixed generated and manual content
pub struct ThreeWayMerger {
    /// Strategy for resolving conflicts
    strategy: MergeStrategy,
}

impl ThreeWayMerger {
    /// Create a new three-way merger with specified strategy
    pub fn new(strategy: MergeStrategy) -> Self {
        Self { strategy }
    }

    /// Merge three versions of a file
    pub fn merge(
        &self, baseline: &str, generated: &str, manual: &str, file_path: &Path,
    ) -> Result<MergeResult> {
        // For now, implement a simple strategy-based merge
        // A full implementation would use proper diff3 algorithm

        match self.strategy {
            MergeStrategy::GeneratedWins => Ok(MergeResult::success(
                generated.to_string(),
                self.strategy.clone(),
            )),
            MergeStrategy::ManualWins => Ok(MergeResult::success(
                manual.to_string(),
                self.strategy.clone(),
            )),
            MergeStrategy::FailOnConflict => {
                // Check for conflicts (simplified)
                if self.has_conflicts(baseline, generated, manual)? {
                    let conflicts =
                        self.detect_conflicts(baseline, generated, manual, file_path)?;
                    Ok(MergeResult::with_conflicts(
                        String::new(),
                        conflicts,
                        self.strategy.clone(),
                    ))
                } else {
                    Ok(MergeResult::success(
                        generated.to_string(),
                        self.strategy.clone(),
                    ))
                }
            }
            MergeStrategy::Interactive => {
                // Interactive resolution would require user input
                // For now, fall back to generated wins
                Ok(MergeResult::success(
                    generated.to_string(),
                    self.strategy.clone(),
                ))
            }
        }
    }

    /// Check if there are conflicts between versions
    fn has_conflicts(&self, _baseline: &str, _generated: &str, _manual: &str) -> Result<bool> {
        // Simplified conflict detection
        // Real implementation would use proper diff algorithm
        Ok(false)
    }

    /// Detect specific conflicts
    fn detect_conflicts(
        &self, _baseline: &str, _generated: &str, _manual: &str, _file_path: &Path,
    ) -> Result<Vec<MergeConflict>> {
        // Simplified conflict detection
        // Real implementation would analyze regions and detect overlaps
        Ok(Vec::new())
    }
}

/// Advanced merger that understands generated vs manual regions
pub struct RegionAwareMerger {
    /// Strategy for conflict resolution
    strategy: MergeStrategy,
}

impl RegionAwareMerger {
    /// Create a new region-aware merger
    pub fn new(strategy: MergeStrategy) -> Self {
        Self { strategy }
    }

    /// Merge using region information from snapshots
    pub fn merge_with_regions(
        &self, baseline: &str, generated: &str, manual: &str, _baseline_snapshot: &FileSnapshot,
        file_path: &Path,
    ) -> Result<MergeResult> {
        let mut result = String::new();
        let mut conflicts = Vec::new();
        let mut has_conflicts = false;

        // Split content into lines for easier processing
        let baseline_lines: Vec<&str> = baseline.lines().collect();
        let generated_lines: Vec<&str> = generated.lines().collect();
        let manual_lines: Vec<&str> = manual.lines().collect();

        // Process line by line, respecting regions
        let mut i = 0;
        while i < baseline_lines
            .len()
            .max(generated_lines.len())
            .max(manual_lines.len())
        {
            let baseline_line = baseline_lines.get(i);
            let generated_line = generated_lines.get(i);
            let manual_line = manual_lines.get(i);

            match (baseline_line, generated_line, manual_line) {
                (Some(bl), Some(gl), Some(ml)) => {
                    if bl == gl && gl == ml {
                        // All versions agree
                        result.push_str(gl);
                        result.push('\n');
                    } else if bl == gl {
                        // Manual differs from baseline/generated
                        result.push_str(ml);
                        result.push('\n');
                    } else if bl == ml {
                        // Generated differs from baseline/manual
                        result.push_str(gl);
                        result.push('\n');
                    } else {
                        // All three differ - conflict
                        has_conflicts = true;
                        conflicts.push(MergeConflict::new(
                            file_path.to_path_buf(),
                            ConflictType::OverlappingEdit,
                            format!("Conflict at line {}", i + 1),
                            gl.to_string(),
                            ml.to_string(),
                            bl.to_string(),
                        ));

                        // Apply strategy for conflict resolution
                        match self.strategy {
                            MergeStrategy::GeneratedWins => {
                                result.push_str(gl);
                                result.push('\n');
                            }
                            MergeStrategy::ManualWins => {
                                result.push_str(ml);
                                result.push('\n');
                            }
                            _ => {
                                // For other strategies, we'd need more sophisticated handling
                                result.push_str(gl);
                                result.push('\n');
                            }
                        }
                    }
                }
                (Some(bl), Some(gl), None) => {
                    // Manual is shorter
                    if bl == gl {
                        // No conflict, use generated
                        result.push_str(gl);
                        result.push('\n');
                    } else {
                        // Conflict
                        has_conflicts = true;
                        conflicts.push(MergeConflict::new(
                            file_path.to_path_buf(),
                            ConflictType::StructuralConflict,
                            "Length mismatch".to_string(),
                            gl.to_string(),
                            String::new(),
                            bl.to_string(),
                        ));
                    }
                }
                (Some(bl), None, Some(ml)) => {
                    // Generated is shorter
                    if bl == ml {
                        // No conflict, use manual
                        result.push_str(ml);
                        result.push('\n');
                    } else {
                        // Conflict
                        has_conflicts = true;
                        conflicts.push(MergeConflict::new(
                            file_path.to_path_buf(),
                            ConflictType::StructuralConflict,
                            "Length mismatch".to_string(),
                            String::new(),
                            ml.to_string(),
                            bl.to_string(),
                        ));
                    }
                }
                (None, Some(gl), Some(ml)) => {
                    // Baseline is shorter - new content in both
                    if gl == ml {
                        result.push_str(gl);
                        result.push('\n');
                    } else {
                        has_conflicts = true;
                        conflicts.push(MergeConflict::new(
                            file_path.to_path_buf(),
                            ConflictType::OverlappingEdit,
                            "New content conflict".to_string(),
                            gl.to_string(),
                            ml.to_string(),
                            String::new(),
                        ));
                    }
                }
                _ => break, // All versions ended
            }

            i += 1;
        }

        if has_conflicts {
            Ok(MergeResult::with_conflicts(
                result,
                conflicts,
                self.strategy.clone(),
            ))
        } else {
            Ok(MergeResult::success(result, self.strategy.clone()))
        }
    }
}

/// Utilities for working with file regions
pub struct RegionUtils;

impl RegionUtils {
    /// Parse generated/manual regions from file content using markers
    pub fn parse_regions(content: &str) -> (Vec<Region>, Vec<Region>) {
        let mut generated_regions = Vec::new();
        let mut manual_regions = Vec::new();
        let mut in_generated = false;
        let mut in_manual = false;
        let mut current_start = 0;
        let mut line_number = 1;

        for line in content.lines() {
            if line.contains("// GENERATED: DO NOT EDIT")
                || line.contains("<!-- GENERATED: DO NOT EDIT")
            {
                if !in_generated && !in_manual {
                    current_start = line_number;
                    in_generated = true;
                }
            } else if line.contains("// END GENERATED") || line.contains("<!-- END GENERATED") {
                if in_generated {
                    generated_regions.push(Region {
                        start: current_start,
                        end: line_number,
                        region_type: RegionType::Generated,
                    });
                    in_generated = false;
                }
            } else if line.contains("// MANUAL:") || line.contains("<!-- MANUAL:") {
                if !in_generated && !in_manual {
                    current_start = line_number;
                    in_manual = true;
                }
            } else if (line.contains("// END MANUAL") || line.contains("<!-- END MANUAL"))
                && in_manual
            {
                manual_regions.push(Region {
                    start: current_start,
                    end: line_number,
                    region_type: RegionType::Manual,
                });
                in_manual = false;
            }

            line_number += 1;
        }

        (generated_regions, manual_regions)
    }

    /// Check if a line number is within any region of a specific type
    pub fn is_in_region(line_number: usize, regions: &[Region], region_type: &RegionType) -> bool {
        regions.iter().any(|r| {
            r.start <= line_number && r.end >= line_number && &r.region_type == region_type
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::snapshot::FileSnapshot;

    #[test]
    fn test_merge_strategies() {
        let merger = ThreeWayMerger::new(MergeStrategy::GeneratedWins);

        let baseline = "line 1\nline 2\n";
        let generated = "line 1\nline 2\nline 3\n";
        let manual = "line 1\nmanual line\nline 3\n";

        let result = merger
            .merge(baseline, generated, manual, Path::new("test.txt"))
            .unwrap();

        assert!(!result.has_conflicts);
        assert_eq!(result.strategy, MergeStrategy::GeneratedWins);
        assert_eq!(result.content, generated);
    }

    #[test]
    fn test_region_aware_merge() {
        let merger = RegionAwareMerger::new(MergeStrategy::ManualWins);

        let baseline = "line 1\n// GENERATED: DO NOT EDIT\nline 2\n// END GENERATED\nline 3\n";
        let generated =
            "line 1\n// GENERATED: DO NOT EDIT\nline 2 modified\n// END GENERATED\nline 3\n";
        let manual = "line 1\n// GENERATED: DO NOT EDIT\nline 2\n// END GENERATED\nmanual line\n";

        // Create mock snapshot
        let snapshot = FileSnapshot {
            path: PathBuf::from("test.txt"),
            hash: String::new(),
            size: 0,
            modified_at: chrono::Utc::now(),
            generated_regions: vec![Region {
                start: 2,
                end: 4,
                region_type: RegionType::Generated,
            }],
            manual_regions: vec![Region {
                start: 5,
                end: 5,
                region_type: RegionType::Manual,
            }],
        };

        let result = merger
            .merge_with_regions(
                baseline,
                generated,
                manual,
                &snapshot,
                Path::new("test.txt"),
            )
            .unwrap();

        assert!(!result.has_conflicts);
        assert_eq!(result.strategy, MergeStrategy::ManualWins);
    }

    #[test]
    fn test_region_parsing() {
        let content = r#"
line 1
// GENERATED: DO NOT EDIT
generated line 1
generated line 2
// END GENERATED
// MANUAL: Safe to edit
manual line 1
manual line 2
// END MANUAL
line 4
"#;

        let (generated, manual) = RegionUtils::parse_regions(content);

        println!("Generated regions: {:?}", generated);
        println!("Manual regions: {:?}", manual);

        assert_eq!(generated.len(), 1);
        assert_eq!(manual.len(), 1);
        assert_eq!(generated[0].start, 3);
        assert_eq!(generated[0].end, 6);
        assert_eq!(manual[0].start, 7);
        assert_eq!(manual[0].end, 10);
    }

    #[test]
    fn test_region_check() {
        let regions = vec![
            Region {
                start: 2,
                end: 4,
                region_type: RegionType::Generated,
            },
            Region {
                start: 6,
                end: 8,
                region_type: RegionType::Manual,
            },
        ];

        assert!(RegionUtils::is_in_region(
            3,
            &regions,
            &RegionType::Generated
        ));
        assert!(!RegionUtils::is_in_region(3, &regions, &RegionType::Manual));
        assert!(RegionUtils::is_in_region(7, &regions, &RegionType::Manual));
        assert!(!RegionUtils::is_in_region(
            1,
            &regions,
            &RegionType::Generated
        ));
    }
}
