//! Three-way merge for delta-driven projection
//!
//! This module provides functionality to merge generated code with manual edits,
//! preserving user customizations during regeneration. It implements a three-way
//! merge algorithm that uses baselines to intelligently combine generated and
//! manual content.
//!
//! ## Features
//!
//! - **Three-Way Merge**: Merge generated, manual, and baseline content
//! - **Region-Aware**: Preserve manual regions while updating generated regions
//! - **Conflict Detection**: Identify and report merge conflicts
//! - **Merge Strategies**: Configurable merge strategies (prefer-generated, prefer-manual, etc.)
//! - **Conflict Resolution**: Tools for resolving merge conflicts
//!
//! ## Merge Process
//!
//! 1. **Identify Regions**: Separate generated and manual regions in files
//! 2. **Compare Changes**: Compare generated and manual changes from baseline
//! 3. **Apply Strategy**: Apply merge strategy to combine changes
//! 4. **Detect Conflicts**: Identify conflicts where both sides changed the same region
//! 5. **Report Results**: Return merge result with conflicts if any
//!
//! ## Examples
//!
//! ### Performing a Three-Way Merge
//!
//! ```rust,no_run
//! use ggen_core::merge::{ThreeWayMerger, MergeStrategy};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let merger = ThreeWayMerger::new(MergeStrategy::PreferGenerated);
//!
//! let result = merger.merge(
//!     "baseline content",
//!     "new generated content",
//!     "manual edits"
//! )?;
//!
//! if result.has_conflicts() {
//!     println!("Conflicts detected: {:?}", result.conflicts);
//! } else {
//!     println!("Merged content: {}", result.merged_content);
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ### Region-Aware Merging
//!
//! ```rust,no_run
//! use ggen_core::merge::RegionAwareMerger;
//! use ggen_core::snapshot::{FileSnapshot, Region, RegionType};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let baseline = FileSnapshot::new(PathBuf::from("file.rs"), "baseline".to_string())?;
//! let generated = "new generated";
//! let manual = "manual edits";
//!
//! let merger = RegionAwareMerger::new();
//! let result = merger.merge_regions(&baseline, generated, manual)?;
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::path::{Path, PathBuf};

use crate::snapshot::{FileSnapshot, Region, RegionType};

/// Represents a merge conflict between generated and manual content
///
/// A `MergeConflict` describes a situation where both generated and manual
/// content have changed the same region, making automatic merging impossible.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::merge::{MergeConflict, ConflictType};
/// use std::path::PathBuf;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let conflict = MergeConflict::new(
///     PathBuf::from("file.rs"),
///     ConflictType::OverlappingEdit,
///     "Both generated and manual edits changed the same function".to_string(),
///     "fn generated() {}".to_string(),
///     "fn manual() {}".to_string(),
///     "fn original() {}".to_string(),
/// );
///
/// println!("Conflict in {:?}: {}", conflict.file_path, conflict.description);
/// # Ok(())
/// # }
/// ```
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
///
/// Describes the nature of a merge conflict to help with resolution.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::merge::ConflictType;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let conflict_type = ConflictType::OverlappingEdit;
/// println!("Conflict type: {}", conflict_type);
/// # Ok(())
/// # }
/// ```
///
/// Type of merge conflict
///
/// Represents different types of conflicts that can occur during three-way merging.
///
/// # Examples
///
/// ```rust
/// use ggen_core::merge::ConflictType;
///
/// # fn main() {
/// let conflict = ConflictType::OverlappingEdit;
/// match conflict {
///     ConflictType::OverlappingEdit => assert!(true),
///     ConflictType::RegionConflict => assert!(true),
///     ConflictType::StructuralConflict => assert!(true),
/// }
/// # }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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
///
/// Contains the merged content, any conflicts that were detected, and the
/// strategy used for resolution.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::merge::{MergeResult, MergeStrategy};
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// // Successful merge
/// let result = MergeResult::success(
///     "merged content".to_string(),
///     MergeStrategy::GeneratedWins
/// );
///
/// assert!(!result.has_conflicts);
/// assert_eq!(result.content, "merged content");
///
/// // Merge with conflicts
/// let result_with_conflicts = MergeResult::with_conflicts(
///     "partial merge".to_string(),
///     vec![],
///     MergeStrategy::GeneratedWins
/// );
/// assert!(result_with_conflicts.has_conflicts);
/// # Ok(())
/// # }
/// ```
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
    ///
    /// Creates a merge result indicating successful merging with no conflicts.
    ///
    /// # Arguments
    ///
    /// * `content` - The merged content
    /// * `strategy` - The merge strategy that was used
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::merge::{MergeResult, MergeStrategy};
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let result = MergeResult::success(
    ///     "merged file content".to_string(),
    ///     MergeStrategy::GeneratedWins
    /// );
    ///
    /// assert!(!result.has_conflicts);
    /// assert_eq!(result.content, "merged file content");
    /// # Ok(())
    /// # }
    /// ```
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
///
/// Defines how conflicts should be resolved when both generated and manual
/// content have changed.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::merge::{ThreeWayMerger, MergeStrategy};
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// // Prefer generated content
/// let merger = ThreeWayMerger::new(MergeStrategy::GeneratedWins);
///
/// // Prefer manual edits
/// let merger = ThreeWayMerger::new(MergeStrategy::ManualWins);
///
/// // Fail on conflicts
/// let merger = ThreeWayMerger::new(MergeStrategy::FailOnConflict);
/// # Ok(())
/// # }
/// ```
/// Merge strategy for resolving conflicts
///
/// Determines how to handle conflicts during three-way merging.
///
/// # Examples
///
/// ```rust
/// use ggen_core::merge::MergeStrategy;
///
/// # fn main() {
/// let strategy = MergeStrategy::GeneratedWins;
/// match strategy {
///     MergeStrategy::GeneratedWins => assert!(true),
///     MergeStrategy::ManualWins => assert!(true),
///     MergeStrategy::Interactive => assert!(true),
///     MergeStrategy::FailOnConflict => assert!(true),
/// }
/// # }
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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
///
/// Performs three-way merges using a baseline, new generated content, and
/// manual edits. Uses the configured merge strategy to resolve conflicts.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::merge::{ThreeWayMerger, MergeStrategy};
/// use std::path::Path;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let merger = ThreeWayMerger::new(MergeStrategy::GeneratedWins);
///
/// let result = merger.merge(
///     "baseline content",
///     "new generated content",
///     "manual edits",
///     Path::new("file.rs")
/// )?;
///
/// if result.has_conflicts {
///     println!("Conflicts: {:?}", result.conflicts);
/// } else {
///     println!("Merged: {}", result.content);
/// }
/// # Ok(())
/// # }
/// ```
pub struct ThreeWayMerger {
    /// Strategy for resolving conflicts
    strategy: MergeStrategy,
}

impl ThreeWayMerger {
    /// Create a new three-way merger with specified strategy
    ///
    /// Creates a merger with the specified conflict resolution strategy.
    ///
    /// # Arguments
    ///
    /// * `strategy` - Strategy to use when conflicts are detected
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::merge::{ThreeWayMerger, MergeStrategy};
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let merger = ThreeWayMerger::new(MergeStrategy::GeneratedWins);
    /// // Use merger to merge content
    /// # Ok(())
    /// # }
    /// ```
    pub fn new(strategy: MergeStrategy) -> Self {
        Self { strategy }
    }

    /// Merge three versions of a file
    ///
    /// Merges three versions of content: baseline (original), generated (new
    /// generated content), and manual (user edits). Returns merged content
    /// and any conflicts detected.
    ///
    /// # Arguments
    ///
    /// * `baseline` - Original content from snapshot
    /// * `generated` - New generated content
    /// * `manual` - User's manual edits
    /// * `file_path` - Path to the file being merged
    ///
    /// # Returns
    ///
    /// A `MergeResult` containing the merged content and any conflicts.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::merge::{ThreeWayMerger, MergeStrategy};
    /// use std::path::Path;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let merger = ThreeWayMerger::new(MergeStrategy::GeneratedWins);
    ///
    /// let result = merger.merge(
    ///     "original function",
    ///     "new generated function",
    ///     "manual function edit",
    ///     Path::new("file.rs")
    /// )?;
    ///
    /// if !result.has_conflicts {
    ///     println!("Successfully merged: {}", result.content);
    /// }
    /// # Ok(())
    /// # }
    /// ```
    pub fn merge(
        &self, baseline: &str, generated: &str, manual: &str, file_path: &Path,
    ) -> Result<MergeResult> {
        // For now, implement a simple strategy-based merge
        // A full implementation would use proper diff3 algorithm

        match self.strategy {
            MergeStrategy::GeneratedWins => {
                Ok(MergeResult::success(generated.to_string(), self.strategy))
            }
            MergeStrategy::ManualWins => {
                Ok(MergeResult::success(manual.to_string(), self.strategy))
            }
            MergeStrategy::FailOnConflict => {
                // Check for conflicts (simplified)
                if self.has_conflicts(baseline, generated, manual)? {
                    let conflicts =
                        self.detect_conflicts(baseline, generated, manual, file_path)?;
                    Ok(MergeResult::with_conflicts(
                        String::new(),
                        conflicts,
                        self.strategy,
                    ))
                } else {
                    Ok(MergeResult::success(generated.to_string(), self.strategy))
                }
            }
            MergeStrategy::Interactive => {
                // Interactive resolution would require user input
                // For now, fall back to generated wins
                Ok(MergeResult::success(generated.to_string(), self.strategy))
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
///
/// A specialized merger that understands file regions (generated vs manual)
/// and preserves manual regions while updating generated regions.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::merge::{RegionAwareMerger, MergeStrategy};
/// use ggen_core::snapshot::FileSnapshot;
/// use std::path::PathBuf;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let merger = RegionAwareMerger::new(MergeStrategy::ManualWins);
/// let baseline = FileSnapshot::new(
///     PathBuf::from("file.rs"),
///     "baseline content".to_string()
/// )?;
///
/// let result = merger.merge_with_regions(
///     "baseline content",
///     "new generated content",
///     "manual edits",
///     &baseline,
///     std::path::Path::new("file.rs")
/// )?;
/// # Ok(())
/// # }
/// ```
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
                self.strategy,
            ))
        } else {
            Ok(MergeResult::success(result, self.strategy))
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
    use chicago_tdd_tools::{async_test, test};

    test!(test_merge_strategies, {
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
    });

    test!(test_region_aware_merge, {
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
    });

    test!(test_region_parsing, {
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
    });

    test!(test_region_check, {
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
    });
}
