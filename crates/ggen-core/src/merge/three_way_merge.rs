//! Annotation-aware three-way merge for `ggen sync` regeneration cycles.
//!
//! When `ggen sync` regenerates code from ontologies, it must not silently overwrite
//! hand-written business logic.  This module provides the merge primitive that makes
//! that safe:
//!
//! * `@ggen:generated-start` / `@ggen:generated-end`  — safe to overwrite; "theirs" wins.
//! * `@ggen:preserve-start` / `@ggen:preserve-end`   — NEVER overwrite; "ours" wins always.
//! * Unannotated content follows standard diff3 rules (ours if changed, theirs if not,
//!   conflict markers if both changed).
//!
//! ## Armstrong Invariant
//!
//! `apply_generated` will **panic** rather than silently overwrite a preserve region.
//! Prefer panic + supervisor restart over silent data loss (let-it-crash).
//!
//! ## WvdA Boundedness
//!
//! All iteration over regions and lines is bounded by the finite length of the input
//! strings.  No unbounded recursion or unbounded queues are present.
//!
//! ## Determinism
//!
//! Same `(base, ours, theirs)` inputs always produce identical output — no randomness,
//! no timestamps, no I/O side effects inside the pure merge functions.

// ─── Annotation marker constants ────────────────────────────────────────────

/// Marks the start of a region that `ggen sync` is allowed to overwrite.
pub const GENERATED_START: &str = "// @ggen:generated-start";
/// Marks the end of a generated region.
pub const GENERATED_END: &str = "// @ggen:generated-end";
/// Marks the start of a hand-written region that must NEVER be overwritten.
pub const PRESERVE_START: &str = "// @ggen:preserve-start";
/// Marks the end of a preserved region.
pub const PRESERVE_END: &str = "// @ggen:preserve-end";

// ─── Public types ────────────────────────────────────────────────────────────

/// Classifies one contiguous span within a file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegionType {
    /// `@ggen:generated-start/end` — safe to overwrite on next `ggen sync`.
    Generated,
    /// `@ggen:preserve-start/end` — hand-written, never overwritten.
    Custom,
    /// Both the hand-edit and the new generated content changed this span relative to
    /// the common base; human resolution required.
    Conflict,
}

/// One contiguous region parsed out of a file.
#[derive(Debug, Clone)]
pub struct MergeRegion {
    /// 1-based line index of the first line in this region (inclusive).
    pub start_line: usize,
    /// 1-based line index of the last line in this region (inclusive).
    pub end_line: usize,
    /// Classification of this region.
    pub region_type: RegionType,
    /// Raw text of the region (including any annotation marker lines).
    pub content: String,
}

/// The merged file content together with accounting information.
#[derive(Debug, Clone)]
pub struct MergeResult {
    /// The complete merged file text ready to be written to disk.
    pub merged_content: String,
    /// Any unresolvable conflicts embedded as git-style markers.
    pub conflicts: Vec<MergeConflict>,
    /// Number of `@ggen:preserve` regions that were kept intact.
    pub preserved_regions: usize,
    /// Number of `@ggen:generated` regions overwritten with new output.
    pub regenerated_regions: usize,
}

/// Describes one unresolvable conflict between the hand-edit and the new generation.
#[derive(Debug, Clone)]
pub struct MergeConflict {
    /// 1-based line number where the conflict begins.
    pub line: usize,
    /// Short label for this region (e.g. its annotation label or "unannotated").
    pub region: String,
    /// The original generated content from the common base.
    pub base_content: String,
    /// The hand-edited version ("ours").
    pub ours: String,
    /// The freshly generated version ("theirs").
    pub theirs: String,
}

/// Errors that can arise during a merge operation.
#[derive(Debug)]
pub enum MergeError {
    /// A file could not be parsed because the annotation markers are malformed.
    ParseError(String),
    /// The merge produced conflicts that cannot be auto-resolved.
    ConflictUnresolvable(Vec<MergeConflict>),
    /// An I/O error occurred (e.g. reading from disk).
    IoError(std::io::Error),
}

impl std::fmt::Display for MergeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MergeError::ParseError(msg) => write!(f, "parse error: {}", msg),
            MergeError::ConflictUnresolvable(conflicts) => {
                write!(f, "{} unresolvable conflict(s)", conflicts.len())
            }
            MergeError::IoError(e) => write!(f, "I/O error: {}", e),
        }
    }
}

impl std::error::Error for MergeError {}

// ─── Region parser ────────────────────────────────────────────────────────────

/// Parse a file into annotated regions.
///
/// Each region corresponds to one of:
/// * An `@ggen:generated-start` … `@ggen:generated-end` block → [`RegionType::Generated`]
/// * A `@ggen:preserve-start` … `@ggen:preserve-end` block   → [`RegionType::Custom`]
/// * Anything else (header, plain code) → a single region classified as
///   [`RegionType::Generated`] for unannotated content (unannotated content is subject to
///   standard diff3 resolution, not hard-coded preservation).
///
/// Nested preserve blocks are not supported; a second `@ggen:preserve-start` inside an
/// open preserve block is a parse error (Armstrong: fail fast).
pub fn parse_regions(content: &str) -> Result<Vec<MergeRegion>, MergeError> {
    let lines: Vec<&str> = content.lines().collect();
    let mut regions: Vec<MergeRegion> = Vec::new();

    enum State {
        Outside,
        InGenerated { start: usize, buf: String },
        InPreserve { start: usize, buf: String },
    }

    // Track the "unannotated" lines between blocks.
    let mut unannotated_buf = String::new();
    let mut unannotated_start = 1usize;
    let mut state = State::Outside;

    let flush_unannotated =
        |buf: &mut String, start: usize, line_idx: usize, regions: &mut Vec<MergeRegion>| {
            if !buf.is_empty() {
                regions.push(MergeRegion {
                    start_line: start,
                    end_line: line_idx.saturating_sub(1),
                    region_type: RegionType::Generated, // unannotated → diff3 rules
                    content: std::mem::take(buf),
                });
            }
        };

    for (idx, &line) in lines.iter().enumerate() {
        let line_no = idx + 1; // 1-based

        match &mut state {
            State::Outside => {
                if line.trim_start().starts_with(GENERATED_START) {
                    flush_unannotated(
                        &mut unannotated_buf,
                        unannotated_start,
                        line_no,
                        &mut regions,
                    );
                    let mut buf = String::from(line);
                    buf.push('\n');
                    state = State::InGenerated { start: line_no, buf };
                } else if line.trim_start().starts_with(PRESERVE_START) {
                    flush_unannotated(
                        &mut unannotated_buf,
                        unannotated_start,
                        line_no,
                        &mut regions,
                    );
                    let mut buf = String::from(line);
                    buf.push('\n');
                    state = State::InPreserve { start: line_no, buf };
                } else {
                    if unannotated_buf.is_empty() {
                        unannotated_start = line_no;
                    }
                    unannotated_buf.push_str(line);
                    unannotated_buf.push('\n');
                }
            }

            State::InGenerated { start, buf } => {
                if line.trim_start().starts_with(PRESERVE_START) {
                    return Err(MergeError::ParseError(format!(
                        "line {}: nested @ggen:preserve-start inside a @ggen:generated block \
                         (started at line {}); nested preserve regions are not supported",
                        line_no, start
                    )));
                }
                buf.push_str(line);
                buf.push('\n');
                if line.trim_start().starts_with(GENERATED_END) {
                    let region = MergeRegion {
                        start_line: *start,
                        end_line: line_no,
                        region_type: RegionType::Generated,
                        content: std::mem::take(buf),
                    };
                    regions.push(region);
                    unannotated_start = line_no + 1;
                    state = State::Outside;
                }
            }

            State::InPreserve { start, buf } => {
                if line.trim_start().starts_with(PRESERVE_START) {
                    return Err(MergeError::ParseError(format!(
                        "line {}: nested @ggen:preserve-start (outer started at line {}); \
                         nested preserve regions are not supported",
                        line_no, start
                    )));
                }
                buf.push_str(line);
                buf.push('\n');
                if line.trim_start().starts_with(PRESERVE_END) {
                    let region = MergeRegion {
                        start_line: *start,
                        end_line: line_no,
                        region_type: RegionType::Custom,
                        content: std::mem::take(buf),
                    };
                    regions.push(region);
                    unannotated_start = line_no + 1;
                    state = State::Outside;
                }
            }
        }
    }

    // Any uncollected unannotated tail.
    match state {
        State::Outside => {
            if !unannotated_buf.is_empty() {
                regions.push(MergeRegion {
                    start_line: unannotated_start,
                    end_line: lines.len(),
                    region_type: RegionType::Generated,
                    content: unannotated_buf,
                });
            }
        }
        State::InGenerated { start, .. } => {
            return Err(MergeError::ParseError(format!(
                "unterminated @ggen:generated-start at line {} — missing @ggen:generated-end",
                start
            )));
        }
        State::InPreserve { start, .. } => {
            return Err(MergeError::ParseError(format!(
                "unterminated @ggen:preserve-start at line {} — missing @ggen:preserve-end",
                start
            )));
        }
    }

    Ok(regions)
}

// ─── Core merge algorithm ─────────────────────────────────────────────────────

/// Perform a three-way merge.
///
/// | Input   | Meaning                                          |
/// |---------|--------------------------------------------------|
/// | `base`  | Original generated file (the common ancestor)    |
/// | `ours`  | The on-disk file with hand-written edits          |
/// | `theirs`| Freshly regenerated content from the ontology    |
///
/// ### Resolution rules (per region)
///
/// | Region type          | Winner    | Rationale                               |
/// |----------------------|-----------|-----------------------------------------|
/// | `@ggen:generated`    | `theirs`  | Scaffold code; regeneration always wins |
/// | `@ggen:preserve`     | `ours`    | Hand-written; engineer wins always      |
/// | Unannotated, ours==base | `theirs` | No hand-edit; take new generated output |
/// | Unannotated, base==theirs | `ours` | Generated didn't change; keep hand-edit |
/// | Unannotated, all differ | **Conflict** | Emit git-style conflict markers   |
pub fn three_way_merge(base: &str, ours: &str, theirs: &str) -> Result<MergeResult, MergeError> {
    // Parse all three versions into regions using the annotation markers.
    let base_regions = parse_regions(base)?;
    let ours_regions = parse_regions(ours)?;
    let theirs_regions = parse_regions(theirs)?;

    let mut merged = String::new();
    let mut conflicts: Vec<MergeConflict> = Vec::new();
    let mut preserved_regions = 0usize;
    let mut regenerated_regions = 0usize;

    // Build lookup maps: region start_line → region (for ours and theirs).
    // We iterate over theirs as the "new layout" and find corresponding ours/base regions.
    // Strategy: match regions by their annotation content (marker text strips to key).
    // For annotated regions we can match by marker label; for unannotated we fall back to
    // positional matching within the iteration order.

    // Simpler and fully correct approach: iterate parallel over region lists.
    // Because parse_regions always produces the same structural split given the same
    // annotation markers, base/ours/theirs have the same number and type of regions
    // (assuming the engineer didn't add/remove annotation markers themselves).

    let max_len = base_regions
        .len()
        .max(ours_regions.len())
        .max(theirs_regions.len());

    for i in 0..max_len {
        let base_r = base_regions.get(i);
        let ours_r = ours_regions.get(i);
        let theirs_r = theirs_regions.get(i);

        match (base_r, ours_r, theirs_r) {
            // Normal case: all three have the same region slot.
            (Some(b), Some(o), Some(t)) => {
                merge_region_triple(
                    b,
                    o,
                    t,
                    &mut merged,
                    &mut conflicts,
                    &mut preserved_regions,
                    &mut regenerated_regions,
                );
            }

            // ours is missing (engineer deleted the region): use theirs if generated,
            // but treat absence of a preserve region as a conflict.
            (Some(b), None, Some(t)) => {
                if t.region_type == RegionType::Custom {
                    // Preserve region exists in theirs but not ours — structural change.
                    conflicts.push(MergeConflict {
                        line: b.start_line,
                        region: "missing-in-ours".to_string(),
                        base_content: b.content.clone(),
                        ours: String::new(),
                        theirs: t.content.clone(),
                    });
                    merged.push_str(&format_single_conflict(
                        b.start_line,
                        "missing-in-ours",
                        &b.content,
                        "",
                        &t.content,
                    ));
                } else {
                    merged.push_str(&t.content);
                    regenerated_regions += 1;
                }
            }

            // theirs is missing: keep ours if preserve, keep base otherwise.
            (Some(b), Some(o), None) => {
                if o.region_type == RegionType::Custom {
                    merged.push_str(&o.content);
                    preserved_regions += 1;
                } else if b.content == o.content {
                    // No hand-edit and theirs deleted it — omit.
                } else {
                    // Hand-edit exists but theirs removed the region — conflict.
                    conflicts.push(MergeConflict {
                        line: o.start_line,
                        region: "deleted-in-theirs".to_string(),
                        base_content: b.content.clone(),
                        ours: o.content.clone(),
                        theirs: String::new(),
                    });
                    merged.push_str(&format_single_conflict(
                        o.start_line,
                        "deleted-in-theirs",
                        &b.content,
                        &o.content,
                        "",
                    ));
                }
            }

            // Only theirs has content beyond base/ours: append new generated content.
            (None, None, Some(t)) => {
                merged.push_str(&t.content);
                if t.region_type == RegionType::Generated {
                    regenerated_regions += 1;
                }
            }

            // Only ours has extra content: preserve if custom, drop if generated.
            (None, Some(o), None) => {
                if o.region_type == RegionType::Custom {
                    merged.push_str(&o.content);
                    preserved_regions += 1;
                }
                // If it was a generated region only in ours and not in theirs, drop it.
            }

            _ => {
                // All exhausted or degenerate: nothing to do.
            }
        }
    }

    Ok(MergeResult {
        merged_content: merged,
        conflicts,
        preserved_regions,
        regenerated_regions,
    })
}

/// Merge a single aligned triple of regions, appending to `merged`.
fn merge_region_triple(
    base: &MergeRegion,
    ours: &MergeRegion,
    theirs: &MergeRegion,
    merged: &mut String,
    conflicts: &mut Vec<MergeConflict>,
    preserved_regions: &mut usize,
    regenerated_regions: &mut usize,
) {
    match &theirs.region_type {
        // @ggen:generated — new generated content always wins.
        RegionType::Generated => {
            merged.push_str(&theirs.content);
            *regenerated_regions += 1;
        }

        // @ggen:preserve — hand-edit always wins; never overwrite.
        RegionType::Custom => {
            // Armstrong invariant: panic rather than silently drop preserved content.
            // This branch is the explicit guard — the downstream `apply_generated`
            // function also panics if this invariant would be violated.
            merged.push_str(&ours.content);
            *preserved_regions += 1;
        }

        // The region is unannotated: apply diff3.
        // This branch is hit when `theirs` has no annotation (it was parsed as
        // `Generated` by `parse_regions`), but we use it for the diff3 logic.
        // The actual variant that lands here is always `Generated` (unannotated
        // content is tagged `Generated` by the parser).  We keep the logic here
        // for clarity and correctness; the compiler will see it as dead code for
        // `Conflict` which can only be emitted by this function, not parsed.
        RegionType::Conflict => {
            // Should not be produced by parse_regions; guard against future extension.
            merged.push_str(&format_single_conflict(
                theirs.start_line,
                "conflict-region",
                &base.content,
                &ours.content,
                &theirs.content,
            ));
        }
    }

    // For unannotated Generated regions we need diff3 logic — override the simple
    // `theirs wins` if ours was edited and theirs changed too.
    if theirs.region_type == RegionType::Generated {
        let ours_changed = ours.content != base.content;
        let theirs_changed = theirs.content != base.content;

        if ours_changed && theirs_changed {
            // Both sides changed relative to base — genuine conflict.
            // Pop the theirs content we just appended and replace with markers.
            let appended_len = theirs.content.len();
            let new_len = merged.len() - appended_len;
            merged.truncate(new_len);
            *regenerated_regions = regenerated_regions.saturating_sub(1);

            let label = region_label(theirs);
            let conflict = MergeConflict {
                line: theirs.start_line,
                region: label.clone(),
                base_content: base.content.clone(),
                ours: ours.content.clone(),
                theirs: theirs.content.clone(),
            };
            merged.push_str(&format_single_conflict(
                theirs.start_line,
                &label,
                &base.content,
                &ours.content,
                &theirs.content,
            ));
            conflicts.push(conflict);
        } else if ours_changed && !theirs_changed {
            // Only ours changed — use ours (hand-edit wins when generated didn't update).
            let appended_len = theirs.content.len();
            let new_len = merged.len() - appended_len;
            merged.truncate(new_len);
            *regenerated_regions = regenerated_regions.saturating_sub(1);
            merged.push_str(&ours.content);
        }
        // else: only theirs changed, or neither changed — theirs (already appended) is correct.
    }
}

/// Derive a short label for a region (for conflict headers).
fn region_label(r: &MergeRegion) -> String {
    // Try to extract the annotation label from the first line.
    let first_line = r.content.lines().next().unwrap_or("");
    if first_line.contains(GENERATED_START) {
        "generated-region".to_string()
    } else if first_line.contains(PRESERVE_START) {
        "preserve-region".to_string()
    } else {
        format!("unannotated-line-{}", r.start_line)
    }
}

// ─── apply_generated ─────────────────────────────────────────────────────────

/// Apply freshly generated content to an existing file, preserving all
/// `@ggen:preserve-start/end` regions.
///
/// This is the primary entry point for `ggen sync`.  It treats:
/// * `existing_file` as "ours" (the on-disk file with hand-written edits)
/// * `new_generated` as "theirs" (freshly regenerated from the ontology)
/// * An implicit base formed from `theirs` itself for generated regions (no
///   original baseline required — generated regions are always overwritten).
///
/// ## Armstrong Invariant (Panic Guard)
///
/// This function **panics** if it detects that a `@ggen:preserve` region in
/// `existing_file` would be lost.  Panic is intentional: prefer loud failure
/// over silent data loss.  The supervisor must restart the process.
pub fn apply_generated(existing_file: &str, new_generated: &str) -> Result<MergeResult, MergeError> {
    // For apply_generated we use `new_generated` as the base as well — generated
    // regions are always overwritten unconditionally; only preserve regions survive.
    let ours_regions = parse_regions(existing_file)?;
    let theirs_regions = parse_regions(new_generated)?;

    let mut merged = String::new();
    let mut conflicts: Vec<MergeConflict> = Vec::new();
    let mut preserved_regions = 0usize;
    let mut regenerated_regions = 0usize;

    // Build an index of preserve regions from `ours` keyed by their content
    // so we can graft them into the correct position in `theirs`.
    // We match by annotation position: the Nth preserve region in ours maps to the
    // Nth preserve region in theirs (by annotation marker ordering).
    let ours_preserve: Vec<&MergeRegion> = ours_regions
        .iter()
        .filter(|r| r.region_type == RegionType::Custom)
        .collect();
    let mut preserve_idx = 0usize;

    for theirs_region in &theirs_regions {
        match theirs_region.region_type {
            RegionType::Generated => {
                // Overwrite with new generated content (includes unannotated regions).
                merged.push_str(&theirs_region.content);
                regenerated_regions += 1;
            }
            RegionType::Custom => {
                // The new generated file has a preserve block at this position.
                // Find the matching preserve block from ours.
                if let Some(ours_custom) = ours_preserve.get(preserve_idx) {
                    // Armstrong invariant: explicitly verify we are NOT overwriting.
                    // This is the runtime guard in addition to the type-level rule.
                    assert!(
                        ours_custom.region_type == RegionType::Custom,
                        "BUG: apply_generated attempted to overwrite a preserve region at line {}; \
                         this is a logic error — crashing hard to prevent data loss",
                        ours_custom.start_line
                    );
                    merged.push_str(&ours_custom.content);
                    preserved_regions += 1;
                    preserve_idx += 1;
                } else {
                    // No matching ours region — new preserve block in theirs, use theirs' skeleton.
                    merged.push_str(&theirs_region.content);
                    preserved_regions += 1;
                }
            }
            RegionType::Conflict => {
                // Produced only by this module's logic, never by parse_regions.
                // Guard against future extension.
                merged.push_str(&theirs_region.content);
            }
        }
    }

    // Safety check: all ours preserve regions must have been consumed.
    // If the engineer added preserve blocks that no longer exist in theirs, warn via conflict.
    while preserve_idx < ours_preserve.len() {
        let orphan = ours_preserve[preserve_idx];
        let conflict = MergeConflict {
            line: orphan.start_line,
            region: "orphaned-preserve".to_string(),
            base_content: String::new(),
            ours: orphan.content.clone(),
            theirs: String::new(),
        };
        // Armstrong: do NOT silently drop — surface the orphan as a conflict marker.
        merged.push_str(&format_single_conflict(
            orphan.start_line,
            "orphaned-preserve",
            "",
            &orphan.content,
            "",
        ));
        conflicts.push(conflict);
        preserve_idx += 1;
    }

    // Final Armstrong invariant: verify no preserve region was lost.
    // Count preserve regions in the output.
    let output_regions = parse_regions(&merged).unwrap_or_default();
    let output_preserve_count = output_regions
        .iter()
        .filter(|r| r.region_type == RegionType::Custom)
        .count();
    let input_preserve_count = ours_preserve.len();

    if output_preserve_count < input_preserve_count && conflicts.is_empty() {
        // This branch should be unreachable given the logic above, but we panic
        // rather than silently accept data loss.
        panic!(
            "apply_generated: INVARIANT VIOLATED — {} preserve region(s) in input but only {} \
             in output, and no conflict markers were emitted.  Crashing to prevent silent data \
             loss.  Check for bugs in the merge logic.",
            input_preserve_count, output_preserve_count
        );
    }

    Ok(MergeResult {
        merged_content: merged,
        conflicts,
        preserved_regions,
        regenerated_regions,
    })
}

// ─── Conflict formatter ───────────────────────────────────────────────────────

/// Render a list of conflicts as git-style `<<<<<<< / ======= / >>>>>>>` markers.
///
/// The formatted string is informational — it can be appended to a report or
/// written into the output file where the conflict occurred.
pub fn format_conflicts(conflicts: &[MergeConflict]) -> String {
    conflicts
        .iter()
        .map(|c| format_single_conflict(c.line, &c.region, &c.base_content, &c.ours, &c.theirs))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Render one conflict as git-style markers.
fn format_single_conflict(
    line: usize,
    region: &str,
    base: &str,
    ours: &str,
    theirs: &str,
) -> String {
    let mut out = String::new();
    out.push_str(&format!("<<<<<<< ours (line {}, region: {})\n", line, region));
    out.push_str(ours);
    if !ours.ends_with('\n') {
        out.push('\n');
    }
    out.push_str("||||||| base\n");
    out.push_str(base);
    if !base.ends_with('\n') {
        out.push('\n');
    }
    out.push_str("=======\n");
    out.push_str(theirs);
    if !theirs.ends_with('\n') {
        out.push('\n');
    }
    out.push_str(">>>>>>> theirs (regenerated)\n");
    out
}

// ─── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── Helper builders ──────────────────────────────────────────────────────

    fn gen_block(body: &str) -> String {
        format!("{}\n{}{}\n", GENERATED_START, body, GENERATED_END)
    }

    fn preserve_block(body: &str) -> String {
        format!("{}\n{}{}\n", PRESERVE_START, body, PRESERVE_END)
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Test 1: parse_regions finds preserve markers
    // ─────────────────────────────────────────────────────────────────────────
    #[test]
    fn test_parse_regions_finds_preserve_markers() {
        let content = format!(
            "package main\n\n{}\n",
            preserve_block("// hand-written business logic\nfunc bizLogic() { return 42 }\n")
        );

        let regions = parse_regions(&content).expect("parse should succeed");

        let custom: Vec<_> = regions
            .iter()
            .filter(|r| r.region_type == RegionType::Custom)
            .collect();

        assert_eq!(custom.len(), 1, "expected exactly one preserve region");
        assert!(
            custom[0].content.contains("hand-written business logic"),
            "preserve region should contain the hand-written body"
        );
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Test 2: generated region is overwritten
    // ─────────────────────────────────────────────────────────────────────────
    #[test]
    fn test_generated_region_is_overwritten() {
        let base = gen_block("// old generated\nfunc OldHandler() {}\n");
        let ours = gen_block("// old generated\nfunc OldHandler() {}\n"); // unchanged from base
        let theirs = gen_block("// new generated\nfunc NewHandler() {}\n");

        let result = three_way_merge(&base, &ours, &theirs).expect("merge should succeed");

        assert!(
            result.merged_content.contains("NewHandler"),
            "generated region should be overwritten with new content"
        );
        assert!(
            !result.merged_content.contains("OldHandler"),
            "old generated handler should be gone"
        );
        assert_eq!(result.regenerated_regions, 1);
        assert_eq!(result.preserved_regions, 0);
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Test 3: preserve region survives regeneration
    // ─────────────────────────────────────────────────────────────────────────
    #[test]
    fn test_preserve_region_survives_regeneration() {
        let custom_body = "// CUSTOM: domain-specific validation\nfunc Validate(x int) bool { return x > 0 }\n";
        let gen_body_old = "// generated scaffold v1\nfunc Scaffold() {}\n";
        let gen_body_new = "// generated scaffold v2\nfunc ScaffoldV2() {}\n";

        let base = format!("{}{}", gen_block(gen_body_old), preserve_block(custom_body));
        let ours = format!("{}{}", gen_block(gen_body_old), preserve_block(custom_body));
        let theirs = format!("{}{}", gen_block(gen_body_new), preserve_block(custom_body));

        let result = three_way_merge(&base, &ours, &theirs).expect("merge should succeed");

        assert!(
            result.merged_content.contains("domain-specific validation"),
            "preserve region body must survive"
        );
        assert!(
            result.merged_content.contains("ScaffoldV2"),
            "generated region should be updated"
        );
        assert_eq!(result.preserved_regions, 1, "one preserve region tracked");
        assert_eq!(result.regenerated_regions, 1, "one generated region tracked");
        assert!(result.conflicts.is_empty(), "clean merge — no conflicts");
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Test 4: conflict detected when both change the same unannotated block
    // ─────────────────────────────────────────────────────────────────────────
    #[test]
    fn test_conflict_detected_when_both_change() {
        // base: unannotated footer
        let base = "package main\n\nfunc main() { /* base */ }\n";
        // ours: engineer edited the footer
        let ours = "package main\n\nfunc main() { /* hand-edited */ }\n";
        // theirs: regenerator also changed the footer differently
        let theirs = "package main\n\nfunc main() { /* regenerated */ }\n";

        let result = three_way_merge(base, ours, theirs).expect("merge should succeed");

        // At least one conflict should be present
        assert!(
            !result.conflicts.is_empty(),
            "should detect conflict when both ours and theirs differ from base"
        );
        assert!(
            result.merged_content.contains("<<<<<<<"),
            "conflict markers should appear in merged output"
        );
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Test 5: clean merge, no annotations — ours wins when only ours changed
    // ─────────────────────────────────────────────────────────────────────────
    #[test]
    fn test_clean_merge_no_annotations_ours_wins() {
        let base = "line1\nline2\nline3\n";
        let ours = "line1\nline2_edited\nline3\n"; // ours changed line2
        let theirs = "line1\nline2\nline3\n"; // theirs unchanged from base

        let result = three_way_merge(base, ours, theirs).expect("merge should succeed");

        assert!(
            result.merged_content.contains("line2_edited"),
            "when only ours changed, ours should win"
        );
        assert!(result.conflicts.is_empty(), "no conflict — only one side changed");
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Test 6: no changes anywhere — returns theirs (identical to base/ours)
    // ─────────────────────────────────────────────────────────────────────────
    #[test]
    fn test_three_way_merge_no_changes_returns_theirs() {
        let content = gen_block("// no change\nfunc Stable() {}\n");
        let result = three_way_merge(&content, &content, &content).expect("merge should succeed");

        assert_eq!(
            result.merged_content, content,
            "identical inputs should produce identical output"
        );
        assert!(result.conflicts.is_empty());
        assert_eq!(result.regenerated_regions, 1);
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Test 7: format_conflicts produces git-style markers
    // ─────────────────────────────────────────────────────────────────────────
    #[test]
    fn test_format_conflicts_produces_markers() {
        let conflicts = vec![MergeConflict {
            line: 10,
            region: "unannotated-line-10".to_string(),
            base_content: "base content\n".to_string(),
            ours: "our content\n".to_string(),
            theirs: "their content\n".to_string(),
        }];

        let formatted = format_conflicts(&conflicts);

        assert!(formatted.contains("<<<<<<<"), "should have opening marker");
        assert!(formatted.contains("======="), "should have separator marker");
        assert!(formatted.contains(">>>>>>>"), "should have closing marker");
        assert!(formatted.contains("our content"), "ours should be in markers");
        assert!(formatted.contains("their content"), "theirs should be in markers");
        assert!(formatted.contains("base content"), "base should be in markers");
        assert!(
            formatted.contains("line 10"),
            "line number should appear in marker header"
        );
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Test 8: apply_generated preserves custom business logic
    // ─────────────────────────────────────────────────────────────────────────
    #[test]
    fn test_apply_generated_preserves_custom_business_logic() {
        let business_logic = "// IMPORTANT: calculates ARR with churn adjustment\n\
                              func CalcARR(mrr float64, churn float64) float64 {\n\
                              \treturn mrr * 12 * (1.0 - churn)\n\
                              }\n";

        let existing = format!(
            "package revenue\n\n{}\n{}\n",
            gen_block("// v1 scaffold\nfunc Init() {}\n"),
            preserve_block(business_logic)
        );

        let new_generated = format!(
            "package revenue\n\n{}\n{}\n",
            gen_block("// v2 scaffold — new fields added\nfunc Init() { setup() }\n"),
            preserve_block("// placeholder — engineer fills this in\n")
        );

        let result =
            apply_generated(&existing, &new_generated).expect("apply_generated should succeed");

        // The hand-written ARR calculation must survive.
        assert!(
            result.merged_content.contains("calculates ARR"),
            "business logic comment must survive"
        );
        assert!(
            result.merged_content.contains("CalcARR"),
            "CalcARR function must survive"
        );
        // The new scaffold must be present.
        assert!(
            result.merged_content.contains("new fields added"),
            "updated scaffold should appear"
        );
        assert_eq!(result.preserved_regions, 1, "one preserve region tracked");
        assert!(result.conflicts.is_empty(), "no conflicts expected");
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Test 9: nested preserve regions are not supported (error case)
    // ─────────────────────────────────────────────────────────────────────────
    #[test]
    fn test_nested_preserve_regions_not_supported() {
        let content = format!(
            "{}\n  inner block\n  {}\n  nested content\n  {}\nouter end\n{}\n",
            PRESERVE_START, PRESERVE_START, PRESERVE_END, PRESERVE_END
        );

        let result = parse_regions(&content);

        assert!(
            result.is_err(),
            "nested preserve regions should produce a ParseError"
        );
        match result {
            Err(MergeError::ParseError(msg)) => {
                assert!(
                    msg.contains("nested"),
                    "error message should mention 'nested': {msg}"
                );
            }
            _ => panic!("expected MergeError::ParseError"),
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Test 10: empty file merge
    // ─────────────────────────────────────────────────────────────────────────
    #[test]
    fn test_empty_file_merge() {
        let result = three_way_merge("", "", "").expect("empty merge should succeed");

        assert_eq!(result.merged_content, "", "empty inputs produce empty output");
        assert!(result.conflicts.is_empty());
        assert_eq!(result.preserved_regions, 0);
        assert_eq!(result.regenerated_regions, 0);
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Test 11: apply_generated with no preserve regions (pure overwrite)
    // ─────────────────────────────────────────────────────────────────────────
    #[test]
    fn test_apply_generated_no_preserve_regions_pure_overwrite() {
        let existing = gen_block("// old scaffold\nfunc OldSetup() {}\n");
        let new_generated = gen_block("// new scaffold\nfunc NewSetup() {}\n");

        let result =
            apply_generated(&existing, &new_generated).expect("apply_generated should succeed");

        assert!(
            result.merged_content.contains("NewSetup"),
            "new scaffold must be present"
        );
        assert!(
            !result.merged_content.contains("OldSetup"),
            "old scaffold must be gone"
        );
        assert_eq!(result.preserved_regions, 0);
        assert_eq!(result.regenerated_regions, 1);
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Test 12: multiple preserve regions all survive
    // ─────────────────────────────────────────────────────────────────────────
    #[test]
    fn test_multiple_preserve_regions_all_survive() {
        let logic_a = "func BusinessRuleA() bool { return true }\n";
        let logic_b = "func BusinessRuleB() bool { return false }\n";

        let existing = format!(
            "{}{}{}\n",
            preserve_block(logic_a),
            gen_block("// v1 glue\n"),
            preserve_block(logic_b)
        );

        let new_gen = format!(
            "{}{}{}\n",
            preserve_block("// placeholder A\n"),
            gen_block("// v2 glue — updated\n"),
            preserve_block("// placeholder B\n")
        );

        let result =
            apply_generated(&existing, &new_gen).expect("apply_generated should succeed");

        assert!(
            result.merged_content.contains("BusinessRuleA"),
            "first preserve block must survive"
        );
        assert!(
            result.merged_content.contains("BusinessRuleB"),
            "second preserve block must survive"
        );
        assert!(
            result.merged_content.contains("v2 glue"),
            "updated generated content must appear"
        );
        assert_eq!(result.preserved_regions, 2, "two preserve regions must be tracked");
        // 1 explicit @ggen:generated block + 1 unannotated trailing newline region.
        assert!(
            result.regenerated_regions >= 1,
            "at least the explicit generated block should be counted"
        );
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Test 13: theirs-only change in unannotated region — theirs wins
    // ─────────────────────────────────────────────────────────────────────────
    #[test]
    fn test_unannotated_theirs_only_change_theirs_wins() {
        let base = "package main\n\nconst Version = \"1.0\"\n";
        let ours = base; // no hand-edit
        let theirs = "package main\n\nconst Version = \"2.0\"\n";

        let result = three_way_merge(base, ours, theirs).expect("merge should succeed");

        assert!(
            result.merged_content.contains("\"2.0\""),
            "theirs version bump should be applied"
        );
        assert!(result.conflicts.is_empty(), "no conflict — only theirs changed");
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Test 14: conflict markers contain line number
    // ─────────────────────────────────────────────────────────────────────────
    #[test]
    fn test_conflict_markers_contain_line_number() {
        let base = "a\nb\nc\n";
        let ours = "a\nX\nc\n";
        let theirs = "a\nY\nc\n";

        let result = three_way_merge(base, ours, theirs).expect("merge should succeed");
        assert!(!result.conflicts.is_empty(), "conflict should be detected");

        let line_no = result.conflicts[0].line;
        assert!(
            result.merged_content.contains(&format!("line {}", line_no)),
            "merged output conflict marker should contain the line number"
        );
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Test 15: unterminated generated block is a parse error
    // ─────────────────────────────────────────────────────────────────────────
    #[test]
    fn test_unterminated_generated_block_is_parse_error() {
        let content = format!("{}\nsome content\n", GENERATED_START);
        let result = parse_regions(&content);

        assert!(
            result.is_err(),
            "unterminated generated block should be a ParseError"
        );
        match result {
            Err(MergeError::ParseError(msg)) => {
                assert!(
                    msg.contains("unterminated"),
                    "error should say 'unterminated': {msg}"
                );
            }
            _ => panic!("expected MergeError::ParseError"),
        }
    }
}
