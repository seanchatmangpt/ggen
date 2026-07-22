//! Hygen-semantics file writer.
//!
//! [`plan_write`] resolves the output path safely inside the project root,
//! decides what to do based on frontmatter flags and the current filesystem
//! state, and applies the decision. All ambiguous states fail closed:
//! escaping the root, injecting into a missing file, a missing marker, or
//! overwriting a differing file without `force` are all hard errors.
//!
//! Decision order (first match wins):
//! 1. path escapes root / traversal → `Err`
//! 2. `unless_exists` && target exists → `Skipped`
//! 3. `skip_if` substring present in existing file → `Skipped`
//! 4. `freeze_policy: always` && target exists → `Skipped`
//! 5. `freeze_policy: checksum` && on-disk content no longer matches ggen's
//!    last-recorded checksum → `Skipped` (human edit detected)
//! 6. `inject` → insert into existing file (`before` / `after` / `at_line` /
//!    append); target missing or marker missing → `Err`; `backup: true` and
//!    a target existed → `<target>.bak` written first
//! 7. `force` → overwrite (`backup: true` and a target existed → backup
//!    first) → `Written`
//! 8. default: absent → `Written`; identical → `Skipped`; differs → `Err`
//!
//! After any successful `Written`/`Injected` outcome under
//! `freeze_policy: checksum`, the new content's BLAKE3 checksum is recorded
//! under `freeze_slots_dir` for the next run to compare against.

use std::path::{Component, Path, PathBuf};

use crate::{
    error::{AppError, Result},
    template::{FreezePolicy, Frontmatter},
};

/// Hard cap on one rendered template's output size. A template producing
/// more than this is almost certainly an unbounded loop over query results
/// or a runaway `{% for %}` — refusing loudly beats writing a
/// multi-hundred-MB file no editor or `git diff` can handle.
pub const MAX_OUTPUT_BYTES: usize = 10 * 1024 * 1024;

/// Outcome of a planned-and-applied write.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WriteOutcome {
    /// The file was created or overwritten with the rendered body.
    Written,
    /// Nothing was written; the reason is recorded.
    Skipped(String),
    /// Content was injected into an existing file.
    Injected,
}

/// Decide and apply a write of `rendered_body` to `rel_to` under `root`,
/// following Hygen semantics driven by `frontmatter`.
///
/// Parent directories are created as needed. See the module docs for the
/// full decision table.
///
/// # Errors
/// - `[FM-WRITE-001]` root missing / not canonicalizable.
/// - `[FM-WRITE-002]` `rel_to` is absolute, contains `..`, or resolves
///   outside the root.
/// - `[FM-WRITE-003]` inject requested but the target file does not exist.
/// - `[FM-WRITE-004]` inject marker (`before`/`after`) not found, or
///   `at_line` beyond end of file.
/// - `[FM-WRITE-005]` target exists with differing content and `force` is
///   not set (refuse silent clobber).
/// - `[FM-WRITE-006]` `freeze_policy: checksum` set without `freeze_slots_dir`.
/// - `[FM-WRITE-007]` rendered body exceeds [`MAX_OUTPUT_BYTES`].
/// - I/O errors from reading/writing the filesystem.
pub fn plan_write(
    root: &Path, rel_to: &str, rendered_body: &str, frontmatter: &Frontmatter,
) -> Result<WriteOutcome> {
    if rendered_body.len() > MAX_OUTPUT_BYTES {
        return Err(AppError::fm_write(
            7,
            format!(
                "rendered output for `{rel_to}` is {} bytes, over the {MAX_OUTPUT_BYTES}-byte cap. \
                 Remediation: check the template for an unbounded loop over query results, or \
                 split it into multiple templates/output files.",
                rendered_body.len()
            ),
        ));
    }
    let target = resolve_target(root, rel_to)?;
    let exists = target.exists();
    let existing = if exists {
        Some(std::fs::read_to_string(&target)?)
    } else {
        None
    };

    if frontmatter.unless_exists && exists {
        return Ok(WriteOutcome::Skipped(format!(
            "unless_exists: {} already exists",
            target.display()
        )));
    }

    if let (Some(needle), Some(content)) = (frontmatter.skip_if.as_deref(), existing.as_deref()) {
        if !needle.is_empty() && content.contains(needle) {
            return Ok(WriteOutcome::Skipped(format!(
                "skip_if: existing file already contains {needle:?}"
            )));
        }
    }

    if let Some(skip_reason) = check_freeze(
        root,
        rel_to,
        existing.as_deref(),
        rendered_body,
        frontmatter,
    )? {
        return Ok(WriteOutcome::Skipped(skip_reason));
    }

    if frontmatter.inject {
        let content = existing.ok_or_else(|| {
            AppError::fm_write(
                3,
                format!(
                    "inject target {} does not exist. \
                     Remediation: create the file first or drop `inject: true`.",
                    target.display()
                ),
            )
        })?;
        maybe_backup(&target, &content, frontmatter)?;
        let injected = inject_into(&content, rendered_body, frontmatter)?;
        std::fs::write(&target, &injected)?;
        record_freeze_checksum(root, rel_to, &injected, frontmatter)?;
        return Ok(WriteOutcome::Injected);
    }

    match existing {
        None => {
            ensure_parent(&target)?;
            std::fs::write(&target, rendered_body)?;
            record_freeze_checksum(root, rel_to, rendered_body, frontmatter)?;
            Ok(WriteOutcome::Written)
        }
        Some(ref content) if frontmatter.force => {
            maybe_backup(&target, content, frontmatter)?;
            std::fs::write(&target, rendered_body)?;
            record_freeze_checksum(root, rel_to, rendered_body, frontmatter)?;
            Ok(WriteOutcome::Written)
        }
        Some(ref content) if content == rendered_body => Ok(WriteOutcome::Skipped(
            "unchanged: content identical".to_string(),
        )),
        Some(_) => Err(AppError::fm_write(
            5,
            format!(
                "{} exists with differing content; refusing silent clobber. \
                 Remediation: set `force: true` to overwrite intentionally.",
                target.display()
            ),
        )),
    }
}

/// Evaluate `frontmatter.freeze_policy` against the current on-disk state.
/// Returns `Some(reason)` when the write must be skipped on freeze grounds.
///
/// `rendered_body` is the candidate content this run would have written had
/// freeze not intervened. It is used ONLY to observe and report drift for
/// `FreezePolicy::Always` (see that arm) -- never written to disk. Freeze
/// semantics are unchanged: an `Always`-frozen target is never overwritten,
/// drifted or not.
fn check_freeze(
    root: &Path, rel_to: &str, existing: Option<&str>, rendered_body: &str,
    frontmatter: &Frontmatter,
) -> Result<Option<String>> {
    let Some(policy) = frontmatter.freeze_policy else {
        return Ok(None);
    };
    match policy {
        FreezePolicy::Never => Ok(None),
        FreezePolicy::Always => match existing {
            None => Ok(None),
            Some(content) if content == rendered_body => Ok(Some(
                "frozen: freeze_policy=always, target already exists (up to date, no drift)"
                    .to_string(),
            )),
            Some(_) => Ok(Some(
                "frozen: freeze_policy=always, target already exists -- DRIFT: candidate \
                 content would differ from the on-disk (frozen) file, meaning the source \
                 ontology/template has changed since this file was last generated or \
                 hand-completed, but the frozen file was NOT updated to reflect it. \
                 Remediation: review the drift and hand-update the frozen file, or drop \
                 freeze_policy if it should track generation again."
                    .to_string(),
            )),
        },
        FreezePolicy::Checksum => {
            let Some(content) = existing else {
                return Ok(None);
            };
            let slots_dir = freeze_slots_dir(frontmatter)?;
            let checksum_path = freeze_checksum_path(root, slots_dir, rel_to)?;
            match std::fs::read_to_string(&checksum_path) {
                Ok(stored) => {
                    let current = blake3::hash(content.as_bytes()).to_hex().to_string();
                    if stored.trim() == current {
                        Ok(None) // untouched since last generation; safe to regenerate
                    } else {
                        Ok(Some(
                            "frozen: freeze_policy=checksum, on-disk content no longer matches \
                             ggen's last-recorded checksum (manual edit detected)"
                                .to_string(),
                        ))
                    }
                }
                Err(_) => Ok(None), // no prior checksum recorded yet; proceed normally
            }
        }
    }
}

/// After a successful write under `freeze_policy: checksum`, record the new
/// content's BLAKE3 checksum so the next run can detect manual edits.
fn record_freeze_checksum(
    root: &Path, rel_to: &str, written_content: &str, frontmatter: &Frontmatter,
) -> Result<()> {
    if frontmatter.freeze_policy != Some(FreezePolicy::Checksum) {
        return Ok(());
    }
    let slots_dir = freeze_slots_dir(frontmatter)?;
    let checksum_path = freeze_checksum_path(root, slots_dir, rel_to)?;
    if let Some(parent) = checksum_path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let hash = blake3::hash(written_content.as_bytes())
        .to_hex()
        .to_string();
    std::fs::write(&checksum_path, hash)?;
    Ok(())
}

fn freeze_slots_dir(frontmatter: &Frontmatter) -> Result<&str> {
    frontmatter.freeze_slots_dir.as_deref().ok_or_else(|| {
        AppError::fm_write(
            6,
            "freeze_policy: checksum requires freeze_slots_dir to be set. \
             Remediation: add `freeze_slots_dir: <dir>` to the frontmatter.",
        )
    })
}

/// `freeze_slots_dir` is a frontmatter path field, so it goes through the
/// same [`resolve_target`] safety check as `to:`/`from:` — an absolute or
/// `..`-containing slots dir must not place checksum files outside the root.
fn freeze_checksum_path(root: &Path, slots_dir: &str, rel_to: &str) -> Result<PathBuf> {
    resolve_target(root, &format!("{slots_dir}/{rel_to}.blake3"))
}

/// If `frontmatter.backup` is set, copy `existing_content` to `<target>.bak`
/// before it is overwritten.
fn maybe_backup(target: &Path, existing_content: &str, frontmatter: &Frontmatter) -> Result<()> {
    if !frontmatter.backup {
        return Ok(());
    }
    let mut backup_path = target.as_os_str().to_owned();
    backup_path.push(".bak");
    std::fs::write(PathBuf::from(backup_path), existing_content)?;
    Ok(())
}

/// Resolve `rel_to` under `root`, rejecting absolute paths, `..` components,
/// and any resolution that escapes the canonicalized root. Shared with
/// `sync::parse_template_file`'s `from:` resolution — any frontmatter field
/// that reads or writes a filesystem path relative to some base directory
/// should route through this same check, not re-implement it.
pub(crate) fn resolve_target(root: &Path, rel_to: &str) -> Result<PathBuf> {
    let root_c = root.canonicalize().map_err(|e| {
        AppError::fm_write(
            1,
            format!("project root {} not canonicalizable: {e}", root.display()),
        )
    })?;
    let rel = Path::new(rel_to);
    if rel.is_absolute() {
        return Err(AppError::fm_write(
            2,
            format!("`to:` path must be relative, got absolute {rel_to:?}"),
        ));
    }
    for component in rel.components() {
        match component {
            Component::Normal(_) | Component::CurDir => {}
            _ => {
                return Err(AppError::fm_write(
                    2,
                    format!(
                        "`to:` path {rel_to:?} contains a traversal component; \
                         it must stay inside the project root"
                    ),
                ));
            }
        }
    }
    let target = root_c.join(rel);
    // Belt and braces: canonicalize the nearest existing ancestor and verify
    // it is still under the root (symlink escapes).
    let mut probe = target
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| root_c.clone());
    while !probe.exists() {
        match probe.parent() {
            Some(p) => probe = p.to_path_buf(),
            None => break,
        }
    }
    let probe_c = probe.canonicalize().map_err(|e| {
        AppError::fm_write(2, format!("cannot canonicalize {}: {e}", probe.display()))
    })?;
    if !probe_c.starts_with(&root_c) {
        return Err(AppError::fm_write(
            2,
            format!(
                "resolved path {} escapes project root {}",
                target.display(),
                root_c.display()
            ),
        ));
    }
    Ok(target)
}

/// Create the parent directory chain for `target`.
fn ensure_parent(target: &Path) -> Result<()> {
    if let Some(parent) = target.parent() {
        std::fs::create_dir_all(parent)?;
    }
    Ok(())
}

/// Compute the injected file content: `before` marker / `after` marker /
/// `at_line` (1-based) / else append. Missing marker or out-of-range line
/// fails closed.
fn inject_into(existing: &str, body: &str, fm: &Frontmatter) -> Result<String> {
    let mut lines: Vec<&str> = existing.lines().collect();
    let body_lines: Vec<&str> = body.lines().collect();

    let insert_at: usize = if let Some(marker) = fm.before.as_deref() {
        find_marker_line(&lines, marker, "before")?
    } else if let Some(marker) = fm.after.as_deref() {
        find_marker_line(&lines, marker, "after")? + 1
    } else if let Some(at) = fm.at_line {
        if at == 0 || at > lines.len() + 1 {
            return Err(AppError::fm_write(
                4,
                format!(
                    "at_line {at} out of range (file has {} lines; valid range 1..={})",
                    lines.len(),
                    lines.len() + 1
                ),
            ));
        }
        at - 1
    } else {
        lines.len()
    };

    lines.splice(insert_at..insert_at, body_lines);
    let mut out = lines.join("\n");
    if existing.ends_with('\n') || !existing.contains('\n') {
        out.push('\n');
    }
    Ok(out)
}

/// Index of the first line containing `marker`, or a fail-closed error.
fn find_marker_line(lines: &[&str], marker: &str, kind: &str) -> Result<usize> {
    lines
        .iter()
        .position(|l| l.contains(marker))
        .ok_or_else(|| {
            AppError::fm_write(
                4,
                format!(
                    "inject `{kind}:` marker {marker:?} not found in target file. \
                     Remediation: add the marker line or fix the frontmatter."
                ),
            )
        })
}

#[cfg(test)]
mod tests {
    use tempfile::TempDir;

    use super::*;

    fn fm(to: &str) -> Frontmatter {
        let yaml = format!("to: {to}");
        serde_yaml::from_str(&yaml).expect("frontmatter")
    }

    #[test]
    fn path_traversal_is_err() {
        let dir = TempDir::new().expect("tempdir");
        let f = fm("../evil.rs");
        let err = plan_write(dir.path(), "../evil.rs", "boom", &f).expect_err("must reject");
        assert!(err.to_string().contains("FM-WRITE-002"), "{err}");
        assert!(!dir
            .path()
            .parent()
            .expect("parent")
            .join("evil.rs")
            .exists());
    }

    #[test]
    fn absolute_to_is_err() {
        let dir = TempDir::new().expect("tempdir");
        let err =
            plan_write(dir.path(), "/tmp/evil.rs", "boom", &fm("x")).expect_err("must reject");
        assert!(err.to_string().contains("FM-WRITE-002"), "{err}");
    }

    #[test]
    fn writes_new_file_and_creates_parents() {
        let dir = TempDir::new().expect("tempdir");
        let out =
            plan_write(dir.path(), "a/b/mod.rs", "pub mod x;\n", &fm("a/b/mod.rs")).expect("write");
        assert_eq!(out, WriteOutcome::Written);
        let content = std::fs::read_to_string(dir.path().join("a/b/mod.rs")).expect("read back");
        assert_eq!(content, "pub mod x;\n");
    }

    #[test]
    fn identical_content_skips_unchanged() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("x.rs"), "same\n").expect("seed");
        let out = plan_write(dir.path(), "x.rs", "same\n", &fm("x.rs")).expect("plan");
        assert!(matches!(out, WriteOutcome::Skipped(ref r) if r.contains("unchanged")));
    }

    #[test]
    fn differing_content_without_force_is_err() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("x.rs"), "old\n").expect("seed");
        let err = plan_write(dir.path(), "x.rs", "new\n", &fm("x.rs")).expect_err("must refuse");
        assert!(err.to_string().contains("FM-WRITE-005"), "{err}");
        assert_eq!(
            std::fs::read_to_string(dir.path().join("x.rs")).expect("read"),
            "old\n"
        );
    }

    #[test]
    fn force_overwrites() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("x.rs"), "old\n").expect("seed");
        let mut f = fm("x.rs");
        f.force = true;
        let out = plan_write(dir.path(), "x.rs", "new\n", &f).expect("plan");
        assert_eq!(out, WriteOutcome::Written);
        assert_eq!(
            std::fs::read_to_string(dir.path().join("x.rs")).expect("read"),
            "new\n"
        );
    }

    #[test]
    fn unless_exists_skips_existing() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("x.rs"), "keep\n").expect("seed");
        let mut f = fm("x.rs");
        f.unless_exists = true;
        let out = plan_write(dir.path(), "x.rs", "new\n", &f).expect("plan");
        assert!(matches!(out, WriteOutcome::Skipped(ref r) if r.contains("unless_exists")));
        assert_eq!(
            std::fs::read_to_string(dir.path().join("x.rs")).expect("read"),
            "keep\n"
        );
    }

    #[test]
    fn inject_after_marker_and_skip_if_idempotent() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("mod.rs"), "// modules\npub mod a;\n").expect("seed");
        let mut f = fm("mod.rs");
        f.inject = true;
        f.after = Some("// modules".to_string());
        f.skip_if = Some("pub mod b;".to_string());

        let out = plan_write(dir.path(), "mod.rs", "pub mod b;", &f).expect("inject");
        assert_eq!(out, WriteOutcome::Injected);
        assert_eq!(
            std::fs::read_to_string(dir.path().join("mod.rs")).expect("read"),
            "// modules\npub mod b;\npub mod a;\n"
        );

        // Second application: skip_if makes it a no-op.
        let out2 = plan_write(dir.path(), "mod.rs", "pub mod b;", &f).expect("second");
        assert!(matches!(out2, WriteOutcome::Skipped(ref r) if r.contains("skip_if")));
        assert_eq!(
            std::fs::read_to_string(dir.path().join("mod.rs")).expect("read"),
            "// modules\npub mod b;\npub mod a;\n"
        );
    }

    #[test]
    fn inject_before_marker() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("f.txt"), "one\ntwo\n").expect("seed");
        let mut f = fm("f.txt");
        f.inject = true;
        f.before = Some("two".to_string());
        plan_write(dir.path(), "f.txt", "middle", &f).expect("inject");
        assert_eq!(
            std::fs::read_to_string(dir.path().join("f.txt")).expect("read"),
            "one\nmiddle\ntwo\n"
        );
    }

    #[test]
    fn inject_at_line_one_based() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("f.txt"), "one\ntwo\n").expect("seed");
        let mut f = fm("f.txt");
        f.inject = true;
        f.at_line = Some(1);
        plan_write(dir.path(), "f.txt", "zero", &f).expect("inject");
        assert_eq!(
            std::fs::read_to_string(dir.path().join("f.txt")).expect("read"),
            "zero\none\ntwo\n"
        );
    }

    #[test]
    fn inject_missing_target_is_err() {
        let dir = TempDir::new().expect("tempdir");
        let mut f = fm("nope.rs");
        f.inject = true;
        let err = plan_write(dir.path(), "nope.rs", "x", &f).expect_err("must refuse");
        assert!(err.to_string().contains("FM-WRITE-003"), "{err}");
    }

    #[test]
    fn inject_missing_marker_is_err() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("f.txt"), "one\n").expect("seed");
        let mut f = fm("f.txt");
        f.inject = true;
        f.after = Some("// nowhere".to_string());
        let err = plan_write(dir.path(), "f.txt", "x", &f).expect_err("must refuse");
        assert!(err.to_string().contains("FM-WRITE-004"), "{err}");
    }

    #[test]
    fn freeze_slots_dir_traversal_is_err() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("x.rs"), "old\n").expect("seed");
        let mut f = fm("x.rs");
        f.freeze_policy = Some(FreezePolicy::Checksum);
        f.freeze_slots_dir = Some("../escaped-slots".to_string());
        let err = plan_write(dir.path(), "x.rs", "old\n", &f).expect_err("must reject");
        assert!(err.to_string().contains("FM-WRITE-002"), "{err}");
        assert!(!dir
            .path()
            .parent()
            .expect("parent")
            .join("escaped-slots")
            .exists());
    }

    #[test]
    fn freeze_slots_dir_absolute_is_err() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("x.rs"), "old\n").expect("seed");
        let mut f = fm("x.rs");
        f.freeze_policy = Some(FreezePolicy::Checksum);
        f.freeze_slots_dir = Some("/tmp/escaped-slots".to_string());
        let err = plan_write(dir.path(), "x.rs", "old\n", &f).expect_err("must reject");
        assert!(err.to_string().contains("FM-WRITE-002"), "{err}");
    }

    #[test]
    fn inject_without_position_appends() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("f.txt"), "one\n").expect("seed");
        let mut f = fm("f.txt");
        f.inject = true;
        plan_write(dir.path(), "f.txt", "two", &f).expect("inject");
        assert_eq!(
            std::fs::read_to_string(dir.path().join("f.txt")).expect("read"),
            "one\ntwo\n"
        );
    }

    // -----------------------------------------------------------------
    // FreezePolicy::Always drift observation (O-5 / FreezeAlwaysNoDriftDetection)
    // -----------------------------------------------------------------

    fn fm_always(to: &str) -> Frontmatter {
        let mut f = fm(to);
        f.freeze_policy = Some(FreezePolicy::Always);
        f
    }

    #[test]
    fn freeze_always_absent_target_writes_normally() {
        let dir = TempDir::new().expect("tempdir");
        let out = plan_write(dir.path(), "x.rs", "new\n", &fm_always("x.rs")).expect("plan");
        assert_eq!(out, WriteOutcome::Written);
        assert_eq!(
            std::fs::read_to_string(dir.path().join("x.rs")).expect("read"),
            "new\n"
        );
    }

    #[test]
    fn freeze_always_identical_content_skips_with_no_drift_reason() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("x.rs"), "same\n").expect("seed");
        let out = plan_write(dir.path(), "x.rs", "same\n", &fm_always("x.rs")).expect("plan");
        match out {
            WriteOutcome::Skipped(reason) => {
                assert!(reason.contains("no drift"), "{reason}");
                assert!(!reason.contains("DRIFT:"), "{reason}");
            }
            other => panic!("expected Skipped, got {other:?}"),
        }
        // Freeze semantics unchanged: file on disk is untouched.
        assert_eq!(
            std::fs::read_to_string(dir.path().join("x.rs")).expect("read"),
            "same\n"
        );
    }

    #[test]
    fn freeze_always_drifted_content_skips_but_reports_drift() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("x.rs"), "hand-completed old content\n").expect("seed");
        let out = plan_write(
            dir.path(),
            "x.rs",
            "regenerated new content\n",
            &fm_always("x.rs"),
        )
        .expect("plan");
        match out {
            WriteOutcome::Skipped(reason) => {
                assert!(reason.contains("DRIFT:"), "{reason}");
            }
            other => panic!("expected Skipped, got {other:?}"),
        }
        // The whole point: freeze mutation behavior is unchanged. The
        // hand-completed file is NEVER overwritten by Always, drifted or not.
        assert_eq!(
            std::fs::read_to_string(dir.path().join("x.rs")).expect("read"),
            "hand-completed old content\n"
        );
    }

    /// Sabotage: a hand-completed bootstrap file (the exact scenario
    /// freeze_policy=always exists for, per CLAUDE.md's mode=Create
    /// precedent) that has diverged from what generation would now produce
    /// must be reported as drifted, not silently treated as up to date --
    /// proving the comparison actually runs, not just returns a constant.
    #[test]
    fn freeze_always_hand_completed_bootstrap_file_reports_real_drift() {
        let dir = TempDir::new().expect("tempdir");
        let hand_completed = "// bootstrap stub, hand-completed by a human after first generation\nfn analyzer() { /* real hand-written logic */ }\n";
        std::fs::write(dir.path().join("analyzer.rs"), hand_completed).expect("seed");
        let regenerated_candidate =
            "// bootstrap stub, hand-completed by a human after first generation\nfn analyzer() { todo!() }\n";
        let out = plan_write(
            dir.path(),
            "analyzer.rs",
            regenerated_candidate,
            &fm_always("analyzer.rs"),
        )
        .expect("plan");
        assert!(
            matches!(out, WriteOutcome::Skipped(ref r) if r.contains("DRIFT:")),
            "{out:?}"
        );
        assert_eq!(
            std::fs::read_to_string(dir.path().join("analyzer.rs")).expect("read"),
            hand_completed,
            "hand-completed content must survive untouched"
        );
    }

    /// Second-sync idempotency under Always: running the identical plan
    /// twice in a row must report the SAME (no-drift) outcome both times --
    /// observation must not itself introduce nondeterminism.
    #[test]
    fn freeze_always_second_sync_idempotent_when_no_drift() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("x.rs"), "stable\n").expect("seed");
        let out1 = plan_write(dir.path(), "x.rs", "stable\n", &fm_always("x.rs")).expect("plan1");
        let out2 = plan_write(dir.path(), "x.rs", "stable\n", &fm_always("x.rs")).expect("plan2");
        assert_eq!(out1, out2);
        match out1 {
            WriteOutcome::Skipped(ref r) => assert!(r.contains("no drift"), "{r}"),
            other => panic!("expected Skipped, got {other:?}"),
        }
    }
}
