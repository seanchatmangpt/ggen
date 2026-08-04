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

use regex::{Regex, RegexBuilder};

use crate::{
    error::{AppError, Result},
    template::{
        FreezePolicy, Frontmatter, MatchKind, MatchOccurrence, MatchRule, MatchScope, MatchSpec,
    },
};

/// Hard cap on one rendered template's output size. A template producing
/// more than this is almost certainly an unbounded loop over query results
/// or a runaway `{% for %}` — refusing loudly beats writing a
/// multi-hundred-MB file no editor or `git diff` can handle.
pub const MAX_OUTPUT_BYTES: usize = 10 * 1024 * 1024;

/// Hard cap on a rendered host-content pattern. Rust's regex engine is
/// linear-time, but bounded pattern size still prevents pathological compile
/// memory and keeps frontmatter reviewable.
pub const MAX_MATCH_PATTERN_BYTES: usize = 64 * 1024;

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
/// - `[FM-WRITE-008]` invalid or oversized matcher, zero-width file regex,
///   or unsatisfied structured cardinality.
/// - I/O errors from reading/writing the filesystem.
pub fn plan_write(
    root: &Path, rel_to: &str, rendered_body: &str, frontmatter: &Frontmatter,
) -> Result<WriteOutcome> {
    validate_match_specs(frontmatter)?;

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

    if let (Some(selector), Some(content)) = (frontmatter.skip_if.as_ref(), existing.as_deref()) {
        // Historical empty `skip_if: ""` was a no-op; preserve that exact
        // compatibility behavior while structured empty patterns refuse.
        if !matches!(selector, MatchSpec::Literal(needle) if needle.is_empty()) {
            let observation = observe_match(content, selector, MatchUse::SkipIf)?;
            if observation.selected.is_some() {
                let reason = if let MatchSpec::Literal(needle) = selector {
                    format!("skip_if: existing file already contains {needle:?}")
                } else {
                    format!("skip_if: {}", observation.describe())
                };
                return Ok(WriteOutcome::Skipped(reason));
            }
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
        let injected = inject_into(&content, rendered_body, frontmatter)?;
        if injected.len() > MAX_OUTPUT_BYTES {
            return Err(AppError::fm_write(
                7,
                format!(
                    "injected output for `{rel_to}` is {} bytes, over the                      {MAX_OUTPUT_BYTES}-byte cap. Remediation: split the host artifact                      or reduce the injected projection.",
                    injected.len()
                ),
            ));
        }
        maybe_backup(&target, &content, frontmatter)?;
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
        // Content-equality is checked BEFORE the `force` arm (2026-08-03,
        // TECH-DEBT-003 fix): the original ordering put `frontmatter.force` first,
        // so match-arm shadowing meant ANY `force: true` template always reported
        // `Written` and always performed a real disk write, even when the rendered
        // content was byte-identical to what was already on disk -- defeating
        // idempotent second-sync detection (`WriteOutcome::Skipped`) for every
        // force-writing template. This was dormant/unexercised until
        // packs/chicago-tdd-tools-pack's four templates gained `force: true`
        // (2026-08-03, same-day adversarial-review follow-up, see that pack's
        // pack.toml `description`), at which point
        // crates/ggen-engine/tests/cross_pack_matrix.rs's
        // `mega_project_all_packs_sync` caught it for real: a second `sync run`
        // reported `docs/chicago_tdd_tools_boundary.md` as `written` instead of
        // `skipped: unchanged`. `force` only needs to change behavior when content
        // actually differs (permit the overwrite instead of refusing with
        // FM-WRITE-005); it was never meant to disable the identical-content skip
        // path. See the `force_overwrites` test below (still forces a write when
        // content differs) and the new `force_skips_when_content_identical` test
        // (this fix's regression guard).
        Some(ref content) if content == rendered_body => Ok(WriteOutcome::Skipped(
            "unchanged: content identical".to_string(),
        )),
        Some(ref content) if frontmatter.force => {
            maybe_backup(&target, content, frontmatter)?;
            std::fs::write(&target, rendered_body)?;
            record_freeze_checksum(root, rel_to, rendered_body, frontmatter)?;
            Ok(WriteOutcome::Written)
        }
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
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(error) => Err(AppError::fm_write(
            11,
            format!(
                "checksum ownership slot `{}` is unreadable: {error}. Refusing to                  treat an unknown ownership state as unfrozen. Remediation: repair or                  remove the slot after reviewing the target.",
                checksum_path.display()
            ),
        )),
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

/// Admit checksum ownership state before shell hooks. `NotFound` means no prior
/// slot; every other read error is an unknown authority state and fails closed.
pub(crate) fn preflight_checksum_slot(
    root: &Path, rel_to: &str, frontmatter: &Frontmatter,
) -> Result<()> {
    if frontmatter.freeze_policy != Some(FreezePolicy::Checksum) {
        return Ok(());
    }
    let target = resolve_target(root, rel_to)?;
    if !target.exists() {
        return Ok(());
    }
    let slots_dir = freeze_slots_dir(frontmatter)?;
    let checksum_path = freeze_checksum_path(root, slots_dir, rel_to)?;
    match std::fs::read_to_string(&checksum_path) {
        Ok(_) => Ok(()),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(AppError::fm_write(
            11,
            format!(
                "checksum ownership slot `{}` is unreadable before actuation: {error}.                  Remediation: repair or remove the slot after reviewing ownership.",
                checksum_path.display()
            ),
        )),
    }
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
///
/// # Errors
/// - `[FM-WRITE-001]` `root` does not exist or cannot be canonicalized.
/// - `[FM-WRITE-002]` `rel_to` is absolute, contains a traversal component, or
///   resolves (directly or via a symlink) outside `root`.
pub fn resolve_target(root: &Path, rel_to: &str) -> Result<PathBuf> {
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
    // Belt and braces: canonicalize the nearest existing path -- starting at
    // the TARGET ITSELF, not its parent -- and verify it is still under the
    // root (symlink escapes). Starting at the parent (the historical bug)
    // means a target whose own leaf component is a symlink is never
    // resolved or checked: only the surrounding directory chain was. A
    // symlinked leaf pointing outside the root would then sail through here
    // and get followed for real by the later `fs::write`.
    //
    // `symlink_metadata` (not `exists`, which follows the final symlink) is
    // used for the "does something occupy this path" test, so a symlink
    // whose destination happens to be missing still counts as present here
    // and gets its link chain canonicalized/checked, instead of silently
    // falling through to the parent directory the way a nonexistent path
    // would.
    let mut probe = target.clone();
    while probe.symlink_metadata().is_err() {
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

#[derive(Debug, Clone, Copy)]
enum MatchUse {
    Before,
    After,
    SkipIf,
}

impl MatchUse {
    const fn label(self) -> &'static str {
        match self {
            Self::Before => "before",
            Self::After => "after",
            Self::SkipIf => "skip_if",
        }
    }

    const fn default_scope(self) -> MatchScope {
        match self {
            Self::Before | Self::After => MatchScope::Line,
            Self::SkipIf => MatchScope::File,
        }
    }

    const fn requires_match(self) -> bool {
        matches!(self, Self::Before | Self::After)
    }
}

#[derive(Debug, Clone, Copy)]
struct MatchSpan {
    start_line: usize,
    end_line: usize,
}

#[derive(Debug)]
struct MatchObservation {
    matcher: MatchKind,
    scope: MatchScope,
    occurrence: MatchOccurrence,
    count: usize,
    selected: Option<MatchSpan>,
}

impl MatchObservation {
    fn describe(&self) -> String {
        let selected = self.selected.map_or_else(
            || "none".to_string(),
            |span| {
                if self.scope == MatchScope::Line || span.start_line == span.end_line {
                    format!("line {}", span.start_line + 1)
                } else {
                    format!("lines {}-{}", span.start_line + 1, span.end_line + 1)
                }
            },
        );
        format!(
            "matcher={:?}, scope={:?}, occurrence={:?}, matches={}, selected={selected}",
            self.matcher, self.scope, self.occurrence, self.count
        )
        .to_lowercase()
    }
}

#[derive(Debug)]
enum CompiledMatcher<'a> {
    Contains(&'a str),
    Exact(&'a str),
    Regex(Regex),
}

impl CompiledMatcher<'_> {
    fn is_match(&self, candidate: &str) -> bool {
        match self {
            Self::Contains(pattern) => candidate.contains(pattern),
            Self::Exact(pattern) => candidate == *pattern,
            Self::Regex(regex) => regex.is_match(candidate),
        }
    }

    fn find_spans(&self, candidate: &str) -> Vec<(usize, usize)> {
        match self {
            Self::Contains(pattern) => candidate
                .match_indices(*pattern)
                .map(|(start, value)| (start, start + value.len()))
                .collect(),
            Self::Exact(pattern) if candidate == *pattern => vec![(0, candidate.len())],
            Self::Exact(_) => Vec::new(),
            Self::Regex(regex) => regex
                .find_iter(candidate)
                .map(|m| (m.start(), m.end()))
                .collect(),
        }
    }
}

struct ResolvedMatch<'a> {
    pattern: &'a str,
    matcher: MatchKind,
    scope: MatchScope,
    occurrence: MatchOccurrence,
    index: usize,
    case_sensitive: bool,
    trim: bool,
}

fn resolve_match(spec: &MatchSpec, use_: MatchUse) -> Result<ResolvedMatch<'_>> {
    let resolved = match spec {
        MatchSpec::Literal(pattern) => ResolvedMatch {
            pattern,
            matcher: MatchKind::Contains,
            scope: use_.default_scope(),
            occurrence: MatchOccurrence::First,
            index: 1,
            case_sensitive: true,
            trim: false,
        },
        MatchSpec::Structured(MatchRule {
            pattern,
            matcher,
            scope,
            occurrence,
            index,
            case_sensitive,
            trim,
        }) => ResolvedMatch {
            pattern,
            matcher: *matcher,
            scope: if *scope == MatchScope::Auto {
                use_.default_scope()
            } else {
                *scope
            },
            occurrence: *occurrence,
            index: *index,
            case_sensitive: *case_sensitive,
            trim: *trim,
        },
    };

    if resolved.pattern.len() > MAX_MATCH_PATTERN_BYTES {
        return Err(AppError::fm_write(
            8,
            format!(
                "{} pattern is {} bytes, over the {MAX_MATCH_PATTERN_BYTES}-byte cap",
                use_.label(),
                resolved.pattern.len()
            ),
        ));
    }
    if matches!(spec, MatchSpec::Structured(_)) && resolved.pattern.is_empty() {
        return Err(AppError::fm_write(
            8,
            format!("{} structured pattern must not be empty", use_.label()),
        ));
    }
    if resolved.index == 0 {
        return Err(AppError::fm_write(
            8,
            format!(
                "{} matcher index is one-based and must be at least 1",
                use_.label()
            ),
        ));
    }
    Ok(resolved)
}

fn compile_matcher<'a>(resolved: &ResolvedMatch<'a>, label: &str) -> Result<CompiledMatcher<'a>> {
    if resolved.case_sensitive {
        match resolved.matcher {
            MatchKind::Contains => return Ok(CompiledMatcher::Contains(resolved.pattern)),
            MatchKind::Exact => return Ok(CompiledMatcher::Exact(resolved.pattern)),
            MatchKind::Regex => {}
        }
    }

    let expression = match resolved.matcher {
        MatchKind::Contains => regex::escape(resolved.pattern),
        MatchKind::Exact => format!(r"\A{}\z", regex::escape(resolved.pattern)),
        MatchKind::Regex => resolved.pattern.to_string(),
    };

    let regex = RegexBuilder::new(&expression)
        .case_insensitive(!resolved.case_sensitive)
        .size_limit(1024 * 1024)
        .build()
        .map_err(|error| {
            AppError::fm_write(
                8,
                format!(
                    "{label} matcher rejected pattern {:?}: {error}. \
                     Remediation: fix the pattern or use matcher: contains/exact.",
                    resolved.pattern
                ),
            )
        })?;
    Ok(CompiledMatcher::Regex(regex))
}

fn candidate_view(candidate: &str, trim: bool) -> (&str, usize) {
    if !trim {
        return (candidate, 0);
    }
    let trimmed_start = candidate.trim_start();
    let start = candidate.len() - trimmed_start.len();
    let trimmed = trimmed_start.trim_end();
    (trimmed, start)
}

fn line_for_offset(content: &str, offset: usize) -> usize {
    content
        .as_bytes()
        .iter()
        .take(offset.min(content.len()))
        .filter(|byte| **byte == b'\n')
        .count()
}

fn select_span(
    spans: &[MatchSpan], resolved: &ResolvedMatch<'_>, use_: MatchUse,
) -> Result<Option<MatchSpan>> {
    let selected = match resolved.occurrence {
        MatchOccurrence::First => spans.first().copied(),
        MatchOccurrence::Last => spans.last().copied(),
        MatchOccurrence::Unique => match spans {
            [] => None,
            [only] => Some(*only),
            _ => {
                return Err(AppError::fm_write(
                    8,
                    format!(
                        "{} occurrence=unique expected exactly one match, observed {}",
                        use_.label(),
                        spans.len()
                    ),
                ));
            }
        },
        MatchOccurrence::Nth => spans.get(resolved.index - 1).copied(),
    };

    if selected.is_none() && use_.requires_match() {
        return Err(AppError::fm_write(
            4,
            format!(
                "inject `{}` selector found no admissible match (observed {}, occurrence={:?}, index={}). \
                 Remediation: fix the pattern/cardinality or add the intended host slot.",
                use_.label(),
                spans.len(),
                resolved.occurrence,
                resolved.index
            ),
        ));
    }
    Ok(selected)
}

fn observe_match(content: &str, spec: &MatchSpec, use_: MatchUse) -> Result<MatchObservation> {
    let resolved = resolve_match(spec, use_)?;
    let compiled = compile_matcher(&resolved, use_.label())?;
    let mut spans = Vec::new();

    match resolved.scope {
        MatchScope::Auto => unreachable!("auto scope must resolve before matching"),
        MatchScope::Line => {
            for (line_index, segment) in content.split_inclusive('\n').enumerate() {
                let line = segment.strip_suffix('\n').unwrap_or(segment);
                let line = line.strip_suffix('\r').unwrap_or(line);
                let (candidate, _) = candidate_view(line, resolved.trim);
                if compiled.is_match(candidate) {
                    spans.push(MatchSpan {
                        start_line: line_index,
                        end_line: line_index,
                    });
                }
            }
            if content.is_empty() {
                let (candidate, _) = candidate_view(content, resolved.trim);
                if compiled.is_match(candidate) {
                    spans.push(MatchSpan {
                        start_line: 0,
                        end_line: 0,
                    });
                }
            }
        }
        MatchScope::File => {
            let (candidate, base_offset) = candidate_view(content, resolved.trim);
            for (relative_start, relative_end) in compiled.find_spans(candidate) {
                let start = base_offset + relative_start;
                let end = base_offset + relative_end;
                if start == end {
                    return Err(AppError::fm_write(
                        8,
                        format!(
                            "{} matcher produced an empty span at byte {start}; \
                             zero-width structural matches are refused",
                            use_.label()
                        ),
                    ));
                }
                let end_probe = end.saturating_sub(1);
                spans.push(MatchSpan {
                    start_line: line_for_offset(content, start),
                    end_line: line_for_offset(content, end_probe),
                });
            }
        }
    }

    let selected = select_span(&spans, &resolved, use_)?;
    Ok(MatchObservation {
        matcher: resolved.matcher,
        scope: resolved.scope,
        occurrence: resolved.occurrence,
        count: spans.len(),
        selected,
    })
}

/// Validate matcher syntax/configuration without reading or changing the host
/// file. Called before shell hooks so invalid regex never receives actuation.
pub(crate) fn validate_match_specs(frontmatter: &Frontmatter) -> Result<()> {
    let placement_count = usize::from(frontmatter.before.is_some())
        + usize::from(frontmatter.after.is_some())
        + usize::from(frontmatter.at_line.is_some());
    if placement_count > 0 && !frontmatter.inject {
        return Err(AppError::fm_write(
            10,
            "`before`, `after`, and `at_line` require `inject: true`; placement              authority without injection is incoherent. Remediation: enable injection              or remove the placement fields.",
        ));
    }
    if placement_count > 1 {
        return Err(AppError::fm_write(
            10,
            "configure exactly one of `before`, `after`, or `at_line`; implicit              precedence between multiple placement authorities is refused.              Remediation: retain the single intended structural port.",
        ));
    }
    for (spec, use_) in [
        (frontmatter.before.as_ref(), MatchUse::Before),
        (frontmatter.after.as_ref(), MatchUse::After),
        (frontmatter.skip_if.as_ref(), MatchUse::SkipIf),
    ] {
        if let Some(spec) = spec {
            let resolved = resolve_match(spec, use_)?;
            let _ = compile_matcher(&resolved, use_.label())?;
        }
    }
    Ok(())
}

/// Observe structured selectors against the current host state before shell or
/// write actuation. Literal declarations retain their historical behavior and
/// decision text; structured declarations contribute evidence to the sync
/// decision/receipt.
pub(crate) fn preflight_structured_matchers(
    root: &Path, rel_to: &str, frontmatter: &Frontmatter,
) -> Result<Vec<String>> {
    validate_match_specs(frontmatter)?;
    let has_structured_selector = [
        frontmatter.before.as_ref(),
        frontmatter.after.as_ref(),
        frontmatter.skip_if.as_ref(),
    ]
    .into_iter()
    .flatten()
    .any(|spec| matches!(spec, MatchSpec::Structured(_)));
    if !has_structured_selector {
        return Ok(Vec::new());
    }

    let target = resolve_target(root, rel_to)?;
    let content = match std::fs::read_to_string(&target) {
        Ok(content) => content,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(error) => return Err(error.into()),
    };

    let mut evidence = Vec::new();
    for (spec, use_) in [
        (frontmatter.before.as_ref(), MatchUse::Before),
        (frontmatter.after.as_ref(), MatchUse::After),
        (frontmatter.skip_if.as_ref(), MatchUse::SkipIf),
    ] {
        if let Some(spec @ MatchSpec::Structured(_)) = spec {
            let observation = observe_match(&content, spec, use_)?;
            evidence.push(format!("{}: {}", use_.label(), observation.describe()));
        }
    }
    Ok(evidence)
}

/// Compute the injected file content: `before` selector / `after` selector /
/// `at_line` (1-based) / else append. Missing or ambiguous selector and
/// out-of-range line fail closed.
fn inject_into(existing: &str, body: &str, fm: &Frontmatter) -> Result<String> {
    let mut lines: Vec<&str> = existing.lines().collect();
    let body_lines: Vec<&str> = body.lines().collect();

    let insert_at: usize = if let Some(selector) = fm.before.as_ref() {
        observe_match(existing, selector, MatchUse::Before)?
            .selected
            .ok_or_else(|| {
                AppError::fm_write(
                    4,
                    "before selector reported no match despite requires_match(); \
                     observe_match invariant violated"
                        .to_string(),
                )
            })?
            .start_line
    } else if let Some(selector) = fm.after.as_ref() {
        observe_match(existing, selector, MatchUse::After)?
            .selected
            .ok_or_else(|| {
                AppError::fm_write(
                    4,
                    "after selector reported no match despite requires_match(); \
                     observe_match invariant violated"
                        .to_string(),
                )
            })?
            .end_line
            + 1
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

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
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

    /// The historical bug: `resolve_target` canonicalized only the nearest
    /// EXISTING ANCESTOR of the target, never the target leaf itself. A
    /// symlink placed exactly at the leaf pointing at an existing file
    /// outside the root sailed through the old check (the ancestor -- the
    /// project root itself here -- canonicalizes fine) and would have been
    /// followed for real by the later `fs::write`. This proves the fixed
    /// `resolve_target` resolves and checks the leaf itself, not just its
    /// parent chain.
    #[cfg(unix)]
    #[test]
    fn resolve_target_refuses_a_symlinked_leaf_pointing_outside_root() {
        use std::os::unix::fs::symlink;

        let root = TempDir::new().expect("root tempdir");
        let outside = TempDir::new().expect("outside tempdir");
        let secret = outside.path().join("secret.txt");
        std::fs::write(&secret, "outside content\n").expect("seed outside file");

        let link = root.path().join("evil.rs");
        symlink(&secret, &link).expect("create symlink");

        let err =
            resolve_target(root.path(), "evil.rs").expect_err("symlinked leaf must be refused");
        assert!(err.to_string().contains("FM-WRITE-002"), "{err}");
        assert!(err.to_string().contains("escapes"), "{err}");
    }

    /// Full write-path version of the same escape, using a DANGLING symlink
    /// (destination does not yet exist, but its parent directory does) --
    /// the shape that actually mattered under the old code: with the old
    /// ancestor-only check, `target.exists()` was `false` (the destination
    /// is missing), so `plan_write` took the `None => { ensure_parent +
    /// fs::write }` branch with no "differs from existing content" guard at
    /// all, and `fs::write` follows a symlink -- it would have silently
    /// CREATED `outside/escaped.rs` with attacker/template-controlled
    /// content. Proves the fix refuses before any byte is written, and that
    /// the file never appears outside the root.
    #[cfg(unix)]
    #[test]
    fn plan_write_refuses_write_through_dangling_symlinked_target() {
        use std::os::unix::fs::symlink;

        let root = TempDir::new().expect("root tempdir");
        let outside = TempDir::new().expect("outside tempdir");
        let escaped_path = outside.path().join("escaped.rs");
        assert!(!escaped_path.exists(), "precondition: destination absent");

        let link = root.path().join("evil.rs");
        symlink(&escaped_path, &link).expect("create dangling symlink");

        let err = plan_write(root.path(), "evil.rs", "malicious content", &fm("evil.rs"))
            .expect_err("write through a symlinked target must be refused");
        assert!(err.to_string().contains("FM-WRITE-002"), "{err}");
        assert!(
            !escaped_path.exists(),
            "the write must never land at the symlink's destination outside root"
        );
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

    /// Regression guard for the 2026-08-03 TECH-DEBT-003 fix: `force: true` must not
    /// defeat identical-content skip detection. Before the fix, this returned `Written`
    /// unconditionally whenever `force` was set, even though nothing on disk needed to
    /// change -- breaking idempotent second-sync semantics for every force-writing
    /// template (caught for real by
    /// crates/ggen-engine/tests/cross_pack_matrix.rs's `mega_project_all_packs_sync`
    /// once packs/chicago-tdd-tools-pack's templates gained `force: true`).
    #[test]
    fn force_skips_when_content_identical() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("x.rs"), "same\n").expect("seed");
        let mut f = fm("x.rs");
        f.force = true;
        let out = plan_write(dir.path(), "x.rs", "same\n", &f).expect("plan");
        assert!(
            matches!(out, WriteOutcome::Skipped(ref r) if r.contains("unchanged")),
            "force=true with identical content must still skip, got {out:?}"
        );
        assert_eq!(
            std::fs::read_to_string(dir.path().join("x.rs")).expect("read"),
            "same\n",
            "file must not be rewritten (mtime/inode churn) when content is unchanged"
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
        f.after = Some(MatchSpec::from("// modules"));
        f.skip_if = Some(MatchSpec::from("pub mod b;"));

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
    fn structured_defaults_are_contains_auto_first_case_sensitive_untrimmed() {
        let yaml = r#"
to: x.rs
before:
  pattern: "// SLOT"
"#;
        let frontmatter: Frontmatter = serde_yaml::from_str(yaml).expect("frontmatter");
        let Some(MatchSpec::Structured(rule)) = frontmatter.before else {
            panic!("structured match rule");
        };
        assert_eq!(rule.matcher, MatchKind::Contains);
        assert_eq!(rule.scope, MatchScope::Auto);
        assert_eq!(rule.occurrence, MatchOccurrence::First);
        assert_eq!(rule.index, 1);
        assert!(rule.case_sensitive);
        assert!(!rule.trim);
    }

    #[test]
    fn regex_unique_injects_at_the_only_matching_line() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(
            dir.path().join("mod.rs"),
            "header\n  // GGEN:SLOT:COMMANDS  \nfooter\n",
        )
        .expect("seed");
        let mut f = fm("mod.rs");
        f.inject = true;
        f.before = Some(MatchSpec::Structured(MatchRule {
            pattern: r"^\s*// GGEN:SLOT:COMMANDS\s*$".to_string(),
            matcher: MatchKind::Regex,
            scope: MatchScope::Line,
            occurrence: MatchOccurrence::Unique,
            index: 1,
            case_sensitive: true,
            trim: false,
        }));
        let out = plan_write(dir.path(), "mod.rs", "generated", &f).expect("inject");
        assert_eq!(out, WriteOutcome::Injected);
        assert_eq!(
            std::fs::read_to_string(dir.path().join("mod.rs")).expect("read"),
            "header\ngenerated\n  // GGEN:SLOT:COMMANDS  \nfooter\n"
        );
    }

    #[test]
    fn unique_refuses_duplicate_matches() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("mod.rs"), "// SLOT\nbody\n// SLOT\n").expect("seed");
        let mut f = fm("mod.rs");
        f.inject = true;
        f.before = Some(MatchSpec::Structured(MatchRule {
            pattern: "// SLOT".to_string(),
            matcher: MatchKind::Exact,
            scope: MatchScope::Line,
            occurrence: MatchOccurrence::Unique,
            index: 1,
            case_sensitive: true,
            trim: false,
        }));
        let error = plan_write(dir.path(), "mod.rs", "generated", &f)
            .expect_err("duplicate unique selector must refuse");
        assert!(error.to_string().contains("FM-WRITE-008"), "{error}");
        assert_eq!(
            std::fs::read_to_string(dir.path().join("mod.rs")).expect("read"),
            "// SLOT\nbody\n// SLOT\n"
        );
    }

    #[test]
    fn nth_uses_one_based_index() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("mod.rs"), "// SLOT\nbody\n// SLOT\n").expect("seed");
        let mut f = fm("mod.rs");
        f.inject = true;
        f.after = Some(MatchSpec::Structured(MatchRule {
            pattern: "// SLOT".to_string(),
            matcher: MatchKind::Exact,
            scope: MatchScope::Line,
            occurrence: MatchOccurrence::Nth,
            index: 2,
            case_sensitive: true,
            trim: false,
        }));
        plan_write(dir.path(), "mod.rs", "generated", &f).expect("inject");
        assert_eq!(
            std::fs::read_to_string(dir.path().join("mod.rs")).expect("read"),
            "// SLOT\nbody\n// SLOT\ngenerated\n"
        );
    }

    #[test]
    fn regex_skip_if_defaults_to_file_scope() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("mod.rs"), "alpha\npub mod beta;\nomega\n").expect("seed");
        let mut f = fm("mod.rs");
        f.skip_if = Some(MatchSpec::Structured(MatchRule {
            pattern: r"(?m)^pub mod beta;$".to_string(),
            matcher: MatchKind::Regex,
            scope: MatchScope::Auto,
            occurrence: MatchOccurrence::First,
            index: 1,
            case_sensitive: true,
            trim: false,
        }));
        let out = plan_write(dir.path(), "mod.rs", "replacement", &f).expect("plan");
        assert!(
            matches!(out, WriteOutcome::Skipped(ref reason) if reason.contains("matcher=regex"))
        );
    }

    #[test]
    fn invalid_regex_refuses_without_mutation() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("mod.rs"), "// SLOT\n").expect("seed");
        let mut f = fm("mod.rs");
        f.inject = true;
        f.before = Some(MatchSpec::Structured(MatchRule {
            pattern: "(".to_string(),
            matcher: MatchKind::Regex,
            scope: MatchScope::Line,
            occurrence: MatchOccurrence::First,
            index: 1,
            case_sensitive: true,
            trim: false,
        }));
        let error = plan_write(dir.path(), "mod.rs", "generated", &f)
            .expect_err("invalid regex must refuse");
        assert!(error.to_string().contains("FM-WRITE-008"), "{error}");
        assert_eq!(
            std::fs::read_to_string(dir.path().join("mod.rs")).expect("read"),
            "// SLOT\n"
        );
    }

    #[test]
    fn inject_before_marker() {
        let dir = TempDir::new().expect("tempdir");
        std::fs::write(dir.path().join("f.txt"), "one\ntwo\n").expect("seed");
        let mut f = fm("f.txt");
        f.inject = true;
        f.before = Some(MatchSpec::from("two"));
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
        f.after = Some(MatchSpec::from("// nowhere"));
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
    /// `freeze_policy=always` exists for, per CLAUDE.md's mode=Create
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
