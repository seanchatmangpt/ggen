//! Pack resolution and content hashing.
//!
//! A pack is a directory containing `pack.toml`, `ontology.ttl`, and a
//! `templates/` directory of `*.tmpl` files. Packs are declared in
//! `ggen.toml` under `[packs]` and resolved fail-closed: a missing pack
//! directory, missing manifest, missing ontology, unknown manifest keys,
//! or an empty template set all refuse by name with an `FM-PACK-*` code.
//!
//! [`content_hash`] computes a deterministic BLAKE3 over every regular file
//! under the pack root (ontology, templates, `pack.toml`, and, when present,
//! `gates/*.rq` and `hook.ttl` -- see that function's doc comment) as sorted
//! `(relative_path, bytes)` pairs.
//!
//! `PackRef::Git` packs are cloned with the system `git` binary (no git
//! library dependency) into `<root>/.ggen-v2/git-packs/<name>/`, pinned by a
//! `.ggen-git-pin` marker recording the exact `version` last checked out —
//! unchanged config reuses the clone with no network call; a changed
//! `version` (or a missing/corrupt cache) wipes and re-clones. Once cloned,
//! a git pack is just a local directory and goes through the exact same
//! validation (`pack.toml`, `ontology.ttl`, `templates/*.tmpl`) as a
//! `PackRef::Path` pack.
//!
//! [`resolve`] permits that clone/wipe/pin-write network I/O. [`resolve_read_only`]
//! does not: a git pack resolves only from an already-correctly-pinned cache,
//! and any cache miss/mismatch is refused (`[FM-PACK-012]`) instead of
//! triggering a clone. Use `resolve_read_only` from any caller that must not
//! perform undisclosed network/filesystem side effects (e.g. a read-only
//! query tool).

use std::path::{Path, PathBuf};
use std::process::Command;

use serde::Deserialize;

use crate::{
    config::{GgenConfig, PackRef},
    error::{AppError, Result},
};

/// A resolved local pack, ready for the sync pipeline.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pack {
    /// Pack name (the `[packs]` key in `ggen.toml`).
    pub name: String,
    /// Version string from `pack.toml`.
    pub version: String,
    /// Human description from `pack.toml`.
    pub description: String,
    /// Absolute (resolved) pack root directory.
    pub root: PathBuf,
    /// Path to the pack's `ontology.ttl`.
    pub ontology_path: PathBuf,
    /// Extra ontology files declared on this pack's `ggen.toml` entry
    /// (`extra_ontologies = [...]`), resolved against the manifest root and
    /// verified readable at resolve time. Unioned into the pack graph after
    /// `ontology.ttl`, in declaration order; each joins the pack content
    /// hash paired with its declared (manifest-relative) path string.
    pub extra_ontology_paths: Vec<(String, PathBuf)>,
    /// Sorted paths of the pack's `templates/*.tmpl` files.
    pub template_paths: Vec<PathBuf>,
    /// Whether this pack participates in `ggen.lock` content-hash pinning
    /// (`PackRef::Path`'s `lock` field; always `true` for `PackRef::Git`
    /// packs, which have no opt-out — a git pack is already pinned by
    /// `version`). `false` means [`lock_entries`] omits this pack entirely:
    /// it is never checked against `ggen.lock` and never written to it.
    pub lock: bool,
}

/// On-disk `pack.toml` schema (closed key set, fail closed).
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct PackToml {
    pack: PackMeta,
}

/// `[pack]` table of `pack.toml` (closed key set).
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct PackMeta {
    name: String,
    version: String,
    description: String,
}

/// Resolve every pack declared in `config.packs`, in name (`BTreeMap`) order.
///
/// `PackRef::Path` entries resolve relative to `config_root` (the directory
/// containing `ggen.toml`); `PackRef::Git` entries are cloned/cached under
/// `config_root` too (see the module docs). This entry point permits the
/// network I/O (`git clone`) and cache writes a git pack may require —
/// callers that must never perform such side effects (read-only
/// tools/queries) should use [`resolve_read_only`] instead.
///
/// # Errors
/// - `[FM-PACK-001]` pack directory missing
/// - `[FM-PACK-002]` `pack.toml` missing or unreadable
/// - `[FM-PACK-003]` `pack.toml` invalid TOML or unknown keys
/// - `[FM-PACK-004]` `ontology.ttl` missing
/// - `[FM-PACK-005]` zero templates under `templates/`
/// - `[FM-PACK-010]` `git` not on `$PATH`, or `git clone` failed
/// - `[FM-PACK-011]` `git checkout <version>` failed
pub fn resolve(config: &GgenConfig, config_root: &Path) -> Result<Vec<Pack>> {
    resolve_inner(config, config_root, true)
}

/// As [`resolve`], but never performs network I/O or writes a git pack's
/// clone cache. A `PackRef::Git` pack resolves only from an existing,
/// correctly-pinned `<config_root>/.ggen-v2/git-packs/<name>/` cache; a
/// cache miss (absent, corrupt, or pinned to a different `version`) is
/// refused with `[FM-PACK-012]` rather than triggering a clone.
///
/// Intended for callers that must uphold a read-only/`readOnlyHint`
/// contract — e.g. an ad-hoc SPARQL query preview — where a real `git
/// clone`, a cache wipe, or a pin-file write would be an undisclosed side
/// effect. `PackRef::Path` packs are unaffected (they are already pure
/// local filesystem reads).
///
/// # Errors
/// Same as [`resolve`], plus `[FM-PACK-012]` for an uncached/mismatched git
/// pack.
pub fn resolve_read_only(config: &GgenConfig, config_root: &Path) -> Result<Vec<Pack>> {
    resolve_inner(config, config_root, false)
}

fn resolve_inner(
    config: &GgenConfig, config_root: &Path, allow_network: bool,
) -> Result<Vec<Pack>> {
    let mut packs = Vec::with_capacity(config.packs.len());
    for (name, pack_ref) in &config.packs {
        match pack_ref {
            PackRef::Git {
                git,
                version,
                subdir,
            } => {
                let clone_root =
                    resolve_git_pack_dir(name, git, version, config_root, allow_network)?;
                let root = match subdir {
                    Some(sub) => {
                        let joined = clone_root.join(sub);
                        if !joined.is_dir() {
                            return Err(AppError::fm_pack(
                                13,
                                format!(
                                    "pack `{name}`: `subdir` `{}` does not exist inside the \
                                     cloned repository `{git}` (checked out at `{version}`). \
                                     Remediation: fix the [packs] subdir entry, or omit it if \
                                     the pack lives at the repository root.",
                                    sub.display()
                                ),
                            ));
                        }
                        joined
                    }
                    None => clone_root,
                };
                packs.push(resolve_pack_dir(name, &root)?);
            }
            PackRef::Path {
                path,
                extra_ontologies,
                lock,
            } => {
                let root = config_root.join(path);
                if !root.is_dir() {
                    return Err(AppError::fm_pack(
                        1,
                        format!(
                            "pack `{name}`: directory `{}` does not exist. \
                             Remediation: fix the [packs] path or vendor the pack.",
                            root.display()
                        ),
                    ));
                }
                let mut pack = resolve_pack_dir(name, &root)?;
                pack.lock = *lock;
                for extra in extra_ontologies {
                    let extra_path = config_root.join(extra);
                    if !extra_path.is_file() {
                        return Err(AppError::fm_pack(
                            4,
                            format!(
                                "pack `{name}`: extra ontology `{}` missing at `{}`. \
                                 Remediation: fix the [packs] extra_ontologies entry.",
                                extra.display(),
                                extra_path.display()
                            ),
                        ));
                    }
                    pack.extra_ontology_paths
                        .push((extra.to_string_lossy().into_owned(), extra_path));
                }
                packs.push(pack);
            }
        }
    }
    Ok(packs)
}

/// Directory name (under `<config_root>/.ggen-v2/git-packs/`) a git pack's
/// clone cache lives in — the marker file inside it, not this name, is the
/// authority on which `version` is actually checked out.
const GIT_PIN_FILE: &str = ".ggen-git-pin";

/// Clone (or reuse a pinned cache of) a `PackRef::Git` pack, returning its
/// local directory. Reuses the cache as-is when `<cache>/.ggen-git-pin`
/// already records the exact `version` requested (no network call, no
/// write, regardless of `allow_network`); otherwise, when `allow_network`
/// is `true`, wipes and re-clones + checks out fresh. When `allow_network`
/// is `false` and the cache does not already satisfy `version`, no clone,
/// wipe, or write is attempted at all — the call fails closed with
/// `[FM-PACK-012]` instead.
///
/// # Errors
/// - `[FM-PACK-010]` `git` not on `$PATH`, or `git clone` failed
/// - `[FM-PACK-011]` `git checkout <version>` failed
/// - `[FM-PACK-012]` cache miss/mismatch with `allow_network: false`
fn resolve_git_pack_dir(
    name: &str, git: &str, version: &str, config_root: &Path, allow_network: bool,
) -> Result<PathBuf> {
    let cache_dir = config_root.join(".ggen-v2/git-packs").join(name);
    let pin_path = cache_dir.join(GIT_PIN_FILE);
    if let Ok(pinned) = std::fs::read_to_string(&pin_path) {
        if pinned.trim() == version {
            return Ok(cache_dir);
        }
    }

    if !allow_network {
        return Err(AppError::fm_pack(
            12,
            format!(
                "pack `{name}`: git pack is not cached at the pinned `{version}` (cache \
                 missing, corrupt, or pinned to a different version), and this operation is \
                 read-only and does not perform network I/O. \
                 Remediation: run `ggen sync run` once so the pack is cloned and pinned, or \
                 vendor it locally with {{ path = \"…\" }}."
            ),
        ));
    }

    clone_and_pin_git_pack(name, git, version, &cache_dir, &pin_path)?;
    Ok(cache_dir)
}

/// Wipe any stale cache, `git clone` + `git checkout <version>`, and write
/// the `.ggen-git-pin` marker. Split out of [`resolve_git_pack_dir`] purely
/// to keep that function under the workspace's line-count lint; behavior is
/// unchanged from when this was inlined there.
///
/// # Errors
/// - `[FM-PACK-010]` `git` not on `$PATH`, or `git clone` failed
/// - `[FM-PACK-011]` `git checkout <version>` failed
fn clone_and_pin_git_pack(
    name: &str, git: &str, version: &str, cache_dir: &Path, pin_path: &Path,
) -> Result<()> {
    // Cache miss (absent, corrupt, or version changed): wipe and re-clone.
    if cache_dir.exists() {
        std::fs::remove_dir_all(cache_dir).map_err(|e| {
            AppError::fm_pack(
                10,
                format!(
                    "pack `{name}`: could not clear stale git cache `{}`: {e}",
                    cache_dir.display()
                ),
            )
        })?;
    }
    let cache_parent = cache_dir.parent().ok_or_else(|| {
        AppError::fm_pack(
            10,
            format!(
                "pack `{name}`: git cache directory `{}` has no parent directory",
                cache_dir.display()
            ),
        )
    })?;
    std::fs::create_dir_all(cache_parent).map_err(|e| {
        AppError::fm_pack(
            10,
            format!("pack `{name}`: could not create git cache directory: {e}"),
        )
    })?;

    let clone = Command::new("git")
        .args([
            "clone",
            "--quiet",
            git,
            cache_dir.to_str().unwrap_or_default(),
        ])
        .output()
        .map_err(|e| {
            AppError::fm_pack(
                10,
                format!(
                    "pack `{name}`: `git` not runnable (is it on $PATH?): {e}. \
                     Remediation: install git, or vendor the pack locally with {{ path = \"…\" }}."
                ),
            )
        })?;
    if !clone.status.success() {
        return Err(AppError::fm_pack(
            10,
            format!(
                "pack `{name}`: `git clone {git}` failed: {}\
                 Remediation: verify the git URL is reachable and correct.",
                String::from_utf8_lossy(&clone.stderr)
            ),
        ));
    }

    let checkout = Command::new("git")
        .args([
            "-C",
            cache_dir.to_str().unwrap_or_default(),
            "checkout",
            "--quiet",
            version,
        ])
        .output()
        .map_err(|e| {
            AppError::fm_pack(
                11,
                format!("pack `{name}`: `git checkout {version}` not runnable: {e}"),
            )
        })?;
    if !checkout.status.success() {
        return Err(AppError::fm_pack(
            11,
            format!(
                "pack `{name}`: `git checkout {version}` failed: {}\
                 Remediation: verify `version` names an existing tag, branch, or commit.",
                String::from_utf8_lossy(&checkout.stderr)
            ),
        ));
    }

    std::fs::write(pin_path, version).map_err(|e| {
        AppError::fm_pack(
            10,
            format!("pack `{name}`: could not write git pin marker: {e}"),
        )
    })?;
    Ok(())
}

/// Resolve one already-on-disk pack directory (a `PackRef::Path` target or a
/// resolved git-pack clone) into a validated [`Pack`].
fn resolve_pack_dir(name: &str, root: &Path) -> Result<Pack> {
    let root = root.to_path_buf();
    let manifest_path = root.join("pack.toml");
    let manifest_raw = std::fs::read_to_string(&manifest_path).map_err(|e| {
        AppError::fm_pack(
            2,
            format!(
                "pack `{name}`: pack.toml unreadable at `{}`: {e}. \
                 Remediation: every pack must ship a pack.toml.",
                manifest_path.display()
            ),
        )
    })?;
    let manifest: PackToml = star_toml::from_str(&manifest_raw).map_err(|e| {
        AppError::fm_pack(
            3,
            format!(
                "pack `{name}`: invalid pack.toml at `{}`: {e}. \
                 Remediation: fix the TOML syntax or remove unknown keys.",
                manifest_path.display()
            ),
        )
    })?;

    let ontology_path = root.join("ontology.ttl");
    if !ontology_path.is_file() {
        return Err(AppError::fm_pack(
            4,
            format!(
                "pack `{name}`: ontology.ttl missing at `{}`. \
                 Remediation: every pack must ship an ontology.ttl.",
                ontology_path.display()
            ),
        ));
    }

    let templates_dir = root.join("templates");
    let mut template_paths: Vec<PathBuf> = Vec::new();
    if templates_dir.is_dir() {
        collect_pack_tmpl_paths(name, &templates_dir, &mut template_paths)?;
    }
    template_paths.sort();
    if template_paths.is_empty() {
        return Err(AppError::fm_pack(
            5,
            format!(
                "pack `{name}`: zero templates under `{}`. \
                 Remediation: a pack must ship at least one templates/*.tmpl.",
                templates_dir.display()
            ),
        ));
    }

    // The [packs] key in ggen.toml is the authoritative resolution name;
    // the manifest's own `name` is informational.
    let _ = &manifest.pack.name;
    Ok(Pack {
        name: name.to_string(),
        version: manifest.pack.version,
        description: manifest.pack.description,
        root,
        ontology_path,
        extra_ontology_paths: Vec::new(),
        template_paths,
        // Default `true` (existing pin-and-check behavior). `resolve`'s
        // `PackRef::Path` arm overwrites this from the declared `lock`
        // field; `PackRef::Git` packs (no opt-out) keep this default.
        lock: true,
    })
}

/// Recursively collect every `*.tmpl` file under a pack's `templates/`
/// directory. Packs are free to organize templates into subdirectories
/// (e.g. `templates/generated/`, `templates/src/bin/`) exactly like a
/// consumer project's own `[templates].dir` does (see `sync::
/// collect_tmpl_paths`, which this mirrors) -- a flat, single-level
/// `read_dir` here would silently drop every nested template with no error,
/// which previously made a pack's own subdirectory-organized templates
/// invisible to `discover_templates` while its `gates/*.rq` still enforced
/// data those absent templates were meant to project.
///
/// # Errors
/// `[FM-PACK-006]`-shaped propagation via `?` when a directory cannot be
/// listed (fails closed, consistent with the rest of pack resolution).
fn collect_pack_tmpl_paths(name: &str, dir: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    for entry in std::fs::read_dir(dir).map_err(|e| {
        AppError::fm_pack(
            5,
            format!(
                "pack `{name}`: could not list `{}` while discovering templates: {e}.",
                dir.display()
            ),
        )
    })? {
        let entry = entry.map_err(|e| {
            AppError::fm_pack(
                5,
                format!(
                    "pack `{name}`: could not read a directory entry under `{}` while \
                     discovering templates: {e}",
                    dir.display()
                ),
            )
        })?;
        let path = entry.path();
        if path.is_dir() {
            collect_pack_tmpl_paths(name, &path, out)?;
        } else if path.is_file() && path.extension().is_some_and(|e| e == "tmpl") {
            out.push(path);
        }
    }
    Ok(())
}

/// Deterministic BLAKE3 content hash of a pack: sorted `(relative_path,
/// bytes)` pairs over EVERY regular file under `pack.root` (not just
/// `ontology.ttl` plus templates -- see [`collect_pack_files_sorted`]) plus
/// any declared extra ontologies. For each pair the path string bytes are
/// hashed, then the file bytes, in sorted relative-path order.
///
/// Before this covered the full pack root, a pack's `gates/*.rq` SPARQL
/// gate files and `hook.ttl` Knowledge Hook document -- both real,
/// sync-time-enforced governing inputs (see `crate::sync`'s pack-gate and
/// pack-hook loading) -- were silently excluded: editing either after
/// `ggen.lock` was written did not change `content_hash`, so `check_lock`
/// could not detect the tamper. Confirmed non-hypothetical: dozens of packs
/// under `packs/` ship a `gates/` directory and at least one ships
/// `hook.ttl`. Full-tree hashing closes that gap the same way
/// `ggen-marketplace`'s `compute_pack_digest`/`hash_installed_content`
/// closes the analogous one for marketplace-installed packs.
///
/// # Errors
/// `[FM-PACK-006]` when a pack file (or directory) becomes unreadable
/// between resolution and hashing.
pub fn content_hash(pack: &Pack) -> Result<[u8; 32]> {
    let mut entries: Vec<(String, PathBuf)> = Vec::new();
    collect_pack_files_sorted(&pack.name, &pack.root, &pack.root, &mut entries)?;
    // Extra ontologies live outside the pack root; their declared
    // manifest-relative path string is the hash key, so an edit to a source
    // like crates/cng/ontologies/pddl-strips.ttl invalidates the lock the
    // same way an in-pack edit would (the drift the old make-ontology.sh
    // committed-union convention could not detect).
    for (declared, path) in &pack.extra_ontology_paths {
        entries.push((declared.clone(), path.clone()));
    }
    entries.sort_by(|a, b| a.0.cmp(&b.0));

    let mut hasher = blake3::Hasher::new();
    for (rel, path) in &entries {
        let bytes = std::fs::read(path).map_err(|e| {
            AppError::fm_pack(
                6,
                format!(
                    "pack `{}`: file `{}` unreadable while hashing: {e}. \
                     Remediation: do not mutate a pack during sync.",
                    pack.name,
                    path.display()
                ),
            )
        })?;
        hasher.update(rel.as_bytes());
        hasher.update(&bytes);
    }
    Ok(*hasher.finalize().as_bytes())
}

/// Recursively collect every regular file under `dir` as `(root-relative
/// path string, absolute path)` pairs into `out`, sorted at each directory
/// level so the walk order never depends on the filesystem's own
/// directory-entry order (matches the sort discipline `content_hash` already
/// applies to its final combined list).
///
/// Skips any directory literally named `.git`. This is deliberate, not an
/// oversight: `resolve_git_pack_dir` populates a `PackRef::Git` pack's root
/// via a real `git clone`, whose `.git/logs/*` reflogs embed the wall-clock
/// time of that clone. Two developers (or two syncs after a cache wipe)
/// resolving the identical pinned `version` would then hash to two different
/// values despite every tracked file being byte-identical -- breaking the
/// "same version -> same hash" invariant [`check_lock`] depends on. No other
/// exclusion exists: `pack.toml`, `gates/*.rq`, `hook.ttl`, and anything else
/// under the pack root all participate.
///
/// # Errors
/// `[FM-PACK-006]` when a directory cannot be listed.
fn collect_pack_files_sorted(
    pack_name: &str, root: &Path, dir: &Path, out: &mut Vec<(String, PathBuf)>,
) -> Result<()> {
    let read_dir = std::fs::read_dir(dir).map_err(|e| {
        AppError::fm_pack(
            6,
            format!(
                "pack `{pack_name}`: could not list `{}` while hashing: {e}. \
                 Remediation: do not mutate a pack during sync.",
                dir.display()
            ),
        )
    })?;
    let mut entries: Vec<std::fs::DirEntry> = Vec::new();
    for entry in read_dir {
        let entry = entry.map_err(|e| {
            AppError::fm_pack(
                6,
                format!(
                    "pack `{pack_name}`: could not read a directory entry under `{}` while \
                     hashing: {e}",
                    dir.display()
                ),
            )
        })?;
        entries.push(entry);
    }
    entries.sort_by_key(std::fs::DirEntry::file_name);

    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            if path.file_name().and_then(|n| n.to_str()) == Some(".git") {
                continue;
            }
            collect_pack_files_sorted(pack_name, root, &path, out)?;
        } else {
            out.push((rel_string(&path, root), path));
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// ggen.lock — deterministic pack lockfile
// ---------------------------------------------------------------------------

/// Lockfile name, at the project root next to `ggen.toml`.
pub const LOCK_FILE_NAME: &str = "ggen.lock";

/// One resolved lock entry: name, `source` string, and BLAKE3 hex.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LockEntry {
    /// Pack name (the `[packs]` key).
    pub name: String,
    /// Source as written in `ggen.toml`, prefixed (`path:…`).
    pub source: String,
    /// `blake3:<hex>` content hash.
    pub content_hash: String,
}

/// On-disk `ggen.lock` shape (closed key set, fail closed).
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct LockDoc {
    #[serde(default)]
    packs: std::collections::BTreeMap<String, LockDocEntry>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct LockDocEntry {
    source: String,
    content_hash: String,
}

/// The `source = "…"` string for a pack ref, as written in `ggen.toml`.
#[must_use]
pub fn source_string(pack_ref: &PackRef) -> String {
    match pack_ref {
        PackRef::Path { path, .. } => format!("path:{}", path.display()),
        PackRef::Git {
            git,
            version,
            subdir: Some(sub),
        } => format!("git:{git}@{version}#{}", sub.display()),
        PackRef::Git {
            git,
            version,
            subdir: None,
        } => format!("git:{git}@{version}"),
    }
}

/// Build lock entries (name → source + `blake3:<hex>`) for resolved packs.
///
/// Packs with `lock == false` (`PackRef::Path`'s opt-out, see
/// [`crate::config::PackRef`]) are skipped entirely — they never appear in
/// the returned entries, so [`check_lock`] never checks them against
/// `ggen.lock` and [`write_lock`]'s full-rewrite-from-current-entries never
/// writes (or preserves a stale prior write of) them.
///
/// # Errors
/// Propagates [`content_hash`] failures.
pub fn lock_entries(config: &GgenConfig, packs: &[Pack]) -> Result<Vec<LockEntry>> {
    let mut entries = Vec::with_capacity(packs.len());
    for pack in packs {
        if !pack.lock {
            continue;
        }
        let source = config
            .packs
            .get(&pack.name)
            .map(source_string)
            .unwrap_or_default();
        let hash = content_hash(pack)?;
        entries.push(LockEntry {
            name: pack.name.clone(),
            source,
            content_hash: format!("blake3:{}", crate::sync::hex32(&hash)),
        });
    }
    Ok(entries)
}

/// Fail closed if `ggen.lock` exists and any pack's content hash differs.
///
/// A missing lockfile is fine (first sync). Packs absent from the lock are
/// fine (they get locked on the next successful sync).
///
/// # Errors
/// - `[FM-PACK-009]` `ggen.lock` unreadable or malformed
/// - `[FM-PACK-008]` pack content hash differs from the locked hash
pub fn check_lock(root: &Path, entries: &[LockEntry]) -> Result<()> {
    let lock_path = root.join(LOCK_FILE_NAME);
    if !lock_path.is_file() {
        return Ok(());
    }
    let raw = std::fs::read_to_string(&lock_path).map_err(|e| {
        AppError::fm_pack(
            9,
            format!("ggen.lock unreadable at `{}`: {e}", lock_path.display()),
        )
    })?;
    let doc: LockDoc = star_toml::from_str(&raw).map_err(|e| {
        AppError::fm_pack(
            9,
            format!(
                "ggen.lock malformed at `{}`: {e}. \
                 Remediation: fix or delete the lockfile.",
                lock_path.display()
            ),
        )
    })?;
    for entry in entries {
        if let Some(locked) = doc.packs.get(&entry.name) {
            if locked.content_hash != entry.content_hash {
                return Err(AppError::fm_pack(
                    8,
                    format!(
                        "pack `{}` (source `{}`) content hash mismatch: ggen.lock \
                         has `{}` but the pack on disk hashes to `{}`. \
                         Remediation: restore the pack, or delete ggen.lock \
                         to intentionally re-lock.",
                        entry.name, locked.source, locked.content_hash, entry.content_hash
                    ),
                ));
            }
        }
    }
    Ok(())
}

/// Write `ggen.lock` deterministically (sorted by pack name, no timestamps).
///
/// Idempotent: if the file already exists with identical content, no write
/// occurs (the lockfile's mtime is left untouched). This matters for
/// [`crate::watch`] mode, which re-runs the pipeline on every debounced
/// filesystem change under the project root — `ggen.lock` lives at the
/// root (not under an ignored directory like `.ggen-v2`), so an
/// unconditional rewrite on every sync would retrigger the watcher on its
/// own output forever.
///
/// # Errors
/// I/O failure reading the existing lockfile (other than "not found") or
/// writing the new one.
pub fn write_lock(root: &Path, entries: &[LockEntry]) -> Result<()> {
    use std::fmt::Write as _;
    let mut sorted: Vec<&LockEntry> = entries.iter().collect();
    sorted.sort_by(|a, b| a.name.cmp(&b.name));
    let mut out = String::from("# ggen.lock — generated by `ggen sync`. Do not edit.\n");
    for entry in sorted {
        let _ = write!(
            out,
            "\n[packs.{}]\nsource = \"{}\"\ncontent_hash = \"{}\"\n",
            entry.name, entry.source, entry.content_hash
        );
    }
    let lock_path = root.join(LOCK_FILE_NAME);
    match std::fs::read_to_string(&lock_path) {
        Ok(existing) if existing == out => return Ok(()),
        Ok(_) | Err(_) => {}
    }
    std::fs::write(lock_path, out)?;
    Ok(())
}

/// Relative path of `path` under `root`, with `/` separators.
fn rel_string(path: &Path, root: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .components()
        .map(|c| c.as_os_str().to_string_lossy().into_owned())
        .collect::<Vec<_>>()
        .join("/")
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use tempfile::TempDir;

    use super::*;

    fn entry(name: &str) -> LockEntry {
        LockEntry {
            name: name.to_string(),
            source: "path:../pack".to_string(),
            content_hash: "blake3:deadbeef".to_string(),
        }
    }

    /// Regression test for the watch-mode infinite-loop bug: writing an
    /// identical lockfile must not touch the file on disk (no new mtime),
    /// or `--watch` would retrigger on its own output forever (`ggen.lock`
    /// lives at the project root, not under an ignored directory).
    #[test]
    fn write_lock_is_idempotent_when_content_is_unchanged() {
        let dir = TempDir::new().expect("tempdir");
        let entries = vec![entry("widget")];

        write_lock(dir.path(), &entries).expect("first write");
        let mtime_1 = std::fs::metadata(dir.path().join(LOCK_FILE_NAME))
            .expect("metadata")
            .modified()
            .expect("mtime");

        std::thread::sleep(std::time::Duration::from_millis(20));
        write_lock(dir.path(), &entries).expect("second write");
        let mtime_2 = std::fs::metadata(dir.path().join(LOCK_FILE_NAME))
            .expect("metadata")
            .modified()
            .expect("mtime");

        assert_eq!(
            mtime_1, mtime_2,
            "unchanged lockfile content must not be rewritten"
        );
    }

    #[test]
    fn write_lock_rewrites_when_content_changes() {
        let dir = TempDir::new().expect("tempdir");
        write_lock(dir.path(), &[entry("widget")]).expect("first write");
        write_lock(dir.path(), &[entry("widget"), entry("gadget")]).expect("second write");

        let contents =
            std::fs::read_to_string(dir.path().join(LOCK_FILE_NAME)).expect("read lockfile");
        assert!(contents.contains("gadget"));
    }

    /// A local scratch git repo with one file committed and tagged `v1`,
    /// used as the clone source — no network needed.
    fn git(args: &[&str], cwd: &Path) {
        let out = Command::new("git")
            .args(args)
            .current_dir(cwd)
            .output()
            .expect("run git");
        assert!(
            out.status.success(),
            "git {args:?} failed: {}",
            String::from_utf8_lossy(&out.stderr)
        );
    }

    fn scratch_git_source(dir: &Path) {
        std::fs::create_dir_all(dir).expect("mkdir source");
        git(&["init", "--quiet"], dir);
        git(&["config", "user.email", "test@example.com"], dir);
        git(&["config", "user.name", "Test"], dir);
        std::fs::write(dir.join("marker.txt"), "v1\n").expect("write marker");
        git(&["add", "."], dir);
        git(&["commit", "--quiet", "-m", "v1"], dir);
        git(&["tag", "v1"], dir);
        std::fs::write(dir.join("marker.txt"), "v2\n").expect("write marker");
        git(&["add", "."], dir);
        git(&["commit", "--quiet", "-m", "v2"], dir);
        git(&["tag", "v2"], dir);
    }

    #[test]
    fn resolve_git_pack_dir_clones_and_pins_the_requested_version() {
        let scratch = TempDir::new().expect("tempdir");
        let source = scratch.path().join("source");
        scratch_git_source(&source);
        let config_root = TempDir::new().expect("tempdir");

        let cache = resolve_git_pack_dir(
            "widget",
            source.to_str().expect("utf8 path"),
            "v1",
            config_root.path(),
            true,
        )
        .expect("clone succeeds");

        assert_eq!(
            std::fs::read_to_string(cache.join("marker.txt")).expect("marker"),
            "v1\n"
        );
        assert_eq!(
            std::fs::read_to_string(cache.join(GIT_PIN_FILE)).expect("pin"),
            "v1"
        );
    }

    #[test]
    fn resolve_git_pack_dir_reuses_the_cache_when_version_is_unchanged() {
        let scratch = TempDir::new().expect("tempdir");
        let source = scratch.path().join("source");
        scratch_git_source(&source);
        let config_root = TempDir::new().expect("tempdir");
        let url = source.to_str().expect("utf8 path");

        let cache = resolve_git_pack_dir("widget", url, "v1", config_root.path(), true)
            .expect("first clone");
        // Plant a sentinel: if the second call re-clones (wiping the cache
        // dir), this file disappears. If it reuses the cache, it survives.
        std::fs::write(cache.join("sentinel.txt"), "still here").expect("write sentinel");

        let cache2 = resolve_git_pack_dir("widget", url, "v1", config_root.path(), true)
            .expect("second call");
        assert!(
            cache2.join("sentinel.txt").is_file(),
            "unchanged version must reuse the cache, not re-clone"
        );
    }

    #[test]
    fn resolve_git_pack_dir_reclones_when_version_changes() {
        let scratch = TempDir::new().expect("tempdir");
        let source = scratch.path().join("source");
        scratch_git_source(&source);
        let config_root = TempDir::new().expect("tempdir");
        let url = source.to_str().expect("utf8 path");

        let cache = resolve_git_pack_dir("widget", url, "v1", config_root.path(), true)
            .expect("first clone");
        std::fs::write(cache.join("sentinel.txt"), "will be wiped").expect("write sentinel");

        let cache2 =
            resolve_git_pack_dir("widget", url, "v2", config_root.path(), true).expect("re-clone");
        assert!(
            !cache2.join("sentinel.txt").exists(),
            "a changed version must wipe and re-clone the cache"
        );
        assert_eq!(
            std::fs::read_to_string(cache2.join("marker.txt")).expect("marker"),
            "v2\n"
        );
        assert_eq!(
            std::fs::read_to_string(cache2.join(GIT_PIN_FILE)).expect("pin"),
            "v2"
        );
    }

    #[test]
    fn resolve_git_pack_dir_refuses_a_bad_version_with_a_typed_error() {
        let scratch = TempDir::new().expect("tempdir");
        let source = scratch.path().join("source");
        scratch_git_source(&source);
        let config_root = TempDir::new().expect("tempdir");

        let err = resolve_git_pack_dir(
            "widget",
            source.to_str().expect("utf8 path"),
            "does-not-exist",
            config_root.path(),
            true,
        )
        .expect_err("bad version must refuse");
        let msg = err.to_string();
        assert!(msg.contains("FM-PACK-011"), "{msg}");
        assert!(msg.contains("checkout"), "{msg}");
    }

    /// Regression test for the red-team finding (F1, contract-drift,
    /// `ggen-mcp`'s `sync_dry_run.rs`): a caller that must not mutate a
    /// project (`allow_network: false`, the mode [`resolve_read_only`] and
    /// dry-run sync now use) must never clone, wipe a stale cache, or write
    /// a pin marker for a `PackRef::Git` pack whose cache is not already
    /// pinned at the requested version -- it must fail closed instead, with
    /// the filesystem left exactly as it started (no cache directory
    /// created at all).
    #[test]
    fn resolve_git_pack_dir_refuses_to_mutate_when_allow_network_is_false_and_cache_is_missing() {
        let scratch = TempDir::new().expect("tempdir");
        let source = scratch.path().join("source");
        scratch_git_source(&source);
        let config_root = TempDir::new().expect("tempdir");
        let cache_dir = config_root.path().join(".ggen-v2/git-packs/widget");

        let err = resolve_git_pack_dir(
            "widget",
            source.to_str().expect("utf8 path"),
            "v2",
            config_root.path(),
            false,
        )
        .expect_err("a dry-run/read-only resolve of an uncached git pack must refuse");

        let msg = err.to_string();
        assert!(msg.contains("FM-PACK-012"), "{msg}");
        assert!(
            !cache_dir.exists(),
            "refusing must not create the cache directory -- no clone may have been attempted"
        );
    }

    /// Companion to the refusal test above: once a real (network-permitted)
    /// sync has cloned and pinned the exact requested version, a later
    /// `allow_network: false` call for that same version must succeed by
    /// reusing the cache -- read-only mode only refuses a *mutation*, not
    /// every git pack.
    #[test]
    fn resolve_git_pack_dir_reuses_an_already_pinned_cache_when_allow_network_is_false() {
        let scratch = TempDir::new().expect("tempdir");
        let source = scratch.path().join("source");
        scratch_git_source(&source);
        let config_root = TempDir::new().expect("tempdir");
        let url = source.to_str().expect("utf8 path");

        resolve_git_pack_dir("widget", url, "v1", config_root.path(), true)
            .expect("real sync clones and pins v1");

        let cache = resolve_git_pack_dir("widget", url, "v1", config_root.path(), false)
            .expect("read-only resolve of an already-pinned version must succeed");
        assert_eq!(
            std::fs::read_to_string(cache.join("marker.txt")).expect("marker"),
            "v1\n"
        );
    }

    /// End-to-end regression for F1: `sync(&root, SyncOptions { dry_run:
    /// true, .. })` against a project whose `ggen.toml` declares a
    /// `PackRef::Git` pack not yet cached must fail closed (typed
    /// `FM-PACK-012` error) rather than performing a real `git clone` --
    /// and, crucially, must leave `.ggen-v2/git-packs/` untouched. Uses a
    /// real local scratch git repo as the clone source (no network, no
    /// mocks) so the only thing standing between this test and a real
    /// clone is the `dry_run` gate itself.
    #[test]
    fn sync_dry_run_does_not_clone_an_uncached_git_pack() {
        use crate::sync::{sync, SyncOptions};

        let scratch = TempDir::new().expect("tempdir");
        let source = scratch.path().join("source");
        scratch_git_source(&source);
        let url = source.to_str().expect("utf8 path");

        let project = TempDir::new().expect("tempdir");
        let root = project.path();
        std::fs::write(
            root.join("ggen.toml"),
            format!(
                r#"[project]
name = "dry-run-git-pack-fixture"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"

[packs.widget]
git = "{url}"
version = "v2"
"#
            ),
        )
        .expect("write ggen.toml");
        std::fs::write(root.join("ontology.ttl"), "").expect("write ontology");
        std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");

        let git_pack_cache = root.join(".ggen-v2/git-packs/widget");
        assert!(
            !git_pack_cache.exists(),
            "fixture must start with no git-pack cache"
        );

        let err = sync(
            root,
            SyncOptions {
                dry_run: true,
                ..Default::default()
            },
        )
        .expect_err("dry-run sync must refuse to clone an uncached git pack");

        assert!(err.to_string().contains("FM-PACK-012"), "{err}");
        assert!(
            !git_pack_cache.exists(),
            "dry-run sync must not have created the git-pack cache directory \
             (no clone may have been attempted): {}",
            git_pack_cache.display()
        );
        assert!(
            !root.join(".ggen-v2/receipt.json").exists(),
            "dry-run sync must not write a receipt either"
        );
    }

    /// A scratch git repo shaped like `ggen-marketplace` itself: a monorepo
    /// with an unrelated top-level file (no `pack.toml` at the clone root)
    /// and one real, valid pack under `packs/<name>/`.
    fn scratch_monorepo_git_source(dir: &Path) {
        std::fs::create_dir_all(dir).expect("mkdir source");
        git(&["init", "--quiet"], dir);
        git(&["config", "user.email", "test@example.com"], dir);
        git(&["config", "user.name", "Test"], dir);
        std::fs::write(dir.join("README.md"), "monorepo root, not a pack\n")
            .expect("write README");

        let pack_dir = dir.join("packs/widget-pack");
        std::fs::create_dir_all(pack_dir.join("templates")).expect("mkdir pack templates");
        std::fs::write(
            pack_dir.join("pack.toml"),
            "[pack]\nname = \"widget-pack\"\nversion = \"1.0.0\"\ndescription = \"test\"\n",
        )
        .expect("write pack.toml");
        std::fs::write(
            pack_dir.join("ontology.ttl"),
            "@prefix dom: <http://example.com/ontology#> .\ndom:Widget a dom:DomainClass .\n",
        )
        .expect("write ontology.ttl");
        std::fs::write(
            pack_dir.join("templates/widget.rs.tmpl"),
            "---\nto: src/widget.rs\n---\n// generated\n",
        )
        .expect("write template");

        git(&["add", "."], dir);
        git(&["commit", "--quiet", "-m", "v1"], dir);
        git(&["tag", "v1"], dir);
    }

    /// A minimal, fully-populated `GgenConfig` with a single `[packs.widget-pack]`
    /// `PackRef::Git` entry (`GgenConfig` has no `Default` impl, and several
    /// of its fields are meaningfully required, so this is the real
    /// construction every test below needs, not a shortcut around it).
    fn git_pack_config(url: &str, version: &str, subdir: Option<&str>) -> crate::config::GgenConfig {
        crate::config::GgenConfig {
            project: crate::config::Project {
                name: "fixture".to_string(),
            },
            ontology: crate::config::Ontology {
                source: PathBuf::from("ontology.ttl"),
                prefixes: std::collections::BTreeMap::new(),
            },
            packs: std::collections::BTreeMap::from([(
                "widget-pack".to_string(),
                PackRef::Git {
                    git: url.to_string(),
                    version: version.to_string(),
                    subdir: subdir.map(PathBuf::from),
                },
            )]),
            templates: crate::config::Templates {
                dir: PathBuf::from("templates"),
                aggregate_modules: false,
            },
            law: crate::config::Law::default(),
        }
    }

    /// `PackRef::Git`'s real `subdir` field must let `resolve()` pull one
    /// pack out of a monorepo clone whose root is not itself a pack --
    /// confirming both the negative (no `subdir`, clone root has no
    /// `pack.toml`, refuses) and positive (`subdir` set to the real pack's
    /// subdirectory, resolves) cases against a real git clone, no mocks.
    #[test]
    fn resolve_git_pack_with_subdir_pulls_one_pack_out_of_a_monorepo_clone() {
        let scratch = TempDir::new().expect("tempdir");
        let source = scratch.path().join("source");
        scratch_monorepo_git_source(&source);
        let url = source.to_str().expect("utf8 path");

        // Negative: no `subdir` -- the clone root has no `pack.toml`, must
        // refuse with a clear, typed error, not panic or silently succeed.
        let config_root_a = TempDir::new().expect("tempdir");
        let config_a = git_pack_config(url, "v1", None);
        let err = resolve(&config_a, config_root_a.path())
            .expect_err("no subdir + no pack.toml at clone root must refuse");
        assert!(
            err.to_string().contains("pack.toml"),
            "must cite the missing pack.toml: {err}"
        );

        // Positive: `subdir = "packs/widget-pack"` resolves the real pack.
        let config_root_b = TempDir::new().expect("tempdir");
        let config_b = git_pack_config(url, "v1", Some("packs/widget-pack"));
        let packs = resolve(&config_b, config_root_b.path())
            .expect("subdir must resolve the real pack inside the monorepo clone");
        assert_eq!(packs.len(), 1);
        assert_eq!(packs[0].name, "widget-pack");
        assert_eq!(packs[0].version, "1.0.0");
        assert_eq!(packs[0].template_paths.len(), 1);
    }

    /// A `subdir` naming a path that does not exist inside the clone must
    /// refuse with a clear, typed `[FM-PACK-013]` error citing the bad
    /// subdir, not a generic `pack.toml unreadable` message that leaves the
    /// author guessing whether the problem is the subdir or the pack itself.
    #[test]
    fn resolve_git_pack_with_nonexistent_subdir_refuses_with_a_typed_error() {
        let scratch = TempDir::new().expect("tempdir");
        let source = scratch.path().join("source");
        scratch_monorepo_git_source(&source);
        let url = source.to_str().expect("utf8 path");

        let config_root = TempDir::new().expect("tempdir");
        let config = git_pack_config(url, "v1", Some("packs/does-not-exist"));
        let err = resolve(&config, config_root.path())
            .expect_err("a nonexistent subdir must refuse, not silently fail some other way");
        let msg = err.to_string();
        assert!(msg.contains("FM-PACK-013"), "{msg}");
        assert!(msg.contains("does-not-exist"), "{msg}");
    }

    /// End-to-end: a real `ggen.toml` declaring `subdir` under `[packs.*]`
    /// syncs successfully against a real monorepo clone via the actual
    /// `sync()` entry point, not just the lower-level `resolve()` — proving
    /// the TOML surface, not only the internal `PackRef` construction.
    #[test]
    fn sync_resolves_a_git_pack_via_subdir_end_to_end() {
        use crate::sync::{sync, SyncOptions};

        let scratch = TempDir::new().expect("tempdir");
        let source = scratch.path().join("source");
        scratch_monorepo_git_source(&source);
        let url = source.to_str().expect("utf8 path");

        let project = TempDir::new().expect("tempdir");
        let root = project.path();
        std::fs::write(
            root.join("ggen.toml"),
            format!(
                r#"[project]
name = "subdir-git-pack-fixture"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"

[packs.widget-pack]
git = "{url}"
version = "v1"
subdir = "packs/widget-pack"
"#
            ),
        )
        .expect("write ggen.toml");
        std::fs::write(root.join("ontology.ttl"), "").expect("write ontology");
        std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");

        let report = sync(
            root,
            SyncOptions {
                dry_run: false,
                ..Default::default()
            },
        )
        .expect("sync must resolve the git pack via its subdir and generate its output");
        assert!(
            report.written.iter().any(|p| p == "src/widget.rs"),
            "the pack's own template must have rendered: {:?}",
            report.written
        );
        assert!(
            root.join("src/widget.rs").is_file(),
            "generated file must actually exist on disk"
        );
    }
}
