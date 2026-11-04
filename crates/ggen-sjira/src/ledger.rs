//! Append-only, file-backed ledger of standing transition events.
//!
//! Port of `GgenIgniter.SemanticJira.TransitionLog`
//! (`ggen_igniter/lib/ggen_igniter/semantic_jira/transition_log.ex`) — the
//! log half of the SemanticJira kernel. The ledger path is either a DIRECTORY
//! or a FILE, resolved by [`kind`]:
//!
//! - an existing directory is a directory ledger; an existing regular file is
//!   a file ledger;
//! - a path that does not exist yet is a file ledger when its extension is
//!   `.ndjson` or `.jsonl`, otherwise a directory ledger.
//!
//! Directory ledger: one immutable JSON file per event, named
//! `<seq>-<digest-fragment>.json`. [`append`] is safe under concurrent
//! writers via per-digest and per-seq exclusive-create claims; the event file
//! appears atomically (temp file + rename), so a crashed writer leaves a gap,
//! never a duplicate or a partial file.
//!
//! File ledger: one JSON event per line (ndjson). [`append`] holds an
//! exclusive-create `<path>.lock` for the read-dedupe-append critical
//! section; the file is only ever appended to, never rewritten.
//!
//! In both forms an event whose `event_digest` is already present is returned
//! unchanged ([`AppendStatus::AlreadyRecorded`], idempotent), [`fetch`]
//! returns events ordered by `seq`, and every read path is behind a typed
//! refusal ([`LedgerError`]): an undecodable line/file, valid JSON that is
//! not an object, or an event whose `event_digest` no longer recomputes (a
//! tampered ledger) is a refusal, never a silent projection and never a
//! panic. [`append`] refuses the same way before writing.
//!
//! Digest law (port of `event_digest/1` over `SemanticJira.digest_exact/1`):
//! canonical sorted-key sha256 over the event minus `seq` and `event_digest`
//! — nothing else is elided, so the event commits to the receipt it records
//! (`receipt_digest` included), independent of key insertion order. The
//! igniter's deprecated `legacy_event_digest/1` compatibility window is NOT
//! ported (that window closed at the igniter's v26.11.1 milestone).
//!
//! Epoch + vector clocks (loops-of-loops spec §1 Loop 2): every event this
//! module writes carries `"epoch"` — the LEDGER-GENERATION counter — and,
//! when the caller passes a `vc`, a `"vc"` map `%{"<replica>" => counter}`.
//! Both are ordinary event fields, so the digest commits to them
//! automatically. Bump law: `epoch_bump` advances (stamped = last + bump),
//! explicit `epoch` must not regress (else [`LedgerError::EpochRegression`],
//! refused before any byte is written), otherwise the epoch stays. A ledger's
//! last epoch is the max over its events' epochs and the `<dir>/epoch` /
//! `<path>.epoch` marker file (default 0). `seq` orders events WITHIN a
//! generation; the epoch orders generations.
//!
//! Disclosure (lane contract): the igniter's `TransitionLog` stores bare JSON
//! objects (maps), not a typed transition struct, so this port stores
//! [`serde_json::Value`] events too — no local `LedgerEvent` struct was
//! needed, hence no duplicated transition type to unify with L7/L8.
//!
//! Modeled on `ggen_igniter/lib/ggen_igniter/semantic_jira/transition_log.ex`
//! (the port spec); digests via [`crate::work_order::digest_of`].

use std::collections::BTreeMap;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

use serde_json::{Map, Value};

use crate::work_order::digest_of;

/// File-ledger extensions for not-yet-existing paths.
const FILE_EXTENSIONS: [&str; 2] = ["ndjson", "jsonl"];

/// Bounded attempts for exclusive-create lock acquisition before refusing.
const LOCK_ATTEMPTS: u32 = 500;

/// Typed refusal for every read/append path (port of
/// `{:error, {:ledger_refused, reason}}`).
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum LedgerError {
    /// Undecodable JSON in a line (file ledger) or file (directory ledger).
    #[error("unreadable: {0}")]
    Unreadable(String),
    /// Valid JSON that decodes to a non-object. `where` names the file
    /// (directory ledger) or the 1-based physical line number (file ledger).
    #[error("not_an_object at {0}")]
    NotAnObject(String),
    /// An event's `event_digest` no longer recomputes over its content — a
    /// tampered ledger. Carries the event's `seq`.
    #[error("event_digest_mismatch at seq {0}")]
    EventDigestMismatch(u64),
    /// An explicit `epoch` below the ledger's last epoch; refused before any
    /// byte is written.
    #[error("epoch_regression")]
    EpochRegression,
    /// The `<path>.lock` could not be acquired within the attempt budget.
    #[error("ledger_locked: {0}")]
    LedgerLocked(PathBuf),
    /// Underlying filesystem error.
    #[error("io: {0}")]
    Io(String),
}

/// How [`append`] resolved.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AppendStatus {
    /// The event was newly written.
    Appended,
    /// An event with the same `event_digest` was already present; the
    /// existing event is returned unchanged (idempotent replay).
    AlreadyRecorded,
}

/// Result of a successful [`append`].
#[derive(Debug, Clone, PartialEq)]
pub struct Appended {
    /// The stamped event as it exists in the ledger (seq/epoch/vc/digest).
    pub event: Value,
    pub status: AppendStatus,
}

/// Options for [`append`] (port of the igniter's `opts` keyword list).
#[derive(Debug, Clone, Default)]
pub struct AppendOpts {
    /// The writer's vector clock, `%{"<replica>" => counter}` (non-negative
    /// integer counters). Absent → the event carries none.
    pub vc: Option<BTreeMap<String, u64>>,
    /// Explicit epoch stamp; must be `>=` the ledger's last epoch.
    pub epoch: Option<u64>,
    /// Explicit advance; stamped epoch = last + bump; the marker is persisted
    /// so the declared generation survives even if no event follows.
    pub epoch_bump: Option<u64>,
}

/// Resolves whether `path` is a [`LedgerKind::Dir`] or [`LedgerKind::File`]
/// ledger (see the module doc).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LedgerKind {
    Dir,
    File,
}

pub fn kind(path: &Path) -> LedgerKind {
    if path.is_dir() {
        LedgerKind::Dir
    } else if path.is_file() {
        LedgerKind::File
    } else if path
        .extension()
        .map(|e| FILE_EXTENSIONS.contains(&e.to_string_lossy().to_string().as_str()))
        .unwrap_or(false)
    {
        LedgerKind::File
    } else {
        LedgerKind::Dir
    }
}

/// The digest [`append`] stamps and [`fetch`] verifies: canonical sha256 over
/// the event minus `seq`/`event_digest` — nothing is elided, so the event
/// commits to the receipt it records, independent of map insertion order.
pub fn event_digest(event: &Map<String, Value>) -> String {
    let mut stripped = event.clone();
    stripped.remove("seq");
    stripped.remove("event_digest");
    digest_of(&Value::Object(stripped))
}

/// `true` when vector clock `a` dominates `b`: for every replica `r`,
/// `a[r] >= b[r]` AND for at least one replica `a[r] > b[r]` — a missing
/// replica counts as 0. Two equal clocks do not dominate each other, and the
/// empty clock dominates nothing.
pub fn vc_dominates(a: &Map<String, Value>, b: &Map<String, Value>) -> bool {
    let mut replicas: Vec<&String> = a.keys().chain(b.keys()).collect();
    replicas.sort();
    replicas.dedup();
    let ge = replicas
        .iter()
        .all(|r| vc_get(a, r) >= vc_get(b, r));
    let gt = replicas
        .iter()
        .any(|r| vc_get(a, r) > vc_get(b, r));
    ge && gt
}

/// `true` when the clocks are UNORDERED: neither `a[r] <= b[r]` for all `r`
/// nor `b[r] <= a[r]` for all `r` (missing replica counts as 0). Two EQUAL
/// clocks are the same history, never a conflict — a replay carries the same
/// clock as the event it replays and must not refuse.
pub fn vc_concurrent(a: &Map<String, Value>, b: &Map<String, Value>) -> bool {
    !vc_le(a, b) && !vc_le(b, a)
}

/// `a <= b` componentwise (missing replica counts 0); enumerating `a` is
/// sufficient — a replica only in `b` has `a[r] = 0 <= b[r]` trivially.
fn vc_le(a: &Map<String, Value>, b: &Map<String, Value>) -> bool {
    a.iter()
        .all(|(r, n)| vc_as_u64(n) <= vc_get(b, r))
}

fn vc_get(clock: &Map<String, Value>, replica: &str) -> u64 {
    clock.get(replica).map(vc_as_u64).unwrap_or(0)
}

fn vc_as_u64(v: &Value) -> u64 {
    v.as_u64().unwrap_or(0)
}

/// The events of the ledger at `path`, ordered by `seq`. Panics naming the
/// typed refusal on a ledger [`fetch`] would refuse — use [`fetch`] for the
/// typed form.
pub fn read(path: &Path) -> Vec<Value> {
    match fetch(path) {
        Ok(events) => events,
        Err(e) => panic!("ledger refused: {e}"),
    }
}

/// [`read`] behind a typed refusal: `Ok(events)`, or
/// `Err(LedgerError::…)` when the ledger cannot be decoded or any event's
/// `event_digest` does not recompute over its content.
pub fn fetch(path: &Path) -> Result<Vec<Value>, LedgerError> {
    let events = match kind(path) {
        LedgerKind::Dir => decode_dir(path)?,
        LedgerKind::File => decode_file(path)?,
    };
    verify(&events)?;
    Ok(events)
}

/// Typed refusal naming the first event whose `event_digest` no longer
/// recomputes over its content (a tampered ledger), `Ok(())` otherwise.
fn verify(events: &[Value]) -> Result<(), LedgerError> {
    for e in events {
        let obj = match e.as_object() {
            Some(obj) => obj,
            None => continue,
        };
        let stored = obj.get("event_digest").and_then(|d| d.as_str());
        if stored != Some(event_digest(obj).as_str()) {
            return Err(LedgerError::EventDigestMismatch(
                obj.get("seq").and_then(|s| s.as_u64()).unwrap_or(0),
            ));
        }
    }
    Ok(())
}

/// Appends `event` to the ledger at `path`, stamping `seq`, `event_digest`,
/// `epoch`, and — only when `opts.vc` is given — `vc`. Any caller-supplied
/// `vc`/`epoch`/`event_digest` is overwritten.
pub fn append(path: &Path, event: &Map<String, Value>, opts: &AppendOpts) -> Result<Appended, LedgerError> {
    match kind(path) {
        LedgerKind::Dir => append_dir(path, event, opts),
        LedgerKind::File => append_file(path, event, opts),
    }
}

// ── shared: the L3 stamp ────────────────────────────────────────────────────

/// The stamp decision, taken against the ledger's own critical section (the
/// events just decoded): explicit `epoch` must not regress; `epoch_bump`
/// advances; otherwise the epoch stays. On success returns the stamped event
/// (vc, then epoch, then digest — in that order, so the digest commits to
/// both new fields).
fn stamp(
    path: &Path,
    events: &[Value],
    event: &Map<String, Value>,
    opts: &AppendOpts,
) -> Result<Value, LedgerError> {
    let last = last_epoch(path, events);
    let epoch = if let Some(e) = opts.epoch {
        if e < last {
            return Err(LedgerError::EpochRegression);
        }
        e
    } else if let Some(b) = opts.epoch_bump {
        last + b
    } else {
        last
    };

    let mut stamped = event.clone();
    stamped.remove("vc");
    stamped.remove("epoch");
    stamped.remove("event_digest");
    if let Some(vc) = &opts.vc {
        stamped.insert(
            "vc".to_string(),
            Value::Object(vc.iter().map(|(k, v)| (k.clone(), Value::from(*v))).collect()),
        );
    }
    stamped.insert("epoch".to_string(), Value::from(epoch));
    stamped.insert("event_digest".to_string(), Value::from(event_digest(&stamped)));

    if epoch > marker_epoch(path) {
        write_epoch_marker(path, epoch)?;
    }
    Ok(Value::Object(stamped))
}

/// Last epoch = max over the ledger's events' epochs and the marker file;
/// pre-L3 events carry no epoch and contribute 0.
fn last_epoch(path: &Path, events: &[Value]) -> u64 {
    events
        .iter()
        .map(|e| e.get("epoch").and_then(|x| x.as_u64()).unwrap_or(0))
        .max()
        .unwrap_or(0)
        .max(marker_epoch(path))
}

fn epoch_marker(path: &Path) -> PathBuf {
    if kind(path) == LedgerKind::Dir {
        path.join("epoch")
    } else {
        PathBuf::from(format!("{}.epoch", path.display()))
    }
}

fn marker_epoch(path: &Path) -> u64 {
    fs::read_to_string(epoch_marker(path))
        .ok()
        .and_then(|body| body.trim().parse::<u64>().ok())
        .unwrap_or(0)
}

/// The marker follows the `.claim` spirit: a durable record of the declared
/// generation, max-guarded so it never moves backwards.
fn write_epoch_marker(path: &Path, epoch: u64) -> Result<(), LedgerError> {
    let marker = epoch_marker(path);
    if epoch > marker_epoch(path) {
        write_atomic_or_create(&marker, epoch.to_string().as_bytes())?;
    }
    Ok(())
}

fn io_err(e: std::io::Error) -> LedgerError {
    LedgerError::Io(e.to_string())
}

// ── directory ledger ────────────────────────────────────────────────────────

fn decode_dir(dir: &Path) -> Result<Vec<Value>, LedgerError> {
    let entries = match fs::read_dir(dir) {
        Ok(entries) => entries,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(e) => return Err(io_err(e)),
    };
    let mut names: Vec<String> = entries
        .filter_map(|e| e.ok())
        .map(|e| e.file_name().to_string_lossy().to_string())
        .filter(|n| n.ends_with(".json"))
        .collect();
    names.sort();
    let mut events = Vec::new();
    for name in names {
        let path = dir.join(&name);
        let body = fs::read_to_string(&path).map_err(io_err)?;
        events.push(decode_event(&body, &name)?);
    }
    Ok(events)
}

/// One ledger entry: only a JSON OBJECT is an event. Valid JSON of any other
/// shape (array, string, number, null) is a typed refusal located at `where`.
fn decode_event(json: &str, where_: &str) -> Result<Value, LedgerError> {
    match serde_json::from_str::<Value>(json) {
        Ok(Value::Object(_)) => Ok(serde_json::from_str(json).map_err(|e| LedgerError::Unreadable(e.to_string()))?),
        Ok(_not_an_object) => Err(LedgerError::NotAnObject(where_.to_string())),
        Err(e) => Err(LedgerError::Unreadable(e.to_string())),
    }
}

fn append_dir(
    dir: &Path,
    event: &Map<String, Value>,
    opts: &AppendOpts,
) -> Result<Appended, LedgerError> {
    fs::create_dir_all(dir).map_err(io_err)?;
    let events = decode_dir(dir)?;
    // Append refuses the same way `fetch` does, before any byte is written.
    verify(&events)?;
    let stamped = stamp(dir, &events, event, opts)?;
    let digest = stamped
        .get("event_digest")
        .and_then(|d| d.as_str())
        .unwrap_or_default()
        .to_string();

    if let Some(existing) = find(&events, &digest) {
        return Ok(Appended {
            event: existing,
            status: AppendStatus::AlreadyRecorded,
        });
    }
    claim_and_write(dir, stamped, &digest)
}

/// Two exclusive-create claims make concurrent writers safe: a per-digest
/// claim (one writer owns an event; duplicates wait for its file) and a
/// per-seq claim (one writer owns a seq). The event file itself is written to
/// a temp name and renamed, so readers never observe a partial file.
fn claim_and_write(dir: &Path, mut stamped: Value, digest: &str) -> Result<Appended, LedgerError> {
    let digest_claim = dir.join(format!(
        "digest-{}.claim",
        digest.strip_prefix("sha256:").unwrap_or(digest)
    ));
    match exclusive_create(&digest_claim) {
        Ok(()) => {}
        Err(LedgerError::Io(_)) => {
            // Another writer owns this digest: it will land the event file.
            // Poll the observable (the file's presence) under a bounded
            // budget — never a fixed wait-and-hope.
            return await_existing(dir, digest);
        }
        Err(e) => return Err(e),
    }

    // Digest claim owned: assign the next free seq under a per-seq claim.
    let seq = next_seq(dir)?;
    let pad = format!("{:08}", seq);
    let seq_claim = dir.join(format!("seq-{pad}.claim"));
    match exclusive_create(&seq_claim) {
        Ok(()) => {}
        Err(_) => return await_existing(dir, digest),
    }

    stamped
        .as_object_mut()
        .expect("stamped is an object")
        .insert("seq".to_string(), Value::from(seq));
    let fragment = &digest["sha256:".len()..std::cmp::min("sha256:".len() + 16, digest.len())];
    let name = format!("{pad}-{fragment}.json");
    let tmp = dir.join(format!(".tmp-{pad}-{}.tmp", std::process::id()));
    write_atomic_or_create(&tmp, serde_json::to_string(&stamped).unwrap().as_bytes())?;
    fs::rename(&tmp, dir.join(&name)).map_err(io_err)?;
    Ok(Appended {
        event: stamped,
        status: AppendStatus::Appended,
    })
}

/// Another writer claimed the digest/seq: poll for the existing event file
/// (the observable) under a bounded budget, then return it idempotently.
fn await_existing(dir: &Path, digest: &str) -> Result<Appended, LedgerError> {
    for _ in 0..LOCK_ATTEMPTS {
        let events = decode_dir(dir)?;
        if let Some(existing) = find(&events, digest) {
            return Ok(Appended {
                event: existing,
                status: AppendStatus::AlreadyRecorded,
            });
        }
        std::thread::yield_now();
    }
    Err(LedgerError::LedgerLocked(dir.to_path_buf()))
}

fn find(events: &[Value], digest: &str) -> Option<Value> {
    events
        .iter()
        .find(|e| e.get("event_digest").and_then(|d| d.as_str()) == Some(digest))
        .cloned()
}

fn next_seq(dir: &Path) -> Result<u64, LedgerError> {
    let claims = fs::read_dir(dir)
        .map_err(io_err)?
        .filter_map(|e| e.ok())
        .filter(|e| {
            let n = e.file_name().to_string_lossy().to_string();
            n.starts_with("seq-") && n.ends_with(".claim")
        })
        .count() as u64;
    let events = decode_dir(dir)?.len() as u64;
    Ok(claims.max(events) + 1)
}

// ── file ledger (ndjson) ────────────────────────────────────────────────────

/// Line numbers are 1-based PHYSICAL lines (blank lines count, and are
/// skipped), so a refusal points at the exact line an operator opens.
fn decode_file(path: &Path) -> Result<Vec<Value>, LedgerError> {
    let body = match fs::read_to_string(path) {
        Ok(body) => body,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(e) => return Err(io_err(e)),
    };
    let mut events = Vec::new();
    for (idx, line) in body.split('\n').enumerate() {
        if line.trim().is_empty() {
            continue;
        }
        let event = decode_event(line, &(idx + 1).to_string())?;
        events.push(event);
    }
    // Ordered by seq; pre-seq events contribute 0 (the igniter's `|| 0`).
    events.sort_by_key(|e| e.get("seq").and_then(|s| s.as_u64()).unwrap_or(0));
    Ok(events)
}

fn append_file(
    path: &Path,
    event: &Map<String, Value>,
    opts: &AppendOpts,
) -> Result<Appended, LedgerError> {
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent).map_err(io_err)?;
        }
    }
    let lock = PathBuf::from(format!("{}.lock", path.display()));
    with_lock(&lock, || {
        let events = decode_file(path)?;
        // Append refuses the same way `fetch` does, before any byte is written.
        verify(&events)?;
        let stamped = stamp(path, &events, event, opts)?;
        let digest = stamped
            .get("event_digest")
            .and_then(|d| d.as_str())
            .unwrap_or_default()
            .to_string();
        if let Some(existing) = find(&events, &digest) {
            return Ok(Appended {
                event: existing,
                status: AppendStatus::AlreadyRecorded,
            });
        }
        // Called only while holding the ledger lock: the next seq is one past
        // the highest seq on disk, and the line is appended (the file is
        // never rewritten).
        let seq = events
            .iter()
            .map(|e| e.get("seq").and_then(|s| s.as_u64()).unwrap_or(0))
            .max()
            .unwrap_or(0)
            + 1;
        let mut stamped = stamped;
        stamped
            .as_object_mut()
            .expect("stamped is an object")
            .insert("seq".to_string(), Value::from(seq));
        let mut f = fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .map_err(io_err)?;
        writeln!(f, "{}", serde_json::to_string(&stamped).unwrap()).map_err(io_err)?;
        Ok(Appended {
            event: stamped,
            status: AppendStatus::Appended,
        })
    })
}

/// Bounded exclusive-create lock; refuses [`LedgerError::LedgerLocked`] after
/// the attempt budget instead of waiting on a clock.
fn with_lock<T>(lock: &Path, mut f: impl FnMut() -> Result<T, LedgerError>) -> Result<T, LedgerError> {
    for _ in 0..LOCK_ATTEMPTS {
        match exclusive_create(lock) {
            Ok(()) => {
                let result = f();
                let _ = fs::remove_file(lock);
                return result;
            }
            Err(LedgerError::Io(_)) => std::thread::yield_now(),
            Err(e) => return Err(e),
        }
    }
    Err(LedgerError::LedgerLocked(lock.to_path_buf()))
}

/// Exclusive-create a file (`File.open(_, [:write, :exclusive])` port).
fn exclusive_create(path: &Path) -> Result<(), LedgerError> {
    fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(path)
        .map(|_| ())
        .map_err(io_err)
}

/// Write via temp + rename so readers never observe a partial file.
fn write_atomic_or_create(path: &Path, bytes: &[u8]) -> Result<(), LedgerError> {
    let tmp = path.with_extension("tmp");
    {
        let mut f = fs::File::create(&tmp).map_err(io_err)?;
        f.write_all(bytes).map_err(io_err)?;
        f.sync_all().ok();
    }
    fs::rename(&tmp, path).map_err(io_err)
}
