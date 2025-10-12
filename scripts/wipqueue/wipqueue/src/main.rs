use anyhow::Result;
use ignore::{DirEntry, WalkBuilder};
use regex::Regex;
use serde::Serialize;
use sha1::{Digest, Sha1};
use std::{cmp::Ordering, fs, path::{Path, PathBuf}};

#[derive(Serialize)]
struct Task {
    path: String,
    line: usize,
    col: usize,
    kind: String,
    text: String,
    lock_path: String,
    locked: bool,
}

fn is_code_file(de: &DirEntry) -> bool {
    if de.file_type().map(|t| t.is_file()).unwrap_or(false) {
        let name = de.path().file_name().and_then(|s| s.to_str()).unwrap_or("");
        let skip = [
            ".lock", ".min.", ".snap", ".png", ".jpg", ".jpeg", ".gif",
            ".wasm", ".class", ".jar", ".bin", ".pdf", ".svg", ".ico",
            ".woff", ".woff2", ".ttf", ".eot", ".mp4", ".webm", ".mp3",
            ".ogg", ".wav", ".flac", ".zip", ".tar", ".gz", ".bz2",
            ".7z", ".rar", ".so", ".dylib", ".dll", ".exe",
        ].iter().any(|s| name.contains(s));
        !skip
    } else {
        false
    }
}

fn sha1_hex(s: &str) -> String {
    let mut h = Sha1::new();
    h.update(s.as_bytes());
    format!("{:x}", h.finalize())
}

fn lock_path_for(root: &Path, file: &Path) -> PathBuf {
    let rel = file.strip_prefix(root).unwrap_or(file);
    let key = rel.to_string_lossy();
    root.join(".wiplocks").join(sha1_hex(&key))
}

fn locked(lock: &Path) -> bool {
    lock.exists()
}

fn main() -> Result<()> {
    let root = std::env::current_dir()?;

    // Enhanced regex to catch all WIP markers
    let re = Regex::new(
        r#"(?x)
        (?://\s*(?P<comment_kind>WIP|TODO|UNIMPL)\s*:\s*(?P<msg_line>.+)$)
        |
        (?:unimplemented!\(\s*"(?P<msg_unimpl>[^"]+)"\s*\))
        |
        (?:todo!\(\s*"(?P<msg_todo>[^"]+)"\s*\))
        "#,
    )?;

    // DFS walk: we build a stack of directories sorted lexicographically.
    // ignore::WalkBuilder already respects .gitignore; we post-sort entries.
    let mut files: Vec<PathBuf> = WalkBuilder::new(&root)
        .hidden(false)
        .git_ignore(true)
        .git_global(true)
        .git_exclude(true)
        .build()
        .filter_map(|r| r.ok())
        .filter(is_code_file)
        .map(|d| d.into_path())
        .collect();

    // Depth-first order: sort by path lexicographically
    files.sort_by(|a, b| {
        let ap = a.to_string_lossy();
        let bp = b.to_string_lossy();
        match ap.cmp(&bp) {
            Ordering::Equal => a.components().count().cmp(&b.components().count()),
            o => o,
        }
    });

    let mut tasks = Vec::<Task>::new();

    for file in files {
        let content = match fs::read_to_string(&file) {
            Ok(c) => c,
            Err(_) => continue,
        };

        let lock = lock_path_for(&root, &file);
        let is_locked = locked(&lock);

        for (idx, line) in content.lines().enumerate() {
            if let Some(caps) = re.captures(line) {
                let msg = caps
                    .name("msg_line")
                    .or_else(|| caps.name("msg_unimpl"))
                    .or_else(|| caps.name("msg_todo"))
                    .map(|m| m.as_str().trim().to_string())
                    .unwrap_or_default();

                let kind = if let Some(k) = caps.name("comment_kind") {
                    k.as_str().to_string()
                } else if caps.name("msg_unimpl").is_some() {
                    "UNIMPL".to_string()
                } else if caps.name("msg_todo").is_some() {
                    "TODO".to_string()
                } else {
                    "WIP".to_string()
                };

                tasks.push(Task {
                    path: file.strip_prefix(&root).unwrap_or(&file).to_string_lossy().into(),
                    line: idx + 1,
                    col: line.find(&msg).unwrap_or(1),
                    kind,
                    text: msg,
                    lock_path: lock.strip_prefix(&root).unwrap_or(&lock).to_string_lossy().into(),
                    locked: is_locked,
                });
            }
        }
    }

    // Sort: unlocked tasks first, then by path and line number
    tasks.sort_by(|a, b| {
        match (a.locked, b.locked) {
            (false, true) => Ordering::Less,
            (true, false) => Ordering::Greater,
            _ => match a.path.cmp(&b.path) {
                Ordering::Equal => a.line.cmp(&b.line),
                o => o,
            }
        }
    });

    println!("{}", serde_json::to_string_pretty(&tasks)?);
    Ok(())
}
