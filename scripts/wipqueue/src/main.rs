use anyhow::Result;
use ignore::{DirEntry, WalkBuilder};
use regex::Regex;
use serde::Serialize;
use sha1::{Digest, Sha1};
use std::{
    cmp::Ordering,
    fs,
    path::{Path, PathBuf},
};

#[derive(Serialize)]
struct Task {
    path: String,
    line: usize,
    col: usize,
    kind: String,
    text: String,
    lock_path: String,
}

fn is_code_file(de: &DirEntry) -> bool {
    if de.file_type().map(|t| t.is_file()).unwrap_or(false) {
        let name = de.path().file_name().and_then(|s| s.to_str()).unwrap_or("");
        let skip = [
            ".lock", ".min.", ".snap", ".png", ".jpg", ".jpeg", ".gif", ".wasm", ".class", ".jar",
            ".bin", ".pdf",
        ]
        .iter()
        .any(|s| name.contains(s));
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
    let re = Regex::new(
        r#"(?x)
        (//\s*(WIP|TODO|UNIMPL)\s*:\s*(?P<msg_line>.+)$)
        |
        (unimplemented!\(\s*"(?P<msg_unimpl>[^"]+)"\s*\))
        |
        (todo!\(\s*"(?P<msg_todo>[^"]+)"\s*\))
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

    // Depth-first order: primary key = depth ascending (visit shallow dirs first),
    // but we want DFS lexicographic: sort by path then stable by depth.
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
        // Even if locked, we still list it; agents will skip locked tasks.
        for (idx, line) in content.lines().enumerate() {
            if let Some(caps) = re.captures(line) {
                let msg = caps
                    .name("msg_line")
                    .or_else(|| caps.name("msg_unimpl"))
                    .or_else(|| caps.name("msg_todo"))
                    .map(|m| m.as_str().trim().to_string())
                    .unwrap_or_default();

                let kind = if caps.name("msg_unimpl").is_some() {
                    "UNIMPL"
                } else if caps.name("msg_todo").is_some() {
                    "TODO"
                } else {
                    "WIP"
                }
                .to_string();

                tasks.push(Task {
                    path: file
                        .strip_prefix(&root)
                        .unwrap_or(&file)
                        .to_string_lossy()
                        .into(),
                    line: idx + 1,
                    col: line.find(&msg).map(|c| c).unwrap_or(1),
                    kind,
                    text: msg,
                    lock_path: lock
                        .strip_prefix(&root)
                        .unwrap_or(&lock)
                        .to_string_lossy()
                        .into(),
                });
            }
        }
    }

    // Filter: prefer unlocked tasks first; stable order preserved.
    let mut unlocked: Vec<&Task> = tasks
        .iter()
        .filter(|t| !locked(&root.join(&t.lock_path)))
        .collect();
    let mut locked_tasks: Vec<&Task> = tasks
        .iter()
        .filter(|t| locked(&root.join(&t.lock_path)))
        .collect();

    // Output: unlocked first, then locked. Agents must pick index 0 if exists.
    unlocked.append(&mut locked_tasks);
    let out: Vec<&Task> = unlocked;

    println!("{}", serde_json::to_string_pretty(&out)?);
    Ok(())
}
