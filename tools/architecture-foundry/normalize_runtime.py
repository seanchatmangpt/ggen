#!/usr/bin/env python3
"""Normalize the foundry runtime before exact-head Rust verification.

This is temporary manufacturing machinery for the initial runtime bootstrap. It is idempotent:
when the canonical source has landed, every replacement is already present and the script emits
no diff.
"""

from pathlib import Path

PATH = Path(__file__).parent / "src" / "lib.rs"


def normalize(text: str, old: str, new: str) -> str:
    if old in text:
        return text.replace(old, new, 1)
    if new not in text:
        raise SystemExit(f"normalization anchor missing: {old!r}")
    return text


def main() -> None:
    text = PATH.read_text()

    replacements = [
        (
            '"corpus:foundry/workstreams/state.json".to_string(),',
            '"projection:foundry/workstreams/state.json".to_string(),',
        ),
        (
            '"corpus:foundry/standing.json".to_string(),',
            '"projection:foundry/standing.json".to_string(),',
        ),
        (
            '        if repository == "external" {\n            continue;\n        }',
            '        if matches!(repository, "external" | "projection") {\n            continue;\n        }',
        ),
        (
            '    let run_id = subject_digest.chars().take(20).collect();',
            '    let subject_digest = digest_named_outputs(&output_digests);\n'
            '    let run_id = subject_digest.chars().take(20).collect();',
        ),
        (
            '    write_json(&state_path, &state)?;',
            '    write_json_replace(&state_path, &state)?;',
        ),
        (
            '    let standing_digest = write_json(&standing_path, &standing)?;',
            '    let standing_digest = write_json_replace(&standing_path, &standing)?;',
        ),
        (
            'fn write_bytes_exact(path: &Path, bytes: &[u8]) -> Result<()> {',
            "fn write_json_replace<T: Serialize>(path: &Path, value: &T) -> Result<String> {\n"
            "    let mut bytes = serde_json::to_vec_pretty(value)?;\n"
            "    bytes.push(b'\\n');\n"
            "    if let Some(parent) = path.parent() {\n"
            "        fs::create_dir_all(parent).map_err(|source| FoundryError::Io {\n"
            "            path: parent.display().to_string(),\n"
            "            source,\n"
            "        })?;\n"
            "    }\n"
            "    fs::write(path, &bytes).map_err(|source| FoundryError::Io {\n"
            "        path: path.display().to_string(),\n"
            "        source,\n"
            "    })?;\n"
            "    Ok(digest_bytes(&bytes))\n"
            "}\n\n"
            "fn write_bytes_exact(path: &Path, bytes: &[u8]) -> Result<()> {",
        ),
        (
            '''    let tracked_raw = git_bytes(&canonical, &["ls-files", "-z"])?;
    let mut tracked_files: Vec<PathBuf> = tracked_raw
        .split(|byte| *byte == 0)
        .filter(|bytes| !bytes.is_empty())
        .map(|bytes| PathBuf::from(String::from_utf8_lossy(bytes).to_string()))
        .collect();
    tracked_files.sort();
    let tracked_tree_digest = digest_relative_files(&canonical, &tracked_files)?;

    Ok(RepositorySnapshot {
        path: canonical.display().to_string(),
        head,
        branch,
        origin,
        clean: dirty_entries.is_empty(),
        dirty_entries,
        tracked_file_count: tracked_files.len(),
        tracked_tree_digest,
    })''',
            '''    // Hash Git index records rather than dereferencing the working tree. The staged
    // record binds mode, object ID, stage, and raw path bytes, so tracked symlinks,
    // gitlinks, and intentionally absent worktree targets remain observable.
    let tracked_index = git_bytes(&canonical, &["ls-files", "--stage", "-z"])?;
    let tracked_file_count = tracked_index
        .split(|byte| *byte == 0)
        .filter(|record| !record.is_empty())
        .count();
    let object_format = git(&canonical, &["rev-parse", "--show-object-format"])?;
    let mut tree_hasher = Hasher::new();
    hash_named_bytes(
        &mut tree_hasher,
        "git-object-format",
        object_format.as_bytes(),
    );
    hash_named_bytes(&mut tree_hasher, "git-index-stage-records", &tracked_index);
    let tracked_tree_digest = tree_hasher.finalize().to_hex().to_string();

    Ok(RepositorySnapshot {
        path: canonical.display().to_string(),
        head,
        branch,
        origin,
        clean: dirty_entries.is_empty(),
        dirty_entries,
        tracked_file_count,
        tracked_tree_digest,
    })''',
        ),
        (
            '''fn digest_relative_files(root: &Path, files: &[PathBuf]) -> Result<String> {
    let mut hasher = Hasher::new();
    for relative in files {
        let bytes = read(&root.join(relative))?;
        hash_named_bytes(&mut hasher, &relative.to_string_lossy(), &bytes);
    }
    Ok(hasher.finalize().to_hex().to_string())
}

''',
            '',
        ),
        (
            '    subject: &str,\n    subject_digest: String,\n    source_head: &str,',
            '    subject: &str,\n    _subject_digest: String,\n    source_head: &str,',
        ),
    ]

    for old, new in replacements:
        text = normalize(text, old, new)

    PATH.write_text(text)


if __name__ == "__main__":
    main()
