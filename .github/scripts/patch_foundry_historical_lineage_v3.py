#!/usr/bin/env python3
"""Compose the proven v2 historical verifier with multi-commit set law."""
from __future__ import annotations

import textwrap
from pathlib import Path

WORKFLOW = Path(".github/workflows/foundry-historical-lineage-clean-room-v2.yml")


def main() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")
    start_marker = "          python3 - <<'PY'\n"
    end_marker = "\n          PY\n"
    start = workflow.index(start_marker) + len(start_marker)
    end = workflow.index(end_marker, start)
    generator = textwrap.dedent(workflow[start:end])

    insertion = r"""
script = script.replace(
    '''    historical_commit: String,
    source_path: String,
''',
    '''    historical_commit: String,
    #[serde(default)]
    historical_commits: Vec<String>,
    source_path: String,
''',
    1,
)
script = script.replace(
    '''    historical_commit: String,
    requested_source_path: String,
''',
    '''    historical_commit: String,
    #[serde(default)]
    historical_commits: Vec<String>,
    requested_source_path: String,
''',
    1,
)
old_component_commit = '''    if !is_full_git_sha(&record.historical_commit) {
        return refusal(
            "HISTORICAL_LINEAGE_COMMIT_INVALID",
            format!("{}: {}", record.capability_id, record.historical_commit),
        );
    }
    let commit_type = git(source_path, &["cat-file", "-t", &record.historical_commit])?;
    if commit_type != "commit" {
        return refusal(
            "HISTORICAL_LINEAGE_COMMIT_NOT_COMMIT",
            format!("{}: {}", record.capability_id, commit_type),
        );
    }
'''
new_component_commit = '''    let record_commits = historical_commit_set(
        &record.capability_id,
        &record.historical_commit,
        &record.historical_commits,
    )?;
    for commit in &record_commits {
        let commit_type = git(source_path, &["cat-file", "-t", commit])?;
        if commit_type != "commit" {
            return refusal(
                "HISTORICAL_LINEAGE_COMMIT_NOT_COMMIT",
                format!("{}: {}={}", record.capability_id, commit, commit_type),
            );
        }
    }
'''
if old_component_commit not in script:
    raise SystemExit("component commit validation marker missing")
script = script.replace(old_component_commit, new_component_commit, 1)

old_manifest_identity = '''    if manifest.capability_id != record.capability_id
        || manifest.historical_commit != record.historical_commit
        || manifest.corpus_destination != record.destination_path
        || manifest.source_removed
    {
        return refusal(
            "HISTORICAL_LINEAGE_MANIFEST_IDENTITY_MISMATCH",
            record.capability_id.clone(),
        );
    }
'''
new_manifest_identity = '''    let manifest_commits = historical_commit_set(
        &manifest.capability_id,
        &manifest.historical_commit,
        &manifest.historical_commits,
    )?;
    if manifest.capability_id != record.capability_id
        || manifest_commits != record_commits
        || manifest.corpus_destination != record.destination_path
        || manifest.source_removed
    {
        return refusal(
            "HISTORICAL_LINEAGE_MANIFEST_IDENTITY_MISMATCH",
            record.capability_id.clone(),
        );
    }
'''
if old_manifest_identity not in script:
    raise SystemExit("manifest identity marker missing")
script = script.replace(old_manifest_identity, new_manifest_identity, 1)

old_object_commit = '''        let object_commit = if source_file.historical_commit.is_empty() {
            &record.historical_commit
        } else {
            &source_file.historical_commit
        };
        if !is_full_git_sha(object_commit) {
            return refusal(
                "HISTORICAL_LINEAGE_FILE_COMMIT_INVALID",
                format!("{}: {}", record.capability_id, object_commit),
            );
        }
'''
new_object_commit = '''        let object_commit = if source_file.historical_commit.is_empty() {
            if record_commits.len() != 1 {
                return refusal(
                    "HISTORICAL_LINEAGE_FILE_COMMIT_MISSING",
                    format!("{}: {} admitted commits", record.capability_id, record_commits.len()),
                );
            }
            record_commits.iter().next().expect("single commit")
        } else {
            &source_file.historical_commit
        };
        if !is_full_git_sha(object_commit) {
            return refusal(
                "HISTORICAL_LINEAGE_FILE_COMMIT_INVALID",
                format!("{}: {}", record.capability_id, object_commit),
            );
        }
        if !record_commits.contains(object_commit) {
            return refusal(
                "HISTORICAL_LINEAGE_FILE_COMMIT_OUTSIDE_COMPONENT_SET",
                format!("{}: {}", record.capability_id, object_commit),
            );
        }
'''
if old_object_commit not in script:
    raise SystemExit("file commit marker missing")
script = script.replace(old_object_commit, new_object_commit, 1)

helper_marker = '''fn is_full_git_sha(value: &str) -> bool {
    value.len() == 40 && value.bytes().all(|byte| byte.is_ascii_hexdigit())
}
'''
commit_set_helper = '''fn historical_commit_set(
    capability_id: &str,
    summary: &str,
    explicit: &[String],
) -> Result<BTreeSet<String>> {
    let summary_set: BTreeSet<String> = summary
        .split('|')
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(str::to_string)
        .collect();
    let explicit_set: BTreeSet<String> = explicit
        .iter()
        .map(|value| value.trim())
        .filter(|value| !value.is_empty())
        .map(str::to_string)
        .collect();
    let commits = if explicit_set.is_empty() {
        summary_set.clone()
    } else {
        if summary_set != explicit_set {
            return refusal(
                "HISTORICAL_LINEAGE_COMMIT_SET_MISMATCH",
                format!(
                    "{}: summary={:?}, explicit={:?}",
                    capability_id, summary_set, explicit_set
                ),
            );
        }
        explicit_set
    };
    if commits.is_empty() {
        return refusal(
            "HISTORICAL_LINEAGE_COMMIT_SET_EMPTY",
            capability_id.to_string(),
        );
    }
    for commit in &commits {
        if !is_full_git_sha(commit) {
            return refusal(
                "HISTORICAL_LINEAGE_COMMIT_INVALID",
                format!("{}: {}", capability_id, commit),
            );
        }
    }
    Ok(commits)
}

''' + helper_marker
if helper_marker not in script:
    raise SystemExit("commit helper marker missing")
script = script.replace(helper_marker, commit_set_helper, 1)
"""

    marker = "          required = [\n"
    if marker not in generator:
        raise SystemExit("v2 required marker missing")
    generator = generator.replace(marker, textwrap.dedent(insertion) + "\n" + marker, 1)
    generator = generator.replace(
        '"historical_commit: String,",',
        '"historical_commits: Vec<String>,",\n              "fn historical_commit_set(",',
        1,
    )
    exec(compile(generator, "historical-lineage-v3-generator", "exec"), {"__name__": "__main__"})


if __name__ == "__main__":
    main()
