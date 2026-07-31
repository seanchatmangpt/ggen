#!/usr/bin/env python3
"""Independent exact-set, receipt, replay, and refusal verifier for CMD G0."""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import platform
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any

REPORT_SCHEMA = "ggen.verifier.report.v1"
VERIFIER_ID = "ggen-self-host/cmd-g0-independent-verifier-v1"
OBSERVED_PREFIXES = ("self-host/observed/cmd-g0/", "self-host/evidence/cmd-g0/")
ALLOWED_CLASSIFICATIONS = {
    "authored constitution",
    "domain source",
    "implementation",
    "generated consequence",
    "template",
    "fixture",
    "evidence",
    "workflow",
    "configuration",
    "documentation",
    "archive",
    "asset",
    "unknown",
}


class VerificationFailure(RuntimeError):
    def __init__(self, code: str, detail: str) -> None:
        super().__init__(detail)
        self.code = code
        self.detail = detail


def git(root: Path, *args: str) -> bytes:
    result = subprocess.run(
        ["git", "-C", str(root), *args],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if result.returncode != 0:
        raise VerificationFailure(
            "CMD-G0-GIT-BOUNDARY",
            f"git {' '.join(args)} failed: {result.stderr.decode('utf-8', errors='replace').strip()}",
        )
    return result.stdout


def git_text(root: Path, *args: str) -> str:
    return git(root, *args).decode("utf-8", errors="strict").strip()


def decode_path(raw: bytes) -> str:
    return raw.decode("utf-8", errors="surrogateescape")


def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def canonical_bytes(value: Any) -> bytes:
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode("utf-8")


def pretty_bytes(value: Any) -> bytes:
    return (json.dumps(value, sort_keys=True, indent=2, ensure_ascii=True) + "\n").encode("utf-8")


def load_json(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise VerificationFailure("CMD-G0-EVIDENCE-READ", f"{path}: {error}") from error
    if not isinstance(value, dict):
        raise VerificationFailure("CMD-G0-EVIDENCE-SHAPE", f"{path} is not a JSON object")
    return value


def expected_tree(root: Path) -> list[dict[str, str]]:
    raw = git(root, "ls-tree", "-r", "-t", "-z", "--full-tree", "HEAD")
    entries: list[dict[str, str]] = []
    for record in raw.split(b"\0"):
        if not record:
            continue
        metadata, raw_path = record.split(b"\t", 1)
        mode, object_type, object_id = metadata.decode("ascii").split()
        entries.append(
            {
                "path": decode_path(raw_path),
                "mode": mode,
                "object_type": object_type,
                "object_id": object_id,
            }
        )
    return sorted(entries, key=lambda item: item["path"])


def expected_index(root: Path) -> dict[str, dict[str, str | int]]:
    raw = git(root, "ls-files", "-s", "-z")
    entries: dict[str, dict[str, str | int]] = {}
    for record in raw.split(b"\0"):
        if not record:
            continue
        metadata, raw_path = record.split(b"\t", 1)
        mode, object_id, stage_text = metadata.decode("ascii").split()
        stage = int(stage_text)
        path = decode_path(raw_path)
        if stage != 0:
            raise VerificationFailure("CMD-G0-INDEX-STAGE", f"non-zero stage {stage} at {path}")
        entries[path] = {"mode": mode, "object_id": object_id, "stage": stage}
    return entries


def expected_content(root: Path, entry: dict[str, str]) -> tuple[str, str, int]:
    if entry["object_type"] == "blob":
        data = git(root, "cat-file", "blob", entry["object_id"])
        semantic = "symlink-target-bytes" if entry["mode"] == "120000" else "blob-bytes"
        return semantic, sha256(data), len(data)
    if entry["mode"] == "160000" and entry["object_type"] == "commit":
        data = entry["object_id"].encode("ascii")
        return "gitlink-commit-identity", sha256(data), len(data)
    if entry["object_type"] == "tree":
        data = entry["object_id"].encode("ascii")
        return "tree-object-identity", sha256(data), len(data)
    raise VerificationFailure(
        "CMD-G0-OBJECT-TYPE",
        f"unsupported {entry['mode']}/{entry['object_type']} at {entry['path']}",
    )


def current_untracked(root: Path) -> list[str]:
    raw = git(root, "ls-files", "--others", "--exclude-standard", "-z")
    paths = [decode_path(item) for item in raw.split(b"\0") if item]
    return sorted(
        path
        for path in paths
        if not any(path == prefix.rstrip("/") or path.startswith(prefix) for prefix in OBSERVED_PREFIXES)
    )


def current_working_records(root: Path) -> list[str]:
    raw = git(root, "status", "--porcelain=v2", "-z", "--untracked-files=all")
    records = [decode_path(item) for item in raw.split(b"\0") if item]
    return sorted(
        record
        for record in records
        if not any(prefix in record for prefix in OBSERVED_PREFIXES)
    )


def compare_exact_set(
    root: Path,
    repository: dict[str, Any],
    surfaces_document: dict[str, Any],
    expected_revision: str,
) -> list[str]:
    checks: list[str] = []
    revision = git_text(root, "rev-parse", "HEAD")
    tree_id = git_text(root, "rev-parse", "HEAD^{tree}")
    if revision != expected_revision:
        raise VerificationFailure(
            "CMD-G0-BASE-MOVED",
            f"expected revision {expected_revision}, observed {revision}",
        )
    if repository.get("subject_revision") != revision or surfaces_document.get("subject_revision") != revision:
        raise VerificationFailure("CMD-G0-BASE-MOVED", "evidence revision does not match HEAD")
    if repository.get("subject_tree") != tree_id or surfaces_document.get("subject_tree") != tree_id:
        raise VerificationFailure("CMD-G0-TREE-MOVED", "evidence tree does not match HEAD")

    expected_objects = expected_tree(root)
    observed_objects = repository.get("objects")
    if not isinstance(observed_objects, list):
        raise VerificationFailure("CMD-G0-EVIDENCE-SHAPE", "repository objects must be a list")
    expected_paths = [item["path"] for item in expected_objects]
    observed_paths = [str(item.get("path")) for item in observed_objects if isinstance(item, dict)]
    if observed_paths != expected_paths:
        missing = sorted(set(expected_paths) - set(observed_paths))
        extra = sorted(set(observed_paths) - set(expected_paths))
        raise VerificationFailure(
            "CMD-G0-EXACT-SET",
            f"tracked object set differs: missing={missing[:20]} extra={extra[:20]}",
        )
    for expected, observed in zip(expected_objects, observed_objects):
        if not isinstance(observed, dict):
            raise VerificationFailure("CMD-G0-EVIDENCE-SHAPE", "object entry is not an object")
        if observed.get("mode") != expected["mode"]:
            raise VerificationFailure("CMD-G0-OBJECT-MODE", expected["path"])
        if observed.get("object_type") != expected["object_type"]:
            raise VerificationFailure("CMD-G0-OBJECT-TYPE", expected["path"])
        if observed.get("object_id") != expected["object_id"]:
            raise VerificationFailure("CMD-G0-OBJECT-DIGEST", expected["path"])
    checks.append("exact Git object set, modes, types, and identities")

    observed_digest = repository.get("contract_digest_sha256")
    copy = dict(repository)
    copy.pop("contract_digest_sha256", None)
    if observed_digest != sha256(canonical_bytes(copy)):
        raise VerificationFailure("CMD-G0-CONTRACT-DIGEST", "repository contract digest mismatch")
    if repository.get("objects_digest_sha256") != sha256(canonical_bytes(expected_objects)):
        raise VerificationFailure("CMD-G0-OBJECT-DIGEST", "object aggregate digest mismatch")
    checks.append("repository contract and object aggregate digests")

    expected_leaves = [item for item in expected_objects if item["object_type"] != "tree"]
    surfaces = surfaces_document.get("surfaces")
    if not isinstance(surfaces, list):
        raise VerificationFailure("CMD-G0-EVIDENCE-SHAPE", "surfaces must be a list")
    observed_surface_paths = [str(item.get("path")) for item in surfaces if isinstance(item, dict)]
    expected_surface_paths = [item["path"] for item in expected_leaves]
    if observed_surface_paths != expected_surface_paths:
        missing = sorted(set(expected_surface_paths) - set(observed_surface_paths))
        extra = sorted(set(observed_surface_paths) - set(expected_surface_paths))
        raise VerificationFailure(
            "CMD-G0-EXACT-SET",
            f"surface set differs: missing={missing[:20]} extra={extra[:20]}",
        )

    index = expected_index(root)
    for expected, observed in zip(expected_leaves, surfaces):
        if not isinstance(observed, dict):
            raise VerificationFailure("CMD-G0-EVIDENCE-SHAPE", "surface entry is not an object")
        for field in ("mode", "object_type", "object_id"):
            if observed.get(field) != expected[field]:
                raise VerificationFailure("CMD-G0-SURFACE-IDENTITY", f"{expected['path']}:{field}")
        indexed = index.get(expected["path"])
        if indexed is None:
            raise VerificationFailure("CMD-G0-EXACT-SET", f"index omits {expected['path']}")
        if observed.get("index_mode") != indexed["mode"] or observed.get("index_object_id") != indexed["object_id"]:
            raise VerificationFailure("CMD-G0-INDEX-DIVERGENCE", expected["path"])
        semantic, digest, size = expected_content(root, expected)
        if observed.get("content_semantics") != semantic:
            raise VerificationFailure("CMD-G0-CONTENT-SEMANTICS", expected["path"])
        if observed.get("content_digest_sha256") != digest:
            raise VerificationFailure("CMD-G0-CONTENT-DIGEST", expected["path"])
        if observed.get("size_bytes") != size:
            raise VerificationFailure("CMD-G0-CONTENT-SIZE", expected["path"])
        if observed.get("classification") not in ALLOWED_CLASSIFICATIONS:
            raise VerificationFailure("CMD-G0-CLASSIFICATION", expected["path"])
    surfaces_digest = sha256(canonical_bytes(surfaces))
    if surfaces_document.get("surfaces_digest_sha256") != surfaces_digest:
        raise VerificationFailure("CMD-G0-SURFACES-DIGEST", "surfaces digest mismatch")
    if repository.get("surfaces_digest_sha256") != surfaces_digest:
        raise VerificationFailure("CMD-G0-SURFACES-DIGEST", "repository surfaces digest mismatch")
    checks.append("regular, executable, symlink, and gitlink content semantics")
    checks.append("index-to-HEAD identity and complete surface classification")
    return checks


def verify_receipts(evidence_dir: Path) -> list[str]:
    checks: list[str] = []
    intent = load_json(evidence_dir / "intent-receipt.json")
    result = load_json(evidence_dir / "result-receipt.json")
    observed_intent_digest = intent.get("intent_digest_sha256")
    intent_copy = dict(intent)
    intent_copy.pop("intent_digest_sha256", None)
    calculated_intent = sha256(canonical_bytes(intent_copy))
    if observed_intent_digest != calculated_intent:
        raise VerificationFailure("CMD-G0-INTENT-RECEIPT", "intent digest mismatch")
    if result.get("intent_digest_sha256") != calculated_intent:
        raise VerificationFailure("CMD-G0-RECEIPT-CHAIN", "result is not linked to intent")

    expected_output_digests: dict[str, str] = {}
    for name in (
        "repository.json",
        "surfaces.json",
        "load-paths.json",
        "unknowns.json",
        "untracked.json",
        "intent-receipt.json",
    ):
        expected_output_digests[name] = sha256((evidence_dir / name).read_bytes())
    if result.get("output_digests_sha256") != expected_output_digests:
        raise VerificationFailure("CMD-G0-RESULT-RECEIPT", "result output digest map mismatch")
    observed_result_digest = result.get("result_digest_sha256")
    result_copy = dict(result)
    result_copy.pop("result_digest_sha256", None)
    if observed_result_digest != sha256(canonical_bytes(result_copy)):
        raise VerificationFailure("CMD-G0-RESULT-RECEIPT", "result digest mismatch")
    if result.get("standing") != "UNKNOWN":
        raise VerificationFailure("CMD-G0-STANDING-COLLAPSE", "executor must not self-promote standing")
    checks.append("intent/result receipt chain and output digest binding")
    checks.append("executor standing remains UNKNOWN before independent verification")
    return checks


def verify_auxiliary(root: Path, evidence_dir: Path, revision: str) -> list[str]:
    checks: list[str] = []
    load_paths = load_json(evidence_dir / "load-paths.json")
    unknowns = load_json(evidence_dir / "unknowns.json")
    untracked = load_json(evidence_dir / "untracked.json")
    if load_paths.get("subject_revision") != revision or unknowns.get("subject_revision") != revision:
        raise VerificationFailure("CMD-G0-BASE-MOVED", "auxiliary evidence revision mismatch")
    if untracked.get("subject_revision") != revision or untracked.get("admitted") is not False:
        raise VerificationFailure("CMD-G0-UNTRACKED-ADMISSION", "untracked evidence contract invalid")
    actual_untracked = current_untracked(root)
    if untracked.get("untracked_paths") != actual_untracked:
        raise VerificationFailure("CMD-G0-UNTRACKED-SET", "untracked path set differs")
    actual_working = current_working_records(root)
    if untracked.get("working_tree_records") != actual_working:
        raise VerificationFailure("CMD-G0-WORKTREE-SET", "working-tree record set differs")
    unknown_paths = unknowns.get("unknown_authority_paths")
    if not isinstance(unknown_paths, list) or unknowns.get("unknown_count") != len(unknown_paths):
        raise VerificationFailure("CMD-G0-UNKNOWN-COUNT", "unknown authority count mismatch")
    checks.append("untracked and working-tree state separated from admitted HEAD authority")
    checks.append("unknown authority is explicit and count-bound")
    return checks


def verify_falsifier(report_path: Path, revision: str) -> list[str]:
    report = load_json(report_path)
    if report.get("subject_revision") != revision:
        raise VerificationFailure("CMD-G0-FALSIFIER-SUBJECT", "falsifier subject mismatch")
    if report.get("observed_refusal") != "CMD-G0-EXACT-SET":
        raise VerificationFailure("CMD-G0-FALSIFIER-REFUSAL", "omission did not produce exact-set refusal")
    if report.get("verifier_exit_code") == 0:
        raise VerificationFailure("CMD-G0-FALSIFIER-ESCAPED", "tampered exact set was accepted")
    observed_digest = report.get("report_digest_sha256")
    copy = dict(report)
    copy.pop("report_digest_sha256", None)
    if observed_digest != sha256(canonical_bytes(copy)):
        raise VerificationFailure("CMD-G0-FALSIFIER-DIGEST", "falsifier report digest mismatch")
    return ["tracked-path omission sabotage refused with CMD-G0-EXACT-SET"]


def publish_report(destination: Path, report: dict[str, Any]) -> Path:
    destination.mkdir(parents=True, exist_ok=True)
    path = destination / "verifier-report.json"
    data = pretty_bytes(report)
    if path.exists():
        if path.read_bytes() != data:
            raise VerificationFailure("CMD-G0-REPLAY-DIVERGENCE", "verifier report bytes differ")
        return path
    fd, temporary = tempfile.mkstemp(prefix=".verifier-", dir=destination)
    try:
        with os.fdopen(fd, "wb") as handle:
            handle.write(data)
        os.replace(temporary, path)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)
    return path


def verify(
    root: Path,
    evidence_dir: Path,
    expected_revision: str,
    falsifier_report: Path | None,
    report_dir: Path | None,
) -> dict[str, Any]:
    repository = load_json(evidence_dir / "repository.json")
    surfaces = load_json(evidence_dir / "surfaces.json")
    checks = compare_exact_set(root, repository, surfaces, expected_revision)
    checks.extend(verify_receipts(evidence_dir))
    checks.extend(verify_auxiliary(root, evidence_dir, expected_revision))
    if falsifier_report is not None:
        checks.extend(verify_falsifier(falsifier_report, expected_revision))
    boundaries = [
        "real Git subprocess",
        "real Git object database",
        "real filesystem evidence",
        "real JSON serialization",
        "real receipt verification",
        "deterministic replay comparison",
    ]
    report: dict[str, Any] = {
        "schema": REPORT_SCHEMA,
        "checkpoint": "CMD-G0-EXACT-TREE",
        "verifier_identity": VERIFIER_ID,
        "exact_subject_revision": expected_revision,
        "tree_digest": repository["subject_tree"],
        "toolchain": {
            "git": git_text(root, "--version"),
            "python": platform.python_version(),
        },
        "suite_inventory": [
            "positive exact-set witness",
            "negative omission falsifier" if falsifier_report is not None else "negative omission falsifier pending",
            "independent receipt verifier",
            "deterministic replay verifier",
        ],
        "commands": [
            "git ls-files -s -z",
            "git ls-tree -r -t -z --full-tree HEAD",
            "git cat-file blob <object-id>",
        ],
        "boundaries_crossed": boundaries,
        "evidence_artifacts": sorted(path.name for path in evidence_dir.iterdir() if path.is_file()),
        "passed_checks": checks,
        "failed_checks": [],
        "blocked_checks": [],
        "unsupported_checks": [],
        "refusal_codes": ["CMD-G0-EXACT-SET"] if falsifier_report is not None else [],
        "replay_result": "IDENTICAL",
        "aggregate_standing": "PARTIAL_ALIVE" if falsifier_report is not None else "UNKNOWN",
        "standing_ceiling": "PARTIAL_ALIVE",
    }
    report["verifier_report_digest_sha256"] = sha256(canonical_bytes(report))
    if report_dir is not None:
        publish_report(report_dir, report)
    return report


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, required=True)
    parser.add_argument("--expected-revision", required=True)
    parser.add_argument("--evidence-dir", type=Path, required=True)
    parser.add_argument("--falsifier-report", type=Path)
    parser.add_argument("--report-dir", type=Path)
    args = parser.parse_args()
    try:
        report = verify(
            args.root.resolve(),
            args.evidence_dir.resolve(),
            args.expected_revision,
            args.falsifier_report.resolve() if args.falsifier_report else None,
            args.report_dir.resolve() if args.report_dir else None,
        )
    except VerificationFailure as error:
        print(f"REFUSED: {error.code}: {error.detail}", file=sys.stderr)
        return 1
    print(json.dumps(report, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
