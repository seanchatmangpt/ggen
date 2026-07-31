#!/usr/bin/env python3
"""Exact Git-object observation and atomic G0 evidence publication."""
from __future__ import annotations

import hashlib
import json
import os
import platform
import subprocess
import tempfile
from pathlib import Path, PurePosixPath
from typing import Any

SCHEMA = "ggen.cmd.exact-tree.v1"
SURFACES_SCHEMA = "ggen.cmd.surfaces.v1"
LOAD_PATHS_SCHEMA = "ggen.cmd.load-paths.v1"
UNKNOWNS_SCHEMA = "ggen.cmd.unknowns.v1"
UNTRACKED_SCHEMA = "ggen.cmd.untracked.v1"
INTENT_SCHEMA = "ggen.cmd.observation-intent.v1"
RESULT_SCHEMA = "ggen.cmd.observation-result.v1"
OBSERVER_ID = "ggen-self-host/cmd-g0-exact-tree-v1"
EVIDENCE_PREFIXES = ("self-host/observed/cmd-g0/", "self-host/evidence/cmd-g0/")
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
AUTHORITY_MAP = {
    "AuthoredConstitution": "authored constitution",
    "BootstrapKernel": "implementation",
    "ExecutableSource": "implementation",
    "GeneratedConsequence": "generated consequence",
    "Template": "template",
    "TestFixture": "fixture",
    "VerificationEvidence": "evidence",
    "Workflow": "workflow",
    "Configuration": "configuration",
    "Documentation": "documentation",
    "Archive": "archive",
    "Asset": "asset",
    "UnknownAuthority": "unknown",
}


class Refusal(RuntimeError):
    """Typed fail-closed outcome."""

    def __init__(self, code: str, detail: str) -> None:
        super().__init__(detail)
        self.code = code
        self.detail = detail


def git(root: Path, *args: str, check: bool = True) -> bytes:
    result = subprocess.run(
        ["git", "-C", str(root), *args],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if check and result.returncode != 0:
        raise Refusal(
            "CMD-G0-GIT-BOUNDARY",
            f"git {' '.join(args)} failed: {result.stderr.decode('utf-8', errors='replace').strip()}",
        )
    return result.stdout


def text(root: Path, *args: str) -> str:
    return git(root, *args).decode("utf-8", errors="strict").strip()


def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def canonical_bytes(value: Any) -> bytes:
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode("utf-8")


def pretty_bytes(value: Any) -> bytes:
    return (json.dumps(value, sort_keys=True, indent=2, ensure_ascii=True) + "\n").encode("utf-8")


def path_text(raw: bytes) -> str:
    return raw.decode("utf-8", errors="surrogateescape")


def parse_tree(root: Path) -> list[dict[str, str]]:
    raw = git(root, "ls-tree", "-r", "-t", "-z", "--full-tree", "HEAD")
    entries: list[dict[str, str]] = []
    for record in raw.split(b"\0"):
        if not record:
            continue
        metadata, raw_path = record.split(b"\t", 1)
        mode, object_type, object_id = metadata.decode("ascii").split()
        entries.append(
            {
                "path": path_text(raw_path),
                "mode": mode,
                "object_type": object_type,
                "object_id": object_id,
            }
        )
    return sorted(entries, key=lambda item: item["path"])


def parse_index(root: Path) -> list[dict[str, str | int]]:
    raw = git(root, "ls-files", "-s", "-z")
    entries: list[dict[str, str | int]] = []
    for record in raw.split(b"\0"):
        if not record:
            continue
        metadata, raw_path = record.split(b"\t", 1)
        mode, object_id, stage = metadata.decode("ascii").split()
        entries.append(
            {
                "path": path_text(raw_path),
                "mode": mode,
                "object_id": object_id,
                "stage": int(stage),
            }
        )
    return sorted(entries, key=lambda item: (str(item["path"]), int(item["stage"])))


def ignored_evidence_path(path: str) -> bool:
    return any(path == prefix.rstrip("/") or path.startswith(prefix) for prefix in EVIDENCE_PREFIXES)


def untracked_paths(root: Path) -> list[str]:
    raw = git(root, "ls-files", "--others", "--exclude-standard", "-z")
    return sorted(
        path
        for path in (path_text(item) for item in raw.split(b"\0") if item)
        if not ignored_evidence_path(path)
    )


def working_tree_records(root: Path) -> list[str]:
    raw = git(root, "status", "--porcelain=v2", "-z", "--untracked-files=all")
    records = [path_text(item) for item in raw.split(b"\0") if item]
    return sorted(record for record in records if not any(prefix in record for prefix in EVIDENCE_PREFIXES))


def load_classifications(source: Path | None) -> tuple[dict[str, dict[str, Any]], list[dict[str, Any]]]:
    if source is None or not source.is_file():
        return {}, []
    try:
        document = json.loads(source.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise Refusal("CMD-G0-CLASSIFICATION-SOURCE", f"cannot read {source}: {error}") from error
    mapping = {
        str(item.get("path")): item
        for item in document.get("files", [])
        if isinstance(item, dict) and item.get("path")
    }
    load_paths = [item for item in document.get("load_paths", []) if isinstance(item, dict)]
    return mapping, load_paths


def fallback_classification(path: str) -> str:
    p = PurePosixPath(path)
    suffix = p.suffix.lower()
    parts = set(p.parts)
    if {"archive", "archive_2025", "legacy_structure"} & parts:
        return "archive"
    if path.startswith(".github/workflows/"):
        return "workflow"
    if "/templates/" in path or suffix in {".tmpl", ".tera", ".j2", ".jinja"}:
        return "template"
    if "/tests/" in path or path.startswith("tests/"):
        return "fixture"
    if suffix in {".ttl", ".rq", ".shacl"}:
        return "domain source"
    if suffix in {".rs", ".py", ".sh", ".js", ".ts", ".tsx", ".go", ".java", ".kt"}:
        return "implementation"
    if suffix in {".toml", ".yaml", ".yml", ".json", ".lock", ".ini"} or p.name in {
        "justfile",
        "Makefile",
        "Makefile.toml",
    }:
        return "configuration"
    if suffix == ".md" or path.startswith(("docs/", "book/")):
        return "documentation"
    return "asset"


def content_semantics(root: Path, entry: dict[str, str]) -> tuple[str, str, int]:
    mode = entry["mode"]
    object_type = entry["object_type"]
    object_id = entry["object_id"]
    if object_type == "blob":
        data = git(root, "cat-file", "blob", object_id)
        semantic = "symlink-target-bytes" if mode == "120000" else "blob-bytes"
        return semantic, sha256(data), len(data)
    if mode == "160000" and object_type == "commit":
        data = object_id.encode("ascii")
        return "gitlink-commit-identity", sha256(data), len(data)
    if object_type == "tree":
        data = object_id.encode("ascii")
        return "tree-object-identity", sha256(data), len(data)
    raise Refusal(
        "CMD-G0-OBJECT-TYPE",
        f"unsupported Git object mode/type {mode}/{object_type} at {entry['path']}",
    )


def assert_base_ancestor(root: Path, base_sha: str | None) -> None:
    if not base_sha:
        return
    result = subprocess.run(
        ["git", "-C", str(root), "merge-base", "--is-ancestor", base_sha, "HEAD"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if result.returncode != 0:
        raise Refusal("CMD-G0-BASE-MOVED", f"base {base_sha} is not an ancestor of HEAD")


def work_order(root: Path, source: Path | None) -> tuple[dict[str, Any] | None, str | None]:
    if source is None or not source.is_file():
        return None, None
    raw = source.read_bytes()
    try:
        value = json.loads(raw)
    except json.JSONDecodeError as error:
        raise Refusal("CMD-G0-WORK-ORDER", f"invalid work order {source}: {error}") from error
    base_sha = value.get("base_sha")
    if base_sha:
        assert_base_ancestor(root, str(base_sha))
    return value, sha256(raw)


def toolchain_identity(root: Path) -> dict[str, str]:
    return {
        "git": text(root, "--version"),
        "python": platform.python_version(),
        "observer": OBSERVER_ID,
    }


def remote_identity(root: Path) -> str | None:
    result = subprocess.run(
        ["git", "-C", str(root), "config", "--get", "remote.origin.url"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if result.returncode != 0:
        return None
    value = result.stdout.decode("utf-8", errors="strict").strip()
    return value or None


def build_documents(
    root: Path,
    expected_revision: str,
    classification_source: Path | None = None,
    work_order_source: Path | None = None,
) -> dict[str, bytes]:
    root = root.resolve()
    revision = text(root, "rev-parse", "HEAD")
    if revision != expected_revision:
        raise Refusal(
            "CMD-G0-BASE-MOVED",
            f"expected revision {expected_revision}, observed {revision}",
        )
    order, order_digest = work_order(root, work_order_source)
    tree_id = text(root, "rev-parse", "HEAD^{tree}")
    commit_time = text(root, "show", "-s", "--format=%cI", "HEAD")
    objects = parse_tree(root)
    index = parse_index(root)
    if any(int(item["stage"]) != 0 for item in index):
        raise Refusal("CMD-G0-INDEX-STAGE", "non-zero index stages are not admissible")
    index_by_path = {str(item["path"]): item for item in index}
    classifications, load_paths = load_classifications(classification_source)

    surfaces: list[dict[str, Any]] = []
    for entry in objects:
        if entry["object_type"] == "tree":
            continue
        indexed = index_by_path.get(entry["path"])
        if indexed is None:
            raise Refusal("CMD-G0-EXACT-SET", f"HEAD path missing from index: {entry['path']}")
        if indexed["mode"] != entry["mode"] or indexed["object_id"] != entry["object_id"]:
            raise Refusal("CMD-G0-INDEX-DIVERGENCE", f"index differs from HEAD at {entry['path']}")
        existing = classifications.get(entry["path"], {})
        authority = str(existing.get("authority_class", ""))
        classification = AUTHORITY_MAP.get(authority, fallback_classification(entry["path"]))
        if classification not in ALLOWED_CLASSIFICATIONS:
            classification = "unknown"
        semantics, digest, size = content_semantics(root, entry)
        surfaces.append(
            {
                **entry,
                "index_mode": str(indexed["mode"]),
                "index_object_id": str(indexed["object_id"]),
                "classification": classification,
                "authority_class": authority or None,
                "content_semantics": semantics,
                "content_digest_sha256": digest,
                "size_bytes": size,
            }
        )
    surfaces.sort(key=lambda item: item["path"])
    object_digest = sha256(canonical_bytes(objects))
    surfaces_digest = sha256(canonical_bytes(surfaces))
    unknown_paths = sorted(item["path"] for item in surfaces if item["classification"] == "unknown")
    untracked = untracked_paths(root)
    working = working_tree_records(root)

    repository: dict[str, Any] = {
        "schema": SCHEMA,
        "observer": OBSERVER_ID,
        "subject_revision": revision,
        "subject_tree": tree_id,
        "base_sha": order.get("base_sha") if order else None,
        "work_order_digest_sha256": order_digest,
        "observation_time": commit_time,
        "observation_time_basis": "subject-commit",
        "repository_remote": remote_identity(root),
        "toolchain": toolchain_identity(root),
        "object_count": len(objects),
        "surface_count": len(surfaces),
        "objects_digest_sha256": object_digest,
        "surfaces_digest_sha256": surfaces_digest,
        "objects": objects,
    }
    repository["contract_digest_sha256"] = sha256(canonical_bytes(repository))

    surfaces_document = {
        "schema": SURFACES_SCHEMA,
        "subject_revision": revision,
        "subject_tree": tree_id,
        "surfaces_digest_sha256": surfaces_digest,
        "surfaces": surfaces,
    }
    load_paths_document = {
        "schema": LOAD_PATHS_SCHEMA,
        "subject_revision": revision,
        "load_paths": sorted(load_paths, key=lambda item: canonical_bytes(item)),
    }
    unknowns_document = {
        "schema": UNKNOWNS_SCHEMA,
        "subject_revision": revision,
        "unknown_authority_paths": unknown_paths,
        "unknown_count": len(unknown_paths),
    }
    untracked_document = {
        "schema": UNTRACKED_SCHEMA,
        "subject_revision": revision,
        "admitted": False,
        "untracked_paths": untracked,
        "working_tree_records": working,
    }
    intent: dict[str, Any] = {
        "schema": INTENT_SCHEMA,
        "subject_revision": revision,
        "subject_tree": tree_id,
        "operation": "observe-exact-git-tree",
        "exact_subject": revision,
        "authority_grant": {
            "scope": "self-host/observed/cmd-g0/<revision>",
            "external_actuation": False,
            "repository_source_mutation": False,
        },
        "planned_outputs": [
            "repository.json",
            "surfaces.json",
            "load-paths.json",
            "unknowns.json",
            "untracked.json",
            "intent-receipt.json",
            "result-receipt.json",
        ],
        "work_order_digest_sha256": order_digest,
    }
    intent["intent_digest_sha256"] = sha256(canonical_bytes(intent))

    documents: dict[str, bytes] = {
        "repository.json": pretty_bytes(repository),
        "surfaces.json": pretty_bytes(surfaces_document),
        "load-paths.json": pretty_bytes(load_paths_document),
        "unknowns.json": pretty_bytes(unknowns_document),
        "untracked.json": pretty_bytes(untracked_document),
        "intent-receipt.json": pretty_bytes(intent),
    }
    output_digests = {name: sha256(data) for name, data in sorted(documents.items())}
    result: dict[str, Any] = {
        "schema": RESULT_SCHEMA,
        "subject_revision": revision,
        "subject_tree": tree_id,
        "operation": "observe-exact-git-tree",
        "intent_digest_sha256": intent["intent_digest_sha256"],
        "observed_consequence": {
            "exact_object_count": len(objects),
            "exact_surface_count": len(surfaces),
            "unknown_authority_count": len(unknown_paths),
            "untracked_count": len(untracked),
            "external_actuation": False,
            "repository_source_mutation": False,
        },
        "output_digests_sha256": output_digests,
        "standing": "UNKNOWN",
        "standing_ceiling": "PARTIAL_ALIVE",
    }
    result["result_digest_sha256"] = sha256(canonical_bytes(result))
    documents["result-receipt.json"] = pretty_bytes(result)

    if text(root, "rev-parse", "HEAD") != expected_revision:
        raise Refusal("CMD-G0-BASE-MOVED", "HEAD changed during exact-tree observation")
    return documents


def compare_existing(destination: Path, documents: dict[str, bytes]) -> None:
    actual_names = sorted(path.name for path in destination.iterdir() if path.is_file())
    expected_names = sorted(documents)
    if actual_names != expected_names:
        raise Refusal(
            "CMD-G0-REPLAY-DIVERGENCE",
            f"evidence file set differs: actual={actual_names} expected={expected_names}",
        )
    for name, expected in documents.items():
        actual = (destination / name).read_bytes()
        if actual != expected:
            raise Refusal("CMD-G0-REPLAY-DIVERGENCE", f"evidence bytes differ: {name}")


def publish_atomic(destination: Path, documents: dict[str, bytes]) -> Path:
    destination = destination.resolve()
    destination.parent.mkdir(parents=True, exist_ok=True)
    if destination.exists():
        compare_existing(destination, documents)
        return destination
    with tempfile.TemporaryDirectory(prefix=".cmd-g0-", dir=destination.parent) as temporary:
        staging = Path(temporary) / "evidence"
        staging.mkdir()
        for name, data in documents.items():
            (staging / name).write_bytes(data)
        os.replace(staging, destination)
    return destination


def default_evidence_dir(root: Path, revision: str) -> Path:
    return root / "self-host" / "observed" / "cmd-g0" / revision
