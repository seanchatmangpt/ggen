#!/usr/bin/env python3
"""Manufacture deterministic, non-actuating pull-request intents from admitted repo observations."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import shutil
import sys
import tempfile
from pathlib import Path, PurePosixPath
from typing import Any

SCHEMA = "ggen.eod-repo-observation.v1"
INTENT_SCHEMA = "ggen.eod-repo-pr-intent.v1"
MANIFEST_SCHEMA = "ggen.eod-repo-synthesis-manifest.v1"
REPO_RE = re.compile(r"^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
TOKEN_RE = re.compile(r"^[a-z0-9][a-z0-9._-]*$")
DAY_RE = re.compile(r"^\d{4}-\d{2}-\d{2}$")


class Refusal(RuntimeError):
    """A typed admission refusal."""

    def __init__(self, code: str, message: str) -> None:
        super().__init__(f"{code}: {message}")
        self.code = code
        self.message = message


def canonical_json(value: Any) -> bytes:
    return (json.dumps(value, indent=2, sort_keys=True, ensure_ascii=False) + "\n").encode("utf-8")


def sha256_bytes(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def require(condition: bool, code: str, message: str) -> None:
    if not condition:
        raise Refusal(code, message)


def require_string_list(value: Any, field: str, *, allow_empty: bool = True) -> list[str]:
    require(isinstance(value, list), "EOD-SYNTH-001", f"{field} must be an array")
    require(all(isinstance(item, str) and item.strip() for item in value), "EOD-SYNTH-001", f"{field} must contain non-empty strings")
    result = sorted(set(item.strip() for item in value))
    require(allow_empty or bool(result), "EOD-SYNTH-001", f"{field} must not be empty")
    return result


def validate_path(raw: Any, field: str) -> str:
    require(isinstance(raw, str) and raw, "EOD-SYNTH-001", f"{field} must be a non-empty string")
    path = PurePosixPath(raw)
    require(not path.is_absolute(), "EOD-SYNTH-006", f"{field} must be repository-relative: {raw}")
    require(".." not in path.parts and "." not in path.parts, "EOD-SYNTH-006", f"{field} contains traversal: {raw}")
    require("\\" not in raw and "\x00" not in raw, "EOD-SYNTH-006", f"{field} is not a canonical Git path: {raw}")
    return path.as_posix()


def contains_direct_actuation(value: Any) -> bool:
    if isinstance(value, dict):
        for key, child in value.items():
            if key == "direct_actuation" and child is not False:
                return True
            if contains_direct_actuation(child):
                return True
    elif isinstance(value, list):
        return any(contains_direct_actuation(item) for item in value)
    return False


def validate_observation(raw: Any) -> dict[str, Any]:
    require(isinstance(raw, dict), "EOD-SYNTH-001", "observation must be an object")
    require(raw.get("schema") == SCHEMA, "EOD-SYNTH-001", f"schema must be {SCHEMA}")
    require(isinstance(raw.get("day"), str) and DAY_RE.fullmatch(raw["day"]), "EOD-SYNTH-001", "day must use YYYY-MM-DD")
    require(isinstance(raw.get("timezone"), str) and raw["timezone"], "EOD-SYNTH-001", "timezone must be non-empty")
    require(not contains_direct_actuation(raw), "EOD-SYNTH-007", "direct actuation cannot enter the observation graph")

    repositories = raw.get("repositories")
    require(isinstance(repositories, list) and len(repositories) >= 2, "EOD-SYNTH-001", "at least two repositories are required")

    normalized: list[dict[str, Any]] = []
    seen_repositories: set[str] = set()
    seen_artifacts: set[tuple[str, str, str, str]] = set()

    for index, repository in enumerate(repositories):
        field = f"repositories[{index}]"
        require(isinstance(repository, dict), "EOD-SYNTH-001", f"{field} must be an object")
        name = repository.get("repository")
        require(isinstance(name, str) and REPO_RE.fullmatch(name), "EOD-SYNTH-001", f"{field}.repository must be owner/name")
        require(name not in seen_repositories, "EOD-SYNTH-002", f"duplicate repository: {name}")
        seen_repositories.add(name)

        base_sha = repository.get("base_sha")
        head_sha = repository.get("head_sha")
        require(isinstance(base_sha, str) and SHA_RE.fullmatch(base_sha), "EOD-SYNTH-003", f"invalid base_sha for {name}")
        require(isinstance(head_sha, str) and SHA_RE.fullmatch(head_sha), "EOD-SYNTH-003", f"invalid head_sha for {name}")
        accepts = require_string_list(repository.get("accepts"), f"{field}.accepts", allow_empty=False)
        require(all(TOKEN_RE.fullmatch(item) for item in accepts), "EOD-SYNTH-001", f"invalid capability token in {field}.accepts")
        constraints = require_string_list(repository.get("constraints", []), f"{field}.constraints")
        verification = require_string_list(repository.get("verification", []), f"{field}.verification")

        work = repository.get("work")
        require(isinstance(work, list) and work, "EOD-SYNTH-001", f"{field}.work must not be empty")
        normalized_work: list[dict[str, Any]] = []
        for work_index, item in enumerate(work):
            work_field = f"{field}.work[{work_index}]"
            require(isinstance(item, dict), "EOD-SYNTH-001", f"{work_field} must be an object")
            work_id = item.get("id")
            summary = item.get("summary")
            observed_from = item.get("observed_from")
            require(isinstance(work_id, str) and work_id.strip(), "EOD-SYNTH-001", f"{work_field}.id must be non-empty")
            require(isinstance(summary, str) and summary.strip(), "EOD-SYNTH-001", f"{work_field}.summary must be non-empty")
            require(isinstance(observed_from, str) and observed_from.startswith("https://github.com/"), "EOD-SYNTH-001", f"{work_field}.observed_from must be a GitHub URL")
            artifacts = item.get("artifacts")
            require(isinstance(artifacts, list) and artifacts, "EOD-SYNTH-001", f"{work_field}.artifacts must not be empty")
            normalized_artifacts: list[dict[str, Any]] = []
            for artifact_index, artifact in enumerate(artifacts):
                artifact_field = f"{work_field}.artifacts[{artifact_index}]"
                require(isinstance(artifact, dict), "EOD-SYNTH-001", f"{artifact_field} must be an object")
                path = validate_path(artifact.get("path"), f"{artifact_field}.path")
                blob_sha = artifact.get("blob_sha")
                require(isinstance(blob_sha, str) and SHA_RE.fullmatch(blob_sha), "EOD-SYNTH-003", f"invalid blob_sha at {artifact_field}")
                exports = require_string_list(artifact.get("exports"), f"{artifact_field}.exports", allow_empty=False)
                require(all(TOKEN_RE.fullmatch(export) for export in exports), "EOD-SYNTH-001", f"invalid capability token at {artifact_field}.exports")
                identity = (name, head_sha, path, blob_sha)
                require(identity not in seen_artifacts, "EOD-SYNTH-004", f"duplicate artifact identity: {name}@{head_sha}:{path}")
                seen_artifacts.add(identity)
                normalized_artifacts.append({"path": path, "blob_sha": blob_sha, "exports": exports})
            normalized_work.append(
                {
                    "id": work_id.strip(),
                    "summary": summary.strip(),
                    "observed_from": observed_from,
                    "artifacts": sorted(normalized_artifacts, key=lambda item: (item["path"], item["blob_sha"])),
                }
            )

        normalized.append(
            {
                "repository": name,
                "base_sha": base_sha,
                "head_sha": head_sha,
                "accepts": accepts,
                "constraints": constraints,
                "verification": verification,
                "work": sorted(normalized_work, key=lambda item: item["id"]),
            }
        )

    return {
        "schema": SCHEMA,
        "day": raw["day"],
        "timezone": raw["timezone"],
        "repositories": sorted(normalized, key=lambda item: item["repository"]),
    }


def safe_name(repository: str) -> str:
    return repository.replace("/", "--").lower()


def transfer_id(target: str, source: str, head_sha: str, path: str, blob_sha: str, capabilities: list[str]) -> str:
    payload = "\0".join([target, source, head_sha, path, blob_sha, *capabilities]).encode("utf-8")
    return f"xfer-{sha256_bytes(payload)[:20]}"


def make_markdown(intent: dict[str, Any]) -> str:
    lines = [
        "## Preserve",
        "",
        f"Target repository: `{intent['target']['repository']}`",
        f"Exact base: `{intent['target']['base_sha']}`",
        "",
        "This artifact is a pull-request intent. It carries no GitHub write authority and performs no direct actuation.",
        "",
        "## Cross-repository synthesis",
        "",
    ]
    if intent["disposition"] == "candidate":
        lines.extend(["| Source | Exact artifact | Capabilities |", "|---|---|---|"])
        for candidate in intent["candidates"]:
            capabilities = ", ".join(f"`{item}`" for item in candidate["capabilities"])
            lines.append(
                f"| `{candidate['source_repository']}` | `{candidate['path']}@{candidate['blob_sha']}` | {capabilities} |"
            )
    else:
        lines.append("No dependency-compatible cross-repository candidate was admitted. The target is skipped.")

    lines.extend(["", "## Target constraints", ""])
    if intent["target"]["constraints"]:
        lines.extend(f"- {item}" for item in intent["target"]["constraints"])
    else:
        lines.append("- No additional target constraint was observed.")

    lines.extend(["", "## Required verification", ""])
    if intent["target"]["verification"]:
        lines.extend(f"- `{item}`" for item in intent["target"]["verification"])
    else:
        lines.append("- Repository-native verification must be resolved before publication.")

    lines.extend(
        [
            "",
            "## Authority and standing",
            "",
            "- authority: `intent_only`",
            "- direct actuation: `false`",
            f"- standing: `{intent['standing']}`",
            "- BLAKE3 receipt: required before any branch, commit, or pull-request write",
            "- target execution and replay: not performed by this intent manufacturer",
            "",
        ]
    )
    return "\n".join(lines)


def build_intent(observation: dict[str, Any], target: dict[str, Any]) -> dict[str, Any]:
    candidates: list[dict[str, Any]] = []
    exclusions: list[dict[str, Any]] = []
    accepts = set(target["accepts"])

    for source in observation["repositories"]:
        for work in source["work"]:
            for artifact in work["artifacts"]:
                if source["repository"] == target["repository"]:
                    exclusions.append(
                        {
                            "source_repository": source["repository"],
                            "path": artifact["path"],
                            "blob_sha": artifact["blob_sha"],
                            "reason": "target_owned",
                        }
                    )
                    continue
                capabilities = sorted(accepts.intersection(artifact["exports"]))
                if not capabilities:
                    exclusions.append(
                        {
                            "source_repository": source["repository"],
                            "path": artifact["path"],
                            "blob_sha": artifact["blob_sha"],
                            "reason": "capability_not_admitted",
                        }
                    )
                    continue
                candidates.append(
                    {
                        "transfer_id": transfer_id(
                            target["repository"],
                            source["repository"],
                            source["head_sha"],
                            artifact["path"],
                            artifact["blob_sha"],
                            capabilities,
                        ),
                        "source_repository": source["repository"],
                        "source_head_sha": source["head_sha"],
                        "source_work_id": work["id"],
                        "source_summary": work["summary"],
                        "observed_from": work["observed_from"],
                        "path": artifact["path"],
                        "blob_sha": artifact["blob_sha"],
                        "capabilities": capabilities,
                    }
                )

    candidates.sort(key=lambda item: (item["source_repository"], item["path"], item["blob_sha"]))
    exclusions.sort(key=lambda item: (item["source_repository"], item["path"], item["blob_sha"], item["reason"]))
    disposition = "candidate" if candidates else "skip"
    title = f"chore(synthesis): apply {observation['day']} cross-repository patterns"
    branch = f"agent/eod-synthesis-{observation['day']}"
    source_set = [
        {
            "source_repository": item["source_repository"],
            "source_head_sha": item["source_head_sha"],
            "path": item["path"],
            "blob_sha": item["blob_sha"],
        }
        for item in candidates
    ]

    intent: dict[str, Any] = {
        "schema": INTENT_SCHEMA,
        "day": observation["day"],
        "timezone": observation["timezone"],
        "disposition": disposition,
        "authority": "intent_only",
        "direct_actuation": False,
        "standing": "UNKNOWN",
        "branch": branch,
        "title": title,
        "target": {
            "repository": target["repository"],
            "base_sha": target["base_sha"],
            "observed_head_sha": target["head_sha"],
            "accepts": target["accepts"],
            "constraints": target["constraints"],
            "verification": target["verification"],
        },
        "candidates": candidates,
        "exclusions": exclusions,
        "source_set_sha256": sha256_bytes(canonical_json(source_set)),
        "receipt_requirement": "ggen-receipt/v2-required-before-actuation",
        "claim_boundary": [
            "No sibling repository was fetched, edited, committed, pushed, or opened as a pull request.",
            "Candidate transfer requires target-repository inspection and dependency-closed implementation.",
            "Target build, integration, replay, and CI standing remain UNKNOWN.",
        ],
    }
    intent["body_markdown"] = make_markdown(intent)
    return intent


def write_bundle(observation: dict[str, Any], output_dir: Path) -> dict[str, Any]:
    require(not output_dir.exists(), "EOD-SYNTH-005", f"output directory already exists: {output_dir}")
    output_dir.parent.mkdir(parents=True, exist_ok=True)
    temporary = Path(tempfile.mkdtemp(prefix=f".{output_dir.name}.", dir=output_dir.parent))
    try:
        targets: list[dict[str, Any]] = []
        file_digests: list[dict[str, str]] = []
        for target in observation["repositories"]:
            intent = build_intent(observation, target)
            stem = safe_name(target["repository"])
            json_name = f"{stem}.intent.json"
            markdown_name = f"{stem}.intent.md"
            json_bytes = canonical_json(intent)
            markdown_bytes = intent["body_markdown"].encode("utf-8")
            (temporary / json_name).write_bytes(json_bytes)
            (temporary / markdown_name).write_bytes(markdown_bytes)
            file_digests.extend(
                [
                    {"path": json_name, "sha256": sha256_bytes(json_bytes)},
                    {"path": markdown_name, "sha256": sha256_bytes(markdown_bytes)},
                ]
            )
            targets.append(
                {
                    "repository": target["repository"],
                    "disposition": intent["disposition"],
                    "candidate_count": len(intent["candidates"]),
                    "intent_json": json_name,
                    "intent_markdown": markdown_name,
                }
            )

        file_digests.sort(key=lambda item: item["path"])
        bundle_subject = canonical_json(file_digests)
        manifest = {
            "schema": MANIFEST_SCHEMA,
            "day": observation["day"],
            "timezone": observation["timezone"],
            "authority": "intent_only",
            "direct_actuation": False,
            "standing": "UNKNOWN",
            "repository_count": len(observation["repositories"]),
            "candidate_target_count": sum(1 for item in targets if item["disposition"] == "candidate"),
            "skipped_target_count": sum(1 for item in targets if item["disposition"] == "skip"),
            "targets": targets,
            "files": file_digests,
            "bundle_sha256": sha256_bytes(bundle_subject),
            "receipt_requirement": "ggen-receipt/v2-required-before-actuation",
        }
        (temporary / "manifest.json").write_bytes(canonical_json(manifest))
        os.replace(temporary, output_dir)
        return manifest
    except Exception:
        shutil.rmtree(temporary, ignore_errors=True)
        raise


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--observation", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    try:
        raw = json.loads(args.observation.read_text(encoding="utf-8"))
        observation = validate_observation(raw)
        manifest = write_bundle(observation, args.output_dir)
    except (OSError, json.JSONDecodeError) as error:
        print(f"EOD-SYNTH-001: {error}", file=sys.stderr)
        return 2
    except Refusal as refusal:
        print(f"{refusal.code}: {refusal.message}", file=sys.stderr)
        return 2
    print(json.dumps(manifest, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
