#!/usr/bin/env python3
"""Independently verify the ggen self-host observation against Git HEAD."""
from __future__ import annotations

import argparse
import hashlib
import io
import json
import subprocess
import sys
import tarfile
from pathlib import Path
from typing import Any


def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def git(root: Path, *args: str) -> bytes:
    return subprocess.run(
        ["git", "-C", str(root), *args],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    ).stdout


def index_modes(root: Path) -> dict[str, str]:
    raw = git(root, "ls-files", "-s", "-z")
    modes: dict[str, str] = {}
    for record in raw.split(b"\0"):
        if not record:
            continue
        metadata, path_raw = record.split(b"\t", 1)
        mode, _blob, _stage = metadata.decode("ascii").split()
        modes[path_raw.decode("utf-8")] = mode
    return modes


def head_snapshot(root: Path) -> dict[str, bytes]:
    """Return tracked blob semantics without following host symlink targets."""
    archive = git(root, "archive", "--format=tar", "HEAD")
    snapshot: dict[str, bytes] = {}
    with tarfile.open(fileobj=io.BytesIO(archive), mode="r:") as stream:
        for member in stream.getmembers():
            if member.isfile():
                handle = stream.extractfile(member)
                if handle is None:
                    raise ValueError(f"archive member unreadable: {member.name}")
                snapshot[member.name] = handle.read()
            elif member.issym():
                snapshot[member.name] = member.linkname.encode("utf-8")
    for path, mode in index_modes(root).items():
        if mode == "160000":
            snapshot[path] = b""
    return snapshot


def verify(root: Path) -> list[str]:
    errors: list[str] = []
    observation_path = root / "self-host" / "observed" / "repository.json"
    receipt_path = root / "self-host" / "observed" / "observation-receipt.json"
    ontology_path = root / "self-host" / "ontology.ttl"
    try:
        observation: dict[str, Any] = json.loads(observation_path.read_text(encoding="utf-8"))
        receipt: dict[str, Any] = json.loads(receipt_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        return [f"observation unreadable: {error}"]

    revision = git(root, "rev-parse", "HEAD").decode().strip()
    if observation.get("revision") != revision:
        errors.append(f"revision mismatch: observed={observation.get('revision')} head={revision}")
    if receipt.get("revision") != revision:
        errors.append(f"receipt revision mismatch: observed={receipt.get('revision')} head={revision}")
    if receipt.get("actuation_performed") is not False:
        errors.append("observer receipt must state actuation_performed=false")

    snapshot = head_snapshot(root)
    observed_files = observation.get("files", [])
    observed_by_path = {item.get("path"): item for item in observed_files if isinstance(item, dict)}
    if set(observed_by_path) != set(snapshot):
        missing = sorted(set(snapshot) - set(observed_by_path))
        extra = sorted(set(observed_by_path) - set(snapshot))
        errors.append(f"path-set mismatch: missing={missing[:20]} extra={extra[:20]}")
    for path, data in snapshot.items():
        item = observed_by_path.get(path)
        if item is None:
            continue
        digest = sha256(data)
        if item.get("digest") != digest:
            errors.append(f"digest mismatch: {path}")
        if item.get("size_bytes") != len(data):
            errors.append(f"size mismatch: {path}")
        if not item.get("authority_class"):
            errors.append(f"missing authority class: {path}")

    counts = observation.get("counts", {})
    if counts.get("files") != len(snapshot):
        errors.append(f"file count mismatch: observed={counts.get('files')} actual={len(snapshot)}")
    copy = dict(observation)
    observed_digest = copy.pop("observation_digest", None)
    canonical = json.dumps(copy, sort_keys=True, separators=(",", ":")).encode("utf-8")
    calculated_digest = sha256(canonical)
    if observed_digest != calculated_digest:
        errors.append("observation digest mismatch")
    if receipt.get("observation_digest") != calculated_digest:
        errors.append("receipt observation digest mismatch")

    expected_outputs = {
        "self-host/ontology.ttl": sha256(ontology_path.read_bytes()),
        "self-host/observed/repository.json": sha256(observation_path.read_bytes()),
    }
    if receipt.get("outputs") != expected_outputs:
        errors.append(f"receipt output map mismatch: observed={receipt.get('outputs')} expected={expected_outputs}")
    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[2])
    args = parser.parse_args()
    errors = verify(args.root.resolve())
    if errors:
        for error in errors:
            print(f"GGEN_SELF_HOST_VERIFY_ERROR: {error}", file=sys.stderr)
        return 1
    print("GGEN_SELF_HOST_OBSERVATION_VERIFIED")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
