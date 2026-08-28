#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path


LINUX_ASSETS = (
    "ggen-x86_64-unknown-linux-gnu.tar.gz",
    "ggen-aarch64-unknown-linux-gnu.tar.gz",
)


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--tag", required=True)
    parser.add_argument("--release-commit", required=True)
    parser.add_argument("--release-dir", type=Path, required=True)
    parser.add_argument("--ggen-oci-digest")
    parser.add_argument("--bootstrap-oci-digest")
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()

    if len(args.release_commit) != 40 or any(c not in "0123456789abcdef" for c in args.release_commit):
        raise SystemExit("REFUSED:RELEASE_COMMIT_INVALID")

    assets = []
    for name in LINUX_ASSETS:
        path = args.release_dir / name
        if not path.is_file():
            raise SystemExit(f"REFUSED:RELEASE_ASSET_MISSING:{name}")
        sidecar = args.release_dir / f"{name}.sha256"
        if not sidecar.is_file():
            raise SystemExit(f"REFUSED:RELEASE_DIGEST_MISSING:{name}")
        expected = sidecar.read_text(encoding="utf-8").split()[0]
        actual = sha256(path)
        if expected != actual:
            raise SystemExit(f"REFUSED:RELEASE_DIGEST_DRIFT:{name}")
        assets.append({"name": name, "sha256": actual, "size_bytes": path.stat().st_size})

    payload = {
        "schema": "ggen.github-package/1",
        "repository": "seanchatmangpt/ggen",
        "release": {"tag": args.tag, "commit": args.release_commit},
        "assets": assets,
        "oci": {
            "ggen": (
                f"ghcr.io/seanchatmangpt/ggen@{args.ggen_oci_digest}"
                if args.ggen_oci_digest
                else None
            ),
            "bootstrap": (
                f"ghcr.io/seanchatmangpt/ggen-bootstrap@{args.bootstrap_oci_digest}"
                if args.bootstrap_oci_digest
                else None
            ),
        },
    }
    encoded = json.dumps(payload, sort_keys=True, separators=(",", ":")).encode()
    payload["manifest_sha256"] = hashlib.sha256(encoded).hexdigest()
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
