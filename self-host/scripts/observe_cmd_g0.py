#!/usr/bin/env python3
"""Publish the exact-tree G0 observation under the existing self-host authority."""
from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

from exact_tree_contract import (
    Refusal,
    build_documents,
    default_evidence_dir,
    publish_atomic,
)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, required=True)
    parser.add_argument("--expected-revision", required=True)
    parser.add_argument("--classification-source", type=Path)
    parser.add_argument("--work-order", type=Path)
    parser.add_argument("--evidence-root", type=Path)
    args = parser.parse_args()
    root = args.root.resolve()
    classification_source = args.classification_source
    if classification_source is None:
        candidate = root / "self-host" / "observed" / "repository.json"
        classification_source = candidate if candidate.is_file() else None
    work_order = args.work_order
    if work_order is None:
        candidate = root / "self-host" / "contracts" / "cmd-g0-exact-tree.work-order.json"
        work_order = candidate if candidate.is_file() else None
    destination = (
        args.evidence_root.resolve() / args.expected_revision
        if args.evidence_root
        else default_evidence_dir(root, args.expected_revision)
    )
    try:
        documents = build_documents(
            root,
            args.expected_revision,
            classification_source.resolve() if classification_source else None,
            work_order.resolve() if work_order else None,
        )
        published = publish_atomic(destination, documents)
    except Refusal as error:
        print(f"REFUSED: {error.code}: {error.detail}", file=sys.stderr)
        return 1
    print(
        json.dumps(
            {
                "checkpoint": "CMD-G0-EXACT-TREE",
                "evidence_dir": str(published),
                "subject_revision": args.expected_revision,
                "executor_standing": "UNKNOWN",
                "standing_ceiling": "PARTIAL_ALIVE",
            },
            sort_keys=True,
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
