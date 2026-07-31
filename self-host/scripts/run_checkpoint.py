#!/usr/bin/env python3
"""Execute CMD G0 through the existing self-host observer and independent verifier."""
from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from pathlib import Path


def git_revision(root: Path) -> str:
    result = subprocess.run(
        ["git", "-C", str(root), "rev-parse", "HEAD"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=True,
    )
    return result.stdout.decode("utf-8", errors="strict").strip()


def run(command: list[str], root: Path) -> None:
    subprocess.run(command, cwd=root, check=True)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--expected-revision")
    args = parser.parse_args()
    root = args.root.resolve()
    expected = args.expected_revision or os.environ.get("GITHUB_SHA")
    if not expected:
        print(
            "REFUSED: CMD-G0-EXPECTED-REVISION: supply --expected-revision or GITHUB_SHA",
            file=sys.stderr,
        )
        return 1
    observed_revision = git_revision(root)
    if observed_revision != expected:
        print(
            f"REFUSED: CMD-G0-BASE-MOVED: expected {expected}, observed {observed_revision}",
            file=sys.stderr,
        )
        return 1

    scripts = root / "self-host" / "scripts"
    observed = root / "self-host" / "observed" / "cmd-g0" / expected
    report_dir = root / "self-host" / "evidence" / "cmd-g0" / expected
    falsifier_report = report_dir / "falsifier.json"
    work_order = root / "self-host" / "contracts" / "cmd-g0-exact-tree.work-order.json"

    run([sys.executable, str(scripts / "observe_self_host.py"), "--root", str(root)], root)
    run([sys.executable, str(scripts / "verify_observation.py"), "--root", str(root)], root)
    run(
        [
            sys.executable,
            str(scripts / "observe_cmd_g0.py"),
            "--root",
            str(root),
            "--expected-revision",
            expected,
            "--work-order",
            str(work_order),
        ],
        root,
    )
    run(
        [
            sys.executable,
            str(scripts / "verify_cmd_g0.py"),
            "--root",
            str(root),
            "--expected-revision",
            expected,
            "--evidence-dir",
            str(observed),
        ],
        root,
    )
    run(
        [
            sys.executable,
            str(scripts / "run_cmd_g0_exact_set_falsifier.py"),
            "--root",
            str(root),
            "--expected-revision",
            expected,
            "--evidence-dir",
            str(observed),
            "--report-path",
            str(falsifier_report),
        ],
        root,
    )
    run(
        [
            sys.executable,
            str(scripts / "verify_cmd_g0.py"),
            "--root",
            str(root),
            "--expected-revision",
            expected,
            "--evidence-dir",
            str(observed),
            "--falsifier-report",
            str(falsifier_report),
            "--report-dir",
            str(report_dir),
        ],
        root,
    )
    if git_revision(root) != expected:
        print("REFUSED: CMD-G0-BASE-MOVED: HEAD changed during checkpoint", file=sys.stderr)
        return 1

    print(
        json.dumps(
            {
                "checkpoint": "CMD-G0-EXACT-TREE",
                "subject_revision": expected,
                "observation": str(observed),
                "verifier_report": str(report_dir / "verifier-report.json"),
                "standing": "PARTIAL_ALIVE",
                "production_behavior_changed": False,
            },
            sort_keys=True,
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
