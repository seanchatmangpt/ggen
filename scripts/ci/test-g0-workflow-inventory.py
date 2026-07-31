#!/usr/bin/env python3
"""Execute the G0 positive and refusal fixtures through real subprocesses."""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
VERIFIER = ROOT / "scripts" / "ci" / "verify-g0-workflow-inventory.py"
FIXTURES = ROOT / "tests" / "fixtures" / "ci-g0"


def execute(fixture: str) -> subprocess.CompletedProcess[str]:
    fixture_root = FIXTURES / fixture
    with tempfile.TemporaryDirectory(prefix=f"ggen-ci-g0-{fixture}-") as report_dir:
        return subprocess.run(
            [
                sys.executable,
                str(VERIFIER),
                "--repo-root",
                str(fixture_root),
                "--manifest",
                str(fixture_root / "manifest.toml"),
                "--report-dir",
                report_dir,
            ],
            check=False,
            capture_output=True,
            text=True,
        )


def require(condition: bool, message: str) -> None:
    if not condition:
        raise SystemExit(message)


def main() -> int:
    accepted = execute("accepted")
    require(accepted.returncode == 0, f"accepted fixture refused:\n{accepted.stderr}")
    accepted_summary = json.loads(accepted.stdout.strip().splitlines()[-1])
    require(accepted_summary["standing"] == "PARTIAL_ALIVE", "accepted fixture lacked PARTIAL_ALIVE")
    require(accepted_summary["workflows"] == 2, "accepted fixture admitted the wrong workflow count")

    omitted = execute("omitted-workflow")
    require(omitted.returncode != 0, "omitted-workflow fixture was incorrectly admitted")
    require("CI-G0-INVENTORY-001" in omitted.stderr, "omitted-workflow refusal code absent")

    duplicate = execute("duplicate-output-owner")
    require(duplicate.returncode != 0, "duplicate-output-owner fixture was incorrectly admitted")
    require("CI-G0-OWNERSHIP-001" in duplicate.stderr, "duplicate-owner refusal code absent")

    print(
        json.dumps(
            {
                "accepted": "PARTIAL_ALIVE",
                "omitted_workflow": "REFUSED:CI-G0-INVENTORY-001",
                "duplicate_output_owner": "REFUSED:CI-G0-OWNERSHIP-001",
            },
            sort_keys=True,
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
