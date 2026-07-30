#!/usr/bin/env python3
"""Pass only when a tampered normalized observation is independently refused."""
from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPTS = ROOT / "self-host" / "scripts"
OBSERVATION = ROOT / "self-host" / "observed" / "repository.json"


def main() -> int:
    subprocess.run(
        [sys.executable, str(SCRIPTS / "observe_self_host.py"), "--root", str(ROOT)],
        cwd=ROOT,
        check=True,
    )
    original = OBSERVATION.read_bytes()
    try:
        value = json.loads(original)
        value["observation_digest"] = "0" * 64
        OBSERVATION.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")
        result = subprocess.run(
            [sys.executable, str(SCRIPTS / "verify_observation.py"), "--root", str(ROOT)],
            cwd=ROOT,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
        )
        if result.returncode == 0:
            print("GGEN_SELF_HOST_FALSIFIER_ESCAPED: tampered digest was accepted", file=sys.stderr)
            return 1
        if b"observation digest mismatch" not in result.stderr:
            print(result.stderr.decode("utf-8", errors="replace"), file=sys.stderr)
            print("GGEN_SELF_HOST_FALSIFIER_WRONG_REFUSAL", file=sys.stderr)
            return 1
        return 0
    finally:
        OBSERVATION.write_bytes(original)


if __name__ == "__main__":
    raise SystemExit(main())
