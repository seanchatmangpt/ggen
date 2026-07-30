#!/usr/bin/env python3
"""Execute the useful self-host observation system and verify its receipt."""
from __future__ import annotations

import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPTS = ROOT / "self-host" / "scripts"


def run(script: str) -> None:
    subprocess.run([sys.executable, str(SCRIPTS / script), "--root", str(ROOT)], check=True, cwd=ROOT)


def main() -> int:
    run("observe_exact_tree.py")
    run("verify_observation.py")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
