#!/usr/bin/env python3
"""Falsifiers for the independent R47 consumer court."""
from __future__ import annotations
import json, pathlib, subprocess, sys, tempfile
ROOT = pathlib.Path(__file__).resolve().parents[2]
VERIFY = ROOT / "verification/r47-consumer/verify.py"

def run(*args: str):
    return subprocess.run([sys.executable, str(VERIFY), *args], cwd=ROOT, text=True, capture_output=True)

def main() -> int:
    head = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip()
    ok = run("--repo", "seanchatmangpt/ggen", "--candidate-sha", head)
    assert ok.returncode == 0, ok.stderr
    receipt = json.loads(ok.stdout)
    assert receipt["standing"] == "ALIVE"
    assert receipt["candidate_sha"] == head
    wrong_repo = run("--repo", "example/wrong", "--candidate-sha", head)
    assert wrong_repo.returncode == 2 and "REFUSED[REPO_IDENTITY]" in wrong_repo.stderr
    wrong_sha = run("--repo", "seanchatmangpt/ggen", "--candidate-sha", "0" * 40)
    assert wrong_sha.returncode == 2 and "REFUSED[EXACT_SUBJECT]" in wrong_sha.stderr
    with tempfile.NamedTemporaryFile("w", suffix=".ttl", delete=False) as f:
        f.write("@prefix esf: <https://ggen.dev/ontology/epistemic-sensor-factory#> .\n")
        bad = f.name
    mismatch = run("--repo", "seanchatmangpt/ggen", "--candidate-sha", head, "--producer-fixture", bad)
    pathlib.Path(bad).unlink(missing_ok=True)
    assert mismatch.returncode == 2 and "REFUSED[PRODUCER_CORRESPONDENCE]" in mismatch.stderr
    print("R47_CONSUMER_FALSIFIERS=4 ALIVE")
    return 0

if __name__ == "__main__": raise SystemExit(main())
