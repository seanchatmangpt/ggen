#!/usr/bin/env python3
"""Independent, non-actuating R47 consumer qualification court."""
from __future__ import annotations
import argparse, hashlib, json, pathlib, subprocess, sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
CONTRACT = ROOT / "verification/r47-consumer/consumer.json"

def git(*args: str) -> str:
    return subprocess.check_output(["git", *args], cwd=ROOT, text=True).strip()

def refuse(code: str, detail: str) -> int:
    print(f"REFUSED[{code}] {detail}", file=sys.stderr)
    return 2

def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--repo", required=True)
    p.add_argument("--candidate-sha", required=True)
    p.add_argument("--producer-fixture")
    p.add_argument("--receipt", default="")
    a = p.parse_args()
    c = json.loads(CONTRACT.read_text())
    if c["authority"] != "VERIFY_ONLY" or c["consequential_do"] is not False:
        return refuse("AUTHORITY", "consumer court must be verify-only")
    if a.repo != c["consumer_repo"]:
        return refuse("REPO_IDENTITY", f"expected {c['consumer_repo']} got {a.repo}")
    actual = git("rev-parse", "HEAD")
    if actual != a.candidate_sha:
        return refuse("EXACT_SUBJECT", f"HEAD={actual} candidate={a.candidate_sha}")
    if subprocess.run(["git", "merge-base", "--is-ancestor", c["admitted_target_base"], actual], cwd=ROOT).returncode != 0:
        return refuse("LINEAGE", "candidate does not descend from admitted target base")
    fixture_digest = None
    if a.producer_fixture:
        data = pathlib.Path(a.producer_fixture).read_bytes()
        fixture_digest = hashlib.sha256(data).hexdigest()
        text = data.decode()
        if c["admitted_target_base"] not in text or "esf:ggenTarget" not in text:
            return refuse("PRODUCER_CORRESPONDENCE", "producer fixture does not bind admitted ggen target")
        if 'esf:eligible true' not in text:
            return refuse("PRODUCER_ELIGIBILITY", "target is not eligible")
    receipt = {
        "schema_version": 1,
        "standing": "ALIVE",
        "consumer_repo": c["consumer_repo"],
        "candidate_sha": actual,
        "admitted_target_base": c["admitted_target_base"],
        "producer_repo": c["producer_repo"],
        "producer_sha": c["producer_sha"],
        "producer_fixture": c["producer_fixture"],
        "producer_fixture_sha256": fixture_digest,
        "authority": "VERIFY_ONLY",
        "consequential_do": False,
        "replay": f"python3 verification/r47-consumer/verify.py --repo {c['consumer_repo']} --candidate-sha {actual}",
    }
    encoded = json.dumps(receipt, sort_keys=True, indent=2) + "\n"
    if a.receipt:
        pathlib.Path(a.receipt).write_text(encoded)
    print(encoded, end="")
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
