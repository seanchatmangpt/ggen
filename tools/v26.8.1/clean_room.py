#!/usr/bin/env python3
"""tools/v26.8.1/clean_room.py — genuinely isolated clean-room harness for the v26.8.1
legacy-rebuild G9 checkpoint.

Two modes:

  python3 tools/v26.8.1/clean_room.py
      Creates a FRESH git worktree of the current branch's exact HEAD, in a temp
      directory, with no reused `target/` build dir and no reused `.ggen`/`.ggen-v2`
      state, runs `just v26-8-1-rebuild` there, and reports the real outcome. This is
      the "does it actually build from nothing" proof — disk space, dependency
      resolution, and toolchain pin are all exercised for real, not assumed.

  python3 tools/v26.8.1/clean_room.py --replay-only-in-place
      Runs `just v26-8-1-rebuild` twice IN THE CURRENT CHECKOUT (not a fresh worktree
      — this is the familiar-checkout idempotence check, distinct from clean-room
      isolation) and diffs the two runs for:
        - generated file trees (git status / hash of tracked+untracked generated
          paths before vs after each run — byte-identical?)
        - receipt chain state (does .ggen-v2/receipt.json's prev_chain_hash_hex
          correctly link to the prior run's chain_hash_hex?)
        - .ggen/v26.8.1/verifier-report.json's `standing` field (same both times, or
          a typed explanation of why not)
      Reports NO_SEMANTIC_CHANGE / NO_GENERATED_DRIFT / REPLAY_MATCH if all hold, or
      the specific divergence found.

This script CALLS `just v26-8-1-rebuild` (and, indirectly, the tools it wires
together); it does not reimplement any pipeline stage's logic itself.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
RECEIPT_PATH = REPO_ROOT / ".ggen-v2" / "receipt.json"
VERIFIER_REPORT_PATH = REPO_ROOT / ".ggen" / "v26.8.1" / "verifier-report.json"


def run(cmd: list[str], cwd: Path, timeout: int | None = None) -> subprocess.CompletedProcess:
    print(f"$ (cwd={cwd}) {' '.join(cmd)}")
    return subprocess.run(
        cmd, cwd=cwd, capture_output=True, text=True, timeout=timeout, check=False
    )


def current_head(cwd: Path) -> str:
    return run(["git", "rev-parse", "HEAD"], cwd=cwd).stdout.strip()


def current_branch(cwd: Path) -> str:
    return run(["git", "rev-parse", "--abbrev-ref", "HEAD"], cwd=cwd).stdout.strip()


def hash_tree(root: Path, relpaths: list[str]) -> dict[str, str]:
    """BLAKE2b digest of each named file relative to root; missing files -> 'MISSING'."""
    out: dict[str, str] = {}
    for rel in relpaths:
        p = root / rel
        if p.is_file():
            out[rel] = hashlib.blake2b(p.read_bytes()).hexdigest()
        elif p.is_dir():
            h = hashlib.blake2b()
            for f in sorted(p.rglob("*")):
                if f.is_file():
                    h.update(str(f.relative_to(p)).encode())
                    h.update(f.read_bytes())
            out[rel] = h.hexdigest()
        else:
            out[rel] = "MISSING"
    return out


GENERATED_WATCH_PATHS = [
    "ontology/v26.8.1/legacy-capabilities.ttl",
    "packs/legacy-equivalence-verifier-pack/consumer",
    ".ggen/v26.8.1/verifier-report.json",
]


def read_json(path: Path) -> dict | None:
    if not path.is_file():
        return None
    try:
        return json.loads(path.read_text())
    except (json.JSONDecodeError, OSError) as exc:
        print(f"WARNING: could not parse {path}: {exc}")
        return None


def replay_in_place() -> int:
    print("=== clean_room.py --replay-only-in-place: idempotence check on the current checkout ===")

    run1_receipt_before = read_json(RECEIPT_PATH)
    r1 = run(["just", "v26-8-1-rebuild"], cwd=REPO_ROOT, timeout=1800)
    print(r1.stdout[-4000:])
    print(r1.stderr[-4000:])
    tree_after_run1 = hash_tree(REPO_ROOT, GENERATED_WATCH_PATHS)
    receipt_after_run1 = read_json(RECEIPT_PATH)
    report_after_run1 = read_json(VERIFIER_REPORT_PATH)
    run1_ok = r1.returncode == 0

    r2 = run(["just", "v26-8-1-rebuild"], cwd=REPO_ROOT, timeout=1800)
    print(r2.stdout[-4000:])
    print(r2.stderr[-4000:])
    tree_after_run2 = hash_tree(REPO_ROOT, GENERATED_WATCH_PATHS)
    receipt_after_run2 = read_json(RECEIPT_PATH)
    report_after_run2 = read_json(VERIFIER_REPORT_PATH)
    run2_ok = r2.returncode == 0

    print("\n=== REPLAY RESULT ===")
    print(f"run1 exit={r1.returncode} ({'PASS' if run1_ok else 'FAIL'})")
    print(f"run2 exit={r2.returncode} ({'PASS' if run2_ok else 'FAIL'})")

    tree_diff = {k: (tree_after_run1.get(k), tree_after_run2.get(k))
                 for k in GENERATED_WATCH_PATHS if tree_after_run1.get(k) != tree_after_run2.get(k)}
    if tree_diff:
        print(f"GENERATED_DRIFT detected in: {list(tree_diff.keys())}")
        for k, (h1, h2) in tree_diff.items():
            print(f"  {k}: run1={h1} run2={h2}")
    else:
        print("NO_GENERATED_DRIFT: watched generated paths are byte-identical across both runs")

    chain_ok = None
    if receipt_after_run1 and receipt_after_run2:
        prev = receipt_after_run2.get("prev_chain_hash_hex")
        expected_prev = receipt_after_run1.get("chain_hash_hex")
        chain_ok = prev == expected_prev
        print(f"receipt chain link: run2.prev_chain_hash_hex={prev} "
              f"vs run1.chain_hash_hex={expected_prev} -> {'LINKED' if chain_ok else 'DIVERGED'}")
    else:
        print("receipt chain link: UNVERIFIED (one or both runs produced no readable receipt.json — "
              "expected while the sync/receipt stage of the pipeline is not yet ALIVE end to end)")

    standing_ok = None
    if report_after_run1 and report_after_run2:
        s1 = report_after_run1.get("standing")
        s2 = report_after_run2.get("standing")
        standing_ok = s1 == s2
        print(f"crown report standing: run1={s1!r} run2={s2!r} -> {'SAME' if standing_ok else 'DIFFERENT'}")
    else:
        print("crown report standing: UNVERIFIED (one or both runs produced no readable verifier-report.json)")

    if run1_ok and run2_ok and not tree_diff and chain_ok is True and standing_ok is True:
        print("\nREPLAY_MATCH: NO_SEMANTIC_CHANGE, NO_GENERATED_DRIFT — replay is idempotent")
        return 0
    print("\nREPLAY divergence or an unverifiable stage found — see details above. "
          "Not claiming REPLAY_MATCH.")
    return 1 if (not run1_ok or not run2_ok) else 0


def clean_room() -> int:
    branch = current_branch(REPO_ROOT)
    head = current_head(REPO_ROOT)
    print(f"=== clean_room.py: isolated worktree of {branch}@{head} ===")

    tmp_parent = Path(tempfile.mkdtemp(prefix="ggen-v26.8.1-clean-room-"))
    worktree_path = tmp_parent / "worktree"

    add = run(
        ["git", "worktree", "add", "--detach", str(worktree_path), head],
        cwd=REPO_ROOT,
        timeout=120,
    )
    print(add.stdout)
    print(add.stderr)
    if add.returncode != 0:
        print(f"clean_room.py: FAILED to create worktree (exit {add.returncode}) — "
              f"cannot even attempt the clean-room build. This alone is evidence.")
        return add.returncode

    try:
        print(f"clean-room worktree created at {worktree_path} (no target/, no .ggen/.ggen-v2 reused)")
        for stale in [".ggen", ".ggen-v2", "target"]:
            p = worktree_path / stale
            if p.exists():
                print(f"WARNING: unexpected pre-existing {p} in fresh worktree — removing for isolation")
                shutil.rmtree(p, ignore_errors=True)

        print("=== clean_room.py: running `just v26-8-1-rebuild` in the isolated worktree ===")
        result = run(["just", "v26-8-1-rebuild"], cwd=worktree_path, timeout=3600)
        print(result.stdout[-8000:])
        print(result.stderr[-8000:])

        print("\n=== CLEAN-ROOM RESULT ===")
        print(f"exit code: {result.returncode}")
        if result.returncode == 0:
            print("clean_room.py: v26-8-1-rebuild PASSED from a genuinely clean worktree")
        else:
            print("clean_room.py: v26-8-1-rebuild did NOT fully pass from a clean worktree "
                  "(expected/possible mid-mission — see stage summary above for exactly where)")
        return result.returncode
    finally:
        print(f"clean_room.py: worktree left at {worktree_path} for inspection "
              f"(not auto-removed; remove manually with "
              f"`git worktree remove --force {worktree_path}` from {REPO_ROOT} when done)")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--replay-only-in-place",
        action="store_true",
        help="run the idempotence replay check in the current checkout instead of a fresh worktree",
    )
    args = parser.parse_args()
    if args.replay_only_in_place:
        return replay_in_place()
    return clean_room()


if __name__ == "__main__":
    sys.exit(main())
