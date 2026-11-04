#!/usr/bin/env python3
"""Shared-index disclosure gate (O3 mechanical-gates-spec Gate 2, S-12).

Refuse-by-default check that the git index contains only paths owned by the
committing lane, plus the attribution-footer writer for disclose-to-adopt.

Consumes docs/jira/<milestone>/lane-registry.json when present (Gate 1's
registry — one registry, two consumers per spec Gate 2); until V6 lands the
coordinator-minted registry it seeds ownership from FALLBACK_LANES below.
COMPOSE-V6: when scripts/lane_lease_hook.py lands with a registry reader,
import it here instead of _load_registry/_owned_match.

Usage:
  As prepare-commit-msg hook (via core.hooksPath .githooks):
      GGEN_LANE_ID=D25 git commit ...
      GGEN_LANE_ID=D25 GGEN_LANE_DISCLOSE=D57 git commit ...  # adopt D57's paths
  Direct:
      GGEN_LANE_ID=D25 python3 scripts/index_disclosure.py --check
      just fleet-disclose
"""

from __future__ import annotations

import fnmatch
import json
import os
import subprocess
import sys
from pathlib import Path

REPO_ROOT_CANDIDATES = [
    Path(os.environ["GGEN_REPO_ROOT"]) if os.environ.get("GGEN_REPO_ROOT") else None,
    Path.cwd(),
    Path(__file__).resolve().parent.parent,
]

REGISTRY_GLOB = "docs/jira/*/lane-registry.json"
FALLBACK_LANES = [
    # Seed ownership so the gate is lawful before the coordinator-minted
    # lane-registry.json exists. Superseded by any lane-registry.json on disk.
    {"lane_id": "O3-2", "owned_paths": ["scripts/index_disclosure.py", ".githooks/**",
                                        "docs/jira/v26.11.4/**"]},
    {"lane_id": "V6", "owned_paths": ["scripts/lane_lease_hook.py", "scripts/hooks/**",
                                      "docs/jira/v26.11.4/**"]},
]

FOOTER_KEY = "Index-disclosure:"


def _repo_root() -> Path:
    for c in REPO_ROOT_CANDIDATES:
        if c and (c / ".git").exists():
            return c
    raise SystemExit("index_disclosure: not a git repo (set GGEN_REPO_ROOT)")


def _staged(root: Path) -> list[str]:
    out = subprocess.run(["git", "diff", "--cached", "--name-only"],
                         cwd=root, capture_output=True, text=True)
    if out.returncode != 0:
        print(f"index_disclosure: git diff failed: {out.stderr}", file=sys.stderr)
        sys.exit(2)
    return [line for line in out.stdout.splitlines() if line]


def _load_registry(root: Path) -> dict:
    """Load lane-registry.json; seed from the spec schema if absent.

    COMPOSE-V6: replace body with V6's lane_lease_hook.py registry reader.
    """
    hits = sorted(root.glob(REGISTRY_GLOB), key=lambda p: p.stat().st_mtime)
    if hits:
        with open(hits[-1]) as f:
            return json.load(f)
    return {"milestone": "(seeded)", "repo": str(root), "lanes": FALLBACK_LANES}


def _owned_match(staged: list[str], lane: dict | None) -> list[str]:
    """gitignore-style fnmatch against owned_paths (spec Gate 1 schema)."""
    if not staged or lane is None:
        return []
    pats = lane.get("owned_paths", [])
    return [f for f in staged
            if any(fnmatch.fnmatch(f, p) or f == p
                   or (p.endswith("/**") and fnmatch.fnmatch(f, p[:-3] + "*"))
                   for p in pats)]


def _resolve_lane(reg: dict, lane_id: str) -> dict | None:
    for lane in reg.get("lanes", []) or []:
        if lane.get("lane_id") == lane_id:
            return lane
    return None


def _owner_of(path: str, reg: dict) -> str | None:
    for lane in reg.get("lanes", []) or []:
        if _owned_match([path], lane):
            return lane["lane_id"]
    return None


def footer(committer_lane: str, disclosed: dict[str, str]) -> str:
    """Attribution footer per spec Gate 2 template."""
    lines = [f"Index-disclosure: foreign paths committed under "
             f"{committer_lane} lease by {committer_lane}:"]
    for path in sorted(disclosed):
        lines.append(f"  {path} (owner: {disclosed[path]})")
    return "\n".join(lines)


def run(msg_file: str | None) -> int:
    root = _repo_root()
    reg = _load_registry(root)
    staged = _staged(root)
    if not staged:
        return 0  # nothing staged; not this gate's problem

    lane_id = os.environ.get("GGEN_LANE_ID", "")
    disclose_raw = os.environ.get("GGEN_LANE_DISCLOSE", "")

    if not lane_id:
        print("index_disclosure: GGEN_LANE_ID not set; commits are "
              "coordinator-only (GGEN_LANE_ID=COORD to audit-only-bypass).",
              file=sys.stderr)
        return 1

    if lane_id == "COORD":
        print("index_disclosure: COORD commit — path check bypassed.",
              file=sys.stderr)
        return 0

    mine = _resolve_lane(reg, lane_id)
    if mine is None:
        print(f"index_disclosure: lane '{lane_id}' not in registry.", file=sys.stderr)
        return 1

    foreign = [f for f in staged if not _owned_match([f], mine)]
    if not foreign:
        return 0  # silent pass

    # Foreign paths present -> disclose or refuse (refuse-by-default).
    if not disclose_raw:
        print("index_disclosure: REFUSED — staged paths outside "
              f"{lane_id}'s owned_paths:", file=sys.stderr)
        for f in foreign:
            print(f"  {f} (owner: {_owner_of(f, reg) or 'UNOWNED'})", file=sys.stderr)
        print("Fix: git restore --staged <paths>  OR  "
              "GGEN_LANE_DISCLOSE=<owner-lane-id> to adopt.", file=sys.stderr)
        return 1

    # Disclose-to-adopt: the named owner(s) must actually own the paths.
    allowed = {s.strip() for s in disclose_raw.split(",") if s.strip()}
    disclosed: dict[str, str] = {}
    uncovered = []
    for f in foreign:
        owner = _owner_of(f, reg)
        if owner is None or owner not in allowed:
            uncovered.append((f, owner))
            continue
        disclosed[f] = owner
    if uncovered:
        print(f"index_disclosure: GGEN_LANE_DISCLOSE={disclose_raw} does not "
              f"cover these foreign paths:", file=sys.stderr)
        for f, owner in uncovered:
            print(f"  {f} (owner: {owner or 'UNOWNED'})", file=sys.stderr)
        return 1

    if msg_file:
        with open(msg_file, "a") as fh:
            fh.write("\n" + footer(lane_id, disclosed) + "\n")
    print(footer(lane_id, disclosed), file=sys.stderr)
    return 0


def main() -> int:
    args = sys.argv[1:]
    msg_file = None
    if args and not args[0].startswith("-"):
        msg_file = args[0]  # prepare-commit-msg convention: first arg = msg file
    return run(msg_file)


if __name__ == "__main__":
    sys.exit(main())
