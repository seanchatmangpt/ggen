#!/usr/bin/env python3
"""Shared-index disclosure gate (O3 mechanical-gates-spec Gate 2, SD-20/O3-2).

Refuse-by-default check that the git index contains only paths owned by the
committing lane, plus the attribution footer for disclose-to-adopt.

One registry, two consumers: reads the registry via V6/SD-19's reader in
scripts/lane_lease_hook.py when importable (COMPOSED mode), otherwise falls
back to an internal reader (fresh checkouts / self-test portability).
"""

from __future__ import annotations

import fnmatch
import json
import os
import subprocess
import sys
from pathlib import Path

_SCRIPT_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(_SCRIPT_DIR))
try:
    import lane_lease_hook as _llh  # V6, SD-19

    COMPOSED = True
except ImportError:
    COMPOSED = False

FOOTER_KEY = "Index-disclosure:"

FALLBACK_REGISTRY_GLOB = "docs/jira/*/lane-registry.json"
FALLBACK_LANES = [
    {"lane_id": "O3", "owned_paths": ["scripts/index_disclosure.py", ".githooks/**",
                                      "docs/jira/v26.11.4/**"]},
]


def _fallback_registry(root):
    hits = sorted(root.glob(FALLBACK_REGISTRY_GLOB), key=lambda p: p.stat().st_mtime)
    if hits:
        with open(hits[-1]) as f:
            return json.load(f)
    return {"milestone": "(seeded)", "repo": str(root), "lanes": FALLBACK_LANES}


def _fallback_matches(path, patterns):
    return any(fnmatch.fnmatch(path, p) for p in patterns)


def _fallback_owner(path, lanes):
    for lane in lanes:
        if _fallback_matches(path, lane.get("owned_paths", [])):
            return lane["lane_id"]
    return None


def _repo_root():
    candidates = [Path(os.environ["GGEN_REPO_ROOT"]) if os.environ.get("GGEN_REPO_ROOT") else None,
                  Path.cwd(),
                  _SCRIPT_DIR.parent]
    for cand in candidates:
        if cand is not None and (cand / ".git").exists():
            return cand
    raise SystemExit("index_disclosure: not a git repo (set GGEN_REPO_ROOT)")


def _staged(root):
    out = subprocess.run(["git", "diff", "--cached", "--name-only"],
                         cwd=root, capture_output=True, text=True)
    if out.returncode != 0:
        print("index_disclosure: git diff failed: %s" % out.stderr, file=sys.stderr)
        sys.exit(2)
    return [line for line in out.stdout.splitlines() if line]


def run(msg_file):
    root = _repo_root()
    if COMPOSED:
        reg, _regpath = _llh.load_registry()
        if reg is None:
            print("index_disclosure: REFUSED - registry missing "
                  "(.ggen/lane-registry.json or docs/jira/*/lane-registry.json).",
                  file=sys.stderr)
            return 1
        lanes = reg.get("lanes", [])
    else:
        reg = _fallback_registry(root)
        lanes = reg.get("lanes", [])

    staged = _staged(root)
    if not staged:
        return 0

    lane_id = os.environ.get("GGEN_LANE_ID", "").strip()
    disclose_raw = os.environ.get("GGEN_LANE_DISCLOSE", "").strip()

    if not lane_id:
        print("index_disclosure: REFUSED - GGEN_LANE_ID not set; commits are "
              "coordinator-only (GGEN_LANE_ID=COORD to audit-only-bypass).",
              file=sys.stderr)
        return 1
    if lane_id == "COORD":
        print("index_disclosure: COORD commit - path check bypassed.", file=sys.stderr)
        return 0

    mine = next((l for l in lanes if l.get("lane_id") == lane_id), None)
    if mine is None:
        print("index_disclosure: REFUSED - lane '%s' not in registry." % lane_id,
              file=sys.stderr)
        return 1

    owned_paths = mine.get("owned_paths", [])
    if COMPOSED:
        foreign = [p for p in staged if not _llh.path_matches(p, owned_paths)]
        def owner_of(p):
            return _llh.owning_lane(p, lanes)
    else:
        foreign = [p for p in staged if not _fallback_matches(p, owned_paths)]
        def owner_of(p):
            return _fallback_owner(p, lanes)

    if not foreign:
        return 0  # silent pass

    pairs = [(p, owner_of(p) or "UNOWNED") for p in foreign]

    if not disclose_raw:
        print("index_disclosure: REFUSED - staged paths outside %s's owned_paths:"
              % lane_id, file=sys.stderr)
        for p, owner in pairs:
            print("  %s (owner: %s)" % (p, owner), file=sys.stderr)
        print("Fix: git restore --staged <paths>  OR  "
              "GGEN_LANE_DISCLOSE=<owner-lane-id> to adopt.", file=sys.stderr)
        return 1

    # Disclose-to-adopt: the named owner(s) must actually own the foreign paths.
    allowed = {s.strip() for s in disclose_raw.split(",") if s.strip()}
    disclosed = []
    uncovered = []
    for p, owner in pairs:
        if owner in allowed:
            disclosed.append((p, owner))
        else:
            uncovered.append((p, owner))
    if uncovered:
        print("index_disclosure: REFUSED - GGEN_LANE_DISCLOSE=%s does not cover "
              "these foreign paths:" % disclose_raw, file=sys.stderr)
        for p, owner in uncovered:
            print("  %s (owner: %s)" % (p, owner), file=sys.stderr)
        return 1

    footer = FOOTER_KEY + " foreign paths committed under %s lease by %s:" % (lane_id, lane_id)
    for p, owner in disclosed:
        footer += "\n  %s (owner: %s)" % (p, owner)
    if msg_file:
        with open(msg_file, "a") as fh:
            fh.write("\n" + footer + "\n")
    print(footer, file=sys.stderr)
    return 0


def main():
    args = sys.argv[1:]
    msg_file = None
    if args and not args[0].startswith("-"):
        msg_file = args[0]  # prepare-commit-msg convention: first arg = msg file
    return run(msg_file)


if __name__ == "__main__":
    sys.exit(main())
