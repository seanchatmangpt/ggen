#!/usr/bin/env python3
"""Lane-lease pre-commit hook (O3 mechanical-gates-spec Gate 1, ggen SD-19).

Refuses commits that stage paths outside the calling lane's owned_paths
globs in .ggen/lane-registry.json (or docs/jira/*/lane-registry.json).

Env:
  GGEN_LANE_ID        lane lease (required; COORD = coordinator bypass w/ orphan audit)
  GGEN_LANE_DISCLOSE  <lane-id> explicitly adopt foreign staged paths; prints the
                      attribution footer (O3 spec-2 compose) and proceeds.

Usage: lane_lease_hook.py [--install]
  --install  writes .git/hooks/pre-commit invoking this script (chmod +x).
"""
import fnmatch
import json
import os
import sys
from pathlib import Path

REGISTRY_CANDIDATES = [
    Path(".ggen/lane-registry.json"),
]


def find_registry():
    env = os.environ.get("GGEN_LANE_REGISTRY")
    if env:
        p = Path(env)
        if p.is_file():
            return p
    candidates = [c for c in registry_candidates() if c.is_file()]
    if not candidates:
        return None
    # newest-wins (composes with O3-2's docs/jira/*/lane-registry.json reader)
    return max(candidates, key=lambda p: p.stat().st_mtime)


def registry_candidates():
    return REGISTRY_CANDIDATES + sorted(Path(".").glob("docs/jira/*/lane-registry.json"))


def load_registry():
    p = find_registry()
    if p is None:
        print(
            "lane-lease: no lane-registry.json found (.ggen/lane-registry.json or "
            "docs/jira/*/lane-registry.json); set GGEN_LANE_REGISTRY to override"
        )
        return None, None
    try:
        with open(p) as f:
            return json.load(f), p
    except (OSError, ValueError) as e:
        print(f"lane-lease: registry {p} unreadable: {e}")
        sys.exit(1)


def path_matches(path, patterns):
    for pat in patterns:
        if fnmatch.fnmatch(path, pat):
            return True
        if pat.endswith("/**") and (
            path.startswith(pat[:-2]) or fnmatch.fnmatch(path, pat[:-2] + "/*")
        ):
            return True
    return False


def staged_files():
    out = os.popen("git diff --cached --name-only").read()
    return [l.strip() for l in out.splitlines() if l.strip()]


def current_repo_root():
    return os.popen("git rev-parse --show-toplevel").read().strip()


def owning_lane(path, lanes):
    root = current_repo_root()
    for lane in lanes:
        # repo-scoped ownership: a lane of another repo never owns paths here
        repo = (lane.get("repo") or "").strip()
        if repo and repo != "multi-repo (cgap,arcus,lsp-max,smalt)" and repo != root:
            continue
        if path_matches(path, lane.get("owned_paths", [])):
            return lane["lane_id"]
    return None


def footer(path_owner_pairs, discloser):
    lines = [
        "Index-disclosure: foreign paths committed under %s lease:" % discloser
    ]
    for p, owner in path_owner_pairs:
        lines.append("  %s (owner: %s)" % (p, owner))
    return "\n".join(lines)


def main():
    if "--install" in sys.argv:
        hook = Path(".git/hooks/pre-commit")
        invocation = "#!/bin/sh\nexec python3 scripts/lane_lease_hook.py\n"
        hook.parent.mkdir(parents=True, exist_ok=True)
        hook.write_text(invocation)
        hook.chmod(0o755)
        print(f"lane-lease: installed {hook} -> scripts/lane_lease_hook.py")
        return 0

    reg, regpath = load_registry()
    lane_id = os.environ.get("GGEN_LANE_ID", "").strip()
    if not lane_id:
        print(
            "lane-lease: REFUSED — no lane lease in environment; "
            "commits are coordinator-only (set GGEN_LANE_ID)"
        )
        return 1
    if reg is None:
        print("lane-lease: REFUSED — registry missing; cannot verify lease")
        return 1

    lanes = reg.get("lanes", [])
    staged = staged_files()
    if not staged:
        return 0

    # Coordinator bypass with orphan audit (spec step 6) — before lane lookup
    if lane_id == "COORD":
        orphans = [p for p in staged if owning_lane(p, lanes) is None]
        if orphans:
            for p in orphans:
                print(f"lane-lease: REFUSED — orphan staged path (no lane owns it): {p}")
            return 1
        return 0

    lane = next((l for l in lanes if l.get("lane_id") == lane_id), None)
    if lane is None:
        print(
            f"lane-lease: REFUSED — lane '{lane_id}' not in registry "
            f"({regpath}); leases are coordinator-minted"
        )
        return 1

    # Stale-lease check
    status = lane.get("status", "")
    expires = lane.get("lease_expires")
    if expires and status not in ("released", "done"):
        try:
            from datetime import datetime, timezone

            exp = datetime.fromisoformat(expires.replace("Z", "+00:00"))
            now = datetime.now(timezone.utc)
            if now > exp:
                print(
                    f"lane-lease: REFUSED — lease for '{lane_id}' expired at "
                    f"{expires} (status {status}); renew with the coordinator"
                )
                return 1
        except ValueError:
            print(f"lane-lease: registry lease_expires unparseable: {expires}")
            return 1

    disclose = os.environ.get("GGEN_LANE_DISCLOSE", "").strip()
    if lane_id == "COORD":
        orphans = [(p, None) for p in staged if owning_lane(p, lanes) is None]
        if orphans:
            for p, _ in orphans:
                print(f"lane-lease: REFUSED — orphan staged path (no lane owns it): {p}")
            return 1
        return 0

    foreign = [
        p for p in staged if not path_matches(p, lane.get("owned_paths", []))
    ]
    if not foreign:
        return 0

    pairs = [(p, owning_lane(p, lanes) or "UNOWNED") for p in foreign]
    if disclose:
        print("lane-lease: DISCLOSED adopt of foreign staged paths:")
        print(footer(pairs, disclose))
        return 0

    print(f"lane-lease: REFUSED — lane '{lane_id}' staged paths outside its lease:")
    for p, owner in pairs:
        print(f"  {p} (owner: {owner})")
    print(
        "fix: git restore --staged <paths>, or set "
        "GGEN_LANE_DISCLOSE=<lane-id> to adopt with an attribution footer"
    )
    return 1


if __name__ == "__main__":
    sys.exit(main())
