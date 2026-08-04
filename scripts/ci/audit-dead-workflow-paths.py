#!/usr/bin/env python3
"""Flag GitHub Actions workflows whose path-filtered trigger can never fire.

For every .github/workflows/*.yml, parses the `paths:` (and `paths-ignore:`)
glob lists under `on.pull_request` / `on.push`, and checks each glob against
the real working tree. A workflow is flagged only when EVERY one of its path
globs matches zero files -- i.e. the feature/area it watches for was deleted
or moved, so its trigger condition is now structurally dead.

Workflows with no `paths:` filter at all (repo-wide triggers), or whose triggers
are exclusively workflow_dispatch/schedule/release/workflow_call, are skipped --
this script only judges path-scoped triggers, since those are the only ones
whose liveness is a function of what's on disk.

Usage: python3 scripts/ci/audit-dead-workflow-paths.py [--verbose]

Exit code 0 always (this is a report, not a gate); flagged workflows are
printed to stdout for human review -- do not auto-delete based on this
output alone, confirm each one by reading the workflow's actual job content.
"""
from __future__ import annotations

import glob
import sys
from pathlib import Path

import yaml

REPO_ROOT = Path(__file__).resolve().parents[2]
WORKFLOWS_DIR = REPO_ROOT / ".github" / "workflows"


def extract_path_globs(doc: dict) -> list[str]:
    """Pull every paths:/paths-ignore: glob out of an `on:` block, any trigger."""
    on = doc.get("on") or doc.get(True)  # PyYAML parses bare `on:` key as True in some yaml versions
    if not isinstance(on, dict):
        return []
    globs: list[str] = []
    for trigger_name in ("pull_request", "push"):
        trigger = on.get(trigger_name)
        if not isinstance(trigger, dict):
            continue
        for key in ("paths", "paths-ignore"):
            val = trigger.get(key)
            if isinstance(val, list):
                globs.extend(str(g) for g in val)
    return globs


def glob_matches_something(pattern: str) -> bool:
    """Check a workflow path glob against the real tree. Handles ** and plain dirs."""
    pattern = pattern.strip().strip('"').strip("'")
    if not pattern or pattern.startswith("!"):
        # Negated glob (exclusion) -- not a liveness signal on its own, skip.
        return True
    # A bare directory prefix like "book/**" or "packs/foo/**" -- also match
    # the bare directory itself (glob.glob requires a trailing pattern).
    candidates = [pattern]
    if pattern.endswith("/**"):
        candidates.append(pattern[: -len("/**")])
    for cand in candidates:
        full = str(REPO_ROOT / cand)
        if glob.glob(full, recursive=True):
            return True
        # glob.glob doesn't match a bare existing directory with no trailing
        # segment reliably across globs containing '**' in the middle; fall
        # back to a direct filesystem check for the non-wildcard prefix.
        prefix = cand.split("*")[0]
        if prefix and (REPO_ROOT / prefix).exists():
            return True
    return False


def main() -> int:
    verbose = "--verbose" in sys.argv
    flagged: list[tuple[str, list[str]]] = []
    skipped_no_paths: list[str] = []
    parse_errors: list[str] = []

    for wf_path in sorted(WORKFLOWS_DIR.glob("*.yml")):
        try:
            doc = yaml.safe_load(wf_path.read_text())
        except yaml.YAMLError:
            # A few workflows use GitHub Actions' flow-mapping shorthand with an
            # unquoted ${{ }} expression inline (e.g. `{ group: foo-${{ x }} }`),
            # which is valid GHA YAML but trips PyYAML's flow-mapping parser.
            # Not a bug in the workflow -- just outside what this audit can judge
            # without a full GHA-aware YAML parser. Report and skip, don't crash.
            parse_errors.append(wf_path.name)
            continue
        if not isinstance(doc, dict):
            continue
        path_globs = extract_path_globs(doc)
        if not path_globs:
            skipped_no_paths.append(wf_path.name)
            continue
        dead_globs = [g for g in path_globs if not glob_matches_something(g)]
        if len(dead_globs) == len(path_globs):
            flagged.append((wf_path.name, path_globs))

    print(f"Scanned {len(list(WORKFLOWS_DIR.glob('*.yml')))} workflow files.")
    print(f"{len(skipped_no_paths)} have no path filter (repo-wide or dispatch-only triggers) -- not judged here.")
    if parse_errors:
        print(f"{len(parse_errors)} could not be parsed by PyYAML (GHA flow-mapping shorthand) -- not judged here: {', '.join(parse_errors)}")
    print()
    if flagged:
        print(f"FLAGGED -- every path glob matches zero files ({len(flagged)}):")
        for name, globs_ in flagged:
            print(f"  {name}")
            for g in globs_:
                print(f"    - {g}")
    else:
        print("None flagged: every path-scoped workflow has at least one live glob.")

    if verbose:
        print()
        print("Skipped (no path filter):")
        for name in skipped_no_paths:
            print(f"  {name}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
