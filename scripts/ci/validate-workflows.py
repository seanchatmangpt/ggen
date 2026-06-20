#!/usr/bin/env python3
"""
validate-workflows.py — structural validator for .github/workflows/*.yml

No actionlint, no yamllint, no network. Uses PyYAML if importable; otherwise
falls back to a tolerant line-based structural check (pyyaml is NOT assumed).

For every workflow file it confirms:
  1. presence of top-level `name:`, `on:` (or `'on':` / `"on":`), and `jobs:`
  2. every `uses:` line pins an action version: `owner/repo@<ref>` (or a local
     `./path` action, which needs no version). Any unpinned `uses:` is a FAIL.

Prints PASS/FAIL per file plus a summary. Exits non-zero if any file FAILs.

Usage:
    python3 scripts/ci/validate-workflows.py [WORKFLOWS_DIR]
Default WORKFLOWS_DIR is <repo-root>/.github/workflows.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

try:
    import yaml  # type: ignore

    HAVE_YAML = True
except Exception:  # pragma: no cover - depends on runner env
    HAVE_YAML = False


# A `uses:` value is considered pinned if it is a local composite action
# (`./...` or `../...`) or a Docker ref, or it contains an `@<ref>`.
USES_LINE = re.compile(r"""^\s*-?\s*uses\s*:\s*['"]?([^'"#]+?)['"]?\s*(?:#.*)?$""")


def uses_is_pinned(value: str) -> bool:
    value = value.strip()
    if not value:
        return False
    # Local composite/dockerfile actions resolve from the repo; no version needed.
    if value.startswith("./") or value.startswith("../"):
        return True
    # Container actions (docker://image:tag) carry their own tag; treat as pinned.
    if value.startswith("docker://"):
        return True
    # Everything else (owner/repo[/subdir]) must carry an @ref.
    return "@" in value


def collect_uses(lines: list[str]) -> list[tuple[int, str]]:
    """Return (line_number, uses_value) for every `uses:` line."""
    found: list[tuple[int, str]] = []
    for i, raw in enumerate(lines, start=1):
        m = USES_LINE.match(raw)
        if m:
            found.append((i, m.group(1).strip()))
    return found


def top_level_keys_yaml(text: str) -> set[str]:
    data = yaml.safe_load(text)
    if not isinstance(data, dict):
        return set()
    # PyYAML parses the bare `on:` key as the boolean True; normalize it back.
    keys: set[str] = set()
    for k in data.keys():
        if k is True:
            keys.add("on")
        elif isinstance(k, str):
            keys.add(k)
    return keys


def top_level_keys_lines(lines: list[str]) -> set[str]:
    """Tolerant fallback: a top-level key starts at column 0 (no indent)."""
    keys: set[str] = set()
    key_re = re.compile(r"""^(["']?)([A-Za-z_][\w-]*)\1\s*:""")
    for raw in lines:
        if not raw.strip() or raw.lstrip().startswith("#"):
            continue
        if raw[0] in (" ", "\t"):  # indented -> not top level
            continue
        m = key_re.match(raw)
        if m:
            keys.add(m.group(2))
    return keys


def validate_file(path: Path) -> tuple[bool, list[str]]:
    problems: list[str] = []
    text = path.read_text(encoding="utf-8", errors="replace")
    lines = text.splitlines()

    # --- required top-level keys ---
    parsed_ok = True
    if HAVE_YAML:
        try:
            keys = top_level_keys_yaml(text)
        except Exception as e:  # malformed YAML -> hard fail, then still line-check
            problems.append(f"YAML parse error: {e}")
            parsed_ok = False
            keys = top_level_keys_lines(lines)
    else:
        keys = top_level_keys_lines(lines)

    for required in ("name", "on", "jobs"):
        if required not in keys:
            problems.append(f"missing top-level `{required}:`")

    # --- pinned `uses:` ---
    for ln, value in collect_uses(lines):
        if not uses_is_pinned(value):
            problems.append(f"line {ln}: unpinned `uses: {value}` (needs @version)")

    ok = parsed_ok and not problems
    # A YAML parse failure already recorded its own problem; reflect in ok.
    if problems:
        ok = False
    return ok, problems


def main(argv: list[str]) -> int:
    if len(argv) > 1:
        wf_dir = Path(argv[1])
    else:
        wf_dir = Path(__file__).resolve().parents[2] / ".github" / "workflows"

    if not wf_dir.is_dir():
        print(f"ERROR: workflows directory not found: {wf_dir}", file=sys.stderr)
        return 2

    files = sorted(p for p in wf_dir.iterdir() if p.suffix in (".yml", ".yaml"))
    if not files:
        print(f"ERROR: no workflow files in {wf_dir}", file=sys.stderr)
        return 2

    backend = "PyYAML" if HAVE_YAML else "line-based fallback (no PyYAML)"
    print(f"validate-workflows.py — {len(files)} file(s) in {wf_dir}")
    print(f"parser backend: {backend}\n")

    failed = 0
    for path in files:
        ok, problems = validate_file(path)
        status = "PASS" if ok else "FAIL"
        print(f"[{status}] {path.name}")
        if not ok:
            failed += 1
            for p in problems:
                print(f"         - {p}")

    print("\n" + "-" * 56)
    passed = len(files) - failed
    print(f"summary: {passed} PASS, {failed} FAIL, {len(files)} total")
    if failed:
        print("RESULT: FAIL")
        return 1
    print("RESULT: PASS")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
