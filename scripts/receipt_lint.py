#!/usr/bin/env python3
"""receipt_lint.py — Gate 3 (SD-21/O3-3/S-10): receipt standing-line lint.

Scans receipt *.md files for a parseable standing line using the accepted
synonym vocabulary from docs/jira/v26.11.4/closure/mechanical-gates-spec.md
(Gate 3). Missing/unparseable standing -> FAIL naming the file.

--backfill prints the proposed insertion as a diff; never writes files.
Emits a standing-map hash so vocabulary changes re-trigger backfill
consciously.

Census jurisdiction note: the E-y2 census (profile_guard_census.py) is NOT
in this repo — it lives in-session. This script owns the file -> standing
map + hash; the census extension (which consumes receipt-standing-map.json)
belongs to the census-owning repo/session and is named here per the O3 spec.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
from pathlib import Path

# Gate 3 accepted synonym keys (case-insensitive, line-start).
KEY_SYNONYMS = [
    "Standing", "Status", "verdict", "Result",
    "Exit-code", "Exit code", "Gate",
]
KEY_RE = re.compile(
    r"^#?\s*(" + "|".join(KEY_SYNONYMS) + r")\s*[:=]\s*(.+)$",
    re.IGNORECASE,
)

# Gate 3 accepted standing values (case-sensitive as written).
STANDING_VOCAB = [
    "ALIVE", "PARTIAL_ALIVE", "PARTIAL", "BLOCKED", "REFUSED",
    "UNSUPPORTED", "UNKNOWN", "VERIFIED", "DECIDED",
    "OPEN", "CLOSED", "CLOSED-SIMULATED", "FAIL-HONEST", "BUILD_BROKEN",
    "CONFORMANT", "PASS", "FAIL",
]

# `Exit code: 0` / `Exit-code: 0` -> Exit-code-0 (spec value family).
EXIT_CODE_VALUE_RE = re.compile(r"^\d+$")

REPO = Path(__file__).resolve().parent.parent

# Receipts surfaces this repo owns (spec: docs/jira/*/receipts/** + receipts/**).
DEFAULT_TARGETS = [
    REPO / "receipts",
    *sorted((REPO / "docs" / "jira").glob("*/receipts"),
            key=lambda p: p.name, reverse=True),
]
STANDING_MAP_PATH = REPO / "receipts" / "receipt-standing-map.json"

BACKFILL_SCAN_LINES = 5


def standing_map_hash() -> str:
    """Hash of the accepted-vocabulary map (keys + values + exit-code rule)."""
    h = hashlib.sha256()
    h.update(json.dumps(
        {"keys": KEY_SYNONYMS, "values": STANDING_VOCAB,
         "exit_code_rule": "Exit-code: N -> Exit-code-N",
         "annotation_strip": ["backticks", "parenthesized-suffix"]},
        sort_keys=True,
    ).encode())
    return h.hexdigest()[:16]


def strip_annotation(value: str) -> tuple[str, str]:
    """Strip backticks and parenthesized suffixes.

    VERIFIED(9/9) -> (VERIFIED, 9/9); `PASS` -> (PASS, "").
    """
    v = value.strip().strip("`").strip()
    annotation = ""
    m = re.match(r"^(\S+?)\(([^()]*)\)\s*$", v)
    if m:
        return m.group(1), m.group(2)
    tokens = v.split()
    if tokens:
        v = tokens[0].strip("`")
    return v, annotation


def parse_standing(line_value: str) -> tuple[str | None, str, str]:
    """Return (standing, annotation, derived_from) or (None, "", reason)."""
    v, ann = strip_annotation(line_value)
    if v.startswith("Exit-code-"):  # already normalized
        return v, ann, "exit-code"
    if EXIT_CODE_VALUE_RE.match(v):
        return "Exit-code-%s" % v, ann, "exit-code"
    if v in STANDING_VOCAB:
        return v, ann, "vocab"
    return None, "", "value '%s' not in vocabulary" % v


def parse_file(path: Path) -> dict:
    """Parse one receipt file into a standing-map entry."""
    try:
        lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    except OSError as e:
        return {"file": str(path), "ok": False,
                "reason": "unreadable: %s" % e}

    for i, line in enumerate(lines):
        m = KEY_RE.match(line)
        if not m:
            continue
        standing, ann, err = parse_standing(m.group(2))
        if standing is None:
            return {
                "file": str(path), "key": m.group(1), "standing": None,
                "annotation": "", "ok": False,
                "reason": "line %d: %s" % (i + 1, err),
            }
        return {
            "file": str(path), "key": m.group(1), "standing": standing,
            "annotation": ann, "ok": True, "line": i + 1,
        }
    return {"file": str(path), "ok": False,
            "reason": "no parseable standing line"}


def backfill_proposal(path: Path) -> str | None:
    """Find an unambiguous value token in the first 5 lines; propose insertion.

    Returns a unified-diff-style proposal string, or None.
    """
    try:
        lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    except OSError:
        return None
    head = lines[:BACKFILL_SCAN_LINES]
    found: tuple[int, str] | None = None
    for i, line in enumerate(head):
        # Bare value token alone on a line (e.g. a status marker line).
        v = line.strip().strip("`").strip()
        m = re.match(r"^(\S+?)\(([^()]*)\)$", v)
        if m:
            v = m.group(1)
        if v in STANDING_VOCAB:
            found = (i, v)
            break
        if EXIT_CODE_VALUE_RE.match(v) and _has_exit_code_context(head):
            found = (i, "Exit-code-%s" % v)
            break
    if found is None:
        return None
    i, value = found
    if i == 0:
        proposal = "Standing: %s" % value
        return "\n".join([
            "--- a/%s" % path.name,
            "+++ b/%s (proposed, not written)" % path.name,
            "@@ -0,0 +1 @@",
            "+%s" % proposal,
            "  (insert as line 1 of %s)" % path,
        ])
    return "\n".join([
        "--- a/%s" % path.name,
        "+++ b/%s (proposed, not written)" % path.name,
        "@@ -%d,1 +%d,2 @@" % (i + 1, i + 1),
        " %s" % lines[i].rstrip(),
        "+Standing: %s" % value,
    ])


def _has_exit_code_context(head: list[str]) -> bool:
    return any(re.search(r"[Ee]xit[- ]?code", ln) for ln in head)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("targets", nargs="*", default=None,
                    help="receipt files or dirs (default: repo receipts surfaces)")
    ap.add_argument("--backfill", action="store_true",
                    help="print proposed Standing insertions as diffs; never writes")
    ap.add_argument("--emit-map", metavar="PATH", default=None,
                    help="write file->standing map JSON here (default: %s)"
                         % STANDING_MAP_PATH)
    ap.add_argument("--no-map", action="store_true",
                    help="skip writing the standing map")
    args = ap.parse_args()

    targets = [Path(t) for t in args.targets] if args.targets else DEFAULT_TARGETS
    files: list[Path] = []
    for t in targets:
        if t.is_dir():
            files.extend(sorted(t.rglob("*.md")))
        elif t.suffix == ".md":
            files.append(t)
        else:
            print("receipt-lint: skipping non-.md target: %s" % t, file=sys.stderr)
    files = sorted(set(files))

    results = [parse_file(f) for f in files]
    failures = [r for r in results if not r["ok"]]

    print("receipt-lint: standing-map hash %s" % standing_map_hash())
    print("receipt-lint: %d files, %d parse OK, %d FAIL"
          % (len(results), len(results) - len(failures), len(failures)))

    write_map = (args.emit_map is not None) or (not args.no_map)
    if write_map:
        map_path = Path(args.emit_map) if args.emit_map else STANDING_MAP_PATH
        map_data = {
            "standing_map_hash": standing_map_hash(),
            "generated_by": "scripts/receipt_lint.py",
            "entries": {r["file"]: {
                "standing": r.get("standing"),
                "key": r.get("key"),
                "annotation": r.get("annotation", ""),
                "ok": r["ok"],
            } for r in results},
        }
        map_path.write_text(json.dumps(map_data, indent=2, sort_keys=True) + "\n",
                            encoding="utf-8")
        print("receipt-lint: map written -> %s" % map_path)

    if args.backfill:
        for r in failures:
            if "no parseable standing line" not in r.get("reason", ""):
                continue  # backfill only applies to files missing a key line
            prop = backfill_proposal(Path(r["file"]))
            if prop:
                print("--- backfill proposal for %s" % r["file"])
                print(prop)
            else:
                print("--- no backfill proposal for %s (%s)"
                      % (r["file"], r.get("reason", "unambiguous token not found")))
        return 0

    for r in failures:
        print("FAIL %s: %s" % (r["file"], r.get("reason", "no parseable standing line")),
              file=sys.stderr)
    if failures:
        print("receipt-lint: %d FAIL" % len(failures), file=sys.stderr)
        return 1
    print("receipt-lint: PASS (0 missing)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
