#!/usr/bin/env python3
"""receipt_census.py — S-12 census-side consumer of receipts/receipt-standing-map.json.

The lint (scripts/receipt_lint.py) owns file -> standing parsing and writes
the map. This script is the census consumer: it re-derives the map's
trustworthiness instead of trusting it.

Verifies, fail-closed:
  1. The map exists and is parseable JSON.
  2. standing_map_hash in the map equals the CURRENT vocabulary hash computed
     from scripts/receipt_lint.py — a vocabulary edit that does not re-lint
     (and so does not refresh the map) fails here, not silently.
  3. Every entry ok:true. Any ok:false entry fails (the map must only exist
     in a state the lint already admitted).
  4. Every entry's file still exists on disk and its pinned sha256 matches
     the file's current content. A stale map (receipt edited after the last
     lint, receipt deleted, map hand-edited) fails here.

Exit 0 only when all four hold. Exit 1 names every violation.

Run standalone:  python3 scripts/ci/receipt_census.py
Run in CI:       .github/workflows/ci.yml job `receipt-lint` (step 2).
"""
from __future__ import annotations

import hashlib
import json
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent.parent
MAP_PATH = REPO / "receipts" / "receipt-standing-map.json"
LINT_SCRIPT = REPO / "scripts" / "receipt_lint.py"


def current_vocabulary_hash() -> str:
    """Import the lint module and reuse its hash — one hash, not two."""
    lint_dir = str(REPO / "scripts")
    if lint_dir not in sys.path:
        sys.path.insert(0, lint_dir)
    import receipt_lint  # noqa: local import keeps the hash single-sourced

    return receipt_lint.standing_map_hash()


def main() -> int:
    violations: list[str] = []

    if not MAP_PATH.is_file():
        print("census FAIL: missing map %s (run: just lint-receipts)"
              % MAP_PATH, file=sys.stderr)
        return 1
    try:
        data = json.loads(MAP_PATH.read_text(encoding="utf-8"))
    except (OSError, ValueError) as e:
        print("census FAIL: unparseable map: %s" % e, file=sys.stderr)
        return 1

    entries = data.get("entries")
    if not isinstance(entries, dict):
        print("census FAIL: map has no 'entries' object", file=sys.stderr)
        return 1

    # 2. Vocabulary currency.
    want_hash = current_vocabulary_hash()
    got_hash = data.get("standing_map_hash")
    if got_hash != want_hash:
        violations.append(
            "map hash %s != current vocabulary hash %s "
            "(vocabulary changed since last lint; re-run just lint-receipts)"
            % (got_hash, want_hash))

    # 3 + 4. Per-entry checks against real disk state.
    stale = 0
    for path_str, entry in sorted(entries.items()):
        if not entry.get("ok"):
            violations.append("%s: ok:false in map (%s)"
                              % (path_str, entry.get("reason", "no reason")))
            continue
        f = Path(path_str)
        if not f.is_file():
            violations.append("%s: in map but missing on disk" % path_str)
            continue
        pin = entry.get("pin") or {}
        want = pin.get("sha256")
        got = hashlib.sha256(
            f.read_bytes().decode("utf-8", errors="replace")
            .encode("utf-8", errors="replace")).hexdigest()
        if got != want:
            stale += 1
            violations.append("%s: content sha256 %s != pinned %s "
                              "(receipt changed after last lint)"
                              % (path_str, got[:12], str(want)[:12]))

    total = len(entries)
    print("receipt-census: %d entries, %d violations"
          % (total, len(violations)))
    if violations:
        for v in violations[:50]:
            print("FAIL %s" % v, file=sys.stderr)
        if len(violations) > 50:
            print("... and %d more" % (len(violations) - 50), file=sys.stderr)
        print("receipt-census: FAIL (%d violations)" % len(violations),
              file=sys.stderr)
        return 1
    print("receipt-census: PASS (map current, %d receipts pinned clean)"
          % total)
    return 0


if __name__ == "__main__":
    sys.exit(main())
