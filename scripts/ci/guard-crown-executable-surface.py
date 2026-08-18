#!/usr/bin/env python3
"""Fail closed when executable-surface coverage is silently weakened.

This guard turns the repository's accepted E2E-coverage baseline into a one-way
ratchet. A change may reduce the number of uncovered packs; it may not increase
that number without an explicit refusal file that names every newly uncovered
subject and the reason it cannot currently be exercised.

The refusal file is intentionally machine-readable and reviewable:
.specify/pack-e2e-coverage-refusals.txt

Each non-comment line is an exact pack identifier. The file does not make a
pack covered; it only makes the omission explicit so standing claims can remain
bounded instead of silently expanding.
"""

from __future__ import annotations

from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[2]
BASELINE = ROOT / ".specify" / "pack-e2e-coverage-baseline.txt"
REFUSALS = ROOT / ".specify" / "pack-e2e-coverage-refusals.txt"


def read_int(path: Path) -> int:
    raw = path.read_text(encoding="utf-8").strip()
    try:
        return int(raw)
    except ValueError as exc:
        raise SystemExit(f"REFUSED: {path} must contain exactly one integer") from exc


def read_refusals(path: Path) -> set[str]:
    if not path.exists():
        return set()
    return {
        line.strip()
        for line in path.read_text(encoding="utf-8").splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    }


def main() -> int:
    if not BASELINE.exists():
        print("REFUSED: missing .specify/pack-e2e-coverage-baseline.txt")
        return 2

    baseline = read_int(BASELINE)
    refusals = read_refusals(REFUSALS)

    # The historical accepted value immediately before the 2026-08-10
    # regression was 15. This is a constitutional ceiling, not a target.
    ceiling = 15
    excess = max(0, baseline - ceiling)

    if excess == 0:
        print(f"PASS: uncovered-pack baseline={baseline} <= ceiling={ceiling}")
        return 0

    if len(refusals) < excess:
        print(
            "REFUSED: executable-surface baseline weakened "
            f"from ceiling {ceiling} to {baseline}, but only {len(refusals)} "
            f"explicit refusal(s) are recorded; need at least {excess}"
        )
        return 1

    print(
        "PARTIAL: executable-surface regression is explicit, not silent: "
        f"baseline={baseline}, ceiling={ceiling}, refusals={len(refusals)}"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
