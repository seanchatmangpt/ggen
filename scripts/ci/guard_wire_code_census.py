#!/usr/bin/env python3
"""TypedBoundaryRefusal census guard (wave-1 lane A16, v26.10.20).

Diffs the wire codes actually present in the refusal-registry sources --
`ontologies/shapes/boundary-refusal.shacl.ttl` plus the generated refusal
enum modules (`crates/ggen-engine/src/generated/refusals.rs`,
`crates/ggen-abb-sbb/src/generated/refusals.rs`) -- against the generated
census file `crates/ggen-engine/src/typed_boundary_census.rs`.

Parsing is regex-based, pinned to the landed formats:

- Registry (``boundary-refusal.shacl.ttl``): ``br:code "X"`` individual
  assertions (``br:codePrefix`` values are prefixes, not codes, and are
  excluded).
- Generated enum modules: the ``wire()`` match arms, ``=> "X"``
  (``Code::EnvelopeUndeclared => "E-ENVELOPE-UNDECLARED"``). Doc-test text
  and comments never match this shape.
- Census file: every string literal inside the ``WIRE_CODES`` array.

A code present in a source but missing from the census (or vice versa) is a
drift: the census no longer describes the registry. Exit 2 + ``CENSUS_STALE``
plus the symmetric difference.

Missing sources are skipped loudly (they are owned by other lanes and may
not have landed); if EVERY source is missing the census cannot be checked
at all, so the guard exits 3 (SKIP-loud) rather than silently passing.

Usage:
    python3 scripts/ci/guard_wire_code_census.py
        [--repo ROOT]
        [--registry PATH]... [--enum PATH]... [--census PATH]

Defaults resolve from ROOT. The --registry/--enum/--census overrides exist
so the RED test can run against a temp fixture without touching the repo.

Exit codes: 0 = census matches, 2 = CENSUS_STALE, 3 = SKIP (no sources).
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

DEFAULT_REGISTRY = "ontologies/shapes/boundary-refusal.shacl.ttl"
DEFAULT_ENUMS = (
    "crates/ggen-engine/src/generated/refusals.rs",
    "crates/ggen-abb-sbb/src/generated/refusals.rs",
)
DEFAULT_CENSUS = "crates/ggen-engine/src/typed_boundary_census.rs"

CODE_RE = re.compile(r'"([A-Z][A-Z0-9_-]{2,})"')
REGISTRY_CODE_RE = re.compile(r'br:code\s+"([A-Z][A-Z0-9_-]+)"')
ENUM_WIRE_RE = re.compile(r'=>\s*"([A-Z][A-Z0-9_-]+)"')
CENSUS_ARRAY_RE = re.compile(r"WIRE_CODES[^=]*=\s*&\[(.*?)\]\s*;", re.DOTALL)


def codes_in_source(path: Path) -> set[str]:
    if not path.exists():
        return set()
    text = path.read_text(errors="replace")
    pattern = REGISTRY_CODE_RE if path.suffix in (".ttl", ".turtle") else ENUM_WIRE_RE
    return set(pattern.findall(text))


def codes_in_census(path: Path) -> set[str]:
    """Codes listed in the census file's WIRE_CODES array. Unlike sources, a
    missing census is a hard error -- the census file is this guard's own
    subject and must exist once the guard is active."""
    text = path.read_text(errors="replace")
    m = CENSUS_ARRAY_RE.search(text)
    if m is None:
        raise SystemExit(f"census file {path} has no WIRE_CODES array")
    return set(CODE_RE.findall(m.group(1)))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--repo", default=str(Path(__file__).resolve().parents[2]))
    parser.add_argument("--registry", action="append", default=[])
    parser.add_argument("--enum", dest="enums", action="append", default=[])
    parser.add_argument("--census")
    args = parser.parse_args()
    repo = Path(args.repo).resolve()

    registry_paths = [Path(p) for p in args.registry] or [repo / DEFAULT_REGISTRY]
    enum_paths = [Path(p) for p in args.enums] or [repo / e for e in DEFAULT_ENUMS]
    census_path = Path(args.census) if args.census else repo / DEFAULT_CENSUS

    present: list[str] = []
    source_codes: set[str] = set()
    for src in registry_paths + enum_paths:
        if not src.exists():
            print(f"SKIP: source not found (lane not landed?): {src}")
            continue
        present.append(str(src))
        found = codes_in_source(src)
        print(f"{src}: {len(found)} code(s)")
        source_codes |= found

    if not present:
        print("SKIP: no registry or enum sources found; census unverifiable")
        return 3
    if not census_path.exists():
        print(f"CENSUS_STALE: census file missing: {census_path}")
        return 2

    census_codes = codes_in_census(census_path)
    missing = sorted(source_codes - census_codes)
    extra = sorted(census_codes - source_codes)
    if missing or extra:
        print("CENSUS_STALE")
        for code in missing:
            print(f"  in sources, missing from census: {code}")
        for code in extra:
            print(f"  in census, not in sources: {code}")
        return 2

    print(f"census matches: {len(census_codes)} wire code(s)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
