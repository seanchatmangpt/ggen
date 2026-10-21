#!/usr/bin/env python3
"""Dialect declaration court (R05, v26.10.20 Slice 1).

Walks a pack root; every RDF file (*.ttl, *.n3, *.nq, *.trig, *.rdf, *.jsonld)
must match exactly one `[[capabilities.dialects]]` glob in pack.toml.

Refusals (exit 1):
- UNDECLARED_DIALECT_FILE: an RDF file matching no declared glob (absent
  `dialects` key = default `*.ttl -> turtle`).
- DIALECT_CONFLICT: an extension-implied dialect (e.g. `.n3` implies `n3`)
  differs from the dialect a matched glob declares (e.g. `*.ttl` glob on a
  `.n3` file).

Exit 0 only when every RDF file is declared exactly once with a consistent
dialect. A court that cannot refuse is VACUOUS GATE — see the RED tests in
test_dialect_declaration_court.py.
"""

import fnmatch
import sys
import tomllib
from pathlib import Path

RDF_EXTENSIONS = {
    ".ttl": "turtle",
    ".nt": "ntriples",
    ".n3": "n3",
    ".trig": "trig",
    ".nq": "nquads",
    ".rdf": "rdf-xml",
    ".jsonld": "json-ld",
}

DIALECT_NAMES = {"turtle", "trig", "ntriples", "nquads", "n3", "rdf-xml", "json-ld"}

DEFAULT_DIALECTS = [{"glob": "*.ttl", "dialect": "turtle"}]


def load_declared(pack_root: Path):
    """Return the effective dialect glob list for a pack root."""
    pack_toml = pack_root / "pack.toml"
    if not pack_toml.exists():
        return None, f"missing pack.toml at {pack_root}"
    with open(pack_toml, "rb") as fh:
        data = tomllib.load(fh)
    caps = data.get("capabilities", {})
    dialects = caps.get("dialects")
    if dialects is None:
        return list(DEFAULT_DIALECTS), None
    if not dialects:
        return None, (
            "FM-PACK-021: pack declares empty `dialects` list; "
            "a pack that declares dialects must declare at least one glob"
        )
    for entry in dialects:
        if set(entry.keys()) != {"glob", "dialect"}:
            return None, f"dialects entry must have exactly {{glob, dialect}}: {entry}"
        if entry["dialect"] not in DIALECT_NAMES:
            return None, (
                f"unknown dialect '{entry['dialect']}' (closed vocabulary: "
                f"{sorted(DIALECT_NAMES)})"
            )
    return dialects, None


def court_pack(pack_root: Path):
    """Run the court over one pack root. Returns (violations, rdf_file_count)."""
    dialects, err = load_declared(pack_root)
    if err:
        return [{"code": "COURT_BLOCKED", "detail": err}], 0
    if dialects is None:
        dialects = []

    violations = []
    rdf_files = [
        p
        for p in sorted(pack_root.rglob("*"))
        if p.is_file() and p.suffix in RDF_EXTENSIONS
    ]
    for rdf in rdf_files:
        rel = rdf.relative_to(pack_root).as_posix()
        matches = [d for d in dialects if fnmatch.fnmatch(rel, d["glob"]) or fnmatch.fnmatch(rdf.name, d["glob"])]
        if not matches:
            violations.append(
                {
                    "code": "UNDECLARED_DIALECT_FILE",
                    "file": rel,
                    "detail": (
                        f"RDF file '{rel}' matches no [[capabilities.dialects]] glob"
                    ),
                }
            )
            continue
        implied = RDF_EXTENSIONS[rdf.suffix]
        for d in matches:
            if d["dialect"] != implied:
                violations.append(
                    {
                        "code": "DIALECT_CONFLICT",
                        "file": rel,
                        "detail": (
                            f"extension '.{rdf.suffix}' implies dialect "
                            f"'{implied}' but glob '{d['glob']}' declares "
                            f"'{d['dialect']}'"
                        ),
                    }
                )
    return violations, len(rdf_files)


def main(argv):
    if len(argv) < 2:
        print("usage: dialect_declaration_court.py <pack-root>...", file=sys.stderr)
        return 2
    total_violations = 0
    for arg in argv[1:]:
        pack_root = Path(arg).resolve()
        violations, n_rdf = court_pack(pack_root)
        status = "PASS" if not violations else "FAIL"
        print(f"[{status}] {pack_root.name}: {n_rdf} RDF file(s)")
        for v in violations:
            print(f"  {v['code']}: {v.get('file', '-')}: {v['detail']}")
            total_violations += 1
    if total_violations:
        print(f"DIALECT COURT: {total_violations} violation(s)")
        return 1
    print("DIALECT COURT: all packs conform")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
