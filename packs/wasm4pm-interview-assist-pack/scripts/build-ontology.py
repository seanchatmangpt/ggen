#!/usr/bin/env python3
"""TICKET-006: deterministic concatenation of the 9 hand-edited ontology/*.ttl
source files into the pack's root ontology.ttl, which crates/ggen-engine's
resolve_pack_dir (pack.rs:271-280) requires to exist as a single file.

ontology.ttl is a BUILD ARTIFACT. Never hand-edit it -- edit ontology/*.ttl
and re-run this script.

Prefix handling: each source file declares the same handful of @prefix/@base
lines. This script collects the union of prefix declarations (verifying no
two files bind the same prefix to different URIs -- a real collision would
be silently wrong Turtle if merged carelessly) and emits them once, followed
by every file's non-prefix content in the fixed corpus order.
"""
import re
import sys
from pathlib import Path

PACK_ROOT = Path(__file__).resolve().parent.parent

ONTOLOGY_FILES = [
    "ontology/00-document.ttl",
    "ontology/10-product.ttl",
    "ontology/20-requirements.ttl",
    "ontology/30-capabilities.ttl",
    "ontology/40-events-workflow.ttl",
    "ontology/50-policy.ttl",
    "ontology/60-provenance-receipts.ttl",
    "ontology/70-packs-datasets.ttl",
    "ontology/80-acceptance.ttl",
]

PREFIX_RE = re.compile(r"^@prefix\s+(\S+)\s+<([^>]+)>\s*\.\s*$")
BASE_RE = re.compile(r"^@base\s+<([^>]+)>\s*\.\s*$")


def main() -> int:
    prefixes: dict[str, str] = {}
    base: str | None = None
    body_chunks: list[str] = []

    for rel in ONTOLOGY_FILES:
        path = PACK_ROOT / rel
        if not path.is_file():
            print(f"ERROR: missing source file {rel}", file=sys.stderr)
            return 1

        body_lines = []
        for line in path.read_text().splitlines():
            m = PREFIX_RE.match(line)
            if m:
                prefix, uri = m.group(1), m.group(2)
                if prefix in prefixes and prefixes[prefix] != uri:
                    print(
                        f"ERROR: prefix collision on {prefix} — "
                        f"{prefixes[prefix]} vs {uri} (in {rel})",
                        file=sys.stderr,
                    )
                    return 1
                prefixes[prefix] = uri
                continue
            mb = BASE_RE.match(line)
            if mb:
                if base is not None and base != mb.group(1):
                    print(
                        f"ERROR: @base mismatch — {base} vs {mb.group(1)} (in {rel})",
                        file=sys.stderr,
                    )
                    return 1
                base = mb.group(1)
                continue
            body_lines.append(line)

        body_chunks.append(
            f"\n# ---- from {rel} ----\n" + "\n".join(body_lines).strip("\n") + "\n"
        )

    if base is None:
        print("ERROR: no @base declaration found in any source file", file=sys.stderr)
        return 1

    header = [f"@prefix {p} <{u}> ." for p, u in sorted(prefixes.items())]
    header.append(f"@base <{base}> .")

    out = (
        "# GENERATED FILE — do not hand-edit.\n"
        "# Built by scripts/build-ontology.py from ontology/*.ttl (TICKET-006).\n"
        "# Edit the source files under ontology/, then re-run the script.\n\n"
        + "\n".join(header)
        + "\n"
        + "".join(body_chunks)
    )

    out_path = PACK_ROOT / "ontology.ttl"
    out_path.write_text(out)
    print(f"wrote {out_path} ({len(out)} bytes, {len(prefixes)} prefixes)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
