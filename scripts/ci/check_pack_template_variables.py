#!/usr/bin/env python3
"""check_pack_template_variables — declared-vs-used template variable check.

For every surface named on the command line (default: all packs), compare:
  used     — the row-context keys a pack template's `for_each` SPARQL query
             binds (the SELECT variable names), which are exactly the
             top-level variables the template body may reference during
             fan-out rendering (ggen-engine sync.rs row_context), and
  declared — the `variables` list the pack (or its marketplace registry
             entry) declares for that template.

Prints a JSON array; each element carries the symmetric difference for one
template surface. An empty array means every declared variable is used and
every used variable is declared — no drift, no undeclared template access.

Exit codes: 0 always (report-only; the JSON is the evidence). Pipe through
`jq -e 'length == 0'` (or read the array) where a gate needs a verdict.
"""
from __future__ import annotations

import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent.parent
PACKS = ROOT / "packs"
MARKETPLACE_PACKS = ROOT / "marketplace" / "packs"


def frontmatter(text: str) -> str:
    if text.startswith("---"):
        end = text.find("\n---", 3)
        if end != -1:
            return text[3:end]
    return ""


def used_variables(fm: str) -> list[str]:
    """Row-context keys bound by the for_each query's SELECT variables."""
    m = re.search(r"^for_each:\s*(\w+)\s*$", fm, re.M)
    if not m:
        return []
    query_name = m.group(1)
    # the named query block: `<name>: |` followed by indented lines
    qm = re.search(rf"^\s*{query_name}:\s*\|.*?\n((?:[ \t]+[^\n]*\n?)*)", fm, re.M)
    if not qm:
        return []
    return re.findall(r"\?(\w+)", qm.group(1))


def declared_variables(entry_text: str, template_file: str) -> list[str]:
    """`variables` list for one template from a marketplace registry entry."""
    for block in re.split(r"\[\[pack\.templates\]\]", entry_text)[1:]:
        if template_file in block:
            m = re.search(r"variables\s*=\s*\[(.*?)\]", block, re.S)
            if m:
                return re.findall(r'"([^"]+)"', m.group(1))
    return []


def main() -> int:
    names = sys.argv[1:]
    mismatches: list[dict] = []

    for pack_dir in sorted(PACKS.iterdir()) if not names else [PACKS / n for n in names]:
        if not pack_dir.is_dir():
            continue
        entry_path = MARKETPLACE_PACKS / (pack_dir.name.removesuffix("-pack") + ".toml")
        entry_text = entry_path.read_text() if entry_path.exists() else ""
        for tpl in sorted((pack_dir / "templates").glob("*.tmpl")):
            used = used_variables(frontmatter(tpl.read_text()))
            declared = declared_variables(entry_text, tpl.name)
            if not declared and not used:
                continue
            undeclared = [v for v in used if v not in declared]
            unused = [v for v in declared if v not in used]
            if undeclared or unused:
                mismatches.append(
                    {
                        "surface": f"{pack_dir.name}/{tpl.name}",
                        "undeclared_used": undeclared,
                        "unused_declared": unused,
                    }
                )

    print(json.dumps(mismatches, indent=2))
    return 0


if __name__ == "__main__":
    sys.exit(main())
