#!/usr/bin/env python3
"""Close pack-inventory drift between packs/*/pack.toml and .specify/repo-facts.ttl's
rf:Pack individuals (condition 22 of docs/PUBLICATION_JUDGMENT.md).

Prior fixes to this exact gap (see gen:G4 in .specify/generations.ttl) were a manual,
one-off transcription pass: `ls packs/*/pack.toml | wc -l` vs `grep -c 'a rf:Pack'`,
then hand-writing each missing individual. That doesn't scale (the gap grew from 4
missing packs in 2026-07-20 to 53 by 2026-08-07) and isn't real 80/20 -- this script
mechanizes the same, honest discipline instead: walk every real pack.toml, read its
real name/version/description fields verbatim (no fabrication, no invented prose),
diff against the individuals already in repo-facts.ttl, and append exactly the
missing ones as real Turtle individuals in the same shape as the existing entries.

Existing individuals (and their hand-curated, deeper `rf:description` prose) are
never touched or overwritten -- this only appends what's missing. Idempotent: a
second run against an up-to-date repo-facts.ttl finds zero missing packs.

Usage:
    python3 scripts/audit/sync_pack_inventory.py [--dry-run]
"""

from __future__ import annotations

import argparse
import re
import sys
import tomllib
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
PACKS_DIR = REPO_ROOT / "packs"
REPO_FACTS_PATH = REPO_ROOT / ".specify" / "repo-facts.ttl"

INDIVIDUAL_RE = re.compile(r"rf:pack_(\w+)\s+a\s+rf:Pack\s*;")
ORDER_RE = re.compile(r'rf:order\s+"(\d+)"')


def normalize_dir_to_individual_name(dir_name: str) -> str:
    """packs/dspy-pack -> pack_dspy_pack (matches this file's existing naming
    convention: hyphens -> underscores, prefixed with pack_)."""
    return "pack_" + dir_name.replace("-", "_")


def escape_ttl_string(value: str) -> str:
    """Escape a value for a TTL double-quoted string literal. Turtle forbids raw
    line breaks inside a plain "..." literal -- several pack.toml descriptions
    are multi-line, so newlines must become the \\n escape, not literal jumps."""
    return (
        value.replace("\\", "\\\\")
        .replace('"', '\\"')
        .replace("\r\n", "\\n")
        .replace("\n", "\\n")
        .replace("\r", "\\n")
    )


def load_registered_individuals(text: str) -> set[str]:
    return {f"pack_{m.group(1)}" for m in INDIVIDUAL_RE.finditer(text)}


def max_order(text: str) -> int:
    orders = [int(m.group(1)) for m in ORDER_RE.finditer(text)]
    return max(orders) if orders else 0


def load_pack_toml(pack_dir: Path) -> dict | None:
    toml_path = pack_dir / "pack.toml"
    if not toml_path.exists():
        return None
    with toml_path.open("rb") as f:
        data = tomllib.load(f)
    pack = data.get("pack", {})
    return {
        "name": pack.get("name", pack_dir.name),
        "version": pack.get("version", "0.0.0"),
        "description": pack.get("description", ""),
    }


def build_individual_ttl(order: int, dir_name: str, pack: dict) -> str:
    individual_name = normalize_dir_to_individual_name(dir_name)
    description = escape_ttl_string(pack["description"])
    version = escape_ttl_string(pack["version"])
    return (
        f'rf:{individual_name} a rf:Pack ;\n'
        f'    rf:order "{order:03d}" ;\n'
        f'    rf:dir "{dir_name}" ;\n'
        f'    rf:version "{version}" ;\n'
        f'    rf:description "{description} '
        f'(mechanically transcribed verbatim from packs/{dir_name}/pack.toml by '
        f'scripts/audit/sync_pack_inventory.py, 2026-08-07 condition-22 gap closure -- '
        f'not independently hand-analyzed like the earlier-registered packs above)." .\n'
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dry-run", action="store_true", help="report only, don't write")
    args = parser.parse_args()

    text = REPO_FACTS_PATH.read_text()
    registered = load_registered_individuals(text)
    next_order = max_order(text) + 1

    pack_dirs = sorted(p for p in PACKS_DIR.iterdir() if p.is_dir())
    missing: list[tuple[str, dict]] = []
    for pack_dir in pack_dirs:
        pack = load_pack_toml(pack_dir)
        if pack is None:
            print(f"WARN: {pack_dir.name} has no pack.toml, skipping", file=sys.stderr)
            continue
        individual_name = normalize_dir_to_individual_name(pack_dir.name)
        if individual_name not in registered:
            missing.append((pack_dir.name, pack))

    print(f"PACK_INVENTORY total_dirs={len(pack_dirs)} registered={len(registered)} missing={len(missing)}")

    if not missing:
        print("PACK_INVENTORY_SYNCED: nothing to do")
        return 0

    new_individuals = []
    order = next_order
    for dir_name, pack in missing:
        new_individuals.append(build_individual_ttl(order, dir_name, pack))
        order += 1

    block = (
        "\n# --- Appended by scripts/audit/sync_pack_inventory.py (2026-08-07, condition-22 "
        f"gap closure): {len(missing)} pack directories on disk with no rf:Pack individual. "
        "Descriptions below are mechanically transcribed verbatim from each pack's own "
        "pack.toml, not independently hand-analyzed like the earlier entries above. ---\n"
        + "\n".join(new_individuals)
    )

    for dir_name, _ in missing:
        print(f"  + {dir_name}")

    if args.dry_run:
        print("DRY_RUN: no file written")
        return 0

    with REPO_FACTS_PATH.open("a") as f:
        f.write(block)

    print(f"PACK_INVENTORY_WRITTEN: appended {len(missing)} individuals to {REPO_FACTS_PATH}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
