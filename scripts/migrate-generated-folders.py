#!/usr/bin/env python3
"""
Migrate ggen.toml from output_dir="generated" to output_dir="."
Eliminates generated/ folders — generated code IS the code.

Usage:
    python3 scripts/migrate-generated-folders.py --dry-run
    python3 scripts/migrate-generated-folders.py
    python3 scripts/migrate-generated-folders.py --path /Users/sac/ggen/examples/observable-agent
"""

import re
import sys
from pathlib import Path
import argparse
from typing import Literal

MigrationMode = Literal["direct", "remove_prefix", "manual"]


def detect_mode(toml_path: Path) -> tuple[MigrationMode, str]:
    """Detect appropriate migration mode based on ggen.toml patterns."""
    content = toml_path.read_text()

    # Pattern A: output_dir = "generated" → direct placement
    if 'output_dir = "generated"' in content:
        return "direct", "Simple migration: generated → ."

    # Pattern B: output_dir = "src/generated" → src/
    if 'output_dir = "src/generated"' in content:
        return "direct", "Nested migration: src/generated → src/"

    # Pattern C: output_dir = "." but has generated/ in output_file paths
    if 'output_dir = "."' in content and 'output_file = "generated/' in content:
        return "remove_prefix", "Remove 'generated/' prefix from output_file paths"

    # Pattern D: output_dir = "world" (factory-paas special case)
    if 'output_dir = "world"' in content:
        return "manual", "Special case: factory-paas world/ concept"

    # Already using "." correctly with no generated/ prefix
    if 'output_dir = "."' in content:
        return "manual", "Already migrated or custom pattern"

    # Any other output_dir
    match = re.search(r'output_dir = "([^"]+)"', content)
    output_dir = match.group(1) if match else "unknown"
    return "manual", f"Custom output_dir: {output_dir}"


def migrate_toml(toml_path: Path, mode: MigrationMode, dry_run: bool = True) -> int:
    """Migrate a single ggen.toml file. Returns number of changes made."""
    content = toml_path.read_text()
    original = content
    changes = 0

    if mode == "direct":
        # Rule 1: output_dir = "generated" → output_dir = "."
        if 'output_dir = "generated"' in content:
            content = content.replace('output_dir = "generated"', 'output_dir = "."')
            changes += 1

        # Rule 2: output_dir = "src/generated" → output_dir = "src"
        if 'output_dir = "src/generated"' in content:
            content = content.replace('output_dir = "src/generated"', 'output_dir = "src"')
            changes += 1

    elif mode == "remove_prefix":
        # Remove "generated/" prefix from output_file paths
        new_content = re.sub(r'output_file = "generated/', 'output_file = "', content)
        if new_content != content:
            content = new_content
            changes = content.count('output_file = "') - original.count('output_file = "')
            changes = max(changes, 1)

    if changes > 0:
        if dry_run:
            print(f"  [DRY RUN] Would make {changes} change(s)")
        else:
            # Backup
            backup = toml_path.with_suffix('.toml.pre-migration')
            if not backup.exists():
                backup.write_text(original)

            # Write migrated version
            toml_path.write_text(content)
            print(f"  Migrated ({changes} change(s))")

    return changes


def main():
    parser = argparse.ArgumentParser(
        description="Migrate ggen.toml to eliminate generated/ folders",
    )
    parser.add_argument(
        "--dry-run", action="store_true", help="Show changes without applying"
    )
    parser.add_argument(
        "--path",
        type=Path,
        default=Path("/Users/sac/ggen/examples"),
        help="Root path to search for ggen.toml files",
    )
    args = parser.parse_args()

    toml_files = sorted(args.path.rglob("ggen.toml"))

    if not toml_files:
        print(f"No ggen.toml files found in {args.path}")
        sys.exit(1)

    print(f"Found {len(toml_files)} ggen.toml files\n")

    auto_count = 0
    manual_count = 0
    total_changes = 0

    for toml_path in toml_files:
        rel_path = toml_path.relative_to(args.path)
        mode, rationale = detect_mode(toml_path)

        if mode in ("direct", "remove_prefix"):
            print(f"{rel_path}: {mode}")
            print(f"  → {rationale}")
            changes = migrate_toml(toml_path, mode, dry_run=args.dry_run)
            auto_count += 1
            total_changes += changes
        else:
            print(f"{rel_path}: MANUAL REVIEW")
            print(f"  → {rationale}")
            manual_count += 1

    print(f"\n{'='*60}")
    print(f"Summary:")
    print(f"  Auto-migrated: {auto_count} files")
    print(f"  Manual review: {manual_count} files")
    print(f"  Total changes:  {total_changes}")

    if args.dry_run and total_changes > 0:
        print(f"\nRun without --dry-run to apply changes.")


if __name__ == "__main__":
    main()
