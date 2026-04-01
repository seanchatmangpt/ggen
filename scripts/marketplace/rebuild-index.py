#!/usr/bin/env python3
"""
Rebuild marketplace registry index from package metadata.
Scans all package.toml files and generates marketplace/index.json
"""

import json
import re
import sys
from pathlib import Path


def extract_field(content: str, pattern: str, default: str = "") -> str:
    """Extract a field from TOML content using regex."""
    match = re.search(pattern, content, re.MULTILINE)
    if match:
        # Clean up quotes
        value = match.group(1).strip('"').strip("'")
        return value
    return default


def rebuild_index(packages_dir: str, index_file: str) -> int:
    """Rebuild the marketplace index from package.toml files."""
    packages = []

    # Process each package.toml
    for toml_path in Path(packages_dir).rglob("package.toml"):
        package_dir = toml_path.parent
        package_name = package_dir.name

        # Read TOML file
        with open(toml_path, 'r') as f:
            content = f.read()

        # Extract metadata
        pkg_id = extract_field(content, r'^name\s*=\s*[\'"](.+?)[\'"]', package_name)
        pkg_version = extract_field(content, r'^version\s*=\s*[\'"](.+?)[\'"]', "0.0.0")
        pkg_description = extract_field(content, r'^description\s*=\s*[\'"](.+?)[\'"]', "")
        pkg_author = extract_field(content, r'^author\s*=\s*[\'"](.+?)[\'"]', "")
        pkg_license = extract_field(content, r'^license\s*=\s*[\'"](.+?)[\'"]', "")
        pkg_category = extract_field(content, r'^category\s*=\s*[\'"](.+?)[\'"]', "")

        # Relative path from marketplace directory
        pkg_path = f"packages/{package_name}"

        # Build entry
        entry = {
            "id": pkg_id,
            "name": pkg_id,
            "version": pkg_version,
            "path": pkg_path
        }

        # Add optional fields
        if pkg_description:
            entry["description"] = pkg_description
        if pkg_author:
            entry["author"] = pkg_author
        if pkg_license:
            entry["license"] = pkg_license
        if pkg_category:
            entry["category"] = pkg_category

        packages.append(entry)
        print(f"✓ Processed: {pkg_id} v{pkg_version}")

    # Sort by id
    packages.sort(key=lambda x: x["id"])

    # Write JSON
    with open(index_file, 'w') as f:
        json.dump(packages, f, indent=2)

    return len(packages)


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <packages_dir> <index_file>", file=sys.stderr)
        sys.exit(1)

    packages_dir = sys.argv[1]
    index_file = sys.argv[2]

    count = rebuild_index(packages_dir, index_file)
    print(f"\nTotal packages indexed: {count}")
    print(f"Index file: {index_file}")
