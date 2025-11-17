#!/usr/bin/env python3
"""
Fix Result type in test modules by adding a type alias that matches what test! macro expects.
"""

import re
import subprocess
from pathlib import Path

def get_problematic_files():
    """Get files that have top-level Result import and also use test! macros."""
    result = subprocess.run(
        ["rg", "-l", "^use ggen_utils::error.*Result", "crates/ggen-core/src/"],
        capture_output=True,
        text=True
    )
    files_with_result = set(Path(line.strip()) for line in result.stdout.splitlines() if line.strip())

    result = subprocess.run(
        ["rg", "-l", r"test!\(|async_test!\(", "crates/ggen-core/src/"],
        capture_output=True,
        text=True
    )
    files_with_tests = set(Path(line.strip()) for line in result.stdout.splitlines() if line.strip())

    return sorted(files_with_result & files_with_tests)

def fix_file(filepath):
    """Add Result type alias to test module."""
    content = filepath.read_text()
    original = content

    # Pattern: Find test module with chicago_tdd_tools import
    # Add type alias after the import
    pattern = r'(#\[cfg\(test\)\]\s*(?:pub\s+)?mod\s+\w+\s*\{[^\}]*?use\s+chicago_tdd_tools::[^;]+;)'

    def add_type_alias(match):
        existing = match.group(1)
        # Check if already has Result type alias
        if 'type Result<T>' in existing:
            return existing
        # Add the type alias
        return existing + '\n    type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;'

    new_content = re.sub(pattern, add_type_alias, content, flags=re.DOTALL)

    if new_content != original:
        filepath.write_text(new_content)
        print(f"✓ Fixed {filepath}")
        return True

    return False

def main():
    files = get_problematic_files()
    print(f"Found {len(files)} files needing Result type alias")

    fixed_count = 0
    for filepath in files:
        if fix_file(filepath):
            fixed_count += 1

    print(f"\nFixed {fixed_count} files")

if __name__ == "__main__":
    main()
