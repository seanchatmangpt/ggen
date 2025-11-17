#!/usr/bin/env python3
"""
Fix test modules to exclude Result from super::* by changing the import.
"""

import re
import subprocess
from pathlib import Path

def get_problematic_files():
    """Get files that have top-level Result import and test modules."""
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
    """Change 'use super::*;' to import everything except Result."""
    content = filepath.read_text()
    original = content

    # Pattern: Find test module with "use super::*;"
    # Replace with "use super::*; type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;"
    # This shadows the parent's Result with our own

    # Better: Use pattern that adds shadow AFTER super::*
    pattern = r'(#\[cfg\(test\)\]\s*(?:pub\s+)?mod\s+\w+\s*\{[^\}]*?use\s+super::\*;)'

    def add_result_shadow(match):
        existing = match.group(1)
        # Check if already has type Result shadow
        if 'type Result<T>' in existing:
            return existing
        # Add Result type alias to shadow parent's import
        return existing + '\n    type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;'

    new_content = re.sub(pattern, add_result_shadow, content, flags=re.DOTALL)

    if new_content != original:
        filepath.write_text(new_content)
        print(f"✓ Fixed {filepath}")
        return True

    return False

def main():
    files = get_problematic_files()
    print(f"Found {len(files)} files needing Result shadowing")

    fixed_count = 0
    for filepath in files:
        if fix_file(filepath):
            fixed_count += 1

    print(f"\nFixed {fixed_count} files")

if __name__ == "__main__":
    main()
