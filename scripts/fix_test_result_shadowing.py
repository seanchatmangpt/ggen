#!/usr/bin/env python3
"""
Fix Result type shadowing in test modules by adding explicit std::result::Result import.
"""

import re
import subprocess
from pathlib import Path

def get_problematic_files():
    """Get files that have top-level Result import and also use test! macros."""
    # Get files with top-level Result import
    result = subprocess.run(
        ["rg", "-l", "^use ggen_utils::error.*Result", "crates/ggen-core/src/"],
        capture_output=True,
        text=True
    )
    files_with_result = set(Path(line.strip()) for line in result.stdout.splitlines() if line.strip())

    # Get files with test macros
    result = subprocess.run(
        ["rg", "-l", r"test!\(|async_test!\(", "crates/ggen-core/src/"],
        capture_output=True,
        text=True
    )
    files_with_tests = set(Path(line.strip()) for line in result.stdout.splitlines() if line.strip())

    # Return intersection
    return sorted(files_with_result & files_with_tests)

def fix_file(filepath):
    """Add std::result::Result import to test module to override top-level import."""
    content = filepath.read_text()
    original = content

    # Pattern to find #[cfg(test)] mod tests { ... use chicago_tdd_tools::test;
    # We want to add "use std::result::Result;" right after chicago_tdd_tools import

    # Find test module start
    test_mod_pattern = r'(#\[cfg\(test\)\]\s*(?:pub\s+)?mod\s+\w+\s*\{[^\}]*?use\s+chicago_tdd_tools::[^;]+;)'

    def add_result_import(match):
        existing = match.group(1)
        # Check if already has std::result::Result import
        if 'std::result::Result' in existing or 'std::result::Result as' in existing:
            return existing
        # Add the import
        return existing + '\n    use std::result::Result;'

    new_content = re.sub(test_mod_pattern, add_result_import, content, flags=re.DOTALL)

    if new_content != original:
        filepath.write_text(new_content)
        print(f"✓ Fixed {filepath}")
        return True

    return False

def main():
    files = get_problematic_files()
    print(f"Found {len(files)} files with Result shadowing issues")

    fixed_count = 0
    for filepath in files:
        if fix_file(filepath):
            fixed_count += 1

    print(f"\nFixed {fixed_count} files")

if __name__ == "__main__":
    main()
