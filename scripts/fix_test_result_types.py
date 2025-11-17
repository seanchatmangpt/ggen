#!/usr/bin/env python3
"""
Fix Result type issues in test modules by removing ggen_utils imports
that shadow std::result::Result.
"""

import re
import subprocess
from pathlib import Path

def get_test_files():
    """Get all files that use test! or async_test! macros."""
    result = subprocess.run(
        ["rg", "-l", r"test!\(|async_test!\(", "crates/ggen-core/src/"],
        capture_output=True,
        text=True
    )
    return [Path(line.strip()) for line in result.stdout.splitlines() if line.strip()]

def fix_file(filepath):
    """Remove problematic imports and .map_err calls from test module."""
    content = filepath.read_text()
    original = content

    # Remove imports that shadow std::result::Result in test modules
    # Pattern: Inside #[cfg(test)] mod, remove "use ggen_utils::error::Result;"
    content = re.sub(
        r'(\s+)use ggen_utils::error::Result;\n',
        '',
        content
    )

    # Remove standalone "use ggen_utils::error::Error;" in test modules
    content = re.sub(
        r'(\s+)use ggen_utils::error::Error;\n',
        '',
        content
    )

    # Remove .map_err(Error::from) calls since the ? operator will handle conversion
    content = re.sub(
        r'\.map_err\(Error::from\)\?',
        '?',
        content
    )

    # Remove .map_err(|e| Error::new(e))? calls
    content = re.sub(
        r'\.map_err\(\|e\| Error::new\(e\)\)\?',
        '?',
        content
    )

    # Remove .map_err(|e| ggen_utils::Error::new(e))? calls
    content = re.sub(
        r'\.map_err\(\|e\| ggen_utils::Error::new\(e\)\)\?',
        '?',
        content
    )

    if content != original:
        filepath.write_text(content)
        print(f"✓ Fixed {filepath}")
        return True

    return False

def main():
    files = get_test_files()
    print(f"Found {len(files)} files with test macros")

    fixed_count = 0
    for filepath in sorted(files):
        if fix_file(filepath):
            fixed_count += 1

    print(f"\nFixed {fixed_count} files")

if __name__ == "__main__":
    main()
