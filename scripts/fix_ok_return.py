#!/usr/bin/env python3
"""
Change Ok(()) to Ok::<(), Box<dyn std::error::Error>>(()) in tests.
"""

import re
import subprocess
from pathlib import Path

def get_test_files():
    """Get all files that use test! macros."""
    result = subprocess.run(
        ["rg", "-l", r"test!\(|async_test!\(", "crates/ggen-core/src/"],
        capture_output=True,
        text=True
    )
    return [Path(line.strip()) for line in result.stdout.splitlines() if line.strip()]

def fix_file(filepath):
    """Change Ok(()) to explicitly typed version in test blocks."""
    content = filepath.read_text()
    original = content

    # Find test! blocks and replace Ok(()) with explicitly typed version
    # This is tricky because we need to be in a test! block

    # Simpler: Just replace all bare Ok(()) with typed version in files that have test! macros
    # This might be too aggressive but let's try
    content = re.sub(
        r'\bOk\(\(\)\)',
        r'Ok::<(), Box<dyn std::error::Error>>(())',
        content
    )

    if content != original:
        filepath.write_text(content)
        print(f"✓ Fixed {filepath}")
        return True

    return False

def main():
    files = get_test_files()
    print(f"Found {len(files)} test files")

    fixed_count = 0
    for filepath in files:
        if fix_file(filepath):
            fixed_count += 1

    print(f"\nFixed {fixed_count} files")

if __name__ == "__main__":
    main()
