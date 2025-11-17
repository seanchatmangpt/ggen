#!/usr/bin/env python3
"""
Fix test modules to exclude Result from super::* import and remove type alias.
"""

import re
from pathlib import Path

def fix_file(filepath):
    """Fix test module imports."""
    content = filepath.read_text()
    original = content

    # Remove the type Result<T> = ... alias
    content = re.sub(
        r'\s+type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;',
        '',
        content
    )

    # Change "use super::*;" to import everything except Result
    # Pattern: Inside test module, find "use super::*;" and potentially add exclusion
    # This is tricky because we need to be selective

    # Better approach: Add "use super::{...}" that explicitly lists what we need
    # OR add "#[allow(unused_imports)] use super::*; use std::result::Result;" to override

    # Simplest: Just remove type alias and see if Ok(()) can infer correctly
    # The test! macro should provide enough context

    if content != original:
        filepath.write_text(content)
        print(f"✓ Fixed {filepath}")
        return True

    return False

def main():
    import subprocess

    result = subprocess.run(
        ["rg", "-l", "type Result<T> = std::result::Result", "crates/ggen-core/src/"],
        capture_output=True,
        text=True
    )
    files = [Path(line.strip()) for line in result.stdout.splitlines() if line.strip()]

    print(f"Found {len(files)} files with Result type alias")

    fixed_count = 0
    for filepath in files:
        if fix_file(filepath):
            fixed_count += 1

    print(f"\nFixed {fixed_count} files")

if __name__ == "__main__":
    main()
