#!/usr/bin/env python3
"""
Remove duplicate Result type aliases from test modules.
"""

import re
from pathlib import Path

def fix_file(filepath):
    """Remove duplicate Result type alias declarations."""
    content = filepath.read_text()
    original = content

    # Pattern to match the type Result line
    result_alias_line = r'^\s*type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;$'

    lines = content.split('\n')
    seen_result_alias = False
    new_lines = []

    in_test_module = False
    for line in lines:
        # Track if we're in a test module
        if re.match(r'#\[cfg\(test\)\]', line) or re.match(r'^\s*mod tests \{', line):
            in_test_module = True
            seen_result_alias = False  # Reset for each test module
        elif re.match(r'^\s*}\s*$', line) and in_test_module:
            in_test_module = False
            seen_result_alias = False

        # If it's a Result type alias line
        if in_test_module and re.match(result_alias_line, line):
            if seen_result_alias:
                # Skip duplicate
                continue
            else:
                seen_result_alias = True

        new_lines.append(line)

    new_content = '\n'.join(new_lines)

    if new_content != original:
        filepath.write_text(new_content)
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
