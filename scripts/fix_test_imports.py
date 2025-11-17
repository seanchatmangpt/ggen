#!/usr/bin/env python3
"""
Fix missing chicago_tdd_tools imports in all test files.
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

def has_import(content):
    """Check if file already has chicago_tdd_tools import."""
    return bool(re.search(r'use\s+chicago_tdd_tools::', content))

def uses_async_test(content):
    """Check if file uses async_test! macro."""
    return bool(re.search(r'async_test!\s*\(', content))

def uses_test(content):
    """Check if file uses test! macro."""
    return bool(re.search(r'\btest!\s*\(', content))

def fix_file(filepath):
    """Add chicago_tdd_tools import to a file if missing."""
    content = filepath.read_text()

    if has_import(content):
        # Check if we need to add async_test to existing import
        has_test_macro = uses_test(content)
        has_async_macro = uses_async_test(content)

        # Check current import
        import_match = re.search(r'use\s+chicago_tdd_tools::([^;]+);', content)
        if import_match:
            imports = import_match.group(1).strip()

            # If it's just 'test' but needs async_test too
            if imports == 'test' and has_async_macro:
                new_content = re.sub(
                    r'use\s+chicago_tdd_tools::test;',
                    'use chicago_tdd_tools::{test, async_test};',
                    content
                )
                if new_content != content:
                    filepath.write_text(new_content)
                    print(f"✓ Updated import in {filepath}")
                    return True

        print(f"  Skipped {filepath} (already has import)")
        return False

    # Need to add import - find the right place
    has_test_macro = uses_test(content)
    has_async_macro = uses_async_test(content)

    if not (has_test_macro or has_async_macro):
        print(f"  Skipped {filepath} (no test macros found)")
        return False

    # Determine what to import
    if has_test_macro and has_async_macro:
        import_line = "use chicago_tdd_tools::{test, async_test};"
    elif has_async_macro:
        import_line = "use chicago_tdd_tools::async_test;"
    else:
        import_line = "use chicago_tdd_tools::test;"

    # Find #[cfg(test)] or mod tests pattern
    cfg_test_pattern = r'(#\[cfg\(test\)\]\s*(?:mod\s+\w+\s*\{|pub\s+mod\s+\w+\s*\{))\s*((?:use\s+[^;]+;\s*)*)'

    match = re.search(cfg_test_pattern, content)
    if match:
        # Insert after the mod declaration, with other use statements
        before = match.group(1)
        existing_uses = match.group(2)

        # Insert after the opening brace and any existing imports
        replacement = f"{before}\n    {import_line}\n{existing_uses}"
        new_content = content[:match.start()] + replacement + content[match.end():]

        filepath.write_text(new_content)
        print(f"✓ Added import to {filepath}")
        return True

    print(f"  Could not find test module in {filepath}")
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
