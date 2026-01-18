#!/usr/bin/env python3
"""Bulk fix unwrap()/expect() violations in production Rust code"""

import re
import os
from pathlib import Path

class UnwrapFixer:
    """Fixes common unwrap() patterns in production code"""

    def __init__(self):
        self.patterns = [
            # Mutex locks
            (
                re.compile(r'\.lock\(\)\.unwrap\(\)'),
                '.lock().expect("Mutex lock failed (poisoned)")',
                'mutex_lock'
            ),
            # Regex compilation
            (
                re.compile(r'Regex::new\(([^)]+)\)\.unwrap\(\)'),
                r'Regex::new(\1).expect("Invalid regex pattern")',
                'regex_new'
            ),
            # NonZeroUsize
            (
                re.compile(r'NonZeroUsize::new\((\d+)\)\.unwrap\(\)'),
                r'NonZeroUsize::new(\1).expect("\1 is non-zero")',
                'nonzero'
            ),
            # SystemTime
            (
                re.compile(r'SystemTime::now\(\)\.duration_since\(UNIX_EPOCH\)\.unwrap\(\)'),
                'SystemTime::now().duration_since(UNIX_EPOCH).expect("System time before UNIX epoch")',
                'systemtime'
            ),
            # partial_cmp
            (
                re.compile(r'\.partial_cmp\(([^)]+)\)\.unwrap\(\)'),
                r'.partial_cmp(\1).expect("NaN in comparison")',
                'partial_cmp'
            ),
        ]

    def should_process(self, filepath):
        """Check if file should be processed"""
        path_str = str(filepath)

        # Only .rs files
        if not path_str.endswith('.rs'):
            return False

        # Skip tests/benches/examples
        skip_patterns = ['/tests/', '/benches/', '/examples/', 'tests-archive', '/target/']
        if any(pattern in path_str for pattern in skip_patterns):
            return False

        # Only process src/ directories
        if '/src/' not in path_str:
            return False

        return True

    def fix_file(self, filepath):
        """Apply all fixes to a file"""
        try:
            with open(filepath, 'r', encoding='utf-8') as f:
                content = f.read()
        except Exception as e:
            return 0, f"Error reading: {e}"

        # Skip if in test module
        if '#[cfg(test)]' in content:
            return 0, "Skipped (test module)"

        original = content
        fixes_applied = []

        # Apply all patterns
        for pattern, replacement, name in self.patterns:
            new_content = pattern.sub(replacement, content)
            if new_content != content:
                fixes_applied.append(name)
                content = new_content

        if content != original:
            # Write back
            try:
                with open(filepath, 'w', encoding='utf-8') as f:
                    f.write(content)
                return len(fixes_applied), ', '.join(fixes_applied)
            except Exception as e:
                return 0, f"Error writing: {e}"

        return 0, "No changes"

    def process_directory(self, root_dir):
        """Process all eligible files in directory tree"""
        root = Path(root_dir)
        total_files = 0
        fixed_files = 0
        total_fixes = 0

        print("🔧 Bulk Unwrap/Expect Fixer")
        print("=" * 60)
        print()

        for rs_file in root.rglob('*.rs'):
            if not self.should_process(rs_file):
                continue

            total_files += 1
            count, details = self.fix_file(rs_file)

            if count > 0:
                fixed_files += 1
                total_fixes += count
                rel_path = rs_file.relative_to(root)
                print(f"✅ {rel_path}: {count} fix(es) [{details}]")

        print()
        print("=" * 60)
        print("Summary:")
        print(f"  Files scanned: {total_files}")
        print(f"  Files fixed:   {fixed_files}")
        print(f"  Total fixes:   {total_fixes}")
        print()
        print("Next steps:")
        print("  1. Run: cargo make check")
        print("  2. Run: cargo make test")
        print("  3. Review: git diff")
        print()

if __name__ == '__main__':
    fixer = UnwrapFixer()
    fixer.process_directory('.')
