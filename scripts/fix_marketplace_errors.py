#!/usr/bin/env python3
"""
Batch fix all 35 compilation errors in ggen-marketplace
80/20 approach: systematic pattern replacement
"""

import re
import subprocess
from pathlib import Path
from typing import List, Tuple

def run_cargo_and_extract_errors():
    """Run cargo build and extract error details"""
    result = subprocess.run(
        ["cargo", "build", "--package", "ggen-marketplace"],
        cwd="/Users/sac/ggen",
        capture_output=True,
        text=True
    )
    return result.stderr

def parse_error_location(error_text: str) -> List[Tuple[str, int, str, str]]:
    """Parse error locations from cargo output"""
    errors = []
    lines = error_text.split('\n')

    i = 0
    while i < len(lines):
        line = lines[i]

        # Match error type and location
        if 'error[E0061]' in line or 'error[E0308]' in line:
            error_type = 'E0061' if 'E0061' in line else 'E0308'

            # Find the --> line with file location
            for j in range(i, min(i+10, len(lines))):
                if '-->' in lines[j]:
                    match = re.search(r'-->\s+(.+?):(\d+)', lines[j])
                    if match:
                        file_path = match.group(1)
                        line_num = int(match.group(2))

                        # Get context
                        context = '\n'.join(lines[i:min(i+15, len(lines))])

                        errors.append((file_path, line_num, error_type, context))
                    break
        i += 1

    return errors

def fix_io_error_calls(file_path: str, content: str) -> str:
    """Fix io_error calls - change 2nd arg from &str to std::io::Error"""

    # Pattern: MarketplaceError::io_error(e.to_string(), path.to_string_lossy().as_ref())
    # Fix: MarketplaceError::io_error(operation_desc, e)

    # Fix backend/local.rs line 49
    content = re.sub(
        r'MarketplaceError::io_error\(e\.to_string\(\),\s+db_path\.to_string_lossy\(\)\.as_ref\(\)\)',
        'MarketplaceError::io_error("open database", e)',
        content
    )

    # Fix backend/local.rs line 74
    content = re.sub(
        r'MarketplaceError::io_error\(e\.to_string\(\),\s+index_path\.to_string_lossy\(\)\.as_ref\(\)\)',
        'MarketplaceError::io_error("read index", e)',
        content
    )

    # Fix backend/local.rs line 94
    content = re.sub(
        r'MarketplaceError::io_error\("write_package"\.to_string\(\),\s+e\.to_string\(\)\)',
        'MarketplaceError::io_error("write package", e)',
        content
    )

    # Fix backend/local.rs line 106
    content = re.sub(
        r'MarketplaceError::io_error\("update_index"\.to_string\(\),\s+e\.to_string\(\)\)',
        'MarketplaceError::io_error("update index", e)',
        content
    )

    # Generic pattern for other io_error calls with string second arg
    content = re.sub(
        r'MarketplaceError::io_error\(([^,]+),\s+e\.to_string\(\)\)',
        r'MarketplaceError::io_error(\1, e)',
        content
    )

    return content

def fix_search_call_arity(file_path: str, content: str) -> str:
    """Fix search() calls - remove limit parameter"""

    # Pattern: .search(&query, limit)
    # Fix: .search(&query)
    content = re.sub(
        r'\.search\(&query,\s+\d+\)',
        '.search(&query)',
        content
    )

    # Pattern: searcher.search(&collector, &query)
    # Fix: searcher.search(&query)
    content = re.sub(
        r'searcher\.search\(&collector,\s+&query\)',
        'searcher.search(&query)',
        content
    )

    return content

def fix_registry_backend_calls(file_path: str, content: str) -> str:
    """Fix RegistryBackend method calls - remove extra parameters"""

    # Pattern: backend.fetch_packages(query, limit)
    # Fix: backend.fetch_packages(query)
    content = re.sub(
        r'backend\.fetch_packages\(([^,]+),\s+[^)]+\)',
        r'backend.fetch_packages(\1)',
        content
    )

    # Pattern: backend.search_packages(query, limit)
    # Fix: backend.search_packages(query)
    content = re.sub(
        r'backend\.search_packages\(([^,]+),\s+[^)]+\)',
        r'backend.search_packages(\1)',
        content
    )

    return content

def fix_facet_type_mismatch(file_path: str, content: str) -> str:
    """Fix Facet type mismatches - convert types::Facet to tantivy::schema::Facet"""

    # Add import if not present
    if 'use tantivy::schema::Facet as TantivyFacet;' not in content:
        # Find the import section
        import_match = re.search(r'(use tantivy::\{[^}]+\};)', content)
        if import_match:
            old_import = import_match.group(1)
            new_import = old_import.replace('};', ', schema::Facet as TantivyFacet};')
            content = content.replace(old_import, new_import)

    # Fix facet collection conversion
    # Pattern: Ok(facets) where facets is Vec<types::Facet>
    # Need to convert to Vec<tantivy::schema::Facet>

    # Find the facet collection code
    content = re.sub(
        r'let facets: Vec<Facet> = facet_counts',
        r'let facets: Vec<TantivyFacet> = facet_counts',
        content
    )

    # Fix Facet construction
    content = re.sub(
        r'Facet\s*\{([^}]+)\}',
        r'TantivyFacet::from(\1)',
        content
    )

    return content

def apply_fixes_to_file(file_path: str) -> bool:
    """Apply all fixes to a file"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()

        original = content

        # Apply all fix patterns
        content = fix_io_error_calls(file_path, content)
        content = fix_search_call_arity(file_path, content)
        content = fix_registry_backend_calls(file_path, content)
        content = fix_facet_type_mismatch(file_path, content)

        # Only write if changed
        if content != original:
            with open(file_path, 'w') as f:
                f.write(content)
            return True
        return False

    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    print("🔧 Extracting compilation errors...")
    error_output = run_cargo_and_extract_errors()

    errors = parse_error_location(error_output)
    print(f"📊 Found {len(errors)} error locations")

    # Group by file
    files_to_fix = {}
    for file_path, line_num, error_type, context in errors:
        if file_path not in files_to_fix:
            files_to_fix[file_path] = []
        files_to_fix[file_path].append((line_num, error_type, context))

    print(f"\n📁 Files to fix: {len(files_to_fix)}")
    for file_path in sorted(files_to_fix.keys()):
        print(f"  - {file_path}: {len(files_to_fix[file_path])} errors")

    # Apply fixes
    print("\n🔨 Applying fixes...")
    fixed_count = 0
    for file_path in files_to_fix.keys():
        full_path = f"/Users/sac/ggen/{file_path}"
        if Path(full_path).exists():
            if apply_fixes_to_file(full_path):
                fixed_count += 1
                print(f"  ✅ Fixed {file_path}")
        else:
            print(f"  ⚠️  File not found: {full_path}")

    print(f"\n✨ Modified {fixed_count} files")

    # Verify
    print("\n🔍 Verifying fixes...")
    result = subprocess.run(
        ["cargo", "build", "--package", "ggen-marketplace"],
        cwd="/Users/sac/ggen",
        capture_output=True,
        text=True
    )

    error_count = result.stderr.count('error[E')
    print(f"\n📈 Remaining errors: {error_count}")

    if error_count == 0:
        print("✅ SUCCESS! All errors fixed!")
    else:
        print(f"⚠️  Still have {error_count} errors to fix")
        print("\nRemaining errors:")
        for line in result.stderr.split('\n'):
            if 'error[E' in line:
                print(f"  {line}")

if __name__ == "__main__":
    main()
