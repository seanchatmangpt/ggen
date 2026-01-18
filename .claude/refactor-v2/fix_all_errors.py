#!/usr/bin/env python3
"""Fix all Error constructor calls in marketplace domain files"""

import re
from pathlib import Path

FILES = [
    "cli/src/domain/marketplace/list.rs",
    "cli/src/domain/marketplace/update.rs",
    "cli/src/domain/marketplace/publish.rs",
    "cli/src/domain/marketplace/search.rs",
]

def fix_io_error(content):
    """Fix IoError struct initialization"""
    # Pattern: Error::IoError { source: ..., path: ... }
    pattern = r'ggen_utils::error::Error::IoError\s*\{\s*source:\s*([^,]+),\s*path:\s*([^}]+)\s*\}'

    def replace(match):
        source = match.group(1).strip()
        path = match.group(2).strip()
        # Remove .to_string() if present
        if path.endswith('.to_string()'):
            path = path[:-12]
        if path.startswith('"') and path.endswith('"'):
            return f'ggen_utils::error::Error::with_context("IO error", {path})'
        return f'ggen_utils::error::Error::new("IO error")'

    return re.sub(pattern, replace, content, flags=re.DOTALL)

def fix_with_context_struct(content):
    """Fix with_context that still has struct syntax"""
    # Pattern: Error::with_context(\n            source: ...,\n            path: ...,\n        })
    pattern = r'ggen_utils::error::Error::with_context\(\s*source:\s*([^,]+),\s*path:\s*([^)]+)\s*\}\)'

    def replace(match):
        source = match.group(1).strip()
        path = match.group(2).strip()
        # Remove .to_string() if present
        if path.endswith('.to_string()'):
            path = path[:-12]
        if path.startswith('"') and path.endswith('"'):
            return f'ggen_utils::error::Error::with_context("IO error", {path})'
        return f'ggen_utils::error::Error::with_context("IO error", &{path})'

    return re.sub(pattern, replace, content, flags=re.DOTALL)

def fix_processing_error(content):
    """Fix ProcessingError struct initialization"""
    # Pattern: Error::ProcessingError { message: ..., context: ... }
    pattern = r'ggen_utils::error::Error::ProcessingError\s*\{\s*message:\s*([^,]+),\s*context:\s*([^}]+)\s*\}'

    def replace(match):
        message = match.group(1).strip()
        context = match.group(2).strip()
        # Remove .to_string() if present
        if context.endswith('.to_string()'):
            context = context[:-12]
        return f'ggen_utils::error::Error::with_context(&{message}, &{context})'

    return re.sub(pattern, replace, content, flags=re.DOTALL)

def main():
    for file_path in FILES:
        path = Path(file_path)
        if not path.exists():
            print(f"⚠️  Skipping {file_path} - file not found")
            continue

        print(f"Fixing {file_path}...")
        content = path.read_text()

        # Apply fixes
        content = fix_io_error(content)
        content = fix_processing_error(content)
        content = fix_with_context_struct(content)

        path.write_text(content)
        print(f"✓ Fixed {file_path}")

    print("\n✅ All files fixed!")

if __name__ == "__main__":
    main()
