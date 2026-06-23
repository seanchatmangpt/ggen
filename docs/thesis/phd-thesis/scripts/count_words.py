#!/usr/bin/env python3
"""Word count utility for LaTeX thesis."""

import os
import re
import sys
from pathlib import Path


def count_latex_words(file_path):
    """Count words in a LaTeX file, excluding commands and comments."""
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()

    # Remove comments
    content = re.sub(r'%.*$', '', content, flags=re.MULTILINE)

    # Remove LaTeX commands but keep content
    # Remove \begin{...}...\end{...} blocks (keep content for some environments)
    content = re.sub(r'\\(section|subsection|subsubsection|chapter|part)\*?\{[^}]*\}', '', content)
    content = re.sub(r'\\(cite|ref|eqref|label|cref)\{[^}]*\}', '', content)

    # Remove remaining commands
    content = re.sub(r'\\[a-zA-Z]+(\[.*?\])?(\{.*?\})?', '', content)

    # Remove special characters
    content = re.sub(r'[{}$&#_^~\\]', ' ', content)

    # Split into words
    words = content.split()

    return len(words)


def count_directory(directory, pattern='*.tex'):
    """Count words in all matching files in directory."""
    total = 0
    files = list(Path(directory).rglob(pattern))

    for file_path in sorted(files):
        count = count_latex_words(file_path)
        print(f"{file_path}: {count:,} words")
        total += count

    return total, len(files)


def main():
    if len(sys.argv) < 2:
        print("Usage: count_words.py <directory> [...]")
        sys.exit(1)

    total_words = 0
    total_files = 0

    for directory in sys.argv[1:]:
        if not os.path.isdir(directory):
            print(f"Warning: {directory} is not a directory, skipping")
            continue

        print(f"\n{directory}:")
        words, files = count_directory(directory)
        total_words += words
        total_files += files
        print(f"Subtotal: {words:,} words in {files} files\n")

    print(f"Total: {total_words:,} words in {total_files} files")


if __name__ == '__main__':
    main()
