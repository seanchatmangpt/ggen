#!/usr/bin/env python3
"""Span-safe access to book:Chapter sourceText literals in pack ontology.

The level-five book ontology contains large Turtle long-string literals. A
single cross-record regex is unsafe because Markdown and code listings can
contain quote-like material. This module anchors on chapter subjects and the
chapter's final book:hasListing predicate, then selects the final *unescaped*
triple-quote delimiter before that predicate as the sourceText terminator.
"""

from __future__ import annotations

from dataclasses import dataclass
import re
from typing import Mapping

CHAPTER_START_RE = re.compile(
    r"(?m)^book:chapter-[^\s]+\s+a\s+book:Chapter\s*;"
)
SOURCE_PATH_RE = re.compile(r'(?m)^\s*book:sourcePath\s+"([^"]+)"\s*;')
SOURCE_OPEN_RE = re.compile(r'(?m)^\s*book:sourceText\s+"""')
HAS_LISTING_RE = re.compile(
    r"(?m)^\s*book:hasListing\s+book:[^\s]+\s*\.\s*$"
)
TRIPLE_QUOTE_RE = re.compile(r'"""')


@dataclass(frozen=True)
class ChapterRecord:
    subject_start: int
    record_end: int
    path: str
    source_start: int
    source_end: int


def is_escaped(text: str, index: int) -> bool:
    backslashes = 0
    cursor = index - 1
    while cursor >= 0 and text[cursor] == "\\":
        backslashes += 1
        cursor -= 1
    return backslashes % 2 == 1


def closing_delimiter(ontology: str, source_start: int, listing_start: int) -> int:
    candidates = [
        match.start()
        for match in TRIPLE_QUOTE_RE.finditer(ontology, source_start, listing_start)
        if not is_escaped(ontology, match.start())
    ]
    if not candidates:
        return -1
    return candidates[-1]


def parse_chapters(ontology: str) -> list[ChapterRecord]:
    starts = list(CHAPTER_START_RE.finditer(ontology))
    records: list[ChapterRecord] = []

    for index, start_match in enumerate(starts):
        search_end = starts[index + 1].start() if index + 1 < len(starts) else len(ontology)
        block = ontology[start_match.start():search_end]

        path_match = SOURCE_PATH_RE.search(block)
        open_match = SOURCE_OPEN_RE.search(block)
        listing_matches = list(HAS_LISTING_RE.finditer(block))
        if path_match is None or open_match is None or not listing_matches:
            raise ValueError(
                f"malformed book:Chapter at byte {start_match.start()}: "
                f"path={path_match is not None} source={open_match is not None} "
                f"listing={bool(listing_matches)}"
            )

        listing_match = listing_matches[-1]
        source_start = start_match.start() + open_match.end()
        listing_start = start_match.start() + listing_match.start()
        source_end = closing_delimiter(ontology, source_start, listing_start)
        if source_end < source_start:
            raise ValueError(
                f"chapter {path_match.group(1)} has no closing sourceText delimiter"
            )

        record_end = start_match.start() + listing_match.end()
        records.append(
            ChapterRecord(
                subject_start=start_match.start(),
                record_end=record_end,
                path=path_match.group(1),
                source_start=source_start,
                source_end=source_end,
            )
        )

    paths = [record.path for record in records]
    duplicates = sorted({path for path in paths if paths.count(path) > 1})
    if duplicates:
        raise ValueError(f"duplicate chapter source paths: {duplicates}")

    return records


def replace_sources(ontology: str, sources: Mapping[str, str]) -> str:
    records = parse_chapters(ontology)
    record_paths = {record.path for record in records}
    source_paths = set(sources)
    if record_paths != source_paths:
        missing = sorted(record_paths - source_paths)
        extra = sorted(source_paths - record_paths)
        raise ValueError(f"chapter source replacement mismatch: missing={missing} extra={extra}")

    pieces: list[str] = []
    cursor = 0
    for record in records:
        pieces.append(ontology[cursor:record.source_start])
        pieces.append(sources[record.path])
        cursor = record.source_end
    pieces.append(ontology[cursor:])
    return "".join(pieces)


def main() -> None:
    from pathlib import Path

    repo = Path(__file__).resolve().parents[2]
    path = repo / "packs" / "level-five-book-pack" / "ontology.ttl"
    records = parse_chapters(path.read_text(encoding="utf-8"))
    print(f"book ontology chapter records: {len(records)}")


if __name__ == "__main__":
    main()
