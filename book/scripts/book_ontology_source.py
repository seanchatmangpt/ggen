#!/usr/bin/env python3
"""Span-safe access to book:Chapter sourceText literals in pack ontology.

The ontology embeds Markdown, Rust, TOML, SPARQL and Turtle examples inside
long-string literals. Subject-like text inside those literals is data, not an
ontology record. This module performs a small lexical scan, admitting chapter
subjects only while outside unescaped Turtle triple-quoted strings.
"""

from __future__ import annotations

from dataclasses import dataclass
import re
from typing import Mapping

CHAPTER_LINE_RE = re.compile(
    r"^book:chapter-[^\s]+\s+a\s+book:Chapter\s*;"
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


def unescaped_triples(text: str, start: int = 0, end: int | None = None):
    stop = len(text) if end is None else end
    for match in TRIPLE_QUOTE_RE.finditer(text, start, stop):
        if not is_escaped(text, match.start()):
            yield match.start()


def chapter_subject_starts(ontology: str) -> list[int]:
    starts: list[int] = []
    inside_long_string = False
    offset = 0

    for line in ontology.splitlines(keepends=True):
        if not inside_long_string and CHAPTER_LINE_RE.match(line):
            starts.append(offset)

        line_end = offset + len(line)
        for _position in unescaped_triples(ontology, offset, line_end):
            inside_long_string = not inside_long_string
        offset = line_end

    if inside_long_string:
        raise ValueError("ontology ends inside an unterminated triple-quoted literal")
    return starts


def first_unescaped_triple(text: str, start: int, end: int) -> int:
    return next(unescaped_triples(text, start, end), -1)


def parse_chapters(ontology: str) -> list[ChapterRecord]:
    starts = chapter_subject_starts(ontology)
    records: list[ChapterRecord] = []

    for index, subject_start in enumerate(starts):
        search_end = starts[index + 1] if index + 1 < len(starts) else len(ontology)
        block = ontology[subject_start:search_end]

        path_match = SOURCE_PATH_RE.search(block)
        open_match = SOURCE_OPEN_RE.search(block)
        if path_match is None or open_match is None:
            raise ValueError(
                f"malformed top-level book:Chapter at byte {subject_start}: "
                f"path={path_match is not None} source={open_match is not None}"
            )

        path = path_match.group(1)
        source_start = subject_start + open_match.end()
        source_end = first_unescaped_triple(ontology, source_start, search_end)
        if source_end < source_start:
            raise ValueError(f"chapter {path} has no closing sourceText delimiter")

        trailing_start = source_end + 3
        trailing = ontology[trailing_start:search_end]
        listing_match = HAS_LISTING_RE.search(trailing)
        if listing_match is None:
            raise ValueError(f"chapter {path} has no top-level book:hasListing predicate")

        record_end = trailing_start + listing_match.end()
        records.append(
            ChapterRecord(
                subject_start=subject_start,
                record_end=record_end,
                path=path,
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
