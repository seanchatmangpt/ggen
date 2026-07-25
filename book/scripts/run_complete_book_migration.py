#!/usr/bin/env python3
"""Run the complete source-law migration with the manual README exception.

`level-five-book-pack` owns 366 chapter products. `book/src/README.md` is the
manual mdBook front door and is linked from SUMMARY.md, but is intentionally not
a book:Chapter in the ontology. The migration core writes all chapter products
and pack law before its final equality check. This wrapper admits exactly that
one documented extra link and refuses every other mismatch.
"""

from __future__ import annotations

import re

import migrate_full_book_to_aligned_pack_law as migration
from book_ontology_source import parse_chapters


def validate_manual_front_door() -> None:
    records = parse_chapters(migration.ONTOLOGY.read_text(encoding="utf-8"))
    record_paths = {record.path for record in records}
    summary_links = set(
        re.findall(
            r"\[[^\]]+\]\(([^)]+\.md)\)",
            migration.SUMMARY.read_text(encoding="utf-8"),
        )
    )
    expected = record_paths | {"README.md"}
    if summary_links != expected:
        missing = sorted(expected - summary_links)
        extra = sorted(summary_links - expected)
        raise SystemExit(
            f"complete summary mismatch after README exception: "
            f"missing={missing} extra={extra}"
        )
    print(
        f"complete book promoted: ontology_chapters={len(record_paths)} "
        f"manual_front_doors=1 summary_links={len(summary_links)}"
    )


def main() -> None:
    try:
        migration.main()
    except SystemExit as error:
        if str(error) != "complete summary mismatch: missing=[] extra=['README.md']":
            raise
        validate_manual_front_door()


if __name__ == "__main__":
    main()
