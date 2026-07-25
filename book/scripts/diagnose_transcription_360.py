#!/usr/bin/env python3
"""Print compact ownership diagnostics for the complete book."""

from pathlib import Path
import re
import validate_transcription_360 as validator

records = validator.parse_records(validator.read_raw(validator.ONTOLOGY))
ttl_chapters = {
    record.path for record in records
    if record.kind == "Chapter" and record.path.endswith(".md")
}
ttl_listings = {record.path for record in records if record.kind == "Listing"}
summary = (validator.SRC / "SUMMARY.md").read_text(encoding="utf-8")
summary_links = set(re.findall(r"\[[^\]]+\]\(([^)]+\.md)\)", summary))
manual_links = {"README.md"}

print(f"TTL_CHAPTER_PATHS {len(ttl_chapters)}")
print(f"TTL_LISTING_PATHS {len(ttl_listings)}")
print(f"SUMMARY_MD_LINKS {len(summary_links)}")
print(f"SUMMARY_NOT_TTL {sorted(summary_links - ttl_chapters - manual_links)}")
print(f"TTL_NOT_SUMMARY {sorted(ttl_chapters - summary_links)}")

actual_listings = {
    path.relative_to(validator.SRC).as_posix()
    for path in (validator.SRC / "listings").rglob("*") if path.is_file()
}
print(f"LISTING_FILES_NOT_TTL {sorted(actual_listings - ttl_listings)}")
print(f"TTL_LISTINGS_NOT_FILES {sorted(ttl_listings - actual_listings)}")

for target in ("SUMMARY.md", "theme/level-five.css"):
    path = validator.SRC / Path(target)
    print(f"MANUAL_CANDIDATE {target} exists={path.is_file()} sha256={validator.sha(path.read_bytes()) if path.is_file() else 'missing'}")
