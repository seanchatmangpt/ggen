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

actual_paths = {
    path.relative_to(validator.SRC).as_posix()
    for path in validator.SRC.rglob("*") if path.is_file()
}
actual_listings = {path for path in actual_paths if path.startswith("listings/")}
print(f"LISTING_FILES_NOT_TTL {sorted(actual_listings - ttl_listings)}")
print(f"TTL_LISTINGS_NOT_FILES {sorted(ttl_listings - actual_listings)}")

print("SUMMARY_335_339")
for line in summary.splitlines():
    if re.search(r"\b33[5-9]\.", line):
        print(line)

print("TTL_335_339")
for record in records:
    if re.search(r"(?:^|/)33[5-9]-", record.path):
        print(f"kind={record.kind} subject={record.subject} path={record.path!r}")

for path in sorted(actual_paths):
    if re.search(r"(?:^|/)33[5-9]-", path):
        print(f"ACTUAL_335_339 {path!r}")

for target in ("SUMMARY.md", "theme/level-five.css"):
    path = validator.SRC / Path(target)
    print(f"MANUAL_CANDIDATE {target} exists={path.is_file()} sha256={validator.sha(path.read_bytes()) if path.is_file() else 'missing'}")
