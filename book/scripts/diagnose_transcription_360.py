#!/usr/bin/env python3
"""Print compact ownership diagnostics for unmapped book outputs."""

import validate_transcription_360 as validator

ontology = validator.read_raw(validator.ONTOLOGY)
for target in (
    "SUMMARY.md",
    "tcps-standing/337-jikoken-kensa.md",
    "listings/337-jikoken-kensa.ttl",
    "theme/level-five.css",
):
    needle = f'book:sourcePath "{target}"'
    index = ontology.find(needle)
    print(f"OWNER_LOOKUP {target} found={index >= 0}")
    if index < 0:
        continue
    start = ontology.rfind("\n", 0, max(0, index - 600))
    end = ontology.find("\n\n", index)
    if end < 0:
        end = min(len(ontology), index + 800)
    print(ontology[start + 1 : end])
