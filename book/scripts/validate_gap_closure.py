#!/usr/bin/env python3
"""Validate that every declared book gap has executable Chicago evidence."""
from __future__ import annotations

import argparse
import re
import tomllib
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
REGISTRY = ROOT / "book/gap-closure.toml"
CAPABILITY_MAP = ROOT / "book/src/CAPABILITY_MAP.md"

PACK_ROW = re.compile(
    r"^\|\s*`(?P<subject>packs/[^`]+)`\s*\|.*\|\s*\*\*(?P<standing>[A-Z_]+)\*\*\s*\|$"
)
FIELD_ROW = re.compile(
    r"^\|\s*\*\*(?P<subject>[IVX]+\.[^*]+)\*\*[^|]*\|.*\|(?P<standing>[^|]+)\|$"
)
LOW = {"PARTIAL", "TARGET", "ARCHIVE_ONLY"}


@dataclass(frozen=True)
class Error:
    code: str
    subject: str
    detail: str


def normalize_field(subject: str) -> str:
    return subject.split("—", 1)[0].strip()


def declared_gaps() -> set[str]:
    result: set[str] = set()
    in_fields = False
    for line in CAPABILITY_MAP.read_text(encoding="utf-8").splitlines():
        pack = PACK_ROW.match(line)
        if pack and pack.group("standing") in LOW:
            result.add(pack.group("subject"))
            continue
        if line.startswith("## Field-to-capability alignment"):
            in_fields = True
            continue
        if in_fields and line.startswith("## "):
            in_fields = False
        if not in_fields:
            continue
        field = FIELD_ROW.match(line)
        if field and any(item in field.group("standing") for item in LOW):
            result.add(normalize_field(field.group("subject")))
    return result


def validate() -> tuple[list[Error], dict[str, int]]:
    data = tomllib.loads(REGISTRY.read_text(encoding="utf-8"))
    obligations = data.get("obligation", [])
    evidence = data.get("evidence", {})
    errors: list[Error] = []

    ids: set[str] = set()
    subjects: set[str] = set()
    used_evidence: set[str] = set()

    for obligation in obligations:
        identifier = str(obligation.get("id", ""))
        subject = str(obligation.get("subject", ""))
        kind = str(obligation.get("kind", ""))
        refs = [str(item) for item in obligation.get("evidence", [])]
        if not identifier or identifier in ids:
            errors.append(Error("DUPLICATE_OR_EMPTY_ID", identifier, subject))
        ids.add(identifier)
        if not subject or subject in subjects:
            errors.append(Error("DUPLICATE_OR_EMPTY_SUBJECT", subject, identifier))
        subjects.add(subject)
        if kind not in {"pack", "field"}:
            errors.append(Error("UNKNOWN_KIND", subject, kind))
        if not refs:
            errors.append(Error("NO_EVIDENCE", subject, identifier))
        for ref in refs:
            used_evidence.add(ref)
            if ref not in evidence:
                errors.append(Error("UNKNOWN_EVIDENCE", subject, ref))

    for evidence_id, record in evidence.items():
        path = ROOT / str(record.get("path", ""))
        marker = str(record.get("marker", ""))
        if not path.is_file():
            errors.append(Error("MISSING_EVIDENCE_PATH", evidence_id, str(path.relative_to(ROOT))))
            continue
        text = path.read_text(encoding="utf-8", errors="replace")
        if not marker or marker not in text:
            errors.append(Error("MISSING_EVIDENCE_MARKER", evidence_id, marker))

    declared = declared_gaps()
    for subject in sorted(declared - subjects):
        errors.append(Error("UNREGISTERED_DECLARED_GAP", subject, "capability map has no obligation"))
    for subject in sorted(subjects - declared):
        errors.append(Error("STALE_OBLIGATION", subject, "registry subject is not currently declared below crown standing"))
    for evidence_id in sorted(set(evidence) - used_evidence):
        errors.append(Error("UNUSED_EVIDENCE", evidence_id, "no obligation cites this evidence"))

    counters = {
        "obligations": len(obligations),
        "declared_gaps": len(declared),
        "evidence_records": len(evidence),
        "used_evidence": len(used_evidence),
        "errors": len(errors),
    }
    return errors, counters


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.parse_args()
    errors, counters = validate()
    for error in errors:
        print(f"GAP_CLOSURE_ERROR {error.code} {error.subject}: {error.detail}")
    print("GAP_CLOSURE " + " ".join(f"{key}={value}" for key, value in counters.items()))
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
