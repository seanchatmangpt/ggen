"""Deterministic RFC-CS2-001 semantic-Jira projection utilities.

This module consumes generated projection rows. It never upgrades authority or
treats Jira state as subject truth.
"""
from __future__ import annotations

from dataclasses import dataclass
from hashlib import sha256
import json
from typing import Iterable, Mapping

SUBJECT = "https://chatman.ai/cs2#RFC-CS2-001"
AUTHORITY = "CONSTRUCT"

@dataclass(frozen=True, order=True)
class SemanticIssue:
    work_id: str
    consumer: str
    projection: str
    status: str = "OPEN"

    def as_dict(self) -> dict[str, str]:
        return {
            "subject": SUBJECT,
            "consumer": self.consumer,
            "workId": self.work_id,
            "projection": self.projection,
            "authorityCeiling": AUTHORITY,
            "status": self.status,
        }

def admit(row: Mapping[str, object]) -> SemanticIssue:
    if row.get("subject") != SUBJECT:
        raise ValueError("DIVERGENT_SUBJECT")
    if row.get("authorityCeiling") != AUTHORITY:
        raise ValueError("AUTHORITY_ESCALATION")
    work_id = str(row.get("workId", ""))
    if not work_id.startswith("CS2-WRK-"):
        raise ValueError("INVALID_WORK_ID")
    return SemanticIssue(work_id, str(row["consumer"]), str(row["projection"]), str(row.get("status", "OPEN")))

def canonicalize(rows: Iterable[Mapping[str, object]]) -> list[dict[str, str]]:
    issues = sorted({admit(row) for row in rows})
    return [issue.as_dict() for issue in issues]

def encode(rows: Iterable[Mapping[str, object]]) -> bytes:
    return (json.dumps(canonicalize(rows), sort_keys=True, separators=(",", ":")) + "\n").encode()

def digest(rows: Iterable[Mapping[str, object]]) -> str:
    return "sha256:" + sha256(encode(rows)).hexdigest()

def jira_payload(row: Mapping[str, object], project_key: str = "CS2") -> dict[str, object]:
    issue = admit(row)
    return {
        "fields": {
            "project": {"key": project_key},
            "summary": f"{issue.work_id}: {issue.projection}",
            "description": {
                "type": "doc", "version": 1,
                "content": [{"type": "paragraph", "content": [{"type": "text", "text": f"Subject: {SUBJECT}\nConsumer: {issue.consumer}\nAuthority: {AUTHORITY}"}]}],
            },
            "issuetype": {"name": "Task"},
            "labels": ["cs2", issue.consumer.rsplit("#", 1)[-1]],
        },
        "semantic": issue.as_dict(),
    }

def batch(rows: Iterable[Mapping[str, object]], project_key: str = "CS2") -> list[dict[str, object]]:
    return [jira_payload(row, project_key) for row in canonicalize(rows)]
