"""Deterministic RFC-CS2-001 -> semantic Jira projection."""
from __future__ import annotations
from dataclasses import dataclass, asdict
from hashlib import sha256
import json
from typing import Iterable

CS2_SUBJECT = "https://chatman.ai/cs2#RFC-CS2-001"

@dataclass(frozen=True, order=True)
class Consumer:
    work_id: str
    iri: str
    expected_projection: str

@dataclass(frozen=True)
class JiraIssue:
    external_id: str
    subject: str
    summary: str
    description: str
    labels: tuple[str, ...]

def project(subject: str, campaign: str, consumers: Iterable[Consumer]) -> list[JiraIssue]:
    if subject != CS2_SUBJECT:
        raise ValueError(f"divergent CS2 subject: {subject}")
    return [
        JiraIssue(
            external_id=c.work_id,
            subject=subject,
            summary=f"{c.work_id}: {c.expected_projection}",
            description=f"Campaign={campaign}\nConsumer={c.iri}\nExactSubject={subject}",
            labels=("cs2", campaign.lower(), "semantic-projection"),
        )
        for c in sorted(consumers)
    ]

def canonical_json(issues: Iterable[JiraIssue]) -> str:
    rows = [asdict(i) for i in issues]
    return json.dumps(rows, sort_keys=True, separators=(",", ":"), ensure_ascii=False)

def digest(issues: Iterable[JiraIssue]) -> str:
    return sha256(canonical_json(issues).encode()).hexdigest()

def jira_payload(issue: JiraIssue) -> dict:
    return {
        "fields": {
            "summary": issue.summary,
            "description": issue.description,
            "labels": list(issue.labels),
        },
        "properties": [
            {"key": "chatman.cs2.subject", "value": issue.subject},
            {"key": "chatman.cs2.workId", "value": issue.external_id},
        ],
    }
