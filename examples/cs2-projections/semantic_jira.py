"""Semantic Jira projection utilities for RFC-CS2-001.

Consumes generated semantic_jira.json. This module constructs reversible Jira
issue payloads; it never performs network I/O or consequential DO.
"""
from __future__ import annotations

from dataclasses import dataclass
from hashlib import sha256
import json
from pathlib import Path
from typing import Any, Iterable

SCHEMA = "https://chatman.ai/cs2/semantic-jira/v1"
SUBJECT = "https://chatman.ai/cs2#RFC-CS2-001"
AUTHORITY = "CONSTRUCT"


class ProjectionError(ValueError):
    """Raised when generated semantic Jira data violates its contract."""


@dataclass(frozen=True)
class JiraIssue:
    external_id: str
    subject: str
    campaign: str
    consumer: str
    summary: str
    authority_ceiling: str
    labels: tuple[str, ...]

    @classmethod
    def from_mapping(cls, value: dict[str, Any]) -> "JiraIssue":
        required = {
            "external_id", "subject", "campaign", "consumer",
            "summary", "authority_ceiling", "labels", "provenance",
        }
        missing = required - value.keys()
        if missing:
            raise ProjectionError(f"missing fields: {sorted(missing)}")
        provenance = value["provenance"]
        if value["subject"] != SUBJECT:
            raise ProjectionError(f"divergent subject: {value['subject']}")
        if value["authority_ceiling"] != AUTHORITY:
            raise ProjectionError("authority ceiling must remain CONSTRUCT")
        if provenance.get("do_authorized") is not False:
            raise ProjectionError("projection must not mint DO authority")
        if provenance.get("canonical_subject") != SUBJECT:
            raise ProjectionError("provenance subject diverges")
        return cls(
            external_id=str(value["external_id"]),
            subject=str(value["subject"]),
            campaign=str(value["campaign"]),
            consumer=str(value["consumer"]),
            summary=str(value["summary"]),
            authority_ceiling=str(value["authority_ceiling"]),
            labels=tuple(str(v) for v in value["labels"]),
        )

    def fields(self) -> dict[str, Any]:
        return {
            "summary": self.summary,
            "labels": list(self.labels),
            "description": {
                "type": "doc",
                "version": 1,
                "content": [{
                    "type": "paragraph",
                    "content": [{
                        "type": "text",
                        "text": (
                            f"Semantic projection {self.external_id}; "
                            f"subject={self.subject}; consumer={self.consumer}; "
                            f"authority={self.authority_ceiling}"
                        ),
                    }],
                }],
            },
        }


@dataclass(frozen=True)
class Projection:
    issues: tuple[JiraIssue, ...]

    @classmethod
    def load(cls, path: str | Path) -> "Projection":
        raw = json.loads(Path(path).read_text(encoding="utf-8"))
        if raw.get("schema") != SCHEMA:
            raise ProjectionError(f"unsupported schema: {raw.get('schema')}")
        if raw.get("source") != "RFC-CS2-001":
            raise ProjectionError("projection source diverges")
        if raw.get("authority") != AUTHORITY:
            raise ProjectionError("projection authority diverges")
        issues = tuple(JiraIssue.from_mapping(v) for v in raw.get("issues", []))
        ids = [i.external_id for i in issues]
        if ids != sorted(ids):
            raise ProjectionError("issues must be deterministically ordered")
        if len(ids) != len(set(ids)):
            raise ProjectionError("duplicate external_id")
        return cls(issues)

    def jira_create_payloads(self, project_key: str, issue_type: str = "Task") -> list[dict[str, Any]]:
        if not project_key.strip():
            raise ProjectionError("project_key must be non-empty")
        return [{
            "external_id": issue.external_id,
            "fields": {
                "project": {"key": project_key},
                "issuetype": {"name": issue_type},
                **issue.fields(),
            },
        } for issue in self.issues]

    def digest(self) -> str:
        canonical = json.dumps(
            [issue.__dict__ for issue in self.issues],
            sort_keys=True,
            separators=(",", ":"),
            default=list,
        ).encode()
        return sha256(canonical).hexdigest()

    def by_consumer(self, consumer: str) -> tuple[JiraIssue, ...]:
        return tuple(i for i in self.issues if i.consumer == consumer)


def semantic_diff(before: Projection, after: Projection) -> dict[str, list[str]]:
    left = {i.external_id: i for i in before.issues}
    right = {i.external_id: i for i in after.issues}
    return {
        "added": sorted(right.keys() - left.keys()),
        "removed": sorted(left.keys() - right.keys()),
        "changed": sorted(k for k in left.keys() & right.keys() if left[k] != right[k]),
    }


def batch(iterable: Iterable[dict[str, Any]], size: int = 50) -> list[list[dict[str, Any]]]:
    if size < 1:
        raise ProjectionError("batch size must be positive")
    items = list(iterable)
    return [items[i:i + size] for i in range(0, len(items), size)]
