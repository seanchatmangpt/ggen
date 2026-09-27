"""Aggregate scheduler capacity observations into conservative bounds."""
from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, Mapping, Any


@dataclass(frozen=True)
class Capacity:
    highest_cycle: int
    durable_commits: int
    durable_code_cycles: int
    notion_reads: int
    notion_searches: int
    notion_writes: int
    slack_reads: int
    slack_searches: int
    slack_writes: int
    slack_edits: int
    slack_reactions: int


def summarize(entries: Iterable[Mapping[str, Any]]) -> Capacity:
    rows = list(entries)
    def count(tool: str, category: str, success_only: bool = True) -> int:
        return sum(
            1 for row in rows
            if tool.lower() in str(row.get("tool", "")).lower()
            and category in str(row.get("operation", "")).lower()
            and (not success_only or row.get("status", row.get("outcome")) == "SUCCESS")
        )

    commits = sum(1 for row in rows if row.get("commit_sha"))
    code_cycles = sum(
        1 for row in rows
        if row.get("commit_sha") and int(row.get("code_lines", 0)) > 0
    )
    return Capacity(
        highest_cycle=max((int(row["cycle"]) for row in rows), default=0),
        durable_commits=commits,
        durable_code_cycles=code_cycles,
        notion_reads=count("Notion", "read") + count("Notion", "fetch"),
        notion_searches=count("Notion", "search"),
        notion_writes=count("Notion", "write"),
        slack_reads=count("Slack", "read"),
        slack_searches=count("Slack", "search"),
        slack_writes=count("Slack", "send"),
        slack_edits=count("Slack", "edit"),
        slack_reactions=count("Slack", "reaction"),
    )


def censored_lower_bound(previous: int, current: Capacity) -> int:
    return max(previous, current.highest_cycle)
