"""Crash-safe scheduler capacity probe ledger primitives.

Experimental helper: represents observable cycles without pretending they are
undocumented scheduler internals. No network or execution side effects.
"""
from __future__ import annotations

from dataclasses import dataclass, asdict
from enum import Enum
from typing import Iterable, Mapping, Any


class Outcome(str, Enum):
    SUCCESS = "SUCCESS"
    REFUSED = "REFUSED"
    ERROR = "ERROR"
    THROTTLED = "THROTTLED"
    TRUNCATED = "TRUNCATED"
    UNKNOWN = "UNKNOWN"


@dataclass(frozen=True)
class Cycle:
    cycle: int
    timestamp: str
    tool: str
    operation: str
    outcome: Outcome
    next_intended_probe: str
    durable_bytes: int = 0
    durable_files: int = 0
    code_lines: int = 0
    commit_sha: str | None = None
    failure_class: str | None = None

    def record(self) -> dict[str, Any]:
        return {**asdict(self), "outcome": self.outcome.value}


def append_cycle(ledger: Mapping[str, Any], cycle: Cycle) -> dict[str, Any]:
    entries = list(ledger.get("entries", ()))
    if entries and cycle.cycle <= int(entries[-1]["cycle"]):
        raise ValueError("cycle counter must increase monotonically")
    entries.append(cycle.record())
    return {**ledger, "entries": entries, "highest_observable_cycle": cycle.cycle}


def outcome_counts(cycles: Iterable[Mapping[str, Any]]) -> dict[str, int]:
    counts = {outcome.value: 0 for outcome in Outcome}
    for cycle in cycles:
        key = str(cycle.get("outcome", Outcome.UNKNOWN.value))
        counts[key if key in counts else Outcome.UNKNOWN.value] += 1
    return counts


def lower_bound(previous: int, observed: int) -> int:
    """Cross-run lower bound; never interprets a censored run as a maximum."""
    return max(previous, observed)
