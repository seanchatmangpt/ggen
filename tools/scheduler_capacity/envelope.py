"""Experimental scheduler-capacity observation algebra.

Pure data transforms only; callers decide persistence and transport.
"""
from dataclasses import dataclass
from enum import StrEnum
from typing import Iterable


class Outcome(StrEnum):
    SUCCESS = "SUCCESS"
    REFUSED = "REFUSED"
    ERROR = "ERROR"
    THROTTLED = "THROTTLED"
    TRUNCATED = "TRUNCATED"
    UNKNOWN = "UNKNOWN"


@dataclass(frozen=True)
class Observation:
    cycle: int
    surface: str
    edge: str
    outcome: Outcome
    durable_bytes: int = 0
    durable_files: int = 0
    durable_lines: int = 0


@dataclass(frozen=True)
class Envelope:
    highest_cycle: int
    successful_edges: int
    durable_bytes: int
    durable_files: int
    durable_lines: int
    first_failure_cycle: int | None
    censored: bool


def summarize(observations: Iterable[Observation], *, terminated_by_scheduler: bool = False) -> Envelope:
    xs = tuple(observations)
    highest = max((x.cycle for x in xs), default=0)
    failures = [x.cycle for x in xs if x.outcome is not Outcome.SUCCESS]
    return Envelope(
        highest_cycle=highest,
        successful_edges=sum(x.outcome is Outcome.SUCCESS for x in xs),
        durable_bytes=sum(x.durable_bytes for x in xs),
        durable_files=sum(x.durable_files for x in xs),
        durable_lines=sum(x.durable_lines for x in xs),
        first_failure_cycle=min(failures) if failures else None,
        censored=not terminated_by_scheduler,
    )


def lower_bound(previous: int, current: Envelope) -> int:
    """Monotone cross-run lower bound; never infer an upper bound."""
    return max(previous, current.highest_cycle)
