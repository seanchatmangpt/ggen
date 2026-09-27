"""Small pure helpers for classifying capacity probe observations."""
from dataclasses import dataclass


@dataclass(frozen=True)
class Observation:
    cycle: int
    surface: str
    operation: str
    outcome: str


def is_failure(row: Observation) -> bool:
    return row.outcome in {"REFUSED", "ERROR", "THROTTLED", "TRUNCATED"}


def next_open_edge(preferred: str, alternates: tuple[str, ...], closed: set[str]) -> str | None:
    if preferred not in closed:
        return preferred
    for edge in alternates:
        if edge not in closed:
            return edge
    return None


def observed_lower_bound(previous: int, cycle: int) -> int:
    return max(previous, cycle)
