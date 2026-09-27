"""Experimental capacity observation primitives."""
from dataclasses import dataclass

@dataclass(frozen=True)
class Observation:
    cycle: int
    surface: str
    operation: str
    outcome: str


def lower_bound(previous: int, observed_cycle: int) -> int:
    return max(previous, observed_cycle)
