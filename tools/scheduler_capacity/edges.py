"""Classify nondeterministic probe-edge observations without global STOP."""
from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import Iterable


class EdgeState(str, Enum):
    OPEN = "OPEN"
    REFUSED = "REFUSED"
    ERROR = "ERROR"
    THROTTLED = "THROTTLED"
    TRUNCATED = "TRUNCATED"


@dataclass(frozen=True)
class EdgeObservation:
    edge: str
    state: EdgeState
    detail: str = ""


def reachable(observations: Iterable[EdgeObservation]) -> set[str]:
    """Return edges not excluded by an observed failure in this run."""
    latest: dict[str, EdgeState] = {}
    for observation in observations:
        latest[observation.edge] = observation.state
    return {edge for edge, state in latest.items() if state is EdgeState.OPEN}


def first_failure(observations: Iterable[EdgeObservation]) -> EdgeObservation | None:
    for observation in observations:
        if observation.state is not EdgeState.OPEN:
            return observation
    return None


def route_after_failure(required: str, alternatives: Iterable[str], closed: set[str]) -> str | None:
    """Select the first lawful alternate edge; failure(e) removes only e."""
    if required not in closed:
        return required
    return next((edge for edge in alternatives if edge not in closed), None)
