"""FOND-style routing for capacity-probe mutation edges."""
from dataclasses import dataclass, field
from enum import StrEnum


class EdgeState(StrEnum):
    UNKNOWN = "UNKNOWN"
    REACHABLE = "REACHABLE"
    REFUSED = "REFUSED"
    ERROR = "ERROR"
    THROTTLED = "THROTTLED"


@dataclass(frozen=True)
class EdgeAttempt:
    edge: str
    cycle: int
    state: EdgeState
    failure_class: str | None = None


@dataclass
class EdgeRouter:
    attempts: list[EdgeAttempt] = field(default_factory=list)
    removed: set[str] = field(default_factory=set)

    def observe(self, attempt: EdgeAttempt) -> None:
        self.attempts.append(attempt)
        if attempt.state in {EdgeState.REFUSED, EdgeState.THROTTLED}:
            self.removed.add(attempt.edge)

    def lawful(self, *candidates: str) -> tuple[str, ...]:
        return tuple(edge for edge in candidates if edge not in self.removed)

    def choose(self, *candidates: str) -> str | None:
        return next(iter(self.lawful(*candidates)), None)

    def latest(self, edge: str) -> EdgeAttempt | None:
        return next((a for a in reversed(self.attempts) if a.edge == edge), None)

    def route_after_failure(self, failed_edge: str, *alternatives: str) -> str | None:
        self.removed.add(failed_edge)
        return self.choose(*alternatives)

    @property
    def blocked(self) -> bool:
        return not self.lawful("update_ref", "create_branch", "update_file", "create_file")
