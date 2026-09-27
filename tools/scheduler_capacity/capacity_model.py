"""Experimental scheduler-capacity observation model.

Pure data machinery: no network access and no hidden scheduler assumptions.
"""
from dataclasses import dataclass, field
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
    operation: str
    outcome: Outcome
    durable_bytes: int = 0
    durable_files: int = 0
    durable_lines: int = 0
    commit_sha: str | None = None


@dataclass
class CapacityEnvelope:
    observations: list[Observation] = field(default_factory=list)

    def append(self, observation: Observation) -> None:
        if self.observations and observation.cycle <= self.observations[-1].cycle:
            raise ValueError("cycles must increase monotonically")
        self.observations.append(observation)

    @property
    def lower_bound_cycles(self) -> int:
        return max((o.cycle for o in self.observations), default=0)

    @property
    def durable_commits(self) -> tuple[str, ...]:
        return tuple(o.commit_sha for o in self.observations if o.commit_sha)

    def outcomes(self, surface: str) -> dict[Outcome, int]:
        counts = {outcome: 0 for outcome in Outcome}
        for observation in self.observations:
            if observation.surface == surface:
                counts[observation.outcome] += 1
        return counts

    def first_non_success(self) -> Observation | None:
        return next((o for o in self.observations if o.outcome != Outcome.SUCCESS), None)

    def durable_totals(self) -> dict[str, int]:
        return {
            "bytes": sum(o.durable_bytes for o in self.observations),
            "files": sum(o.durable_files for o in self.observations),
            "lines": sum(o.durable_lines for o in self.observations),
        }


def cross_run_lower_bound(envelopes: Iterable[CapacityEnvelope]) -> int:
    return max((e.lower_bound_cycles for e in envelopes), default=0)
