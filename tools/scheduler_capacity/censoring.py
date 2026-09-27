"""Censoring-aware capacity result model."""
from dataclasses import dataclass


@dataclass(frozen=True)
class RunResult:
    observed_cycles: int
    durable_cycles: int
    scheduler_termination_observed: bool
    throttles: int = 0
    truncations: int = 0

    @property
    def right_censored(self) -> bool:
        return not self.scheduler_termination_observed

    def update_lower_bound(self, previous: int) -> int:
        return max(previous, self.observed_cycles)

    def hard_maximum(self) -> int | None:
        return self.observed_cycles if self.scheduler_termination_observed else None


def aggregate_lower_bound(previous: int, runs: tuple[RunResult, ...]) -> int:
    bound = previous
    for run in runs:
        bound = run.update_lower_bound(bound)
    return bound
