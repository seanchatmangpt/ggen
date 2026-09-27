"""Separate scheduler, read-plane, and mutation-plane observations."""
from dataclasses import dataclass


@dataclass(frozen=True)
class SurfaceBudget:
    successful: int = 0
    refused: int = 0
    errors: int = 0
    throttled: int = 0
    truncated: int = 0

    def observe(self, outcome: str) -> "SurfaceBudget":
        field = {
            "SUCCESS": "successful",
            "REFUSED": "refused",
            "ERROR": "errors",
            "THROTTLED": "throttled",
            "TRUNCATED": "truncated",
        }.get(outcome)
        if field is None:
            return self
        values = self.__dict__ | {field: getattr(self, field) + 1}
        return SurfaceBudget(**values)


@dataclass(frozen=True)
class CapacityVector:
    cycles: int
    github: SurfaceBudget
    notion: SurfaceBudget
    slack: SurfaceBudget

    @property
    def read_plane_healthy(self) -> bool:
        return self.notion.successful > 0 and self.slack.successful > 0

    @property
    def mutation_refusal_is_scheduler_boundary(self) -> bool:
        return False

    def scheduler_lower_bound(self, prior: int = 0) -> int:
        return max(prior, self.cycles)
