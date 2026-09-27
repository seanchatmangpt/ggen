"""Represent probe surface budgets independently from scheduler-cycle bounds."""
from dataclasses import dataclass


@dataclass
class SurfaceBudget:
    success: int = 0
    refused: int = 0
    error: int = 0
    throttled: int = 0
    truncated: int = 0

    def observe(self, outcome: str) -> None:
        key = outcome.lower()
        if not hasattr(self, key):
            key = "error"
        setattr(self, key, getattr(self, key) + 1)

    @property
    def available(self) -> bool:
        return self.throttled == 0 and self.truncated == 0


def termination_evidence(budgets: dict[str, SurfaceBudget]) -> str:
    if budgets and all(not budget.available for budget in budgets.values()):
        return "all_probe_surfaces_unavailable"
    return "censored_or_continuing"
