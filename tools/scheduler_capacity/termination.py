"""Conservative classification of censored capacity-probe runs."""
from dataclasses import dataclass
from enum import StrEnum


class TerminationClass(StrEnum):
    OBSERVED_SCHEDULER_BOUNDARY = "OBSERVED_SCHEDULER_BOUNDARY"
    EXECUTION_BUDGET_CENSORED = "EXECUTION_BUDGET_CENSORED"
    TOOL_SURFACE_EXHAUSTED = "TOOL_SURFACE_EXHAUSTED"
    MUTATION_RAIL_CENSORED = "MUTATION_RAIL_CENSORED"
    UNKNOWN = "UNKNOWN"


@dataclass(frozen=True)
class RunEvidence:
    highest_cycle: int
    successful_read_after_last_mutation_failure: bool
    any_throttle: bool
    any_truncation: bool
    all_lawful_surfaces_unavailable: bool
    explicit_scheduler_termination: bool = False


def classify(evidence: RunEvidence) -> TerminationClass:
    if evidence.explicit_scheduler_termination:
        return TerminationClass.OBSERVED_SCHEDULER_BOUNDARY
    if evidence.all_lawful_surfaces_unavailable:
        return TerminationClass.TOOL_SURFACE_EXHAUSTED
    if evidence.successful_read_after_last_mutation_failure:
        return TerminationClass.EXECUTION_BUDGET_CENSORED
    if evidence.any_throttle or evidence.any_truncation:
        return TerminationClass.UNKNOWN
    return TerminationClass.MUTATION_RAIL_CENSORED


def hard_max_supported(repeated_boundaries: list[RunEvidence]) -> bool:
    """Require repeated explicit scheduler boundaries at one cycle."""
    if len(repeated_boundaries) < 2:
        return False
    cycles = {r.highest_cycle for r in repeated_boundaries}
    return len(cycles) == 1 and all(r.explicit_scheduler_termination for r in repeated_boundaries)
