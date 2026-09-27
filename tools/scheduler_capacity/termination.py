"""Conservative termination classification for capacity experiments."""


def classify(*, explicit_scheduler_stop: bool, budget_exhausted: bool,
             all_surfaces_unavailable: bool, tool_throttled: bool) -> str:
    if explicit_scheduler_stop:
        return "scheduler_boundary_observed"
    if budget_exhausted:
        return "execution_budget_censored"
    if all_surfaces_unavailable:
        return "surface_exhaustion"
    if tool_throttled:
        return "tool_throttle_censored"
    return "right_censored_without_boundary"


def may_claim_hard_max(classification: str, repeated_same_boundary: bool) -> bool:
    return classification == "scheduler_boundary_observed" and repeated_same_boundary
