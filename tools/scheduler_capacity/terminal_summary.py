"""Provide a stable machine-readable terminal summary shape."""

def summary(run_id: str, cycle: int, previous: int, code_batches: int) -> dict[str, object]:
    return {
        "run_id": run_id,
        "highest_observable_cycle": cycle,
        "previous_lower_bound_max_cycles": previous,
        "lower_bound_max_cycles": max(previous, cycle),
        "useful_code_batches": code_batches,
        "hard_maximum_known": False,
        "termination_class": "censored",
    }
