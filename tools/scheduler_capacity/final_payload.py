"""Generate a compact final receipt payload from observed counters."""

def payload(run_id: str, cycle: int, previous: int, code_batches: int, commits: int, first_failure: str | None) -> dict[str, object]:
    return {
        "run_id": run_id,
        "highest_observable_cycle": cycle,
        "previous_lower_bound_max_cycles": previous,
        "new_lower_bound_max_cycles": max(previous, cycle),
        "code_batches": code_batches,
        "durable_commits": commits,
        "first_failing_edge": first_failure,
        "hard_maximum": None,
    }
