"""Encode a no-false-maximum invariant for downstream consumers."""

def validate_summary(summary: dict[str, object]) -> bool:
    hard = summary.get("hard_maximum")
    censored = bool(summary.get("censored", hard is None))
    if censored and hard is not None:
        return False
    observed = int(summary.get("highest_observable_cycle", 0))
    lower = int(summary.get("lower_bound_max_cycles", observed))
    return lower >= observed

def conservative_max(summary: dict[str, object]) -> int | None:
    return None if summary.get("censored", True) else int(summary["highest_observable_cycle"])
