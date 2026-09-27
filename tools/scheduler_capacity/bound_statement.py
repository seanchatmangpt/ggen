"""Bound reporting avoids claiming an uncensored maximum."""

def statement(previous: int, observed: int, terminal_scheduler_signal: bool) -> dict[str, object]:
    lower = max(previous, observed)
    return {
        "previous_lower_bound": previous,
        "observed_cycles": observed,
        "lower_bound_max_cycles": lower,
        "hard_maximum": observed if terminal_scheduler_signal else None,
        "censored": not terminal_scheduler_signal,
    }
