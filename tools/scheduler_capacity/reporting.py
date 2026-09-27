"""Normalize milestone reporting for capacity receipts."""

def report(cycle: int, step: int = 20) -> tuple[tuple[int, bool], ...]:
    ceiling = max(100, ((cycle + step - 1) // step) * step)
    return tuple((mark, cycle >= mark) for mark in range(100, ceiling + step, step))

def label(cycle: int, previous: int) -> str:
    bound = max(cycle, previous)
    return f"lower_bound_max_cycles>={bound}; observed={cycle}; hard_maximum=unknown"
