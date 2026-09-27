"""280-cycle milestone projection remains conservative."""

def project(cycle: int, target: int = 280) -> tuple[int, bool, int]:
    return target, cycle >= target, max(0, target - cycle)

def lower_bound(previous: int, cycle: int) -> int:
    return max(previous, cycle)

def hard_maximum(censored: bool, cycle: int) -> int | None:
    return None if censored else cycle
