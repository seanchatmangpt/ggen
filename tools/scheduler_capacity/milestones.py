"""Milestone reachability is an observation, not an assumed limit."""

def reached(cycle: int, marks: tuple[int, ...] = (20, 40, 60, 80, 100, 120, 140, 160)) -> dict[int, bool]:
    return {mark: cycle >= mark for mark in marks}

def next_unreached(cycle: int, step: int = 20) -> int:
    return ((cycle // step) + 1) * step
