"""Sparse checkpoint planner for long capacity probes."""

def due(cycle: int, *, interval: int = 20) -> bool:
    return cycle >= interval and cycle % interval == 0

def next_checkpoint(cycle: int, *, interval: int = 20) -> int:
    return ((cycle // interval) + 1) * interval

def milestones(cycle: int, *, interval: int = 20) -> tuple[int, ...]:
    return tuple(range(interval, cycle + 1, interval))
