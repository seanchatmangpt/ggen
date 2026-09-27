"""Track whether milestone observations are independently durable."""

from dataclasses import dataclass

@dataclass(frozen=True)
class Milestone:
    cycle: int
    reached: bool
    durable: bool

def coverage(items: tuple[Milestone, ...]) -> float:
    reached = [x for x in items if x.reached]
    if not reached:
        return 0.0
    return sum(1 for x in reached if x.durable) / len(reached)

def highest_durable(items: tuple[Milestone, ...]) -> int:
    return max((x.cycle for x in items if x.reached and x.durable), default=0)
