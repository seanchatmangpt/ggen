"""Plan the next milestone without making it a stop condition."""

def next_mark(cycle: int, step: int = 20) -> int:
    return max(120, ((cycle // step) + 1) * step)

def remaining(cycle: int, mark: int) -> int:
    return max(0, mark - cycle)

def continue_probe(*, useful_work: bool, computation_available: bool) -> bool:
    return useful_work and computation_available
