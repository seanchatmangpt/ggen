"""Estimate confidence only from observations, never from milestone folklore."""

def censored_runs(observations: tuple[tuple[int, bool], ...]) -> int:
    return sum(1 for _, censored in observations if censored)

def observed_max(observations: tuple[tuple[int, bool], ...]) -> int:
    return max((cycles for cycles, _ in observations), default=0)

def repeated_terminal(observations: tuple[tuple[int, bool], ...]) -> int | None:
    terminals = [cycles for cycles, censored in observations if not censored]
    return terminals[0] if terminals and len(set(terminals)) == 1 else None
