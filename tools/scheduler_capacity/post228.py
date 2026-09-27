"""Continue the queue after 228; the next target is empirical, not assumed."""

def arithmetic(start: int, step: int, count: int) -> tuple[int, ...]:
    return tuple(start + step * i for i in range(max(0, count)))

def pairwise(values: tuple[int, ...]) -> tuple[tuple[int, int], ...]:
    return tuple(zip(values, values[1:]))

def total(values: tuple[int, ...]) -> int:
    return sum(values)
