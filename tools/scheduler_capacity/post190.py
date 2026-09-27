"""Manufacture a compact deterministic batch at the 190+ region."""

def cubes(start: int, count: int = 24) -> tuple[int, ...]:
    return tuple(n ** 3 for n in range(start, start + count))

def normalize(values: tuple[int, ...]) -> tuple[int, ...]:
    if not values:
        return ()
    base = values[0]
    return tuple(value - base for value in values)

def span(values: tuple[int, ...]) -> int:
    return max(values, default=0) - min(values, default=0)
