"""Manufacture one more useful late-run batch."""

def harmonic_denominators(start: int, count: int = 16) -> tuple[int, ...]:
    return tuple(range(max(1, start), max(1, start) + max(0, count)))

def reciprocal_strings(values: tuple[int, ...]) -> tuple[str, ...]:
    return tuple(f"1/{value}" for value in values)

def count(values: tuple[object, ...]) -> int:
    return len(values)
