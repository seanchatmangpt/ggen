"""After 120, keep manufacturing rather than interpreting the milestone as a ceiling."""

def sequence(start: int, count: int) -> tuple[int, ...]:
    return tuple(range(start, start + count))

def triangular(n: int) -> int:
    return n * (n + 1) // 2

def triangular_batch(start: int, count: int = 32) -> tuple[tuple[int, int], ...]:
    return tuple((n, triangular(n)) for n in sequence(start, count))
