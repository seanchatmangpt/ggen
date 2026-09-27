"""Local-only continuation queue for late probe cycles."""

def chunks(start: int, count: int = 16, width: int = 4) -> tuple[tuple[int, ...], ...]:
    values = tuple(range(start, start + count))
    return tuple(values[i:i + width] for i in range(0, len(values), width))

def flatten(parts: tuple[tuple[int, ...], ...]) -> tuple[int, ...]:
    return tuple(value for part in parts for value in part)

def checksum(parts: tuple[tuple[int, ...], ...]) -> int:
    return sum(flatten(parts))
