"""220+ work remains ordinary manufacturing, not a special terminal path."""

def squares(start: int, count: int = 20) -> tuple[tuple[int, int], ...]:
    return tuple((n, n * n) for n in range(start, start + count))

def odd_differences(rows: tuple[tuple[int, int], ...]) -> tuple[int, ...]:
    values = tuple(value for _, value in rows)
    return tuple(b - a for a, b in zip(values, values[1:]))

def sum_values(rows: tuple[tuple[int, int], ...]) -> int:
    return sum(value for _, value in rows)
