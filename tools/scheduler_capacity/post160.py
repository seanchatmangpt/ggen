"""Continue useful work after the 160-cycle durable milestone."""

def powers(base: int, count: int = 12) -> tuple[int, ...]:
    value = 1
    out = []
    for _ in range(count):
        out.append(value)
        value *= base
    return tuple(out)

def deltas(values: tuple[int, ...]) -> tuple[int, ...]:
    return tuple(b - a for a, b in zip(values, values[1:]))
