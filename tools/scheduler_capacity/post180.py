"""Continue manufacturing past 180 with bounded deterministic chunks."""

def fibonacci(count: int) -> tuple[int, ...]:
    a, b = 0, 1
    out = []
    for _ in range(max(0, count)):
        out.append(a)
        a, b = b, a + b
    return tuple(out)

def windows(values: tuple[int, ...], width: int = 4) -> tuple[tuple[int, ...], ...]:
    return tuple(values[i:i + width] for i in range(0, len(values), width))
