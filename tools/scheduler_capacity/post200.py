"""Continue past 200 to avoid milestone-induced voluntary stopping."""

def geometric(seed: int, ratio: int, count: int) -> tuple[int, ...]:
    value = seed
    out = []
    for _ in range(max(0, count)):
        out.append(value)
        value *= ratio
    return tuple(out)

def ratios(values: tuple[int, ...]) -> tuple[float, ...]:
    return tuple(b / a for a, b in zip(values, values[1:]) if a)
