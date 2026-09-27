"""Continue useful construction beyond the 240 milestone."""

def factorial_prefix(count: int) -> tuple[int, ...]:
    out = []
    value = 1
    for n in range(1, max(0, count) + 1):
        value *= n
        out.append(value)
    return tuple(out)

def digit_counts(values: tuple[int, ...]) -> tuple[int, ...]:
    return tuple(len(str(value)) for value in values)
