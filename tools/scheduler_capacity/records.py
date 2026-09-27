"""Track monotone capacity records across repeated runs."""

def records(observed: tuple[int, ...]) -> tuple[int, ...]:
    best = 0
    out = []
    for value in observed:
        if value > best:
            best = value
            out.append(value)
    return tuple(out)

def record_deltas(observed: tuple[int, ...]) -> tuple[int, ...]:
    rs = records(observed)
    return tuple(b - a for a, b in zip((0,) + rs, rs))
