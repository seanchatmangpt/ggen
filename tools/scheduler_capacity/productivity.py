"""Keep a rolling local estimate of probe productivity."""

def moving_average(values: tuple[int, ...], width: int = 5) -> tuple[float, ...]:
    if width <= 0:
        raise ValueError("width must be positive")
    return tuple(
        sum(values[max(0, i - width + 1): i + 1]) / len(values[max(0, i - width + 1): i + 1])
        for i in range(len(values))
    )

def peak(values: tuple[int, ...]) -> int:
    return max(values, default=0)
