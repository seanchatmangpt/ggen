"""Calculate conservative experiment deltas."""

def delta(previous: dict[str, int], current: dict[str, int]) -> dict[str, int]:
    keys = set(previous) | set(current)
    return {key: current.get(key, 0) - previous.get(key, 0) for key in sorted(keys)}

def positive_only(values: dict[str, int]) -> dict[str, int]:
    return {key: value for key, value in values.items() if value > 0}

def changed(values: dict[str, int]) -> bool:
    return any(value != 0 for value in values.values())
