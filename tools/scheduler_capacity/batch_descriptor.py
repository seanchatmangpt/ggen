"""Generate the next useful batch descriptor."""

def descriptor(cycle: int, family: str = "capacity") -> dict[str, object]:
    return {
        "cycle": cycle,
        "family": family,
        "ordinal": cycle // 4,
        "checkpoint_due": cycle % 20 == 0,
    }

def window(start: int, count: int = 8) -> tuple[dict[str, object], ...]:
    return tuple(descriptor(cycle) for cycle in range(start, start + count))
