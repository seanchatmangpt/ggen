"""Construct a compact late-run summary without external dependencies."""

def compact(cycle: int, previous: int, commits: int, files: int) -> tuple[tuple[str, int], ...]:
    return (
        ("observed_cycle", cycle),
        ("previous_bound", previous),
        ("new_bound", max(cycle, previous)),
        ("commits", commits),
        ("files", files),
    )

def as_dict(items: tuple[tuple[str, int], ...]) -> dict[str, int]:
    return dict(items)
