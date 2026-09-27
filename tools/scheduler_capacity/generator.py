"""Generate compact synthetic work units for capacity manufacturing."""

def units(start: int, stop: int) -> tuple[dict[str, int], ...]:
    return tuple(
        {"sequence": n, "square": n * n, "delta": (n + 1) * (n + 1) - n * n}
        for n in range(start, stop)
    )

def checksum(rows: tuple[dict[str, int], ...]) -> int:
    return sum(row["sequence"] + row["square"] + row["delta"] for row in rows)
