"""Generate deterministic receipt rows for sparse checkpoints."""

def row(cycle: int, tool: str, operation: str, status: str, *, commit: str | None = None) -> dict[str, object]:
    return {
        "cycle": cycle,
        "tool": tool,
        "operation": operation,
        "status": status,
        "commit_sha": commit,
    }

def compact(rows: tuple[dict[str, object], ...]) -> tuple[dict[str, object], ...]:
    return tuple(sorted(rows, key=lambda item: int(item["cycle"])))
