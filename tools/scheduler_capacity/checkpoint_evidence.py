"""Represent sparse-checkpoint success as evidence, never as scheduler authority."""

def checkpoint_evidence(cycle: int, success: bool, sha: str | None = None) -> dict[str, object]:
    return {"cycle": cycle, "success": success, "commit_sha": sha}

def durable_cycles(items: tuple[dict[str, object], ...]) -> tuple[int, ...]:
    return tuple(int(x["cycle"]) for x in items if x["success"])

def latest_durable(items: tuple[dict[str, object], ...]) -> int:
    return max(durable_cycles(items), default=0)
