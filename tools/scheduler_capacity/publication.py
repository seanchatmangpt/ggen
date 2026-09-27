"""Plan immutable checkpoint branch names when ref mutation is unavailable."""


def checkpoint_branch(run_id: str, ordinal: int) -> str:
    if ordinal < 1:
        raise ValueError("ordinal must be positive")
    safe = run_id.replace("/", "-").replace(" ", "-")
    return f"probe/{safe}-c{ordinal}"


def publication_route(update_ref_available: bool) -> str:
    return "update_ref" if update_ref_available else "immutable_checkpoint_branch"
