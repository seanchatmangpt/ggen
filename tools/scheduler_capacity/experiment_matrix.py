"""Compact experiment matrix for isolating terminal causes."""

def matrix() -> tuple[dict[str, object], ...]:
    return (
        {"name": "local-heavy", "git_interval": 40, "notion_interval": 80, "slack_interval": None},
        {"name": "git-sparse", "git_interval": 80, "notion_interval": 40, "slack_interval": None},
        {"name": "read-control", "git_interval": None, "notion_interval": 20, "slack_interval": 40},
    )

def select(name: str) -> dict[str, object] | None:
    return next((row for row in matrix() if row["name"] == name), None)
