"""Select future experiments by remaining uncertainty."""

def uncertainty(*, scheduler_terminal_seen: bool, git_failures: int, notion_failures: int, slack_throttles: int) -> tuple[str, ...]:
    items = []
    if not scheduler_terminal_seen:
        items.append("scheduler-terminal")
    if git_failures:
        items.append("git-edge-nondeterminism")
    if slack_throttles:
        items.append("slack-quota")
    if notion_failures:
        items.append("notion-quota")
    return tuple(items)

def primary(items: tuple[str, ...]) -> str:
    return items[0] if items else "replicate"
