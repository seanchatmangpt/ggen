"""Derive uncertainty-reducing next probes from observations."""

def next_probe(*, scheduler_lower_bound: int, git_refusals: int, notion_failures: int, slack_failures: int) -> str:
    if scheduler_lower_bound < 120:
        return "maximize-local-cycles"
    if git_refusals:
        return "isolate-git-edge-density"
    if slack_failures:
        return "isolate-slack-search-quota"
    if notion_failures:
        return "isolate-notion-read-quota"
    return "extend-local-envelope"
