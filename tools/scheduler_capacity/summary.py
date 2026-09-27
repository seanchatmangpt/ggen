"""Generate a compact human-readable probe summary from counters."""


def summary(run_id: str, highest_cycle: int, lower_bound: int,
            durable_commits: int, code_cycles: int,
            first_failure: str | None) -> str:
    boundary = max(highest_cycle, lower_bound)
    failure = first_failure or "none observed"
    return (
        f"# Scheduler capacity probe {run_id}\n\n"
        f"- Highest observable cycle this run: {highest_cycle}\n"
        f"- Cross-run lower bound: >= {boundary}\n"
        f"- Durable commits: {durable_commits}\n"
        f"- Durable code cycles: {code_cycles}\n"
        f"- First failure/refusal: {failure}\n"
        "- Hard maximum: unknown unless a repeated scheduler boundary is observed.\n"
    )
