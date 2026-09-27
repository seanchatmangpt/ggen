"""Summarize edge failures conservatively."""

from collections import defaultdict

def by_edge(rows: tuple[tuple[int, str, str], ...]) -> dict[str, dict[str, int]]:
    result: dict[str, dict[str, int]] = defaultdict(dict)
    for cycle, edge, status in rows:
        result[edge].setdefault("first_cycle", cycle)
        result[edge][status] = result[edge].get(status, 0) + 1
        result[edge]["last_cycle"] = cycle
    return dict(result)
