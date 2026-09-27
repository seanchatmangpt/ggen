"""Compact operation accounting for capacity receipts."""

from collections import Counter

def count(operations: tuple[tuple[str, str], ...]) -> dict[str, int]:
    c = Counter(f"{surface}:{status}" for surface, status in operations)
    return dict(sorted(c.items()))

def successes(operations: tuple[tuple[str, str], ...], surface: str) -> int:
    return sum(1 for s, status in operations if s == surface and status == "SUCCESS")
