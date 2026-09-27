"""Model first-failure observations without promoting them to run termination."""

from dataclasses import dataclass

@dataclass(frozen=True)
class Failure:
    cycle: int
    edge: str
    kind: str

def first(items: tuple[Failure, ...], kind: str | None = None) -> Failure | None:
    eligible = (x for x in items if kind is None or x.kind == kind)
    return min(eligible, key=lambda x: x.cycle, default=None)

def remove_edge(edges: tuple[str, ...], failed: Failure) -> tuple[str, ...]:
    return tuple(edge for edge in edges if edge != failed.edge)
