"""Edge-local FOND state for probe instrumentation."""

from dataclasses import dataclass, replace

@dataclass(frozen=True)
class Edge:
    name: str
    available: bool = True
    failures: int = 0

    def remove(self) -> "Edge":
        return replace(self, available=False, failures=self.failures + 1)

def choose(edges: tuple[Edge, ...]) -> Edge | None:
    return next((edge for edge in edges if edge.available), None)

def failure(edges: tuple[Edge, ...], name: str) -> tuple[Edge, ...]:
    return tuple(edge.remove() if edge.name == name else edge for edge in edges)
