"""FOND-style probe edge routing for experimental capacity measurement."""
from dataclasses import dataclass, field

@dataclass
class EdgeState:
    available: set[str]
    removed: dict[str, str] = field(default_factory=dict)

    def fail(self, edge: str, failure_class: str) -> None:
        self.available.discard(edge)
        self.removed[edge] = failure_class

    def choose(self, candidates: list[str]) -> str | None:
        return next((edge for edge in candidates if edge in self.available), None)

    def blocked(self, required_alternatives: set[str]) -> bool:
        return not bool(self.available & required_alternatives)

DEFAULT_PUBLICATION_EDGES = {
    "update_ref",
    "create_branch_from_commit",
    "create_pull_request",
}

def publication_route(state: EdgeState) -> str | None:
    return state.choose([
        "update_ref",
        "create_branch_from_commit",
        "create_pull_request",
    ])
