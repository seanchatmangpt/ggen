"""Choose the next local batch by information gain without external reads."""

from dataclasses import dataclass

@dataclass(frozen=True)
class Candidate:
    name: str
    information_gain: float
    cost: float

    @property
    def score(self) -> float:
        return self.information_gain / self.cost if self.cost > 0 else float("inf")

def choose(items: tuple[Candidate, ...]) -> Candidate | None:
    return max(items, key=lambda item: item.score, default=None)
