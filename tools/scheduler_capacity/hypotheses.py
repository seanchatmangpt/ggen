"""Keep terminal-cause hypotheses separate from observations."""

from dataclasses import dataclass

@dataclass(frozen=True)
class Hypothesis:
    name: str
    supporting_observations: int = 0
    counterexamples: int = 0

    @property
    def net_support(self) -> int:
        return self.supporting_observations - self.counterexamples

def rank(items: tuple[Hypothesis, ...]) -> tuple[Hypothesis, ...]:
    return tuple(sorted(items, key=lambda x: x.net_support, reverse=True))
