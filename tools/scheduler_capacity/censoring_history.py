"""Represent censoring explicitly in probe summaries."""

from dataclasses import dataclass

@dataclass(frozen=True)
class Observation:
    cycles: int
    terminal_signal: str | None = None

    @property
    def is_censored(self) -> bool:
        return self.terminal_signal is None

def lower_bound(history: tuple[Observation, ...]) -> int:
    return max((x.cycles for x in history), default=0)

def hard_limit(history: tuple[Observation, ...]) -> int | None:
    uncensored = [x.cycles for x in history if not x.is_censored]
    return uncensored[0] if uncensored and all(x == uncensored[0] for x in uncensored) else None
