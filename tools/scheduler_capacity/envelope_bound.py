"""Monotone lower-bound aggregation for censored capacity observations."""

from dataclasses import dataclass

@dataclass(frozen=True)
class Envelope:
    previous_lower_bound: int
    observed_cycles: int
    censored: bool = True

    @property
    def lower_bound(self) -> int:
        return max(self.previous_lower_bound, self.observed_cycles)

    @property
    def hard_maximum_known(self) -> bool:
        return not self.censored and self.observed_cycles >= self.previous_lower_bound

def combine(envelopes: tuple[Envelope, ...]) -> int:
    return max((e.lower_bound for e in envelopes), default=0)
