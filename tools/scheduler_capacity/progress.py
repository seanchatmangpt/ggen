"""Separate scheduler progress from instrumentation progress."""

from dataclasses import dataclass

@dataclass(frozen=True)
class Progress:
    cycle: int
    manufactured: int
    durable_witness_cycle: int

    @property
    def witness_lag(self) -> int:
        return max(0, self.cycle - self.durable_witness_cycle)

    def advance(self, units: int = 1) -> "Progress":
        return Progress(self.cycle + 1, self.manufactured + units, self.durable_witness_cycle)

    def witness(self) -> "Progress":
        return Progress(self.cycle, self.manufactured, self.cycle)
