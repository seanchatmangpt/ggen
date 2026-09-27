"""Accumulate useful manufacturing independently of publication."""

from dataclasses import dataclass

@dataclass(frozen=True)
class Production:
    batches: int = 0
    files: int = 0
    lines: int = 0
    bytes: int = 0

    def add(self, *, files: int, lines: int, bytes: int) -> "Production":
        return Production(self.batches + 1, self.files + files, self.lines + lines, self.bytes + bytes)

    @property
    def density(self) -> float:
        return self.lines / self.batches if self.batches else 0.0
