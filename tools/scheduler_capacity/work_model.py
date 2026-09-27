"""Experimental capacity-probe work model."""

from dataclasses import dataclass

@dataclass(frozen=True)
class Batch:
    sequence: int
    label: str
    units: int

def manufacture(labels: tuple[str, ...]) -> tuple[Batch, ...]:
    return tuple(Batch(i, label, len(label)) for i, label in enumerate(labels, 1))
