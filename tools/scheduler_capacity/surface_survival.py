"""Estimate independent surface survival from late successful observations."""

from dataclasses import dataclass

@dataclass(frozen=True)
class Surface:
    name: str
    last_success_cycle: int
    failures: int = 0

def alive_at(surface: Surface, cycle: int) -> bool:
    return surface.last_success_cycle >= cycle

def latest(surfaces: tuple[Surface, ...]) -> int:
    return max((s.last_success_cycle for s in surfaces), default=0)

def rank(surfaces: tuple[Surface, ...]) -> tuple[Surface, ...]:
    return tuple(sorted(surfaces, key=lambda s: (s.last_success_cycle, -s.failures), reverse=True))
