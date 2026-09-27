"""Compact local state transition model for repeated capacity cycles."""

from dataclasses import dataclass

@dataclass(frozen=True)
class State:
    cycle: int
    useful_units: int

def step(state: State, manufactured: int = 1) -> State:
    return State(state.cycle + 1, state.useful_units + manufactured)

def advance(state: State, count: int) -> State:
    for _ in range(max(0, count)):
        state = step(state)
    return state
