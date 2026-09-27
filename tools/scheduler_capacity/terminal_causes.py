"""Model terminal observations with explicit cause typing."""

from dataclasses import dataclass

@dataclass(frozen=True)
class Terminal:
    cycle: int
    cause: str
    external: bool

def scheduler_terminal(item: Terminal) -> bool:
    return not item.external and item.cause == "scheduler"

def external_terminal(item: Terminal) -> bool:
    return item.external

def comparable(left: Terminal, right: Terminal) -> bool:
    return left.cause == right.cause and left.external == right.external
