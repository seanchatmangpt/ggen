"""Classify observed probe outcomes without treating tool failure as scheduler failure."""

from enum import StrEnum

class Outcome(StrEnum):
    SUCCESS = "SUCCESS"
    REFUSED = "REFUSED"
    ERROR = "ERROR"
    THROTTLED = "THROTTLED"
    TRUNCATED = "TRUNCATED"
    UNKNOWN = "UNKNOWN"

def external_edge_available(outcome: Outcome) -> bool:
    return outcome is Outcome.SUCCESS

def scheduler_censored(*, useful_surface_remained: bool, explicit_scheduler_stop: bool) -> bool:
    return useful_surface_remained and not explicit_scheduler_stop
