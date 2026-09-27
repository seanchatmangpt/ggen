"""Termination labels keep scheduler and external-edge endings distinct."""

from enum import StrEnum

class Termination(StrEnum):
    SCHEDULER = "scheduler"
    EXECUTION_BUDGET = "execution_budget"
    USEFUL_WORK_EXHAUSTED = "useful_work_exhausted"
    EXTERNAL_EDGE_ONLY = "external_edge_only"
    CENSORED = "censored"

def classify(*, explicit_scheduler_stop: bool, useful_work: bool, external_edges: bool) -> Termination:
    if explicit_scheduler_stop:
        return Termination.SCHEDULER
    if useful_work:
        return Termination.CENSORED
    if external_edges:
        return Termination.USEFUL_WORK_EXHAUSTED
    return Termination.EXTERNAL_EDGE_ONLY
