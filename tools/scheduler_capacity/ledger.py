"""Experimental scheduler-capacity receipt model."""
from dataclasses import asdict, dataclass, field

@dataclass(frozen=True)
class ProbeCycle:
    cycle: int
    tool: str
    operation: str
    status: str
    durable_bytes: int = 0
    durable_files: int = 0
    code_lines: int = 0
    commit_sha: str | None = None
    failure_class: str | None = None

    def record(self) -> dict:
        return {k: v for k, v in asdict(self).items() if v is not None}

@dataclass
class CapacityLedger:
    run_id: str
    base_sha: str
    branch: str
    lower_bound_max_cycles: int
    cycles: list[ProbeCycle] = field(default_factory=list)

    def append(self, observation: ProbeCycle) -> None:
        expected = len(self.cycles) + 1
        if observation.cycle != expected:
            raise ValueError(f"cycle must be {expected}, got {observation.cycle}")
        self.cycles.append(observation)
        self.lower_bound_max_cycles = max(self.lower_bound_max_cycles, observation.cycle)

    def summary(self) -> dict:
        durable = [c for c in self.cycles if c.commit_sha]
        failures = [c for c in self.cycles if c.status != "SUCCESS"]
        return {
            "run_id": self.run_id,
            "highest_observable_cycle": len(self.cycles),
            "lower_bound_max_cycles": self.lower_bound_max_cycles,
            "durable_commits": len(durable),
            "durable_bytes": sum(c.durable_bytes for c in self.cycles),
            "durable_files": sum(c.durable_files for c in self.cycles),
            "code_lines": sum(c.code_lines for c in self.cycles),
            "first_non_success": failures[0].record() if failures else None,
        }
