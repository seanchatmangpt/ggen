"""Track independent edge lifetimes within a run."""

from dataclasses import dataclass

@dataclass(frozen=True)
class EdgeLife:
    name: str
    first_success: int | None = None
    first_failure: int | None = None
    last_success: int | None = None

    @property
    def survived_after_failure(self) -> bool:
        return self.first_failure is not None and self.last_success is not None and self.last_success > self.first_failure
