"""Compact counters for independent probe surfaces."""

from dataclasses import dataclass

@dataclass(frozen=True)
class Counters:
    cycles: int = 0
    code_batches: int = 0
    github_ops: int = 0
    notion_ops: int = 0
    slack_ops: int = 0

    def tick(self, surface: str = "local") -> "Counters":
        values = self.__dict__ | {"cycles": self.cycles + 1}
        key = f"{surface}_ops"
        if key in values:
            values[key] += 1
        return Counters(**values)
