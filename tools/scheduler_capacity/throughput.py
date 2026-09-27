"""Compute conservative throughput summaries from authored batches."""

from dataclasses import dataclass

@dataclass(frozen=True)
class BatchStat:
    files: int
    lines: int
    bytes: int

def totals(stats: tuple[BatchStat, ...]) -> BatchStat:
    return BatchStat(
        sum(x.files for x in stats),
        sum(x.lines for x in stats),
        sum(x.bytes for x in stats),
    )

def per_cycle(stats: tuple[BatchStat, ...], cycles: int) -> float:
    return totals(stats).lines / cycles if cycles else 0.0
