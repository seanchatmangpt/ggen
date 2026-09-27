"""Estimate checkpoint overhead relative to useful manufacturing."""

def overhead_ratio(*, checkpoint_ops: int, total_ops: int) -> float:
    return checkpoint_ops / total_ops if total_ops else 0.0

def useful_ratio(*, useful_cycles: int, observed_cycles: int) -> float:
    return useful_cycles / observed_cycles if observed_cycles else 0.0

def should_sparsify(*, checkpoint_ops: int, total_ops: int, threshold: float = 0.25) -> bool:
    return overhead_ratio(checkpoint_ops=checkpoint_ops, total_ops=total_ops) > threshold
