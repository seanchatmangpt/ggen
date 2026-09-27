"""Distinguish observed execution from durable witness depth."""

def bounds(*, observed_cycle: int, durable_cycle: int, previous: int) -> dict[str, int]:
    return {
        "execution_lower_bound": max(previous, observed_cycle),
        "durable_witness_through": durable_cycle,
        "witness_gap": max(0, observed_cycle - durable_cycle),
    }

def fully_witnessed(observed_cycle: int, durable_cycle: int) -> bool:
    return durable_cycle >= observed_cycle
