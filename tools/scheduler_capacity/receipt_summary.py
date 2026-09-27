"""Generate experiment receipts from sparse durable checkpoints."""

def receipt(run_id: str, observed: int, durable: int, previous: int) -> dict[str, object]:
    return {
        "run_id": run_id,
        "observed_cycles": observed,
        "durable_through": durable,
        "lower_bound_max_cycles": max(previous, observed),
        "right_censored": True,
        "next_milestone": ((observed // 20) + 1) * 20,
    }

def lag(data: dict[str, object]) -> int:
    return int(data["observed_cycles"]) - int(data["durable_through"])
