"""240-cycle checkpoint helper; reaching it still implies only a lower bound."""

def status(cycle: int, previous: int = 110) -> dict[str, object]:
    return {
        "cycle": cycle,
        "reached_240": cycle >= 240,
        "lower_bound": max(previous, cycle),
        "hard_maximum_known": False,
        "next_target": ((cycle // 20) + 1) * 20,
    }

def improvement(cycle: int, previous: int = 110) -> int:
    return max(0, cycle - previous)
