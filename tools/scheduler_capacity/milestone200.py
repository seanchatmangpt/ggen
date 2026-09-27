"""A 200-cycle milestone remains a lower-bound observation, not a ceiling."""

def milestone_200(cycle: int) -> dict[str, object]:
    return {
        "milestone": 200,
        "reached": cycle >= 200,
        "exceeded": cycle > 200,
        "distance": max(0, 200 - cycle),
    }

def next_after_200(cycle: int) -> int:
    return max(220, ((cycle // 20) + 1) * 20)
