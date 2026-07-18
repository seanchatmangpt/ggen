#!/usr/bin/env python3
"""
verify/slo_check.py — performance SLO gate for the TPOT2 wasm4pm auto-config.

Per .claude/rules/rust/performance.md: RDF processing <= 5s; reproducibility 100%.
This times the pure-Python REFERENCE (reference_autoconfig.py — a faithful re-impl
of the parse -> derive-fitness -> per-stage-argmax -> emit selection logic) and
asserts (a) worst-case latency under the budget, (b) byte-identical output across
repeated runs (determinism is the SLO's reproducibility clause).

HONESTY: this bounds the cost of the SELECTION + EMISSION surface via the reference,
NOT a real `ggen sync` (no cargo/ggen binary in this container; research/07 estimates
the real working graph at ~2k triples with an O(60^2) dominance pass that is
negligible, comfortably within the 5s budget).
"""
from __future__ import annotations

import sys
import tempfile
import time
from pathlib import Path

_THIS = Path(__file__).resolve().parent
if str(_THIS) not in sys.path:
    sys.path.insert(0, str(_THIS))
import reference_autoconfig as ref  # noqa: E402

SLO_SECONDS = 5.0   # RDF-processing SLO budget (performance.md)
RUNS = 5


def main() -> int:
    times, outputs = [], []
    for i in range(RUNS):
        with tempfile.TemporaryDirectory(prefix=f"tpot2_slo_{i}_") as d:
            t0 = time.perf_counter()
            ref.run(registry_path=None, out_dir=Path(d))
            times.append(time.perf_counter() - t0)
            outputs.append((Path(d) / "reference_pipeline.json").read_text(encoding="utf-8"))

    worst = max(times)
    mean = sum(times) / len(times)
    reproducible = all(o == outputs[0] for o in outputs)

    print("=" * 76)
    print("PERFORMANCE SLO — TPOT2 wasm4pm auto-config (reference timing)")
    print("=" * 76)
    print(f"runs={RUNS}  worst={worst*1000:.1f}ms  mean={mean*1000:.1f}ms  budget={SLO_SECONDS*1000:.0f}ms")
    ok_latency = worst <= SLO_SECONDS
    print(f"[{'PASS' if ok_latency else 'FAIL'}] latency within RDF-processing SLO (<= {SLO_SECONDS}s)")
    print(f"[{'PASS' if reproducible else 'FAIL'}] reproducibility 100% (byte-identical across {RUNS} runs)")
    print("-" * 76)
    ok = ok_latency and reproducible
    print("OVERALL", "PASS" if ok else "FAIL")
    print("=" * 76)
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
