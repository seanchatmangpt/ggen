#!/usr/bin/env python3
"""
test_tpot2_autoconfig.py — Chicago-TDD checks for the TPOT2 wasm4pm auto-config.

Chicago TDD discipline (see .claude/rules/rust/testing.md, testing-anti-cheating.md):
  * REAL collaborators only — no mocks, no test doubles, no fabricated fixtures.
    Each test reads the REAL algorithms.ttl off disk and/or runs the REAL reference
    implementation (reference_autoconfig.py), then asserts on OBSERVABLE state.
  * State-based / externalizable evidence — assertions are on parsed registry data,
    recomputed fitness, and files actually written to verify/out/ (e.g. tomllib must
    load reference_ggen.toml from disk). Faking is harder than the real computation.
  * AAA pattern — Arrange (real paths/data) -> Act (real parse/compute/IO) -> Assert.

Runs with PLAIN python3 (no pytest required):  python3 verify/test_tpot2_autoconfig.py
Also pytest-compatible if pytest is installed:  python3 -m pytest -q verify/test_tpot2_autoconfig.py

The registry under test defaults to the project copy, falling back to the canonical
catalogue copy (so the suite runs even before the project copy lands).
"""

from __future__ import annotations

import json
import sys
import tempfile
import traceback
from pathlib import Path

# Make the reference module importable when run from any cwd.
_THIS_DIR = Path(__file__).resolve().parent
if str(_THIS_DIR) not in sys.path:
    sys.path.insert(0, str(_THIS_DIR))

import reference_autoconfig as ref  # noqa: E402  (real collaborator, not a mock)

# tomllib is stdlib in 3.11+.
try:
    import tomllib
except ModuleNotFoundError:  # pragma: no cover
    tomllib = None  # type: ignore


# ---------------------------------------------------------------------------
# Shared real fixtures (computed once from REAL files — not fabricated)
# ---------------------------------------------------------------------------

def _registry_path() -> Path:
    return ref._resolve_registry(None)


def _algorithms():
    return ref.parse_registry(_registry_path())


def _algos_by_id():
    return {a.algorithm_id: a for a in _algorithms()}


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

def test_registry_has_60_algorithms():
    """REAL file read: the registry parses to exactly 60 algorithm individuals."""
    # Arrange / Act
    algos = _algorithms()
    # Assert (observable: parsed count of real .ttl content)
    assert len(algos) == 60, f"expected 60 algorithms, parsed {len(algos)} from {_registry_path()}"
    # IDs are unique (no parse duplication).
    ids = [a.algorithm_id for a in algos]
    assert len(set(ids)) == 60, "duplicate algorithm IDs parsed from registry"


def test_nine_stage_bijection_covers_all_categories():
    """All 9 pi:category values map to the 9 stages; per-category counts sum to 60."""
    # Arrange / Act
    algos = _algorithms()
    grouping = ref.assert_taxonomy(algos)  # raises if taxonomy is violated
    # Assert: 9 categories, each non-empty, counts match the frozen §4 table.
    assert len(grouping) == 9, f"expected 9 categories, got {len(grouping)}"
    total = sum(len(v) for v in grouping.values())
    assert total == 60, f"category counts sum to {total}, expected 60"
    expected_counts = {cat: spec[3] for cat, spec in ref.CATEGORY_TO_STAGE.items()}
    actual_counts = {cat: len(v) for cat, v in grouping.items()}
    assert actual_counts == expected_counts, (
        f"per-category counts mismatch:\n  expected {expected_counts}\n  actual   {actual_counts}"
    )
    # Every algorithm's category is one of the 9 known categories.
    known = set(ref.CATEGORY_TO_STAGE)
    for a in algos:
        assert a.category in known, f"{a.algorithm_id} has out-of-taxonomy category {a.category!r}"


def test_fitness_formula_matches_contract():
    """Recompute fitness for 3 known algorithms by hand and assert EXACT equality.

    FROZEN formula: fitness = quality_tier - 0.5 * speed_tier.
      ilp        q=90 s=40 -> 90 - 20   = 70.0
      alignments q=95 s=25 -> 95 - 12.5 = 82.5
      dfg        q=30 s=1  -> 30 - 0.5  = 29.5
    """
    # Arrange: real algorithms from the real registry.
    algos = _algos_by_id()
    expectations = [
        ("ilp", 90, 40, 70.0),
        ("alignments", 95, 25, 82.5),
        ("dfg", 30, 1, 29.5),
    ]
    for aid, q, s, expected_fitness in expectations:
        assert aid in algos, f"expected algorithm {aid!r} absent from registry"
        a = algos[aid]
        # Assert the registry tiers are what the contract states (guards against drift).
        assert a.quality_tier == q, f"{aid}: registry quality_tier {a.quality_tier} != contract {q}"
        assert a.speed_tier == s, f"{aid}: registry speed_tier {a.speed_tier} != contract {s}"
        # Assert the computed fitness is EXACT (these values are exactly representable in float).
        assert a.fitness == expected_fitness, (
            f"{aid}: fitness {a.fitness} != expected {expected_fitness}"
        )


def test_elite_pipeline_is_deterministic():
    """Run the reference twice into two ephemeral dirs; pipeline JSON must be byte-identical."""
    # Arrange: two independent ephemeral output dirs (real IO, auto-cleaned).
    with tempfile.TemporaryDirectory(prefix="tpot2_det_a_") as da, \
         tempfile.TemporaryDirectory(prefix="tpot2_det_b_") as db:
        out_a, out_b = Path(da), Path(db)
        # Act: two full reference runs against the same registry.
        ref.run(registry_path=None, out_dir=out_a)
        ref.run(registry_path=None, out_dir=out_b)
        # Assert: pipeline artifacts are identical on disk (externalizable evidence).
        pa = (out_a / "reference_pipeline.json").read_text(encoding="utf-8")
        pb = (out_b / "reference_pipeline.json").read_text(encoding="utf-8")
        # Also assert the generated TOML is byte-stable across runs.
        ta = (out_a / "reference_ggen.toml").read_text(encoding="utf-8")
        tb = (out_b / "reference_ggen.toml").read_text(encoding="utf-8")
    assert pa == pb, "reference_pipeline.json differs between runs — non-deterministic selection"
    assert ta == tb, "reference_ggen.toml differs between runs — non-deterministic emission"
    # And the elite operator per stage is stable and there are exactly 9.
    data = json.loads(pa)
    assert data["stage_count"] == 9, f"stage_count {data['stage_count']} != 9"
    assert len(data["stages"]) == 9, f"len(stages) {len(data['stages'])} != 9"
    # Stage orders are exactly 1..9 in ascending order (deterministic emission).
    orders = [s["stage_order"] for s in data["stages"]]
    assert orders == list(range(1, 10)), f"stage_order sequence {orders} != 1..9"
    # The 9 stage_ids are exactly the frozen taxonomy stage_ids, in order.
    expected_stage_ids = [spec[1] for spec in sorted(ref.CATEGORY_TO_STAGE.values())]
    actual_stage_ids = [s["stage_id"] for s in data["stages"]]
    assert actual_stage_ids == expected_stage_ids, (
        f"stage_ids {actual_stage_ids} != frozen order {expected_stage_ids}"
    )


def test_reference_ggen_toml_is_valid_toml():
    """The reference-generated ggen.toml must load via tomllib and obey driver rules."""
    assert tomllib is not None, "tomllib unavailable (need Python 3.11+)"
    # Arrange: ensure the artifact exists (run the reference if needed — real IO).
    toml_path = _THIS_DIR / "out" / "reference_ggen.toml"
    if not toml_path.is_file():
        ref.run(registry_path=None, out_dir=_THIS_DIR / "out")
    # Act: parse the real file off disk (observable, externalizable).
    with toml_path.open("rb") as fh:
        cfg = tomllib.load(fh)
    # Assert: structural validity + CONTRACT.md §8 driver rules.
    for table in ("project", "ontology", "generation"):
        assert table in cfg, f"reference_ggen.toml missing [{table}]"
    onto = cfg["ontology"]
    assert "additional" not in onto, "reference_ggen.toml uses forbidden `additional` (BUG-001)"
    assert "imports" in onto, "reference_ggen.toml [ontology] missing `imports`"
    # No `../` escape in any output_file (GGEN-YIELD-001).
    for r in cfg.get("generation", {}).get("rules", []):
        of = r.get("output_file", "")
        assert ".." not in of.split("/"), f"output_file escapes root: {of!r}"
    # Prefixes declare pi, compat, tpot (CONTRACT.md §8).
    prefixes = onto.get("prefixes", {})
    for px in ("pi", "compat", "tpot"):
        assert px in prefixes, f"reference_ggen.toml [ontology.prefixes] missing {px!r}"


def test_pareto_front_nondominated():
    """The computed Pareto front must contain no member that dominates another."""
    # Arrange: real registry.
    algos = _algorithms()
    # Act: compute the global Pareto front.
    front = ref.compute_pareto_front(algos)
    # Assert: front is non-empty and internally non-dominated.
    assert front, "Pareto front is empty"
    for a in front:
        for b in front:
            if a is b:
                continue
            assert not ref.dominates(a, b), (
                f"Pareto front member {a.algorithm_id} dominates {b.algorithm_id} — invalid front"
            )
    # Sanity: every front member is non-dominated by ANY algorithm (not just front members).
    for a in front:
        assert not any(ref.dominates(other, a) for other in algos if other is not a), (
            f"front member {a.algorithm_id} is dominated by a non-front operator"
        )
    # Sanity: at least one known extreme is on the front.
    front_ids = {a.algorithm_id for a in front}
    # alignments (q=95, highest quality) MUST be on the front.
    assert "alignments" in front_ids, "alignments (max quality) absent from Pareto front"


def test_elite_pipeline_matches_argmax_fitness():
    """Cross-check: each chosen elite is the true argmax-fitness operator in its stage
    under the frozen tie-break — corroborating the SPARQL NOT EXISTS logic."""
    # Arrange
    algos = _algorithms()
    grouping = ref.assert_taxonomy(algos)
    decisions = ref.compute_pipeline(algos)
    # Act / Assert: for every stage, no other operator out-ranks the chosen elite.
    for d in decisions:
        stage_algos = grouping[d.for_category]
        elite = d.elite
        for other in stage_algos:
            if other.algorithm_id == elite.algorithm_id:
                continue
            # "other out-ranks elite" must be FALSE for the true argmax.
            out_ranks = (
                other.fitness > elite.fitness
                or (other.fitness == elite.fitness and other.speed_tier < elite.speed_tier)
                or (
                    other.fitness == elite.fitness
                    and other.speed_tier == elite.speed_tier
                    and other.algorithm_id < elite.algorithm_id
                )
            )
            assert not out_ranks, (
                f"stage {d.stage_id}: {other.algorithm_id} out-ranks chosen elite {elite.algorithm_id}"
            )


# ---------------------------------------------------------------------------
# Plain-python runner (works without pytest)
# ---------------------------------------------------------------------------

def _all_tests():
    g = globals()
    return [(name, g[name]) for name in sorted(g) if name.startswith("test_") and callable(g[name])]


def main() -> int:
    passed, failed = 0, 0
    failures = []
    print("=" * 76)
    print("CHICAGO-TDD CHECKS — TPOT2 wasm4pm auto-config (real files, observable state)")
    print(f"registry under test: {_registry_path()}")
    print("=" * 76)
    for name, fn in _all_tests():
        try:
            fn()  # default args handle the deterministic-run dirs
            print(f"[PASS] {name}")
            passed += 1
        except Exception as exc:  # noqa: BLE001
            print(f"[FAIL] {name}: {exc}")
            failures.append((name, traceback.format_exc()))
            failed += 1
    print("-" * 76)
    print(f"RESULTS: {passed} passed, {failed} failed, {passed + failed} total")
    print("=" * 76)
    if failures:
        print("\nFAILURE DETAILS:")
        for name, tb in failures:
            print(f"\n--- {name} ---\n{tb}")
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())
