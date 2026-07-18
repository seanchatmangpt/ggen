#!/usr/bin/env python3
"""
test_vision2030.py — Vision-2030 elevation checks for the TPOT2 wasm4pm
auto-config generator (see .claude/rules/vision-2030-mandate.md).

The four Vision-2030 pillars, each enforced by a real, observable, mock-free test:

  1. Absolute Chicago TDD (zero mocks)      — every test below uses the REAL
     reference implementation + REAL files; no mocks/doubles/fabricated fixtures.
  2. Observable state & evidence emitters    — assertions are on files actually
     written to disk and re-parsed (tomllib / json), not in-process flags.
  3. Strict performance SLOs                  — test_slo_generation_latency asserts
     the generation latency budget (RDF-processing SLO: <= 5s; performance.md).
  4. End-to-End boundary crossing            — test_e2e_boundary_crossing drives
     the WHOLE chain across 4 boundaries: registry .ttl (file IO) -> operator
     selection (compute) -> emitted ggen.toml (artifact) -> tomllib re-ingestion
     (re-parse), asserting the elite operators survive the round trip.

Plus two integration-closure tests proving the 6th proof (detect-drift.rq) and the
per-stage descriptor templates are GENUINELY wired (not just declared), matching
the hardened anti-cheating evidence rule in validate_artifacts.py.

Run:  python3 verify/test_vision2030.py          (plain python, no pytest needed)
      python3 -m pytest -q verify/test_vision2030.py
"""

from __future__ import annotations

import json
import sys
import tempfile
import time
import traceback
from pathlib import Path

_THIS_DIR = Path(__file__).resolve().parent
PROJECT_DIR = _THIS_DIR.parent
if str(_THIS_DIR) not in sys.path:
    sys.path.insert(0, str(_THIS_DIR))

import reference_autoconfig as ref  # noqa: E402  (real collaborator, not a mock)

try:
    import tomllib
except ModuleNotFoundError:  # pragma: no cover
    tomllib = None  # type: ignore

# RDF-processing SLO from .claude/rules/rust/performance.md: <= 5s / 1k+ triples.
# The registry is ~60 algorithms (~700 triples); the budget is generous on purpose.
SLO_GENERATION_SECONDS = 5.0

# The 6 wasm4pm semconv proofs that MUST be wired into real generation artifacts.
SEMCONV_PROOFS = (
    "ocel-load.rq", "discover-dfg.rq", "conformance-check.rq",
    "ml-classify.rq", "predict-activity.rq", "detect-drift.rq",
)

# Generation artifacts that may legitimately carry a proof reference (NOT docs,
# NOT the verify/ scripts — those are excluded so the wiring can't be self-faked).
_DOC_OR_VERIFY = {
    "CONTRACT.md", "README.md", "INTEGRATION_REPORT.md",
    "validate_artifacts.py", "reference_autoconfig.py",
    "test_tpot2_autoconfig.py", "test_vision2030.py",
}


# ---------------------------------------------------------------------------
# Pillar 3 — strict performance SLO
# ---------------------------------------------------------------------------

def test_slo_generation_latency():
    """The full selection+emission runs within the RDF-processing SLO budget."""
    with tempfile.TemporaryDirectory(prefix="tpot2_slo_") as d:
        start = time.perf_counter()
        ref.run(registry_path=None, out_dir=Path(d))
        elapsed = time.perf_counter() - start
    assert elapsed <= SLO_GENERATION_SECONDS, (
        f"generation took {elapsed:.3f}s, exceeds SLO budget {SLO_GENERATION_SECONDS}s"
    )


# ---------------------------------------------------------------------------
# Pillar 4 — End-to-End boundary crossing
# ---------------------------------------------------------------------------

def test_e2e_boundary_crossing():
    """Cross every boundary and verify the elite pipeline survives the round trip.

    Boundary 1 (file IO):     parse the real algorithms.ttl off disk.
    Boundary 2 (computation): select the per-stage elite pipeline.
    Boundary 3 (artifact):    emit reference_ggen.toml + reference_pipeline.json.
    Boundary 4 (re-ingest):   re-parse the emitted ggen.toml with tomllib and the
                              pipeline JSON, and assert they agree with the in-memory
                              decision — proving no drift across the boundary.
    """
    assert tomllib is not None, "tomllib unavailable (need Python 3.11+)"

    # Boundary 1 + 2: real parse + real selection (in-memory ground truth).
    algos = ref.parse_registry(ref._resolve_registry(None))
    decisions = ref.compute_pipeline(algos)
    expected = {d.stage_id: d.elite.algorithm_id for d in decisions}
    assert len(expected) == 9, f"expected 9 stage decisions, got {len(expected)}"

    with tempfile.TemporaryDirectory(prefix="tpot2_e2e_") as d:
        out = Path(d)
        # Boundary 3: emit the artifacts to disk.
        ref.run(registry_path=None, out_dir=out)
        toml_path = out / "reference_ggen.toml"
        json_path = out / "reference_pipeline.json"
        assert toml_path.is_file(), "reference_ggen.toml was not emitted"
        assert json_path.is_file(), "reference_pipeline.json was not emitted"

        # Boundary 4: re-ingest both artifacts and reconcile with ground truth.
        with toml_path.open("rb") as fh:
            cfg = tomllib.load(fh)  # proves the emitted config is valid TOML
        pipeline = json.loads(json_path.read_text(encoding="utf-8"))

    # The re-parsed pipeline JSON must carry the SAME elite per stage as the
    # in-memory decision (no information lost crossing compute -> artifact -> parse).
    round_tripped = {s["stage_id"]: s["operator_id"] for s in pipeline["stages"]}
    assert round_tripped == expected, (
        f"elite operators changed across the boundary:\n  in-memory  {expected}\n"
        f"  round-trip {round_tripped}"
    )
    # The emitted ggen.toml is a real, schema-shaped driver (observable at boundary 4).
    for table in ("project", "ontology", "generation"):
        assert table in cfg, f"emitted reference_ggen.toml missing [{table}]"
    assert "imports" in cfg["ontology"], "emitted ggen.toml [ontology] missing imports"
    assert "additional" not in cfg["ontology"], "emitted ggen.toml uses forbidden `additional`"


# ---------------------------------------------------------------------------
# Integration closure — all 6 proofs genuinely wired (anti-cheating evidence)
# ---------------------------------------------------------------------------

def _generation_artifact_texts():
    """All real generation artifacts (queries/templates/ttl/toml), excluding docs and
    the verify/ scripts — mirroring validate_artifacts.py's hardened evidence rule."""
    texts = {}
    for p in PROJECT_DIR.rglob("*"):
        if not p.is_file():
            continue
        parts = p.relative_to(PROJECT_DIR).parts
        if any(seg in {"out", "__pycache__", ".git"} for seg in parts):
            continue
        if p.name in _DOC_OR_VERIFY:
            continue
        if p.suffix.lower() in {".rq", ".tera", ".toml", ".ttl"}:
            texts[str(p.relative_to(PROJECT_DIR))] = p.read_text(encoding="utf-8", errors="ignore")
    return texts


def test_all_six_proofs_wired_in_generation_artifacts():
    """Each of the 6 wasm4pm semconv proofs is referenced by a REAL generation
    artifact (not a doc, not a verify script). detect-drift.rq must now appear."""
    texts = _generation_artifact_texts()
    blob = "\n".join(texts.values())
    missing = [proof for proof in SEMCONV_PROOFS if proof not in blob]
    assert not missing, f"proofs not wired into any generation artifact: {missing}"
    # detect-drift.rq specifically must be wired by a template (the drift monitor).
    drift_sites = [f for f, t in texts.items() if "detect-drift.rq" in t]
    assert any(f.endswith("drift.tera") for f in drift_sites), (
        f"detect-drift.rq not wired by a drift template; sites={drift_sites}"
    )


def test_per_stage_and_drift_templates_present():
    """The 5 per-stage descriptor templates + the drift monitor template exist and
    each filters to the correct stage / wires its proof — so the generated ggen.toml's
    `template = { file = ... }` references all resolve (no broken refs / contract drift)."""
    tdir = PROJECT_DIR / "templates"
    for stage in ("ingest", "discover", "conform", "learn", "predict"):
        f = tdir / f"{stage}.tera"
        assert f.is_file(), f"missing per-stage template {f.name}"
        body = f.read_text(encoding="utf-8")
        assert "sparql_results" in body, f"{f.name} is not a batch template"
        assert f'== "{stage}"' in body, f"{f.name} does not filter to its own stage_id"
        assert not any(ln.startswith("---") for ln in body.splitlines()), f"{f.name} has frontmatter"
    drift = tdir / "drift.tera"
    assert drift.is_file(), "missing drift.tera"
    assert "detect-drift.rq" in drift.read_text(encoding="utf-8"), "drift.tera does not wire detect-drift.rq"


# ---------------------------------------------------------------------------
# Plain-python runner
# ---------------------------------------------------------------------------

def _all_tests():
    g = globals()
    return [(n, g[n]) for n in sorted(g) if n.startswith("test_") and callable(g[n])]


def main() -> int:
    passed, failed, failures = 0, 0, []
    print("=" * 76)
    print("VISION-2030 CHECKS — TPOT2 wasm4pm auto-config (SLO + E2E + wiring)")
    print(f"project: {PROJECT_DIR}")
    print("=" * 76)
    for name, fn in _all_tests():
        try:
            fn()
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
