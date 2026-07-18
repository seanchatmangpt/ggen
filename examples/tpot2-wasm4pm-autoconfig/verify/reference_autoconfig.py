#!/usr/bin/env python3
"""
reference_autoconfig.py — Pure-Python reference implementation of the TPOT2
auto-configuration selection for the wasm4pm process-mining engine.

WHY THIS EXISTS
---------------
The container has NO `cargo`, NO `ggen` binary, NO `rdflib` (CONTRACT.md §9).
We therefore CANNOT run `ggen sync` and MUST NOT fabricate a receipt. Instead,
this script is an INDEPENDENT, deterministic reference computation that reads the
SAME ontology the SPARQL queries read and computes the SAME elite pipeline the
SPARQL ranking would produce — writing REAL output artifacts to verify/out/ as
observable, externalizable evidence (Chicago TDD: real file read -> real compute
-> real files on disk).

It uses ONLY the Python standard library. The .ttl registry is parsed by line/
regex scanning (NO rdflib), so the script runs even before Agent 1 finishes the
project copy: it defaults to the project copy `ontology/algorithms.ttl` and falls
back to the canonical catalogue copy.

FROZEN CONTRACT POINTS IMPLEMENTED (CONTRACT.md):
  * §4  9-stage taxonomy bijection to the 9 pi:category values (4+20+6+10+4+8+5+2+1=60)
  * §5  fitness = qualityTier - (0.5 * speedTier)          (lambdaCost = 0.5)
  * §5  per-stage ELITE = argmax fitness; tie-break: lower speedTier, then
        operatorId ascending  (matches the SPARQL NOT EXISTS strict total order)
  * §5  Pareto dominance: A dominates B iff q_A>=q_B and s_A<=s_B and at least one
        strict; Pareto-front = globally non-dominated operators
  * §6  stage -> semconv proof file mapping (the 6 proof files)
  * §5  random_seed = 42, deterministic (sorted iteration everywhere)

OUTPUT ARTIFACTS (verify/out/):
  * reference_pipeline.json     the 9 elite stages, ordered, full detail + seed 42
  * reference_ggen.toml         a valid, tomllib-loadable ggen.toml emitting the pipeline
  * reference_pareto_front.txt  the globally non-dominated operators, human-readable
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from dataclasses import dataclass, field, asdict
from pathlib import Path
from typing import Dict, List, Optional, Tuple

# ---------------------------------------------------------------------------
# FROZEN constants (CONTRACT.md §4, §5, §6)
# ---------------------------------------------------------------------------

LAMBDA_COST = 0.5
RANDOM_SEED = 42
EXPECTED_TOTAL_ALGORITHMS = 60

# CONTRACT.md §4 — bijection: pi:category string -> (stage_order, stage_id, stage_label, expected_count)
# Counts: import_export=4 discovery=20 object_centric=6 discovery_analytics=10
#         conformance=4 ml_analytics=8 prediction=5 simulation=2 agentic=1  => 60
CATEGORY_TO_STAGE: Dict[str, Tuple[int, str, str, int]] = {
    "import_export":        (1, "ingest",      "Ingest",                4),
    "discovery":            (2, "discover",     "Discover",             20),
    "object_centric":       (3, "discover_oc",  "DiscoverObjectCentric", 6),
    "discovery_analytics":  (4, "analyze",      "Analyze",              10),
    "conformance":          (5, "conform",      "Conform",               4),
    "ml_analytics":         (6, "learn",        "Learn",                 8),
    "prediction":           (7, "predict",      "Predict",               5),
    "simulation":           (8, "simulate",     "Simulate",              2),
    "agentic":              (9, "orchestrate",  "Orchestrate",           1),
}

# CONTRACT.md §6 — stage_id -> semconv proof file (else "")
STAGE_PROOF_QUERY: Dict[str, str] = {
    "ingest":      "ocel-load.rq",
    "discover":    "discover-dfg.rq",
    "conform":     "conformance-check.rq",
    "learn":       "ml-classify.rq",
    "predict":     "predict-activity.rq",
    # analyze / discover_oc / simulate / orchestrate -> "" (no dedicated proof)
}

# Default + fallback registry locations (CONTRACT.md §1, §9).
_THIS_DIR = Path(__file__).resolve().parent                       # .../verify
_PROJECT_DIR = _THIS_DIR.parent                                   # .../tpot2-wasm4pm-autoconfig
_REPO_ROOT = _PROJECT_DIR.parent.parent                          # .../ggen
PROJECT_REGISTRY = _PROJECT_DIR / "ontology" / "algorithms.ttl"
CANONICAL_REGISTRY = (
    _REPO_ROOT / "ontology_catalogue" / "wasm4pm" / "ggen" / "ontology" / "algorithms.ttl"
)
OUT_DIR = _THIS_DIR / "out"


# ---------------------------------------------------------------------------
# Data model
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class Algorithm:
    algorithm_id: str
    algorithm_label: str
    category: str
    speed_tier: int
    quality_tier: int
    wasm_export: str
    input_format: str
    output_type: str
    cli_alias: Optional[str] = None  # OMITTED in TTL when absent (never empty string)

    @property
    def fitness(self) -> float:
        # FROZEN formula (CONTRACT.md §5): quality - lambdaCost * speed
        return self.quality_tier - (LAMBDA_COST * self.speed_tier)


@dataclass
class StageDecision:
    stage_order: int
    stage_id: str
    stage_label: str
    for_category: str
    operator_count: int
    elite: Algorithm
    proof_query: str


# ---------------------------------------------------------------------------
# TTL parsing (stdlib only — NO rdflib). Block-oriented line scanner.
# ---------------------------------------------------------------------------

# Matches the subject line of an algorithm block, e.g.:
#   pi:Algo_a_star a pi:ProcessIntelligenceAlgorithm ;
_SUBJECT_RE = re.compile(
    r"^\s*pi:(\S+)\s+a\s+pi:ProcessIntelligenceAlgorithm\b"
)
# Matches a `pi:predicate "value" ...` or `pi:predicate "value"^^xsd:type` line.
# Captures the predicate localname and the first double-quoted literal.
_PRED_STR_RE = re.compile(r'^\s*pi:(\w+)\s+"([^"]*)"')


def _resolve_registry(explicit: Optional[Path]) -> Path:
    """Pick the registry path: explicit arg > project copy > canonical fallback."""
    if explicit is not None:
        p = Path(explicit).expanduser().resolve()
        if not p.is_file():
            raise FileNotFoundError(f"registry not found: {p}")
        return p
    if PROJECT_REGISTRY.is_file():
        return PROJECT_REGISTRY
    if CANONICAL_REGISTRY.is_file():
        return CANONICAL_REGISTRY
    raise FileNotFoundError(
        f"no registry found at {PROJECT_REGISTRY} or {CANONICAL_REGISTRY}"
    )


def parse_registry(registry_path: Optional[Path] = None) -> List[Algorithm]:
    """
    Parse all pi:ProcessIntelligenceAlgorithm individuals from a Turtle file using
    only stdlib line/regex scanning. Returns algorithms sorted by algorithm_id
    (determinism). A block runs from a subject line until the next subject line.
    """
    path = _resolve_registry(registry_path)
    text = path.read_text(encoding="utf-8")
    lines = text.splitlines()

    # First, locate all subject-block start indices.
    starts: List[Tuple[int, str]] = []
    for idx, line in enumerate(lines):
        m = _SUBJECT_RE.match(line)
        if m:
            starts.append((idx, m.group(1)))  # (line index, subject localname e.g. Algo_a_star)

    algorithms: List[Algorithm] = []
    for i, (start_idx, _subject) in enumerate(starts):
        end_idx = starts[i + 1][0] if i + 1 < len(starts) else len(lines)
        block = lines[start_idx:end_idx]
        props: Dict[str, str] = {}
        for bl in block:
            pm = _PRED_STR_RE.match(bl)
            if pm:
                pred, val = pm.group(1), pm.group(2)
                # First occurrence wins (predicates are single-valued here).
                props.setdefault(pred, val)

        # Required predicates per CONTRACT.md §5 / extract-algorithms.rq.
        required = (
            "algorithmId", "algorithmLabel", "category", "speedTier",
            "qualityTier", "wasmExport", "inputFormat", "outputType",
        )
        missing = [r for r in required if r not in props]
        if missing:
            raise ValueError(
                f"algorithm block at line {start_idx + 1} missing {missing}: {props.get('algorithmId', '?')}"
            )

        try:
            speed = int(props["speedTier"])
            quality = int(props["qualityTier"])
        except ValueError as exc:
            raise ValueError(
                f"non-integer tier on {props['algorithmId']}: {exc}"
            ) from exc

        algorithms.append(
            Algorithm(
                algorithm_id=props["algorithmId"],
                algorithm_label=props["algorithmLabel"],
                category=props["category"],
                speed_tier=speed,
                quality_tier=quality,
                wasm_export=props["wasmExport"],
                input_format=props["inputFormat"],
                output_type=props["outputType"],
                cli_alias=props.get("cliAlias"),  # None when absent — never ""
            )
        )

    # Deterministic order.
    algorithms.sort(key=lambda a: a.algorithm_id)
    return algorithms


# ---------------------------------------------------------------------------
# Core TPOT2 selection logic
# ---------------------------------------------------------------------------

def assert_taxonomy(algorithms: List[Algorithm]) -> Dict[str, List[Algorithm]]:
    """
    Group algorithms by pi:category, asserting all 9 categories are present, that
    the total is 60, and that per-category counts match the FROZEN §4 table.
    Returns the {category: [algorithms]} grouping.
    """
    if len(algorithms) != EXPECTED_TOTAL_ALGORITHMS:
        raise AssertionError(
            f"expected {EXPECTED_TOTAL_ALGORITHMS} algorithms, parsed {len(algorithms)}"
        )

    grouping: Dict[str, List[Algorithm]] = {cat: [] for cat in CATEGORY_TO_STAGE}
    for alg in algorithms:
        if alg.category not in CATEGORY_TO_STAGE:
            raise AssertionError(
                f"algorithm {alg.algorithm_id} has unknown category '{alg.category}' "
                f"(not in the frozen 9-stage taxonomy)"
            )
        grouping[alg.category].append(alg)

    # All 9 categories present and counts match the frozen table.
    for cat, (_order, stage_id, _label, expected_n) in CATEGORY_TO_STAGE.items():
        actual_n = len(grouping[cat])
        if actual_n == 0:
            raise AssertionError(f"category '{cat}' (stage '{stage_id}') has 0 algorithms")
        if actual_n != expected_n:
            raise AssertionError(
                f"category '{cat}' expected {expected_n} algorithms, found {actual_n}"
            )

    if len(grouping) != 9:
        raise AssertionError(f"expected 9 categories, grouped into {len(grouping)}")

    return grouping


def _elite_sort_key(alg: Algorithm) -> Tuple[float, int, str]:
    """
    Strict total order for argmax (CONTRACT.md §5), matching the SPARQL NOT EXISTS:
      another operator out-ranks me iff
        other.fitness > mine
        OR (== fitness AND other.speed < mine)
        OR (== fitness AND == speed AND other.id < mine)
    The elite is the UNIQUE maximum. We pick it by sorting DESC on fitness, then
    ASC on speed_tier, then ASC on operator_id, and taking the first.
    """
    # We want: max fitness, then min speed, then min id.
    # Sort ascending by (-fitness, speed, id) and take [0].
    return (-alg.fitness, alg.speed_tier, alg.algorithm_id)


def choose_elite(stage_algorithms: List[Algorithm]) -> Algorithm:
    """Per-stage elite = the unique argmax under the frozen lexicographic order."""
    if not stage_algorithms:
        raise ValueError("cannot choose elite from empty stage")
    return sorted(stage_algorithms, key=_elite_sort_key)[0]


def compute_pipeline(algorithms: List[Algorithm]) -> List[StageDecision]:
    """
    Compute the 9-stage chosen pipeline: one elite operator per stage, ordered by
    stage_order. This is the deterministic result the SPARQL extract-pareto-pipeline.rq
    would produce.
    """
    grouping = assert_taxonomy(algorithms)

    decisions: List[StageDecision] = []
    for category, (order, stage_id, label, _n) in CATEGORY_TO_STAGE.items():
        stage_algos = grouping[category]
        elite = choose_elite(stage_algos)
        decisions.append(
            StageDecision(
                stage_order=order,
                stage_id=stage_id,
                stage_label=label,
                for_category=category,
                operator_count=len(stage_algos),
                elite=elite,
                proof_query=STAGE_PROOF_QUERY.get(stage_id, ""),
            )
        )

    decisions.sort(key=lambda d: d.stage_order)
    return decisions


def dominates(a: Algorithm, b: Algorithm) -> bool:
    """
    Pareto dominance (CONTRACT.md §5): A dominates B iff
      quality_A >= quality_B AND speed_A <= speed_B AND (strict in at least one).
    (Maximize quality, minimize speed-tier cost.)
    """
    if a is b:
        return False
    no_worse = a.quality_tier >= b.quality_tier and a.speed_tier <= b.speed_tier
    strictly_better = a.quality_tier > b.quality_tier or a.speed_tier < b.speed_tier
    return no_worse and strictly_better


def compute_pareto_front(algorithms: List[Algorithm]) -> List[Algorithm]:
    """
    Global Pareto front = operators not dominated by ANY other operator.
    Sorted (CONTRACT.md §6 extract-pareto-front ordering): DESC quality, ASC speed,
    ASC operator_id.
    """
    front: List[Algorithm] = []
    for cand in algorithms:
        if not any(dominates(other, cand) for other in algorithms if other is not cand):
            front.append(cand)
    front.sort(key=lambda a: (-a.quality_tier, a.speed_tier, a.algorithm_id))
    return front


# ---------------------------------------------------------------------------
# Output artifact writers
# ---------------------------------------------------------------------------

def _stage_to_dict(d: StageDecision) -> dict:
    e = d.elite
    return {
        "stage_order": d.stage_order,
        "stage_id": d.stage_id,
        "stage_label": d.stage_label,
        "for_category": d.for_category,
        "operator_count": d.operator_count,
        "operator_id": e.algorithm_id,
        "algorithm_label": e.algorithm_label,
        "wasm_export": e.wasm_export,
        "input_format": e.input_format,
        "output_type": e.output_type,
        "speed_tier": e.speed_tier,
        "quality_tier": e.quality_tier,
        "fitness_score": round(e.fitness, 4),
        "cli_alias": e.cli_alias,  # may be null
        "proof_query": d.proof_query,
    }


def write_pipeline_json(decisions: List[StageDecision], registry_path: Path, out_path: Path) -> dict:
    payload = {
        "generator": "reference_autoconfig.py (pure-Python TPOT2 reference, no ggen binary)",
        "registry_source": str(registry_path),
        "random_seed": RANDOM_SEED,
        "lambda_cost": LAMBDA_COST,
        "fitness_formula": "quality_tier - (lambda_cost * speed_tier)",
        "selection_rule": "per-stage argmax fitness; tie-break lower speed_tier, then operator_id asc",
        "stage_count": len(decisions),
        "stages": [_stage_to_dict(d) for d in decisions],
    }
    out_path.write_text(json.dumps(payload, indent=2, sort_keys=False) + "\n", encoding="utf-8")
    return payload


def _toml_escape(s: str) -> str:
    return s.replace("\\", "\\\\").replace('"', '\\"')


def write_reference_ggen_toml(decisions: List[StageDecision], out_path: Path) -> None:
    """
    Emit a valid ggen.toml that drives the chosen pipeline. This mirrors what
    templates/generated-ggen-toml.tera renders, but is produced directly by the
    reference impl so tomllib can independently confirm the design yields valid TOML.
    Follows CONTRACT.md §8 driver rules: [ontology] imports (not additional), no
    `../` in output_file, [ontology.prefixes] for pi/compat/tpot, direct file= refs.
    """
    lines: List[str] = []
    lines.append("# ===========================================================================")
    lines.append("# REFERENCE ggen.toml — produced by verify/reference_autoconfig.py")
    lines.append("# Independent pure-Python proof that the auto-config design yields valid TOML.")
    lines.append("# NOT generated by `ggen sync` (no binary in container). Mirrors the elite pipeline.")
    lines.append("#")
    lines.append("# Chosen pipeline (stage_order. stage_id -> operator_id  [fitness]):")
    for d in decisions:
        lines.append(
            f"#   {d.stage_order}. {d.stage_id} -> {d.elite.algorithm_id} "
            f"[q={d.elite.quality_tier} s={d.elite.speed_tier} fit={round(d.elite.fitness, 4)}]"
        )
    lines.append("# ===========================================================================")
    lines.append("")
    lines.append("[project]")
    lines.append('name = "wasm4pm-autoconfigured-pipeline"')
    lines.append('version = "0.1.0"')
    lines.append(
        'description = "Pareto-optimal wasm4pm process-mining pipeline auto-selected '
        'by TPOT2-style fitness ranking (reference impl)"'
    )
    lines.append('authors = ["tpot2-wasm4pm-autoconfig"]')
    lines.append('license = "MIT"')
    lines.append("")
    lines.append("[ontology]")
    lines.append("# CONTRACT.md §8: extra ontologies via `imports`, NEVER `additional` (BUG-001).")
    lines.append('source = "ontology/tpot-search-space.ttl"')
    lines.append('imports = ["ontology/algorithms.ttl", "ontology/breeds.ttl"]')
    lines.append("")
    lines.append("[ontology.prefixes]")
    lines.append('pi = "https://wasm4pm.dev/pi#"')
    lines.append('compat = "https://wasm4pm.dev/ns#"')
    lines.append('tpot = "https://wasm4pm.dev/tpot#"')
    lines.append("")
    lines.append("[generation]")
    lines.append('output_dir = "generated/"')
    lines.append("")

    # One [[generation.rules]] per stage that has a dedicated semconv proof query.
    for d in decisions:
        if not d.proof_query:
            continue
        e = d.elite
        lines.append(
            f"# stage {d.stage_order} ({d.stage_id}): elite operator "
            f'"{e.algorithm_id}" | wasm_export = {e.wasm_export} | '
            f"speed_tier = {e.speed_tier} | quality_tier = {e.quality_tier} | "
            f"fitness_score = {round(e.fitness, 4)}"
        )
        lines.append("[[generation.rules]]")
        lines.append(f'name = "stage-{d.stage_id}-{e.algorithm_id}"')
        lines.append(f'query = {{ file = "queries/{d.proof_query}" }}')
        lines.append(f'template = {{ file = "templates/{d.stage_id}.tera" }}')
        lines.append(f'output_file = "{d.stage_id}-result.ttl"')
        lines.append('mode = "Overwrite"')
        lines.append("")

    lines.append("[sync]")
    lines.append("enabled = true")
    lines.append('on_change = "manual"')
    lines.append("validate_after = true")
    lines.append('conflict_mode = "fail"')
    lines.append("")
    lines.append("[rdf]")
    lines.append('formats = ["turtle"]')
    lines.append('default_format = "turtle"')
    lines.append("strict_validation = false")
    lines.append("")

    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_pareto_front_txt(front: List[Algorithm], out_path: Path) -> None:
    lines: List[str] = []
    lines.append("# Global Pareto front (non-dominated operators) — reference computation")
    lines.append("# Dominance: A dominates B iff quality_A>=quality_B and speed_A<=speed_B and strict in one.")
    lines.append("# Ordered: DESC(quality_tier), ASC(speed_tier), ASC(operator_id).")
    lines.append(f"# Members: {len(front)}")
    lines.append("#")
    lines.append("# operator_id                          quality  speed  fitness   category")
    for a in front:
        lines.append(
            f"{a.algorithm_id:<36}  {a.quality_tier:>6}  {a.speed_tier:>5}  "
            f"{round(a.fitness, 4):>8}   {a.category}"
        )
    out_path.write_text("\n".join(lines) + "\n", encoding="utf-8")


# ---------------------------------------------------------------------------
# Public entry point (importable + CLI)
# ---------------------------------------------------------------------------

def run(registry_path: Optional[Path] = None, out_dir: Optional[Path] = None) -> dict:
    """
    Full reference computation. Reads the registry, computes the pipeline + Pareto
    front, writes the 3 artifacts. Returns the pipeline payload dict.
    Deterministic: same inputs -> byte-identical outputs.
    """
    resolved = _resolve_registry(registry_path)
    out = out_dir if out_dir is not None else OUT_DIR
    out.mkdir(parents=True, exist_ok=True)

    algorithms = parse_registry(resolved)
    decisions = compute_pipeline(algorithms)
    front = compute_pareto_front(algorithms)

    pipeline_json = out / "reference_pipeline.json"
    ggen_toml = out / "reference_ggen.toml"
    pareto_txt = out / "reference_pareto_front.txt"

    payload = write_pipeline_json(decisions, resolved, pipeline_json)
    write_reference_ggen_toml(decisions, ggen_toml)
    write_pareto_front_txt(front, pareto_txt)

    payload["_pareto_front_size"] = len(front)
    payload["_out_dir"] = str(out)
    return payload


def _print_summary(payload: dict) -> None:
    print("=" * 76)
    print("TPOT2 AUTO-CONFIG — PURE-PYTHON REFERENCE (no ggen binary; observable evidence)")
    print("=" * 76)
    print(f"registry        : {payload['registry_source']}")
    print(f"random_seed     : {payload['random_seed']}   lambda_cost: {payload['lambda_cost']}")
    print(f"fitness formula : {payload['fitness_formula']}")
    print(f"stages          : {payload['stage_count']}")
    print(f"pareto front    : {payload['_pareto_front_size']} operators")
    print(f"artifacts -> {payload['_out_dir']}")
    print("-" * 76)
    print(f"{'#':>2}  {'stage_id':<13} {'elite operator_id':<26} {'q':>3} {'s':>3} "
          f"{'fitness':>8}  {'proof_query':<20}")
    print("-" * 76)
    for s in payload["stages"]:
        print(
            f"{s['stage_order']:>2}  {s['stage_id']:<13} {s['operator_id']:<26} "
            f"{s['quality_tier']:>3} {s['speed_tier']:>3} {s['fitness_score']:>8}  "
            f"{(s['proof_query'] or '-'):<20}"
        )
    print("=" * 76)


def main(argv: Optional[List[str]] = None) -> int:
    parser = argparse.ArgumentParser(
        description="Pure-Python TPOT2 auto-config reference for wasm4pm (no ggen binary)."
    )
    parser.add_argument(
        "registry", nargs="?", default=None,
        help="path to algorithms.ttl (default: project copy, fallback: canonical catalogue)",
    )
    parser.add_argument(
        "--out", default=None,
        help="output directory for artifacts (default: verify/out/)",
    )
    args = parser.parse_args(argv)

    registry = Path(args.registry) if args.registry else None
    out_dir = Path(args.out) if args.out else None

    try:
        payload = run(registry_path=registry, out_dir=out_dir)
    except (FileNotFoundError, AssertionError, ValueError) as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 1

    _print_summary(payload)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
