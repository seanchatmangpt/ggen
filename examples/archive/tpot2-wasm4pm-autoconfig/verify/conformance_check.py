#!/usr/bin/env python3
"""
verify/conformance_check.py — object-centric process-mining conformance for the
generator's OWN declared execution model (Van der Aalst doctrine:
.claude/rules/process-mining-chicago-tdd.md + .claude/rules/testing-anti-cheating.md).

It mirrors the lifecycle-order semantics of the REAL in-repo engine
`ggen_graph::ocel::conformance::check_lifecycle_order`
(crates/ggen-graph/src/ocel/conformance.rs:23) and `discover_dfg`
(crates/ggen-graph/src/ocel/dfg.rs:38): an activity sequence conforms iff each
activity occurs on the SAME case object with strictly increasing timestamps.

Two modes (anti-cheating by construction — see §4.2 of research/05):
  MODE A (runs today, stdlib only): build a CLEARLY-LABELED-SYNTHETIC expected OCEL
    log from the real verify/out/reference_pipeline.json + the declared model, assert
    it PASSES all 8 lawful transitions + the cross-type invariants, AND inject the
    doctrine's 8 IMPOSSIBLE logs and require each to be REJECTED. A stub that always
    returns conforms=True fails the 8 negatives; a stub that always returns False
    fails the positive — so only a real checker passes. Faking > real cost.
  MODE B (flips automatically once a real log exists): if .ggen/ocel/*.ocel.jsonl is
    present, run the IDENTICAL check_declared_model against the captured log. The
    run-derived doctrine criteria (temporal lawfulness, variant control, deviation-
    free, fitness) become EVIDENCE rather than self-consistency.

HONESTY: no real ggen sync runs in this container (no cargo/ggen binary), so the
expected log built here is explicitly synthetic and is NEVER presented as a captured
run. The declared model is frozen in verify/conformance_spec.md.
"""
from __future__ import annotations

import json
import sys
from pathlib import Path

_THIS = Path(__file__).resolve().parent
PROJECT = _THIS.parent
OUT = _THIS / "out"
REF_PIPELINE = OUT / "reference_pipeline.json"
REAL_OCEL_GLOBS = (".ggen/ocel/*.ocel.jsonl", ".ggen/ocel/*.jsonl")

# Declared model constants (frozen — see verify/conformance_spec.md).
GENERATION_RULES = [
    "generate-ggen-toml", "generate-tpot-config", "generate-pipeline-manifest",
    "generate-search-space-report", "generate-stage-plan", "generate-fitness-objectives",
]
OUTPUT_ARTIFACTS = [
    "ggen.toml", "tpot_config.py", "pipeline.json",
    "SEARCH_SPACE.md", "STAGE_PLAN.md", "objectives.json",
]
# artifact -> producing generation_rule (for the no-orphan invariant)
ARTIFACT_RULE = dict(zip(OUTPUT_ARTIFACTS, GENERATION_RULES))


# ---------------------------------------------------------------------------
# Minimal OCEL-ish event model (mirrors ggen_graph OcelEvent / OcelObjectRef)
# ---------------------------------------------------------------------------

def event(eid, activity, ts, objects):
    """objects: list of (obj_id, obj_type, qualifier)."""
    return {
        "id": eid, "activity": activity, "timestamp": ts,
        "objects": [{"id": o[0], "type": o[1], "qualifier": o[2]} for o in objects],
    }


def _refs(ev, obj_id):
    return any(o["id"] == obj_id for o in ev["objects"])


def lifecycle_order(events, case_id, sequence):
    """Mirror check_lifecycle_order: every activity in `sequence` occurs on case
    object `case_id` with strictly increasing (earliest) timestamps."""
    earliest = {}
    for ev in events:
        if _refs(ev, case_id):
            a = ev["activity"]
            ts = ev["timestamp"]
            if a not in earliest or ts < earliest[a]:
                earliest[a] = ts
    prev_ts = None
    for a in sequence:
        if a not in earliest:
            return False
        if prev_ts is not None and not (prev_ts < earliest[a]):
            return False
        prev_ts = earliest[a]
    return True


def directly_follows(events, case_type):
    """Object-centric DFG over case objects of `case_type` (mirrors dfg.rs)."""
    from collections import defaultdict
    cases = defaultdict(list)
    for ev in events:
        for o in ev["objects"]:
            if o["type"] == case_type:
                cases[o["id"]].append((ev["timestamp"], ev["activity"]))
    edges = defaultdict(int)
    for seq in cases.values():
        seq.sort()
        for (_, a), (_, b) in zip(seq, seq[1:]):
            edges[(a, b)] += 1
    return dict(edges)


# ---------------------------------------------------------------------------
# The declared-model conformance check (§2.3 transitions + §2.4 invariants)
# ---------------------------------------------------------------------------

def check_declared_model(log):
    """Return (conforms: bool, failures: list[str]). The 8 lawful transitions and
    the cross-type invariants from research/05 §2."""
    events = log["events"]
    failures = []

    def acts():
        return {e["activity"] for e in events}

    run_ids = {o["id"] for e in events for o in e["objects"] if o["type"] == "generation_run"}
    run = next(iter(run_ids), None)

    # T1: no inference before load (run-level).
    if "OntologyLoaded" in acts() and "InferenceRuleApplied" in acts():
        if run is None or not lifecycle_order(events, run, ["OntologyLoaded", "InferenceRuleApplied"]):
            failures.append("T1: InferenceRuleApplied not after OntologyLoaded")

    # T2: no derivation before its inference rule (run-level).
    if "OperatorDerived" in acts():
        if run is None or not lifecycle_order(events, run, ["InferenceRuleApplied", "OperatorDerived"]):
            failures.append("T2: OperatorDerived not after InferenceRuleApplied")

    # T3: no elite without a prior OperatorDerived (per-operator case).
    op_ids = {o["id"] for e in events if e["activity"] in ("OperatorDerived", "EliteSelected")
              for o in e["objects"] if o["type"] == "operator"}
    for op in op_ids:
        elite_here = any(e["activity"] == "EliteSelected" and _refs(e, op) for e in events)
        if elite_here and not lifecycle_order(events, op, ["OperatorDerived", "EliteSelected"]):
            failures.append(f"T3: EliteSelected before OperatorDerived for {op}")

    # T4: run-level pipeline chain Elite -> Queried -> Rendered -> Emitted -> Receipt.
    if run is not None:
        chain = ["EliteSelected", "GenerationRuleQueried", "ArtifactRendered",
                 "ArtifactEmitted", "ReceiptEmitted"]
        present = [a for a in chain if a in acts()]
        if not lifecycle_order(events, run, present):
            failures.append(f"T4: run-level chain out of order ({present})")

    # T5: per generation_rule, Queried before Rendered.
    rule_ids = {o["id"] for e in events for o in e["objects"] if o["type"] == "generation_rule"}
    for r in rule_ids:
        rendered = any(e["activity"] == "ArtifactRendered" and _refs(e, r) for e in events)
        if rendered and not lifecycle_order(events, r, ["GenerationRuleQueried", "ArtifactRendered"]):
            failures.append(f"T6: ArtifactRendered before GenerationRuleQueried for {r}")

    # T6: per output_artifact, Rendered before Emitted (the headline rule).
    art_ids = {o["id"] for e in events for o in e["objects"] if o["type"] == "output_artifact"}
    for a in art_ids:
        emitted = any(e["activity"] == "ArtifactEmitted" and _refs(e, a) for e in events)
        if emitted and not lifecycle_order(events, a, ["ArtifactRendered", "ArtifactEmitted"]):
            failures.append(f"T_render-before-emit: ArtifactEmitted before ArtifactRendered for {a}")

    # INV-dup-terminal: no artifact emitted twice without an intervening re-render.
    from collections import Counter
    emit_counts = Counter()
    for e in events:
        if e["activity"] == "ArtifactEmitted":
            for o in e["objects"]:
                if o["type"] == "output_artifact":
                    emit_counts[o["id"]] += 1
    render_counts = Counter()
    for e in events:
        if e["activity"] == "ArtifactRendered":
            for o in e["objects"]:
                if o["type"] == "output_artifact":
                    render_counts[o["id"]] += 1
    for a, c in emit_counts.items():
        if c > render_counts.get(a, 0):
            failures.append(f"INV-dup-terminal: {a} emitted {c}x but rendered {render_counts.get(a,0)}x")

    # INV-no-orphan-run: every event references the generation_run case.
    for e in events:
        if not any(o["type"] == "generation_run" for o in e["objects"]):
            failures.append(f"INV-orphan-run: event {e['id']} ({e['activity']}) has no generation_run case")

    # INV-no-orphan-artifact: every emitted artifact has a GenerationRuleQueried of its rule.
    queried_rules = {o["id"] for e in events if e["activity"] == "GenerationRuleQueried"
                     for o in e["objects"] if o["type"] == "generation_rule"}
    for a in emit_counts:
        rule = ARTIFACT_RULE.get(a)
        if rule is not None and rule not in queried_rules:
            failures.append(f"INV-orphan-artifact: {a} emitted with no GenerationRuleQueried for rule {rule}")

    # INV-failed-and-released: no artifact both ArtifactRenderFailed and ArtifactEmitted.
    failed = {o["id"] for e in events if e["activity"] == "ArtifactRenderFailed"
              for o in e["objects"] if o["type"] == "output_artifact"}
    for a in failed & set(emit_counts):
        failures.append(f"INV-failed-and-released: {a} both failed render and emitted (no rework)")

    # INV-receipt-binds-real: receipt references only artifacts that were emitted.
    receipt_bound = {o["id"] for e in events if e["activity"] == "ReceiptEmitted"
                     for o in e["objects"] if o["type"] == "output_artifact"}
    for a in receipt_bound:
        if a not in emit_counts:
            failures.append(f"INV-receipt-phantom: receipt binds {a} which was never emitted")

    return (len(failures) == 0, failures)


# ---------------------------------------------------------------------------
# MODE A — synthetic expected log + the 8 impossible logs
# ---------------------------------------------------------------------------

def build_expected_log(pipeline):
    """SYNTHETIC EXPECTED log derived from the declared model + the real
    reference_pipeline.json elites. NOT a captured ggen run."""
    run = "run:R1"
    rc = ("run:R1", "generation_run", "case")
    evs = []
    t = 0

    def add(activity, ts, objs):
        evs.append(event(f"e{len(evs)+1}", activity, ts, [rc] + objs))

    add("OntologyLoaded", t := t + 1, [("ontology:set", "ontology", "loads")])
    add("InferenceRuleApplied", t := t + 1, [("inference_rule:derive-operators", "inference_rule", "applies")])
    for s in pipeline["stages"]:
        add("OperatorDerived", t := t + 1,
            [(f"operator:{s['operator_id']}", "operator", "derives"),
             (f"stage:{s['stage_id']}", "stage", "at")])
    add("InferenceRuleApplied", t := t + 1, [("inference_rule:derive-pareto-dominance", "inference_rule", "applies")])
    add("DominanceDerived", t := t + 1, [("dominance_edge:front", "dominance_edge", "derives")])
    for s in pipeline["stages"]:
        add("EliteSelected", t := t + 1,
            [(f"operator:{s['operator_id']}", "operator", "selects"),
             (f"stage:{s['stage_id']}", "stage", "at")])
    for r in GENERATION_RULES:
        add("GenerationRuleQueried", t := t + 1, [(f"generation_rule:{r}", "generation_rule", "queries")])
    for art in OUTPUT_ARTIFACTS:
        add("ArtifactRendered", t := t + 1,
            [(f"generation_rule:{ARTIFACT_RULE[art]}", "generation_rule", "renders"),
             (f"output_artifact:{art}", "output_artifact", "produces")])
    for art in OUTPUT_ARTIFACTS:
        add("ArtifactEmitted", t := t + 1, [(f"output_artifact:{art}", "output_artifact", "writes")])
    add("ReceiptEmitted", t := t + 1,
        [("receipt:sync", "receipt", "emits")]
        + [(f"output_artifact:{art}", "output_artifact", "binds") for art in OUTPUT_ARTIFACTS])

    return {"meta": "SYNTHETIC EXPECTED log — derived from the declared model, NOT a captured ggen run.",
            "events": evs}


def impossible_logs(expected):
    """The doctrine's 8 impossible logs (§7), specialized to our verbs. Each must be
    REJECTED by check_declared_model. Returns list of (name, log)."""
    import copy

    def clone():
        return {"meta": "IMPOSSIBLE (negative test)", "events": copy.deepcopy(expected["events"])}

    out = []

    # 1. release before validate: ReceiptEmitted before an ArtifactEmitted.
    g = clone()
    rcpt = next(e for e in g["events"] if e["activity"] == "ReceiptEmitted")
    first_emit = min(e["timestamp"] for e in g["events"] if e["activity"] == "ArtifactEmitted")
    rcpt["timestamp"] = first_emit - 1
    out.append(("1_receipt_before_emit", g))

    # 2. render before select: ArtifactRendered before GenerationRuleQueried.
    g = clone()
    rendered = next(e for e in g["events"] if e["activity"] == "ArtifactRendered")
    queried = next(e for e in g["events"] if e["activity"] == "GenerationRuleQueried")
    rendered["timestamp"], queried["timestamp"] = queried["timestamp"], rendered["timestamp"] + 1000
    out.append(("2_render_before_query", g))

    # 3. duplicate terminal state: emit one artifact twice with no extra render.
    g = clone()
    an_emit = next(e for e in g["events"] if e["activity"] == "ArtifactEmitted")
    art_obj = next(o for o in an_emit["objects"] if o["type"] == "output_artifact")
    g["events"].append(event("e_dup", "ArtifactEmitted", max(e["timestamp"] for e in g["events"]) + 1,
                             [("run:R1", "generation_run", "case"), (art_obj["id"], "output_artifact", "writes")]))
    out.append(("3_duplicate_terminal_emit", g))

    # 4. receipt for non-existent artifact.
    g = clone()
    rcpt = next(e for e in g["events"] if e["activity"] == "ReceiptEmitted")
    rcpt["objects"].append({"id": "output_artifact:PHANTOM", "type": "output_artifact", "qualifier": "binds"})
    out.append(("4_receipt_phantom_artifact", g))

    # 5. benchmark before compile: EliteSelected before any OperatorDerived.
    g = clone()
    min_op = min(e["timestamp"] for e in g["events"] if e["activity"] == "OperatorDerived")
    for e in g["events"]:
        if e["activity"] == "EliteSelected":
            e["timestamp"] = min_op - 1
    out.append(("5_elite_before_operator", g))

    # 6. missing causal predecessor: drop a rule's GenerationRuleQueried but keep its emit.
    g = clone()
    g["events"] = [e for e in g["events"]
                   if not (e["activity"] == "GenerationRuleQueried"
                           and any(o["id"] == "generation_rule:generate-ggen-toml" for o in e["objects"]))]
    out.append(("6_orphan_artifact_no_query", g))

    # 7. orphan invocation: an ArtifactEmitted with no generation_run case.
    g = clone()
    orphan = event("e_orphan", "ArtifactEmitted", max(e["timestamp"] for e in g["events"]) + 1,
                   [("output_artifact:orphan.md", "output_artifact", "writes")])
    g["events"].append(orphan)
    out.append(("7_orphan_no_run_case", g))

    # 8. failed and released without rework.
    g = clone()
    art = next(o for e in g["events"] if e["activity"] == "ArtifactEmitted"
               for o in e["objects"] if o["type"] == "output_artifact")
    g["events"].append(event("e_fail", "ArtifactRenderFailed", 2,
                             [("run:R1", "generation_run", "case"), (art["id"], "output_artifact", "fails")]))
    out.append(("8_failed_and_released", g))

    return out


# ---------------------------------------------------------------------------
# MODE B — real captured OCEL log (auto-detected)
# ---------------------------------------------------------------------------

def find_real_log():
    for pat in REAL_OCEL_GLOBS:
        for p in sorted(PROJECT.glob(pat)):
            return p
    return None


def load_ndjson_ocel(path):
    events = []
    for line in path.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if not line:
            continue
        rec = json.loads(line)
        objs = []
        for o in rec.get("objects", rec.get("omap", [])):
            if isinstance(o, dict):
                objs.append((o.get("id"), o.get("type", o.get("objectType", "")), o.get("qualifier", "")))
        events.append(event(rec.get("id", f"e{len(events)+1}"),
                            rec.get("activity", rec.get("ocel:activity", "")),
                            rec.get("timestamp", rec.get("ocel:timestamp", len(events))),
                            objs))
    return {"meta": f"REAL captured log: {path}", "events": events}


# ---------------------------------------------------------------------------
# Runner
# ---------------------------------------------------------------------------

def run():
    if not REF_PIPELINE.is_file():
        # regenerate via the reference if needed
        sys.path.insert(0, str(_THIS))
        import reference_autoconfig as ref  # noqa
        ref.run(registry_path=None, out_dir=OUT)
    pipeline = json.loads(REF_PIPELINE.read_text(encoding="utf-8"))

    print("=" * 76)
    print("PROCESS-MINING CONFORMANCE — generator's declared execution model")
    print("=" * 76)

    real = find_real_log()
    mode = "B (REAL captured log)" if real else "A (SYNTHETIC expected log — no ggen run in container)"
    print(f"MODE: {mode}")

    passed = failed = 0

    if real:
        log = load_ndjson_ocel(real)
        conforms, fails = check_declared_model(log)
        print(f"[{'PASS' if conforms else 'FAIL'}] real-log conformance ({real})")
        for f in fails:
            print(f"        - {f}")
        passed += int(conforms); failed += int(not conforms)
    else:
        expected = build_expected_log(pipeline)
        # write the synthetic log as an observable, clearly-labeled artifact
        OUT.mkdir(parents=True, exist_ok=True)
        (OUT / "expected_run.ocel.json").write_text(json.dumps(expected, indent=2) + "\n", encoding="utf-8")

        # Positive: the expected log must conform.
        conforms, fails = check_declared_model(expected)
        print(f"[{'PASS' if conforms else 'FAIL'}] positive: synthetic expected log conforms to declared model")
        for f in fails:
            print(f"        - {f}")
        passed += int(conforms); failed += int(not conforms)

        # DFG sanity (mirrors discover_dfg): run-level DFG has the lawful backbone.
        dfg = directly_follows(expected["events"], "generation_run")
        backbone_ok = ("OntologyLoaded", "InferenceRuleApplied") in dfg
        print(f"[{'PASS' if backbone_ok else 'FAIL'}] DFG backbone discovered (OntologyLoaded->InferenceRuleApplied)")
        passed += int(backbone_ok); failed += int(not backbone_ok)

        # Negative: each of the 8 impossible logs MUST be rejected (anti-cheating core).
        for name, bad in impossible_logs(expected):
            conforms_bad, fails_bad = check_declared_model(bad)
            rejected = not conforms_bad
            print(f"[{'PASS' if rejected else 'FAIL'}] negative rejected: impossible log {name}"
                  + ("" if rejected else "  <-- checker FAILED to catch an impossible log!"))
            passed += int(rejected); failed += int(not rejected)

    print("-" * 76)
    print(f"RESULTS: {passed} passed, {failed} failed, {passed + failed} total")
    print("=" * 76)
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    raise SystemExit(run())
